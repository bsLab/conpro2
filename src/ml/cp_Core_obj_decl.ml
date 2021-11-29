(*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse, http://www.bsslab.de
**
**      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
**                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
**                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
**                 OUTSIDE OF THE SOFTWARE SYSTEM.
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     19.3.2006
**    $VERSION:     2.10
**
**    $INFO:
**
**  Core module.
**
**    $ENDOFINFO
**
*)

open Cp_common
open Cp_types
open Cp_symbol
open Cp_utils
open Cp_data
open Cp_analysis
open Cp_ucode
open Cp_expr
open Cp_syntax
open Printf
open Cp_Core_head
open Cp_stat

let modu_comps = ref [] 

(*
** Object declaration in architecture header.
** pro = None -> mod_objs (used by processes of this module...)
** pro <> None -> pro_objs (process local objects)
*)

let rec obj_decl sym modu pro =
  let proo = pro in
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_signal co ->
    begin
        let inout = (List.mem Obj_port_in co.co_flags) &&
                    (List.mem Obj_port_out co.co_flags) in
        (*
        ** Signals can only have one write source!
        *)
        if (List.length co.co_writer) > 1 then
          error 0 (sprintf "Found signal <%s> with more than one write source!"
                           co.co_name);
        match pro with
        | Some pro ->
            (*
            ** Local process signal
            *)
            [sprintf "signal %s: %s;" 
                     co.co_name
                     (obj_decl_type co.co_type)],[]
        | None ->
            let modu = co.co_module in
            let ob_name = co.co_name in
            
            (*
            ** Signal can be:
            **  1. module local (Temp. sig. req. here)
            **  2. module exported and not main module 
            **  3. module exported and main module (Temp. sig. req. here)
            *)
            if not (sym_check_obj modu.mod_import ob_name) ||
               (List.mem Mod_main modu.mod_flags) then
            begin
                (*
                ** Only temporary signals for read and write access
                ** are required.
                *)
                let sigl =
                        (if co.co_reader <> [] && not inout then
                            [sprintf "signal %s_RD: %s;" 
                                     co.co_name
                                     (obj_decl_type co.co_type)]
                        else
                            []
                        )@
                        (if co.co_writer <> [] && not inout then
                            [sprintf "signal %s_WR: %s;" 
                                     co.co_name
                                     (obj_decl_type co.co_type)]
                        else
                            [])@
                        (if inout then
                            [
                              (* THIS MUST BE A PORT SIGNAL!!! *)
                            ]
                         else [])  in
                    sigl,(if co.co_reader <> [] &&
                                  co.co_writer <> [] && 
                                  not inout then
                               [
                                sprintf "%s_RD <= %s_WR;" 
                                        co.co_name co.co_name;
                               ] else  [])
            end
            else
                [],[]
    end;
    | OT_reg co ->
    begin       
        let rec fifo l =
            match l with
            | (Obj_schedule m)::tl -> 
              m = "fifo"; 
            | _ :: tl -> fifo tl;
            | [] -> false in
        let fifo = fifo co.co_flags in

        let prd,pwr,pblocked,pall = get_procs co in

        match pro with
        | Some pro ->
            stat (sprintf "process.%s" pro.pro_name) "register";
            stat "register-local" (obj_decl_type co.co_type);
            (*
            ** Local process object
            *)
            let ob_signal = 
                    [sprintf "signal %s%s: %s;"
                        (if is_ot_local (Some pro) obj then "" else "REG_")
                        co.co_name
                        (obj_decl_type co.co_type)] in
            if not (List.mem Obj_created co.co_flags) then
            begin
                (*
                ** Create object
                *)
                co.co_flags <- co.co_flags @
                               [Obj_created];
                ob_signal,[]
            end
            else [],[]
        | None -> 
            (*
            ** Check object place.
            *)
            if sym_check_obj modu.mod_import co.co_name then
            begin
                (*
                ** Object is somewhere externally. All register
                ** signals are routed to the module port.
                *)
                [],[]
            end
            else
            begin
                (*
                ** 1. Object must be created (only one time!).
                ** 2. All temporary signals must be created for
                **    all processes sharing this object.
                *)
                stat "register-shared" (obj_decl_type co.co_type);
                let gd = co.co_guard <> None in
                let gd_num = List.length pwr in
                let ob_signal = 
                    [sprintf "signal REG_%s: %s;"
                        co.co_name
                        (obj_decl_type co.co_type)] in
                let ob_aux = 
                      (*
                      ** Auxilliary signals for guard process
                      *)
                      if gd then
                      [sprintf "signal REG_%s_LOCKED: %s;"
                        co.co_name  
                        (get_env_str "logic_type")] 
                      else [] in
                let ob_rd =
                    [sprintf "signal REG_%s_RD: %s;"
                        co.co_name
                        (obj_decl_type co.co_type)] in

                let s = ref (if not (List.mem Obj_created co.co_flags) then
                             begin
                                (*
                                ** Create object
                                *)
                                co.co_flags <- co.co_flags @
                                               [Obj_created];
                                ob_signal @ ob_aux @
                                (if co.co_reader <> [] then
                                    ob_rd else [])
                             end
                                else []) in

                  let s_sched =
                    if not fifo then [] else
                    begin
                      let n = List.length pall in
                      let w = const_width (V_int (Int64.of_int n)) in
                      let dt_prio = DT_logic w in
                      let s = ref [] in
                      List.iter (fun pro ->
                        s := !s @ [sprintf "signal REG_%s_%s_QUEUED: std_logic;"
                                   co.co_name pro.pro_name;
                           sprintf "signal REG_%s_%s_PRIO: %s;"
                                   co.co_name pro.pro_name
                                   (obj_decl_type dt_prio);
                          ];
                          ) pall;
                        s := !s @ [sprintf "signal REG_%s_HEAD: %s;"
                                 co.co_name 
                                 (obj_decl_type dt_prio);
                                sprintf "signal REG_%s_TAIL: %s;"
                                 co.co_name 
                                 (obj_decl_type dt_prio);
                                sprintf "signal REG_%s_OWNER: %s;"
                                 co.co_name 
                                 (obj_decl_type dt_prio);
                            ];
                      !s
                    end in
                List.iter (fun pro ->
                    s := !s @ 
                    [sprintf "signal REG_%s_%s_WR: %s;"
                        co.co_name
                        pro.pro_name
                        (obj_decl_type co.co_type);
                     sprintf "signal REG_%s_%s_WE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type")];
                    ) pwr;
                List.iter (fun pro ->
                     s := !s @ 
                     [sprintf "signal REG_%s_%s_GD: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ];
                    ) pblocked;
                !s @ s_sched,[]
            end;
    end;
    | OT_channel ch ->
    begin       
        let co = ch.ch_obj in
        let ao = ch.ch_ao in
        (*
        ** Determine connection model
        *)
        let rn = List.length co.co_reader in
        let wn = List.length co.co_writer in
        if rn = 1 && wn = 1 && co.co_reader = co.co_writer then
            error 0 (sprintf "Channel <%s> used only by one process <%s>!"
                             co.co_name
                             (match co.co_reader with
                              | [pro] -> pro.pro_name;
                              | _ -> "?")
                       );
        let uni = rn=1 && wn = 1 in
        let bi =
          (rn=2 && wn=2) &&
          (
            co.co_reader = co.co_writer ||
            (List.rev co.co_reader) = co.co_writer ||
            co.co_reader = (List.rev co.co_writer)
          ) in
        let model_conn = 
            if uni  then Chan_uni
            else if bi then Chan_bi
            else Chan_multi in
        (*
        ** already user specified
        *)
        let model_buf =  List.hd ch.ch_model in
        ch.ch_model <- ch.ch_model @ [model_conn];

        match pro with
        | Some pro ->
            error 0 (sprintf "Process local channel object <%s> not supported!"
                             co.co_name);
        | None -> 
        begin
          match ch.ch_ot with
          | None ->
          begin
            (*
            ** Check object place.
            *)
            if sym_check_obj modu.mod_import co.co_name then
            begin
                (*
                ** Object is somewhere externally. All register
                ** signals are routed to the module port.
                *)
                [],[]
            end
            else
            begin
                (*
                ** 1. Object must be created (only one time!).
                ** 2. All temporary signals must be created for
                **    all processes sharing this object.
                *)
                let unlock = ref [] in
                let empty = ref [] in
                let full = ref [] in
                List.iter (fun (sel,pro) ->
                    match sel with
                    | "unlock" -> unlock := !unlock @ [pro];
                    | "empty" -> empty := !empty @ [pro];
                    | "full" -> full := !full @ [pro];
                    | _ -> error 0 
                                (sprintf "Channel: unknown method <%s> found in object <%s> and process <%s>"
                                          sel ao.ao_name pro.pro_name);
                    ) ao.ao_procs;
                let pro_gds = ref co.co_writer in
                List.iter (fun pro ->
                    if not (List.mem pro !pro_gds) then
                        pro_gds := !pro_gds @ [pro];
                    ) (co.co_reader@ !unlock @ !empty @ !full);
                let gd_num = List.length !pro_gds in
                let ob_signal = 
                    if model_buf = Chan_buffered && not bi then 
                    [
                        sprintf "signal CHAN_%s: %s;"
                        co.co_name
                        (obj_decl_type co.co_type);
                    ]
                    else if model_buf = Chan_buffered && bi then
                    [
                        sprintf "signal CHAN_%s_A: %s;"
                        co.co_name
                        (obj_decl_type co.co_type); 
                        sprintf "signal CHAN_%s_B: %s;"
                        co.co_name
                        (obj_decl_type co.co_type); 
                    ]
                    else [] in
                let ob_aux = 
                      (*
                      ** Auxilliary signals for guard process
                      *)
                      if model_buf = Chan_buffered && not bi then
                      [sprintf "signal CHAN_%s_AVAIL: %s;"
                        co.co_name  
                        (get_env_str "logic_type")]
                      else
                      if model_buf = Chan_buffered && bi then
                      [
                        sprintf "signal CHAN_%s_AVAIL_A: %s;"
                        co.co_name  
                        (get_env_str "logic_type");
                        sprintf "signal CHAN_%s_AVAIL_B: %s;"
                        co.co_name  
                        (get_env_str "logic_type")
                      ]
                      else [] in
                let s = ref (if not (List.mem Obj_created co.co_flags) then
                             begin
                                (*
                                ** Create object
                                *)
                                co.co_flags <- co.co_flags @
                                               [Obj_created];
                                ob_signal @ ob_aux
                             end
                                else []) in

                List.iter (fun pro ->
                    s := !s @ 
                    [sprintf "signal CHAN_%s_%s_WR: %s;"
                        co.co_name
                        pro.pro_name
                        (obj_decl_type co.co_type);
                     sprintf "signal CHAN_%s_%s_WE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type")];
                    ) co.co_writer;
                List.iter (fun pro ->
                    s := !s @ 
                    [sprintf "signal CHAN_%s_%s_RD: %s;"
                        co.co_name
                        pro.pro_name
                        (obj_decl_type co.co_type);
                     sprintf "signal CHAN_%s_%s_RE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type")];
                    ) co.co_reader;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal CHAN_%s_%s_GD: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ] @
                     (if bi then
                     [
                        sprintf "signal CHAN_%s_%s_GD_A: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                        sprintf "signal CHAN_%s_%s_GD_B: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ]                      else [])
                    ) !pro_gds;
                !s,[]
            end;
          end;
          | Some (OT_struct st) ->
          begin
            (*
            ** Check object place.
            *)
            if sym_check_obj modu.mod_import co.co_name then
            begin
                (*
                ** Object is somewhere externally. All register
                ** signals are routed to the module port.
                *)
                [],[]
            end
            else
            begin
                (*
                ** 1. Object must be created (only one time!).
                ** 2. All temporary signals must be created for
                **    all processes sharing this object.
                *)
                let pro_gds = ref co.co_writer in
                List.iter (fun pro ->
                    if not (List.mem pro !pro_gds) then
                        pro_gds := !pro_gds @ [pro];
                    ) co.co_reader;
                let gd_num = List.length !pro_gds in
                let s = ref [] in
                List.iter (fun ot ->
                  match co_of_ot ot with
                  | Some co ->
                    if model_buf = Chan_buffered && not bi then 
                      s := !s @ [
                        sprintf "signal CHAN_%s: %s;"
                        co.co_name
                        (obj_decl_type co.co_type);
                      ]
                    else if model_buf = Chan_buffered && bi then
                      s := !s @ [
                        sprintf "signal CHAN_%s_A: %s;"
                        co.co_name
                        (obj_decl_type co.co_type); 
                        sprintf "signal CHAN_%s_B: %s;"
                        co.co_name
                        (obj_decl_type co.co_type); 
                      ];
                  | None -> error 345042 "" ) st.st_objs;

                let ob_signal = !s in
                let ob_aux = 
                      (*
                      ** Auxilliary signals for guard process
                      *)
                      if model_buf = Chan_buffered && not bi then
                      [sprintf "signal CHAN_%s_AVAIL: %s;"
                        co.co_name  
                        (get_env_str "logic_type")]
                      else
                      if model_buf = Chan_buffered && bi then
                      [
                        sprintf "signal CHAN_%s_AVAIL_A: %s;"
                        co.co_name  
                        (get_env_str "logic_type");
                        sprintf "signal CHAN_%s_AVAIL_B: %s;"
                        co.co_name  
                        (get_env_str "logic_type")
                      ]
                      else [] in
                let s = ref (if not (List.mem Obj_created co.co_flags) then
                             begin
                                (*
                                ** Create object
                                *)
                                co.co_flags <- co.co_flags @
                                               [Obj_created];
                                ob_signal @ ob_aux
                             end
                                else []) in

                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal CHAN_%s_%s_WE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type")];
                    ) co.co_writer;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal CHAN_%s_%s_RE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type")];
                    ) co.co_reader;
                List.iter (fun pro ->
                 List.iter (fun ot ->
                  match co_of_ot ot with
                  | Some co ->
                    s := !s @ 
                    [sprintf "signal CHAN_%s_%s_WR: %s;"
                        co.co_name
                        pro.pro_name
                        (obj_decl_type co.co_type)]
                  | None -> error 596712 "";
                  ) st.st_objs) co.co_writer;
                List.iter (fun pro ->
                 List.iter (fun ot ->
                  match co_of_ot ot with
                  | Some co ->
                    s := !s @ 
                    [sprintf "signal CHAN_%s_%s_RD: %s;"
                        co.co_name
                        pro.pro_name
                        (obj_decl_type co.co_type)];
                  | None -> error 596713 "";
                  ) st.st_objs) co.co_reader;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal CHAN_%s_%s_GD: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ] @
                     (if bi then
                     [
                        sprintf "signal CHAN_%s_%s_GD_A: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                        sprintf "signal CHAN_%s_%s_GD_B: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ]                      else [])
                    ) !pro_gds;
                !s,[]
            end;
          end;
          | _ -> error 588169 "";
        end;
    end;
    | OT_queue qu ->
    begin       
        let co = qu.qu_obj in
        let ao = qu.qu_ao in
        (*
        ** Determine connection model
        *)
        let rn = List.length co.co_reader in
        let wn = List.length co.co_writer in
        if rn = 1 && wn = 1 && co.co_reader = co.co_writer then
            error 0 (sprintf "Queue <%s> used only by one process <%s>!"
                             co.co_name
                             (match co.co_reader with
                              | [pro] -> pro.pro_name;
                              | _ -> "?")
                       );

        match pro with
        | Some pro ->
            error 0 (sprintf "Process local queue object <%s> not supported!"
                             co.co_name);
        | None -> 
        begin
          match qu.qu_ot with
          | None ->
          begin
            (*
            ** already user specified
            *)
            let width = size_of_dt co.co_type in
            let depth = qu.qu_depth in
            let depth' = sel_width depth in

            (*
            ** Check object place.
            *)
            if sym_check_obj modu.mod_import co.co_name then
            begin
                (*
                ** Object is somewhere externally. All register
                ** signals are routed to the module port.
                *)
                [],[]
            end
            else
            begin
                (*
                ** 1. Object must be created (only one time!).
                ** 2. All temporary signals must be created for
                **    all processes sharing this object.
                *)
                let unlock = ref [] in
                let empty = ref [] in
                let full = ref [] in
                List.iter (fun (sel,pro) ->
                    match sel with
                    | "unlock" -> unlock := !unlock @ [pro];
                    | "empty" -> empty := !empty @ [pro];
                    | "full" -> full := !full @ [pro];
                    | _ -> error 0 
                                (sprintf "Queue: unknown method <%s> found in object <%s> and process <%s>"
                                          sel ao.ao_name pro.pro_name);
                    ) ao.ao_procs;
                let pro_gds = ref co.co_writer in
                List.iter (fun pro ->
                    if not (List.mem pro !pro_gds) then
                        pro_gds := !pro_gds @ [pro];
                    ) (co.co_reader@ !unlock @ !empty @ !full);
                let gd_num = List.length !pro_gds in
                let s = ref
                  [
                    sprintf "type QRAM_%s_type is array(%d to %d)"
                         co.co_name 0 (max 1 (depth-1));
                    sprintf "  of std_logic_vector(%d downto %d);"
                        (width-1) 0;
                    sprintf "signal QRAM_%s: QRAM_%s_type;"
                        co.co_name co.co_name;
                    sprintf "signal QUEUE_%s_EMPTY: std_logic;" co.co_name;
                    sprintf "signal QUEUE_%s_FULL: std_logic;" co.co_name;
                    sprintf "signal QUEUE_%s_LOCKED: std_logic;" co.co_name;
                    sprintf "signal QUEUE_%s_ADDR_RD: %s;" 
                        co.co_name (obj_decl_type (DT_logic depth'));
                    sprintf "signal QUEUE_%s_ADDR_WR: %s;" 
                        co.co_name (obj_decl_type (DT_logic depth'));
                    sprintf "signal QRAM_%s_ADDR_AUX: %s;" 
                        co.co_name (obj_decl_type (DT_logic depth'));
                    sprintf "signal QRAM_%s_RD_ADDR: %s;" 
                        co.co_name (obj_decl_type (DT_logic depth'));
                    sprintf "signal QRAM_%s_DIN_AUX: %s;" 
                        co.co_name (obj_decl_type (DT_logic width));
                    sprintf "signal QRAM_%s_DOUT_AUX: %s;" 
                        co.co_name (obj_decl_type (DT_logic width));
                    sprintf "signal QRAM_%s_WE_AUX: %s;" 
                        co.co_name (obj_decl_type (DT_logic 1));
                  ] in

                List.iter (fun pro ->
                    s := !s @ 
                    [sprintf "signal QUEUE_%s_%s_WR: %s;"
                        co.co_name
                        pro.pro_name
                        (obj_decl_type co.co_type);
                     sprintf "signal QUEUE_%s_%s_WE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type")];
                    ) co.co_writer;
                List.iter (fun pro ->
                    s := !s @ 
                    [sprintf "signal QUEUE_%s_%s_RD: %s;"
                        co.co_name
                        pro.pro_name
                        (obj_decl_type co.co_type);
                     sprintf "signal QUEUE_%s_%s_RE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type")];
                    ) co.co_reader;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal QUEUE_%s_%s_GD: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ];
                    ) !pro_gds;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal QUEUE_%s_%s_UNLOCK: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ];                    
                    ) !unlock;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal QUEUE_%s_%s_EMPTY_RE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                      sprintf "signal QUEUE_%s_%s_EMPTY_RD: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");                     ];                    
                    ) !empty;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal QUEUE_%s_%s_FULL_RE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                      sprintf "signal QUEUE_%s_%s_FULL_RD: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");                     ];                    
                    ) !full;
                !s,[]
            end;
          end;
          | Some (OT_struct st) ->
          begin
            (*
            ** Check object place.
            *)
            if sym_check_obj modu.mod_import co.co_name then
            begin
                (*
                ** Object is somewhere externally. All register
                ** signals are routed to the module port.
                *)
                [],[]
            end
            else
            begin
                (*
                ** OT_queue of structure type.
                ** 1. Object must be created (only one time!).
                ** 2. All temporary signals must be created for
                **    all processes sharing this object.
                *)
                (*
                ** already user specified
                *)
                let depth = qu.qu_depth in
                let depth' = sel_width depth in

                let unlock = ref [] in
                let empty = ref [] in
                let full = ref [] in
                List.iter (fun (sel,pro) ->
                    match sel with
                    | "unlock" -> unlock := !unlock @ [pro];
                    | "empty" -> empty := !empty @ [pro];
                    | "full" -> full := !full @ [pro];
                    | _ -> error 0 
                                (sprintf "Queue: unknown method <%s> found in object <%s> and process <%s>"
                                          sel ao.ao_name pro.pro_name);
                    ) ao.ao_procs;
                let pro_gds = ref co.co_writer in
                List.iter (fun pro ->
                    if not (List.mem pro !pro_gds) then
                        pro_gds := !pro_gds @ [pro];
                    ) co.co_reader;
                let gd_num = List.length !pro_gds in
                let s = ref [] in
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->
                        let width = size_of_dt co.co_type in
                        s := !s @ [
                            sprintf "type QRAM_%s_type is array(%d to %d)"
                                 co.co_name 0 (max 1 (depth-1));
                            sprintf "  of std_logic_vector(%d downto %d);"
                                (width-1) 0;
                            sprintf "signal QRAM_%s: QRAM_%s_type;"
                                co.co_name co.co_name;
                            sprintf "signal QRAM_%s_DIN_AUX: %s;" 
                                co.co_name (obj_decl_type (DT_logic width));
                            sprintf "signal QRAM_%s_DOUT_AUX: %s;" 
                                co.co_name (obj_decl_type (DT_logic width));
                            sprintf "signal QRAM_%s_ADDR_AUX: %s;" 
                                co.co_name (obj_decl_type (DT_logic depth'));
                            sprintf "signal QRAM_%s_RD_ADDR: %s;" 
                                co.co_name (obj_decl_type (DT_logic depth'));
                            ];
                    | None -> error 680332 "" ;
                    ) st.st_objs;
                s := !s @ 
                  [
                    sprintf "signal QUEUE_%s_EMPTY: std_logic;" co.co_name;
                    sprintf "signal QUEUE_%s_FULL: std_logic;" co.co_name;
                    sprintf "signal QUEUE_%s_LOCKED: std_logic;" co.co_name;
                    sprintf "signal QUEUE_%s_ADDR_RD: %s;" 
                        co.co_name (obj_decl_type (DT_logic depth'));
                    sprintf "signal QUEUE_%s_ADDR_WR: %s;" 
                        co.co_name (obj_decl_type (DT_logic depth'));
                    sprintf "signal QRAM_%s_ADDR_AUX: %s;" 
                        co.co_name (obj_decl_type (DT_logic depth'));
                    sprintf "signal QRAM_%s_WE_AUX: %s;" 
                        co.co_name (obj_decl_type (DT_logic 1));
                  ];

                List.iter (fun pro ->
                    s := !s @ 
                        (List.map (fun ot ->
                          match co_of_ot ot with
                          | Some co ->
                            sprintf "signal QUEUE_%s_%s_WR: %s;"
                                co.co_name
                                pro.pro_name
                                (obj_decl_type co.co_type);
                          | None -> error 229448 "";
                          ) st.st_objs) @ 
                        [sprintf "signal QUEUE_%s_%s_WE: %s;"
                            co.co_name
                            pro.pro_name 
                            (get_env_str "logic_type")];
                    ) co.co_writer;
                List.iter (fun pro ->
                    s := !s @ 
                        (List.map (fun ot ->
                          match co_of_ot ot with
                          | Some co ->
                            sprintf "signal QUEUE_%s_%s_RD: %s;"
                                co.co_name
                                pro.pro_name
                                (obj_decl_type co.co_type);
                          | None -> error 875438 "";
                          ) st.st_objs) @ 
                        [sprintf "signal QUEUE_%s_%s_RE: %s;"
                            co.co_name
                            pro.pro_name 
                            (get_env_str "logic_type")];
                    ) co.co_reader;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal QUEUE_%s_%s_GD: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ];
                    ) !pro_gds;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal QUEUE_%s_%s_UNLOCK: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ];                    
                    ) !unlock;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal QUEUE_%s_%s_EMPTY_RE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                      sprintf "signal QUEUE_%s_%s_EMPTY_RD: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");                     ];                    
                    ) !empty;
                List.iter (fun pro ->
                    s := !s @ 
                     [sprintf "signal QUEUE_%s_%s_FULL_RE: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                      sprintf "signal QUEUE_%s_%s_FULL_RD: %s;"
                        co.co_name
                        pro.pro_name 
                        (get_env_str "logic_type");                     ];                    
                    ) !full;
                !s,[]
            end;
          end;
          | _ -> error 85642 "";
        end;
    end;
    | OT_array at ->
    if not (List.mem AT_temp at.at_flags) then
    begin
    match pro with
    | None ->
      let is_block = List.mem AT_block at.at_flags in
      let is_dyn = List.mem AT_dyn at.at_flags in
      let size = proda at.at_dim in

      if not is_dyn && not is_block then
      begin
        let s = ref [] in
        let t = ref [] in
        Array.iter (fun ot ->
                    let co =
                        match co_of_ot ot with
                        | Some co -> co;
                        | None -> error 594174 ""; in
                    let ex = sym_check_sym modu.mod_export (Sym_obj ot) in                    
                    let rd = co.co_reader <> [] in
                    let wr = co.co_writer <> [] in
                    let sym = Sym_obj ot in
                    if rd or wr then 
                    begin
                        let s',t' = obj_decl sym modu pro in
                        s := !s @ s';
                        t := !t @ t';
                    end;
            ) at.at_objs;
            !s,!t
      end
      else  if not is_block then
      begin
        (*
        ** Non block array was changed to
        ** dynamic selection mode.
        ** Now all processes access all
        ** array objects. Thus co_reader and
        ** co_writer must be updated for each object.
        *)
        let prd = ref [] in
        let pwr = ref [] in
        let punlock = ref [] in
        
        (*
        ** Get all reader and writer processes...
        *)
        Array.iter (fun ot -> 
            let co =
              match co_of_ot ot with
              | Some co -> co
              | None -> error 0 (sprintf "OT <%s> without <CO>." (name_of_ot ot)) in
              
            List.iter (fun pro ->
                    if not (List.mem pro !prd) then
                        prd := !prd @ [pro];
                    ) co.co_reader;
            List.iter (fun pro ->
                    if not (List.mem pro !pwr) then
                        pwr := !pwr @ [pro];
                    ) co.co_writer;
            match ot with
            | OT_channel ch -> 
              let co = ch.ch_obj in
              let ao = ch.ch_ao in
              
              List.iter (fun (sel,pro) ->
                match sel with
                | "unlock" -> 
                    if not (List.mem pro !punlock) then
                        punlock := !punlock @ [pro];
                | _ -> ()) ao.ao_procs; 
            | OT_queue qu -> 
              let co = qu.qu_obj in
              let ao = qu.qu_ao in
              
              List.iter (fun (sel,pro) ->
                match sel with
                | "unlock" -> 
                    if not (List.mem pro !punlock) then
                        punlock := !punlock @ [pro];
                | _ -> ()) ao.ao_procs; 
            | _ -> ();
            )  at.at_objs;
        (*
        ** Update co_reader and co_writer
        ** for all array objects
        *)
        Array.iter (fun ot -> 
                let co = 
                    match co_of_ot ot with
                    | Some co -> co;
                    | None -> error 709867 ""; in
                List.iter (fun pro ->
                    if not (List.mem pro co.co_reader) then
                        co.co_reader <- co.co_reader @ [pro];
                    ) !prd;
                List.iter (fun pro ->
                    if not (List.mem pro co.co_writer) then
                        co.co_writer <- co.co_writer @ [pro];
                    ) !pwr;
            )  at.at_objs;
        
        match at.at_objs.(0) with
        | OT_reg co ->
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let sel_size = proda at.at_dim in
                let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
                 let gd = ref [] in
                let sel = ref [] in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709868 "" in
                List.iter (fun pro ->
                    if not (List.mem pro !sel) then
                        sel := !sel @ [pro];
                    ) (!prd @ !pwr);
                let rd,wr,gd,sel = !prd, !pwr, !pwr, !sel in
                let rds,wrs,gds,sels =
                   ref [],
                   ref [],
                   ref [],
                   ref [] in
                let gd_rd,gd_wr = array_guard at in
                List.iter (fun pro ->
                     rds := !rds @ 
                         [
                            sprintf "signal ARRAY_%s_%s_RD: %s;" 
                                    at.at_name pro.pro_name
                                    (obj_decl_type dt);
                         ]
                     ) rd;
                List.iter (fun pro ->
                     wrs := !wrs @ 
                            [sprintf "signal ARRAY_%s_%s_WR: %s;" 
                                     at.at_name pro.pro_name
                                     (obj_decl_type dt);
                             sprintf "signal ARRAY_%s_%s_WE: %s;"
                                     at.at_name pro.pro_name
                                     (get_env_str "logic_type");
                             sprintf "signal ARRAY_%s_%s_WEA: %s;"
                                     at.at_name pro.pro_name
                                     (obj_decl_type (DT_logic size));
                            ]
                     ) wr;
                if gd_wr then
                  List.iter (fun pro ->
                     gds := !gds @ 
                            [sprintf "signal ARRAY_%s_%s_GD: %s;"
                                     at.at_name pro.pro_name
                                     (get_env_str "logic_type");
                             sprintf "signal ARRAY_%s_%s_GDA: %s;"
                                     at.at_name pro.pro_name
                                     (obj_decl_type (DT_logic size));
                            ]
                     ) gd;
                if !gds <> [] then
                    gds := !gds @ 
                            [
                                sprintf "signal ARRAY_%s_LOCKED: %s;"
                                at.at_name
                                (obj_decl_type (DT_logic size));
                            ];
                List.iter (fun pro ->
                      sels := !sels @ [
                            sprintf "signal ARRAY_%s_%s_SEL: %s;"
                                    at.at_name pro.pro_name
                                    (obj_decl_type (DT_logic sel_width)); 
                      ] 
                    ) sel;
                 let sigs =
                    [
                        sprintf "type ARRAY_%s_TYPE is array(0 to %d)"
                                at.at_name 
                                (size-1);
                        sprintf "  of %s;"
                                (obj_decl_type co.co_type);
                        sprintf "signal ARRAY_%s: ARRAY_%s_TYPE;"
                                at.at_name at.at_name;
                    ] in
                sigs @ !rds @ !wrs @ !gds @ !sels,[]

        | OT_channel ch ->
                let co = ch.ch_obj in

                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let sel_size = proda at.at_dim in
                let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
                let gd = ref [] in
                let sel = ref [] in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709868 "" in
                List.iter (fun pro ->
                    if not (List.mem pro !sel) then
                        sel := !sel @ [pro];
                    ) (!prd @ !pwr);
                let prd,pwr,pgd,psel = !prd, !pwr, !sel, !sel in
                let rds,wrs,gds,sels,unlock =
                   ref [],
                   ref [],
                   ref [],
                   ref [],
                   ref [] in
                let gd_rd,gd_wr = array_guard at in
                let gd = gd_rd || gd_wr in
                List.iter (fun pro ->
                     rds := !rds @ 
                         [
                            sprintf "signal CHAN_%s_%s_RD: %s;" 
                                    at.at_name pro.pro_name
                                    (obj_decl_type dt);
                             sprintf "signal CHAN_%s_%s_RE: %s;"
                                     at.at_name pro.pro_name
                                     (get_env_str "logic_type");
                         ]
                     ) prd;
                List.iter (fun pro ->
                     wrs := !wrs @ 
                            [sprintf "signal CHAN_%s_%s_WR: %s;" 
                                     at.at_name pro.pro_name
                                     (obj_decl_type dt);
                             sprintf "signal CHAN_%s_%s_WE: %s;"
                                     at.at_name pro.pro_name
                                     (get_env_str "logic_type");
                            ]
                     ) pwr;
                if gd then
                  List.iter (fun pro ->
                     gds := !gds @ 
                            [sprintf "signal CHAN_%s_%s_GD: %s;"
                                     at.at_name pro.pro_name
                                     (get_env_str "logic_type");
                            ]
                     ) pgd;
                List.iter (fun pro ->
                      sels := !sels @ [
                            sprintf "signal CHAN_%s_%s_SEL: %s;"
                                    at.at_name pro.pro_name
                                    (obj_decl_type (DT_logic sel_width)); 
                      ] 
                    ) psel;
               List.iter (fun pro ->
                    unlock := !unlock @ 
                     [sprintf "signal CHAN_%s_%s_UNLOCK: %s;"
                        at.at_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ];                    
                    ) !punlock;
                let sl,al = ref [], ref [] in
                Array.iter (fun obj' ->
                    let sl',al' = obj_decl (Sym_obj obj') modu pro in
                    sl := !sl @ sl';
                    al := !al @ al';
                    ) at.at_objs;
                !rds @ !wrs @ !gds @ !sels @ !unlock @ !sl, !al
        | OT_queue qu ->
                let co = qu.qu_obj in

                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let sel_size = proda at.at_dim in
                let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
                let gd = ref [] in
                let sel = ref [] in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709878 "" in
                List.iter (fun pro ->
                    if not (List.mem pro !sel) then
                        sel := !sel @ [pro];
                    ) (!prd @ !pwr @ !punlock);
                  
                let prd,pwr,pgd,psel = !prd, !pwr, !sel, !sel in
                let rds,wrs,gds,sels,unlock =
                   ref [],
                   ref [],
                   ref [],
                   ref [],
                   ref [] in
                let gd_rd,gd_wr = array_guard at in
                let gd = gd_rd || gd_wr in
                List.iter (fun pro ->
                     rds := !rds @ 
                         [
                            sprintf "signal QUEUE_%s_%s_RD: %s;" 
                                    at.at_name pro.pro_name
                                    (obj_decl_type dt);
                             sprintf "signal QUEUE_%s_%s_RE: %s;"
                                     at.at_name pro.pro_name
                                     (get_env_str "logic_type");
                         ]
                     ) prd;
                List.iter (fun pro ->
                     wrs := !wrs @ 
                            [sprintf "signal QUEUE_%s_%s_WR: %s;" 
                                     at.at_name pro.pro_name
                                     (obj_decl_type dt);
                             sprintf "signal QUEUE_%s_%s_WE: %s;"
                                     at.at_name pro.pro_name
                                     (get_env_str "logic_type");
                            ]
                     ) pwr;
                if gd then
                  List.iter (fun pro ->
                     gds := !gds @ 
                            [sprintf "signal QUEUE_%s_%s_GD: %s;"
                                     at.at_name pro.pro_name
                                     (get_env_str "logic_type");
                            ]
                     ) pgd;
                List.iter (fun pro ->
                      sels := !sels @ [
                            sprintf "signal QUEUE_%s_%s_SEL: %s;"
                                    at.at_name pro.pro_name
                                    (obj_decl_type (DT_logic sel_width)); 
                      ] 
                    ) psel;
               List.iter (fun pro ->
                    unlock := !unlock @ 
                     [sprintf "signal QUEUE_%s_%s_UNLOCK: %s;"
                        at.at_name
                        pro.pro_name 
                        (get_env_str "logic_type");
                     ];                    
                    ) !punlock;
                let sl,al = ref [], ref [] in
                Array.iter (fun obj' ->
                    let sl',al' = obj_decl (Sym_obj obj') modu pro in
                    sl := !sl @ sl';
                    al := !al @ al';
                    ) at.at_objs;
                !rds @ !wrs @ !gds @ !sels @ !unlock @ !sl, !al
        | _ -> error 237268 "";     
      end
      else [],[];
    | Some pro ->
      let is_block = List.mem AT_block at.at_flags in
      let is_dyn = List.mem AT_dyn at.at_flags in
      let size = proda at.at_dim in

      if not is_dyn && not is_block then
      begin
        let s = ref [] in
        let t = ref [] in
        Array.iter (fun ot ->
                    let co =
                        match co_of_ot ot with
                        | Some co -> co;
                        | None -> error 594174 ""; in
                    let rd = co.co_reader <> [] in
                    let wr = co.co_writer <> [] in
                    let sym = Sym_obj ot in
                    if rd && wr then 
                    begin
                        let s',t' = obj_decl sym modu (Some pro) in
                        s := !s @ s';
                        t := !t @ t';
                    end
                    else if not rd && not wr then
                      out (sprintf "Warning: Removed array element <%s> due missing read and write access!"
                                   co.co_name)
                    else if rd then
                      error 0 (sprintf "Found array element <%s> with read but no write access!"
                                   co.co_name);
            ) at.at_objs;
            !s,!t
      end
      else  if not is_block then
      begin
        match at.at_objs.(0) with
        | OT_reg co ->
                let local = List.mem Obj_local co.co_flags in
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let sel_size = proda at.at_dim in
                let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
                let rd = ref [] in
                let wr = ref [] in
                let gd = ref [] in
                let sel = ref [] in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709869 "" in
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                if not rd || not wr then
                  error 0 (sprintf "Local array <%s> with missing read and/or write access found!"
                                   at.at_name);

                let rds =
                         [
                            sprintf "signal ARRAY_%s_RD: %s;" 
                                    at.at_name 
                                    (obj_decl_type dt);
                         ]
                     in
                let wrs = 
                    if not local then
                            [sprintf "signal ARRAY_%s_WR: %s;" 
                                     at.at_name
                                     (obj_decl_type dt);
                             sprintf "signal ARRAY_%s_WE: %s;"
                                     at.at_name 
                                     (get_env_str "logic_type");
                             sprintf "signal ARRAY_%s_WEA: %s;"
                                     at.at_name 
                                     (obj_decl_type (DT_logic size));
                            ]
                    else []
                     in
                let sels =
                            [sprintf "signal ARRAY_%s_SEL: %s;"
                                    at.at_name 
                                    (obj_decl_type (DT_logic sel_width)); 
                            ] 
                      in
                let sigs =
                    [
                        sprintf "type ARRAY_%s_TYPE is array(0 to %d)"
                                at.at_name 
                                (size-1);
                        sprintf "  of %s;"
                                (obj_decl_type co.co_type);
                        sprintf "signal ARRAY_%s: ARRAY_%s_TYPE;"
                                at.at_name at.at_name;
                    ] in
                sigs @ rds @ wrs @ sels,[]
        | _ -> error 237268 "";     
      end
      else [],[];
    end else [],[];
    | OT_array_sel (at,sel) -> [],[]; 
    | OT_struct st -> [],[];
    | OT_component st -> 
    if not (sym_check_obj modu.mod_export st.st_name) &&
       not (List.mem (modu.mod_name,st.st_type.ts_name) !modu_comps) then
    begin
        modu_comps := !modu_comps @ [modu.mod_name,st.st_type.ts_name];
        (*
        ** Create component port - maybe already created by another
        *+ instantiation!
        *)
        
        let comp = ref [] in
        let aux = ref [] in
        let top = ref [] in
        let st_type = st.st_type in
        comp := !comp @ [sprintf "component %s" st_type.ts_name];
        comp := !comp @ ["port("];
        let i = ref (List.length st_type.ts_elems) in
        List.iter (fun te ->
            decr i;
            comp := !comp @ [sprintf "  %s : %s %s%s"
                    te.te_name 
                    (
                      match te.te_port with
                      | Some tp ->
                        let str = 
                            match tp with
                            | PT_in -> "in";
                            | PT_out -> "out";
                            | PT_bus -> "inout"; in
                        str
                      | None -> error 0 
                                (sprintf "\nComponent <%s.%s> with unspecified port!"
                                st.st_name te.te_name);
                            
                    )
                    (obj_decl_type (let dt,_=te.te_type in dt))
                    (if !i = 0 then "" else ";")];
            ) st_type.ts_elems;
        comp := !comp @ [");";"end component;"];
        !comp @ !aux, !top
    end else [],[];
    | OT_var co -> [],[];  (* handled by Sym_block *)
    | OT_named_value _ 
    | OT_const _ -> [],[]; (* ignored *)
    | _ -> 
        error 548650 "";
  end;
  | Sym_block db ->
  begin

    let async = List.mem Mp_readasync db.db_params in
    let inline = List.mem Mp_inline db.db_params in
    let singleport = List.mem Mp_singleport db.db_params in
    let dualport = List.mem Mp_dualport db.db_params in
    let multiport = List.mem Mp_multiport db.db_params in
    let addr_width = const_width (V_int (i64(max 2 (db.db_size-1)))) in


    let prd,pwr,pgd = (ref []),(ref []),(ref []) in
    List.iter (fun obj ->
            let add_rd pl = 
                List.iter (fun pro ->
                    if not (List.mem pro !prd) then prd := pro :: !prd;
                    ) pl;
                in
            let add_wr pl = 
                List.iter (fun pro ->
                    if not (List.mem pro !pwr) then pwr := pro :: !pwr;
                    ) pl;
                in
            let rec get_obj obj =
                match obj with
                | OT_var co -> 
                    add_rd co.co_reader;
                    add_wr co.co_writer;
                | OT_array at -> 
                  if Array.length at.at_objs = 1 then
                    get_obj at.at_objs.(0)
                  else
                  begin
                    Array.iter get_obj at.at_objs;
                  end;
                | _ -> error 431198 "";
                in
            get_obj obj;
          ) db.db_objs;
    List.iter (fun pro ->
            if not (List.mem pro !pgd) then pgd := pro :: !pgd;
            ) !prd;
    List.iter (fun pro ->
            if not (List.mem pro !pgd) then pgd := pro :: !pgd;
            ) !pwr;
    

    (*
    ** Configure, update and compile EMI Ram.ram.obj NOW.
    ** It's the child of this object.
    ** EMI Ram.ram.obj is requied for obj_decl and obj_code output.
    *)
    let db_rules = get_some db.db_rules in
    let emi = get_some db_rules.rl_child in
    List.iter (fun pro ->
      __(emi.rl_interp (sprintf "access read %s" pro.pro_name)); 
      ) !prd;
    List.iter (fun pro ->
      __(emi.rl_interp (sprintf "access write %s" pro.pro_name)); 
      ) !pwr;
    __(emi.rl_interp (sprintf "set datawidth %d" db.db_width));
    __(emi.rl_interp (sprintf "set addrwidth %d" (addr_width-1)));
    __(emi.rl_interp "compile");

    match pro with
    | Some pro ->
      (*
      ** Local process object
      *)
      if sym_check_sym pro.pro_import sym then
      begin
        (*
        ** Object is somewhere externally. All variable
        ** signals are routed to the module port.
        *)
        [],[]
      end
      else
      begin
        (*
        ** RAM is always of type DT_logic!
        *)
        let array = 
          [
            sprintf "type RAM_%s_TYPE is array(%d to %d)"
                    db.db_name 0 (max 1 (db.db_size-1));
            sprintf "  of %s;" (obj_decl_type (DT_logic db.db_width));
          ] in
        let signal = 
          if not inline then
          (
            [
            sprintf "signal RAM_%s: RAM_%s_TYPE;"
                    db.db_name db.db_name;
            ]
          ) @ (
            [
                sprintf "signal RAM_%s_WE_AUX: std_logic;"
                        db.db_name;
                sprintf "signal RAM_%s_DIN_AUX: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
            ] 
         ) @ 
          (
            [
                sprintf "signal RAM_%s_DOUT_AUX: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
                sprintf "signal RAM_%s_RD_ADDR: std_logic_vector(%d downto %d);"
                            db.db_name
                            (addr_width-1)
                            0;
            ]
          )@
          (
            [
                sprintf "signal RAM_%s_ADDR_AUX: std_logic_vector(%d downto %d);"
                            db.db_name
                            (addr_width-1)
                            0;
            ]

          )
          else
          [
            sprintf "signal RAM_%s: RAM_%s_TYPE;"
                    db.db_name db.db_name;
          ] 
          in
        (*
        ** Create process reader and writer lists
        *)
        let prd,pwr,pgd = (ref []),(ref []),(ref []) in
        List.iter (fun obj ->
            let add_rd pl = 
                List.iter (fun pro ->
                    if not (List.mem pro !prd) then prd := pro :: !prd;
                    ) pl;
                in
            let add_wr pl = 
                List.iter (fun pro ->
                    if not (List.mem pro !pwr) then pwr := pro :: !pwr;
                    ) pl;
                in
            let rec get_obj obj =
                match obj with
                | OT_var co -> 
                    add_rd co.co_reader;
                    add_wr co.co_writer;
                | OT_array at -> get_obj at.at_objs.(0);
                | _ -> error 676816 "";
                in
            get_obj obj;
          ) db.db_objs;
        List.iter (fun pro ->
            if not (List.mem pro !pgd) then pgd := pro :: !pgd;
            ) !prd;
        List.iter (fun pro ->
            if not (List.mem pro !pgd) then pgd := pro :: !pgd;
            ) !pwr;
        
        let ram_rd =
          [
                sprintf "signal RAM_%s_DOUT: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
          ] @ 
          [
                sprintf "signal RAM_%s_RE: std_logic;"
                        db.db_name;
          ]
          in
        let ram_wr =
          [
            sprintf "signal RAM_%s_DIN: %s;"
                    db.db_name (obj_decl_type (DT_logic db.db_width));
          ] @ [
            sprintf "signal RAM_%s_WE: std_logic;"
                    db.db_name;
          ]
          in
        let ram_addr =
          [
            sprintf "signal RAM_%s_ADDR: std_logic_vector(%d downto %d);"
                            db.db_name
                            ((const_width 
                                (V_int (i64 (max 2 (db.db_size-1)))))
                             -1) 
                            0;
           ] in

        let ram_gd =
          [
            sprintf "signal RAM_%s_GD: std_logic;"
                    db.db_name;
          ] in

        let ram_lock =
            [   
                sprintf "signal RAM_%s_LOCKED: std_logic;"
                        db.db_name
            ]
            in
        (* array @ signal @ ram_addr @ ram_rd @ ram_wr @ ram_gd @ ram_lock,[] *)
        emi.rl_obj_decl (Sym_obj (ao_of_sym modu sym)) modu proo
      end;
    | None ->
      (*
      ** Check object place.
      *)
      if sym_check_sym modu.mod_import sym then
      begin
        (*
        ** Object is somewhere externally. All variable
        ** signals are routed to the module port.
        *)
        [],[]
      end
      else
      begin
        (*
        ** RAM is always of type DT_logic!
        *)
        let array = 
          [
            sprintf "type RAM_%s_TYPE is array(%d to %d)"
                    db.db_name 0 (max 1 (db.db_size-1));
            sprintf "  of %s;" (obj_decl_type (DT_logic db.db_width));
          ] in
        let signal = 
          if not inline then
          (
            if singleport || multiport then
            [
            sprintf "signal RAM_%s: RAM_%s_TYPE;"
                    db.db_name db.db_name;
            ]
            else
            [
            sprintf "shared variable RAM_%s: RAM_%s_TYPE;"
                    db.db_name db.db_name;
            ]
          ) @ (
            if singleport  then
            [
                sprintf "signal RAM_%s_WE_AUX: std_logic;"
                        db.db_name;
                sprintf "signal RAM_%s_DIN_AUX: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
            ] 
            else if dualport then
            [
                sprintf "signal RAM_%s_WE_AUX_A: std_logic;"
                        db.db_name;
                sprintf "signal RAM_%s_WE_AUX_B: std_logic;"
                        db.db_name;
                sprintf "signal RAM_%s_DIN_AUX_A: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
                sprintf "signal RAM_%s_DIN_AUX_B: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
            ] 
            else
            [
                sprintf "signal RAM_%s_WE_AUX_WR: std_logic;"
                        db.db_name;
                sprintf "signal RAM_%s_DIN_AUX_WR: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
            ] 
          ) @ 
          (
            if singleport then
            [
                sprintf "signal RAM_%s_DOUT_AUX: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
                sprintf "signal RAM_%s_RD_ADDR: std_logic_vector(%d downto %d);"
                            db.db_name
                            (addr_width-1)
                            0;
            ]
            else if dualport then
            [
                sprintf "signal RAM_%s_DOUT_AUX_A: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
                 sprintf "signal RAM_%s_DOUT_AUX_B: %s;"
                        db.db_name (obj_decl_type (DT_logic db.db_width));
            ]
            else
            begin
              let n = ref 0 in
            
              let l1 = List.map (fun pro ->
                let str = 
                    sprintf "signal RAM_%s_DOUT_AUX_P%d: %s;"
                        db.db_name !n (obj_decl_type (DT_logic db.db_width));
                         in
                incr n;
                str  
                ) !prd in
              let n = ref 0 in
              let l2 = List.map (fun pro ->
                let str = 
                    sprintf "signal RAM_%s_RD_ADDR_P%d: std_logic_vector(%d downto %d);"
                            db.db_name !n
                            (addr_width-1)
                            0 in
                incr n;
                str  
                ) !prd in
               l1 @ l2
            end;
          )@
          (
            if singleport then
            [
                sprintf "signal RAM_%s_ADDR_AUX: std_logic_vector(%d downto %d);"
                            db.db_name
                            (addr_width-1)
                            0;
            ]
            else if dualport then
            [
                sprintf "signal RAM_%s_ADDR_AUX_A: std_logic_vector(%d downto %d);"
                            db.db_name
                            (addr_width-1)
                            0;
                sprintf "signal RAM_%s_ADDR_AUX_B: std_logic_vector(%d downto %d);"
                            db.db_name
                            (addr_width-1)
                            0;
            ]
            else
            begin
              let n = ref 0 in
              List.map (fun pro ->
                let str = 
                    sprintf "signal RAM_%s_ADDR_AUX_P%d: std_logic_vector(%d downto %d);"
                        db.db_name !n (addr_width-1) 0 in
                incr n;
                str  
                ) !prd @
                [
                sprintf "signal RAM_%s_ADDR_AUX_WR: std_logic_vector(%d downto %d);"
                            db.db_name
                            (addr_width-1)
                            0;
                ]
            end;
          )
          else
          [
            sprintf "signal RAM_%s: RAM_%s_TYPE;"
                    db.db_name db.db_name;
          ] 
          in
        (*
        ** Create process reader and writer lists
        *)
        let prd,pwr,pgd = (ref []),(ref []),(ref []) in
        List.iter (fun obj ->
            let add_rd pl = 
                List.iter (fun pro ->
                    if not (List.mem pro !prd) then prd := pro :: !prd;
                    ) pl;
                in
            let add_wr pl = 
                List.iter (fun pro ->
                    if not (List.mem pro !pwr) then pwr := pro :: !pwr;
                    ) pl;
                in
            let rec get_obj obj =
                match obj with
                | OT_var co -> 
                    add_rd co.co_reader;
                    add_wr co.co_writer;
                | OT_array at -> get_obj at.at_objs.(0);
                | _ -> error 676816 "";
                in
            get_obj obj;
          ) db.db_objs;
        List.iter (fun pro ->
            if not (List.mem pro !pgd) then pgd := pro :: !pgd;
            ) !prd;
        List.iter (fun pro ->
            if not (List.mem pro !pgd) then pgd := pro :: !pgd;
            ) !pwr;
        
        let ram_rd =
          (List.map (fun pro ->
                sprintf "signal RAM_%s_%s_DOUT: %s;"
                    db.db_name pro.pro_name (obj_decl_type (DT_logic db.db_width));
                    
            ) !prd) @ 
          (List.map (fun pro ->
                sprintf "signal RAM_%s_%s_RE: std_logic;"
                    db.db_name pro.pro_name;
            ) !prd) 
          in
        let ram_wr =
          (List.map (fun pro ->
            sprintf "signal RAM_%s_%s_DIN: %s;"
                    db.db_name pro.pro_name (obj_decl_type (DT_logic db.db_width));
            ) !pwr ) @ 
          (List.map (fun pro ->
            sprintf "signal RAM_%s_%s_WE: std_logic;"
                    db.db_name pro.pro_name ;
            ) !pwr ) 
          in
        let ram_addr =
           List.map (fun pro ->
            sprintf "signal RAM_%s_%s_ADDR: std_logic_vector(%d downto %d);"
                            db.db_name
                            pro.pro_name 
                            ((const_width 
                                (V_int (i64 (max 2 (db.db_size-1)))))
                             -1) 
                            0;
            ) !pgd in

        let ram_gd =
           let sgd = ref [] in
           List.iter (fun pro ->
              sgd := !sgd @ [sprintf "signal RAM_%s_%s_GD: std_logic;"
                              db.db_name pro.pro_name;]
            ) !pgd;
            !sgd in

        let ram_lock =
            if singleport then
            [   
                sprintf "signal RAM_%s_LOCKED: std_logic;"
                        db.db_name
            ]
            else if dualport then
            [   
                sprintf "signal RAM_%s_LOCKED_A: std_logic;"
                        db.db_name;
                sprintf "signal RAM_%s_LOCKED_B: std_logic;"
                        db.db_name
            ]
            else
            begin
              let n = ref 0 in
              List.map (fun pro ->
                let str = 
                    sprintf "signal RAM_%s_LOCKED_P%d: std_logic;"
                        db.db_name !n in
                incr n;
                str  
                ) !prd @
                [
                sprintf "signal RAM_%s_LOCKED_WR: std_logic;"
                            db.db_name;
                ]
            end;
            in
        (* array @ signal @ ram_addr @ ram_rd @ ram_wr @ ram_gd @ ram_lock,[] *)
        emi.rl_obj_decl (Sym_obj (ao_of_sym modu sym)) modu proo
      end;
  end;
  | _ -> error 171838 ""
    
