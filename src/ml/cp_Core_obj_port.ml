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
**    $VERSION:     2.13
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

(*
** Object port declaration in entity header.
*)
let rec obj_port sym modu pro =
  let is_mon,is_debug,sym = get_mon sym in
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_signal co ->
    begin
        let inout = (List.mem Obj_port_in co.co_flags) &&
                    (List.mem Obj_port_out co.co_flags) in
        match pro with
        | Some pro ->
            if is_mon then
                error 462192 ""
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read or write access ?
                *)
                let rds,wrs=
                    (if (List.mem pro co.co_reader) then
                        [sprintf "signal %s_RD: in %s;" 
                                 co.co_name
                                 (obj_decl_type co.co_type)]
                    else
                        []
                    ),
                    (if (List.mem pro co.co_writer) then
                        [sprintf "signal %s_WR: out %s;" 
                                 co.co_name
                                 (obj_decl_type co.co_type)]
                    else    
                        []) in
                rds @ wrs, []
            end
            (* else error 990610 ""; *)
        | None -> 
            if is_mon then
                error 518280 ""
            else if not (List.mem Mod_main modu.mod_flags) then
            begin
                (*
                ** Route temporary signals up.
                *)
                let rds,wrs=
                    (if co.co_reader <> [] then
                        [sprintf "signal %s_RD: in %s;" 
                                 co.co_name
                                 (obj_decl_type co.co_type)]
                    else
                        []
                    ),
                    (if co.co_writer <> [] then
                        [sprintf "signal %s_WR: out %s;" 
                                 co.co_name
                                 (obj_decl_type co.co_type)]
                    else    
                        []) in
                rds @ wrs, []
            end
            else
            begin
                (*
                ** Create original signal. Direction depends on
                ** read and write access.
                *)
                let rd = (co.co_reader <> []) || (List.mem Obj_port_in co.co_flags) in
                let wr = (co.co_writer <> []) || (List.mem Obj_port_out co.co_flags) in
                (if wr && not rd  && not inout then
                    [sprintf "signal %s: out %s;" 
                             co.co_name
                             (obj_decl_type co.co_type)]
                 else if inout then
                    [sprintf "signal %s: inout %s;" 
                             co.co_name
                             (obj_decl_type co.co_type)]
                 else
                    [sprintf "signal %s: in %s;" 
                             co.co_name
                             (obj_decl_type co.co_type)]
                ),
                (if co.co_writer <> [] && not inout then
                    [sprintf "%s <= %s_WR;" 
                             co.co_name
                             co.co_name
                    ] 
                else []) @
                (if co.co_reader <> [] && not inout then
                    [sprintf "%s_RD <= %s;" 
                             co.co_name
                             co.co_name
                    ]
                else []) @
                (if inout then
                    [
                      (* THIS MUST BE A PORT SIGNAL! NO AUX REQUIRED! *)
                    ]
                else [])
            end;
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
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                
                (*
                ** With auxilliary signals, too.
                *)
                let s_sched =
                  if not fifo then [] else
                  begin
                      let n = List.length pall in
                      let w = const_width (V_int (Int64.of_int n)) in
                      let dt_prio = DT_logic w in
                      let s = ref [] in
                      List.iter (fun pro ->
                        s := !s @ [sprintf "signal MON_REG_%s_%s_QUEUED: out std_logic;"
                                   co.co_name pro.pro_name;
                           sprintf "signal MON_REG_%s_%s_PRIO: out %s;"
                                   co.co_name pro.pro_name
                                   (obj_decl_type dt_prio);
                        ];
                       ) pall;
                     s := !s @ [sprintf "signal MON_REG_%s_HEAD: out %s;"
                                 co.co_name 
                                 (obj_decl_type dt_prio);
                        sprintf "signal MON_REG_%s_TAIL: out %s;"
                                 co.co_name
                                 (obj_decl_type dt_prio);
                        sprintf "signal MON_REG_%s_OWNER: out %s;"
                                 co.co_name
                                 (obj_decl_type dt_prio);
                     ];
                   !s
                  end in
                let s_locked = 
                    if co.co_guard <> None then
                    [
                    (sprintf "signal MON_REG_%s_LOCKED: out %s;"
                             co.co_name (get_env_str "logic_type"));     
                    ] else [] in
                        
                let s_we = 
                    let l = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_REG_%s_%s_WE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str "logic_type")
                                     );
                            ];
                            if co.co_guard <> None then
                            l := !l @ [ 
                            (sprintf "signal MON_REG_%s_%s_GD: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str "logic_type"));
                            ];
                            ) co.co_writer;
                    !l
                    in
                ([sprintf "signal MON_REG_%s: out %s;" 
                         co.co_name
                         (obj_decl_type (dt_of_td 
                                            T_logic
                                            (size_of_dt co.co_type)
                                        )
                         )
                  ] @ s_locked @ s_we @ s_sched), [];
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = co.co_guard <> None in
                let rds,wrs,gds =
                    (if rd then
                            [sprintf "signal REG_%s_RD: in %s;" 
                                     co.co_name
                                     (obj_decl_type co.co_type)]
                     else
                            []
                     ),
                     (if wr then
                            [sprintf "signal REG_%s_WR: out %s;" 
                                     co.co_name
                                     (obj_decl_type co.co_type);
                             sprintf "signal REG_%s_WE: out %s;"
                                     co.co_name
                                     (get_env_str"logic_type");
                            ]
                      else
                            []
                     ),
                     (if wr && gd then
                            [sprintf "signal REG_%s_GD: in %s;"
                                     co.co_name
                                     (get_env_str"logic_type")
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds, []
            end
            (* else error 513037 ""; *)
        | None -> 
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                (*
                ** With auxilliary signals, too.
                *)
                let s_sched =
                  if not fifo then [] else
                  begin
                      let n = List.length pall in
                      let w = const_width (V_int (Int64.of_int n)) in
                      let dt_prio = DT_logic w in
                      let s = ref [] in
                      List.iter (fun pro ->
                        s := !s @ [sprintf "signal MON_REG_%s_%s_QUEUED: out std_logic;"
                                   co.co_name pro.pro_name;
                           sprintf "signal MON_REG_%s_%s_PRIO: out %s;"
                                   co.co_name pro.pro_name
                                   (obj_decl_type dt_prio);
                        ];
                       ) pall;
                     s := !s @ [sprintf "signal MON_REG_%s_HEAD: out %s;"
                                 co.co_name 
                                 (obj_decl_type dt_prio);
                        sprintf "signal MON_REG_%s_TAIL: out %s;"
                                 co.co_name
                                 (obj_decl_type dt_prio);
                        sprintf "signal MON_REG_%s_OWNER: out %s;"
                                 co.co_name
                                 (obj_decl_type dt_prio);
                     ];
                   !s
                  end in
                let s_locked = 
                    if co.co_guard <> None then
                    [
                    (sprintf "signal MON_REG_%s_LOCKED: out %s;"
                             co.co_name (get_env_str"logic_type"));     
                    ] else [] in
                        
                let s_we = 
                    let l = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_REG_%s_%s_WE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if co.co_guard <> None then
                            l := !l @ [ 
                            (sprintf "signal MON_REG_%s_%s_GD: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type"));
                            ];
                            ) co.co_writer;
                    !l
                    in
                ([sprintf "signal MON_REG_%s: out %s;" 
                         co.co_name
                         (obj_decl_type (dt_of_td 
                                            T_logic
                                            (size_of_dt co.co_type)
                                        )
                         )
                  ] @ s_locked @ s_we @ s_sched), [];
            end
            else
            begin
              (*
              ** Only read port is exported. Exported type is always of type std_logic_vector!
              *)
              match co.co_type with
              | DT_logic n ->
                [sprintf "signal %s_RD: out %s;" 
                         co.co_name
                         (obj_decl_type (DT_logic n))],
                [sprintf "%s_RD <= REG_%s;" 
                               co.co_name
                               co.co_name]
              | DT_int n ->
                [sprintf "signal %s_RD: out %s;" 
                         co.co_name
                         (obj_decl_type (DT_logic n))],
                [sprintf "%s_RD <= std_logic_vector(REG_%s);" 
                               co.co_name
                               co.co_name]
              | DT_bool ->
                [sprintf "signal %s_RD: out std_logic;" 
                         co.co_name
                         ],
                [sprintf "%s_RD <= '1' when REG_%s = '1' else '0';" 
                               co.co_name
                               co.co_name]
              | DT_char ->
                [sprintf "signal %s_RD: out std_logic_vector(%d downto 0);" 
                         co.co_name
                         7],
                [sprintf "%s_RD <= REG_%s;" 
                               co.co_name
                               co.co_name]
              | _ -> error 0 "Export of register type not supported!";
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

      let model_buf =  List.hd ch.ch_model in

      let ao_p,ao_t = 
        match pro with
        | Some pro ->
          let ao = ch.ch_ao in
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

          let s_unlock =
              if (List.mem pro !unlock) then
                  [
                  sprintf "signal CHANNEL_%s_UNLOCK: out std_logic;" ao.ao_name;
                  ]         
                  else [] in
          let s_empty =
              if (List.mem pro !empty) then
                  [
                  sprintf "signal CHANNEL_%s_EMPTY_RE: out std_logic;" ao.ao_name;
                  sprintf "signal CHANNEL_%s_EMPTY_RD: in std_logic;" ao.ao_name;
                  ]         
                  else [] in

          let s_full =
              if (List.mem pro !full) then
                  [
                  sprintf "signal CHANNEL_%s_FULL_RE: out std_logic;" ao.ao_name;
                  sprintf "signal CHANNEL_%s_FULL_RD: in std_logic;" ao.ao_name;
                  ]         
                  else [] in

          
          s_unlock@s_empty@s_full,[] 
        | None -> [],[] in
        
      let p,t = 
        match pro with
        | Some pro ->
        begin
          match ch.ch_ot with
          | None ->
          begin
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                let pro_con = 
                    let l = ref [] in
                    let pro_gds = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_WE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ] @ (if bi then [ 
                                sprintf "signal MON_%s_%s_GD_A: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                sprintf "signal MON_%s_%s_GD_B: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                ] else []); 
                              pro_gds := !pro_gds @ [pro];
                            end;                              
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_RE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ] @ (if bi then [ 
                                  sprintf "signal MON_%s_%s_GD_A: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                  sprintf "signal MON_%s_%s_GD_B: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                ] else []); 
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                pro_con,[]
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = rd || wr || ao_p <> [] in
                let rds,wrs,gds =
                    (if rd then
                            [sprintf "signal CHAN_%s_RD: in %s;" 
                                     co.co_name
                                     (obj_decl_type co.co_type);
                             sprintf "signal CHAN_%s_RE: out %s;"
                                     co.co_name
                                     (get_env_str"logic_type");
                            ]
                     else
                            []
                     ),
                     (if wr then
                            [sprintf "signal CHAN_%s_WR: out %s;" 
                                     co.co_name
                                     (obj_decl_type co.co_type);
                             sprintf "signal CHAN_%s_WE: out %s;"
                                     co.co_name
                                     (get_env_str"logic_type");
                            ]
                      else
                            []
                     ),
                     (if gd then
                            [sprintf "signal CHAN_%s_GD: in %s;"
                                     co.co_name
                                     (get_env_str"logic_type")
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds, []
            end;
          end;
          | Some (OT_struct st) ->
          begin
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                let pro_con = 
                    let l = ref [] in
                    let pro_gds = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_WE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ] @ (if bi then [ 
                                  sprintf "signal MON_%s_%s_GD_A: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                  sprintf "signal MON_%s_%s_GD_B: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                ] else []); 
                              pro_gds := !pro_gds @ [pro];
                            end;                              
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_RE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ] @ (if bi then [ 
                                  sprintf "signal MON_%s_%s_GD_A: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                  sprintf "signal MON_%s_%s_GD_B: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                ] else []); 
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                pro_con,[]
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = rd || wr || ao_p <> [] in
                let rds,wrs,gds =
                    (if rd then
                      (List.map (fun ot ->
                        match co_of_ot ot with
                        | Some co ->
                            sprintf "signal CHAN_%s_RD: in %s;" 
                                    co.co_name
                                    (obj_decl_type co.co_type);
                        | None -> error 523644 "" ;
                        ) st.st_objs )@
                        [sprintf "signal CHAN_%s_RE: out %s;"
                                     co.co_name
                                     (get_env_str"logic_type");
                        ]
                     else
                        []
                     ),
                     (if wr then
                       (List.map (fun ot ->
                        match co_of_ot ot with
                        | Some co ->
                            sprintf "signal CHAN_%s_WR: out %s;" 
                                    co.co_name
                                    (obj_decl_type co.co_type);
                        | None -> error 587374 "" ;
                        ) st.st_objs )@
                        [sprintf "signal CHAN_%s_WE: out %s;"
                                     co.co_name
                                     (get_env_str"logic_type");
                        ]
                      else
                        []
                     ),
                     (if gd then
                            [sprintf "signal CHAN_%s_GD: in %s;"
                                     co.co_name
                                     (get_env_str"logic_type")
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds, []
            end;
          end;
          | _ -> error 196964 "";
        end;
        | None -> 
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                let pro_gds = ref [] in                        
                let pro_con = 
                    let l = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_WE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type"));
                              ] @ (if bi then [ 
                                  sprintf "signal MON_%s_%s_GD_A: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                  sprintf "signal MON_%s_%s_GD_B: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                ] else []); 
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_RE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type"));
                              ] @ (if bi then [ 
                                  sprintf "signal MON_%s_%s_GD_A: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                  sprintf "signal MON_%s_%s_GD_B: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type");
                                ] else []); 
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                pro_con,[];
            end
            else
                error 0  (sprintf "Channel <%s> can't be exported in toplevel module!" co.co_name);
        in
      ao_p@p,ao_t@t
    end;
    | OT_queue qu ->
    begin
      let co = qu.qu_obj in
      let width = size_of_dt co.co_type in
      let depth = qu.qu_depth in
      let depth' = sel_width depth in
        
      let ao_p,ao_t = 
        match pro with
        | Some pro ->
          let ao = qu.qu_ao in
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

          let s_unlock =
              if (List.mem pro !unlock) then
                  [
                  sprintf "signal QUEUE_%s_UNLOCK: out std_logic;" ao.ao_name;
                  ]         
                  else [] in
          let s_empty =
              if (List.mem pro !empty) then
                  [
                  sprintf "signal QUEUE_%s_EMPTY_RE: out std_logic;" ao.ao_name;
                  sprintf "signal QUEUE_%s_EMPTY_RD: in std_logic;" ao.ao_name;
                  ]         
                  else [] in

          let s_full =
              if (List.mem pro !full) then
                  [
                  sprintf "signal QUEUE_%s_FULL_RE: out std_logic;" ao.ao_name;
                  sprintf "signal QUEUE_%s_FULL_RD: in std_logic;" ao.ao_name;
                  ]         
                  else [] in

          s_unlock@s_empty@s_full,[]
        | None -> [],[] in



      let p,t = 
        match pro with
        | Some pro ->
        begin
          match qu.qu_ot with
          | None ->
          begin
            (*
            ** Queue of scalar type
            *)
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                [sprintf "signal MON_%s_empty: out %s;" 
                         co.co_name
                         (get_env_str"logic_type");
                 sprintf "signal MON_%s_full: out %s;" 
                         co.co_name
                         (get_env_str"logic_type");
                 sprintf "signal MON_%s_addr_rd: out %s;" 
                         co.co_name
                         (obj_decl_type (DT_logic depth'));
                 sprintf "signal MON_%s_addr_wr: out %s;" 
                         co.co_name
                         (obj_decl_type (DT_logic depth'));
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let pro_con = 
                    let l = ref [] in
                    let pro_gds = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_WE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;                              
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_RE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                pro_con
                end else [];
                ), [];
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = rd || wr || ao_p <> [] in
                let rds,wrs,gds =
                    (if rd then
                            [sprintf "signal QUEUE_%s_RD: in %s;" 
                                     co.co_name
                                     (obj_decl_type co.co_type);
                             sprintf "signal QUEUE_%s_RE: out %s;"
                                     co.co_name
                                     (get_env_str"logic_type");
                            ]
                     else
                            []
                     ),
                     (if wr then
                            [sprintf "signal QUEUE_%s_WR: out %s;" 
                                     co.co_name
                                     (obj_decl_type co.co_type);
                             sprintf "signal QUEUE_%s_WE: out %s;"
                                     co.co_name
                                     (get_env_str"logic_type");
                            ]
                      else
                            []
                     ),
                     (if gd then
                            [sprintf "signal QUEUE_%s_GD: in %s;"
                                     co.co_name
                                     (get_env_str"logic_type")
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds, []
            end
          end;
          | Some (OT_struct st) ->
          begin
            (*
            ** Queue of structure type
            *)
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                [sprintf "signal MON_%s_empty: out %s;" 
                         co.co_name
                         (get_env_str"logic_type");
                 sprintf "signal MON_%s_full: out %s;" 
                         co.co_name
                         (get_env_str"logic_type");
                 sprintf "signal MON_%s_addr_rd: out %s;" 
                         co.co_name
                         (obj_decl_type (DT_logic depth'));
                 sprintf "signal MON_%s_addr_wr: out %s;" 
                         co.co_name
                         (obj_decl_type (DT_logic depth'));
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let pro_con = 
                    let l = ref [] in
                    let pro_gds = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_WE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;                              
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_RE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                pro_con
                end else [];
                ), [];
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = rd || wr || ao_p <> [] in
                let rds,wrs,gds =
                    (if rd then
                       (List.map (fun ot ->
                        match co_of_ot ot with
                        | Some co ->
                            sprintf "signal QUEUE_%s_RD: in %s;" 
                                    co.co_name
                                    (obj_decl_type co.co_type);
                        | None -> error 409435 "" ;
                        ) st.st_objs )@
                        [sprintf "signal QUEUE_%s_RE: out %s;"
                                     co.co_name
                                     (get_env_str"logic_type");
                        ]
                     else
                            []
                     ),
                     (if wr then
                       (List.map (fun ot ->
                        match co_of_ot ot with
                        | Some co ->
                            sprintf "signal QUEUE_%s_WR: out %s;" 
                                    co.co_name
                                    (obj_decl_type co.co_type);
                        | None -> error 409435 "" ;
                        ) st.st_objs )@
                        [sprintf "signal QUEUE_%s_WE: out %s;"
                                     co.co_name
                                     (get_env_str"logic_type");
                        ]
                      else
                            []
                     ),
                     (if gd then
                            [sprintf "signal QUEUE_%s_GD: in %s;"
                                     co.co_name
                                     (get_env_str"logic_type")
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds, []
            end
          end;
          | _ -> error 162084 "";
        end;
        | None -> 
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                [sprintf "signal MON_%s_empty: out %s;" 
                         co.co_name
                         (get_env_str"logic_type");
                 sprintf "signal MON_%s_full: out %s;" 
                         co.co_name
                         (get_env_str"logic_type");
                 sprintf "signal MON_%s_addr_rd: out %s;" 
                         co.co_name
                         (obj_decl_type (DT_logic depth'));
                 sprintf "signal MON_%s_addr_wr: out %s;" 
                         co.co_name
                         (obj_decl_type (DT_logic depth'));
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let pro_con = 
                    let l = ref [] in
                    let pro_gds = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_WE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;                              
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "signal MON_%s_%s_RE: out %s;"
                                     co.co_name pro.pro_name
                                     (get_env_str"logic_type")
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "signal MON_%s_%s_GD: out %s;"
                                       co.co_name pro.pro_name
                                       (get_env_str"logic_type"));
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                pro_con
                end else [];
                ), [];
            end
            else
                error 0  (sprintf "Queue <%s> can't be exported in toplevel module!" co.co_name);
        in
      ao_p@p,ao_t@t                         
    end;
    | OT_array at ->
      let is_block = List.mem AT_block at.at_flags in
      let is_dyn = List.mem AT_dyn at.at_flags in
      if not is_block then
      begin
        match pro with
        | Some pro ->
            if not is_dyn && not is_block then
            begin
                let s1,s2 = ref [], ref [] in
                Array.iter (fun ot ->
                    let s1',s2' = obj_port (Sym_obj ot) modu (Some pro) in
                    s1 := !s1 @ s1';
                    s2 := !s2 @ s2';
                    ) at.at_objs;
                !s1, !s2
            end
            else 
            begin
              match at.at_objs.(0) with
              | OT_reg co ->
                (*
                ** Process port
                *)
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

                let rd = ref false in
                let wr = ref false in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709866 "" in
                Array.iter (fun ot -> 
                        let co = 
                            match co_of_ot ot with
                            | Some co -> co;
                            | None -> error 709865 ""; in
                        if List.mem pro co.co_reader then
                            rd := true;
                        if List.mem pro co.co_writer then
                            wr := true;
                    )  at.at_objs;
                let rd,wr = !rd, !wr in
                let gd_rd,gd_wr=array_guard at in
                let rds,wrs,gds,sel =
                    (if rd then
                            [sprintf "signal ARRAY_%s_RD: in %s;" 
                                     at.at_name
                                     (obj_decl_type dt);
                            ]
                     else
                            []
                     ),
                     (if wr then
                            [sprintf "signal ARRAY_%s_WR: out %s;" 
                                     at.at_name
                                     (obj_decl_type dt);
                             sprintf "signal ARRAY_%s_WE: out %s;"
                                     at.at_name
                                     (get_env_str"logic_type");
                            ]
                      else
                            []
                     ),
                     (if gd_wr && wr then
                            [sprintf "signal ARRAY_%s_GD: in %s;"
                                     at.at_name
                                     (get_env_str"logic_type")
                            ]
                      else
                            []
                      ),
                      [
                            sprintf "signal ARRAY_%s_SEL: out %s;"
                                    at.at_name
                                    (obj_decl_type (DT_logic sel_width)); 
                      ]
                      in
                rds @ wrs @ gds @ sel, []
              | OT_channel ch ->
                (*
                ** Process port
                *)
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

                let rd = ref false in
                let wr = ref false in
                let unlock = ref false in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709868 "" in
                Array.iter (fun ot -> 
                    match ot with
                    | OT_channel ch -> 
                        let co = ch.ch_obj in
                        let ao = ch.ch_ao in
                        if List.mem pro co.co_reader then
                            rd := true;
                        if List.mem pro co.co_writer then
                            wr := true;
                        List.iter (fun (sel',pro') ->
                          match sel' with
                          | "unlock" -> if pro' = pro then unlock := true;
                          | _ -> ()) ao.ao_procs; 
                    | _ -> ();
                    )  at.at_objs;
                let rd,wr = !rd, !wr in
                let gd_rd,gd_wr=array_guard at in
                let gd = gd_rd || gd_wr || !unlock in
                let s_unlock =
                  if  !unlock then
                  [
                    sprintf "signal CHAN_%s_UNLOCK: out std_logic;" at.at_name;
                  ]         
                  else [] in
                let rds,wrs,gds,sel =
                    (if rd then
                            [sprintf "signal CHAN_%s_RD: in %s;" 
                                     at.at_name
                                     (obj_decl_type dt);
                             sprintf "signal CHAN_%s_RE: out %s;"
                                     at.at_name
                                     (get_env_str"logic_type");
                            ]
                     else
                            []
                     ),
                     (if wr then
                            [sprintf "signal CHAN_%s_WR: out %s;" 
                                     at.at_name
                                     (obj_decl_type dt);
                             sprintf "signal CHAN_%s_WE: out %s;"
                                     at.at_name
                                     (get_env_str"logic_type");
                            ]
                      else
                            []
                     ),
                     (if gd then
                            [sprintf "signal CHAN_%s_GD: in %s;"
                                     at.at_name
                                     (get_env_str"logic_type")
                            ]
                      else
                            []
                      ),
                      [
                            sprintf "signal CHAN_%s_SEL: out %s;"
                                    at.at_name
                                    (obj_decl_type (DT_logic sel_width)); 
                      ]
                      in
                rds @ wrs @ gds @ sel @ s_unlock, []
              | OT_queue qu ->
                (*
                ** Process port
                *)
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

                let rd = ref false in
                let wr = ref false in
                let unlock = ref false in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709869 "" in
                Array.iter (fun ot -> 
                    match ot with
                    | OT_queue qu -> 
                        let co = qu.qu_obj in
                        let ao = qu.qu_ao in
                        if List.mem pro co.co_reader then
                            rd := true;
                        if List.mem pro co.co_writer then
                            wr := true;
                        List.iter (fun (sel',pro') ->
                          match sel' with
                          | "unlock" -> if pro' = pro then unlock := true;
                          | _ -> ()) ao.ao_procs; 
                    | _ -> ();
                    )  at.at_objs;
                let rd,wr = !rd, !wr in
                let gd_rd,gd_wr=array_guard at in
                let gd = gd_rd || gd_wr || !unlock in
                let s_unlock =
                  if  !unlock then
                  [
                    sprintf "signal QUEUE_%s_UNLOCK: out std_logic;" at.at_name;
                  ]         
                  else [] in
                let rds,wrs,gds,sel =
                    (if rd then
                            [sprintf "signal QUEUE_%s_RD: in %s;" 
                                     at.at_name
                                     (obj_decl_type dt);
                             sprintf "signal QUEUE_%s_RE: out %s;"
                                     at.at_name
                                     (get_env_str"logic_type");
                            ]
                     else
                            []
                     ),
                     (if wr then
                            [sprintf "signal QUEUE_%s_WR: out %s;" 
                                     at.at_name
                                     (obj_decl_type dt);
                             sprintf "signal QUEUE_%s_WE: out %s;"
                                     at.at_name
                                     (get_env_str"logic_type");
                            ]
                      else
                            []
                     ),
                     (if gd then
                            [sprintf "signal QUEUE_%s_GD: in %s;"
                                     at.at_name
                                     (get_env_str"logic_type")
                            ]
                      else
                            []
                      ),
                      [
                            sprintf "signal QUEUE_%s_SEL: out %s;"
                                    at.at_name
                                    (obj_decl_type (DT_logic sel_width)); 
                      ]
                      in
                rds @ wrs @ gds @ sel @ s_unlock, []
              | _ -> error 521030 "";
            end
        | None -> 
            if not is_dyn && not is_block then
            begin
                let s1,s2 = ref [], ref [] in
                Array.iter (fun ot ->
                    let sym = Sym_obj ot in
                    let s1',s2' = obj_port sym modu pro in
                    s1 := !s1 @ s1';
                    s2 := !s2 @ s2';
                    ) at.at_objs;
                !s1, !s2
            end
            else if not is_block then
            begin
              match at.at_objs.(0) with
              | OT_reg co ->
                let w = size_of_dt co.co_type in
                let s1,s2 = ref [], ref [] in
                for i = 0 to (Array.length at.at_objs)-1 
                do
                  s1 := !s1 @ [sprintf "signal ARRAY_%s_%d_RD: out %s;"
                                       at.at_name
                                       i
                                       (obj_decl_type (DT_logic w))];
                  s2 := !s2 @ [sprintf "ARRAY_%s_%d_RD <= std_logic_vector(ARRAY_%s(%d));" 
                                       at.at_name
                                       i
                                       at.at_name
                                       i];
                done;
                !s1, !s2        
              | _ -> error 0 (sprintf "Can't export array <%s>." at.at_name);      
            end
            else
                error 125342  ""
      end
      else 
        [],[]; (* emitted just only one time with Sym_block *)
    | OT_array_sel (at,sel) -> 
        let obj = at.at_objs.(sel) in
        let is_dyn = List.mem AT_dyn at.at_flags in
        if not is_dyn then obj_port (Sym_obj obj) modu pro 
        else [],[]
    | OT_component st -> 
        if pro <> None then [],[]
        else
        begin
            let sl,al=ref [], ref [] in
            List.iter (fun ot ->
                let sl',al' = obj_port (Sym_obj ot) modu pro in
                sl := !sl @ sl';
                al := !al @ al') st.st_objs;
            !sl, !al
        end;
    | OT_struct st -> 
        if pro <> None then [],[]
        else
        begin
            let sl,al=ref [], ref [] in
            List.iter (fun ot ->
                let sl',al' = obj_port (Sym_obj ot) modu pro in
                sl := !sl @ sl';
                al := !al @ al') st.st_objs;
            !sl, !al
        end;

    | OT_var co -> [],[];  (* emitted just only one time with Sym_block *)
    | OT_named_value _
    | OT_const _ -> [],[]; (* ignored *)
    | OT_object ao ->
    begin

      match (ao.ao_obj,pro) with
      | (Some (OT_queue qu),Some pro) ->
        obj_port (Sym_obj (OT_queue qu)) modu (Some pro)
      | (Some (OT_channel ch),Some pro) ->
        obj_port (Sym_obj (OT_channel ch)) modu (Some pro) 
      | _ -> error 94984 "";
    end;
    | _ -> 
        error 517761 "";
  end;
  | Sym_block db ->
  begin

    if (List.mem Mp_multiport db.db_params) then
    begin
        let prd = ref [] in 
        let pwr = ref [] in 
        List.iter (fun obj ->
            match co_of_ot obj with
            | Some co -> 
                List.iter (fun pro ->
                    if not (List.mem pro !prd) then prd := !prd @ [pro];
                    ) co.co_reader;
                List.iter (fun pro ->
                    if not (List.mem pro !pwr) then pwr := !pwr @ [pro];
                    ) co.co_writer;
            | None -> ();
            ) db.db_objs;

        let numwr = List.length !pwr in
        if numwr > 1 then
        begin
            out (sprintf "Warning: Changing multiport RAM <%s> with %d writers to single port RAM!"
                         db.db_name numwr);
            db.db_params <- List.filter (fun p -> p <> Mp_multiport) db.db_params;
            db.db_params <- db.db_params @ [Mp_singleport];
            match db.db_rules with
            | Some rl -> 
            begin
                match rl.rl_child with
                | Some rl' -> __(rl'.rl_interp "set arch singleport") ;
                | None -> error 0 (sprintf "block db=<%s> without rl_child" db.db_name)
            end;
            | None -> error 0 (sprintf "block db=<%s> without rules" db.db_name) 
        end;
    end;

    if (List.mem Mp_dualport db.db_params) then
    begin
        let prd = ref [] in 
        let pwr = ref [] in 
        List.iter (fun obj ->
            match co_of_ot obj with
            | Some co -> 
                List.iter (fun pro ->
                    if not (List.mem pro !prd) then prd := !prd @ [pro];
                    ) co.co_reader;
                List.iter (fun pro ->
                    if not (List.mem pro !pwr) then pwr := !pwr @ [pro];
                    ) co.co_writer;
            | None -> ();
            ) db.db_objs;

        let numwr = List.length !pwr in
        let numrd = List.length !prd in
        if numwr > 2 || numrd > 2 then
        begin
            out (sprintf "Warning: Changing dualport RAM <%s> with %d writers and %d readers to single port RAM!"
                         db.db_name numwr numrd);
            db.db_params <- List.filter (fun p -> p <> Mp_dualport) db.db_params;
            db.db_params <- db.db_params @ [Mp_singleport];
        end;
    end;


    let async = List.mem Mp_readasync db.db_params in
    let dualport = List.mem Mp_dualport db.db_params in
    let inline = List.mem Mp_inline db.db_params in
    let addr_width = const_width (V_int (i64(max 2 (db.db_size-1)))) in
    let twoproc = dualport && (compiler.t_synth_tool.syn_tool = "xilinx") in

    if not is_mon then
    begin
        match pro with
        | Some pro ->
            if (sym_check_sym pro.pro_import sym) then
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Read and Write access requires guards and write
                ** enable signals.
                *)
                let rd = ref false in 
                let wr = ref false in 
                List.iter (fun obj ->
                  match obj with
                  | OT_var co -> if List.mem pro co.co_reader then
                                  rd := true;
                                 if List.mem pro co.co_writer then
                                  wr := true;
                  | _ -> ();
                  ) db.db_objs;
                let ram_in = 
                  if !wr  then
                  [
                    sprintf "signal RAM_%s_WR: out %s;"
                            db.db_name (obj_decl_type (DT_logic db.db_width));
                    sprintf "signal RAM_%s_WE: out std_logic;"
                            db.db_name;
                  ]
                  else [] in
                let ram_out = 
                  if !rd then
                  [
                    sprintf "signal RAM_%s_RD: in %s;"
                            db.db_name (obj_decl_type (DT_logic db.db_width));
                    sprintf "signal RAM_%s_RE: out std_logic;"
                            db.db_name;
                  ]
                  else [] in
                let ram_addr = 
                  [
                    sprintf "signal RAM_%s_ADDR: out %s;"
                            db.db_name 
                            (obj_decl_type (DT_logic addr_width));
                    sprintf "signal RAM_%s_GD: in std_logic;"
                            db.db_name;
                  ] in
                  
                ram_in @ ram_out @ ram_addr,[]
            end
            else
              error 806664 "";
        | None -> error 49632 "";
    end
    else
    begin
        match pro with
        | Some pro -> error 138048 "";
        | None -> 
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
        
        let ram_rd =
          (List.map (fun pro ->
                sprintf "signal MON_RAM_%s_%s_RE: out %s;"
                    db.db_name pro.pro_name
                    (get_env_str"logic_type");
            ) !prd) 
          in
        let ram_wr =
          (List.map (fun pro ->
            sprintf "signal MON_RAM_%s_%s_WE: out %s;"
                    db.db_name pro.pro_name 
                    (get_env_str"logic_type");
          ) !pwr) 
          in

        let ram_addr =
           List.map (fun pro ->
            sprintf "signal MON_RAM_%s_%s_ADDR: out %s;"
                            db.db_name
                            pro.pro_name 
                            (obj_decl_type (DT_logic addr_width));
            ) !pgd in
        let ram_gd =
           List.map (fun pro ->
            sprintf "signal MON_RAM_%s_%s_GD: out %s;"
                    db.db_name pro.pro_name
                    (get_env_str"logic_type");
            ) !pgd
           in
        let ram_lock =
            if not dualport then
            [
                sprintf "signal MON_RAM_%s_LOCKED: out %s;"
                    db.db_name 
                    (get_env_str"logic_type");
            ]
            else
            []
            in
        let ram_aux =
          if not inline then
            (
              if not dualport then
                [
                sprintf "signal MON_RAM_%s_WE_AUX: out %s;"
                    db.db_name 
                    (get_env_str "logic_type");              
                sprintf "signal MON_RAM_%s_WR: out %s;"
                    db.db_name 
                    (obj_decl_type (DT_logic db.db_width));
                ]
              else
                [
                sprintf "signal MON_RAM_%s_WE_AUX_A: out %s;"
                    db.db_name 
                    (get_env_str "logic_type");              
                sprintf "signal MON_RAM_%s_WE_AUX_B: out %s;"
                    db.db_name 
                    (get_env_str "logic_type");              
                sprintf "signal MON_RAM_%s_WR_AUX_A: out %s;"
                    db.db_name 
                    (obj_decl_type (DT_logic db.db_width));
                sprintf "signal MON_RAM_%s_WR_AUX_B: out %s;"
                    db.db_name 
                    (obj_decl_type (DT_logic db.db_width));
                ]
            ) @(
              if not dualport then
              [
                sprintf "signal MON_RAM_%s_RD_AUX: out %s;"
                    db.db_name 
                    (obj_decl_type (DT_logic db.db_width))
              ]
              else
              [
                sprintf "signal MON_RAM_%s_RD_AUX_A: out %s;"
                    db.db_name 
                    (obj_decl_type (DT_logic db.db_width));
                sprintf "signal MON_RAM_%s_RD_AUX_B: out %s;"
                    db.db_name 
                    (obj_decl_type (DT_logic db.db_width));
              ]
            ) @ (
              if not dualport then
                [
                    sprintf "signal MON_RAM_%s_ADDR_AUX: out %s;"
                    db.db_name 
                    (obj_decl_type (DT_logic addr_width));
                ]
                else
                [
                    sprintf "signal MON_RAM_%s_ADDR_AUX_A: out %s;"
                    db.db_name 
                    (obj_decl_type (DT_logic addr_width));
                    sprintf "signal MON_RAM_%s_ADDR_AUX_B: out %s;"
                    db.db_name 
                    (obj_decl_type (DT_logic addr_width));
                ];
            )
          else [] in
        ram_rd @ ram_wr @ ram_gd @ ram_addr @ ram_lock @ ram_aux,[]
    end;
  end;
  | _ -> error 985541 ""

