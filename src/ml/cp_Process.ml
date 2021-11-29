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
**    $CREATED:     23.7.2006
**    $VERSION:     2.07
**
**    $INFO:
**
**  Process Abstract Object Implementation. Part of Core.
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
open Printf
open Cp_syntax

let (self:(rl_module option) ref) = ref None

(*
** Return true if object is handled by this rule module.
*)
let rec my sym =
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_object ao -> ao.ao_type.ta_name = "process" 
    | _ -> false;
  end;
  | _ -> false


let emit_states pro =
    let states = get_state_names pro in    
    let len = List.length states in
    let bits = const_width (V_int (i64 len)) in
    let dt = DT_logic bits in
    let i = ref Int64.zero in
    (List.map (fun s -> 
        i := Int64.add !i Int64.one;
        sprintf "    %s when pro_state = %s else" 
                (val_str dt (V_int !i))
                s;
        ) states) @
    [
        sprintf "    %s;" (zeros bits);
    ]

let get_procs ao pro =
  let is_array,is_dyn,size,ao_name =
    match ao.ao_array with
    | at :: _ ->
    begin
      true, List.mem AT_dyn at.at_flags, at.at_dim.(0),
      if List.mem AT_dyn at.at_flags then at.at_name else ao.ao_name
    end;
    | [] -> false,false,0,ao.ao_name in
  let start = ref [] in    
  let call = ref [] in    (* requiring blocking! *)
  let stop   = ref [] in
  let blocked = ref [] in
  let sel = ref [] in
  let all = ref [] in
  
  if not is_dyn then List.iter (fun (sel,pro) ->
    match sel with
    | "start" -> start := !start @ [pro];
    | "stop" -> stop := !stop @ [pro];
    | "call" -> call := !call @ [pro];
    | _ -> error 0 
      (sprintf "Process: unknown operator <%s> found in object <%s> and process <%s>"
               sel ao_name pro.pro_name);
    ) ao.ao_procs
  else
  begin
    match ao.ao_array with
    | at :: _ ->
      Array.iter (fun ot ->
        match ot with
        | OT_object ao ->
          List.iter (fun (sel,pro) ->
            match sel with
            | "start" -> if not (List.mem pro !start) then
                                start := !start @ [pro];
            | "stop" -> if not (List.mem pro !stop) then
                                stop := !stop @ [pro];
            | "call" -> if not (List.mem pro !call) then
                                call := !call @ [pro];
            | _ -> error 0 
                        (sprintf "Process: unknown operator <%s> found in object <%s> and process <%s>"
                                 sel ao_name pro.pro_name);
            ) ao.ao_procs
        | _ -> error 335283 "";
        ) at.at_objs;
    | [] -> error 109392 "";
  end;
  List.iter (fun pro ->
    if not (List.mem pro !blocked) then
      blocked := !blocked @ [pro]
      ) (!start @ !stop @ !call);
  List.iter (fun pro ->
    if not (List.mem pro !all) then
      all := !all @ [pro]
      ) (!start @ !stop @ !call);
  List.iter (fun pro ->
    if not (List.mem pro !sel) then
      sel := !sel @ [pro]
      ) (!start @ !stop @ !call);
   
  !start, !stop, !call, !blocked, !sel, !all

    
(*
** Object port declaration in entity header.
*)
let rec obj_port sym modu opro =
  let is_mon,is_debug,sym = get_mon sym in
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_object ao -> 
      begin
        let is_array,is_dyn,size,ao_name =
          match ao.ao_array with
          | at :: _ ->
          begin
            true, List.mem AT_dyn at.at_flags, at.at_dim.(0),
            if List.mem AT_dyn at.at_flags then at.at_name else ao.ao_name
          end;
          | [] -> false,false,0,ao.ao_name in

        let pstart,pstop,pcall,pblocked,psel,pros = get_procs ao opro in    
        match opro with
        | Some pro ->
          if not is_mon then
          begin            
            (if List.mem pro pstart then
              [sprintf "signal PRO_%s_START: out std_logic;" ao_name]
             else [])@
            (if List.mem pro pstop then
              [sprintf "signal PRO_%s_STOP: out std_logic;" ao_name]
             else [])@
            (if List.mem pro pcall then
              [sprintf "signal PRO_%s_CALL: out std_logic;" ao_name;]
             else []) @
            (if List.mem pro pblocked then
              [sprintf "signal PRO_%s_GD: in std_logic;" ao_name;]
             else []) @
            (if is_array && is_dyn then  
              [sprintf "signal PRO_%s_SEL: out integer;" ao_name;]
             else [])
            ,[]
          end
          else
          begin
            let states = get_state_names pro in
            let len = const_width (V_int (i64 (List.length states))) in
            let mon_states = 
                sprintf "signal MON_PRO_%s_state: out %s_vector(%d downto 0);"
                        pro.pro_name (get_env_str "logic_type")
                        (len-1) in
            let mon_states_aux = 
                [
                    sprintf "MON_PRO_%s_state <= " pro.pro_name;
                ] @ (emit_states pro) in

              [
                mon_states;
              ],
              mon_states_aux
          end;
        | None ->
          if not is_mon then
          begin
              [],[]
          end
          else
          begin
            (*
            ** Find process...
            *)
            let pro' = sym_get_pro modu.mod_objs ao.ao_name in
            let states = get_state_names pro' in
            let len = const_width (V_int (i64 (List.length states))) in
            let mon_states = 
                sprintf "signal MON_PRO_%s_state: out %s_vector(%d downto 0);"
                        pro'.pro_name (get_env_str "logic_type")
                        (len-1) in
              [
                sprintf "signal MON_PRO_%s_ENABLE: out std_logic;" 
                            ao.ao_name;
                mon_states;
              ],
              [
                sprintf "MON_PRO_%s_ENABLE <= PRO_%s_ENABLE;" 
                        ao.ao_name ao.ao_name;
              ];
          end;
      end;
    | OT_array at ->
    begin
      let merge_to_one at ao =
        let merge_procs p_0 p =
            let p_0' = ref p_0 in
            List.iter (fun (sel,pro) ->
                if not (List.mem (sel,pro) !p_0') then
                    p_0' := !p_0' @ [sel,pro];
                ) p;
            !p_0' in
        let ao_0 = 
            match ao with
            | OT_object ao -> ao;
            | _ -> error 342881 "" in
        Array.iter (fun ot ->
            match ot with
            | OT_object ao ->
                ao_0.ao_procs <- merge_procs ao_0.ao_procs ao.ao_procs;
            | _ -> error 589203 "";
          ) at.at_objs;
        OT_object ao_0
        in
      let is_dyn = List.mem AT_dyn at.at_flags in
      match opro with
      | Some pro ->
        let ports,auxs= ref [], ref [] in
        Array.iter (fun ot' ->
          match ot' with
          | OT_object ao ->
            let used = ref false in
            List.iter (fun (sel',pro') ->
                used := !used or (pro = pro');
                ) ao.ao_procs;
            if !used then 
            begin
              let pl,al= obj_port (if not is_mon then Sym_obj ot'
                                                 else (Sym_mon (false,Sym_obj ot'))) modu opro in
              ports := !ports @ pl;
              auxs := !auxs @ al;
            end;
          | _ -> error 926206 "";          
          ) (if not is_dyn || is_mon then at.at_objs 
                else [|merge_to_one at at.at_objs.(0)|]);
        !ports, !auxs
      | None -> 
        if is_mon then
        begin
          let ports,auxs= ref [], ref [] in
          Array.iter (fun ot' ->
              match ot' with
              | OT_object ao ->
                  let pl,al= obj_port (Sym_mon (false,Sym_obj ot')) modu opro in
                  ports := !ports @ pl;
                  auxs := !auxs @ al;
                | _ -> error 453998 "";          
            ) at.at_objs;
          !ports, !auxs
        end
        else error 704886 "";
    end;
    | _ -> 
        error 517773 "";
  end;
  | _ -> error 985553 ""


(*
** Object port mapping.
*)
let rec obj_map sym modu opro =
  let is_mon,is_debug,sym = get_mon sym in
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_object ao -> 
    begin
        let is_array,is_dyn,size,ao_name =
          match ao.ao_array with
          | at :: _ ->
          begin
            true, List.mem AT_dyn at.at_flags, at.at_dim.(0),
            if (List.mem AT_dyn at.at_flags) && not is_mon then at.at_name else ao.ao_name
          end;
          | [] -> false,false,0,ao.ao_name in

        let pstart,pstop,pcall,pblocked,psel,pros = get_procs ao opro in    
        match opro with
        | Some pro ->
          if not is_mon then        
          begin
            (if List.mem pro pstart then
              [sprintf "PRO_%s_START => PRO_%s_%s_START," 
                       ao_name
                       ao_name pro.pro_name]
             else [])@
            (if List.mem pro pstop then
              [sprintf "PRO_%s_STOP => PRO_%s_%s_STOP," 
                       ao_name
                       ao_name pro.pro_name]
             else [])@
            (if List.mem pro pcall then
              [sprintf "PRO_%s_CALL => PRO_%s_%s_CALL," 
                       ao_name
                       ao_name pro.pro_name;]
             else []) @
            (if List.mem pro pblocked then
              [sprintf "PRO_%s_GD => PRO_%s_%s_GD," 
                       ao_name
                       ao_name pro.pro_name;]
             else []) @
            (if is_array && is_dyn then
             [
               sprintf "PRO_%s_SEL => PRO_%s_%s_SEL,"
                       ao_name ao_name pro.pro_name;
             ]
             else []) 
          end
          else
          [
            sprintf "MON_PRO_%s_state => MON_PRO_%s_state,"
                    ao_name ao_name;
          ]
        | None -> [];            
    end;
    | OT_array at ->
    begin
      let merge_to_one at ot =
        let merge_procs p_0 p =
            let p_0' = ref p_0 in
            List.iter (fun (sel,pro) ->
                if not (List.mem (sel,pro) !p_0') then
                    p_0' := !p_0' @ [sel,pro];
                ) p;
            !p_0' in
        let ao_0 = 
            match ot with
            | OT_object ao -> ao;
            | _ -> error 334283 "" in
        Array.iter (fun ot ->
            match ot with
            | OT_object ao ->
                ao_0.ao_procs <- merge_procs ao_0.ao_procs ao.ao_procs;
            | _ -> error 289303 "";
          ) at.at_objs;
        OT_object ao_0
        in
      let is_dyn = List.mem AT_dyn at.at_flags in
      match opro with
      | Some pro ->
        let maps= ref [] in
        Array.iter (fun ot' ->
          match ot' with
          | OT_object ao ->
            let used = ref false in
            List.iter (fun (sel',pro') ->
                used := !used or (pro = pro');
                ) ao.ao_procs;
            if !used then 
            begin
              maps := !maps @ (obj_map (Sym_obj (OT_object ao)) modu opro);
            end;
          | _ -> error 724191 "";          
          ) (if not is_dyn then at.at_objs 
                           else [|merge_to_one at at.at_objs.(0)|]);
        !maps
      | None -> error 25225 "";
    end;
    | _ -> 
        error 994286 "";
  end;
  | _ -> error 264229 ""


(*
** Object declaration in architecture header.
** pro = None -> mod_objs (used by processes of this module...)
** pro <> None -> pro_objs (process local objects)
*)
let rec obj_decl sym modu opro =
  let is_mon,is_debug,sym = get_mon sym in

  match sym with
  | Sym_pro pro ->
  begin
    let ao = pro.pro_ao in
    let is_array,is_dyn,size,ao_name,ao_num =
          match ao.ao_array with
          | at :: _ ->
          begin
            true, List.mem AT_dyn at.at_flags, at.at_dim.(0),
            (if List.mem AT_dyn at.at_flags then at.at_name else ao.ao_name),
            (
              let i = ref 0 in
              let num = ref 0 in
              Array.iter (fun ot' -> 
                match ot' with
                | OT_object ao' ->
                  if ao.ao_name=ao'.ao_name then num:= !i;
                  incr i;
                | _ -> error 841457 "";
                ) at.at_objs;
              !num
            )
          end;
          | [] -> false,false,0,ao.ao_name,0 in

    let pstart,pstop,pcall,pblocked,psel,pros = get_procs ao opro in    


    (if is_array & is_dyn & ao_num = 0 then List.map (fun pro' ->
            sprintf "signal PRO_%s_%s_START: std_logic;"
                     ao_name pro'.pro_name) pstart
     else [])@
    (if is_array & is_dyn & ao_num = 0 then List.map (fun pro' ->
            sprintf "signal PRO_%s_%s_STOP: std_logic;"
                     ao_name pro'.pro_name) pstop
     else [])@
    (if is_array & is_dyn & ao_num = 0 then List.map (fun pro' ->
            sprintf "signal PRO_%s_%s_CALL: std_logic;"
                     ao_name pro'.pro_name) pcall
     else [])@
    (if is_array & is_dyn & ao_num = 0 then List.map (fun pro' ->
            sprintf "signal PRO_%s_%s_SEL: integer;"
                     ao_name pro'.pro_name) psel
     else [])@
    (if is_array & is_dyn & ao_num = 0 then List.map (fun pro' ->
            sprintf "signal PRO_%s_%s_GD: std_logic;"
                     ao_name pro'.pro_name) pblocked
     else [])@
    [sprintf "signal PRO_%s_ENABLE: std_logic;"
             pro.pro_name;
     sprintf "signal PRO_%s_END: std_logic;"
             pro.pro_name;]@
    (List.map (fun pro' ->
            sprintf "signal PRO_%s_%s_START: std_logic;"
                     pro.pro_name pro'.pro_name) pstart)@
    (List.map (fun pro' ->
            sprintf "signal PRO_%s_%s_STOP: std_logic;"
                     pro.pro_name pro'.pro_name) pstop)@
    (List.map (fun pro' ->
            sprintf "signal PRO_%s_%s_CALL: std_logic;"
                     pro.pro_name pro'.pro_name) pcall)@
    (List.map (fun pro' ->
            sprintf "signal PRO_%s_%s_GD: std_logic;"
                     pro.pro_name pro'.pro_name) pros)
    ,[];
  end;
  | Sym_obj obj -> [],[];
  | _ -> error 171850 ""
    

(*
** Object implementation in architecture body.
*)
let rec obj_code sym modu pro =
  let vhdl_ind = ref 0 in
  let strl = ref [] in
  let vhdl str =
    let spaces n = String.make n ' ' in
    strl := !strl @ [(spaces !vhdl_ind)^str] 
    in

  match sym with
  | Sym_pro pro ->
  begin
    let pro_main = pro.pro_name = "main" in
    let ao = pro.pro_ao in
    let is_array,is_dyn,size,ao_name,ao_num =
          match ao.ao_array with
          | at :: _ ->
          begin
            true, List.mem AT_dyn at.at_flags, at.at_dim.(0),
            (if List.mem AT_dyn at.at_flags then at.at_name else ao.ao_name),
            (
              let i = ref 0 in
              let num = ref 0 in
              Array.iter (fun ot' -> 
                match ot' with
                | OT_object ao' ->
                  if ao.ao_name=ao'.ao_name then num:= !i;
                  incr i;
                | _ -> error 841457 "";
                ) at.at_objs;
              !num
            )
          end;
          | [] -> false,false,0,ao.ao_name,0 in

    let pstart,pstop,pcall,pblocked,psel,pros = get_procs ao pro in    


    vhdl "-- Process control";

    if is_array && is_dyn && ao_num = 0 then
    begin
      vhdl "-- Process control multiplexer";
      vhdl "-- Blocking collectors";
      List.iter (fun pro ->
                vhdl (sprintf "PRO_%s_%s_GD <=" ao_name pro.pro_name);
                for i = 0 to (size-1)
                do    
                    vhdl (sprintf "  PRO_%s_%d_%s_GD %s" 
                              ao_name i pro.pro_name
                              (if i <> (size-1) then "and" else ";")
                     );
                done;
                ) pblocked;
      vhdl "-- Operation selectors";
      List.iter (fun pro ->
                for i = 0 to (size-1)
                do    
                  vhdl (sprintf 
                        "PRO_%s_%d_%s_START <= '1' when PRO_%s_%s_START='1' and PRO_%s_%s_SEL=%d else '0';" 
                        ao_name i pro.pro_name 
                        ao_name pro.pro_name 
                        ao_name pro.pro_name i
                        );
                done;
                ) pstart;
      List.iter (fun pro ->
                for i = 0 to (size-1)
                do    
                  vhdl (sprintf 
                        "PRO_%s_%d_%s_STOP <= '1' when PRO_%s_%s_STOP='1' and PRO_%s_%s_SEL=%d else '0';" 
                        ao_name i pro.pro_name 
                        ao_name pro.pro_name 
                        ao_name pro.pro_name i
                        );
                done;
                ) pstop;
      List.iter (fun pro ->
                for i = 0 to (size-1)
                do    
                  vhdl (sprintf 
                        "PRO_%s_%d_%s_CALL <= '1' when PRO_%s_%s_CALL='1' and PRO_%s_%s_SEL=%d else '0';" 
                        ao_name i pro.pro_name 
                        ao_name pro.pro_name 
                        ao_name pro.pro_name i
                        );
                done;
                ) pcall;
      vhdl "";
    end;
    vhdl (sprintf "PRO_CONTROL_%s: process(" pro.pro_name);
    vhdl_ind := 8;
    List.iter (fun pro' ->
        vhdl (sprintf "PRO_%s_%s_START," pro.pro_name pro'.pro_name);
        ) pstart;
    List.iter (fun pro' ->
        vhdl (sprintf "PRO_%s_%s_STOP," pro.pro_name pro'.pro_name);
        ) pstop;
    List.iter (fun pro' ->
        vhdl (sprintf "PRO_%s_%s_CALL," pro.pro_name pro'.pro_name);
        vhdl (sprintf "PRO_%s_END," pro.pro_name);
        ) pcall;
    vhdl (sprintf "conpro_system_clk,");
    vhdl (sprintf "conpro_system_reset");
    vhdl ")";
    vhdl_ind := 0; 
    vhdl "begin";
    vhdl_ind := 2;
    if pro_main then 
    begin
        vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk = '%d' then"
                      (get_env_int "clock_level"));
        vhdl_ind := 4;
        vhdl (sprintf "if conpro_system_reset = '%d' then"
                      (get_env_int "reset_level"));
        vhdl_ind := 6;
        vhdl (sprintf "PRO_%s_ENABLE <= '1'; -- main process activated on reset"
                      pro.pro_name);
        vhdl_ind := 4;
        vhdl "end if;";
        vhdl_ind := 2;
        vhdl "end if;";                
    end
    else
    begin
        vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk = '%d' then"
                      (get_env_int "clock_level"));
        vhdl_ind := 4;
        vhdl (sprintf "if conpro_system_reset = '%d' then"
                      (get_env_int "reset_level"));
        vhdl_ind := 6;
        vhdl (sprintf "PRO_%s_ENABLE <= '0';"
                      pro.pro_name);
        List.iter (fun pro' ->
            vhdl (sprintf "PRO_%s_%s_GD <= '1';"
                      pro.pro_name pro'.pro_name);            
            ) pros;
        List.iter (fun pro' ->
            vhdl_ind := 4;
            vhdl (sprintf "elsif PRO_%s_%s_START = '1' then"
                          pro.pro_name pro'.pro_name);
            vhdl_ind := 6;
            vhdl (sprintf "PRO_%s_ENABLE <= '1';"
                          pro.pro_name);            
            vhdl (sprintf "PRO_%s_%s_GD <= '0';"
                          pro.pro_name pro'.pro_name);            
            ) pstart;
        List.iter (fun pro' ->
            vhdl_ind := 4;
            vhdl (sprintf "elsif PRO_%s_%s_STOP = '1' then"
                          pro.pro_name pro'.pro_name);
            vhdl_ind := 6;
            vhdl (sprintf "PRO_%s_ENABLE <= '0';"
                          pro.pro_name);            
            vhdl (sprintf "PRO_%s_%s_GD <= '0';"
                          pro.pro_name pro'.pro_name);            
            ) pstop;
        List.iter (fun pro' ->
            vhdl_ind := 4;
            vhdl (sprintf "elsif PRO_%s_%s_CALL = '1' and PRO_%s_END = '1' then"
                          pro.pro_name pro'.pro_name pro.pro_name);
            vhdl_ind := 6;
            vhdl (sprintf "PRO_%s_%s_GD <= '0';"
                          pro.pro_name pro'.pro_name);            
            vhdl (sprintf "PRO_%s_ENABLE <= '0';"
                          pro.pro_name);            
            ) pcall;
        List.iter (fun pro' ->
            vhdl_ind := 4;
            vhdl (sprintf "elsif PRO_%s_%s_CALL = '1' and PRO_%s_END = '0' then"
                          pro.pro_name pro'.pro_name pro.pro_name);
            vhdl_ind := 6;
            vhdl (sprintf "PRO_%s_ENABLE <= '1';"
                          pro.pro_name);            
            ) pcall;
        vhdl_ind := 4;
        vhdl "else";
        vhdl_ind := 6;
        List.iter (fun pro' ->
            vhdl (sprintf "PRO_%s_%s_GD <= '1';"
                      pro.pro_name pro'.pro_name);            
            ) pros;    
        vhdl_ind := 4;
        vhdl "end if;";
        vhdl_ind := 2;
        vhdl "end if;";                

    end;
    vhdl_ind := 0;
    vhdl (sprintf "end process PRO_CONTROL_%s;" ao.ao_name);
    vhdl "";
    !strl
  end;
  | Sym_obj obj -> [];
  | _ -> error 420036 ""
  
(*
** Synthesize one instruction or instructions from a block list
** and create linear MicroCode list. 
*)

let rec instr_ucode instr id pro =
  let modu = pro.pro_module in
  let immed_id = ref 0 in
  let immed () =
      incr immed_id;
      UC_immed !immed_id in

  let rec get_mod instr =
    match instr with
    | PI_fun (src,(opl,ot),_,_) -> 
          line src; 
          let ao = ao_of_ot opl ot in
          ao.ao_type.ta_rules;
    | _ -> get_some !self;
    in
  let rec search_label li =
    match li with
    | hd::tl ->
    begin
      match hd.ui_code with
      | Label label -> UC_label label;
      | _ -> search_label tl;
    end;
    | [] -> UC_next;
    in

  let rec unroll ucll =
          match ucll with
          | ucl::tl ->
          begin
              (List.map (fun uc -> 
                  let next = 
                      match tl with
                      | ucl'::_ -> search_label ucl';
                      | [] -> UC_next;
                      in
                  match uc.ui_code with
                  | Falsejump (uo,l) -> if l = UC_next then 
                                      {uc with ui_code=Falsejump (uo,next)}
                                   else uc;
                  | Jump l -> if l = UC_next then 
                                      {uc with ui_code=Jump next}
                                   else uc;
                  | _ -> uc;
                  ) ucl) @ (unroll tl);
          end;
          | [] -> [];
      in

  (*
  ** Try to bind instructions bound by only one label pair.
  ** Actually only unguarded data transfers and expressions
  ** are allowed.
  *)
  let bind ucll id1 id2  =
    let rec bind ucll =
          match ucll with
          | ucl::tl ->
          begin
              (List.filter (fun uc -> 
                  let next = 
                      match tl with
                      | ucl'::_ -> search_label ucl';
                      | [] -> UC_next;
                      in
                  match uc.ui_code with
                  | Falsejump (uo,l) -> 
                    error 0 "\nUnexpected instruction in bounded block";
                  | Jump l -> false;
                  | Move _ -> true;
                  | Expr _ -> true;
                  | _ -> false;
                  ) ucl) @ (bind tl);
          end;
          | [] -> []; 
     in
    let label_start = 
      {
        ui_code=Label (sprintf "i%d_bind_to_%d" id1 id2);
        ui_frame=List.hd pro.pro_frame;
      }
      in
    let label_end = 
      {
        ui_code=Label (sprintf "i%d_bind_to_%d_end" id1 id2);
        ui_frame=List.hd pro.pro_frame;
      }
      in
    let ul = bind ucll in
    let bind_instr = 
      {
        ui_code=Bind (List.length ul);
        ui_frame=List.hd pro.pro_frame;
      } in
    [label_start;bind_instr] @ ul @ [label_end];
    in

  incr id;
  let id1 = !id in
  let label name = 
      {
          ui_code = Label (sprintf "i%d_%s" id1 name);
          ui_frame=List.hd pro.pro_frame;
      } in
  let next name = 
      let l = {
          ui_code = Label (sprintf "i%d_%s_end" id1 name);
          ui_frame=List.hd pro.pro_frame;
      } in
      let j = {
          ui_code = Jump UC_next;
          ui_frame=List.hd pro.pro_frame;
      } in
      [l;j]
      in

  let remove_label il = List.filter (fun i ->
          match i.ui_code with
          | Label _ -> false;
          | _ -> true ) il in
  let remove_jump il = List.filter (fun i ->
          match i.ui_code with
          | Jump _ -> false;
          | _ -> true ) il in
  let remove_label_and_jump il =
          remove_jump (remove_label il) in

  match instr with
  | PI_fun (src,(opl,ot),sel,il) ->
      line src;
      (*
      ** Actually there are only unit arguments
      *)

      (*
      ** call: if both actual process and called
      **       process have a PRO_%s_EXCEPTION
      **       register, transfer value and
      **       jump to PRO_%s_EXCEPTION if 
      **       register <> 0.
      *)
      let exc =
        if sel = "call" then
        begin
          let r1 = sprintf "PRO_%s_EXCEPTION" pro.pro_name in
          let exn = r1 in
          let r2 = sprintf "PRO_%s_EXCEPTION" (name_of_ot ot) in
          if (sym_check_obj modu.mod_objs r1) &&
             (sym_check_obj modu.mod_objs r2) then
          begin
            let r_1 = sym_get_obj modu.mod_objs r1  in
            let r_1' = PI_obj ([],r_1) in
            let r_2 = sym_get_obj modu.mod_objs r2  in
            let r_2' = PI_obj ([],r_2) in
                
            let expr = PI_assign (nilsrc (),r_1',r_2') in
            let expr' =  remove_label_and_jump ((get_some !core_rules).rl_instr_ucode expr id pro) in
            let expr_cond = PI_bool (Relational,
                                     OP_eq,
                                     r_1',
                                     PI_obj([],OT_value (V_int Int64.zero))) in
            let res,condl = expr_synth pro expr_cond (sprintf "i%d_fun" id1)  None in
            let falsejump = {
                    ui_code = Falsejump (res,UC_label exn);
                    ui_frame=List.hd pro.pro_frame;
                  } in
            expr'@ condl@[falsejump]
          end
          else
            []  
        end  
        else
          [] in
      [label "fun";{ui_code=Fun ((opl,ot),sel,[]);
                    ui_frame=List.hd pro.pro_frame;}] @ exc @ (next "fun");
  | _ -> [label "unknown"] @ (next "unknwon")

(*
** Emit monitor object
*)
let emit_mon instr modu pro sym =
  match sym with
  | Sym_mon (dbg,sym) ->
  begin
    match sym with
    | Sym_obj obj ->
    begin
        (*
        ** Monitor signals always of type logic!
        *)
        match obj with
        | OT_object ao -> []
        | OT_array at -> [];
        | _ -> error 825910 ""
    end;
    | _ -> error 825920 ""
  end;
  | _ -> error 556617 ""



(*
** Synthesize a function into state list.
*)
let fun_scode ui label next modu pro =
    let instr,block = ui.ui_code,ui.ui_frame in
    let pro_main = pro.pro_name = "main" in
    match instr with
    | Fun ((opl,ot),sel,args) ->
    begin
      let ao = ao_of_ot opl ot in
      let is_array,is_dyn,size,ato,ao_name,ao_num =
          match ao.ao_array with
          | at :: _ ->
          begin
            true, List.mem AT_dyn at.at_flags, at.at_dim.(0), Some at,
            (if List.mem AT_dyn at.at_flags then at.at_name else ao.ao_name),
            (
              let i = ref 0 in
              let num = ref 0 in
              Array.iter (fun ot' -> 
                match ot' with
                | OT_object ao' ->
                  if ao.ao_name=ao'.ao_name then num:= !i;
                  incr i;
                | _ -> error 841457 "";
                ) at.at_objs;
              !num
            )
          end;
          | [] -> false,false,0,None,ao.ao_name,0 in
      let pstart,pstop,pcall,pblocked,psel,pros = get_procs ao pro in    
      let pro_name' = ao.ao_name in

      if not is_dyn then
      begin
        match sel with
        | "start" -> 
            let dp = [
                        Data_out (sprintf   
                                    "PRO_%s_START <= '1';"
                                    ao.ao_name 
                                   );
                        Data_def (sprintf "PRO_%s_START"
                                      ao.ao_name,"'0'");                    
                ] in
            let cp = [
                    Data_cond (sprintf "PRO_%s_GD = '1'"
                                       ao.ao_name);
                    Data_cond_sens (sprintf "PRO_%s_GD" 
                                       ao.ao_name);
                ] in
            let rec s = {
                      s_name = label;
                      s_next = Branch (cp,  
                                       Next label,
                                       Next next);
                      s_data = dp;
                      s_block = block;
                    } in
            [s];
        | "stop" -> 
            let dp = [
                        Data_out (sprintf   
                                    "PRO_%s_STOP <= '1';"
                                    ao.ao_name 
                                   );
                        Data_def (sprintf "PRO_%s_STOP"
                                      ao.ao_name,"'0'");                    
                ] in
            let cp = [
                    Data_cond (sprintf "PRO_%s_GD = '1'"
                                       ao.ao_name);
                    Data_cond_sens (sprintf "PRO_%s_GD" 
                                       ao.ao_name);
                ] in
            let rec s = {
                      s_name = label;
                      s_next = Branch (cp,  
                                       Next label,
                                       Next next);
                      s_data = dp;
                      s_block = block;
                    } in
            [s]
        | "call" -> 
            let dp = [
                        Data_out (sprintf   
                                    "PRO_%s_CALL <= '1';"
                                    ao.ao_name 
                                   );
                        Data_def (sprintf "PRO_%s_CALL"
                                      ao.ao_name,"'0'");                    
                ] in
            let cp = [
                    Data_cond (sprintf "PRO_%s_GD = '1'"
                                       ao.ao_name);
                    Data_cond_sens (sprintf "PRO_%s_GD" 
                                       ao.ao_name);
                ] in
            let rec s = {
                      s_name = label;
                      s_next = Branch (cp,  
                                       Next label,
                                       Next next);
                      s_data = dp;
                      s_block = block;
                    } in
            [s];
        | _ -> 
                error 0 
                (sprintf "Process: unknown method <%s> found in object <%s>"
                sel ao.ao_name);
      end
      else 
      begin
        let at = get_some ato in
        (*
        ** Dynamic array selector access 
        *)
        let sel_ot = 
          if is_sel_obj opl then
          begin
            match obj_sel_obj opl with
            | PI_obj (opl',ot) -> ot;
            | _ -> error 858138 ""; 
          end
          else
          begin
            let n = at_index at (obj_sel opl) in
            OT_value (V_int (Int64.of_int n));
          end in
        let is_local = sym_check_obj pro.pro_objs (name_of_ot sel_ot) in
        let sel_flags =   
           if is_local then [
             UO_rhs;
             UO_loc; (* local or temporary register ! *)
           ] else [UO_rhs] in
        let sel_params =
           [
           ] in
                                                                 
        let expr_dt = 
          match (get_some (dt_of_ot sel_ot)) with
          | DT_logic n -> DT_natural n;
          | DT_int n -> DT_natural n;
          | _ -> DT_natural 0 in     
        let ud_sel = ud_of_ot sel_ot sel_params sel_flags expr_dt in
             
        let vdr = vhdl_of_ud pro None ud_sel None in
        match sel with
        | "start" -> 
            let sel = [
                        Data_out (sprintf "PRO_%s_SEL <= %s;"
                                          ao_name vdr.dp_sig);
                        Data_def (sprintf "PRO_%s_SEL"
                                          ao_name,"0");
               
              ] @
              (List.map (fun str -> Data_sens str) vdr.dp_sen) @
              (List.map (fun str -> Data_top_def str) vdr.top_def) @ 
              (List.map (fun str -> Data_top str) vdr.top_expr) in
            let dp = [
                        Data_out (sprintf   
                                    "PRO_%s_START <= '1';"
                                    ao_name 
                                   );
                        Data_def (sprintf "PRO_%s_START"
                                          ao_name,"'0'");                    
                ] @ sel in
            let cp = [
                    Data_cond (sprintf "PRO_%s_GD = '1'"
                                       ao_name);
                    Data_cond_sens (sprintf "PRO_%s_GD" 
                                            ao_name);
                ] in
            let rec s = {
                      s_name = label;
                      s_next = Branch (cp,  
                                       Next label,
                                       Next next);
                      s_data = dp;
                      s_block = block;
                    } in
            [s];
        | "stop" -> 
            let sel = [
                        Data_out (sprintf "PRO_%s_SEL <= %s;"
                                          ao_name vdr.dp_sig);
                        Data_def (sprintf "PRO_%s_SEL"
                                          ao_name,"0");
               
              ] @
              (List.map (fun str -> Data_sens str) vdr.dp_sen) @
              (List.map (fun str -> Data_top_def str) vdr.top_def) @ 
              (List.map (fun str -> Data_top str) vdr.top_expr) in
            let dp = [
                        Data_out (sprintf   
                                    "PRO_%s_STOP <= '1';"
                                    ao_name 
                                   );
                        Data_def (sprintf "PRO_%s_STOP"
                                          ao_name,"'0'");                    
                ] @ sel in
            let cp = [
                    Data_cond (sprintf "PRO_%s_GD = '1'"
                                       ao_name);
                    Data_cond_sens (sprintf "PRO_%s_GD" 
                                       ao_name);
                ] in
            let rec s = {
                      s_name = label;
                      s_next = Branch (cp,  
                                       Next label,
                                       Next next);
                      s_data = dp;
                      s_block = block;
                    } in
            [s]
        | "call" -> 
            let sel = [
                        Data_out (sprintf "PRO_%s_SEL <= %s;"
                                          ao_name vdr.dp_sig);
                        Data_def (sprintf "PRO_%s_SEL"
                                          ao_name,"0");
               
              ] @
              (List.map (fun str -> Data_sens str) vdr.dp_sen) @
              (List.map (fun str -> Data_top_def str) vdr.top_def) @ 
              (List.map (fun str -> Data_top str) vdr.top_expr) in
            let dp = [
                        Data_out (sprintf   
                                    "PRO_%s_CALL <= '1';"
                                    ao_name 
                                   );
                        Data_def (sprintf "PRO_%s_CALL"
                                          ao_name,"'0'");                    
                ] @ sel in
            let cp = [
                    Data_cond (sprintf "PRO_%s_GD = '1'"
                                       ao_name);
                    Data_cond_sens (sprintf "PRO_%s_GD" 
                                            ao_name);
                ] in
            let rec s = {
                      s_name = label;
                      s_next = Branch (cp,  
                                       Next label,
                                       Next next);
                      s_data = dp;
                      s_block = block;
                    } in
            [s];
        | _ -> 
                error 0 
                (sprintf "Process: unknown method <%s> found in object <%s>"
                sel ao.ao_name);
      
      end;
    end;
    | _ -> error 161 ""

(*
** Toplevel instructions of module modu or process pro.
*)
let emit_top_code instr modu pro =
    let top_expr = ref [] in
    let top_def = ref [] in
    begin
      match instr with
      | PI_monitor (src,obj,_) ->
            List.iter (fun sd ->
                match sd with
                | Data_top s -> top_expr := !top_expr @ [s];
                | Data_top_def s -> top_def := !top_def @ [s];
                | _ -> ();
                ) (emit_mon instr modu pro obj);
     | _ -> ();
    end;
    !top_expr, !top_def
    
let fun_compile modu pro instr top =
    ()

let bf_time modu proo f =
    match f with
    | PI_fun (src,(opl,ot),"call",il) ->
    begin
        let ao = ao_of_ot opl ot in
        let pro_name' = ao.ao_name in
        let pro' = List.find (fun p ->
            p.pro_name = pro_name') modu.mod_procs in
        match pro'.pro_instr with
        | [PI_block (_,bf)] -> bf.bf_time
        | _ -> error 648139 "";       
    end;
    | _ ->
      FT_min 1
                                                              
let rec rules = {
    rl_name = "Process";
    rl_my = my;
    rl_obj_port = obj_port;
    rl_obj_map = obj_map;
    rl_obj_decl = obj_decl;
    rl_obj_code = obj_code;
    rl_instr_ucode = instr_ucode;
    rl_types = [{
        ta_name = "process";
        ta_rules = rules; 
        ta_flags = [];
    }];
    rl_methods = [
      "start",[];
      "stop",[];
      "call",[];
    ];
    rl_fun_compile = fun_compile;
    rl_fun_scode = fun_scode;
    rl_top_vcode = emit_top_code;
    rl_time = bf_time;
    rl_new_obj = (fun _ _ _ -> rules);
    rl_interp = (fun _ -> "");
    rl_child=None;
}

let init () =
    out "Init: Process.";
    self := Some rules;
    process_rules := Some rules;
    sym_add top_syms (Sym_rule rules);
    
