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
**    $CREATED:     16.4.2006
**    $VERSION:     2.08
**
**    $INFO:
**
**  Utils
**
**    $ENDOFINFO
**
*)

open Unix
open Printf

open Cp_syntax
open Cp_types
open Cp_symbol
open Cp_common
open Cp_utils_1
open Cp_utils_2
open Cp_utils_3
open Cp_utils_4
open Cp_utils_5
open Cp_data_core
open Cp_vhdl
   
(*
** Extract rule module from object
*)
let get_rules obj =
    let get_rs rs n =
      match rs with
      | Some r -> r;
      | None ->
        error 645574 (sprintf "get_rules: missing rules for <%s>" n);
      in 
    let v co = () in
    let rec iter obj =
      match obj with
      | OT_const co -> v co; get_rs co.co_rules co.co_name;
      | OT_named_value _ -> get_some !core_rules;
      | OT_signal co -> v co; get_rs co.co_rules co.co_name;
      | OT_reg co -> v co; get_rs co.co_rules co.co_name;
      | OT_var co -> v co; get_rs co.co_rules co.co_name;
      | OT_channel ch -> let co = ch.ch_obj in
                        v co; get_rs co.co_rules co.co_name;
      | OT_queue qu -> let co = qu.qu_obj in
                        v co; get_rs co.co_rules co.co_name;
      | OT_object ao -> ao.ao_type.ta_rules;
      | OT_array at -> if List.mem AT_temp at.at_flags then
                        get_some !core_rules
                       else iter at.at_objs.(0);
      | OT_array_sel (at,sel) -> iter at.at_objs.(sel);
      | OT_struct st -> iter (List.hd st.st_objs);
      | OT_component st -> iter (List.hd st.st_objs);      
      | _ -> error 604606 ""
      in
    iter obj

(*
** Return state name list for process pro.
*)
let get_state_names pro = 
    let rec states sbl =
        match sbl with
        | sb::tl -> 
        begin
            match sb with
            | State s ->  s.s_name :: (states tl);
            | State_block sbl' -> (states sbl') @ (states tl);
            | State_top _ -> states tl;
        end;
        | [] -> []
        in
    states pro.pro_states

(*
** Object monitor?
*)
let is_mon sym =
  match sym with
  | Sym_mon _ -> true;
  | _ -> false

let get_mon sym =
  match sym with
  | Sym_mon (dbg,mon) -> true,dbg,mon;
  | _ -> false,false,sym




(*
** Transform any value logic string to default zero value string
*)
let to_default str =
  let len = String.length str in
  let str' = String.copy str in
  match str.[0] with
  | '\'' ->
    for i = 1 to len-2 
    do
      str'.[i] <- '0';
    done;
    str'
  | '"' ->
  begin
    match str.[1] with
    | 'X' ->
      for i = 2 to len-2 
      do
        str'.[i] <- '0';
      done; 
      str'    
    | _ -> 
      for i = 1 to len-2 
      do
        str'.[i] <- '0';
      done;
      str'
  end;
  | _ -> progerr "to_default: unexpected logic string"

(*
** Return default value for object
*)
let default_by_name pro name =
    if sym_check_obj pro.pro_objs name then
    begin
        let ot = sym_get_obj pro.pro_objs name in
        match co_of_ot ot with
        | Some co -> val_str co.co_type (V_int Int64.zero);
        | None -> error 883203 (sprintf 
                         "default_by_name: unknown OT in <%s>" name);
    end
    else if sym_check_obj pro.pro_import name then
    begin
        let ot = sym_get_obj pro.pro_import name in
        match co_of_ot ot with
        | Some co -> val_str co.co_type (V_int Int64.zero);
        | None -> error 285336 (sprintf 
                         "default_by_name: unknown OT in <%s>" name);
    end
    else
        error 380566 (sprintf "default_by_name: OT <%s> not found" name)

let default_by_obj obj =
    match obj with
    | PI_obj (opl,ot) ->
    begin
        match co_of_ot ot with
        | Some co -> val_str co.co_type (V_int Int64.zero);
        | None -> error 32910 "";
    end;
    | _ -> error 588190 ""

let default_by_dt dt = val_str dt (V_int Int64.zero)
    
let to_lower str =
    let str' = String.copy str in
    let len = String.length str in
    for i = 0 to len-1
    do
        match str.[i] with
        | 'A'..'Z' -> str'.[i] <- char_of_int ((int_of_char str.[i]) +32);
        | _ -> (); 
    done;
    str'

(*
** emit IEEE library usage
*)
let ieee () =
  List.filter (fun str -> str <> "") 
    (List.map (fun name -> vhdl_map name) 
      ["libIEEE";"useIEEE1";"useIEEE2";"useIEEE3";"useIEEE4";"useIEEE5";"useIEEE6";"useIEEE7";"useIEEE8"])

let get_block_params pi_bl =
    match pi_bl with
    | PI_block (_,bf) -> bf.bf_params;
    | _ -> error 987111 ""


let name_of_modgen modgen =
    match modgen with
    | Mod_mult _ -> "multiplier";
    | Mod_add _ -> "adder";
    | Mod_shift _ -> "shifter";
    | Mod_mux _ -> "multiplexer";
    | Mod_ram _ -> "ram"

let find_modgen modgen =
  let rec iter li =
    match li with
    | hd::tl ->
        if (name_of_modgen hd) = modgen then
        begin
            match hd with
            | Mod_mult v -> v;
            | Mod_add v -> v;
            | Mod_shift v -> v;
            | Mod_mux v -> v;
            | Mod_ram v -> v;
        end
        else iter tl;
    | [] -> "auto" in

  iter compiler.t_modgens



(*
** Some utils
*)
let rec is_local ud =
  match ud with
  | UC_sig uo 
  | UC_reg uo 
  | UC_chan uo
  | UC_queue uo
  | UC_var uo -> List.mem UO_loc uo.uo_flags;
  | UC_array ua -> List.mem UO_loc ua.uat_flags;
  | UC_sel us -> is_local us.us_obj;
  | UC_alu _ -> true;
  | UC_list ul -> List.mem UO_loc ul.ul_flags;
  | _ -> true

let rec is_ot_local pro ot =
      match pro with
      | Some pro ->
      begin
        let pro_local = 
          (sym_check_sym pro.pro_objs (Sym_obj ot)) ||
          (List.mem (name_of_ot ot) (List.map (fun co -> co.co_name) pro.pro_temps)) in
        if is_mod_pro pro.pro_name then false else
        match ot with
        | OT_var co -> false
            (*
            ** No matter, RAM block is
            ** always implemented with
            ** non embedded block
            *)
            (*
            ** Place of data block decides!
            
            let db =
                match co.co_block with
                | Some db -> db;
                | None -> error 608513 ""; in
            sym_check_sym pro.pro_objs (Sym_block db)
            *)
        | OT_signal co 
        | OT_reg co ->
          (List.mem Obj_local co.co_flags) ||
          pro_local;
        | OT_array at ->
        begin 
          match at.at_objs.(0) with
          | OT_var co -> is_ot_local (Some pro) (OT_var co);
          | _ -> pro_local;
        end;
        
        | _ ->
            pro_local;
      end;
      | None -> false 




let fix_range str =
        (*
        ** Remove (x downto y) from string
        *)
        let pos = ref 0 in
        protect (String.iter (fun c -> if c = '(' then raise Exit 
                                        else incr pos) str);
        String.sub str 0 !pos

    
let fix_data_type lhs rhs =
        match uo_of_ud lhs with
        | Some uo ->
            if uo.uo_type.uo_expr_type <> uo.uo_type.uo_data_type then
            begin
                match uo_of_ud rhs with
                | Some uo' -> ();
                | None ->
                begin
                    match rhs with
                    | UC_temp ut -> ut.ut_type.uo_expr_type <- 
                                        uo.uo_type.uo_data_type;
                    | UC_val uv -> 
                      if uo.uo_type.uo_sub_type = None 
                        then uv.uv_type.uo_expr_type <- uo.uo_type.uo_data_type
                        else uv.uv_type.uo_expr_type <- uo.uo_type.uo_expr_type;
                    | _ -> ();
                end;
            end;
        | None ->
        begin
            match lhs with
            | UC_temp ut ->
                if ut.ut_type.uo_expr_type <> ut.ut_type.uo_data_type then
                begin
                    match uo_of_ud rhs with
                    | Some uo' -> ();
                    | None ->
                    begin
                        match rhs with
                        | UC_val uv -> 
                          if ut.ut_type.uo_sub_type = None 
                            then uv.uv_type.uo_expr_type <- ut.ut_type.uo_data_type
                            else uv.uv_type.uo_expr_type <- ut.ut_type.uo_expr_type;
                        | _ -> ();
                    end;
                end;
            | _ -> ();
        end

let fix_val_type lhs rhs =
        match uo_of_ud lhs with
        | Some uo ->
            if uo.uo_type.uo_expr_type <> uo.uo_type.uo_data_type then
            begin
                match rhs with
                | UC_val uv -> uv.uv_type.uo_expr_type <- uo.uo_type.uo_expr_type;
                | _ -> ();
            end;
        | None ->
        begin
            match lhs with
            | UC_temp ut ->
                if ut.ut_type.uo_expr_type <> ut.ut_type.uo_data_type then
                begin
                  match rhs with
                  | UC_val uv -> uv.uv_type.uo_expr_type <- ut.ut_type.uo_expr_type;
                  | _ -> ();
                end;
            | _ -> ();
        end

let is_val ud =
        match ud with
        | UC_val _ -> true;
        | _ -> false

(*
** Reorder data path list:
**  1. Data_in
**  2. Data_out
**  3. Data_default
**  4. Data_sens
**  5. Data_trans 
**
*)
let sort_data_path dpl =
  let data_in = ref [] in
  let data_out = ref [] in
  let data_def = ref [] in
  let data_sens = ref [] in
  let data_trans = ref [] in
  let data_aux = ref [] in
  List.iter (fun dp ->
    match dp with
    | Data_in _ -> data_in := !data_in  @ [dp];
    | Data_out _ -> data_out := !data_out  @ [dp];
    | Data_def _ -> data_def := !data_def  @ [dp];
    | Data_trans_def _ -> data_def := !data_def  @ [dp];
    | Data_sens _ -> data_sens := !data_sens  @ [dp];
    | Data_trans_sens _ -> data_sens := !data_sens  @ [dp];
    | Data_trans _ -> data_trans := !data_trans  @ [dp];
    | _ -> data_aux := !data_aux @ [dp];
    ) dpl;
  !data_in @ !data_out @ !data_def @ !data_sens @ !data_trans @ !data_aux

(*
** Return compound (type+component) name prefix: "PREF_XXXX" => "PREF"
*)
let prefix  name =
    try
        let i = String.index name '_' in
        String.sub name 0 i
    with | _ -> name

(*
** Dummy process wrapped around module 
*)

let module_pro_main modu =
    let name = sprintf "MOD_%s" modu.mod_name in
    let temp_pro_main = {
            pro_name = name;
            pro_module = modu;
            pro_syntax = T_empty;
            pro_objs = modu.mod_objs;
            pro_import = modu.mod_import;
            pro_export = modu.mod_export;
            pro_temps = [];
            pro_instr = modu.mod_instr;
            pro_ucode = [];
            pro_states = [];
            pro_alu = [];
            pro_constr = [];
            pro_frame = [];
            pro_ao = 
              {
                ao_name = name;
                ao_module = modu;
                ao_type = {
                    ta_name = "process";
                    ta_rules = (
                        match !process_rules with
                        | Some rl -> rl;
                        | None -> error 353394 "");
                    ta_flags = [];
                };
                ao_procs = [];
                ao_array = [];
                ao_struct = [];
                ao_obj = None;
                ao_flags = [];
                ao_objs = [];
                ao_domain = modu.mod_name;
                ao_env = [];
              };
            pro_domain = modu.mod_name;
            pro_control = {
              pro_start = [];
              pro_stop = [];
              pro_call = [];
              pro_raise = [];
              pro_catch = [];
            };
        } in
    temp_pro_main

(*
**
*)
let get_int_of_str str =
  try
    int_of_string str
  with
    _ -> error 0 (sprintf "\nUnexpected integer value: <%s>!"
                          str)


(*
** Returns all intermediate logic vector string ranging from la to lb.
** Format: "0010"
*)
let logic_range ra rb =
  let ral = Str.split (Str.regexp " ") ra in
  let rbl = Str.split (Str.regexp " ") rb in
  let ra = List.find (fun str -> str <> "") ral in
  let rb = List.find (fun str -> str <> "") rbl in
  let rl = ref [] in
  let dna = (String.length ra)-2 in
  let dnb = (String.length rb)-2 in
  if dna <> dnb then error 550841 "";
  let ra' = String.sub ra 1 dna in
  let rb' = String.sub rb 1 dnb in
  let i64 = ref (Int64.of_string (sprintf "0b%s" ra')) in
  let i64b = Int64.of_string (sprintf "0b%s" rb') in
  rl := !rl @ [val_str (DT_logic dna) (V_int !i64)];
  while !i64 <> i64b 
  do
    i64 := Int64.add !i64 (Int64.one);  
    rl := !rl @ [val_str (DT_logic dna) (V_int !i64)];
  done;
  !rl
  
let int_range ra rb =
  let ral = Str.split (Str.regexp " ") ra in
  let rbl = Str.split (Str.regexp " ") rb in
  let ra = List.find (fun str -> str <> "") ral in
  let rb = List.find (fun str -> str <> "") rbl in
  let rl = ref [] in
  let conv = "to_signed" in
  let rae = List.hd (Str.split (Str.regexp conv) ra) in 
  let rbe = List.hd (Str.split (Str.regexp conv) rb) in 
  let rael = Str.split (Str.regexp ",") rae in
  let rbel = Str.split (Str.regexp ",") rbe in
  let rav = 
    let e = List.nth rael 0 in
    let en = String.length e in
    String.sub e 1 (en-1) in   
  let rbv = 
    let e = List.nth rbel 0 in
    let en = String.length e in
    String.sub e 1 (en-1) in   
  let radn = 
    let e = List.nth rael 1 in
    let en = String.length e in
    String.sub e 0 (en-1) in   
  let rbdn = 
    let e = List.nth rbel 1 in
    let en = String.length e in
    String.sub e 0 (en-1) in   
  let i64,i64b=ref (Int64.of_string rav),Int64.of_string rbv in
  let dna,dnb=int_of_string radn,int_of_string rbdn in
  if dna <> dnb then error 550842 "";
  rl := !rl @ [val_str (DT_int dna) (V_int !i64)];
  while !i64 <> i64b 
  do
    i64 := Int64.add !i64 (Int64.one);  
    rl := !rl @ [val_str (DT_int dna) (V_int !i64)];
  done;
  !rl





(*
** Create block constrain structure from block parameter list.
** The new parameter list bpl is compared with the parameters in
** the head of the actual block constraint list bcl, unchanged
** parameters are copied to the new constraint block (inheritated).
*)
let constr_of_block name bpl bcl =
    let bpl' = ref bpl in
    let bp_name bp =
        match bp with
        | BP_unroll -> "unroll";
        | BP_bind -> "bind";
        | BP_expr _ -> "expr";
        | BP_temp _ -> "temp";
        | BP_alu_min_width _ -> "alu_thres";
        | BP_alu_ops _ -> "alu_ops";
        | BP_alu_type _ -> "alu_type";
        | BP_schedule _ -> "schedule";
        | BP_inline -> "inline"
        | BP_locked -> "locked" in
    let bp_names = List.map bp_name bpl in
    (
        match bcl with
        | bc::_ ->
            List.iter (fun bp ->
                if not (List.mem (bp_name bp) bp_names) then
                    bpl' := !bpl' @ [bp];
                ) bc.bc_params;
        | [] -> ();
    );
    {
        bc_name=name;
        bc_src = source ();
        bc_params = !bpl';
    }

let print_block_constr bc =
    out (sprintf "<%s>" bc.bc_name);
    List.iter (fun bp ->
        match bp with
        | BP_unroll -> out ">> Loop unrolling: enabled";
        | BP_expr et -> out (sprintf ">> Expression type: %s" (str_of_expr_type et));
        | BP_temp et -> out (sprintf ">> Temporary register: %s" et);
        | BP_alu_min_width n -> out (sprintf ">> ALU data width threshold: %d" n);
        | BP_alu_ops opl ->
            let n = ref (List.length opl) in
            let str = ref "" in
            List.iter (fun op ->
                decr n;
                str := !str ^ (sprintf "%s,"  (op_name op));
                if !n > 0 then str := !str ^ ",";
              ) opl;
            out (sprintf ">> Preferred ALU operations: %s" !str);
        | BP_inline -> out ">> Control path inline arithmetic: enabled";
        | BP_schedule sm -> 
                out (sprintf ">> Schedule mode: %s"
                                (match sm with
                                 |Sched_auto -> "auto";
                                 |Sched_custom cl -> "custom";
								 |Sched_def -> "user,static";
                        ));
        | _ -> ();
        ) bc.bc_params


let expr_model pro =
    let model = ref EXPR_flat in
    List.iter (fun p ->
        match p with
        | BP_expr m  -> model := m;
        | _ -> ();
        ) (List.hd pro.pro_constr).bc_params;
    !model    

let add_module mdname =
  if (sym_check_rule top_syms mdname) then
  begin
    let rules = sym_get_rule top_syms mdname in
    if not (List.mem rules an.a_modu.mod_rules) then
    begin
      an.a_modu.mod_rules <- an.a_modu.mod_rules @ [rules];
      out (sprintf "Added module <%s>." mdname);
      (*
      ** Add abstract type definitions to module symbols.
      *)
      List.iter (fun t ->
        sym_add an.a_modu.mod_objs (Sym_type (Type_abstract t));
        ) rules.rl_types;
    end;
    true
  end
  else false
    
                                                                                                                                                                                                                                                                                                        
