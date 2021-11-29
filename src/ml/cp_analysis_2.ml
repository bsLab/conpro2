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
**    $INITIAL:     (C) 2006-2010 BSSLAB
**    $CREATED:     5.3.2006
**    $VERSION:     2.17
**
**    $INFO:
**
**  Analysis part of compiling: syntax and structure compiling.
**
**    $ENDOFINFO
**
*)


open Cp_syntax
open Cp_types
open Cp_symbol
open Cp_common
open Cp_utils
open Cp_data
open Cp_expr
open Cp_printtypes

open Unix
open Printf

open Cp_analysis_1

let init_analysis an =
  an.a_curpos<-nilsrc();
  an.a_funpos<-nilsrc();
  an.a_toplevel<-false;
  an.a_pro_syms<-None;
  an.a_pro_import<-None;
  an.a_pro_export<-None;
  an.a_pro_name<-"";
  an.a_pro_subst<-[];
  an.a_pro_num<-0;
  an.a_fun_subst<-[];
  an.a_fun_syms<-None;
  an.a_fun_name<-"";
  an.a_procs_to_compile<-[];
  an.a_block_level<-0
  
let init_modu an mname filemap =
  an.a_mname <- mname;
  an.a_modu.mod_name <- to_mod mname;
  an.a_modu.mod_objs <- Hashtbl.create 100;
  an.a_modu.mod_export <- Hashtbl.create 30;
  an.a_modu.mod_procs <- [];
  an.a_modu.mod_external <- [];
  an.a_modu.mod_import <- Hashtbl.create 100;
  an.a_modu.mod_rules <- [];
  an.a_modu.mod_instr <- [];
  an.a_modu.mod_fmap <- an.a_modu.mod_fmap @ filemap;
  an.a_modu.mod_flags <- []

(*
** T_ident substition (for example in loop unrolling)
*)

let subst_ident str =
  let str' = ref str in
  List.iter (fun (str_i,str_o) ->
    if str_i = str then str' := str_o;
    ) an.a_pro_subst;
  !str'


    
(*
** Returns abstract object structure of a core object.
*)
let core_abstract_object name =
      {
        ao_name = name;
        ao_module = an.a_modu;
        ao_type = {
            ta_name = "Core";
            ta_rules = (
                match !core_rules with
                | Some rl -> rl;
                | None -> error 253394 "");
            ta_flags = [];
        };
        ao_procs = [];
        ao_array = [];
        ao_struct = [];
        ao_obj = None;
        ao_flags = [];
        ao_objs = [];
        ao_domain = ".";
        ao_env = [];
      } 


      
let source () = an.a_curpos 


(*
** Get name string
*)
let rec get_name vname =
  match vname with
  | T_ident (pos,str) -> 
    if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos; 
    if str = "" then error 0 "\nEmpty identifier found.";
    subst_ident str;
  | T_character (pos,c) ->
    if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos; 
    sprintf "'%c'" c;
  | T_string (pos,s) ->
    if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos; 
    sprintf "\"%s\"" s;
  | T_empty -> "";
  | T_selector (obj,sel) -> get_name obj;
  | T_sub (vname,vrange) -> get_name vname;
  | T_proc -> "process";
  | T_id -> if compiler.t_C or compiler.t_ml then "#" else sprintf "%d" an.a_pro_num;
  | T_z -> "Z";
  | T_OP_bool  (_,_,op1,op2)
  | T_OP_arith (_,op1,op2) ->
    (*
    ** Required for array selectors - just find first 
    ** data object...
    *)
    let nop1 = get_name op1 in
    let nop2 = get_name op2 in
    if nop1 <> "" then nop1 
    else if nop2 <> "" then nop2
    else error 0 "\nCan't resolve data object in expression!";
  | T_interval _ -> "%";
  | T_reg | T_var | T_sig | T_chan | T_que -> error 0 "\nFound unexpected object specifier keyword (reg,sig,...) instead of name.";
  | _ -> error 0 "\nFound unexpected name identifier." 

let get_names vnamel =
        match vnamel with
        | T_list sl -> List.map get_name sl;
        | _ -> [get_name vnamel]

(*
** Get extended name string with subrange and selectors.
*)
let rec get_name_ext vname =
  match vname with
  | T_ident (pos,str) -> 
    if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos; 
    if str = "" then error 0 "\nUnexpected identifier found.";
    subst_ident str;
  | T_empty -> "";
  | T_selector (obj,sel) -> 
    sprintf "%s_%s" (get_name_ext obj) 
                      (get_name_ext sel);
  | T_sub (vname,vrange) -> sprintf "%s[%s]"  (get_name_ext vname)
                                              (get_name_ext vrange);
  | T_proc -> "process";
  | T_id -> sprintf "%d" an.a_pro_num;
  | T_z -> "Z";
  | T_OP_bool  (_,_,op1,op2)
  | T_OP_arith (_,op1,op2) ->
    (*
    ** Required for array selectors - just find first 
    ** data object...
    *)
    let nop1 = get_name_ext op1 in
    let nop2 = get_name_ext op2 in
    if nop1 <> "" then nop1 
    else if nop2 <> "" then nop2
    else error 0 "\nCan't resolve data object in expression!";
  | T_interval _ -> "%";
  | _ -> error 0 "\nFound unexpected name."


(*
** Return a value.
*)
let rec get_val vval =
  let get_int64 v =
      match v with
      | V_int i64 -> i64;
      | _-> error 0 "get_val: Unexpected value found." in

  let add_val op op1 op2 =
      let v1 = get_int64 op1 in
      let v2 = get_int64 op2 in
      match op with
      | "+" -> V_int (Int64.add v1 v2);
      | "-" -> V_int (Int64.sub v1 v2);
      | "*" -> V_int (Int64.mul v1 v2);
      | "/" -> V_int (Int64.div v1 v2);
      | "lsl" -> V_int (Int64.shift_left v1 (Int64.to_int v2));
      | "lsr" -> V_int (Int64.shift_right v1 (Int64.to_int v2));
      | "lor" -> V_int (Int64.logor v1 v2);
      | "land" -> V_int (Int64.logand v1 v2);
      | "^" -> if v1 = (Int64.of_int 2) then V_int (Int64.shift_left Int64.one (Int64.to_int v2))
               else error 0 (sprintf "get_val: Unsupported base in operator <%s> in constant expression found." op);
      | "~" -> if v2 = (Int64.of_int 2) then V_int (Int64.of_int (log2 (Int64.to_int v1)))
               else error 0 (sprintf "get_val: Unsupported base in operator <%s> in constant expression found." op);
      | _ -> error 0 (sprintf "get_val: Unexpected operator <%s> in constant expression found." op) in

  match vval with
  | T_ident (pos,str) ->
      let str = subst_ident str in
      if str <> "" then
      begin
          if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos;
          let len = String.length str in
          match str with
          | "true" -> V_bool true;
          | "false" -> V_bool false;
          | _ ->
          (*
          ** Value or already defined constant identifier?
          *)
          if sym_check_obj an.a_modu.mod_objs str then
          begin
            match sym_get_obj an.a_modu.mod_objs str with
            | OT_named_value (_,v) -> v;
            | OT_const co -> co.co_init;
            | _ -> error 0 (sprintf "get_val: Invalid value object: %s." str);
          end
          else if len > 2 then
          begin
              let fmt = String.sub str 0 2 in
              match fmt with
              | "0B"
              | "0b" 
              | "0L"
              | "0l" ->
                  V_logic (String.sub str 2 (len-2));

              | "0X"
              | "0x" 
              | _ -> 
                  (*
                  ** Must be an integer value!
                  *)
                  let w = ref Int64.zero in
                  let ok = protects (w := Int64.of_string str) in
                  if not ok then error 0 (sprintf "get_val: Invalid value or unknown identifier: %s ." str);
                  V_int !w
          end
          else
          begin
              (*
              ** Must be an integer value!
              *)
              let w = ref Int64.zero in
              let ok = protects (w := Int64.of_string str) in
              if not ok then error 0 (sprintf "get_val: Invalid value or unknown identifier: %s." str);
              V_int !w
          end;
      end
      else
          error 0 (sprintf "get_val: empty value identifier?");
  | T_empty -> V_null;
  | T_id -> V_int (Int64.of_int an.a_pro_num);
  | T_character (pos,c) -> 
      if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos;
      V_char c;
  | T_z -> V_z;
  | T_OP_arith (op,op1,op2) when op1 <> T_empty ->
          let v1 = get_val op1 in
          let v2 = get_val op2 in
          add_val op v1 v2;
  | T_OP_arith (op,op1,op2) when op1 = T_empty ->
          let v2 = get_val op2 in
          add_val op (V_int Int64.zero) v2;
  | _ -> error 245199 ""

(*
** Return integer value
*)
let rec get_int vval =
  match vval with
  | T_ident (pos,str) ->
      let str = subst_ident str in
      if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos;
      let w = ref 0 in
      let ok = protects (w := int_of_string str) in
      if not ok then 
      begin
          (*
          ** Named value?
          *)
          let ot =
              if sym_check_obj an.a_modu.mod_objs str then
                  sym_get_obj an.a_modu.mod_objs str    
              else if sym_check_obj_ext an.a_modu str then
                  sym_get_obj_ext an.a_modu str
              else
                  error 0 "get_int: invalid data format or undefined constant name.";
              in
          match ot with
          | OT_named_value (name,V_int i64) -> w := Int64.to_int i64;
          | OT_const co -> 
          begin
              match co.co_init with
              | V_int i64 -> w := Int64.to_int i64;
              | _ -> error 0 "get_int: unexpected integer value";
          end;
          | _ -> error 0 "get_int: unexpected integer value";
      end;
      !w
  | T_id -> an.a_pro_num;
  | T_empty -> 0;
  | T_OP_arith (op,op1,op2) -> 
  begin
    match op with
    | "+" -> (get_int op1)+(get_int op2) 
    | "-" -> (get_int op1)-(get_int op2) 
    | "*" -> (get_int op1)*(get_int op2) 
    | "/" -> (get_int op1)/(get_int op2) 
    | _ -> error 0 (sprintf "Unexpected arithmetic operator <%s> in constant expression found." op)
  end;
  | _ -> error 0 "Unknown object in constant expression found."

let ast_is_int vval =
  try
  begin
    match vval with
    | T_ident (pos,str) ->
        let str = subst_ident str in
        if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos;
        let w = ref 0 in
        let ok = protects (w := int_of_string str) in
        if not ok then 
        begin
            (*
            ** Named value?
            *)
            let ot =
                if sym_check_obj an.a_modu.mod_objs str then
                    sym_get_obj an.a_modu.mod_objs str    
                else if sym_check_obj_ext an.a_modu str then
                    sym_get_obj_ext an.a_modu str
                else
                    raise Exit;
                in
            match ot with
            | OT_named_value (name,V_int i64) -> true;
            | OT_const co -> 
            begin
                match co.co_init with
                | V_int i64 -> true
                | _ -> false
            end;
            | _ -> false
        end
        else
          true
    | T_id -> true;
    | T_empty -> true;
    | _ -> false;
  end
  with _ -> false

(*
** Get parameters from syntax format tree.
** Returns [name,value] list.
*)
let get_param_list  vp =
  let params = ref [] in
  let name = ref None in
  let value = ref None in

  let rec par_list is =
    let add str =
        if !name = None then name := Some str
        else if !value = None then value := Some str;
        if !name <> None && !value <> None then
        begin
          params := !params @ [get_some !name, get_some !value];
          name := None;
          value := None;
        end;
        in
    let rec get_ident is =
      match is with
      | T_ident (src,str) -> str;
      | T_string (src,str) -> str;
      | T_selector (op1,op2) -> sprintf "%s.%s" (get_ident op1) (get_ident op2)
      | _ -> error 0 "parameter: invalid name format"; in

    match is with
    | T_OP_bool (kind,op,op1,op2) ->
    begin
        match op with
        | "=" ->
        begin
              add (get_ident op1); 
              (
                match op2 with
                | T_ident (src,str) -> 
                  add (string_of_int (get_int op2));
                | T_string (src,str) -> add str;
                | T_Fun (name,args,_) -> 
                  let str = ref (sprintf "%s(" (get_ident name)) in
                  let first = ref true in
                  List.iter (fun arg ->
                    let argname = get_ident arg in
                    str := sprintf "%s%s%s" !str (if !first then "" else ",") argname;
                    first := false;
                    ) args;
                  str := sprintf "%s)" !str;
                  add !str;
                | _ -> error 0 "parameter: invalid value format";
              );    
        end;
        | "and" ->
              (
                match op1 with
                | T_ident _  
                | T_string _ 
                | T_selector _ -> 
                  (* short case for boolean parameters *)
                  let str = get_ident op1 in
                  params := !params @ [str,"true"]; 
                | T_OP_bool _ -> par_list op1;
                | _ -> error 0 "\nparameter: invalid format";
              );
              (
                match op2 with
                | T_ident (src,str) -> params := !params @ [str,"true"]; 
                | T_string (src,str) -> params := !params @ [str,"true"]; 
                               (* short case for boolean parameters *)
                | T_OP_bool _ -> par_list op2;
                | _ -> error 0 "\nparameter: invalid format";
              );    
        | _ -> error 0 "\nInvalid parameter specification format found."; 
    end;
    | T_selector _
    | T_ident _ -> 
      (* short case for boolean parameters *)
      let str = get_ident is in
      params := !params @ [str,"true"]; 
    | _ -> error 0 "\nInvalid parameter specification format found."; in
  match vp with
  | T_empty -> []
  | _ ->
    par_list vp;
    if !name <> None or !value <> None then
      error 0 "Unbalanced <name=value> parameter specification format found.";
    !params
    
(*
** Returns parameter environment 
*)
let get_param_env vp =
  let params = get_param_list vp in
  List.map (fun (n,v) ->
    n,(
      if v <> "" then
      begin
        match v.[0] with
        | '$' ->  ENV_var (String.sub v 1 ((String.length v)-1))
        | '0' .. '9' -> 
          ENV_int (try Int64.of_string v with _-> 
                      error 0 (sprintf "Unexpected format of parameter <%s>=<%s> found." n v))
        | _ -> ENV_str v
      end
      else ENV_str ""
    )) params
 
let filter_env modname envl =
  let envl' = ref [] in
  let rec cat strl =
    match strl with
    | hd :: [] -> hd;
    | hd :: tl -> sprintf "%s.%s" hd (cat tl);
    | [] -> "" in
  List.iter (fun (n,e) ->
      match Str.split (Str.regexp "\.") n with
      | [n] -> ()
      | modname' :: tl -> 
        if modname' = modname then
          envl' := !envl' @ [cat tl,e];
      | [] -> ()
    ) envl;
  !envl'
  
(*
** Convert all parameter strings ("n"="v") to block parameters
*)
let to_param_list pal =
        let s2i s = 
            try 
                int_of_string s 
            with
                _ -> error 0 (sprintf "\ninvalid parameter number %s" s) in
        let s2b s = 
            match s with
            | "true" -> true;
            | "false" -> false;
            | _ -> error 0 "\nunknown block parameter value" in
        let pl = ref [] in
        List.iter (fun (p,v) -> 
            match p with
            | "alu_thres" -> 
            begin
                let thres = ref 0 in
                let is_num = protects (thres := int_of_string v) in
                if is_num then
                    pl := !pl @ [BP_alu_min_width !thres]
                else
                    error 0 (sprintf "\nInvalid alu_thres=<%s> setting." v); 
            end;
            | "expr" ->
            begin
              pl := !pl @ [BP_expr (expr_type_of_str v)];
            end;
            | "temp" ->
            begin
              match v with
              | "shared"
              | "not_shared" -> pl := !pl @ [BP_temp v];
              | _ -> error 0 (sprintf "\nInvalid temp=<%s> setting." v);

            end;
            | "alu_op" ->
            begin
                let op = 
                    try op_type v 
                    with _ -> 
                        error 0 (sprintf "\nInvalid alu_op=<%s> setting." v);
                    in
                let rec add li =
                    match li with
                    | BP_alu_ops ops::tl -> BP_alu_ops (op::ops) :: tl;
                    | hd::tl -> hd::(add tl);
                    | [] -> [BP_alu_ops [op]] in
                pl := add !pl;
            end;
            | "unroll" -> if s2b v then pl := !pl @ [BP_unroll];
            | "inline" -> if s2b v then pl := !pl @ [BP_inline];
            | "bind" -> if s2b v then pl := !pl @ [BP_bind];
            | "schedule" ->
            begin
              let vl = Str.split (Str.regexp ",") v in
              match vl with
              | ["auto"] -> 
			  	pl := !pl @ [BP_schedule Sched_auto];
              | ["default"] -> 
			  	pl := !pl @ [BP_schedule Sched_def];
			  | vl ->
				let schedulers = List.map (fun arg ->
				  match arg with
				  | "refstack" -> Sched_refstack;
				  | "expr" -> Sched_expr;
				  | "basicblock" -> Sched_basicblock;
				  | _ -> error 0 (sprintf "Invalid custom scheduler <%s> setting." arg) ) vl in
				pl := !pl @ [BP_schedule (Sched_custom schedulers)]; 
          end;
            | _ -> error 0 (sprintf "\nUnknown parameter name <%s> found."
                                    p);
            ) pal;
        !pl 



(*
** Return time value and unit
*)
let get_time tvu =
    match tvu with
    | T_time (v,u) ->
        get_int v,u
    | T_empty -> 0,Cycles
    | T_ident _ -> get_int tvu,Cycles
    | _ -> error 249830 "" 

(*
** Check for constant value name string
*)
let check_const name =
        match name with
        | "true" | "false" -> true;
        | _ ->
        begin
            match name.[0] with
            | '0'..'9' -> true;
            | '\'' -> true;
            | _ -> false;
        end

(*
** Get constant value
*)
let get_const name = OT_value (get_val name) 

(*
** Return symbolic name of data type (applies only to Type_const)
*)
let get_type_name vtype =
  let dt_of_str str w =
      match str with
      | "logic"
      | "int" 
      | "string" 
      | "char" 
      | "bool" -> "";
      | _ -> 
      begin
          (*
          ** Check user defined types 
          *)
          match an.a_pro_syms with
          | Some syms ->
          begin
              let tp =
                  if sym_check_type syms str then
                      sym_get_type syms str 
                  else if sym_check_type an.a_modu.mod_objs str then
                      sym_get_type an.a_modu.mod_objs str 
                  else if sym_check_type_ext an.a_modu str then
                      sym_get_type_ext an.a_modu str 
                  else
                      error 0 (sprintf "Can't find data type <%s>." str); in
              match tp with
              | Type_const tc -> tc.tc_name;
              | Type_struct ts -> "";
              | Type_bit tb -> "";
              | Type_abstract ta -> "";
          end;    
          | None -> 
              let tp = 
                  if sym_check_type an.a_modu.mod_objs str then
                      sym_get_type an.a_modu.mod_objs str 
                  else if sym_check_type_ext an.a_modu str then
                      sym_get_type_ext an.a_modu str
                  else
                      error 0 
                          (sprintf "\nunknown type <%s>" str); in
              match tp with
              | Type_const tc -> tc.tc_name;
              | Type_struct ts -> "";
              | Type_bit tb -> "";
              | Type_abstract ta -> "";
      end in
  match vtype with
  | T_ident (pos,str) -> 
  begin
      if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos;
      dt_of_str (subst_ident str) 1;
  end;
  | T_sub (vtype,vrange) ->
  begin
      let stype = get_name vtype in
      let srange = 
        let try_val = get_name vrange in
        match try_val.[0] with
        | '0'..'9' -> try_val;
        | _ -> 
          (*
          ** Perhaps constant value name...
          *)
          let cv =
            let str = try_val in
            match an.a_pro_syms with
            | Some syms ->
            begin
              if sym_check_obj syms str then
                  sym_get_obj syms str 
              else if sym_check_obj an.a_modu.mod_objs str then
                  sym_get_obj an.a_modu.mod_objs str 
              else if sym_check_obj_ext an.a_modu str then
                  sym_get_obj_ext an.a_modu str 
              else
                  error 0 (sprintf "Can't find object <%s>." str);
            end;    
            | None -> 
               if sym_check_obj an.a_modu.mod_objs str then
                   sym_get_obj an.a_modu.mod_objs str 
               else if sym_check_obj_ext an.a_modu str then
                   sym_get_obj_ext an.a_modu str
               else
                   error 0 
                       (sprintf "Can't find object <%s>" str); in

          match cv with
          | OT_const co -> str_of_val co.co_init;
          | OT_named_value (name,v) -> str_of_val v; 
          | _ ->  error 0 (sprintf "Unexpected object <%s> found." try_val)
          in

      let w = ref 0 in
      let d = ref 1 in
      let srange',sdiv=
        let strl = Str.split (Str.regexp "%") srange in
        let n= List.length strl in
        match n with
        | 1 -> List.hd strl,"";
        | 2 -> List.nth strl 0,
               List.nth strl 1;
        | _ -> error 0 "get_type: invalid range.";
        in


      let ok = protects (w := int_of_string srange') in
      if not ok then error 0 "get_type: invalid range.";

      if sdiv <> "" then
      begin
        let ok = protects (d := int_of_string sdiv) in
        if not ok then error 0 "get_type: invalid range.";
      end;
      (dt_of_str stype !w);
  end;
  | T_block _ -> ""
  | _ -> error 0 (sprintf "Unexpected type identifier found:\n %s." (ast_sprint_syntax vtype))
  
(*
** Return data type <DT>
*)
let get_type vtype =
  let dt_of_str str w =
      match str with
      | "logic" -> DT_logic w;
      | "int" -> DT_int w;
      | "string" -> DT_string w;
      | "char" -> DT_char;
      | "bool" -> DT_bool;
      | _ -> 
      begin
          (*
          ** Check user defined types 
          *)
          match an.a_pro_syms with
          | Some syms ->
          begin
              let tp =
                  if sym_check_type syms str then
                      sym_get_type syms str 
                  else if sym_check_type an.a_modu.mod_objs str then
                      sym_get_type an.a_modu.mod_objs str 
                  else if sym_check_type_ext an.a_modu str then
                      sym_get_type_ext an.a_modu str 
                  else
                      error 0 (sprintf "Can't find data type <%s>." str); in
              match tp with
              | Type_const tc -> tc.tc_type;
              | Type_struct ts -> DT_object ts.ts_name;
              | Type_bit tb -> DT_object tb.tb_name;
              | Type_abstract ta -> DT_object ta.ta_name;
          end;    
          | None -> 
              let tp = 
                  if sym_check_type an.a_modu.mod_objs str then
                      sym_get_type an.a_modu.mod_objs str 
                  else if sym_check_type_ext an.a_modu str then
                      sym_get_type_ext an.a_modu str
                  else
                      error 0 
                          (sprintf "\nunknown type <%s>" str); in
              match tp with
              | Type_const tc -> tc.tc_type;
              | Type_struct ts -> DT_object ts.ts_name;
              | Type_bit tb -> DT_object tb.tb_name;
              | Type_abstract ta -> DT_object ta.ta_name;
      end in
  match vtype with
  | T_ident (pos,str) -> 
  begin
      if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos;
      dt_of_str (subst_ident str) 1,1;
  end;
  | T_sub (vtype,vrange) ->
  begin
      let stype = get_name vtype in
      let srange = 
        let fold vrange =
          match vrange with
          | T_OP_arith (op,op1,op2) ->
          begin
            (*
            ** This must be constant expression,
            ** fold it.
            *)
            string_of_int (get_int vrange)
          end;
          | T_ident _ ->
          begin
            let try_val = get_name vrange in
            match try_val.[0] with
            | '0'..'9' -> try_val;
            | _ -> 
              (*
              ** Perhaps constant value name...
              *)
              let cv =
                let str = try_val in
                match an.a_pro_syms with
                | Some syms ->
                begin
                  if sym_check_obj syms str then
                      sym_get_obj syms str 
                  else if sym_check_obj an.a_modu.mod_objs str then
                      sym_get_obj an.a_modu.mod_objs str 
                  else if sym_check_obj an.a_modu.mod_import str then
                      sym_get_obj an.a_modu.mod_import str 
                  else
                      error 0 (sprintf "Can't find object <%s>." str);
                end;    
                | None -> 
                   if sym_check_obj an.a_modu.mod_objs str then
                       sym_get_obj an.a_modu.mod_objs str 
                   else if sym_check_obj an.a_modu.mod_import str then
                       sym_get_obj an.a_modu.mod_import str
                   else
                       error 0 
                           (sprintf "Can't find object <%s>" str); in
              match cv with
              | OT_const co -> str_of_val co.co_init;
              | OT_named_value (name,v) -> str_of_val v; 
              | _ ->  error 0 (sprintf "Unexpected object <%s> found." try_val)
            end
            | _ -> error 0 (sprintf "Unexpected statement in range specifier found.") in
          fold vrange in


      let w = ref 0 in
      let d = ref 1 in
      let srange',sdiv=
        let strl = Str.split (Str.regexp "%") srange in
        let n= List.length strl in
        match n with
        | 1 -> List.hd strl,"";
        | 2 -> List.nth strl 0,
               List.nth strl 1;
        | _ -> error 0 "get_type: invalid range.";
        in


      let ok = protects (w := int_of_string srange') in
      if not ok then error 0 "get_type: invalid range.";

      if sdiv <> "" then
      begin
        let ok = protects (d := int_of_string sdiv) in
        if not ok then error 0 "get_type: invalid range.";
      end;
      (dt_of_str stype !w),!d;
  end;
  | T_block _ -> 
    (*
    ** Process array!
    *)
    DT_object "process",0
  | _ -> error 0 (sprintf "Unexpected type identifier found:\n %s." (ast_sprint_syntax vtype))

(*
** Return port type
*)
let get_port vtype =
        let pname = get_name vtype in
        match pname with
        | "input" -> PT_in;
        | "output" -> PT_out;
        | "inout" | "bus" -> PT_bus;
        | _ -> error 0 "\ninvalid port name."

let new_obj name dt = 
      {
        co_name = name;
        co_module = an.a_modu;
        co_rules = None;
        co_process = None;
        co_type = dt;
        co_type_name = "";
        co_level = an.a_block_level;
        co_init = V_null;
        co_default = V_int Int64.zero;
        co_guard = None;    (* resolved during synthesis *)
        co_reader = [];
        co_writer = [];
        co_array = [];
        co_struct = [];
        co_index = 0;
        co_size = 1;
        co_subsize=1;
        co_block = None;
        co_flags = [];
        co_bittype = None;
        co_domain = an.a_modu.mod_name;
      } 

let get_ts str =
        match an.a_pro_syms with
        | Some syms ->
          if sym_check_type syms str then
          begin
            match sym_get_type syms str with
            | Type_struct ts -> ts;
            | _ -> error 104283 "";
          end
          else if sym_check_type an.a_modu.mod_objs str then
          begin
            match sym_get_type an.a_modu.mod_objs str with
            | Type_struct ts -> ts;
            | _ -> error 471470 "";
          end
          else if sym_check_type_ext an.a_modu str then
          begin
            match sym_get_type_ext an.a_modu str with
            | Type_struct ts -> ts;
            | _ -> error 543211 "";
          end
          else
            error 483882 ""; 
        | None -> 
          if sym_check_type an.a_modu.mod_objs str then
          begin
            match sym_get_type an.a_modu.mod_objs str with
            | Type_struct ts -> ts;
            | _ -> error 680301 "";
          end
          else if sym_check_type_ext an.a_modu str then
          begin
            match sym_get_type_ext an.a_modu str with
            | Type_struct ts -> ts;
            | _ -> error 228203 "";
          end
          else
            error 805987 ""
            
let get_tb str =
        match an.a_pro_syms with
        | Some syms ->
          if sym_check_type syms str then
          begin
            match sym_get_type syms str with
            | Type_bit tb -> tb;
            | _ -> error 104284 "";
          end
          else if sym_check_type an.a_modu.mod_objs str then
          begin
            match sym_get_type an.a_modu.mod_objs str with
            | Type_bit tb -> tb;
            | _ -> error 471471 "";
          end
          else if sym_check_type_ext an.a_modu str then
          begin
            match sym_get_type_ext an.a_modu str with
            | Type_bit tb -> tb;
            | _ -> error 543212 "";
          end
          else
            error 483882 ""; 
        | None -> 
          if sym_check_type an.a_modu.mod_objs str then
          begin
            match sym_get_type an.a_modu.mod_objs str with
            | Type_bit tb -> tb;
            | _ -> error 680303 "";
          end
          else if sym_check_type_ext an.a_modu str then
          begin
            match sym_get_type_ext an.a_modu str with
            | Type_bit tb -> tb;
            | _ -> error 228204 "";
          end
          else
            error 805986 ""

let check_tb str =
        match an.a_pro_syms with
        | Some syms ->
          if sym_check_type syms str then
          begin
            match sym_get_type syms str with
            | Type_bit tb -> true;
            | _ -> false;
          end
          else if sym_check_type an.a_modu.mod_objs str then
          begin
            match sym_get_type an.a_modu.mod_objs str with
            | Type_bit tb -> true;
            | _ -> false;
          end
          else if sym_check_type_ext an.a_modu str then
          begin
            match sym_get_type_ext an.a_modu str with
            | Type_bit tb -> true;
            | _ -> false;
          end
          else
            false; 
        | None -> 
          if sym_check_type an.a_modu.mod_objs str then
          begin
            match sym_get_type an.a_modu.mod_objs str with
            | Type_bit tb -> true;
            | _ -> false;
          end
          else if sym_check_type_ext an.a_modu str then
          begin
            match sym_get_type_ext an.a_modu str with
            | Type_bit tb -> true;
            | _ -> false;
          end
          else
            false


(*
** Create structure object. All elements of the structure are
** either signals, registers or variables.
*)
let get_struc syn =
        match syn with
        | T_OT_sig (vnamel,vtype,vinit) ->
        begin
            let vname = List.hd vnamel in
            let name = get_name vname in
            let tname = get_name vtype in
            let ts = get_ts tname in
            let st = {
                st_name = name;
                st_type = ts;
                st_objs = [];
                st_connect = [];
                st_array = None;
              } in
            List.iter (fun el ->
                let el_name = sprintf "%s_%s" name el.te_name in
                let dt,_= el.te_type in
                let co = new_obj el_name dt in
                co.co_process <- an.a_pro;
                co.co_struct <- [st];
                st.st_objs <- st.st_objs @ [OT_signal co];
              ) ts.ts_elems;
            debug "get_struc" with (sprintf "get_struc: created OT_struct with %d elements"
                                  (List.length st.st_objs));
            st
        end;
        | T_OT_reg (vnamel,vtype,vinit,vparams) ->
        begin
            let vname = List.hd vnamel in
            let name = get_name vname in
            let tname = get_name vtype in
            let ts = get_ts tname in
            let st = {
                st_name = name;
                st_type = ts;
                st_objs = [];
                st_connect = [];
                st_array = None;
              } in
            List.iter (fun el ->
                let el_name = sprintf "%s_%s" name el.te_name in
                let dt,_= el.te_type in
                let co = new_obj el_name dt in
                co.co_process <- an.a_pro;
                co.co_struct <- [st];
                st.st_objs <- st.st_objs @ [OT_reg co];
              ) ts.ts_elems;
            debug "get_struc" with (sprintf "get_struc: created OT_struct with %d elements"
                           (List.length st.st_objs));
            st
        end;
        | T_OT_var (vnamel,vtype,vblock,vinit,vparams) ->
        begin
            let vname = List.hd vnamel in
            let name = get_name vname in
            let tname = get_name vtype in
            let params = get_param_list vparams in
            let block = get_name vblock in 
            let ts = get_ts tname in
            let st =             {
                st_name = name;
                st_type = ts;
                st_objs = [];
                st_connect = [];
                st_array = None;
              } in

            List.iter (fun el ->
                let el_name = sprintf "%s_%s" name el.te_name in
                let dt,div= el.te_type in
                let co = new_obj el_name dt in
                co.co_subsize <- div;
                co.co_process <- an.a_pro;
                co.co_struct <- [st];
                st.st_objs <- st.st_objs @ [OT_var co];
              ) ts.ts_elems;
            debug "get_struc" with (sprintf "get_struc: created OT_struct with %d elements"
                           (List.length st.st_objs));
            st
        end;
        | T_OT_comp (vnamel,vtype,vinit) ->
        begin
            let vname = List.hd vnamel in
            let name = get_name vname in
            let tname = get_name vtype in
            let ts = get_ts tname in
            let connect =
              if vinit <> [] then
              begin
                if (List.length vinit) <> (List.length ts.ts_elems) then
                    error 0 (sprintf "Length of connect list differs from component list <%s>, got %d, expected %d."
                                    name (List.length vinit) (List.length ts.ts_elems)); 
                List.map2 (fun el vc -> 
                  let cn = get_name_ext vc in
                  let obj = if sym_check_obj an.a_modu.mod_objs cn then 
                              sym_get_obj an.a_modu.mod_objs cn
                            else
                              error 0 (sprintf "Can't find connect symbol <%s> in component <%s> list." cn name) in
                  obj  
                  ) ts.ts_elems vinit;
              end else [] in
            let st = {
                st_name = name;
                st_type = ts;
                st_objs = [];
                st_connect = connect;
                st_array = None;
              } in
            List.iter (fun el ->
                let el_name = sprintf "%s_%s" name el.te_name in
                let dt,_= el.te_type in
                let co = new_obj el_name dt in
                co.co_process <- an.a_pro;
                co.co_struct <- [st];
                st.st_objs <- st.st_objs @ [OT_signal co];
                match el.te_port with
                | Some p ->
                    co.co_flags <- co.co_flags @ (
                        match p with
                        | PT_in -> [Obj_port_in];
                        | PT_out -> [Obj_port_out];
                        | PT_bus -> [Obj_port_in;Obj_port_out];
                        );        
                | None -> ();
              ) ts.ts_elems;
            debug "get_struc" with (sprintf "get_struc: created OT_component with %d elements"
                           (List.length st.st_objs));
            st
        end;
        | T_OT_que (vnamel,vtype,vparams) ->
        begin
            let vname = List.hd vnamel in
            let name = get_name vname in
            let tname = get_name vtype in
            let params = get_param_list vparams in
            let dt,_ = get_type vtype in 
            let ao = core_abstract_object name in

            let depth = ref 8 in
            List.iter (fun (n,v) ->
              match n with
              | "depth" -> depth := get_int_of_str v;
              | _ -> error 0 (sprintf "\nUnknown queue <%s> parameter <%s=%s>!"
                                      name n v);
              ) params;

            
            let ts = get_ts tname in
            let st = {
                st_name = name;
                st_type = ts;
                st_objs = [];
                st_connect = [];
                st_array = None;
              } in
            List.iter (fun el ->
                let el_name = sprintf "%s_%s" name el.te_name in
                let dt,_= el.te_type in
                let co = new_obj el_name dt in
                co.co_process <- an.a_pro;
                let qu = { qu_obj = co;
                           qu_ot = None;
                           qu_depth = !depth;
                           qu_ao = ao} in
                co.co_struct <- [st];
                st.st_objs <- st.st_objs @ [OT_queue qu];
              ) ts.ts_elems;
            debug "get_struc" with (sprintf "get_struc: created OT_struct with %d elements"
                           (List.length st.st_objs));
            st
        end;
        | T_OT_chan (vnamel,vtype,vparams) ->
        begin
            let vname = List.hd vnamel in
            let name = get_name vname in
            let tname = get_name vtype in
            let params = get_param_list vparams in
            let dt,_ = get_type vtype in 
            let ao = core_abstract_object name in

            let mo = ref Chan_unbuffered in
            List.iter (fun (n,v) ->
              match (n,v) with
              | "model","buffered" -> mo := Chan_buffered;
              | "model","unbuffered" -> mo := Chan_unbuffered;
              | _ -> error 0 (sprintf "\nUnknown channel <%s> parameter <%s=%s>!"
                                      name n v);
              ) params;

            
            let ts = get_ts tname in
            let st = {
                st_name = name;
                st_type = ts;
                st_objs = [];
                st_connect = [];
                st_array = None;
              } in
            List.iter (fun el ->
                let el_name = sprintf "%s_%s" name el.te_name in
                let dt,_= el.te_type in
                let co = new_obj el_name dt in
                co.co_process <- an.a_pro;
                let ch = { ch_obj = co;
                           ch_ot = None;
                           ch_model = [!mo];
                           ch_ao = ao} in
                co.co_struct <- [st];
                st.st_objs <- st.st_objs @ [OT_channel ch];
              ) ts.ts_elems;
            debug "get_struc" with (sprintf "get_struc: created OT_struct with %d elements"
                           (List.length st.st_objs));
            st
        end;

        | _ -> error 580538 "" 
        
(*
** Return object name.
*)
let get_obj_name tob = name_of_ot tob 
    
(*
** Find rule module for object.
*)
let assign_rule obj =
        let not_found = protects (
                List.iter (fun rl ->
                  if rl.rl_my (Sym_obj obj) then
                  begin
                    let rec assign obj =
                        match obj with
                        | OT_const co -> co.co_rules <- Some rl;
                        | OT_signal co -> co.co_rules <- Some rl;
                        | OT_reg co -> co.co_rules <- Some rl;
                        | OT_var co -> co.co_rules <- Some rl;
                        | OT_channel ch -> ch.ch_obj.co_rules <- Some rl;
                        | OT_queue qu -> qu.qu_obj.co_rules <- Some rl;
                        | OT_object _ -> ();
                        | OT_array at -> Array.iter assign at.at_objs;
                        | OT_struct st -> List.iter assign st.st_objs;
                        | OT_component st -> List.iter assign st.st_objs;
                        | OT_named_value _ -> ();
                        | _ -> error 678084 "";
                        in
                    assign obj;
                    raise Exit;
                  end;
                ) an.a_modu.mod_rules) in
        if not_found then
            error 0 (sprintf "\nCan't find rules for object <%s>. Maybe module Core not opened!"
                           (get_obj_name obj))

(*
** Set parameters. The coding is something blurred using
** T_OP_bool operations.
** Format:  <x>=<v> [and <y>=<v> ...]
*)
let set_param tparam ve =
        let params = ref [] in
        let name = ref None in
        let value = ref None in
        
        let rec par_list is =
          let add str =
            if !name = None then name := Some str
            else if !value = None then value := Some str;
            if !name <> None && !value <> None then
            begin
              params := !params @ [get_some !name, get_some !value];
              name := None;
              value := None;
            end;
            in
            
          match is with
          | T_OP_bool (kind,op,op1,op2) ->
          begin
                match op with
                | "=" ->
                begin
                  (
                    match op1 with
                    | T_ident (src,str) -> add (subst_ident str);
                    | T_OP_bool _ -> par_list op1;
                    | _ -> error 0 (sprintf "\nset_param: invalid format in parameters in <%s>"
                                            tparam);
                  );
                  (
                    match op2 with
                    | T_ident (src,str) -> add (subst_ident str);
                    | T_OP_bool _ -> par_list op2;
                    | _ -> error 0 (sprintf "\nset_param: invalid format in parameters in <%s>"
                                            tparam);
                  );    
                end;
                | "and" ->
                  (
                    match op1 with
                    | T_ident (src,str) -> add (subst_ident str);
                    | T_OP_bool _ -> par_list op1;
                    | _ -> error 0 (sprintf "\nset_param: invalid format in parameters in <%s>"
                                            tparam);
                  );
                  (
                    match op2 with
                    | T_ident (src,str) -> add (subst_ident str);
                    | T_OP_bool _ -> par_list op2;
                    | _ -> error 0 (sprintf "\nset_param: invalid format in parameters in <%s>"
                                            tparam);
                  );    
                | _ -> error 0 (sprintf "\nset_param: invalid format in parameters in <%s>"
                                            tparam);
            end;
            | _ -> error 0 (sprintf "\nset_param: invalid format in parameters in <%s>"
                                            tparam);
            in
        par_list ve;
        if !name <> None or !value <> None then
          error 0 (sprintf "\nset_param: unbalanced format in parameters in <%s>"
                           tparam);

        let type_par name =
          let rec find_name pl =
            match pl with
            | (name',value')::tl ->
              if name' = name then Some value' else find_name tl;
            | [] -> None in
          
          if name = "code" then
          begin
            match find_name !params with
            | Some "bin" | Some "binary" -> Some TP_bin;
            | Some "gray" -> Some TP_gray;
            | Some "name" | Some "statename" -> Some TP_name;
            | Some "one" | Some "onehot" -> Some TP_onehot;
            | Some name -> error 0 (sprintf 
                                "\nset_param: unknown code parameter <%s> in <%s>."
                                name tparam);
            | None -> None;
          end
          else
            error 983359 "";
          in
        let block_par name =
          let rec find_name pl remains =
            match pl with
            | (name',value')::tl ->
              if name' = name then Some value', (remains @ tl) else 
                    find_name tl ((name',value') :: tl);
            | [] -> None, remains in
          

          if name = "model" then
          begin
            let this,params' = find_name !params [] in
            params := params';
            match this with
            | Some "erew" -> Some Mp_EREW;
            | Some "crew" -> Some Mp_CREW;
            | Some "inline" -> Some Mp_inline;
            | Some "outline" -> Some Mp_outline;
            | Some "singleport" -> Some Mp_singleport;
            | Some "dualport" -> Some Mp_dualport;
            | Some "multiport" -> Some Mp_multiport;
            | Some "readsync" -> Some Mp_readsync;
            | Some "readasync" -> Some Mp_readasync;
            | Some name -> error 0 (sprintf 
                                "\nset_param: unknown model parameter <%s> in <%s>."
                                name tparam);
            | None -> None;
          end
          else if name = "schedule" then
          begin
            let this,params' = find_name !params [] in
            match this with
            | Some "fifo" -> Some (Mp_schedule "fifo");
            | _ -> None;
          end else
            None;
          in
          
        let sym = 
            if an.a_pro_syms <> None && sym_check (get_some an.a_pro_syms) tparam then
                sym_lookup (get_some an.a_pro_syms) tparam
            else if sym_check an.a_modu.mod_objs tparam then
                sym_lookup an.a_modu.mod_objs tparam
            else if sym_check_ext an.a_modu tparam then
                sym_lookup_ext an.a_modu tparam
            else
                error 0 (sprintf "\nset_param: unknown object name <%s>." tparam); 
            in
        match sym with
        | Sym_type tp ->
        begin
            match tp with
            | Type_const tc ->
            begin
                let n = List.length tc.tc_elems in
                List.iter (fun parname ->
                    let value = type_par parname in
                    match value with
                    | Some TP_onehot -> 
                        if n > 64 then 
                            error 0 "\nmodel TD_onehot not possible here.";
                        tc.tc_params <- [TP_onehot];
                        tc.tc_type <- DT_logic n;
                        let i = ref 0 in
                        List.iter (fun el ->
                          match el with
                          | OT_const co ->
                            co.co_init <- V_int (Int64.shift_left
                                                    Int64.one !i);
                            incr i;
                          | _ -> error 177816 "";
                          ) tc.tc_elems;
                    | Some TP_bin -> ();
                    | Some _ -> error 0 "\nmodel not supported.";
                    | None -> ();
                  ) ["code"];
            end;
            | Type_abstract _ 
            | Type_bit _
            | Type_struct _ -> error 0 "\nmodel settings not supported.";
            
        end;
        | Sym_block db -> 
                List.iter (fun parname ->
                    let value = block_par parname in
                    match value with
                    | Some p -> 
                    begin
                      match p with
                      | Mp_readasync -> 
                        let ps = List.filter (fun p -> p <> Mp_readsync &&
                                                       p <> Mp_readasync) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_readsync -> 
                        let ps = List.filter (fun p -> p <> Mp_readasync &&
                                                       p <> Mp_readsync) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_inline -> 
                        let ps = List.filter (fun p -> p <> Mp_outline &&
                                                       p <> Mp_inline) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_outline -> 
                        let ps = List.filter (fun p -> p <> Mp_inline &&
                                                       p <> Mp_outline) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_CREW -> 
                        let ps = List.filter (fun p -> p <> Mp_EREW &&
                                                       p <> Mp_CREW) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_EREW -> 
                        let ps = List.filter (fun p -> p <> Mp_CREW &&
                                                       p <> Mp_EREW) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_singleport -> 
                        let ps = List.filter (fun p -> p <> Mp_dualport &&
                                                       p <> Mp_singleport &&
                                                       p <> Mp_multiport) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_dualport -> 
                        let ps = List.filter (fun p -> p <> Mp_dualport &&
                                                       p <> Mp_singleport &&
                                                       p <> Mp_multiport) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_multiport -> 
                        let ps = List.filter (fun p -> p <> Mp_dualport &&
                                                       p <> Mp_singleport &&
                                                       p <> Mp_multiport) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_littleendian -> 
                        let ps = List.filter (fun p -> p <> Mp_littleendian &&
                                                       p <> Mp_bigendian) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_bigendian -> 
                        let ps = List.filter (fun p -> p <> Mp_littleendian &&
                                                       p <> Mp_bigendian) 
                                            db.db_params in
                        db.db_params <- ps @ [p];
                      | Mp_schedule _ -> ();
                    end;
                    | None -> ()
                  ) ["model";"model";"model";"model";"model";"model"];
        | _-> error 0 "\nmodel parameter for object not supported."


(*
** Evaluate  function/procedure call, return substituted procedure
** instruction list.
*)
let rec fun_subst subst instr =
  debug "fun_subst" with (sprintf "fun_subst with [subst(%d)]..." (List.length subst));
  let rec fix_fpos instr fpos =
    match instr with    
    | T_ident (_,name) -> T_ident (fpos,name);
    | T_sub (s1,s2) -> T_sub ((fix_fpos s1 fpos),(fix_fpos s2 fpos));
    | T_selector (s1,s2) -> T_selector ((fix_fpos s1 fpos),(fix_fpos s2 fpos));
    | _ -> instr in
    
  let rec check_subst instr =
    match instr with
    | T_ident (fpos,name) ->
    begin  
      let rec do_subst sl =
        match sl with
        | (parname,argnam,arginstr)::tl ->
          debug "fun_subst" with (sprintf "T_ident(%s) -> %s?" name parname);
          if parname = name then
            fix_fpos arginstr fpos
          else
            do_subst tl;
        | [] -> instr in
      do_subst subst;
    end;
    | _ -> error 969029 "" in


  match instr with
  | T_ident _ ->
    check_subst instr;
  | T_concat (op1,op2) ->
    T_concat ((fun_subst subst op1),(fun_subst subst op2));
  | T_assign (lhs,rhs) -> 
    let lhs' = fun_subst subst lhs in
    let rhs' = fun_subst subst rhs in
    T_assign (lhs',rhs')
  | T_typeconv (c,op) ->
    T_typeconv (c,(fun_subst subst op));
  | T_map (lhs,rhs) ->
    T_map ((fun_subst subst lhs),(fun_subst subst rhs));
  | T_OP_arith (op,op1,op2) ->
    T_OP_arith (op,(fun_subst subst op1),(fun_subst subst op2));
  | T_OP_bool (kind,op,op1,op2) ->
    T_OP_bool (kind,op,(fun_subst subst op1),(fun_subst subst op2));
  | T_waitfor (w1,w2,w3,w4) ->
    T_waitfor ((fun_subst subst w1),
               (fun_subst subst w2),
               (fun_subst subst w3),
               (fun_subst subst w4));
  | T_time (ident,uni) ->
    T_time ((fun_subst subst ident),uni);
  | T_freq (ident,uni) ->
    T_freq ((fun_subst subst ident),uni);
  | T_branch (b1,b2,b3) ->
    T_branch ((fun_subst subst b1),
              (fun_subst subst b2),
              (fun_subst subst b3));
  | T_forloop (f1,dir,f2,f3,f4) ->
    T_forloop ((fun_subst subst f1),
                dir,
               (fun_subst subst f2),
               (fun_subst subst f3),
               (fun_subst subst f4));
  | T_loop (kind,l1,l2) ->
    T_loop (kind, 
            (fun_subst subst l1),
            (fun_subst subst l2));
  | T_select (s1,s2) ->
    T_select ((fun_subst subst s1),
              (fun_subst subst s2));
  | T_case (c1l,c2) ->
    T_case (List.map (fun_subst subst) c1l,
            (fun_subst subst c2));
  | T_block (b1,bo) ->
    T_block ((fun_subst subst b1),
             bo);
  | T_bind b ->
    T_bind (fun_subst subst b);
  | T_instr i ->
    T_instr (fun_subst subst i);
  | T_list li ->
    T_list (List.map (fun_subst subst) li);
  | T_sub (s1,s2) ->
    T_sub ((fun_subst subst s1),
           (fun_subst subst s2));
  | T_interval (i1,i2) ->
    T_interval ((fun_subst subst i1),
                (fun_subst subst i2));
  | T_selector (s1,s2) ->
    T_selector ((fun_subst subst s1),
                (fun_subst subst s2));
  | T_Fun (f1,fl2,fret) ->
    T_Fun ((fun_subst subst f1),
           List.map (fun_subst subst) fl2,
           (fun_subst subst fret));
  | _ -> instr 
  
                                                                 
