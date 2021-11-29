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
**      BSSLAB, Dr. Stefan Bosse http://www.bsslab.de
**
**      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
**                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
**                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
**                 OUTSIDE OF THE SOFTWARE SYSTEM.
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2006-2011 BSSLAB
**    $CREATED:     18.5.2006
**    $VERSION:     2.23
**
**    $INFO:
**
**  Expression evaluation
**
**    $ENDOFINFO
**
*)
open Cp_common
open Cp_types
open Cp_syntax
open Cp_symbol
open Cp_utils
open Cp_data
open Cp_printtypes
open Cp_data_core
open Printf
open Cp_vhdl

let is_assign expr =
    match expr with
    | PI_assign _ -> true;
    | _ -> false
let split_assign expr =
    match expr with
    | PI_assign (src,lhs,rhs) -> lhs,rhs;
    | _ -> error 990073 ""
let is_sym_const ot =
  match ot with
  | OT_const co -> co.co_type_name <> ""
  | _ -> false
  
(*
** Derive expression type from object used in expression (LHS)
*)
let expr_dt_of_ot opl ot =
  match dt_of_ot ot with
  | Some dt ->
  begin
    if is_sub opl then
    begin
      let a,b= obj_sub opl in
      Some (dt_of_td (td_of_dt dt) (b-a+1))
    end
    else if is_index opl then
    begin
      Some (dt_of_td (td_of_dt dt) 1)
    end
    else Some dt;
  end;
  | None -> None

let rec is_const_expr pi =
  let rec iter pi =
    match pi with
    | PI_arithm (_,op1,op2) ->
        (iter op1) && (iter op2);
    | PI_bool (kind,_,op1,op2) ->
        ((iter op1) && (iter op2)) && kind = Bool;
    | PI_obj (_,ot) ->
    begin
        match ot with
        | OT_value _
        | OT_const _ -> true;
        | OT_named_value (n,v) -> if (compiler.t_C || compiler.t_ml) && n="#" then false else true
        | _ -> false;
    end;        
    | PI_block (il,_) -> 
    begin
      match il with
      | hd::[] -> is_const_expr hd;
      | _ -> error 785919 "";
    end;
    | _ -> error 541349 "";
    in
    iter pi

(*
** Return integer value of constant
*)
let get_const_expr pi =
    match pi with
    | PI_obj (_,ot) ->
    begin
        match ot with
        | OT_value v
        | OT_named_value (_,v) ->
        begin
          match v with
          | V_int i64 -> i64;
          | V_float f -> Int64.of_float f;
          | V_char c -> Int64.of_int (int_of_char c);
          | V_bool t -> if t then Int64.one else Int64.zero;
          | _ -> error 716609 "";
        end;
        | OT_const co -> 
        begin
          match co.co_init with
          | V_int i64 -> i64;
          | V_float f -> Int64.of_float f;
          | V_char c -> Int64.of_int (int_of_char c);
          | V_bool t -> if t then Int64.one else Int64.zero;
          | _ -> error 716610 "";
        end;
        | _ -> error 786919 "";
    end;        
    | _ -> error 733195 ""

let get_const_expri pi = Int64.to_int (get_const_expr pi)

let obj_dt' pi =
  (*
  ** Resolve ADTO type, too
  *)
  match pi with
  | PI_obj (opl,ot) ->
  begin
    match ot with
    | OT_object ao ->   
    begin
      let dt = ref None in
      List.iter (fun (fn,fv) ->
        if fn = "DT_mapped" then
        begin
          let strl = Str.split (Str.regexp " ") fv in
          match strl with
          | ["DT_logic";n] -> dt := Some (DT_logic (int_of_string n));
          | ["DT_char"] -> dt := Some DT_char;
          | ["DT_bool"] -> dt := Some DT_bool;
          | ["DT_int";n] -> dt := Some (DT_int (int_of_string n));
          | _ -> error 222183 "";
        end;
        ) ao.ao_type.ta_flags;
      match !dt with
      | Some dt -> dt;
      | None -> error 0 (sprintf 
                    "Found abstract object <%s> in expression without real data type!"
                    ao.ao_name);
                                 
    end;
    | _ -> 
      let dt = obj_dt pi in
      debug "obj_dt'" with (sprintf "obj_dt: %s\n%s" (name_of_ot ot)
                                              (sprint_dt dt));
      dt
  end;
  | _ -> obj_dt pi
    
(*
** expr(instruction) -> 
** calculate full arithmetic data type width -> 
** {OP_bool | OP_arith} * data_type
**
** If full_width is false, the largest data object within a arithmetic
** expression determines data width, else the arithmetic operation
** is concerned, too. 
*)
let expr_type expr full_width =
    let get_some_dt dts =
        match dts with
        | Some dt -> dt;
        | None -> error 826195 "";
        in
    let bound v = 
        match v with
        | V_int i64 ->
            if i64 = Int64.zero ||
               i64 = Int64.one then 1 else
                (const_width v)-1;
        | V_bool _ -> 1;
        | _ -> const_width v in


    (*
    ** Derive common (target) DT from expression. 
    ** Note: Constant values are filtered out (size=0!)
    ** Note: Relational operations return operand types, not DT_bool!
    *)
    let rec get_dt pi =
        let dt_max dt1 dt2 =
          let s1 = size_of_dt dt1 in
          let s2 = size_of_dt dt2 in
          if s1 > s2 then dt1 else dt2
          in
        let dt_add dt1 dt2 =
          let s1 = size_of_dt dt1 in
          let s2 = size_of_dt dt2 in
          let td = td_of_dt dt1 in
          dt_of_td td (s1 + s2)
          in
		let dt_of_n dt n =
		  let td = td_of_dt dt in
		  dt_of_td td n in
		  
        match pi with
        | PI_obj (opl,ot) -> 
        begin
            match ot with
            | OT_named_value (_,v)
            | OT_value v ->
            begin
                match v with
                | V_bool _ -> DT_bool;
                | _ -> DT_logic (bound v);
            end;
            | _ -> 
                let dt' = obj_dt' pi in
                if is_sub opl then
                begin
                  let a,b = obj_sub opl in
                  let n = b-a+1 in
                  dt_of_td (td_of_dt dt') n;
                end 
                else if is_index opl then
                  dt_of_td (td_of_dt dt') 1
                else dt'
        end;
        | PI_arithm (op,op1,op2) ->
		begin
            let dt1 = get_dt op1 in
            let dt2 = get_dt op2 in
			if not full_width then
			  (dt_max dt1 dt2)
			else
            match op with
			| OP_add -> dt_add (dt_max dt1 dt2) (dt_of_n dt1 1);
			| OP_mul -> dt_add dt1 dt2;
			| _ -> dt_max dt1 dt2;
			
		end;
        | PI_bool (kind,op,op1,op2) ->
        begin
            match kind with
            | Relational ->
                let dt1 = get_dt op1 in
                let dt2 = get_dt op2 in
                dt_max dt1 dt2;
            | Bool -> DT_bool;
        end;
        | PI_concat (op1,op2) ->
            let dt1 = get_dt op1 in
            let dt2 = get_dt op2 in
            let td1 = td_of_dt dt1 in
            let td2 = td_of_dt dt2 in
            if td1 <> td2 then error 0 (sprintf "Found incompatible types in concat expression.");
            dt_add dt1 dt2;                           
        | _ -> error 673520 "";
        in

    (*
    ** Derive all operators from expression
    *)
    let rec get_ops expr =
        match expr with
        | PI_arithm (op,op1,op2) -> op ::((get_ops op1) @ (get_ops op2));
        | PI_bool (kind,op,op1,op2) -> op ::((get_ops op1) @ (get_ops op2));
        | PI_obj _ -> [];
        | PI_concat _ -> [];
        | _ -> error 137393 "";
        in

    let op,dt = 
      if (is_assign expr) then
      begin
        let lhs,rhs = split_assign expr in
        (*
        ** Data type is always derived from LHS!
        *)
        let dt = get_dt lhs in
        let ops = List.map (fun op -> op_mode op) (get_ops rhs) in
        if (List.mem OP_bool ops) || (List.mem OP_relat ops) then
            OP_bool,dt
        else
            OP_arith,dt;
      end
      else
      begin
        (*
        ** Data type is derived from RHS=expr!
        *)
        let dt = get_dt expr in
        let ops = List.map (fun op -> op_mode op) (get_ops expr) in
        if (List.mem OP_bool ops) || (List.mem OP_relat ops) then
            OP_bool,dt
        else
            OP_arith,dt;
      end in
    debug "expr_type" with (sprintf "expr_type: %s" (sprint_dt dt));
    op,dt
    

(*
** Do additional checks on expressions:
**
**  1. Assignments: DT(LHS) = ALL DTs(RHS)
**  2. No nested boolean expressions in arithmetic expressions
*)
let check_expr expr =
  debug "check_expr" with "check_expr";
  
  let error _ str = raise (Synthesis str) in
  let valdt = DT_object "%V" in
  try 
  begin
    let rec check_range a b ot =
        let dt_size dt =
            match dt with
            | DT_logic n -> n;
            | DT_int n -> n;  
            | DT_string _ -> 8;
            | DT_char -> 8;
            | DT_bool -> 1;
            | DT_object _ -> 0;
            | DT_aneg | DT_lneg | DT_bneg -> 0;
            | DT_natural n -> n
            in
            
        match ot with
        | OT_const co ->
                let size = dt_size co.co_type in
                let failed = a >= size ||
                             b >= size ||
                             a < 0 ||
                             b < 0 in
                if failed then error 0 (sprintf "check_expr: range check failed for object <%s>."
                                                (sprint_ot ot));    
        | OT_signal co ->
                let size = dt_size co.co_type in
                let failed = a >= size ||
                             b >= size ||
                             a < 0 ||
                             b < 0 in
                if failed then error 0 (sprintf "check_expr: range check failed for object <%s>."
                                                (sprint_ot ot));    
        | OT_reg co ->
                let size = dt_size co.co_type in
                let failed = a >= size ||
                             b >= size ||
                             a < 0 ||
                             b < 0 in
                if failed then error 0 (sprintf "check_expr: range check failed for object <%s>."
                                                (sprint_ot ot));    
        | OT_channel ch ->
                let co = ch.ch_obj in
                let size = dt_size co.co_type in
                let failed = a >= size ||
                             b >= size ||
                             a < 0 ||
                             b < 0 in
                if failed then error 0 (sprintf "check_expr: range check failed for object <%s>."
                                                (sprint_ot ot));    
        | OT_queue qu ->
                let co = qu.qu_obj in
                let size = dt_size co.co_type in
                let failed = a >= size ||
                             b >= size ||
                             a < 0 ||
                             b < 0 in
                if failed then error 0 (sprintf "check_expr: range check failed for object <%s>."
                                                (sprint_ot ot));    
        | OT_var co ->
                let size = dt_size co.co_type in
                let failed = a >= size ||
                             b >= size ||
                             a < 0 ||
                             b < 0 in
                if failed then error 0 (sprintf "check_expr: range check failed for object <%s>."
                                                (sprint_ot ot));    
        | OT_array at -> 
                check_range a b at.at_objs.(0);
        | OT_array_sel (at,sel) -> 
                check_range a b at.at_objs.(0);
        | OT_component _ 
        | OT_struct _ 
        | OT_object _ 
        | OT_value _
        | OT_reference _
        | OT_named_value _ 
          -> error 0 (sprintf "check_expr: range not supported for object <%s>."
                     (sprint_ot ot));
        
        in

    let get_some_dt dts =
        match dts with
        | Some dt -> dt;
        | None -> error 0 "check_expr: unexpected dataobject";
        in
    (*
    ** Derive common (target)/first  DT from expression
    ** Note: Constant values are filtered out (size=0!)
    *)
    let rec get_dt pi =
        let dt_max dt1 dt2 =
            let s1 = size_of_dt dt1 in
            let s2 = size_of_dt dt2 in
            if s1 > s2 then dt1 else dt2
            in
        let dt_add dt1 dt2 =
            let s1 = size_of_dt dt1 in
            let s2 = size_of_dt dt2 in
            let td = td_of_dt dt1 in
            dt_of_td td (s1 + s2)
            in
        match pi with
        | PI_obj (opl,ot) -> 
          if is_value ot then valdt else obj_dt' pi;
        | PI_arithm (op,op1,op2) ->
            let dt1 = get_dt op1 in
            let dt2 = get_dt op2 in
            dt_max dt1 dt2;
        | PI_bool (kind,op,op1,op2) ->
            DT_bool;
        | PI_concat (op1,op2) ->
            let dt1 = get_dt op1 in
            let dt2 = get_dt op2 in
            dt_add dt1 dt2;
        | _ -> error 876050 "";
        in

    (*
    ** Compare all subexpressions with target dt type.
    *)
    let rec check_dt in_arith in_bool pi dt =
        let dt_add dt1 dt2 =
            let s1 = size_of_dt dt1 in
            let s2 = size_of_dt dt2 in
            let td = td_of_dt dt1 in
            dt_of_td td (s1 + s2)
            in
        match pi with
        | PI_obj (opl,ot) -> 
            if not (is_value ot) && not (is_const ot) then
            begin
                let dt' = obj_dt' pi in
                if (id_of_dt dt) <> (id_of_dt dt') then
                begin
                    out (pi_sprint_instr pi);
                    error 0 (sprintf "check_expr: dataobject <%s> of unexpected type: expected %s, but got %s."
                                       (name_of_ot ot) (sprint_dt dt) (sprint_dt dt'));
                end;
                if is_sub opl then
                begin
                    let (a,b) = obj_sub opl in
                    check_range a b ot;
                end;
                if is_index opl then
                begin
                    let a = obj_index opl in
                    check_range a a ot;
                end;
            end
            else
            begin
                let v = get_value ot in
                let err =
                    match dt with
                    | DT_bool -> 
                    begin
                      let rec check v =
                        match v with
                        | V_bool _ -> false;
                        | V_int v' -> v' <> Int64.zero && v' <> Int64.one;
                        | V_logic v' -> v' <> "0" && v' <> "1";
                        | V_list vl -> 
                          let b = ref false in
                          List.iter (fun v' -> b := !b or (check v')) vl;
                          !b            
                        | _  -> true in
                      check v;
                    end;
                    | DT_logic n ->
                    begin
                      let rec check v =
                        match v with
                        | V_bool _ -> n <> 1;
                        | V_int v' -> 
                            n < ((const_width v)-1);
                        | V_logic _ -> n < (const_width v);
                        | V_char _ -> n < 8;
                        | V_string _ -> n < 8;
                        | V_z -> false;
                        | V_list vl -> 
                          let b = ref false in
                          List.iter (fun v' -> b := !b or (check v')) vl;
                          !b            
                        | _  -> true in
                      check v;
                    end;
                    | DT_int n ->
                    begin
                      let rec check v =
                        match v with
                        | V_int v' -> n < (const_width v)
                        | V_char _ -> n < 8;
                        | V_string _ -> n < 8;
                        | V_list vl -> 
                          let b = ref false in
                          List.iter (fun v' -> b := !b or (check v')) vl;
                          !b            
                        | V_logic s' ->
                        begin
                          try
                          begin
                            let v' =  Int64.of_string s' in
                            n < (const_width v)
                          end with _ -> true
                        end;
                        | _  -> true in
                      check v;
                    end;
                    | _ -> false;    (* TODO *)
                    in
                if err then 
                begin
                  let dt2 = get_dt pi in
                  error 0 (sprintf "check_expr: invalid value type or constant value doesn't fit: exptected dt1=%s, but got dt2=%s"
                                    (sprint_dt dt) (sprint_dt dt2));
                end;
            end;

        | PI_arithm (op,op1,op2) ->
            check_dt true false op1 dt;
            check_dt true false op2 dt;            
        | PI_bool (kind,op,op1,op2) ->
            if in_arith then 
                error 0 "check_expr: arithmetic mode in boolean expression found.";
            if kind = Bool then
            begin
                check_dt false true op1 dt;
                check_dt false true op2 dt;
            end
            else
            begin
                (*
                ** Relational operator with operands of any type
                ** and possible nested arithmetic expressions.
                ** Use first found DT as initial new DT value...
                *)
                let dt1 = get_dt op1 in
                let dt2 = get_dt op2 in
                debug "check_expr" with (sprintf "dt1=%s dt2=%s" (sprint_dt dt1) (sprint_dt dt2)); 
                let dt = if dt1 <> valdt then dt1 else dt2 in
                check_dt false true op1 dt;
                check_dt false true op2 dt;
            end;
        | PI_concat (op1,op2) ->
            let rec get_ops pi =
                match pi with
                | PI_concat (op1,op2) ->
                begin
                    (get_ops op1)@ 
                    (get_ops op2)
                end;
                | PI_obj _ -> [pi];
                | _ -> error 165961 "" in
            let ops = get_ops pi in

            let op1,ops' = 
                match ops with
                | op::tl -> op,tl;
                | _ -> error 528799 "" in
            let dt1 = 
              let ot = ot_of_pi op1 in
              if not (is_value ot) then 
                get_dt op1 
              else
              begin
                let v = get_value ot in
                DT_logic (const_width v) 
              end in
            List.iter (fun op2 ->
                let dt2 = 
                  let ot = ot_of_pi op2 in
                  if not (is_value ot) then 
                    get_dt op2 
                  else
                  begin
                    let v = get_value ot in
                    DT_logic (const_width v) 
                  end in
                let td1 = td_of_dt dt1 in
                let td2 = td_of_dt dt2 in
                let dt' = dt_add dt1 dt2 in
                let td' = td_of_dt dt in
                if td1 <> td2 then 
                    error 0 (sprintf "check_expr: concat expression with different types: %s <> %s"
                                     (sprint_dt dt1) (sprint_dt dt2));
                if td1 <> td' then
                    error 0 (sprintf "check_expr: inconsistent types in concat expression: %s <> %s."
                                     (sprint_dt dt1) (sprint_dt dt));
            ) ops';
        | _ -> error 0 "check_expr: unexpected operand in expression found.";
        in


    let rec find_dt in_arith in_bool pi =
        let dt_add dt1 dt2 =
            let s1 = size_of_dt dt1 in
            let s2 = size_of_dt dt2 in
            let td = td_of_dt dt1 in
            dt_of_td td (s1 + s2)
            in
        match pi with
        | PI_obj (opl,ot) -> 
            let dt' = obj_dt' pi in
            debug "check_expr" with (sprintf "PI_obj dt=%s" (sprint_dt dt')); 
            if is_sub opl then
            begin
                let (a,b) = obj_sub opl in
                check_range a b ot;
            end;
            if is_index opl then
            begin
                let a = obj_index opl in
                check_range a a ot;
            end;
            if (is_value ot) || (is_sym_const ot) then valdt else dt'
        | PI_arithm (op,op1,op2) ->
            let dt1 = find_dt true false op1 in
            let dt2 = find_dt true false op2 in            
            if dt1 <> valdt && dt2 <> valdt &&
               (id_of_dt dt1) <> (id_of_dt dt2) then
                error 0 (sprintf "check_expr: different types in arithmetic expression: %s <> %s."
                                 (sprint_dt dt1) (sprint_dt dt2));
            if dt1 <> valdt then dt1 else dt2;
        | PI_bool (kind,op,op1,op2) ->
            let dt1 = find_dt false true op1 in
            let dt2 = find_dt false true op2 in
            if kind = Bool && 
               ((dt1 <> DT_bool && dt1 <> valdt) ||
                (dt2 <> DT_bool && dt2 <> valdt)) then
                error 0 (sprintf "check_expr: unexpected types in boolean expression (bool required): %s <> %s."
                                (sprint_dt dt1) (sprint_dt dt2));
            if (dt1 <> valdt && dt2 <> valdt) &&
               ((id_of_dt dt1) <> (id_of_dt dt2)) 
                then
                error 0 (sprintf "check_expr: different types in relational expression: %s [%s] <> %s [%s]."
                                 (sprint_dt dt1) (pi_sprint_instr op1) (sprint_dt dt2) (pi_sprint_instr op2));

(***************

            if kind = Bool then
                (if dt1 <> valdt then dt1 else dt2)
            else
            begin
                (*
                ** Relational operator with operands of any type
                ** and possible nested arithmetic expressions.
                ** Use first found DT as initial new DT value...
                *)
                let dt = get_dt op1 in
                if dt <> valdt then
                begin
                    check_dt false true op1 dt;
                    check_dt false true op2 dt;
                    (*
                    ** Result: always of type DT_bool!
                    *)
                    dt1
                end
                else
                begin
                    let dt = get_dt op2 in
                    check_dt false true op1 dt;
                    check_dt false true op2 dt;
                    (*
                    ** Result: always of type DT_bool!
                    *)
                    dt2
                end
            end;
**********************)
            DT_bool
        | PI_concat (op1,op2) ->
            let dt1 = find_dt true false op1 in
            let dt2 = find_dt true false op2 in            
            if (id_of_dt dt1) <> (id_of_dt dt2) then
                error 0 (sprintf "check_expr: different types in concat expression: %s <> %s."
                         (sprint_dt dt1) (sprint_dt dt2));
            dt_add dt1 dt2;
        | _ -> error 190445 "";
        in

    if (is_assign expr) then
    begin
        let lhs,rhs = split_assign expr in
        match lhs with
        | PI_tuple il ->
        begin
          match rhs with
          | PI_tuple il' -> 
            List.iter2 (fun lhs rhs -> 
              let lhs_dt = get_dt lhs in
              check_dt false false rhs lhs_dt;
              ) il il';
          | PI_fun (src,(opl,ot),sel,args) -> raise (Synthesis "check_expr: Tuple <- Fun not implemented");  
          | _ -> raise (Synthesis "check_expr: Tuple <- ? not implemented");
        end;
        | _ ->
          (*
          ** Data type is always derived from LHS!
          *)
          let lhs_dt = get_dt lhs in
          check_dt false false rhs lhs_dt;
    end
    else
    begin
        let dt = find_dt false false expr in
        check_dt false false expr dt;
    end;
    true;
  end
  with | Synthesis err -> out_ err; false


(*
**********************************************
** Constant folding in expressions           *
**********************************************
*)

let reduced = ref 0 

type op = {
  mutable op : operator;
  mutable op1: tree;
  mutable op2: tree;
}
(*
** Required for normalized tree
*)
and invert = {
  mutable inv_add: bool;
  mutable inv_mul: bool;
}
(*
** Tree consists of nodes (operators) 
** and leafes (constant or objects)
*)
and tree = 
  | Node of (invert*op)
  | Leaf of (invert*instr)

let is_tree_leaf nd =
  match nd with
  | Leaf _ -> true;
  | _ -> false 

let is_tree_value nd =
  let is_int_val v =
    match v with
    | V_int _| V_char _-> true;
    | _ -> false in
  match nd with
  | Leaf (_,PI_obj (_,OT_value v))  when (is_int_val v)-> true;
  | Leaf (_,PI_obj (_,OT_named_value (n,v))) when (is_int_val v) && not ((compiler.t_C || compiler.t_ml) && n="#")-> true;
  | Leaf (_,PI_obj (_,OT_const co)) when (is_int_val co.co_init)-> true;
  | _ -> false 

let is_tree_object nd =
  (is_tree_leaf nd) && not (is_tree_value nd)
 
let is_tree_node nd =
  not (is_tree_leaf nd)  
  
let get_tree_value lf =
  let eval_const opl v =
        (*
        ** Evaluate some special value conversions here.
        *)
        if List.mem OD_lneg opl then
        begin
            match v with
            | V_int w ->
                V_int (Int64.lognot w);
            | V_char c -> V_int (Int64.lognot 
                                      (Int64.of_int (int_of_char c)));
            | _ -> error 284858 "";
        end
        else if List.mem OD_aneg opl then
        begin
            match v with
            | V_int w ->
                V_int (Int64.neg w);
            | V_char c -> V_int (Int64.neg 
                                      (Int64.of_int (int_of_char c)));
            | _ -> error 284857 "";
        end
        else v in
  match lf with
  | Leaf (inv,PI_obj (opl,OT_named_value (_,v))) 
  | Leaf (inv,PI_obj (opl,OT_value v)) ->
    let get_int64 v =
        match v with
        | V_int i64 -> i64;
        | V_char c -> Int64.of_int (int_of_char c);
        | V_bool b -> if b then Int64.one else Int64.zero;
        | _ -> error 776203 ""; in
    get_int64 (eval_const opl v)
  | Leaf (inv,PI_obj (opl,OT_const co)) ->
    let get_int64 v =
        match v with
        | V_int i64 -> i64;
        | V_char c -> Int64.of_int (int_of_char c);
        | _ -> error 776204 ""; in
    get_int64 (eval_const opl co.co_init)
  | _ -> error 357790 "" 

let get_tree_inv tree =
  match tree with
  | Node (inv,_) -> inv;
  | Leaf (inv,_) -> inv
  
(*
** Build intermediate tree of expression
*)
let rec to_tree op pi =
  let is_val = is_tree_value in
  let inv b = {inv_add=b;inv_mul=b} in
  
  match pi with
  | PI_obj _ -> Leaf (inv false,pi);
  | PI_arithm (op',op1,op2) -> 
    let op1' = to_tree op' op1 in
    let op2' = to_tree op' op2 in
    let t = Node 
          (inv false,
            {op=op';
             op1=op1';
             op2=op2';     
            }) in
    t
  | PI_bool (kind,op',op1,op2) -> 
    let op1' = to_tree op' op1 in
    let op2' = to_tree op' op2 in
    let t = Node 
          (inv false,
            {op=op';
             op1=op1';
             op2=op2';     
            }) in
    t
  | PI_concat _ -> Leaf (inv false,pi);
  | _ -> error 241711 ""

  
let rec sprint_tree t =
  let is_val nd = is_tree_value in
  match t with
  | Node (inv,nd) ->
    let s1 = sprintf "ND(%s%s%s)" 
                      (op_name nd.op)
                      (if inv.inv_add then ",-" else "")
                      (if inv.inv_mul then ",/" else "") in
    let s2 = sprint_tree nd.op1 in
    let s3 = sprint_tree nd.op2 in
    sprintf "%s\n%s\n%s\n" s1 s2 s3
  | Leaf (inv,lf) -> 
    let name = 
      if is_tree_value t then 
        Int64.to_string (get_tree_value t)
      else
        name_of_pi lf in
    if is_tree_value t then 
      sprintf "V[%s%s%s]" 
              name
              (if inv.inv_add then ",-" else "")
              (if inv.inv_mul then ",/" else "")      
    else 
      sprintf "O[%s%s%s]" 
              name
              (if inv.inv_add then ",-" else "")
              (if inv.inv_mul then ",/" else "")

let sprint_node nd =
    let s1 = sprintf "ND(%s)" (op_name nd.op) in
    let s2 = sprint_tree nd.op1 in
    let s3 = sprint_tree nd.op2 in
    sprintf "%s\n%s\n%s\n" s1 s2 s3
                      
let rec print_tree t =
  let is_val = is_tree_value in
  match t with
  | Node (inv,nd) ->
    out (sprintf "ND(%s%s%s)" 
                 (op_name nd.op)
                 (if inv.inv_add then ",-" else "")
                 (if inv.inv_mul then ",/" else ""));
    ind_incr ();
    print_tree nd.op1;
    print_tree nd.op2;
    ind_decr ();
  | Leaf (inv,lf) -> 
    let name = 
      if is_tree_value t then 
        Int64.to_string (get_tree_value t)
      else
        name_of_pi lf in
    if is_tree_value t then 
      out (sprintf "V[%s%s%s]" 
                   name
                   (if inv.inv_add then ",-" else "")
                   (if inv.inv_mul then ",/" else ""))
    else 
      out (sprintf "O[%s%s%s]" 
                   name                      
                   (if inv.inv_add then ",-" else "")
                   (if inv.inv_mul then ",/" else ""))

let rec of_tree tree =
  let get_inv = get_tree_inv in
  let rec filter_inv_add tree =
    match tree with
    | Node (inv,nd) -> 
      Node ( {inv with inv_add=not inv.inv_add},
            {nd with 
              op1=nd.op1;
              op2=nd.op2;});
    | Leaf (inv,lf) ->
      Leaf ({inv with inv_add=not inv.inv_add},
            lf) in
  match tree with
  | Node (inv,nd) ->
  begin
    match op_mode nd.op with
    | OP_logic -> 
      PI_arithm (nd.op,of_tree nd.op1,
                       of_tree nd.op2);
    | OP_arith -> 
    begin
      let inv_a = get_inv nd.op1 in
      let inv_b = get_inv nd.op2 in
      match nd.op with
      | OP_add -> 
      begin
        match inv_a.inv_add,inv_b.inv_add with
        | (false,false) -> 
          PI_arithm (nd.op,of_tree nd.op1,
                           of_tree nd.op2);
        | (true,false) ->
          PI_arithm (OP_sub,of_tree nd.op2,
                            of_tree (filter_inv_add nd.op1));
        | (false,true) ->
          PI_arithm (OP_sub,of_tree nd.op1,
                            of_tree (filter_inv_add nd.op2));
        | (true,true) ->
          PI_arithm (OP_sub,of_tree (filter_inv_add nd.op1),
                            of_tree (filter_inv_add nd.op2));
      end;
      | _ -> 
        PI_arithm (nd.op,of_tree nd.op1,
                         of_tree nd.op2);
    end;      
    | OP_bool -> PI_bool (Bool,nd.op,of_tree nd.op1, 
                                   of_tree nd.op2);
    | OP_relat -> PI_bool (Relational,nd.op,of_tree nd.op1, 
                                         of_tree nd.op2);
  end;
  | Leaf (inv,lf) -> 
  begin
    match lf with
    | PI_obj (opl,ot) ->
      let inv_l = List.mem OD_lneg opl in
      let inv_a = List.mem OD_aneg opl in
      let opl' = List.filter (fun op -> op <> OD_lneg && op <> OD_aneg) opl in
      PI_obj (opl' @ (if (inv.inv_add || inv.inv_mul) || inv_a then [OD_aneg]
                      else if not (inv.inv_add || inv.inv_mul) && inv_l then [OD_lneg]
                      else []),ot);
    | _ -> lf
  end


(*
** Normalize tree:
**   Transform anti operations - and / into + and *.
**
**  Rules:
**    1. if there is a node with op = '-' operation, normalize_tree inv_add = true for op2
**    2. propagate inv_add downto to leaves if possible, leaf.inv_add = true
**    3. inv_add with inv_mul stops propagation; node.inv_add = true
**    4. if there is a node with op = '/' operation, normalize_tree inv_mul = true for op2
**    5. propagate inv_mul downto to leaves if possible, leaf.inv_mul = true
**    6. inv_mul with inv_add stops propagation; node.inv_mul = true
*)
let rec normalize_tree inv tree =
  let inv0 = {inv_add=false;inv_mul=false} in
  let op_inv op =
    match op with
    | OP_add -> OP_sub;
    | OP_sub -> OP_add;
    | OP_mul -> OP_div;
    | OP_div -> OP_mul;
    | _ -> op in
  match tree with
  | Node (inv',nd) ->
  begin
    match nd.op with
    | OP_sub ->
      let inv_propagate = not inv.inv_mul in
      if inv_propagate then
        Node (inv',{nd with
                    op=op_inv nd.op;
                    op1=normalize_tree inv nd.op1;
                    op2=normalize_tree {inv with inv_add = not inv.inv_add} nd.op2})
      else
        Node (inv,{nd with
                    op=nd.op;
                    op1=normalize_tree inv0 nd.op1;
                    op2=normalize_tree inv0 nd.op2})
    | OP_div ->
      let inv_propagate = not inv.inv_add in
      if inv_propagate then
        Node (inv',{nd with
                    op=op_inv nd.op;
                    op1=normalize_tree inv nd.op1;
                    op2=normalize_tree {inv with inv_mul = not inv.inv_mul} nd.op2})
      else
        Node (inv,{nd with
                    op=nd.op;
                    op1=normalize_tree inv0 nd.op1;
                    op2=normalize_tree inv0 nd.op2})
    | OP_add ->
      let inv_propagate = not inv.inv_mul in
      if inv_propagate then
        Node (inv',{nd with
                    op=nd.op;
                    op1=normalize_tree inv nd.op1;
                    op2=normalize_tree inv nd.op2})
      else
        Node (inv,{nd with
                    op=nd.op;
                    op1=normalize_tree inv0 nd.op1;
                    op2=normalize_tree inv0 nd.op2})
    | OP_mul ->
      let inv_propagate = not inv.inv_add in
      if inv_propagate then
        Node (inv',{nd with
                    op=nd.op;
                    op1=normalize_tree inv nd.op1;
                    op2=normalize_tree inv nd.op2})
      else
        Node (inv,{nd with
                    op=nd.op;
                    op1=normalize_tree inv0 nd.op1;
                    op2=normalize_tree inv0 nd.op2})
          
    | _ ->
        Node (inv,{nd with
                    op=nd.op;
                    op1=normalize_tree inv0 nd.op1;
                    op2=normalize_tree inv0 nd.op2})

  end;         
  | Leaf (inv',lf) ->
    let inv'' = { inv_add = if inv.inv_add then 
                              not inv'.inv_add 
                            else 
                              inv'.inv_add;
                  inv_mul = if inv.inv_mul then
                              not inv'.inv_mul
                            else
                              inv'.inv_mul;} in
    Leaf (inv'',lf) 

(*
** Try to propagate constant value leafes downward to the
** last nodes in the tree (lowest level of tree). 
*)


let rec propagate_tree tree =
  let is_val = is_tree_value in
  let is_obj = is_tree_object in
  let is_node = is_tree_node in
  let get_val = get_tree_value in
  
  let rec find_node op nd =
    let same op_1 op_2 =
      let op_inv op =
        match op with
        | OP_add -> OP_sub;
        | OP_sub -> OP_add;
        | OP_mul -> OP_div;
        | OP_div -> OP_mul;
        | _ -> op in
      op_1 = op_2 ||
      (op_inv op_1) = op_2 ||
      op_1 = (op_inv op_2) in    
 
    match nd with 
    | Node (inv,nd') when (nd'.op = op) ->
    begin
      if is_obj nd'.op1 then Some nd'
      else if is_obj nd'.op2 then Some nd'
      else if is_node nd'.op1 then find_node op nd'.op1
      else if is_node nd'.op2 then find_node op nd'.op2
      else None;
    end;
    | Node (inv,nd') when (same nd'.op op) ->
    begin
      if is_obj nd'.op1 then Some nd'
      else if is_obj nd'.op2 then Some nd'
      else None;
    end;
    | _ -> None in

  let swap nd1 nd2 =
    debug "propagate_tree" with (sprintf "swap{\n%s\n <-> %s}\n" (sprint_node nd1) (sprint_node nd2));
    (*
    ** Try to move constant value (Leaf) from op1 downto op2
    *)
    if is_val nd1.op1 then
    begin
      if is_obj nd2.op1 then
      begin
        let nd2' = nd2.op1 in
        nd2.op1 <- nd1.op1;
        nd1.op1 <- nd2'; 
        true
      end
      else
      begin
        if is_obj nd2.op2 then
        begin
          let nd2' = nd2.op2 in
          nd2.op2 <- nd1.op1;
          nd1.op1 <- nd2'; 
          true
        end
        else false;
      end;
    end
    else
    begin
      if is_val nd1.op2 then 
      begin
        if is_obj nd2.op1 then
        begin
          let nd2' = nd2.op1 in
          nd2.op1 <- nd1.op2;
          nd1.op2 <- nd2'; 
          true
        end
        else
        begin
          if is_obj nd2.op2 then
          begin
            let nd2' = nd2.op2 in
            nd2.op2 <- nd1.op2;
            nd1.op2 <- nd2'; 
            true
          end
          else false;
        end;          
      end
      else false;
    end in

  debug "propagate_tree" with (sprintf "propagate: {%s}\n" (sprint_tree tree));
  match tree with
  | Node (inv,nd) ->
    if is_val nd.op1 then
    begin
      (*
      ** Find next node with at least one Leaf which is
      ** an object. MAybe we're already at the end
      ** of the tree.
      *)

      let next_node = find_node nd.op nd.op2 in
      match next_node with
      | Some nd' -> 
        let succ = swap nd nd' in
        succ || (propagate_tree nd.op2); 
      | None -> 
      begin
        (*
        ** I there is a value in the next node
        ** rebuild nodes...
        *)
        match nd.op2 with
        | Node (inv',nd') when (nd.op = nd'.op) ->
        begin
          if is_val nd'.op1 then
          begin
            let nd_op1 = nd.op1 in
            let nd_op2 = nd.op2 in
            let nd'_op1 = nd'.op1 in
            let nd'_op2 = nd'.op2 in
            nd.op1 <- nd_op2;
            nd.op2 <- nd'_op2;
            nd'.op1 <- nd_op1;
            nd'.op2 <- nd'_op1;
            true
          end
          else if is_val nd'.op2 then
          begin
            let nd_op1 = nd.op1 in
            let nd_op2 = nd.op2 in
            let nd'_op1 = nd'.op1 in
            let nd'_op2 = nd'.op2 in
            nd.op1 <- nd_op2;
            nd.op2 <- nd'_op1;
            nd'.op1 <- nd_op1;
            nd'.op2 <- nd'_op2;
            true
          end
          else
            propagate_tree nd.op2;
        end;
        | _ ->
          propagate_tree nd.op2; 
      end;
    end
    else if is_val nd.op2 then
    begin
      (*
      ** Find next node with at least one Leaf which is
      ** an object. MAybe we're already at the end
      ** of the tree.
      *)

      let next_node = find_node nd.op nd.op1 in
      match next_node with
      | Some nd' -> 
        let succ = swap nd nd' in
        succ || (propagate_tree nd.op1); 
      | None -> 
      begin
        (*
        ** I there is a value in the next node
        ** rebuild nodes...
        *)
        match nd.op1 with
        | Node (inv',nd') when (nd.op = nd'.op) ->
        begin
          if is_val nd'.op1 then
          begin
            let nd_op1 = nd.op1 in
            let nd_op2 = nd.op2 in
            let nd'_op1 = nd'.op1 in
            let nd'_op2 = nd'.op2 in
            nd.op1 <- nd'_op2;
            nd.op2 <- nd_op1;
            nd'.op1 <- nd'_op1;
            nd'.op2 <- nd_op2;
            true
          end
          else if is_val nd'.op2 then
          begin
            let nd_op1 = nd.op1 in
            let nd_op2 = nd.op2 in
            let nd'_op1 = nd'.op1 in
            let nd'_op2 = nd'.op2 in
            nd.op1 <- nd'_op1;
            nd.op2 <- nd_op1;
            nd'.op1 <- nd_op2;
            nd'.op2 <- nd'_op2;
            true
          end
          else
            propagate_tree nd.op1;
        end;
        | _ ->
          propagate_tree nd.op1; 
      end;
    end
    else 
    begin
      (propagate_tree nd.op1) ||
      (propagate_tree nd.op2)
    end;
  | Leaf lf -> false 

(*
** Due constant value evaluation
*)
let compacted = ref 0 

let rec compact_tree tree =
  let is_val = is_tree_value in
  let is_obj = is_tree_object in
  let is_node = is_tree_node in
  let get_val = get_tree_value in
  let get_inv = get_tree_inv in
  let inv b = {inv_add=b;inv_mul=b} in
  let op_inv op =
    match op with
    | OP_add -> OP_sub;
    | OP_sub -> OP_add;
    | OP_mul -> OP_div;
    | OP_div -> OP_mul;
    | _ -> op in
  let eval inv nd =
    let a,inv_a = get_val nd.op1, get_inv nd.op1 in
    let b,inv_b = get_val nd.op2, get_inv nd.op2 in
    let a' = if inv_a.inv_add then Int64.neg a else a in 
    let b' = if inv_b.inv_add then Int64.neg b else b in 
    debug "compact_tree"  with (sprintf "eval A=%s B=%s" (Int64.to_string a) (Int64.to_string b));
    incr compacted;
    let inv,v = 
      let b2i b = if b then Int64.one else Int64.zero in
      match nd.op with
      | OP_add -> inv,Int64.add a' b';
      | OP_sub -> inv,Int64.sub a' b';
      | OP_max -> inv,max a' b';
      | OP_min -> inv,min a' b';
      | OP_mul -> 
      begin
        match inv_a.inv_mul,inv_b.inv_mul with
        | (false,false) ->  inv,Int64.mul a' b';
        | (true,false) ->  inv,Int64.div b' a';
        | (false,true) -> inv,Int64.div a' b';
        | (true,true) -> {inv with inv_mul=not inv.inv_mul},Int64.mul a' b'
      end;
      | OP_div -> if b = Int64.zero then error 0 "fold_tree: Division by zero.";
                  inv,Int64.div a' b';
      | OP_lor  | OP_bor -> inv,Int64.logor a b;
      | OP_land | OP_band -> inv,Int64.logand a b;
      | OP_lxor | OP_bxor -> inv,Int64.logxor a b;
      | OP_eq -> inv,b2i (a=b);
      | OP_neq -> inv,b2i (a<>b);
      | OP_le -> inv,b2i (a <= b);
      | OP_ge -> inv,b2i (a >= b);
      | OP_lt -> inv,b2i (a < b);
      | OP_gt -> inv,b2i (a > b);
      | OP_lsl -> inv,Int64.shift_left a (Int64.to_int b);
      | OP_lsr -> inv,Int64.shift_right a (Int64.to_int b);
      | _ -> error 928548 "" in
    let v',inv' = 
      if v < Int64.zero then Int64.neg v,{inv with inv_add=true}
                        else v,inv in
    Leaf (inv',PI_obj ([],OT_value (V_int v'))) in

  match tree with
  | Node (inv,nd) ->
  begin
    if is_val nd.op1 && is_val nd.op2 then
      eval inv nd
    else 
    begin
      if is_val nd.op1 then
      begin
        match nd.op with
        | OP_band -> 
          let a = get_val nd.op1 in
          if a = Int64.zero then Leaf (inv,PI_obj ([],OT_value (V_bool false)))
          else nd.op2
        | OP_bor -> 
          let a = get_val nd.op1 in
          if a = Int64.one then Leaf (inv,PI_obj ([],OT_value (V_bool true)))
          else nd.op2
        | OP_add -> 
          let a = get_val nd.op1 in
          if a = Int64.zero then nd.op2
          else
            Node (inv,{nd with op1=compact_tree nd.op1;
                               op2=compact_tree nd.op2}); 
        | OP_mul -> 
          let a = get_val nd.op1 in
          if a = Int64.zero then Leaf (inv,PI_obj ([],OT_value (V_int Int64.zero)))
          else if a = Int64.one then nd.op2
          else
            Node (inv,{nd with op1=compact_tree nd.op1;
                               op2=compact_tree nd.op2}); 
        | _ ->
          Node (inv,{nd with op1=compact_tree nd.op1;
                             op2=compact_tree nd.op2}); 
      end else if is_val nd.op2 then
      begin
        match nd.op with
        | OP_band -> 
          let b = get_val nd.op2 in
          if b = Int64.zero then Leaf (inv,PI_obj ([],OT_value (V_bool false)))
          else nd.op1
        | OP_bor -> 
          let b = get_val nd.op2 in
          if b = Int64.one then Leaf (inv,PI_obj ([],OT_value (V_bool true)))
          else nd.op1
        | OP_add -> 
          let b = get_val nd.op2 in
          if b = Int64.zero then nd.op1
          else
            Node (inv,{nd with op1=compact_tree nd.op1;
                               op2=compact_tree nd.op2}); 
        | OP_mul -> 
          let b = get_val nd.op2 in
          if b = Int64.zero then Leaf (inv,PI_obj ([],OT_value (V_int Int64.zero)))
          else if b = Int64.one then nd.op1
          else
            Node (inv,{nd with op1=compact_tree nd.op1;
                               op2=compact_tree nd.op2}); 
        | _ ->
          Node (inv,{nd with op1=compact_tree nd.op1;
                             op2=compact_tree nd.op2});         
      end
      else
        Node (inv,{nd with op1=compact_tree nd.op1;
                           op2=compact_tree nd.op2}); 
    end;
  end;
  | Leaf (inv,lf) -> tree 

   
(*
** Fold intermediate tree. Take care of sign propagation (Substraction
** operations and negative signed operands).
** Rules:
**  type element = Constant | Object | Node/Operation
**
**  I. if op(n1) = op(n2) then
**      if left(n1) is C and left(n2) is C or O then swap(left(n1),left(n2));
**      if left(n1) is C and right(n2) is C or O then swap(left(n1),right(n2));
**      if right(n1) is C and left(n2) is C or O then swap(right(n1),left(n2));
**      if right(n1) is C and right(n2) is C or O then swap(right(n1),right(n2));
**     end if;
**  II. do apply (I) for all nodes while #swap > 0
**  III. if left(n) is C and right(n) is C then replcae n with eval(left(n),right(n))
**  IV.  apply (III) for all nodes 
**     
*)

let fold_tree tree =
  if debug_it "fold_tree" then 
  begin
    debug "fold_tree" with "tree:";
    print_tree tree;
  end;
  let inv b = {inv_add=b;inv_mul=b} in
  let tree' = ref (normalize_tree (inv false) tree) in
  if debug_it "expr_fold" then 
  begin
    debug "fold_tree" with "normalized tree:";
    print_tree !tree';
  end;

  let iter = ref true in
  let n = ref 0 in
  while !iter
  do
    incr n;
    let progress' = propagate_tree !tree' in
    if debug_it "expr_fold" then 
    begin
      debug "fold_tree"  with "propagated tree:";
      print_tree !tree';
    end;
    compacted := 0;
    let tree'' = compact_tree !tree' in
    if debug_it "expr_fold" then 
    begin
      debug "fold_tree" with (sprintf 
                              "pass# %d: progress=%b, compacted=%d, compacted tree:"
                               !n progress' !compacted);
      print_tree !tree';
    end;
    tree' := tree'';
    iter := !compacted > 0 || progress';
  done;   
  !tree'
  
let rec expr_fold pi =
    match pi with
    | PI_assign (src,lhs,rhs) ->
            line src;
            debug "expr_fold" with (sprintf "Assign: %s" (pi_sprint_instr pi));
            let rhs' = expr_fold rhs in
(*            
                let tree = to_tree OP_nop rhs in
                let tree' = fold_tree tree in 
                of_tree tree' in
*)
            PI_assign (src,lhs,rhs');
    | PI_forloop (src,expr,dir,lim1,lim2,block) ->
            line src;
            debug "expr_fold" with  "Forloop:";
            let lim1' =
                let tree = to_tree OP_nop lim1 in
                let tree' = fold_tree tree in
                of_tree tree' in
            let lim2' =
                let tree = to_tree OP_nop lim2 in
                let tree' = fold_tree tree in
                of_tree tree' in
            let block' = expr_fold block in
            PI_forloop (src,expr,dir,lim1',lim2',block');
    | PI_loop (src,kind,expr,block) ->
            line src;
            let block' = expr_fold block in
            PI_loop (src,kind,expr,block');
    | PI_block (pil,bf) ->
    begin
        (*
        ** Maybe removed functional blocks like branches.
        ** Propagate removal upwards (remove block) or change block frame.
        ** Remove dummy place holders for removed instructions (PI_nop).
        *)
        
        let pil' = List.filter (fun i -> 
                    match i with
                    | PI_nop -> false;
                    | _ -> true;
                    ) 
                                (List.map expr_fold pil) in
        match pil' with
        | [] -> PI_nop;
        | [PI_block (pil'',bf'')] ->
        begin
          if bf.bf_type = BF_branch && 
             bf''.bf_type = BF_conditional then
          begin
            bf''.bf_type <- BF_compound;
            bf''.bf_name <- "BLOCK";
            PI_block (pil'',bf'');
          end
          else PI_block (pil',bf);
        end;
        | _ -> 
            PI_block (pil',bf)
    end;
    | PI_branch (src,expr,b1,b2) ->
       line src;
       let expr' = expr_fold expr in
       let b1' = expr_fold b1 in
       let b2' = expr_fold b2 in
       if not (is_const_expr expr') then
         PI_branch (src,expr',b1',b2')
       else
       begin
         let b = (get_const_expri expr') > 0 in
         out (sprintf "Resolving %b branch %s due to constant folding."
                      b (print_src src));
         if b then b1' else 
         begin
           match b2' with
           | PI_block ([],_) -> PI_nop;
           | PI_block ([PI_nop],_) -> PI_nop;
           | _ -> b2';
         end;
       end;
    | PI_select (src,expr,cl) ->
       line src;
       let expr' = expr_fold expr in
       let cl' = expr_fold cl in
       if not (is_const_expr expr) then
         PI_select (src,expr',cl')
       else
       begin
         let v1 = get_const_expri expr' in
         match List.filter (fun c ->
           match c with
           | PI_block ([PI_case (src,exprl,_)],_)
           | PI_case (src,exprl,_) ->
             let found = ref false in
             List.iter (fun e -> 
               found := !found or (v1 = (get_const_expri e))) exprl;
             !found
           | _ -> error 827219 ""; 
           ) 
           (match cl' with
            | PI_block (cl'',_) -> cl'';
            | _ -> error 0 "No case list found in match expression.";
           ) with
         | PI_block ([PI_case (src,exprl,block)],_) :: _ 
         | PI_case (src,exprl,block) ::_ -> block;
         | _ -> error 0 "No constant matching expression found.";
       end;
    | PI_case (src,exprl,block) ->
            line src;
            let exprl' = List.map (fun expr -> expr_fold expr) exprl in
            let block' = expr_fold block in
            PI_case (src,exprl',block');
    | PI_try (block,cl) ->
      
      let cl' = expr_fold cl in
      let block' = expr_fold block in
      PI_try (block',cl')

    | PI_list l -> 
        let l' = List.map expr_fold l in
        let l'' = List.filter (fun i -> i <> PI_nop) l' in
        if l'' <> [] then PI_list l'' else PI_nop
    | PI_bool (kind,op,op1,op2) ->
    begin
      match kind with
      | Relational ->
      begin
(*
        let op1' = 
                let tree = to_tree OP_nop op1 in
                let tree' = fold_tree tree in
                of_tree tree' in
        let op2' = 
                let tree = to_tree OP_nop op2 in
                let tree' = fold_tree tree in
                of_tree tree' in
*)
        let op1' = expr_fold op1 in
        let op2' = expr_fold op2 in
        debug "expr_fold" with (sprintf "Relational: op1=%b op2=%b // %s" 
                               (is_const_expr op1') (is_const_expr op2') (pi_sprint_instr pi));
        let bool_false = PI_obj ([],OT_value (V_bool false)) in
        let bool_true = PI_obj ([],OT_value (V_bool true)) in
        match op with
        | OP_eq ->
            if is_const_expr op1' && is_const_expr op2' then
            begin
              let v1,v2 = get_const_expr op1',
                          get_const_expr op2' in
              if v1 = v2 then bool_true else bool_false
            end
            else
                PI_bool (kind,op,op1',op2');
        | OP_neq ->
            if is_const_expr op1' && is_const_expr op2' then
            begin
              let v1,v2 = get_const_expr op1',
                          get_const_expr op2' in
              if v1 <> v2 then bool_true else bool_false
            end
            else
                PI_bool (kind,op,op1',op2');
        | OP_lt ->
            if is_const_expr op1' && is_const_expr op2' then
            begin
              let v1,v2 = get_const_expr op1',
                          get_const_expr op2' in
              if v1 < v2 then bool_true else bool_false
            end
            else
                PI_bool (kind,op,op1',op2');
        | OP_gt ->
            if is_const_expr op1' && is_const_expr op2' then
            begin
              let v1,v2 = get_const_expr op1',
                          get_const_expr op2' in
              if v1 > v2 then bool_true else bool_false
            end
            else
                PI_bool (kind,op,op1',op2');
        | OP_ge ->
            if is_const_expr op1' && is_const_expr op2' then
            begin
              let v1,v2 = get_const_expr op1',
                          get_const_expr op2' in
              if v1 >= v2 then bool_true else bool_false
            end
            else
                PI_bool (kind,op,op1',op2');
        | OP_le ->
            if is_const_expr op1' && is_const_expr op2' then
            begin
              let v1,v2 = get_const_expr op1',
                          get_const_expr op2' in
              if v1 <= v2 then bool_true else bool_false
            end
            else
                PI_bool (kind,op,op1',op2');
        | _ ->            
            PI_bool (kind,op,op1',op2');
      end;
      | Bool ->
      begin
        let get_bool expr = 
            let vi = get_const_expr expr in
            if vi = Int64.zero then false else true in

        let bool_false = PI_obj ([],OT_value (V_bool false)) in
        let bool_true = PI_obj ([],OT_value (V_bool true)) in
        let op1' = expr_fold op1 in
        let op2' = expr_fold op2 in
(*                let tree = to_tree OP_nop op1 in
                let tree' = fold_tree tree in
                of_tree tree' in
        let op2' = 
                let tree = to_tree OP_nop op2 in
                let tree' = fold_tree tree in
                of_tree tree' in
*)
        debug "expr_fold" with (sprintf "Bool: op1=%b op2=%b // %s" 
                               (is_const_expr op1') (is_const_expr op2') (pi_sprint_instr pi));
        match op with
        | OP_band -> 
            
            if is_const_expr op1' then
            begin
              let b = get_bool op1' in
              if b = false then bool_false else op2'
            end
            else if is_const_expr op2' then
            begin
              let b = get_bool op2' in
              if b = false then bool_false else op1' 
            end
            else
                PI_bool (kind,op,op1',op2');
        | OP_bor ->
            if is_const_expr op1' then
            begin
              let b = get_bool op1' in
              if b = true then bool_true else op2'
            end
            else if is_const_expr op2' then
            begin
              let b = get_bool op2' in
              if b = true then bool_true else op1'            
            end
            else
                PI_bool (kind,op,op1',op2');
        | _ -> PI_bool (kind,op,op1',op2');
      end;      
    end;
    | PI_arithm (op,op1,op2) ->
            let pi' =
                let tree = to_tree OP_nop pi in
                let tree' = fold_tree tree in
                of_tree tree' in
            pi'
    | _ -> pi 
    
let expr_fold pi =
  let pi' = expr_fold pi in
  pi'
  
(*
** Synthesize CONPRO expression {PI_arith,PI_bool,PI_concat} into MicroCode
*)

let expr_synth pro expr label target =
    debug "expr_synth" with (sprintf "expr_synth: target dt: %s" 
                  (match target with | Some dt -> sprint_dt dt | None -> "."));

    let immed_id = ref 0 in
    let bool_id = ref 0 in
    let immed () =
        incr immed_id;
        UC_immed !immed_id
        in
    let bool () =
        incr bool_id;
        UC_bool !bool_id
        in
    let ucl = ref [] in
    let ucl_tmp = ref [] in
    
    let numtemp = ref 1 in
    let srctemp = ref [] in

    let last li n =
      let rec iter li n =
        match li with
        | hd::tl -> if n = 0 then hd else iter tl (n-1);
        | [] -> error 495033 "";
        in
      iter li ((List.length li)-n)
      in
        
    let is_op pi =
        match pi with
        | PI_obj _ -> true;
        | _ -> false  
        in

    let is_local ot =
        match ot with
        | OT_var co -> 
          (*
          ** No matter, always implemented
          ** with a non embedded block
          *)
          false 
        
        | _ ->
            sym_check_sym pro.pro_objs (Sym_obj ot) in

      
    (*
    ** ALU inference required?
    *)
    let check_alu_op op os =
        match op with
        | PI_obj (opl,ot) -> 
        begin
            if not (is_value ot) then
            begin
                let dt = obj_dt' op in
                let spec = op_special os in
                ((size_of_dt dt) > alu_thres pro) && not spec;
            end
            else
                false
        end;
        | _ -> false;
        in
    let check_alu_dt dt os =
        let spec = op_special os in
        ((size_of_dt dt) > alu_thres pro) && not spec
        in        

        let check_temp obj =
        let rhs = true in
        let lhs = not rhs in
        let temp ot =
            match ot with
            | OT_channel _
            | OT_queue _ 
            | OT_var _ -> true;
            | OT_array at -> 
		let gd_rd,gd_wr = array_guard at in
		(gd_rd && rhs) || (gd_wr && lhs)		
            | _ -> false;
            in
        match obj with
        | PI_obj (opl,ot) -> 
            if is_sel opl then
            begin
                let at = 
                    match ot with
                    | OT_array at -> at;
                    | _ -> error 232170 ""; in
		let gd_rd,gd_wr = array_guard at in
		(gd_rd && rhs) || (gd_wr && lhs)
            end
            else temp ot;
        | _ -> false;
        in

    let temp_name obj =
        (*
        ** Take care of dynamic array selectors!
        *)
        let temp ot =
          match ot with
          | _ -> name_of_pi obj;
          in
        match obj with
        | PI_obj (opl,ot) -> 
            if is_sel opl then
            begin
                let at = 
                    match ot with
                    | OT_array at -> at;
                    | _ -> error 232170 ""; in
                let sel = obj_sel opl in
                let is_block = List.mem AT_block at.at_flags in
                let is_dyn = List.mem AT_dyn at.at_flags in
                if is_block or is_dyn then 
                  sprintf "%s_%d" at.at_name (Random.int 1000000)
                else
                  at.at_name;
            end
            else temp ot;
        | _ -> error 92460 "";
        in

    let temp_expr_name op1 op2 =
        (*
        ** Take care of dynamic array selectors!
        *)
        let temp ot obj =
          match ot with
          | _ -> name_of_pi obj;
          in
        let get_name obj =
          match obj with
          | PI_obj (opl,ot) -> 
            if is_sel opl then
            begin
                let at = 
                    match ot with
                    | OT_array at -> at;
                    | _ -> error 292170 ""; in
                let sel = obj_sel opl in
                let is_block = List.mem AT_block at.at_flags in
                let is_dyn = List.mem AT_dyn at.at_flags in
                if is_block or is_dyn then 
                  sprintf "%s_%d" at.at_name (Random.int 1000000)
                else
                  at.at_name;
            end
            else temp ot obj;
          | _ -> sprintf "V%d" (Random.int 1000000);
          in
        sprintf "%s_%s" (get_name op1) (get_name op2) 
        in

    let alu_src src =
        match src with
        | UC_alu _ -> true;
        | _ -> false in

    let is_expr pi =
      match pi with
      | PI_arithm _
      | PI_bool _ -> true;
      | _ -> false in

    let rec eval in_arith in_bool pi target =
        let fix_sel_expr opl ot =
          (*
          ** Array selector expressions must be replaced by temporary
          ** registers!
          *)
          List.map (fun op ->
            match op with
            | OD_sel_obj pi' ->
              if is_expr pi' then
              begin
                let _,dt = expr_type pi' false in
                let temp = new_tmp pro dt in  
                temp.co_rules <- !core_rules;
                let temp_ot = OT_reg temp in
                let temp_ud = ud_of_ot temp_ot [] [UO_loc;UO_lhs] dt in
                (*
                ** Keep move(tmp,expr) and expr together.
                ** There can be additional temporary moves...
                *)
                let ucl' = !ucl in
                ucl := [];
                let expr = eval false false pi' (Some dt) in
                let ucl'' = !ucl in
                ucl := ucl';
                let uc = Move (temp_ud,expr) in
                ucl_tmp := !ucl_tmp @ ucl'' @ [{
                          ui_code = uc;
                          ui_frame=List.hd pro.pro_frame;
                        }];
                release_tmp pro temp;
                OD_sel_obj (PI_obj ([],temp_ot));
              end
              else op;
            | _ -> op;
            ) opl
          in
        match pi with
        | PI_obj (opl,ot) -> 
            let opl' = opl in
            let opl'' = fix_sel_expr opl' ot in 
            let expr_dt =
              match target with
              | Some dt -> dt;
              | None -> get_some (dt_of_ot ot) in
            let uc = ud_of_ot ot opl'' 
                     (if is_ot_local (Some pro) ot then 
                        [UO_loc;UO_rhs] else [UO_rhs]) expr_dt in
            debug "expr_synth" with  (sprintf "expr_synth: expr ot <%s> with target=<%s> => uc<%s>" 
                    (name_of_ot ot) 
                    (match target with | Some dt -> sprint_dt dt | None -> ".")                    
                    (ui_sprint_uc uc)); 
            uc
        | PI_arithm (op,op1,op2) ->

            let expr_dt = 
                match target with
                | Some dt -> dt;
                | None ->
                    let _,dt = expr_type pi false in
                    dt
                in    
            let target = Some expr_dt in

            let temp1 = ref None in
            let temp2 = ref None in
            let dst_reg = ref None in

            let is_op1 = is_op op1 in
            let is_op2 = is_op op2 in
            let temp_op1 = check_temp op1 in
            let temp_op2 = check_temp op2 in
            (*
            ** Different expression models: [flat,alu,binary]
            **
            ** alu -> alu_req, alu register is result
            ** binray -> dst_reg_req, temporary register is result
            *)
            let alu_req = check_alu_op op1 op ||
                          check_alu_op op2 op ||
                          (if target <> None then 
                                check_alu_dt (get_some target) op
                           else
                                false) in
            let dst_reg_req = (expr_model pro) = EXPR_binary in

            let alu = if alu_req then [OP_alu] else [] in  

            if temp_op1 then
            begin
              let dt1 = dt_of_pi op1 in
              let conv1 = conv_dt_of_pi op1 in
              let name1 = temp_name op1 in
              let dst = {
                      ut_type = uo_type dt1 dt1;
                      ut_range = None;
                      ut_name = name1;
                      ut_frame = Some (UC_label label);
                      ut_obj = None;
                      ut_flags = [UO_lhs;UO_loc];
                  } in
              temp1 := Some {dst with
                              ut_type = {(copy_uo_type dst.ut_type) with uo_expr_type=expr_dt;uo_conv=conv1};
                              ut_flags = [UO_rhs;UO_loc]};
              let src = eval true false op1 target in
              if pi_frag op1 then
              begin
                  let moves = frag_src_split pro (UC_temp dst)
                                             src in
                  ucl := !ucl @ moves;
              end
              else
              begin
                  ud_conv src dt1;
                  let uc = {
                          ui_code=Move (UC_temp dst,src);
                          ui_frame=List.hd pro.pro_frame; 
                      } in
                  if not (List.mem name1 !srctemp) then
                  begin
                      ucl_tmp := !ucl_tmp @ [uc];
                      srctemp := !srctemp @ [name1];
                  end;
              end;
            end;
            if temp_op2 then
            begin
              let dt2 = dt_of_pi op2 in
              let conv2 = conv_dt_of_pi op2 in
              let name2 = temp_name op2 in
              let dst = {
                      ut_type = uo_type dt2 dt2;
                      ut_range = None;
                      ut_name = name2;
                      ut_frame = Some (UC_label label);
                      ut_obj = None;
                      ut_flags = [UO_lhs;UO_loc];
                  } in
              temp2 := Some {dst with
                              ut_type = {(copy_uo_type dst.ut_type) with uo_expr_type=expr_dt;uo_conv=conv2};
                              ut_flags = [UO_rhs;UO_loc]};
              let src = eval true false op2 target in
              if pi_frag op2 then
              begin
                  let moves = frag_src_split pro (UC_temp dst)
                                             src in
                  ucl := !ucl @ moves;
              end
              else
              begin
                  ud_conv src dt2;
                  let uc = {
                          ui_code=Move (UC_temp dst,src);
                          ui_frame=List.hd pro.pro_frame; 
                      } in
                  if not (List.mem name2 !srctemp) then
                  begin
                      ucl_tmp := !ucl_tmp @ [uc];
                      srctemp := !srctemp @ [name2];
                  end;
              end;
            end;
            if dst_reg_req then
            begin
                let dt = 
                    match target with
                    | Some dt -> dt;
                    | None ->
                        if is_op1 && not (is_pi_value op1) then 
                            dt_of_pi op1 
                        else if is_op2 && not (is_pi_value op2)then
                            dt_of_pi op2
                        else
                            error 0 "\nexpr_synth: both op1 and op2 are constant values!" in
                let name = temp_expr_name op1 op2 in
                let dst = {
                            ut_type = uo_type dt dt;
                            ut_range = None;
                            ut_name = name;
                            ut_frame = Some (UC_label label);
                            ut_obj = None;
                            ut_flags = [UO_lhs;UO_loc];
                        } in
                dst_reg := Some (UC_temp dst);
            end;

            if is_op1 then
            begin
                let dst = if alu_req then 
                                UC_alu {ua_type=expr_dt;
                                        ua_flags=[UO_lhs;UO_loc]}
                          else if dst_reg_req then
                                (get_some !dst_reg)
                          else immed () in
                let src1 = if not temp_op1 then
                                eval true false op1 target
                           else
                                UC_temp {
                                  (get_some !temp1) 
                                  with ut_flags=[UO_rhs;UO_loc]} in
                let src2 = if is_op2 then
                           begin 
                            if not temp_op2 then
                                eval true false op2 target
                            else
                              UC_temp {
                                (get_some !temp2) 
                                with ut_flags=[UO_rhs;UO_loc]}
                           end
                           else
                                eval true false op2 target
                    in

                let uc = Expr ([op]@alu,dst,ud_fix_rhs src1,
                                            ud_fix_rhs src2) in
                ucl := !ucl @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
                dst
            end
            else if is_op2 then
            begin
                let dst = if alu_req then 
                                UC_alu {ua_type=expr_dt;
                                        ua_flags=[UO_lhs;UO_loc]}
                          else if dst_reg_req then
                                (get_some !dst_reg)
                          else immed () in
                let src2 = if not temp_op2 then
                                eval true false op2 target
                           else
                             UC_temp {
                               (get_some !temp2)
                               with ut_flags=[UO_rhs;UO_loc]} in
                let src1 = if is_op1 then
                           begin 
                            if not temp_op1 then
                                eval true false op1 target
                            else
                              UC_temp {
                                (get_some !temp1)  
                                with ut_flags=[UO_rhs;UO_loc]}
                           end
                           else
                                eval true false op1 target
                    in

                let uc = Expr ([op]@alu,dst,ud_fix_rhs src1,
                                            ud_fix_rhs src2) in
                ucl := !ucl @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
                dst
            end
            else
            begin
              if alu_req then
              begin
                (*
                ** Both operands are expressions!
                *)
                let res1 = eval true false op1 target in
                (*
                ** Transfer result (ALU) to temporary
                ** register.
                *)
                let _,dt1 = expr_type op1 false in
                let name1 = sprintf "#%d" !numtemp in
                incr numtemp;
                let tempx = {
                            ut_type = uo_type dt1 dt1;
                            ut_range = None;
                            ut_name = name1;
                            ut_frame = Some (UC_label label);
                            ut_obj = None;
                            ut_flags = [UO_lhs;UO_loc];
                        } in

                let uc = Move (UC_temp tempx,res1) in
                ucl_tmp := !ucl_tmp @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
                
                let res2 = eval true false op2 target in
                let dst = UC_alu {ua_type=expr_dt;
                                  ua_flags=[UO_lhs;UO_loc]} in

                let uc = Expr ([op]@alu,dst,
                               UC_temp
                               {tempx with ut_flags=[UO_rhs;UO_loc]},
                               ud_fix_rhs res2) in
                ucl := !ucl @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
                dst
              end
              else
              begin
                let res1 = eval true false op1 target in
                let res2 = eval true false op2 target in
                let dst = if dst_reg_req 
                          then (get_some !dst_reg) 
                          else immed () in
                let uc = Expr ([op]@alu,dst,ud_fix_rhs res1,
                                            ud_fix_rhs res2) in
                ucl := !ucl @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
                dst
                
              end;
            end;
        | PI_bool (kind,op,op1,op2) ->

            let temp1 = ref None in
            let temp2 = ref None in

            let is_op1 = is_op op1 in
            let is_op2 = is_op op2 in
            let temp_op1 = check_temp op1 in
            let temp_op2 = check_temp op2 in
            let alu_req = 
                kind = Relational && (
                    check_alu_op op1 op ||
                    check_alu_op op2 op ||
                    (if target <> None then 
                        check_alu_dt (get_some target) op
                     else
                        false)) in
            let alu = if alu_req then [OP_alu] else [] in  

            let _,expr_dt = expr_type pi false in
            let target = Some expr_dt in

            if temp_op1 then
            begin
              let dt1 = dt_of_pi op1 in
              let conv1 = conv_dt_of_pi op1 in
              let name1 = temp_name op1 in
              let dst = {
                      ut_type = uo_type dt1 dt1;
                      ut_range = None;
                      ut_name = name1;
                      ut_frame = Some (UC_label label);
                      ut_obj = None;
                      ut_flags = [UO_lhs;UO_loc];
                  } in
              temp1 := Some (UC_temp {dst with 
                              	  	  ut_type = {(copy_uo_type dst.ut_type) with uo_expr_type=expr_dt;uo_conv=conv1};
                                      ut_flags = [UO_rhs;UO_loc]});
              let src = eval false true op1 (Some dt1) in
              if pi_frag op1 then
              begin
                  let moves = frag_src_split pro (UC_temp dst)
                                             src in
                  ucl := !ucl @ moves;
              end
              else
              begin
				  ud_conv src dt1;
                  let uc = {
                          ui_code=Move (UC_temp dst,src);
                          ui_frame=List.hd pro.pro_frame; 
                      } in
                  if not (List.mem name1 !srctemp) then
                  begin
                      ucl_tmp := !ucl_tmp @ [uc];
                      srctemp := !srctemp @ [name1];
                  end;
              end;
            end;
            if temp_op2 then
            begin
              let dt2 = dt_of_pi op2 in
              let conv2 = conv_dt_of_pi op2 in
              let name2 = temp_name op2 in
              let dst = {
                      ut_type = uo_type dt2 dt2;
                      ut_range = None;
                      ut_name = name2;
                      ut_frame = Some (UC_label label);
                      ut_obj = None;
                      ut_flags = [UO_lhs;UO_loc];
                  } in
              temp2 := Some (UC_temp {dst with 
                              	  	  ut_type = {(copy_uo_type dst.ut_type) with uo_expr_type=expr_dt;uo_conv=conv2};
                                      ut_flags = [UO_rhs;UO_loc]});
              let src = eval false true op2 (Some dt2) in
              if pi_frag op2 then
              begin
                  let moves = frag_src_split pro (UC_temp dst)
                                             src in
                  ucl := !ucl @ moves;
              end
              else
              begin
				  ud_conv src dt2;
                  let uc = {
                          ui_code=Move (UC_temp dst,src);
                          ui_frame=List.hd pro.pro_frame; 
                      } in
                  if not (List.mem name2 !srctemp) then
                  begin
                      ucl_tmp := !ucl_tmp @ [uc];
                      srctemp := !srctemp @ [name2];
                  end;
              end;
            end;

            if kind = Relational then
            begin
              if is_op1 then
              begin
                let src1 = if not temp_op1 then
                                eval false true op1 target
                           else
                                (get_some !temp1) in
                let src2 = if is_op2 then
                           begin 
                            if not temp_op2 then
                                eval false true op2 target
                            else
                                (get_some !temp2)  
                           end
                           else
                                eval false true op2 target
                    in
                let alu_req = alu_req || 
                              alu_src src1 ||
                              alu_src src2 in
                let alu = if alu_req then [OP_alu] else [] in
                let dst = bool () in
                let uc = Expr ([op]@alu,dst,ud_fix_rhs src1,
                                            ud_fix_rhs src2) in
                ucl := !ucl @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
                dst
              end
              else if is_op2 then
              begin
                let src2 = if not temp_op2 then
                                eval false true op2 target
                           else
                                (get_some !temp1) in
                let src1 = if is_op1 then
                           begin 
                            if not temp_op1 then
                                eval false true op1 target
                            else
                                (get_some !temp1)  
                           end
                           else
                                eval false true op1 target
                    in
                let alu_req = alu_req || 
                              alu_src src1 ||
                              alu_src src2 in
                let alu = if alu_req then [OP_alu] else [] in
                let dst = bool () in

                let uc = Expr ([op]@alu,dst,ud_fix_rhs src1,
                                            ud_fix_rhs src2) in
                ucl := !ucl @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
                dst
              end
              else
              begin
                (*
                ** Both operands are expressions!
                *)
                let res1 = eval false true op1 target in
                (*
                ** Transfer result (either ALU or IMMED) to temporary
                ** register.
                *)
                let _,dt1 = expr_type op1 false in
                let name1 = sprintf "#%d" !numtemp in
                incr numtemp;
                let tempx = UC_temp {
                            ut_type = uo_type dt1 dt1;
                            ut_range = None;
                            ut_name = name1;
                            ut_frame = Some (UC_label label);
                            ut_obj = None;
                            ut_flags = [UO_lhs;UO_loc];
                        } in

                let uc = Move (tempx,res1) in
                ucl_tmp := !ucl_tmp @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
                
                let res2 = eval false true op2 target in
                let alu_req = alu_req || 
                              alu_src res1 ||
                              alu_src res2 in
                let alu = if alu_req then [OP_alu] else [] in
                let dst = bool () in

                let uc = Expr ([op]@alu,dst,tempx,ud_fix_rhs res2) in
                ucl := !ucl @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
                dst
              end;
            end
            else
            begin
                (*
                ** Different cases:
                **  1. OP1 <boolop> OP2 -> IMMED
                **  2. OP1 <boolop> <EXPR>
                **  2.1. <EXPR>: OP1 <boolop> OP2 -> IMMED
                **  2.2. <EXPR>: OP1 <boolop> <EXPR>
                *)
                if is_op1 && is_op2 then
                begin
                    let src1 = if not temp_op1 then
                                eval false true op1 target
                           else
                                (get_some !temp1) 
                        in
                    let src2 = if not temp_op2 then
                                eval false true op2 target
                            else
                                (get_some !temp2)  
                        in
                    let dst = bool () in
                    let uc = Expr ([op],dst,ud_fix_rhs src1,
                                            ud_fix_rhs src2) in
                    ucl := !ucl @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                        }];
                    dst
                end
                else
                begin
                    let src1 = 
                        if is_op1 then
                        begin
                            if not temp_op1 then
                                eval false true op1 target
                            else
                                (get_some !temp1)
                        end
                        else
                            eval false true op1 target
                        in
                    let src2 = 
                        if is_op2 then
                        begin
                            if not temp_op2 then
                                eval false true op2 target
                            else
                                (get_some !temp2)
                        end
                        else
                            eval false true op2 target
                        in
                    let dst = bool () in

                    let uc = Expr ([op],dst,ud_fix_rhs src1,
                                              ud_fix_rhs src2) in
                    ucl := !ucl @ [{
                          ui_code = uc;
                          ui_frame=List.hd pro.pro_frame;
                          }];
                    dst
                end;
            end;
        | PI_concat (op1,op2) -> ();
            let expr_dt = 
                match target with
                | Some dt -> dt;
                | None ->
                    let _,dt = expr_type pi false in
                    dt
                in    

            let temp1 = ref None in
            let temp2 = ref None in

            let is_op1 = is_op op1 in
            let is_op2 = is_op op2 in

            let temp_op1 = check_temp op1 in
            let temp_op2 = check_temp op2 in
            if temp_op1 then
            begin
                    let dt1 = dt_of_pi op1 in
                    let name1 = temp_name op1 in
                    let dst = {
                            ut_type = uo_type dt1 dt1;
                            ut_range = None;
                            ut_name = name1;
                            ut_frame = Some (UC_label label);
                            ut_obj = None;
                            ut_flags = [UO_lhs;UO_loc];
                        } in
                    temp1 := Some (UC_temp dst);
                    let src = eval true false op1 None in
                    let uc = Move (UC_temp dst,src) in
                    ucl_tmp := !ucl_tmp @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
            end;
            if temp_op2 then
            begin
                    let dt2 = dt_of_pi op2 in
                    let name2 = temp_name op2 in
                    let dst = {
                            ut_type = uo_type dt2  dt2;
                            ut_range = None;
                            ut_name = name2;
                            ut_frame = Some (UC_label label);
                            ut_obj = None;
                            ut_flags = [UO_lhs;UO_loc];
                        } in
                    temp2 := Some (UC_temp dst);
                    let src = eval true false op2 None in
                    let uc = Move (UC_temp dst,src) in
                    ucl_tmp := !ucl_tmp @ [{
                        ui_code = uc;
                        ui_frame=List.hd pro.pro_frame;
                    }];
            end;

            let src1 = 
              if not temp_op1 then
                eval false false op1 None
              else
                (get_some !temp1) 
                        in
            let src2 = 
              if not temp_op2 then
                eval false false op2 None
              else
                (get_some !temp2)  
              in

            let list_size = ref 0 in
            
            let rec expand_list ud =
              let get_size ud =
                match ud with
                | UC_list _ -> 0;
                | _ -> 
                begin
                  match uo_of_ud ud with
                  | Some uo -> 
                  begin
                    match uo.uo_range with
                    | Some (a,b) -> b-a+1;
                    | None -> size_of_dt uo.uo_type.uo_data_type;
                  end;
                  | None -> 
                  begin
                    match ud with
                    | UC_val uv -> 
                      size_of_dt uv.uv_type.uo_data_type;
                    | UC_temp ut ->
                      size_of_dt ut.ut_type.uo_data_type;
                    | _ -> 0;
                  end;
                end
                in
              match ud with
              | UC_list ul -> 
              begin
                let rec iter ul =
                  match ul with
                  | hd::tl -> 
                    (expand_list hd) @ (iter tl);
                  | [] -> [];
                  in
                iter ul.ul_list;
              end;
              | _ -> 
                list_size := !list_size + (get_size ud);
                [ud]
              in
                
            let data_list = (expand_list src1) @
                            (expand_list src2) in
            let data_dt = dt_of_td (td_of_dt expr_dt) !list_size in

            if (size_of_dt data_dt) > (size_of_dt expr_dt) then
              error 0 "expr_synth: concat product larger than target";

            let ul = {
               ul_type = uo_type data_dt expr_dt;
               ul_list = data_list;
               ul_flags = [UO_rhs];
            } in
            (*
            ** Fix uo_expr_type, must be min (uo_data_type | uo_log_type | uo_phys_type)
            *)
            ul.ul_list <- List.map (fun ud ->
              match uo_of_ud ud with
              | Some uo ->
                  uo.uo_type.uo_expr_type <- (
                      if uo.uo_type.uo_log_type <> None then (get_some uo.uo_type.uo_log_type)
                      else if uo.uo_type.uo_phys_type <> None then (get_some uo.uo_type.uo_phys_type)
                      else uo.uo_type.uo_data_type;  
                    );
                  ud;
              | None -> ud;
              ) ul.ul_list;
            UC_list ul;
        | _ -> error 979829 "";
        in

    let res = eval false false expr target in
    res,(!ucl_tmp @ !ucl)


(*
** PI -> UC
*)
let fun_get_expr pro modname name il num =
      let arg = 
          try
            (List.nth il (num-1))
          with
            | _ -> error 0 (sprintf "%s: too few function arguments in <%s>"
                                    modname name);
            in    
      match arg with
      | PI_arithm (op,op1,op2) ->
        let ud,uil = expr_synth pro arg (sprintf "%s.%s" modname name) None in
        uil
      | _ -> error 0 (sprintf "%s: unexpected function argument (expression) in <%s>"
                              modname name)

let fun_is_expr modname name il num =
      let arg = 
          try
            (List.nth il (num-1))
          with
            | _ -> error 0 (sprintf "%s: too few function arguments in <%s>"
                                    modname name);
            in    
      match arg with
      | PI_arithm _ 
      | PI_bool _ -> true
      | _ ->  false

let prev_alu = ref None


(*
** Non bounded EXPR(op,dst,src1,src2)
*)
let expr_assign pro ops lhs rhs1 rhs2 =
  let is_terminated str =
    (*
    ** dp_conv: either expression or terminated statement;
    *)
    str.[(String.length str)-1] = ';' in

  debug "expr_assign" with (sprintf "expr_assign %s" an.a_errlabel);

  let alu_def alu_type = val_str alu_type (V_int Int64.zero) in
  let is_relat = List.mem OP_relat (
            List.map 
                (fun op -> 
                    if op <> OP_alu then
                        op_mode op
                    else
                        OP_arith
                 ) ops
               ) in
  let use_alu = List.mem OP_alu ops in
  let use_alu_bool = 
        (List.mem OP_bool (
            List.map 
                (fun op -> 
                    if op <> OP_alu then
                        op_mode op
                    else
                        OP_arith
                 ) ops
               ))
        or is_relat
    in
  let l_alu_bool =
    match lhs with
    | UC_alu ua -> ua.ua_type = DT_bool;
    | _ -> false in
  let alu = 
    if use_alu then
    begin
        (*
        ** Add alu_type conversions to operands if required.
        *)
        let alu_dt = dt_of_udl [lhs;rhs1;rhs2] in
        let alu = find_alu pro alu_dt in
        let alu_type = alu.alu_type in
        ud_conv rhs1 alu_type;
        ud_conv rhs2 alu_type;
        Some alu
    end
    else
    begin
        let expr_dt = dt_of_udl [lhs;rhs1;rhs2] in
        ud_conv rhs1 expr_dt;
        ud_conv rhs2 expr_dt;
        None
    end;
    in
  let alu =
      if alu = None && !prev_alu <> None then 
      begin
        (*
        ** Old ALU information is required for possible ALU[B]
        ** operand!
        *)
        let old = !prev_alu in
        prev_alu := None;
        old
      end  
      else  
      begin
        prev_alu := alu;
        alu
      end
      in


  let vdr1 = vhdl_of_ud pro alu rhs1 None in
  let vdr2 = vhdl_of_ud pro alu rhs2 None in
  let guard =
      let con1 () = 
        (* r_con: SIG = expr *)
        let n = String.index vdr1.cp_sig ' ' in
        String.sub vdr1.cp_sig 0 n in

      let con2 () = 
        (* r_con: SIG = expr *)
        let n = String.index vdr2.cp_sig ' ' in
        String.sub vdr2.cp_sig 0 n in
      let gd_rd1,gd_wr1 = ud_guard rhs1 in
      let gd_rd2,gd_wr2 = ud_guard rhs2 in
      if (gd_rd1 or gd_wr1) && vdr1.cp_sig <> "" then
        Some ("not",con1 ())
      else if (gd_rd2 or gd_wr2) && vdr2.cp_sig <> "" then
        Some ("not",con2 ())
      else
      begin
        match lhs with
        | UC_chan uo ->
          Some ("",sprintf "CHAN_%s_GD" uo.uo_name);
        | UC_queue uo ->
          Some ("",sprintf "QUEUE_%s_GD" uo.uo_name);              
        | _ -> None;
      end in

  let vdl = vhdl_of_ud pro alu lhs guard in

  let lhs_et = (type_of_ud lhs).uo_expr_type in
  let rhs1_et = (type_of_ud rhs1).uo_expr_type in
  let rhs2_et = (type_of_ud rhs2).uo_expr_type in
  let bool_reg_conv = (lhs_et = DT_bool) && ((rhs1_et <> DT_bool) || is_relat) in

  if bool_reg_conv then vdl.dp_conv <- "B_to_L(%s)";
  let op_prim,op_sec =
    let op1 = ref OP_nop in
    let op2 = ref OP_nop in
    List.iter (fun op ->
        if op <> OP_alu then
        begin
            let opm = op_mode op in
            if !op1 = OP_nop then
                op1 := op
            else if !op2 = OP_nop then
                op2 := op;
        end;
        ) ops;
    !op1,!op2
    in
  (*
  ** Temporary alu_res signal required for correct type conversion...
  *)
  let alu_res alu_type =
    let ud dt dt' = UC_sig {
            uo_name = sprintf "alu_%s_res" (get_some alu).alu_name;
            uo_obj = None;
            uo_type = uo_type dt  dt';
            uo_range = None;
            uo_index = None;
            uo_addr = (fun _ -> "");
            uo_sel = [];
            uo_block = None;
            uo_flags = [UO_loc;UO_rhs];
        } in
    match lhs with
    | UC_alu ua -> if ua.ua_type <> DT_bool 
                    then ud alu_type alu_type
                   else lhs;
    | _ -> 
    begin
        match uo_of_ud lhs with
        | Some uo -> ud alu_type uo.uo_type.uo_expr_type;
        | None -> 
        begin
            match lhs with
            | UC_temp ut -> 
                ud alu_type ut.ut_type.uo_expr_type;
            | _ -> error 844609 "";
        end;
    end;
    in


  (*
  ** Data Path
  *)
  let dp =
    if use_alu then
    begin
        let alu = get_some alu in
        let alu_type = alu.alu_type in
        let alu_name = alu.alu_name in
        [
            Data_in (sprintf "alu_%s_op1 <= %s;"
                             alu_name
                             vdr1.dp_sig);
            Data_in (sprintf "alu_%s_op2 <= %s;"
                             alu_name
                             vdr2.dp_sig);
            Data_in (sprintf "alu_%s_op  <= %s;"  
                             alu_name
                             (alu_state_str op_prim));
            Data_def (sprintf "alu_%s_op" alu_name,"Alu_nop");
            Data_def (sprintf "alu_%s_op1" alu_name,
                          val_str alu_type (V_int Int64.zero));
            Data_def (sprintf "alu_%s_op2" alu_name,
                          val_str alu_type (V_int Int64.zero));
        ] @
        (if op_sec <> OP_nop then
            [Data_in (sprintf "alu_%s_bool_op  <= %sB;"
                             alu_name
                             (alu_state_str op_sec));
             Data_def (sprintf "alu_%s_bool_op" alu_name,"Alu_nopB");]
         else
            []) @
        (
          if is_local lhs then
          [
            (*
            ** RTL Data Path
            *)
            (if not l_alu_bool && not use_alu_bool then
             begin
                Data_trans (sprintf "%s <= %s;"
                                    vdl.dp_sig
                                    (sprintf (Obj.magic vdl.dp_conv)
                                             (vhdl_sig_of_ud pro (alu_res alu_type)))
                            )
             end
             else
                Data_trans (sprintf "%s <= alu_%s_bool;" 
                                    vdl.dp_sig
                                    alu_name);
            );    
            (if not l_alu_bool && not use_alu_bool then
                Data_trans_sens (sprintf "alu_%s_res" alu_name)
             else
                Data_trans_sens (sprintf "alu_%s_bool" alu_name);
            );
          ] @
            (List.map (fun v -> Data_out v) vdl.dp_aux) @
            (List.map (fun v -> Data_def v) vdl.dp_aux_def) @
            (List.map (fun v -> Data_sens v) vdl.dp_aux_sen) @
            (List.map (fun sv -> Data_trans_def sv) vdl.dp_def) @ 
            (List.map (fun v -> Data_trans_sens v) vdl.dp_sen) @ 

            (List.map (fun v -> Data_in v) vdr1.dp_aux) @
            (List.map (fun v -> Data_def v) vdr1.dp_aux_def) @
            (List.map (fun v -> Data_sens v) vdr1.dp_aux_sen) @
            (List.map (fun v -> Data_trans_sens v) vdr1.dp_sen) @
            (List.map (fun v -> Data_top v) vdr1.top_expr) @
            (List.map (fun v -> Data_top_def v) vdr1.top_def) @

            (List.map (fun v -> Data_in v) vdr2.dp_aux) @
            (List.map (fun v -> Data_def v) vdr2.dp_aux_def) @
            (List.map (fun v -> Data_sens v) vdr2.dp_aux_sen) @
            (List.map (fun v -> Data_trans_sens v) vdr2.dp_sen) @
            (List.map (fun v -> Data_top v) vdr2.top_expr) @
            (List.map (fun v -> Data_top_def v) vdr2.top_def) 
          else
          [
            (*
            ** RTL Data Path
            *)
            (if not l_alu_bool && not use_alu_bool then
                Data_out  (sprintf "%s <= %s;"
                                    vdl.dp_sig 
                                    (sprintf (Obj.magic vdl.dp_conv) 
                                             (vhdl_sig_of_ud pro (alu_res alu_type)))
                           )
             else
                Data_out (sprintf "%s <= alu_%s_bool;" 
                                  vdl.dp_sig
                                  alu_name);
            );    
            (if not l_alu_bool && not use_alu_bool then
                Data_sens (sprintf "alu_%s_res" alu_name)
             else
                Data_sens (sprintf "alu_%s_bool" alu_name);
            );
          ] @
            (List.map (fun v -> Data_out v) vdl.dp_aux) @
            (List.map (fun v -> Data_def v) vdl.dp_aux_def) @
            (List.map (fun v -> Data_sens v) vdl.dp_aux_sen) @
            (List.map (fun sv -> Data_def sv) vdl.dp_def) @ 
            (List.map (fun v -> Data_sens v) vdl.dp_sen) @

            (List.map (fun v -> Data_in v) vdr1.dp_aux) @
            (List.map (fun v -> Data_def v) vdr1.dp_aux_def) @
            (List.map (fun v -> Data_sens v) vdr1.dp_aux_sen) @
            (List.map (fun v -> Data_sens v) vdr1.dp_sen) @
            (List.map (fun v -> Data_top v) vdr1.top_expr) @
            (List.map (fun v -> Data_top_def v) vdr1.top_def) @

            (List.map (fun v -> Data_in v) vdr2.dp_aux) @
            (List.map (fun v -> Data_def v) vdr2.dp_aux_def) @
            (List.map (fun v -> Data_sens v) vdr2.dp_aux_sen) @
            (List.map (fun v -> Data_sens v) vdr2.dp_sen) @
            (List.map (fun v -> Data_top v) vdr2.top_expr) @
            (List.map (fun v -> Data_top_def v) vdr2.top_def) 
        ) 
    end
    else
    begin
      (*
      ** Expression without ALU usage
      *)
      (
        if is_local lhs then
        (
            (*
            ** RTL Data Path
            *)
			let vd_expr =  vhdl_of_expr pro rhs1 rhs2 op_prim in
            [if not (is_terminated vdl.dp_conv) then
              Data_trans (sprintf "%s <= %s;"
                                 vdl.dp_sig
                                 (sprintf (Obj.magic vdl.dp_conv) 
                                          vd_expr.dp_sig)
                        )
             else
              Data_trans (sprintf (Obj.magic vdl.dp_conv) 
                                  vd_expr.dp_sig)

            ]@
            (List.map (fun s -> Data_in s) vd_expr.dp_aux)@
            (List.map (fun s -> Data_trans_def s) vd_expr.dp_def)@
            (List.map (fun s -> Data_def s) vd_expr.dp_aux_def)@
            (List.map (fun s -> Data_trans_sens s) vd_expr.dp_sen)@
            (List.map (fun s -> Data_sens s) vd_expr.dp_aux_sen)@
            (List.map (fun s -> Data_top s) vd_expr.top_expr)@
            (List.map (fun s -> Data_top_def s) vd_expr.top_def)
        ) @
          (List.map (fun v -> Data_out v) vdl.dp_aux) @
          (List.map (fun v -> Data_def v) vdl.dp_aux_def) @
          (List.map (fun v -> Data_sens v) vdl.dp_aux_sen) @
          (List.map (fun sv -> Data_trans_def sv) vdl.dp_def) @
          (List.map (fun v -> Data_trans_sens v) vdl.dp_sen) 

        else
        (
            (*
            ** RTL Data Path
            *)
			let vd_expr =  vhdl_of_expr pro rhs1 rhs2 op_prim in
            [if not (is_terminated vdl.dp_conv) then
              Data_out (sprintf "%s <= %s;"
                               vdl.dp_sig
                               (sprintf (Obj.magic vdl.dp_conv) 
                                        vd_expr.dp_sig)
                      )
             else
              Data_out (sprintf (Obj.magic vdl.dp_conv) 
                                vd_expr.dp_sig)
            ]@
            (List.map (fun s -> Data_in s) vd_expr.dp_aux)@
            (List.map (fun s -> Data_def s) vd_expr.dp_def)@
            (List.map (fun s -> Data_def s) vd_expr.dp_aux_def)@
            (List.map (fun s -> Data_sens s) vd_expr.dp_sen)@
            (List.map (fun s -> Data_sens s) vd_expr.dp_aux_sen)@
            (List.map (fun s -> Data_top s) vd_expr.top_expr)@
            (List.map (fun s -> Data_top_def s) vd_expr.top_def)
        ) @
          (List.map (fun v -> Data_out v) vdl.dp_aux) @
          (List.map (fun v -> Data_def v) vdl.dp_aux_def) @
          (List.map (fun v -> Data_sens v) vdl.dp_aux_sen) @
          (List.map (fun sv -> Data_def sv) vdl.dp_def) @
          (List.map (fun v -> Data_sens v) vdl.dp_sen)
(*
          (List.map (fun v -> Data_in v) vdr1.dp_aux) @
          (List.map (fun v -> Data_def v) vdr1.dp_aux_def) @
          (List.map (fun v -> Data_sens v) vdr1.dp_aux_sen) @
          (List.map (fun v -> Data_sens v) vdr1.dp_sen) @
          (List.map (fun v -> Data_top v) vdr1.top_expr) @
          (List.map (fun v -> Data_top_def v) vdr1.top_def) @

          (List.map (fun v -> Data_in v) vdr2.dp_aux) @
          (List.map (fun v -> Data_def v) vdr2.dp_aux_def) @
          (List.map (fun v -> Data_sens v) vdr2.dp_aux_sen) @
          (List.map (fun v -> Data_sens v) vdr2.dp_sen) @
          (List.map (fun v -> Data_top v) vdr2.top_expr) @
          (List.map (fun v -> Data_top_def v) vdr2.top_def) 
*)
      )
    end in

    (*
    ** Write Control Path.
    *)
    let cp_wr =
        if vdl.cp_sig <> "" then
        [
           Data_cond vdl.cp_sig;
        ] @
        (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)            
        else
        []
        in
    let cp_rd1 =
        (*
        ** Control Path RD
        *)
        if vdr1.cp_sig <> "" then
        [
            Data_cond vdr1.cp_sig;
        ] @
        (List.map (fun s -> Data_cond_sens s) vdr1.cp_sen)
        else
        []
        in
    let cp_rd2 =
        (*
        ** Control Path RD
        *)
        if vdr2.cp_sig <> "" then
        [
            Data_cond vdr2.cp_sig;
        ] @
        (List.map (fun s -> Data_cond_sens s) vdr2.cp_sen)
        else
        []
        in

    dp,cp_wr,(cp_rd1@cp_rd2)

let branch_expr pro ops op1 op2 =
  let aux = ref  [] in
  let get_arg ud =
    match ud with
    | UC_reg uo -> 
        let sens = List.hd (vhdl_sens_of_ud pro ud) in
        aux := !aux @ [Data_cond_sens sens];
        vhdl_sig_of_ud pro ud
    | UC_temp ut -> 
    begin
        match vhdl_sens_of_ud pro ud with
        | sens::_ ->
            aux := !aux @ [Data_cond_sens sens];
            vhdl_sig_of_ud pro ud;
        | _ -> vhdl_sig_of_ud pro ud
    end;
    | UC_val uv -> 
        val_str uv.uv_type.uo_expr_type uv.uv_val;
    | UC_sig uo ->
    begin
        match vhdl_sens_of_ud pro ud with
        | sens::_ ->
            aux := !aux @ [Data_cond_sens sens];
            vhdl_sig_of_ud pro ud;
        | _ -> vhdl_sig_of_ud pro ud
    end;
    | _ -> error 504054 "";
    in
  let get_op ops = List.hd ops in
  let cp =
    [
      Data_cond (
        sprintf "%s %s %s"
                (get_arg op1)
                (op_vhdl (get_op ops))
                (get_arg op2)
              
        );
      
    ] in
  [],[],(cp @ !aux)


(*
** Merge all expressions (IMMED,TEMPS) contained in bounded expression list. No ALU 
** is involved. Expression functionality and usage of operators, especially nested expressions,
** can be limited. Maybe expressions were splitted and temporary registers were introduced. 
** Here, we must merge temporary expressions again (TEMPS_nn), resulting eventually in not synthesizable VHDL 
** code! 
** Returns:
**	dp,cp,rest
**
**	  dp: data path list
**	  cp: control path list
**    rest: rest of expression list not merged
*)

let expr_bind pro exprl =
  let idl = ref [] in
  (*
  ** Search for temporary registers TEMPS_nn, treated here as IMMED...
  *)
  let is_tmp_name str =
	let n = String.length str in
	(n > 6) && (String.sub str 0 6) = "TEMPS_" in
  let tmp_id str =
	let vl = Str.split (Str.regexp "_") str in
	(int_of_string (List.nth vl 1))+1001 in
  let is_terminated str =
    (*
    ** dp_conv: either expression or terminated statement;
    *)
    str.[(String.length str)-1] = ';' in
	 
  let is_cond = ref false in
  let is_trans = ref false in
  let aux = ref [] in
  let cp = ref [] in
  let get_op ops = List.hd ops in

  let get_arg ud =
	let vd = vhdl_of_ud pro None ud None in
	aux := !aux @ (List.map (fun sens ->
                  if not !is_cond && not !is_trans then
                      Data_sens sens
                  else if not !is_cond then
                      Data_trans_sens sens
                  else
                      Data_cond_sens sens) vd.dp_sen) @
                  (List.map (fun aux ->
                      Data_out aux) vd.dp_aux) @
                  (List.map (fun aux_sen ->
                      Data_sens aux_sen) vd.dp_aux_sen) @
                  (List.map (fun aux_def ->
                      Data_def aux_def) vd.dp_aux_def) @
                  (List.map (fun s -> Data_top s) vd.top_expr)@
                  (List.map (fun s -> Data_top_def s) vd.top_def);
	vd.dp_sig in
  (*	  	  
          match ud with
          | UC_reg uo -> 
              let sens = List.hd (vhdl_sens_of_ud pro ud) in
              aux := !aux @ [
                  if not !is_cond && not !is_trans then
                      Data_sens sens
                  else if not !is_cond then
                      Data_trans_sens sens
                  else
                      Data_cond_sens sens];
              vhdl_sig_of_ud pro ud
          | UC_temp ut -> 
          begin
              match vhdl_sens_of_ud pro ud with
              | sens::_ ->
                  aux := !aux @ [
                      if not !is_cond && not !is_trans then
                          Data_sens sens
                      else if not !is_cond then
                          Data_trans_sens sens
                      else
                          Data_cond_sens sens];
                  vhdl_sig_of_ud pro ud;
              | _ -> vhdl_sig_of_ud pro ud
          end;
          | UC_val uv -> 
              val_str uv.uv_type.uo_expr_type uv.uv_val;
          | UC_sig uo ->
          begin
              match vhdl_sens_of_ud pro ud with
              | sens::_ ->
                  aux := !aux @ [
                      if not !is_cond && not !is_trans then
                          Data_sens sens
                      else if not !is_cond then
                          Data_trans_sens sens
                      else
                          Data_cond_sens sens];
                  vhdl_sig_of_ud pro ud;
              | _ -> vhdl_sig_of_ud pro ud
          end;
          | UC_sel us -> 
              let vd = vhdl_of_ud pro None ud None in
              aux := !aux @ 
                  (if not !is_cond && not !is_trans then
                      List.map (fun s -> Data_sens s) vd.dp_sen
                   else if not !is_cond then
                      List.map (fun s -> Data_trans_sens s) vd.dp_sen
                   else []) @
                  (List.map (fun s -> Data_in s) vd.dp_aux);
              vhdl_sig_of_ud pro ud; 
          | _ -> error 504053 "";
          in
  *)

  let get_dst ud =
    match ud with
    | UC_reg uo -> 
        let vdl = vhdl_of_ud pro None ud None in
        is_trans := is_local ud;
        aux := !aux @ 
            (List.map (fun v -> Data_out v) vdl.dp_aux);
        if not !is_trans then 
            aux := !aux @ 
                (List.map (fun sv -> Data_def sv) vdl.dp_def)
        else
            aux := !aux @ 
                (List.map (fun sv -> Data_trans_def sv) vdl.dp_def);

        if vdl.cp_sig <> "" then
        begin
            cp := !cp @ 
                [
                    Data_cond vdl.cp_sig;
                ] @
                (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)

        end;
        vdl
    | UC_var uo -> 
        let vdl = vhdl_of_ud pro None ud None in
        aux := !aux @ 
            (List.map (fun v -> Data_out v) vdl.dp_aux);
        aux := !aux @ 
                (List.map (fun sv -> Data_def sv) vdl.dp_def);

        if vdl.cp_sig <> "" then
        begin
            cp := !cp @ 
                [
                    Data_cond vdl.cp_sig;
                ] @
                (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)

        end;
        vdl
    | UC_sig uo -> 
        let vdl = vhdl_of_ud pro None ud None in
        is_trans := is_local ud;
        aux := !aux @ 
            (List.map (fun v -> Data_out v) vdl.dp_aux);
        if not !is_trans then 
            aux := !aux @ 
                (List.map (fun sv -> Data_def sv) vdl.dp_def)
        else
            aux := !aux @ 
                (List.map (fun sv -> Data_trans_def sv) vdl.dp_def);

        if vdl.cp_sig <> "" then
        begin
            cp := !cp @ 
                [
                    Data_cond vdl.cp_sig;
                ] @
                (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)

        end;
        vdl
    | UC_temp ut -> 
        let vdl = vhdl_of_ud pro None ud None in
        is_trans := true;
        aux := !aux @ 
            (List.map (fun v -> Data_out v) vdl.dp_aux);
        if not !is_trans then 
            aux := !aux @ 
                (List.map (fun sv -> Data_def sv) vdl.dp_def)
        else
            aux := !aux @ 
                (List.map (fun sv -> Data_trans_def sv) vdl.dp_def);

        if vdl.cp_sig <> "" then
        begin
            cp := !cp @ 
                [
                    Data_cond vdl.cp_sig;
                ] @
                (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)

        end;
        vdl
    | UC_queue uo -> 
        let vdl = vhdl_of_ud pro None ud None in
        is_trans := is_local ud;
        aux := !aux @ 
            (List.map (fun v -> Data_out v) vdl.dp_aux);
        if not !is_trans then 
            aux := !aux @ 
                (List.map (fun sv -> Data_def sv) vdl.dp_def)
        else
            aux := !aux @ 
                (List.map (fun sv -> Data_trans_def sv) vdl.dp_def);

        if vdl.cp_sig <> "" then
        begin
            cp := !cp @ 
                [
                    Data_cond vdl.cp_sig;
                ] @
                (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)

        end;
        vdl
    | UC_chan uo -> 
        let vdl = vhdl_of_ud pro None ud None in
        is_trans := is_local ud;
        aux := !aux @ 
            (List.map (fun v -> Data_out v) vdl.dp_aux);
        if not !is_trans then 
            aux := !aux @ 
                (List.map (fun sv -> Data_def sv) vdl.dp_def)
        else
            aux := !aux @ 
                (List.map (fun sv -> Data_trans_def sv) vdl.dp_def);

        if vdl.cp_sig <> "" then
        begin
            cp := !cp @ 
                [
                    Data_cond vdl.cp_sig;
                ] @
                (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)

        end;
        vdl
    | UC_sel us -> 
        let vdl = vhdl_of_ud pro None ud None in
        is_trans := is_local ud;
        aux := !aux @ 
            (List.map (fun v -> Data_out v) vdl.dp_aux);
        if not !is_trans then 
            aux := !aux @ 
                (List.map (fun sv -> Data_def sv) vdl.dp_def)
        else
            aux := !aux @ 
                (List.map (fun sv -> Data_trans_def sv) vdl.dp_def);

        if vdl.cp_sig <> "" then
        begin
            cp := !cp @ 
                [
                    Data_cond vdl.cp_sig;
                ] @
                (List.map (fun s -> Data_cond_sens s) vdl.cp_sen)

        end;
        vdl
    | _ -> 
        error 286463 (Cp_printtypes.ui_sprint_uc ud);
    in

  let rec contains_tmp_dst el id =
     match el with
     | e :: tl ->
     begin
        match e.ui_code with
        | Move (dst,_) 
        | Expr(_,dst,_,_) -> 
        begin
            match dst with
            | UC_immed  id' -> (id = id') || (contains_tmp_dst tl id);
	        | UC_reg uo when (is_tmp_name uo.uo_name) -> ((tmp_id uo.uo_name) = id) || (contains_tmp_dst tl id);
            | _ -> (contains_tmp_dst tl id) 
        end;
        | _ -> (contains_tmp_dst tl id);
     end;
     | [] -> false in 

  let rest = ref [] in
  let rec iter el immed_id =
      match el with
      | expr::tl ->
      begin
        rest := tl;
        match expr.ui_code with
        | Expr(ops,dst,op1,op2) -> 
        begin
          let is_relat = List.mem OP_relat (
                    List.map 
                        (fun op -> 
                            if op <> OP_alu then
                                op_mode op
                            else
                                OP_arith
                         ) ops
                       ) in
          (*
          ** UC_immed XX indicate
          ** subexpression evaluation.
          *)
          let op1_eval,op1_id = 
                match op1 with
                | UC_immed id -> true,id;
				| UC_reg uo when (is_tmp_name uo.uo_name) -> true && (contains_tmp_dst tl (tmp_id uo.uo_name)) ,
                                                             tmp_id uo.uo_name;
                | _ -> false,0 in
          let op2_eval,op2_id = 
                match op2 with
                | UC_immed  id -> true,id;
				| UC_reg uo when (is_tmp_name uo.uo_name) -> true && (contains_tmp_dst tl (tmp_id uo.uo_name)),
                                                             tmp_id uo.uo_name;
                | _ -> false,0 in
          let dst_eval,dst_id = 
                match dst with
                | UC_immed  id -> true,id;
				| UC_reg uo when (is_tmp_name uo.uo_name) -> true && (List.mem (tmp_id uo.uo_name) !idl),tmp_id uo.uo_name;
                | _ -> false,0 in
          if op1_id <> 0 then idl := op1_id :: !idl;
          if op2_id <> 0 then idl := op2_id :: !idl;
          
          debug "expr_bind" with (sprintf "expr_bind: Expr: immed_id=%d dst_id=%d dst_eval=%b op1_id=%d op1_eval=%b op2_id=%d op2_eval=%b op=<%s>"
                          	  	  	   immed_id dst_id dst_eval op1_id op1_eval op2_id op2_eval (op_vhdl (get_op ops))); 

          if immed_id = 0 || dst_id = immed_id then
          begin
            if dst_eval then 
            begin
                if op1_eval && not op2_eval then
                begin
                  let op = op_vhdl (get_op ops) in
                  if (vhdl_map op) <> op then
                    sprintf "%s((%s),%s)"
                        (vhdl_map op)
                        (iter tl op1_id)
                        (get_arg op2)
                  else 
                    sprintf "(%s) %s %s"
                        (iter tl op1_id)
                        op
                        (get_arg op2)
                end
                else if op2_eval && not op1_eval then
                begin
                  let op = op_vhdl (get_op ops) in
                  if (vhdl_map op) <> op then
                   sprintf "%s(%s,(%s))"
                        (vhdl_map op)
                        (get_arg op1)
                        (iter tl op2_id)
                  else
                    sprintf "%s %s (%s)"
                        (get_arg op1)
                        op
                        (iter tl op2_id)
                end
                else if not op1_eval && not op2_eval then
                begin
                  let vd = vhdl_of_expr pro op1 op2 (get_op ops) in
                  aux := !aux @ 
                    (List.map (fun s -> Data_top s) vd.top_expr)@
                    (List.map (fun s -> Data_top_def s) vd.top_def) @
                    (List.map (fun sens ->
                      if not !is_cond && not !is_trans then
                        Data_sens sens
                      else if not !is_cond then
                        Data_trans_sens sens
                      else
                        Data_cond_sens sens) vd.dp_sen) @
                    (List.map (fun aux ->
                        Data_out aux) vd.dp_aux) @
                    (List.map (fun aux_sen ->
                        Data_sens aux_sen) vd.dp_aux_sen) @
                    (List.map (fun aux_def ->
                        Data_def aux_def) vd.dp_aux_def);
                  vd.dp_sig
                  (*
                   sprintf "%s %s %s"
                   (get_arg op1)
                   (op_vhdl (get_op ops))
                   (get_arg op2)
                  *)
                end
                else
                begin
                  let op = op_vhdl (get_op ops) in
                 if (vhdl_map op) <> op then
                    sprintf "%s((%s),(%s))"
                        (vhdl_map op)
                        (iter tl op1_id)
                        (iter tl op2_id)
                 else
                    sprintf "(%s) %s (%s)"
                        (iter tl op1_id)
                        op
                        (iter tl op2_id);
                end;
            end
            else
            begin
                let lhs_et = (type_of_ud dst).uo_expr_type in
                let rhs1_et = (type_of_ud op1).uo_expr_type in
                let rhs2_et = (type_of_ud op2).uo_expr_type in
                let bool_reg_conv = (lhs_et = DT_bool) && ((rhs1_et <> DT_bool) || is_relat) in

                let dst = get_dst dst in
                if bool_reg_conv then dst.dp_conv <- "B_to_L(%s)";

                aux := !aux @ 
                    (List.map (fun aux_def ->
                        Data_def aux_def) dst.dp_aux_def);
                if op1_eval && not op2_eval then
                begin
                  let op = op_vhdl (get_op ops) in
                  if not (is_terminated dst.dp_conv) then
                  begin
                    if (vhdl_map op) <> op then
                      sprintf "%s <= %s;"
                        dst.dp_sig
                        (sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "%s((%s),%s)"
                                          (vhdl_map op)
                                          (iter tl op1_id)
                                          (get_arg op2)))
                    else
                      sprintf "%s <= %s;"
                        dst.dp_sig
                        (sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "(%s) %s %s"
                                          (iter tl op1_id)
                                          op
                                          (get_arg op2)))
                  end
                  else
                  begin
                    if (vhdl_map op) <> op then
                      sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "%s((%s),%s)"
                                          (vhdl_map op)
                                          (iter tl op1_id)
                                          (get_arg op2))
                     else
                      sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "(%s) %s %s"
                                          (iter tl op1_id)
                                          op
                                          (get_arg op2))
                  end;
                end
                else if op2_eval && not op1_eval then
                begin
                  let op = op_vhdl (get_op ops) in
                  if not (is_terminated dst.dp_conv) then
                  begin
                    if (vhdl_map op) <> op then
                      sprintf "%s <= %s;"
                        dst.dp_sig
                        (sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "%s(%s,(%s))"
                                          (vhdl_map op)
                                          (get_arg op1)
                                          (iter tl op2_id)))
                    else
                      sprintf "%s <= %s;"
                        dst.dp_sig
                        (sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "%s %s (%s)"
                                          (get_arg op1)
                                          op
                                          (iter tl op2_id)))
                  end
                  else
                  begin
                    if (vhdl_map op) <> op then
                      sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "%s(%s,(%s))"
                                          (vhdl_map op)
                                          (get_arg op1)
                                          (iter tl op2_id))
                    else
                      sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "%s %s (%s)"
                                          (get_arg op1)
                                          op
                                          (iter tl op2_id))
                  end;
                end
                else if not op1_eval &&
                        not op2_eval then
				begin
				  let vd = vhdl_of_expr pro op1 op2 (get_op ops) in
				  aux := !aux @ 
                  	(List.map (fun s -> Data_top s) vd.top_expr)@
                  	(List.map (fun s -> Data_top_def s) vd.top_def)@
					(List.map (fun sens ->
                  	  if not !is_cond && not !is_trans then
                      	Data_sens sens
                  	  else if not !is_cond then
                      	Data_trans_sens sens
                  	  else
                      	Data_cond_sens sens) vd.dp_sen) @
                    (List.map (fun aux ->
                        Data_out aux) vd.dp_aux) @
                    (List.map (fun aux_sen ->
                        Data_sens aux_sen) vd.dp_aux_sen) @
                    (List.map (fun aux_def ->
                        Data_def aux_def) vd.dp_aux_def);
                  if not (is_terminated dst.dp_conv) then
                    sprintf "%s <= %s;"
                          dst.dp_sig
                         (sprintf (Obj.magic dst.dp_conv) vd.dp_sig)
                  else
                    sprintf (Obj.magic dst.dp_conv) vd.dp_sig;
				  (*
                                 (sprintf "%s %s %s"
                                          (get_arg op1)
                                          (op_vhdl (get_op ops))
                                          (get_arg op2)))
				  *)
	  	  	  	end
                else
                begin
                  let op = op_vhdl (get_op ops) in
                  if not (is_terminated dst.dp_conv) then
                  begin
                    if (vhdl_map op) <> op then
                      sprintf "%s <= %s;"
                        dst.dp_sig
                        (sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "%s((%s),(%s))"
                                          (vhdl_map op)
                                          (iter tl op1_id)
                                          (iter tl op2_id)))
                    else
                      sprintf "%s <= %s;"
                        dst.dp_sig
                        (sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "(%s) %s (%s)"
                                          (iter tl op1_id)
                                          op
                                          (iter tl op2_id)))
                  end
                  else
                  begin
                    if (vhdl_map op) <> op then
                      sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "%s((%s),(%s))"
                                          (vhdl_map op)
                                          (iter tl op1_id)
                                          (iter tl op2_id))
                    else
                      sprintf (Obj.magic dst.dp_conv) 
                                 (sprintf "(%s) %s (%s)"
                                          (iter tl op1_id)
                                          op
                                          (iter tl op2_id))
                  end;
                end;
             end;                        
          end
          else 
            iter tl immed_id;
        end;
        | Falsejump (dst,label) ->
          let dst_eval,immed,dst_id = 
                match dst with
                | UC_immed  id -> true,true,id;
                | UC_alu ua ->
                    if ua.ua_type <> DT_bool then
                        error 558500 "";
                    true,false,0
                | _ -> false,false,0 in

          debug "expr_bind"  with (sprintf "expr_bind: Falsejump: immed_id=%d dst_id=%d"
                          	  	  	  	immed_id dst_id); 

          is_cond := true;
          if dst_eval && immed then
          begin
            iter tl dst_id 
          end
          else if dst_eval then
          begin
            match tl with
            | next::tl' ->
            begin
              match next.ui_code with 
              | Expr(ops,lhs,rhs1,rhs2) ->
                let alu_dt = dt_of_udl [lhs;rhs1;rhs2] in
                let alu = find_alu pro alu_dt in
				let dp',_,_ = expr_assign pro ops lhs rhs1 rhs2 in
                aux := !aux @ dp' @ [Data_cond_sens
                                     (sprintf "alu_%s_bool"
                                              alu.alu_name)];
                rest := [];
                sprintf "alu_%s_bool = '1'" alu.alu_name 
              | _ -> error 902147 "";
            end;
            | [] -> error 267467 "";
          end
          else
            error 567116 "";
  	  	| Nop -> iter tl immed_id;
        | _ -> 
            error 667658 "";
      end;
      | [] -> "";
      in
  let res = iter (List.rev exprl) 0 in
  let dp = 
              [
                  if not !is_cond && not !is_trans then
                      Data_out res
                  else if not !is_cond then
                      Data_trans res
                  else
                      Data_cond res
              ] @ !aux in
  (*
  ** Write Control Path.
  *)
  debug "expr_bind" with (sprintf ">>> #dp=%d #cp=%d #aux=%d #rest=%d" 
  	  	  	  	  	  	  	  	(List.length dp)
								(List.length !cp)
								(List.length !aux)
								(List.length !rest));
  dp,!cp,(List.rev !rest)

