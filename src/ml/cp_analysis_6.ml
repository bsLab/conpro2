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
**    $VERSION:     2.33
**
**    $INFO:
**
**  Analysis part of compiling process: syntax and structure compiling.
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
open Cp_block_frame
open Cp_printtypes

open Unix
open Printf

open Cp_analysis_1
open Cp_analysis_2
open Cp_analysis_3
open Cp_analysis_4
open Cp_analysis_5

let check_dyn ot at =
  match ot with
  | OT_reg co 
  | OT_var co -> if not (List.mem Obj_array co.co_flags)
                 then co.co_flags <- co.co_flags @ [Obj_array]; 
  | OT_queue qu -> if not (List.mem Obj_array qu.qu_obj.co_flags)
                   then qu.qu_obj.co_flags <- qu.qu_obj.co_flags @ [Obj_array];
  | OT_channel ch -> if not (List.mem Obj_array ch.ch_obj.co_flags)
                   then ch.ch_obj.co_flags <- ch.ch_obj.co_flags @ [Obj_array];
  | OT_struct _ -> ()
  | OT_object ao -> 
      let rules = ao.ao_type.ta_rules in
      if (rules.rl_interp "get $ARRAY") = "not found" then 
        error 0 (sprintf
                  "Dynamic selector with Array <%s> not supported for this particular abstract object type."
                  at.at_name) 
  | _ -> error 0 (sprintf
                  "Dynamic selector with Array <%s> not supported for this object type."
                  at.at_name) 

let array_change_dyn at =
  let size = proda at.at_dim in
  let is_block = List.mem AT_block at.at_flags in
  if not is_block then
  begin
    Array.iter (fun ot -> check_dyn ot at) at.at_objs;
    if not (List.mem AT_dyn at.at_flags) then
    begin
      at.at_flags <- at.at_flags @ [AT_dyn];
      (*
      ** If array elements are of type abstract object, all AOs 
      ** must have an updated environment reflecting AT_dyn!
      *)
      let i = ref 0 in
      let acc = ref "" in
      Array.iter (fun ot ->
        match ot with
        | OT_object ao ->
          let rules = ao.ao_type.ta_rules in
          __(rules.rl_interp (sprintf "set $ARRAY 1" ));
          acc := sprintf "%s %s" !acc (rules.rl_interp "get_access");
          incr i;
        | _ -> ();
        ) at.at_objs;
      Array.iter (fun ot ->
        match ot with
        | OT_object ao ->
          let rules = ao.ao_type.ta_rules in
          let acc_l = Str.split (Str.regexp " ") !acc in
          List.iter (fun acc ->
            match Str.split (Str.regexp ":") acc with
            | [meth;pro] -> __(rules.rl_interp (sprintf "access %s %s"  meth pro))
            | [] -> ();
            | _ -> progerr "access format";
            ) acc_l;
        | _ -> ()
        ) at.at_objs;
      warning (sprintf 
        "Changing non block array <%s> to dynamic selection mode." 
        at.at_name);
        
      (*
      ** If multidimensional, array dimensions
      ** must be aligned to power 2. New objects
      ** must be created, too!
      *)
      let more = ref 0 in
      if (Array.length at.at_dim) > 1 then
      begin
        at.at_dim <- Array.map (fun d->
          let w = const_width (V_int
                              (Int64.of_int d)) in 
          let d1 = 1 lsl (w-1) in
          let d2 = 1 lsl w in
          if d1 = d then d else 
          begin
            out (sprintf "Array <%s>: expanding dimension %d to %d."
                          at.at_name
                          d d2);
            more := !more + (d2-d1);
            d2 
          end;
          )at.at_dim;
        let last = size in
        let name = at.at_name in
        let dt = 
          match dt_of_ot at.at_objs.(0) with
          | Some dt -> dt;
          | None -> error 352252 "" in
        let objs' = Array.init !more (fun n ->
                  let name' = sprintf "%s_%d" name (last+n) in
                  OT_reg {(new_obj name' dt) with 
                          co_init = V_null;
                          co_index = (last+n);
                          co_array = [at];
                          }
                          ) in
        at.at_objs <- Array.append at.at_objs objs';
      end
    end
  end
  
(*
** Extract and compile one (executive) instruction.
** Translation syntax -> PI.
** Do first checks: 
**      1. Object type consistency
**      2. Data type consistency in expressions
**
*)
let rec get_instr instr =
  debug "get_instr" with  (sprintf ">>>> get_instr %s" (print_src (source ())));
  let instrl = ref [] in
  List.iter (fun instr -> 
    let instrl' = 
        let get_instr1 instr =
          match get_instr instr with
          | [hd] -> hd
          | [] -> error 0 (sprintf "get_instr1: empty instruction list found");
          | _ -> error 0 (sprintf "get_instr1: instruction list contains more than 1 instruction") in

        (*
        ** Get value or object name, but try first constant folding if
        ** any...
        *)
        let get_const_name vname =
            (*
            ** Maybe constant arithmetic expression.
            *)
            let pi = expr_fold(get_instr1 vname) in
            match pi with
            | PI_obj (_,OT_value (V_int i64)) ->
                Int64.to_string i64;
			| PI_arithm _ -> "$";
            | _ ->
                get_name vname in

        let get_const_names vnamel =
            match vnamel with
            | T_list sl -> List.map get_const_name sl;
            | _ -> [get_const_name vnamel] in

        let get_const_val v = 
          match expr_fold (get_instr1 v) with
          | PI_obj (opl,ot) ->
          begin
            match ot with
            | OT_named_value (_,v)
            | OT_value v -> v;
            | OT_const co -> co.co_init;
            | _ -> error 0 (sprintf "Unexpected non-constant object found: %s." 
                                    (ast_sprint_syntax v));
          end;
          | _ -> error 0 (sprintf "Unexpected non-constant expression found: %s." 
                                  (ast_sprint_syntax v)) in

        let check_op () =
            (*
            ** Instruction not allowed in module toplevel!
            *)
            if an.a_toplevel then
                error  0 (sprintf "\nProcess instruction occurred while in module toplevel mode in module %s." 
                        an.a_mname) 
            in

        let check_signal_map lhs rhs =
            let loc = ref ' ' in
            (*
            ** LHS: only signals
            ** RHS: only signals, registers, values and expressions
            ** In process: not both LHS and RHS may be external
            *)
            let rec get_obj ident =
              match ident with
              | T_ident _ ->
              begin
                let name = get_name ident in
                (*
                ** A constant value ?
                *)
                
                if check_const name then
                    get_const ident
                else
                begin
                    (*  
                    ** First check all local symbols...
                    *)
                    if (pro_sym_check name) then
                    begin
                        loc := 'l';
                        let obj = pro_sym_lookup name in
                        obj;
                    end
                    else
                    begin
                        loc := 'e';
                        (*
                        ** Module level
                        *)
                        if (sym_check_obj an.a_modu.mod_objs name) then
                        begin
                            let obj = sym_get_obj an.a_modu.mod_objs name in
                            obj;
                        end
                        else
                        begin
                            (*
                            ** External modules (included external modules)
                            *)
                            if sym_check_obj_ext an.a_modu name then
                                (sym_get_obj_ext an.a_modu name)
                            else
                                error 0 (sprintf
                                   "\nUnknown LHS Symbol <%s> in mapping instruction."
                                   name);
                        end;
                    end;
                end;
              end;
              | T_selector (obj,sel) -> 
              begin
                let is_sel2,name = 
                  (*
                  ** Nested array and structure selectors ?
                  *)
                  match obj with
                  | T_selector (obj',sel') -> 
                    let sel_name = get_name sel' in
                    true,
                    sprintf "%s_%s" (get_name obj) sel_name
                  | _ -> 
                    false,
                    get_name obj  in
                let aobj = get_instr1 (T_ident (pos_of_source an.a_curpos,name)) in
                __(get_obj (T_ident (pos_of_source an.a_curpos,name))); (* just for loc *)

                match aobj with
                | PI_obj (opl,ot) -> 
                begin
                    match ot with
                    | OT_struct st
                    | OT_component st ->
                        let sel= sprintf "%s_%s" st.st_name
                                         (get_name sel) in
                        let ot = 
                          try
                            List.find (fun ot ->
                                let el_name = name_of_ot ot in
                                el_name = sel
                                 ) st.st_objs 
                          with
                          | Not_found -> error 0 (sprintf "Unknown structure element selector found: %s." sel);
                          in
                        ot
                    | OT_array at ->
                        let is_block = List.mem AT_block at.at_flags in
                        if not is_block then
                        begin
                            let sel_str=get_const_name sel in
                            let w = ref Int64.zero in
                            let i = Int64.to_int !w in
                            let is_num = protects (w := Int64.of_string sel_str) in
                            if is_num then
                                at.at_objs.(i)
                            else
                                error 0 "Dynamic array access not possible in mapping instruction.";
                            
                        end
                        else
                            error 0 "Block array in mapping instruction not possible.";
                    | _ -> error 0 "Unexpected LHS object in mapping instruction.";
                end;
                | _ -> error 0 "Uunexpected LHS symbol in mapping instruction.";
              end;
              | T_typeconv (tt,vexpr) -> get_obj vexpr;
              | T_sub (obj,sub) -> get_obj obj;
              | _ -> error 0 "\ninvalid object in map instruction?";
              in
            let lhs_dt =
                match get_obj lhs with
                | OT_signal co -> co.co_type;    (* OK *)
                | _ -> error 0 "Unexpected object type in LHS of mapping instruction.";
                in
            let rec check_rhs synt =
                match synt with
                | T_ident _ -> 
                begin
                    match get_obj synt with 
                    | OT_signal _
                    | OT_value _
                    | OT_named_value _
                    | OT_const _
                    | OT_reg _ -> (); (* OK *)
                    | OT_array at ->
                        let is_block = List.mem AT_block at.at_flags in
                        if not is_block then
                        begin
                            match at.at_objs.(0) with 
                            | OT_signal _
                            | OT_value _
                            | OT_named_value _
                            | OT_component _
                            | OT_const _
                            | OT_reg _ -> (); (* OK *)
                            | _ -> 
                                error 0 
                                    "Invalid object type in RHS of array mapping instruction.";
                        end
                        else
                            error 0 "Block array in RHS of mapping instruction ot possible.";
                    | OT_component _ -> ();
                    | OT_struct st -> 
                        begin
                            match List.hd st.st_objs with 
                            | OT_signal _
                            | OT_value _
                            | OT_named_value _
                            | OT_const _
                            | OT_reg _ -> (); (* OK *)
                            | _ -> 
                                error 0 
                                    "Invalid object type in RHS of structure mapping instruction.";
                        end;
                    | _ -> 
                        error 0 "Invalid object type in RHS of mapping instruction.";
                end;
                | T_OP_arith (op,op1,op2) -> check_rhs op1;
                                            check_rhs op2;                
                | T_OP_bool (kind,op,op1,op2) -> check_rhs op1;
                                                 check_rhs op2;                
                | T_concat (op1,op2) -> check_rhs op1;
                                        check_rhs op2;
                | T_typeconv (tt,vexpr) -> check_rhs vexpr;
                | T_selector (obj,sel) -> check_rhs obj;
                | T_sub (obj,sub) -> check_rhs obj;
                | _ -> error 0 "Invalid expression in RHS of mapping instruction.";
                in
            check_rhs rhs;
            let pi = PI_assign (source (),get_instr1 lhs,get_instr1 rhs) in
            if not (check_expr pi) then 
                error 0 "Type check or range overflow error in signal mapping expression found.";
            let lhs_loc,rhs_loc =
                loc := ' ';
                __(get_obj lhs);
                let l = !loc = 'l' in
                loc := ' ';
                __(get_obj rhs);
                let r = !loc = 'l' in
                l,r
                in
             if not an.a_toplevel && not lhs_loc && not rhs_loc then
                error 0 "Found external objects both in LHS and RHS of signal mapping expression.";
             in

        match instr with
        | T_instr instr -> get_instr instr;
        
        | T_topinstr instr -> if an.a_toplevel then get_instr instr
                              else
                                error 0 (sprintf
                                       "\nFatal: toplevel instruction in process <%s> in module <%s> found!"
                                       an.a_pro_name an.a_mname);
                                       
        | T_assign (lhs,rhs) ->
            check_op ();
            let pi = PI_assign (source (),get_instr1 lhs,get_instr1 rhs) in
            if not (check_expr pi) then error 0 "Type check or range overflow error in assignment.";
            [pi]
            
        | T_export ol ->
          List.iter (fun o ->
            if an.a_toplevel then
            begin
                (*
                ** Module export
                *)
                let ob_name = 
                    match o with
                    | T_selector (obj,sel) -> 
                        error 0 (sprintf "\nCan't export component or structure element <%s.%s>."
                                          (get_name o) (get_name sel));
                    | _ -> get_name o in
                if (sym_check an.a_modu.mod_objs ob_name) then
                begin
                    (*
                    ** Special case: component export in toplevel!
                    ** In this case this is an internal ,not external
                    ** signal direction view! Therefore reverse
                    ** signal directions! (indicated now only by Obj_port flag)
                    *)
                    let sym = sym_lookup an.a_modu.mod_objs ob_name in
                    
                    match sym with
                    | Sym_obj (OT_struct st) ->
                      sym_add an.a_modu.mod_export sym;  
                      List.iter (fun ot ->
                                (*
                                ** Mark all objects with export flag
                                *)
                                match co_of_ot ot with
                                | Some co -> 
                                  if not (List.mem Obj_port co.co_flags) then 
                                    co.co_flags <- co.co_flags @
                                        [Obj_port];                                
                                | None -> ();   (* ??? *)
                            ) st.st_objs;
                    | Sym_obj (OT_component st) ->
                      sym_add an.a_modu.mod_export sym;
                      List.iter (fun ot ->
                                match co_of_ot ot with
                                | Some co -> 
                                  let flags = co.co_flags in
                                  (*
                                  co.co_flags <- List.map (fun f->
                                      if f = Obj_port_in then Obj_port_out else
                                      if f = Obj_port_out then Obj_port_in else
                                      f ) flags;
                                  *)
                                  if not (List.mem Obj_port co.co_flags) then 
                                    co.co_flags <- co.co_flags @
                                        [Obj_port];                                
                                | None -> ();   (* ??? *)
                            ) st.st_objs;
                    | Sym_obj (OT_array at) ->
                      sym_add an.a_modu.mod_export sym; 
                      (*
                      Array.iter (fun ot ->
                        sym_add an.a_modu.mod_export (Sym_obj ot);
                        ) at.at_objs;
                      *)
                    | _ -> sym_add an.a_modu.mod_export sym;
                end
                else
                    error 0 (sprintf "\nUnknown export symbol <%s> in module <%s>."
                                   ob_name an.a_modu.mod_name);
            end
            else
            begin
                (*
                ** Process export
                *)
                let ob_name = get_name o in
                if (sym_check (get_some an.a_pro_syms) ob_name) then
                begin
                    (*
                    ** Move symbol from process to module symbol table.
                    ** Add symbol both to import and export table.
                    *)
                    let sym = sym_lookup (get_some an.a_pro_syms) ob_name in
                    sym_add (get_some an.a_pro_export) sym;
                    sym_add (get_some an.a_pro_import) sym;
                    sym_move (get_some an.a_pro_syms)
                             an.a_modu.mod_objs ob_name;
                end
                else
                    error 0 (sprintf "\nUnknown export symbol <%s> in process <%s>."
                                   ob_name an.a_pro_name);
            end;
          ) ol;
          [PI_nop]
          
        | T_map (lhs,rhs) ->
            (*
            ** LHS <=> RHS
            *)
            check_signal_map lhs rhs;
            [PI_map (source (),get_instr1 lhs,get_instr1 rhs)];
            
        | T_OP_arith (op,op1,op2) ->
            if op1 <> T_empty then
                [PI_arithm ((op_type op),get_instr1 op1,get_instr1 op2)]
            else
            begin
                let obj = get_instr op2 in
                let flags =
                    match op_type op with
                    | OP_sub -> [OD_aneg];
                    | OP_lnot -> [OD_lneg];
                    | _ -> error 0 "\nUnknown prefix operator";
                    in

                match obj with
                | (PI_obj (opl,ot))::_ ->
                    [PI_obj (opl@flags,ot)];
                | _ -> error 0 "\nUnexpected prefix operand";
            end;
            
        | T_OP_bool (kind,op,op1,op2) ->
            if op1 <> T_empty then
              [PI_bool (kind,(op_type op),get_instr1 op1,get_instr1 op2)]
            else
            begin
                let obj = get_instr op2 in
                let flags =
                    match op_type op with
                    | OP_bnot -> [OD_bneg];
                    | _ -> error 0 "\nUnknown boolean prefix operator";
                    in

                match obj with
                | (PI_obj (opl,ot))::_ ->
                    [PI_obj (opl@flags,ot)];
                | _ -> error 0 "\nUnexpected prefix operand";
            end;
            
        | T_unit ->
            [PI_obj ([],OT_value V_null)];
            
        | T_string (pos,str) ->
            [PI_obj ([],OT_value (V_string str))];
            
        | T_character (pos,c) ->
            [PI_obj ([],OT_value (V_char c))];

        | T_objlist cl ->
            let is_tuple = ref false in
            let vl = ref [] in
            List.iter (fun obj ->
                           match obj with
                           | T_character _ 
                           | T_ident _ ->
                                let name = get_name obj in
                                is_tuple := !is_tuple || (not (check_const name));
                                  
                                vl := !vl @ [obj];
                           | _ -> 
                                is_tuple := true;
                                vl := !vl @ [obj]; ) cl;
            if not !is_tuple then 
              [PI_obj ([],OT_value (V_list (List.map get_val !vl)))]
            else
              [PI_tuple (List.map get_instr1 !vl)];

        | T_concat (op1,op2) ->
            check_op ();
            [PI_concat (get_instr1 op1,get_instr1 op2)];
            
        | T_waitfor (expr,time,e_false,e_true) ->
            check_op();
            let expr = get_instr1 expr in
            let t_v,t_u=get_time time in
            let src = source () in
            if expr <> PI_nop &&
               (not (check_expr expr)) then 
                error 0 "Type check or range overflow error in wait-for expression.";
            let pi1 = get_instr1 e_false in
            let pi2 = get_instr1 e_true in
            [PI_waitfor (src,expr,t_v,t_u,pi1,pi2)];
            
        | T_branch (expr,s1,s2) ->
            check_op();
            let expr = expr_fold (get_instr1 expr) in
            if not (check_expr expr) then error 0 "Type check or range overflow error in branch expression.";
            if not (is_const_expr expr) then 
            begin
                let src = source () in
                [PI_branch (src,
                       expr,
                       (get_instr1 s1),
                       (get_instr1 s2))]
            end
            else
            begin
              let i = Int64.to_int (get_const_expr expr) in
              if i = 1 then
              begin
                info (sprintf "Replacing branch with conditionial true block due to constant expression.");
                [get_instr1 s1];
              end
              else
              begin
                match s2 with
                | T_empty -> 
                  info (sprintf "Removing branch due to constant expression (false branch not present).");
                  [];
                | _ ->
                  info (sprintf "Replacing branch with conditionial false block due to constant expression.");
                  [get_instr1 s2];
              end;
            end;
            
        | T_forloop (expr,dir,lim1,lim2,block) ->
            check_op ();
            let is_unroll =
                match block with
                | T_block (vb,vpo) ->
                  let optl = get_param_list (match vpo with Some vp ->vp| None -> T_empty) in
                  let bind = get_param "unroll" optl in
                  (bind <> None);
                | _ -> false; in  
            
            let lim1 = expr_fold (get_instr1 lim1) in
            let lim2 = expr_fold (get_instr1 lim2) in
            let get_lim_dt lim =
              let bound v =
                let b = const_width v in
                let v = match v with
                        | V_int i64 -> i64;
                        | _ -> error 716152 "" in
                (*
                ** Binary limit value "11" must be extended to 3 bits!
                *)
                let v' = Int64.shift_left Int64.one b in
                if (Int64.sub v' Int64.one) = v then
                    b+1
                else b in

              match lim with
              | PI_obj (opl,ot) ->
              begin
                match ot with
                | OT_named_value (_,v)
                | OT_value v -> DT_int (max 2 (bound v)),Some v;
                | OT_const co -> DT_int (max 2 (bound co.co_init)),
                                 Some co.co_init;
                | _ -> let _,dt = expr_type lim true in dt,None
                end;
              | _-> let _,dt = expr_type lim true in dt,None
              in
            let dt_a,val_a = get_lim_dt lim1 in
            let dt_b,val_b = get_lim_dt lim2 in
            let dt = if (size_of_dt dt_a) > 
                        (size_of_dt dt_b) then
                        dt_a else dt_b in
             
            let src = source () in           
            if not is_unroll then
            begin
             let expr,name,name' =
              match expr with
              | T_ident ind ->
				let name = get_name expr in 
                let name' = sprintf "LOOP_%s_%d" name an.a_loop_index in
				an.a_loop_index <- an.a_loop_index + 1;
                if (pro_sym_check name) then
                begin
                  (*
                  ** Exists already, object covering not allowed...
                  *)

                  error 0 (sprintf "\nFor-loop variable name <%s> is coverd by already existing object."
				  	  	  	  	  	name);
                end
                else
                begin
				  (*
				  ** Index register must be always of type integer!
				  *)
				  match dt with
				  | DT_int n ->
					(*
					** Absolute value Range is limited to n-1 bits, and
					** expression type may consist of to_int converted 
					** data objects, therefore add 1 bit to make sure
					** to cover the range of n-bit unsigned objects, too.
					*)
					let dt = DT_int (n+1) in
                	let co = new_obj name' dt in
                	co.co_flags <- co.co_flags @ [Obj_loop];
                	co.co_process <- an.a_pro;
                	let obj = OT_reg co in
                	sym_add (get_some an.a_pro_syms) (Sym_obj obj);
                	assign_rule obj;
                	PI_obj ([],obj),name,name';					
				  | _ -> error 0 (sprintf "\nFor-loop expression <%s> must be of type integer!"
				  	  	  	  	  	  	  name);
                end;                
              | _ -> error 0 "\nUnexpected loop variable found.";
             in
             if not (check_expr lim1) then error 0 "Type check or range overflow error in loop limit 1.";
             if not (check_expr lim2) then error 0 "Type check or range overflow error in loop limit 2.";
             let src = source () in
			 (*
			 ** Substitute index <name> with unique <name'>
			 *)
			 let old_subst = an.a_pro_subst in
			 an.a_pro_subst <- (name,name') :: an.a_pro_subst;
             let block = get_instr1 block in
			 an.a_pro_subst <- old_subst;
             [PI_forloop (src,
                       expr,
                       dir,
                       lim1,
                       lim2,
                       block)];
            end
            else 
            begin
              (*
              ** Process loop to be unrolled..
              *)
              let optl' =
                match block with
                | T_block (vb,vpo) ->
                  let optl = get_param_list (match vpo with Some vp -> vp|None -> T_empty) in
                  List.filter (fun (n,v) -> n <> "unroll") optl 
                | _ -> [] in 
				
		      let opl = to_param_list optl' in
              let iname =
                  match expr with
                  | T_ident ind -> 
                    let name = get_name expr in
                    name
                  | _ -> error 0 "\nNon constant expression found in for-loop to be unrolled.";   
                  in
              let blocks = ref [] in
              let a =
                  match val_a with
                  | Some (V_int w) -> Int64.to_int w;
                  | _ -> error 0 "\nCan't unroll non static loop."; in
              let b =
                  match val_b with
                  | Some (V_int w) -> Int64.to_int w;
                  | _ -> error 0 "\nCan't unroll non static loop."; in
              out (sprintf "Unrolling loop for <%s> = {%d,%d}" 
                           iname a b);
              if a < b then
                  for i = a to b
                  do
                      let old_subst = an.a_pro_subst in
                      an.a_pro_subst <- [iname,sprintf "%d" i] @ an.a_pro_subst;
                      (match get_instr1 block with
                       | PI_block (isl,bf) ->
                          blocks := !blocks @ isl;
                       | instr ->
                          blocks := !blocks @ [instr];
                      );
                      an.a_pro_subst <- old_subst;
                  done
              else
                  for i = a downto b
                  do
                      let old_subst = an.a_pro_subst in
                      an.a_pro_subst <- [iname,sprintf "%d" i] @ an.a_pro_subst;
                      (match get_instr1 block with
                       | PI_block (isl,bf) ->
                          blocks := !blocks @ isl;
                       | instr ->
                          blocks := !blocks @ [instr];
                      );
                      an.a_pro_subst <- old_subst;
                  done;
              out (sprintf "Extracted %d instructions in unrolled process block."
                           (List.length !blocks));
			  let bf = create_frame !blocks in
			  bf.bf_params <- opl;
              [PI_block (!blocks,bf)];
            end;

        | T_loop (kind,expr,block) ->
            check_op();
            let expr = get_instr1 expr in
            let src = source () in
            if expr <> PI_nop && 
               not (check_expr expr) then error 0 "Type check or range overflow error in loop expression.";
            [PI_loop (src,
                      kind,
                      expr,
                      (get_instr1 block))];

        | T_select (expr,csl) ->
            check_op();
            let src = source () in
            let expr = expr_fold (get_instr1 expr) in
            if not (check_expr expr) then error 0 "Type check or range overflow error in match expression.";
            if not (is_const_expr expr) then 
            begin
              [PI_select (src,
                       expr,
                       (get_instr1 csl))];
            end
            else
            begin
              let i = Int64.to_int (get_const_expr expr) in
              info (sprintf "Match with constant expression <%d>..." i);
              let csl' =
                match csl with
                | T_block (T_list il,_) -> il
                | _ -> [] in
              let matching = List.flatten (List.map (fun cs ->
                match cs with
                | T_case (exprl,il) ->
                  let exprl = List.map (fun expr -> expr_fold (get_instr1 expr)) exprl in
                  let equals = List.filter (fun expr ->
                    if is_const_expr expr then
                    begin
                      let i' = Int64.to_int (get_const_expr expr) in
                      i=i'
                    end  
                    else
                      false
                    ) exprl in
                  if equals <> [] then [get_instr1 il] else []
                 | _ -> []
                ) csl') in
              if matching = [] then
                [PI_select (src,
                       expr,
                       (get_instr1 csl))]
              else
              begin
                info (sprintf "Replacing match with conditionial block due to constant expression.");
                let bf = create_frame matching in
                [PI_block (matching,bf)]
              end;
            end;
                       
        | T_try (block,csl) ->
            [PI_try ((get_instr1 block),
                     (get_instr1 csl))];
                       
        | T_case (exprl,il) ->
            check_op();
            let exprl = List.map (fun expr -> 
                    match expr with
                    | T_interval (a,b) ->
                        PI_list [get_instr1 a;get_instr1 b];
                    | _ -> get_instr1 expr) exprl in
            let src = source () in
            [PI_case (src,
                      exprl,
                     (get_instr1 il))];
        | T_raise id ->
          let name = get_name id in
          if sym_check_type an.a_modu.mod_objs name then
          begin
            match sym_get_type an.a_modu.mod_objs name with
            | Type_exc ex -> [PI_raise ex.tx_id];
            | _ ->    
              error 0 (sprintf
                       "Unknown exception <%s> found."
                       name);
          end
          else
            error 0 (sprintf
                       "Unknown exception <%s> found."
                       name);
                       
        | T_Fun (fname,fargs,fret) ->
        begin
            (* check_op(); *)
            match fname with
            | T_selector (toname,tsname) ->
                let oname = get_name toname in
                let sname = get_name tsname in
                let src = source () in
                let lookup_ot oname =
                  if sym_check_obj an.a_modu.mod_objs oname then
                    sym_get_obj an.a_modu.mod_objs oname
                  else if an.a_pro <> None && 
                          sym_check_obj (get_some an.a_pro).pro_objs oname then
                    sym_get_obj (get_some an.a_pro).pro_objs oname 
                  else 
                    error 0 (sprintf 
                      "Undefined abstract object <%s> in method call <%s> found." 
                      oname sname) in
                let get_ot oname =
                  match lookup_ot oname with
                  | OT_object ao -> ([],OT_object ao),ao;
                  | OT_array at -> 
                  begin
                    let size = proda at.at_dim in
                    (*
                    ** oname can be a selector, too.
                    *)
                    match toname with 
                    | T_selector (obj',sel') ->
                    begin
                      (*
                      ** Must be a one-dim. array!
                      *)
                      let is_static = List.mem AT_static at.at_flags in
                      let sel_str= get_const_name sel' in
                      let array_size = at.at_dim.(0) in
                      let w = ref Int64.zero in
                      let is_num = protects 
                        (w := Int64.of_string sel_str) in
                      if is_static && not is_num then
                        error 0 (sprintf "Found static array <%s> with dynamic selector!"
                                         at.at_name);
                      let i = Int64.to_int !w in
                      if is_num then
                      begin
                        if i < 0 ||
                           i >= array_size then
                                  error 0 (sprintf "Array <%s> accessed with out of bound index [expected 0..%d, got %d]."
                                                   oname (array_size-1) i);
                        match at.at_objs.(i) with
                        | OT_object ao -> 
                            if not is_static then
                                ([OD_sel [|i|]],OT_array at),ao
                            else
                                ([],OT_object ao),ao
                        | OT_queue qu ->
                        begin
                          let ao = qu.qu_ao in
                          if not is_static then
                           ([OD_sel [|i|]],OT_array at),ao
                          else
                            ([],OT_object ao),ao;
                        end;
                        | OT_channel ch ->
                        begin
                          let ao = ch.ch_ao in
                          if not is_static then
                           ([OD_sel [|i|]],OT_array at),ao
                          else
                            ([],OT_object ao),ao;
                        end;
                        | _ -> error 0 
                                (sprintf "Not an abstract object array <%s> in method call <%s>."
                                                 oname sname);
                      end
                      else
                      begin
                        if not (List.mem AT_dyn at.at_flags) then
                          array_change_dyn at;
                        let sel''= get_instr1 sel' in
                        match at.at_objs.(0) with
                        | OT_object ao -> 
                          ([OD_sel_obj sel''],OT_array at),ao;
                        | OT_queue qu ->
                        begin
                          let ao = qu.qu_ao in
                          ([OD_sel_obj sel''],OT_array at),ao;
                        end;
                        | OT_channel ch ->
                        begin
                          let ao = ch.ch_ao in
                          ([OD_sel_obj sel''],OT_array at),ao;
                        end;
                        | _ -> error 0 
                                (sprintf "Not an abstract object array <%s> in method call <%s>."
                                                 oname sname);

                      end;
                    end;
                    | T_ident _ -> 
                        (*
                        ** Must be a Core method applied to an array!
                        *)
                        ([],OT_object at.at_ao),at.at_ao;
                    | _ -> error 0 (sprintf "\nInvalid abstract object <%s> in method call <%s> found."
                                               oname sname);
                  end;
                  | OT_queue qu ->
                  begin
                    let ao = qu.qu_ao in
                    ([],OT_object ao),ao;
                  end;
                  | OT_channel ch ->
                  begin
                    let ao = ch.ch_ao in
                    ([],OT_object ao),ao;
                  end;
                  | _ -> error 0 (sprintf "Unknown abstract object <%s> in method call <%s> found." oname sname);
                  in
                let (opl,ot),ao = get_ot oname in
                let args = List.map (fun arg -> get_instr1 arg) fargs in
                let num_args =
                  let n = List.length args in
                  if n = 0 || n > 1 then n 
                  else
                  begin
                    (*
                    ** Check for unity operator (== 0 args).
                    *)
                    match List.hd args with
                    | PI_obj ([],(OT_value V_null)) -> 0;
                    | _ -> 1;
                  end in
                if not (
                    if ao.ao_type.ta_rules.rl_methods = [] then
                      error 0 (sprintf "Empty method set found for object <%s>." ao.ao_name);
                    let found,sargs = ref false,ref [] in
                    List.iter (fun (sname',sargs') ->
                        if sname = sname' then
                        begin
                            found := true; sargs := sargs';
                        end;
                        )ao.ao_type.ta_rules.rl_methods;
                    if !found then
                    begin
                        let na = List.length !sargs in
                        let ni = List.length args in
                        if na <> ni then
                            error 0 (sprintf "\nExpected %d arguments in method call <%s.%s>, but got %d!"
                                             na oname sname ni);
                    end;
                    !found
                    ) 
                then
                  error 0 (sprintf "\nunknown object method <%s.%s>" oname sname);

                (*
                ** Perform some expression transformations of arguments here.
                *)
                
                let pil = 
                    expand_expr an.a_pro [PI_fun (src,
                            (opl,ot),
                            sname,
                            args)] in
                let pi = List.find (fun pi -> match pi with PI_fun _ -> true| _ -> false) pil in
                ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                                   an.a_pro
                                                   pi an.a_toplevel;
                if (List.length pil) > 1 then
                begin
                    let instrl' = pil in
                    let bf = create_frame instrl' in
                    bf.bf_params <- bf.bf_params @ [BP_locked];
                    [PI_block (instrl',bf)]
                end
                else
                    pil
            | _ when compiler.t_ml || compiler.t_C ->
            begin
                (*
                ** Non object related procedure or function (fret <> T_empty).
                *)
                let name str = T_ident (pos_of_source an.a_curpos,str) in
                let fname = get_name fname in
                let src = source () in
                let is_fun = fret <> T_empty in
                let ftype = if is_fun then "function" else "procedure" in
                let get_fun () =
                    if an.a_pro = None then
                      error 0 (sprintf
                               "User defined %s <%s> called outside process."
                               ftype fname);
                    let pro = get_some an.a_pro in
                    let pro_syms = get_some an.a_pro_syms in
                    let pro_import = get_some an.a_pro_import in
                    if sym_check_fun pro_syms fname then
                      sym_get_fun pro_syms fname                               
                    else if sym_check_fun pro_import fname then
                      sym_get_fun pro_import fname                               
                    else if sym_check_fun an.a_modu.mod_objs fname then
                    begin
                      let f = sym_get_fun an.a_modu.mod_objs fname in
                      (*
                      ** For the first time:
                      ** Import function and add function objects to process
                      ** object list.
                      *)
                      sym_add pro_import (Sym_fun f);
                      let syml = Cp_symbol.list_of_sym f.fun_objs in
                      List.iter (fun sym ->
                            sym_add pro_syms (sym_copy pro sym);
                        ) syml;
                      f
                    end
                    else error 0 (sprintf 
                                   "\nunknown %s <%s>"
                                    ftype fname);
                  in                                                       
                let f = get_fun () in
                if f.fun_inline then
                begin
                  let retl = 
                    if not is_fun then []
                    else match fret with
                    | T_objlist tl -> List.map (fun fret -> get_instr1 fret) tl
                    | _ -> [get_instr1 fret] in

                  let args = 
                      retl @(List.map (fun arg ->
                       get_instr1 arg; 
                      ) fargs) in
                  let nfargs = (List.length f.fun_args)+(List.length f.fun_ret) in
                  let nargs = List.length args in
                  let nfretl = List.length f.fun_ret in
                  let nretl = List.length retl in
                  if nfargs <> nargs then
                    error 0 (sprintf "Expected %d argument(s), got %d in function call <%s>."
                                     nfargs nargs fname);
                  if nfretl <> nretl then
                    error 0 (sprintf "Expected %d return parameters(s), got %d in function call <%s>."
                                     nfretl nretl fname);
                  let oname = sprintf "function_%s" f.fun_name in
                  let lookup_ot oname =
                    if sym_check_obj an.a_modu.mod_objs oname then
                      sym_get_obj an.a_modu.mod_objs oname
                    else if an.a_pro <> None && 
                            sym_check_obj (get_some an.a_pro).pro_objs oname then
                      sym_get_obj (get_some an.a_pro).pro_objs oname 
                    else 
                      error 0 (sprintf 
                        "No abstract object <%s> found." 
                        oname) in
                  let ot = lookup_ot oname in
                  let sname = "call" in
                  let opl = [] in
                  [PI_fun (src,
                              (opl,ot),
                              sname,
                              args)]  
              end
              else
              begin
                  let nfargs = List.length f.fun_args_obj in
                  let nargs = List.length fargs in
                  let fretl =
                    match fret with
                    | T_objlist tl -> tl;
                    | T_empty -> [];
                    | _ -> [fret] in
                  let nretl = List.length fretl in
                  let nfretl = List.length f.fun_ret in
                  if nfargs <> nargs then
                    error 0 (sprintf "Expected %d argument(s), got %d in function call <%s>."
                                     nfargs nargs fname);
                  if nfretl <> nretl then
                    error 0 (sprintf "Expected %d return parameters(s), got %d in function call <%s>."
                                     nfretl nretl fname);
                  let name str = T_ident (pos_of_source an.a_curpos,str) in
                  let il = ref [] in
                  let lock =
                    T_Fun (T_selector (name (sprintf "LOCK_FUN_%s" f.fun_name),
                                       name "lock"),[],T_empty) in
                    
                  il := !il @ (get_instr lock);
                  let assigns = 
                    List.map2 (fun co varg ->
                      T_assign (name co.co_name,varg)                      
                      ) f.fun_args_obj fargs in 
                  List.iter (fun i -> il := !il @ (get_instr i)) assigns;
                  let call =
                    T_Fun (T_selector (name (sprintf "FUN_%s" f.fun_name),
                                       name "call"),[],T_empty) in
                  il := !il @ (get_instr call);
                  il := !il @ (List.map2 (fun fret co -> get_instr1 (T_assign (fret,name co.co_name))) fretl f.fun_ret_obj);
                   
                  let unlock =
                    T_Fun (T_selector (name (sprintf "LOCK_FUN_%s" f.fun_name),
                                       name "unlock"),[],T_empty) in
                    
                  il := !il @ (get_instr unlock);
                  let bf = create_frame !il in
                  bf.bf_name <- "FUN_CALL";
                  [PI_block (!il,bf)];
                
              end;  
            end;
            | _ ->
            begin
                (*
                ** Non object related procedure or function (fret <> T_empty).
                *)
                let fname = get_name fname in
                let src = source () in
                let is_fun = fret <> T_empty in
                let ftype = if is_fun then "function" else "procedure" in
                let get_fun () =
                    if an.a_pro = None then
                      error 0 (sprintf
                               "User defined %s <%s> called outside process."
                               ftype fname);
                    let pro = get_some an.a_pro in
                    let pro_syms = get_some an.a_pro_syms in
                    let pro_import = get_some an.a_pro_import in
                    if sym_check_fun pro_syms fname then
                      sym_get_fun pro_syms fname                               
                    else if sym_check_fun pro_import fname then
                      sym_get_fun pro_import fname                               
                    else if sym_check_fun an.a_modu.mod_objs fname then
                    begin
                      let f = sym_get_fun an.a_modu.mod_objs fname in
                      (*
                      ** For the first time:
                      ** Import function and add function objects to process
                      ** object list.
                      *)
                      sym_add pro_import (Sym_fun f);
                      let syml = Cp_symbol.list_of_sym f.fun_objs in
                      List.iter (fun sym ->
                            sym_add pro_syms (sym_copy pro sym);
                        ) syml;
                      f
                    end
                    else error 0 (sprintf 
                                   "\nunknown %s <%s>"
                                    ftype fname);
                  in                                                       
                let f = get_fun () in

                if f.fun_inline && not compiler.t_ml && not compiler.t_C then
                begin
                  let old_subst = an.a_pro_subst in
                  an.a_fun_subst <- [];
                  (*
                  ** Map procedure arguments to parameters
                  *)
                  let retl = 
                      if not is_fun then []
                      else match fret with
                      | T_objlist tl -> List.map (fun fret -> get_name fret,fret) tl
                      | _ -> [get_name fret,fret] in
                      
                  let fargs'' = (List.map (fun arg ->
                        (get_name arg),arg
                    ) fargs) @ retl in
                  let n = List.length fargs'' in
                  let n' = (List.length f.fun_args)+(List.length f.fun_ret) in
                  let nretl = List.length retl in
                  let nfretl = List.length f.fun_ret in
                  if n <> n' then
                    error 0 (sprintf "Expected %d %s arguments, but got %d."  
                                     n' ftype n);
                  if nfretl <> nretl then
                    error 0 (sprintf "Expected %d return parameters(s), got %d in function call <%s>."
                                     nfretl nretl fname);
                  (*
                  ** Build symbol name substitution list
                  *)
                  an.a_fun_subst <- List.map2
                    (fun (arg_name,arg_syntax) param ->
                        debug "get_instr" with (sprintf "%s <%s> call: <%s> -> <%s>"
                                         ftype fname arg_name param);
                        param,arg_name,arg_syntax
                    ) fargs'' 
                      (f.fun_args@f.fun_ret);

                  an.a_pro_subst <- an.a_pro_subst @ (List.map (fun sym ->
                        (*
                        ** Substitute symbol names (preceeded with
                        ** the function name), too.
                        *)
                        let n = (String.length fname) + 1 in
                        let sname' = sym_name sym in
                        let len = String.length sname' in
                        let sname =  String.sub sname' n (len-n)  in
                        debug "get_instr" with (sprintf "<%s> -> <%s>"
                                         sname sname');
                        sname,sname'
                    ) (list_of_sym f.fun_objs) );
                  an.a_funpos <- an.a_curpos;                
                  let il = ref [] in
                  let rec iter syn =
                    match syn with
                    | T_block (syn,_) -> iter syn;
                    | T_list li ->
                        List.iter (fun instr -> 
                            il := !il @ 
                                    (get_instr instr)) li;
                    | _ -> error 0 (sprintf
                            "\nMissing instruction list in %s <%s>."
                            ftype fname);
                    in
                  let fun_instr' = fun_subst an.a_fun_subst f.fun_instr in
                  iter fun_instr';
                  an.a_funpos <- nilsrc ();
                  an.a_pro_subst <- old_subst;
                  an.a_fun_subst <- [];
                  !il
                end
                else
                begin
                  let nfargs = List.length f.fun_args_obj in
                  let nargs = List.length fargs in
                  let fretl =
                    match fret with
                    | T_objlist tl -> tl;
                    | T_empty -> [];
                    | _ -> [fret] in
                  let nretl = List.length fretl in
                  let nfretl = List.length f.fun_ret in
                  if nfargs <> nargs then
                    error 0 (sprintf "Expected %d argument(s), got %d in function call <%s>."
                                     nfargs nargs fname);
                  if nfretl <> nretl then
                    error 0 (sprintf "Expected %d return parameters(s), got %d in function call <%s>."
                                     nfretl nretl fname);
                  let name str = T_ident (pos_of_source an.a_curpos,str) in
                  let il = ref [] in
                  let lock =
                    T_Fun (T_selector (name (sprintf "LOCK_FUN_%s" f.fun_name),
                                       name "lock"),[],T_empty) in
                    
                  il := !il @ (get_instr lock);
                  let assigns = 
                    List.map2 (fun co varg ->
                      T_assign (name co.co_name,varg)                      
                      ) f.fun_args_obj fargs in 
                  List.iter (fun i -> il := !il @ (get_instr i)) assigns;
                  let call =
                    T_Fun (T_selector (name (sprintf "FUN_%s" f.fun_name),
                                       name "call"),[],T_empty) in
                  il := !il @ (get_instr call);
                  il := !il @ (List.map2 (fun fret co -> get_instr1 (T_assign (fret,name co.co_name))) fretl f.fun_ret_obj);
                   
                  let unlock =
                    T_Fun (T_selector (name (sprintf "LOCK_FUN_%s" f.fun_name),
                                       name "unlock"),[],T_empty) in
                    
                  il := !il @ (get_instr unlock);
                  let bf = create_frame !il in
                  bf.bf_name <- "FUN_CALL";
                  [PI_block (!il,bf)];
                end;
            end;
        end;    
        
        | T_Fun_def (fname,fret,fparams,finstr) ->
            []
            
        | T_sub (obj,sel) ->
        begin
            let is_sel =
                match obj with
                | T_selector _ -> true;
                | _ -> false in
            let a,b,selobj =
                match sel with
                | T_OP_arith _ ->
                begin
                    (*  
                    ** Maybe constant arithmetic expression.
                    *)
                    let pi = expr_fold (get_instr1 sel) in
                    match pi with
                    | PI_obj (_,OT_value (V_int i64)) ->
                        let x = Int64.to_int i64 in
                        x,x,OD_sub (x,x)
                    | PI_arithm _ ->
                        (*
                        ** Dynamic bit selector expression
                        *)
                        0,0,OD_index (get_instr1 sel);
                    | _ -> 
                        let a,b = get_sub sel in
                        a,b,OD_sub (a,b)                        
                end;
                | T_interval (va,vb) ->
                begin
                    (*  
                    ** Maybe constant arithmetic expression.
                    *)
                    let pia = expr_fold (get_instr1 va) in
                    let pib = expr_fold (get_instr1 vb) in
                    let a = 
                        match pia with
                        | PI_obj (_,OT_value (V_int i64)) ->
                            let x = Int64.to_int i64 in
                            x
                        | _ -> 
                            let a',_ = get_sub sel in
                            a' in

                    let b = 
                        match pib with
                        | PI_obj (_,OT_value (V_int i64)) ->
                            let x = Int64.to_int i64 in
                            x
                        | _ -> 
                            let _,b' = get_sub sel in
                            b' in
                    a,b,OD_sub (a,b)
                end;  
                | T_ident s when (ast_is_int sel) ->
                  let a,b = get_sub sel in
                  a,b,OD_sub (a,b)
                | T_ident _ -> 
                  (*
                  ** Dynamic bit selector
                  *)
                  0,0,OD_index (get_instr1 sel);
                | T_id -> 
                  let i = an.a_pro_num in
                  if compiler.t_C || compiler.t_ml then 
                    0,0,OD_index (PI_obj ([],OT_named_value ("#",V_int (Int64.of_int an.a_pro_num)))) 
                  else 
                    i,i,OD_sub (i,i);                 
                | _ -> error 0 (sprintf "Unexpected subrange or bit selector expression found.") in

            let name = get_name obj in
            let oobj = get_instr1 obj in
            if not is_sel then
            begin
                match dt_of_pi oobj with
                | DT_logic _ 
                | DT_int _ -> ();
                | _ -> error 0 "Subrange operation only allowed with logic or integer data type.";
            end;
            match oobj with
            | PI_obj (opl,ot) -> 
                (*
                ** Range check
                *)
                let ok =
                    match dt_of_ot ot with
                    | Some dt -> 
                        (size_of_dt dt) >= (b-a+1) &&
                        a >= 0 &&
                        b >= 0 &&
                        b < (size_of_dt dt)
                    | None ->
                        error 0 (sprintf
                                 "Range with object <%s> not supported!"
                                 name);
                    in
                if not ok then
                        error 0 (sprintf
                                 "\nUnexpected range in object <%s>!"
                                 name);
                    
                [PI_obj ([selobj]@opl,ot)];
            | _ -> error 629244 ""; 
        end;
        
        | T_ident _ -> 
            [get_obj_of_ident instr]
            
        | T_id ->
            [PI_obj ([],OT_named_value ("#",V_int (Int64.of_int an.a_pro_num)))];
            
        | T_typeconv (tt,vexpr) ->
        begin
            let oobj = get_instr1 vexpr in
            let new_type =
                match tt with
                | 'c' -> DT_char;
                | 'l' -> DT_logic 0;
                | 'i' -> DT_int 0;
                | 'b' -> DT_bool;
                | _ -> error 0 "Invalid target type.";
                in
            let obj =
                match oobj with
                | PI_obj (opl,ot) -> PI_obj (opl@[OD_conv new_type],ot);
                | _ -> error 0 "Invalid type conversion.";
                in

            [obj]
        end;
        
        | T_selector (obj,sel) ->
        begin
            let is_sel2,sel_array,sel_struct,name=
                (*
                ** Nested array and structure/component selectors ?
                *)
                match obj with
                | T_selector (obj',sel') -> 
                begin
                    let array_name = get_name obj' in
                    let obj_root = get_instr1 obj' in
                    match obj_root with
                    | PI_obj (_,OT_array at_root) ->
                    begin
                      match at_root.at_objs.(0) with
                      | OT_struct _ ->
                        let sel_name = get_name sel in
                        true,sel',sel,
                        sprintf "%s_%s" (get_name obj) sel_name
                      | OT_component _ ->
                        let sel_name = get_name sel' in
                        true,sel',sel,
                        sprintf "%s_%s" (get_name obj) sel_name                        
                      | _ -> error 15077 "";
                    end;
                    | _ -> error 0 (sprintf "\nunexpected selector object <%s>"
                                            array_name);
                end;
                | _ -> 
                    false,sel,sel,get_name obj  in
            (*
            ** array subrange (only 1-dim)?
            *)
            let sel_range =
                match sel with
                | T_interval _ -> [OD_sub (get_sub sel)];
                | _ -> [];
                in
                
            let sel_sub = 
                match sel with 
                | T_sub (_,subsel) -> 
                begin
                  match subsel with
                  | T_OP_arith _ ->
                  begin
                      (*  
                      ** Maybe constant arithmetic expression.
                      *)
                      let pi = expr_fold (get_instr1 subsel) in
                      match pi with
                      | PI_obj (_,OT_value (V_int i64)) ->
                          let x = Int64.to_int i64 in
                          [OD_sub (x,x)]
                      | _ -> 
                          let a,b = get_sub subsel in
                          [OD_sub (a,b)]                        
                  end;
                  | T_interval (va,vb) ->
                  begin
                      (*  
                      ** Maybe constant arithmetic expression.
                      *)
                      let pia = expr_fold (get_instr1 va) in
                      let pib = expr_fold (get_instr1 vb) in
                      let a = 
                          match pia with
                          | PI_obj (_,OT_value (V_int i64)) ->
                              let x = Int64.to_int i64 in
                              x
                          | _ -> 
                              let a',_ = get_sub subsel in
                              a' in
                      let b = 
                          match pib with
                          | PI_obj (_,OT_value (V_int i64)) ->
                              let x = Int64.to_int i64 in
                              x
                          | _ -> 
                              let _,b' = get_sub subsel in
                              b' in
                      [OD_sub (a,b)]
                  end;  
                  | T_ident s when (ast_is_int subsel) ->
                    let a,b = get_sub subsel in
                    [OD_sub (a,b)]
                  | T_ident _ -> 
                    (*
                    ** Dynamic bit selector
                    *)
                    [OD_index (get_instr1 subsel)];
                  | T_id -> 
                    let i = an.a_pro_num in
                    if compiler.t_C || compiler.t_ml then 
                      [OD_index (PI_obj ([],OT_named_value ("#",V_int (Int64.of_int an.a_pro_num))))]
                    else 
                      [OD_sub (i,i)];                 
                  | _ -> error 0 (sprintf "Unexpected array or structure selector expression found.") 
                end;
                | _ -> [] in
            
            let aobj = get_instr1 (T_ident (pos_of_source an.a_curpos,name)) in
            match aobj with
            | PI_obj (opl,ot) -> 
            begin
              match ot with
              | OT_array at ->
              if sel_range = [] then
              begin
                let size = proda at.at_dim in

                let sel_strl = get_const_names sel_array in
				debug "get_instr" with (let s = ref "" in List.iter (fun n -> s:= sprintf "%s<%s>" !s n) sel_strl; sprintf "sel_strl:[%s]" !s);
                let sel_str,sel_n=
                    List.hd sel_strl,
                    List.length sel_strl in
                let size = proda at.at_dim in

                let is_block = List.mem AT_block at.at_flags in
                let is_static = List.mem AT_static at.at_flags in
                if is_block then
                begin
                  let w = Array.create sel_n Int64.zero in
                  let is_num = protects (
                        let i = ref 0 in
                        List.iter (fun sel_str ->
                            w.(!i) <- Int64.of_string sel_str;
                            incr i) sel_strl) in
                  if not is_num then
                  begin
                      check_dyn at.at_objs.(0) at;
                      at.at_flags <- at.at_flags @ [AT_dyn];
                      (*
                      ** Import array, too!
                      *)
                      
(**************
                      if not (sym_check_obj (get_some an.a_pro_import) at.at_name)
                        then sym_add (get_some an.a_pro_import) (Sym_obj ot);
****************)
                      let selobj = expr_fold (get_instr1 sel_array) in
                      [PI_obj ([OD_sel_obj selobj]@
                                sel_sub,OT_array at)];
                  end
                  else
                  begin
                      let i = Array.map Int64.to_int w in
                      for n = 0 to sel_n-1
                      do
                        if i.(n) < 0 || i.(n) >= at.at_dim.(n) then
                              error 0 (sprintf "\nArray <%s, dim %d> accessed with out of bound index [expected 0..%d, got %d]."
                                               at.at_name (n+1) (at.at_dim.(n)-1) i.(n));
                      done;
                      [PI_obj ([OD_sel i]@sel_sub,OT_array at)];
                  end;
                end
                else
                begin
                  (*
                  ** Selector can be a static number or an expression.
                  *)
                  let w = Array.create sel_n Int64.zero in
                  let is_num = protects (
                        let i = ref 0 in
                        List.iter (fun sel_str ->
                            w.(!i) <- Int64.of_string sel_str;
                            incr i) sel_strl) in
                  if not is_num && is_static then
                    error 0 (sprintf "\nFound dynamic selector in array <%s>, but static is required!"
                                     at.at_name);
                  if is_num then
                  begin
                      let i = Array.map Int64.to_int w in
                      for n = 0 to sel_n-1
                      do
                        if i.(n) < 0 || i.(n) >= at.at_dim.(n) then
                              error 0 (sprintf "\nArray <%s, dim %d> accessed with out of bound index [expected 0..%d, got %d]."
                                               at.at_name (n+1) (at.at_dim.(n)-1) i.(n));
                      done;
                      [PI_obj ([OD_sel i]@sel_sub,OT_array at)];
                  end
                  else
                  begin
                      Array.iter (fun ot -> check_dyn ot at) at.at_objs;

                      info (sprintf 
                            "found dynamic selector in non block array <%s>" 
                            at.at_name);

                      if not (List.mem AT_dyn at.at_flags) then
                        array_change_dyn at;

                                    
                      (*
                      ** Import array, too!
                      *)
(**********
                      if not (sym_check_obj (get_some an.a_pro_import) at.at_name)
                        then sym_add (get_some an.a_pro_import) (Sym_obj ot);
**********)
                      let selobj = get_instr1 sel_array in
                      [PI_obj ([OD_sel_obj selobj]@
                                sel_sub,OT_array at)];
                  end;
                end;
              end
              else
              begin
                (*
                ** .... array.[a to b] ....
                *)
                [PI_obj (sel_range,OT_array at)];
              end;
              | OT_component st                
              | OT_struct st ->
              begin
                let sel= sprintf "%s_%s" st.st_name
                                         (get_name sel) in
                let ot = 
                  try
                    List.find (fun ot ->
                            let el_name = name_of_ot ot in
                            el_name = sel
                         ) st.st_objs 
                  with
                    | Not_found -> error 0 (sprintf "Unknown structure element found: %s." sel) in

                match ot with
                | OT_var _
                | OT_reg _
                | OT_signal _ ->
                    [PI_obj (sel_sub,ot)];
                | _ -> error 712866 "";
              end;

              | OT_signal co 
              | OT_reg co 
              | OT_var co ->
              begin
                match co.co_bittype with
                | Some tb ->
                    let sel_name = get_name sel in
                    let get_bitrange () =
                        let rec iter el =
                        match el with
                        | tr::tl ->
                            if tr.tr_name = sel_name then
                                tr.tr_range
                            else iter tl;
                        | [] ->
                            error 0 (sprintf 
                                     "\nUnknown Bittype selector <%s>"
                                     sel_name);
                                in
                        iter tb.tb_elems; in
                    let opl' =
                    opl @ [
                            OD_sub (get_bitrange ());
                        ] in
                    [PI_obj (opl',ot)];                
                | None -> error 0 (sprintf "Unexpected selector (neither structure nor bittype) for object <%s> found."
                                  name)
              end;
              | _ -> error 196728 "";
            end;
            | _ -> error 68257 ""; 
        end;
        
        | T_z -> [PI_obj([],OT_value V_z)];
        
        | T_block (vb,vpo) ->
        begin
          let optl = get_param_list (match vpo with Some vp -> vp | None -> T_empty) in
          let bind = get_param "bind" optl in
          let opl = to_param_list optl in
          match vb with
          | T_list il ->
            let pil = ref [] in
            List.iter (fun i -> pil := !pil @ (get_instr i)) il;
            let bf = create_frame !pil in
            bf.bf_params <- opl;
            let pi = PI_block (!pil,bf) in
            if bind = Some "true" then check_bind pi;
            [pi];
          | _ -> error 0 "\nBlock without instruction list.";
        end;
        
        | T_monitor (vexprl,bdebug) ->
          let pil = ref [] in
          List.iter (fun vexpr ->
            let ob_name = get_name_ext vexpr in
            debug "get_instr" with (sprintf "adding monitor for object <%s>" ob_name);
            let pro_mon ot name =
                if (sym_check an.a_modu.mod_objs name) then
                begin
                  let sym = sym_lookup an.a_modu.mod_objs name in
                  match sym with
                  | Sym_pro pro' ->
                    let sym' = Sym_obj ot in
                    sym_add pro'.pro_export (Sym_mon (bdebug,sym'));
                    sym_add pro'.pro_import (Sym_mon (bdebug,sym'));
                  | _ -> ();
                end;
                in

            if an.a_toplevel then
            begin
              (*
              ** Module toplevel monitor
              *)
              let mon_name = sprintf "MON_%s" ob_name in
              let rec search ob_name =
                if (sym_check an.a_modu.mod_objs ob_name) then
                begin
                  let sym = sym_lookup an.a_modu.mod_objs ob_name in
                  match sym with
                  | Sym_obj obj ->
                    let rec check obj = 
                        match obj with
                        | OT_signal _ 
                        | OT_reg _ 
                        | OT_channel _
                        | OT_queue _ -> ();
                        | OT_object ao ->
                            (*
                            ** Process ?
                            *)
                            let name = sprintf "PRO_%s" (name_of_ot obj) in
                            pro_mon obj name; 
                        | OT_array at -> 
                          let is_dyn = List.mem AT_dyn at.at_flags in
                          let is_block = List.mem AT_block at.at_flags in
                          if not is_block then
                            Array.iter (fun ot ->
                                (*
                                ** Process ?
                                *)
                                let name = sprintf "PRO_%s" (name_of_ot ot) in
                                pro_mon ot name;
                              ) at.at_objs;
                        | OT_struct _ -> ();
                        | _ -> error 0 (sprintf 
                                      "\nUnexpected object type in monitor <%s>."
                                      ob_name);
                        in
                    check obj;
                    sym_add an.a_modu.mod_objs (Sym_mon (bdebug,sym));
                    sym_add an.a_modu.mod_export (Sym_mon (bdebug,sym));
                    pil := !pil @ 
                        [PI_monitor (source (),(Sym_mon (bdebug,sym)),bdebug)];
                  | Sym_pro pro ->
                    (*
                    ** It's indeed a process instruction!
                    *)
                    pro.pro_instr <- pro.pro_instr @ 
                        [PI_monitor (source (),(Sym_mon (bdebug,sym)),bdebug)];
                    sym_add pro.pro_export (Sym_mon (bdebug,sym));
                    sym_add pro.pro_import (Sym_mon (bdebug,sym));
                    sym_add an.a_modu.mod_objs (Sym_mon (bdebug,sym));
                    sym_add an.a_modu.mod_export (Sym_mon (bdebug,sym));
                    pil := !pil @ [PI_nop];
                  | Sym_block db ->
                    sym_add an.a_modu.mod_objs (Sym_mon (bdebug,sym));
                    sym_add an.a_modu.mod_export (Sym_mon (bdebug,sym));
                    pil := !pil @ [PI_monitor (source (),(Sym_mon (bdebug,sym)),bdebug)];
                  | _ -> error 0 (sprintf
                                      "\nUnexpected symbol for monitor <%s>"
                                ob_name);
                end
                else
                    error 0 (sprintf "\nUnknown monitor symbol <%s>."
                                   ob_name);
              in
              search ob_name;
            end
            else
            begin
                (*
                ** Process monitor
                *)
                let mon_name = sprintf "MON_%s" ob_name in
                if (sym_check (get_some an.a_pro_syms) ob_name) then
                begin
                  let sym = sym_lookup (get_some an.a_pro_syms) ob_name in
                  match sym with
                  | Sym_obj obj ->   
                    let check () = 
                        match obj with
                        | OT_signal _ 
                        | OT_reg _ -> ();
                        | _ -> error 0 (sprintf 
                                      "\nUnexpected object type in monitor <%s>."
                                      ob_name);
                        in
                    check ();
                    sym_add (get_some an.a_pro_import) (Sym_mon (bdebug,sym));
                    sym_add (get_some an.a_pro_export) (Sym_mon (bdebug,sym));
                    sym_add an.a_modu.mod_objs (Sym_mon (bdebug,sym));
                    sym_add an.a_modu.mod_export (Sym_mon (bdebug,sym));
                    pil := !pil @ 
                        [PI_monitor (source (),(Sym_mon (bdebug,sym)),bdebug)];
                  | Sym_block db ->
                    sym_add (get_some an.a_pro_import) (Sym_mon (bdebug,sym));
                    sym_add (get_some an.a_pro_export) (Sym_mon (bdebug,sym));
                    sym_add an.a_modu.mod_objs (Sym_mon (bdebug,sym));
                    sym_add an.a_modu.mod_export (Sym_mon (bdebug,sym));
                    pil := !pil @
                        [PI_monitor (source (),(Sym_mon (bdebug,sym)),bdebug)];

                  | _ -> error 0 (sprintf
                                      "\nUnexpected symbol for monitor <%s>."
                                ob_name);
                
                end
                else
                    error 0 (sprintf "\nUnknown monitor symbol <%s> in process <%s>."
                                   ob_name an.a_pro_name);
              
            end;
          ) vexprl;
          !pil
        | T_domain (vexprl,(pos,dname)) ->
        begin
          List.iter (fun vexpr ->
            let objname = get_name vexpr in
            if (sym_check an.a_modu.mod_objs objname) then
            begin
                match sym_lookup an.a_modu.mod_objs objname with
                | Sym_pro pro -> pro.pro_domain <- dname;
                | Sym_obj ot ->
                begin
                  match co_of_ot ot with
                  | Some co -> co.co_domain <- dname;
                  | None -> error 0 (sprintf "\nObject <%s> can't belong to domain <%s>!"
                                             objname dname); 
                end;
                | Sym_block db -> db.db_domain <- dname;
                | _ -> error 0 (sprintf "\nObject <%s> can't belong to domain <%s>!"
                                        objname dname); 
            end;
            ) vexprl;
          [PI_nop]
        end;
        
        | T_empty -> [PI_nop];
        
        | T_typedef _ -> [PI_nop];
        
        (*
        ** Ignored here. Handled by get_obj.
        *)
        | T_OT_const _
        | T_OT_sig _
        | T_OT_reg _
        | T_OT_var _
        | T_OT_array _
        | T_OT_comp _
        | T_OT_object _
            -> [];
            
        | T_time (v,u) ->
            let ot = OT_value (V_time (Int64.of_string (str_of_val (get_const_val v)),u)) in
            [PI_obj ([],ot)]
            
        | T_freq (v,u) ->
            let ot = OT_value (V_freq (Int64.of_string (str_of_val (get_const_val v)),u)) in
            [PI_obj ([],ot)]
            
        | T_list isl ->
            [PI_list (List.map get_instr1 isl)];
        | T_module_def (vname,vblock) ->
        begin
          (*
          ** Defintion of architecture module
          *)
          if not an.a_toplevel then
            error 0 "\nUnexpected process instruction or object in statement found!";
          let mod_name' = get_name vname in
          out (sprintf "Analyzing architecture module <%s>..." mod_name');
          ind_incr ();
          let modu' = {
            mod_name = mod_name';
            mod_objs = Hashtbl.create 100;
            mod_import = Hashtbl.create 100;
            mod_export = Hashtbl.create 100;
            mod_procs = [];
            mod_external = [];
            mod_rules = [];
            mod_instr = [];
            mod_fmap = an.a_modu.mod_fmap;
            mod_flags = [Mod_main];
            mod_syntax = [vblock];
            } in
          sym_add top_syms (Sym_mod modu');
          let modu_top = an.a_modu in
          an.a_modu <- modu';
          (*
          ** Import all constant values
          *)
          Hashtbl.iter (fun name sym ->
            match sym with
            | Sym_obj (OT_named_value _)
            | Sym_obj (OT_const _ ) ->
              sym_add modu'.mod_import sym;
            | _ -> (); 
            ) modu_top.mod_objs;
        
          let rec iter instr =
            match instr with
            | T_topinstr instr' -> iter instr';
            | T_block (T_list vl,_) -> List.iter iter vl;
            | T_block (v,_) -> iter v;
            | T_import vnamel ->
              List.iter (fun vname ->
                let name = get_name vname in
                if sym_check_mod top_syms name then
                begin
                    let modu'' = sym_get_mod top_syms name in
                    out (sprintf "Importing module <%s>." name);
                    sym_add modu'.mod_import (Sym_mod modu'');
                    let s2p port =
                      match List.filter (fun s -> s <> "") 
                            (Str.split (Str.regexp "signal\|:\|;\|(\|)\| ") port) with
                      | [sname;sdir;stype] ->
                        {
                          te_name=sname;
                          te_type= (
                            match stype with
                            | "std_logic" -> DT_logic 1
                            | _ -> error 0 (sprintf "Unexpected port signal type <%s> found." stype);
                            ),1;
                          te_port=(
                            match sdir with
                            | "in" -> Some PT_in;
                            | "out" -> Some PT_out;
                            | "inout" -> Some PT_bus;
                            | _ -> error 0 (sprintf "Unexpected port signal direction <%s> found." sdir);
                            );
                        }
                      | [sname;sdir;stype;sa;stypedir;sb] ->
                        {
                          te_name=sname;
                          te_type= (
                            match stype with
                            | "std_logic_vector" -> 
                              let a = int_of_string sa in
                              let b = int_of_string sb in
                              if stypedir = "downto" then DT_logic (a-b+1) else DT_logic(b-a+1)
                            | _ -> error 0 (sprintf "Unexpected port signal type <%s> found." stype);
                            ),1;
                          te_port=(
                            match sdir with
                            | "in" -> Some PT_in;
                            | "out" -> Some PT_out;
                            | "inout" -> Some PT_bus;
                            | _ -> error 0 (sprintf "Unexpected port signal direction <%s> found." sdir);
                            );
                        }
                        
                      | _-> error 0 (sprintf "Unexpected port signal found: <%s>." port) in
                    let portl = List.flatten (
                                  List.map (fun sym ->
                                          let is_mon,is_debug,sym' = get_mon sym in
                                          match sym' with
                                          | Sym_obj obj ->
                                              let ports',tops'=(get_rules obj).rl_obj_port
                                                              sym modu'' None in
                                              List.map s2p ports';
                                          | Sym_block db ->
                                              let ports',tops'= (get_some !core_rules).rl_obj_port   
                                                              sym modu'' None in
                                              List.map s2p ports';
                                          | Sym_pro pro' ->
                                              if is_mon then
                                              begin
                                                  let states = get_state_names pro' in
                                                  let len = const_width (V_int (i64 (List.length states))) in
                                                  List.map s2p [
                                                    sprintf "signal MON_PRO_%s_state: out %s_vector(%d downto 0);"
                                                        pro'.pro_name (get_env_str "logic_type")
                                                        (len-1);
                                                    sprintf "signal MON_PRO_%s_enable: out %s;"
                                                        pro'.pro_name (get_env_str "logic_type")];
                                              end else []
                                          | _ -> [];
                                    ) (list_of_sym modu''.mod_export)) in
                    let modu_type'' = Type_struct {
                      ts_name = name;
                      ts_elems = portl;
                      } in
                    sym_add modu'.mod_objs (Sym_type modu_type'');
                end
                else
                  error 0 (sprintf "Trying to import unknown module <%s>." name);
                ) vnamel;
            | T_typedef (vt,T_list vl,vfl) -> 
              let tp_name = get_name vt in
              let obl = get_typedef tp_name vl in
              if obl <> [] then error 0 (sprintf "Unexpected type defintion found.");
            | T_OT_comp _ -> 
              let objs = get_obj instr in
              List.iter (fun obj -> sym_add modu'.mod_objs (Sym_obj obj)) objs;
            | T_map (lhs,rhs) ->
            begin
              (*
              ** LHS <=> RHS
              *)
              check_signal_map lhs rhs;
              modu'.mod_instr <- modu'.mod_instr @ [PI_map (source (),get_instr1 lhs,get_instr1 rhs)];
            end;
            | _ -> error 0 (sprintf "Unexpected statement in module defintition found."); in
          iter vblock;
          ind_decr ();    
          an.a_modu <- modu_top;      
          [PI_nop]
        end;
        | _ -> if not an.a_toplevel then
                error 0 "\nUnexpected process instruction or object in statement found!"
               else
                [PI_nop]
    in
    instrl := !instrl @ instrl';
  ) (fun_eval instr);
  !instrl

let get_instr instr =
    debug "get_instr" with (sprintf ">> get_instr: %s" (print_src an.a_curpos));
    let il = get_instr instr in
    List.iter (fun pi ->
      if debug_it "get_instr" then
      begin
        print_instr pi;
      end;
      ) il;
    debug "get_instr" with (sprintf "<< get_instr: %s" (print_src an.a_curpos));
    il 

(*
** Resolve read and write dependencies of objects used in
** instructions.
*)
let resolve_obj_dep pro =
        let pro_name =
            match an.a_pro_syms with
            | Some _ -> an.a_pro_name;
            | None -> pro.pro_name;
            in
        let pro_syms =
            match an.a_pro_syms with
            | Some syms -> syms;
            | None -> pro.pro_objs;
            in
        let pro_import =
            match an.a_pro_import with
            | Some syms -> syms;
            | None -> pro.pro_import;
            in
        let pro_export =
            match an.a_pro_export with
            | Some syms -> syms;
            | None -> pro.pro_export;
            in
        let modu =
            match an.a_pro_syms with
            | Some _ -> an.a_modu;
            | None -> pro.pro_module;
            in
            
        out (sprintf "Resolving object dependencies for process <%s>..."
                     pro.pro_name);
        let is_co ot =
            match ot with
            | OT_const _
            | OT_signal _
            | OT_reg _
            | OT_var  _
            | OT_channel _
            | OT_queue _
            | OT_struct _
            | OT_array _ 
            | OT_array_sel _ ->
                   true
            | _ -> false
            in
        let rec get_col opl ot =
            match ot with
            | OT_const co -> [co];
            | OT_signal co -> [co];
            | OT_reg co -> [co];
            | OT_var co -> [co];
            | OT_channel ch -> [ch.ch_obj];
            | OT_queue qu -> [qu.qu_obj];
            | OT_struct st -> 
                let col = ref [] in
                List.iter (fun ot -> 
                    col := !col @ (get_col [] ot);
                    ) st.st_objs;
                !col
            | OT_array at -> 
                let is_block =  List.mem AT_block at.at_flags in
                let is_dyn =  List.mem AT_dyn at.at_flags in
                if is_block then get_col [] at.at_objs.(0)
                else if is_sel_obj opl then
                begin
                    debug "resolve_obj_dep" with (sprintf 
                             "resolve_obj_dep: AT_dyn for array <%s> found" 
                             at.at_name);
                    let col = ref [] in
                    Array.iter (fun ot -> 
                        col := !col @ (get_col [] ot) 
                        ) at.at_objs;
                    !col
                end
                else
                begin
                    if is_sel opl then 
                    begin
                        let i = at_index at (obj_sel opl) in
                        if i >= (Array.length at.at_objs)  then 
                          error 0 (sprintf "Something wrong with array access <%s>." at.at_name);
                        get_col [] at.at_objs.(i);
                    end
                    else 
                    begin
                      let l = ref [] in
                      Array.iter (fun ot -> 
                          l := !l @ (get_col [] ot) 
                          ) at.at_objs;
                      !l
                    end;
                end;
            | _ -> error 674391 "";
            in


        let write_add opl ot =
            debug "resolve_obj_dep" with (sprintf 
                             "resolve_obj_dep: write_add <%s>" 
                             (name_of_ot ot));
            if is_co ot then
            begin
                match ot with
                | OT_signal co -> 
                  (*
                  ** Signal sanity check
                  *)
                  if not (List.mem pro co.co_writer) &&
                     co.co_writer <> [] then
                      error 0 (sprintf "\nFound singal <%s> with more than one write source."
                               co.co_name)
                  else if not (List.mem pro co.co_writer) then
                    co.co_writer <- co.co_writer @ [pro];
                | _ ->
                  let col = get_col opl ot in
                  List.iter (fun co ->
                    if not (List.mem pro co.co_writer) then
                        co.co_writer <- co.co_writer @ [pro];
                    ) col;
            end
            in
            
        let read_add opl ot =
            debug "resolve_obj_dep" with (sprintf 
                             "resolve_obj_dep: read_add <%s>" 
                             (name_of_ot ot));
            if is_co ot then
            begin
                let col = get_col opl ot in
                List.iter (fun co -> 
                    if not (List.mem pro co.co_reader) then
                        co.co_reader <- co.co_reader @ [pro];
                    ) col;
            end
            in
        (*
        ** Only LHS of assignment allowed!
        *)
        let write_dep instr =
            match instr with
            | PI_obj (opl,ot) ->
            begin
                write_add opl ot;
                match ot with
                | OT_reg _ -> 
                    let req_rd = ref false in
                    List.iter (fun op -> 
                        match op with
                        | OD_sub _ 
                        | OD_index _ -> req_rd := true;
                        | _ -> ()) opl;
                    if !req_rd then
                        read_add opl ot;
                | _ -> ();
            end;
            | _ -> error 0 (sprintf "\nresolve_obj_dep: unexpected instruction in process <%s>."
                                  pro.pro_name)
            in

        
        let rec read_dep instr =
            match instr with
            | PI_obj (opl,ot) ->
                read_add opl ot;
                if is_sel_obj opl then
                  read_dep (obj_sel_obj opl);
            | PI_arithm (_,i1,i2) ->
                read_dep i1;
                read_dep i2;
            | PI_bool (_,_,i1,i2) ->
                read_dep i1;
                read_dep i2;
            | PI_concat (i1,i2) ->
                read_dep i1;
                read_dep i2;
            | PI_waitfor (src,i,time,unit,i1,i2) ->
                line src;
                read_dep i;
                read_dep i1;
                read_dep i2;
            | PI_assign (src,lhs,rhs) ->
            begin
                line src;
                read_dep rhs;
                write_dep lhs;
                match lhs with
                | PI_obj (opl,ot) ->
                  if is_sel_obj opl then
                    read_dep (obj_sel_obj opl);     
                | _ -> ();           
            end;
            | PI_nop -> ();
            | PI_block (il,_) -> List.iter read_dep il;
            | PI_list il -> List.iter read_dep il;
            | _ -> 
                   error 0 (sprintf "\nresolve_obj_dep: unexpected instruction in process <%s>."
                                      pro.pro_name)
            in
        (*
        ** Import list update. Objects actually not appearing
        ** in the process list must be checked and added to the
        ** process import table.
        *)
        let import_obj opl ot =
(*************
            let ot =
                match ot with
                | OT_array at ->
                    let is_block = List.mem AT_block at.at_flags in
                    if is_sel opl && not is_block  then
                        OT_array_sel (at,obj_sel opl)
                    else ot;
                | _ -> ot in
*************)
            let ot_name = get_obj_name ot in
            let is_loc_array_with_ext_block,block =
                let loc = sym_check_obj pro_syms ot_name in
                if loc then
                begin
                  let obj = sym_get_obj pro_syms ot_name in
                  match obj with
                  | OT_array at -> 
                  begin
                    match at.at_objs.(0) with
                    | OT_var co ->
                    begin
                      match co.co_block with
                      | Some db ->
                        not (sym_check_block pro_syms db.db_name) &&
                        not (sym_check_block pro_import db.db_name),
                        co.co_block;
                      | None -> false,None;
                    end; 
                    | _ -> false,None;
                  end;
                  | _ -> false,None;
                end
                else false,None in
            let temp_names = List.map (fun co -> co.co_name) 
                                        !(an.a_pro_temps) in
            if ot_name <> "%V" && 
               not (sym_check_obj pro_syms ot_name) &&
               not (List.mem ot_name temp_names) then
            begin
                (*
                ** Must be imported...
                *)
                debug "resolve_obj_dep" with (sprintf "checking object <%s> in process <%s>..."
                                 ot_name pro_name);
                if not (sym_check_obj an.a_modu.mod_objs ot_name) &&
                     not (sym_check_obj_ext an.a_modu ot_name) then
                      error 0 (sprintf "\nUnknown object <%s> in process <%s> from module <%s>."
                                   ot_name pro_name an.a_mname); 
                if not (sym_check_obj pro_import ot_name) then
                begin
                      debug "resolve_obj_dep" with (sprintf "importing object <%s> in process <%s>..."
                                 ot_name pro_name);
                      sym_add pro_import (Sym_obj ot);
                end;
            end
            else if is_loc_array_with_ext_block then
            begin
              sym_add pro_import (Sym_block (get_some block));
            end;
            in

        let import_pro pro =
            let ob_name = pro.pro_name in
            if ob_name <> "%V" && 
               not (sym_check_pro pro_syms ob_name) then
            begin
                (*
                ** Must be imported...
                *)
                debug "resolve_obj_dep" with (sprintf "importing object <%s> in process <%s>..."
                                 ob_name pro_name);
                if not (sym_check_pro an.a_modu.mod_objs ob_name) &&
                   not (sym_check_pro_ext an.a_modu ob_name) then
                    error 0 (sprintf "\nUnknown object <%s> in process <%s> from module <%s>."
                                 ob_name pro_name an.a_mname); 
                if not (sym_check_obj pro_import ob_name) then
                    sym_add pro_import (Sym_pro pro);
            end;
            in

        let rec import instr =
            match instr with
            | PI_obj (opl,ot) ->
                import_obj opl ot;
                if is_sel_obj opl then
                  import (obj_sel_obj opl);
            | PI_arithm (_,i1,i2) ->
                import i1;
                import i2;
            | PI_bool (_,_,i1,i2) ->
                import i1;
                import i2;
            | PI_concat (i1,i2) ->
                import i1;
                import i2;
            | PI_branch (src,expr,s1,s2) ->
                import expr;
            | PI_waitfor (src,i,time,unit,i1,i2) ->
                line src;
                import i;
                import i1;
                import i2;
            | PI_assign (src,lhs,rhs) ->
                line src;
                import lhs;
                import rhs;
            | PI_nop -> ();
            | PI_list il -> List.iter import il;
            | PI_block (il,_) -> List.iter import il;
            | _ -> error 0 (sprintf "\nresolve_obj_dep: unexpected instruction in process <%s>."
                                      pro.pro_name);
            in

        let rec iter instr =
            match instr with
            | PI_assign (src,lhs,rhs) ->
            begin
                line src;
                write_dep lhs;
                read_dep rhs;
                import lhs;
                import rhs;
                match lhs with
                | PI_obj (opl,ot) ->
                  if is_sel_obj opl then
                    read_dep (obj_sel_obj opl);     
                | _ -> ();           
            end;
            | PI_map (src,lhs,rhs) ->
                line src;
                write_dep lhs;
                read_dep rhs;
                import lhs;
                import rhs;
            | PI_waitfor (src,i,time,unit,i1,i2) ->
                line src;
                read_dep i;
                import i;
                iter i1;
                iter i2;
            | PI_branch (src,expr,s1,s2) ->
                line src;
                read_dep expr;
                import expr;
                iter s1;
                iter s2;
            | PI_case (src,exprl,b) ->
                line src;
                List.iter (fun expr ->
                  match expr with
                  | PI_list li -> List.iter read_dep li;
                                  List.iter import li;
                  | _ ->
                      read_dep expr;
                      import expr; ) exprl;
                iter b;
            | PI_select (src,expr,cl) ->
                read_dep expr;
                import expr;
                iter cl;
            | PI_try (i1,i2) ->
                iter i1;
                iter i2;
            | PI_forloop (src,expr,dir,lim1,lim2,s) ->
                line src;
                read_dep expr;
                write_dep expr;
                import lim1;
                import lim2;
                read_dep lim1;
                read_dep lim2;
                iter s;
            | PI_loop (src,_,expr,s) ->
                line src;
                read_dep expr;
                import expr;
                iter s;
            | PI_fun (src,(opl,ot),sel,argl) ->
                line src;
                let ao = ao_of_ot opl ot in
                let is_array,is_dyn,is_core_ao =
                  match ot with
                  | OT_array at ->
                    true,List.mem AT_dyn at.at_flags,false;
                  | OT_object ao ->
                    false,false,
                    ao.ao_type.ta_rules.rl_name = "Core" &&
                    (
                        match ao.ao_obj with
                        | Some (OT_queue _ )
                        | Some (OT_channel _ ) -> false; (* !!! *)
                        | _ -> true;
                    );
                  | _ -> false,false,false; in
                    
                if is_sel_obj opl then
                begin
                  read_dep (obj_sel_obj opl);
                  import (obj_sel_obj opl);
                end;
                  
                if not is_core_ao then 
                begin
                  let (_,parl) = List.find (fun (sel',parl) -> sel=sel') ao.ao_type.ta_rules.rl_methods in
                  List.iter2 (fun arg par -> 
                    import arg;
                    match par.arg_type with
                    | Arg_lhs -> write_dep arg;
                    | Arg_rhs -> read_dep arg;
                    | Arg_lrhs -> write_dep arg; read_dep arg;
                    ) argl parl;
                  import_obj opl ot;
                end
                else
                begin
                  (*
                  ** Read and write dependencies of Core ao (array).
                  *)
                  match sel with
                  | "set"
                  | "copy"
                  | "copyn"
                  | "init" ->
                  begin
                    match ao.ao_array with
                    | at :: _ ->
                        write_dep (PI_obj ([],OT_array at));
                        import (PI_obj ([],OT_array at));
                    | [] -> ();
                  end;
                  | "unlock"
                  | "cmp_eq"
                  | "cmp_neq" 
                  | "cmpn_eq"
                  | "cmpn_neq" -> 
                  begin
                    match ao.ao_array with
                    | at :: _ ->
                        read_dep (PI_obj ([],OT_array at));
                        import (PI_obj ([],OT_array at));
                    | [] -> ();
                  end;
                  | "guard" -> (); 
                  | _ -> error 620279 "";
                  
                end;
                let rl = ao.ao_type.ta_rules in
                let rec arg_dep il al =
                  match il with
                  | ins::ins_tl ->
                  begin
                    match al with
                    | arg::arg_tl ->
                      begin
                        match arg.arg_type with
                        | Arg_lhs  -> write_dep ins
                        | Arg_rhs  -> read_dep ins
                        | Arg_lrhs  ->
                        begin
                          read_dep ins;
                          write_dep ins;
                        end;
                      end;
                      import ins;
                      arg_dep ins_tl arg_tl;
                    | [] -> error 0 (sprintf
                              "\ntoo few arguments in object method <%s.%s>"
                              ao.ao_name sel);
                  end;
                  | [] -> () in
                let rec find_args rml =
                  match rml with
                  | (sname,sargs)::tl ->
                    if sname = sel then sargs
                    else find_args tl;
                  | [] -> error 0 (sprintf "Found unexpected method selector <%s>."
                                          sel) in
                let ni = List.length argl in
                let parl= find_args rl.rl_methods in
                let na = List.length parl in
                if  ni <> na then
                    error 0 (sprintf "Expected %d arguments, but got %d!"
                             na ni);
                arg_dep argl parl; 
                (*
                ** Check and compile function call.
                *)
(*                List.iter (fun expr -> read_dep expr;import expr;) il; *)
                if not is_dyn && not (List.mem (sel,pro) ao.ao_procs) then
                    ao.ao_procs <- ao.ao_procs @ [sel,pro];
                if is_dyn then
                begin
                  match ot with
                  | OT_array at ->
                    (*
                    ** Apply to all objects
                    *)
                    Array.iter (fun ot' ->
                      match ot' with
                      | OT_object ao' ->
                        if not (List.mem (sel,pro) ao'.ao_procs) then
                          ao'.ao_procs <- ao'.ao_procs @ [sel,pro];
                      | OT_queue qu ->
                        let ao' = qu.qu_ao in
                        if not (List.mem (sel,pro) ao'.ao_procs) then
                          ao'.ao_procs <- ao'.ao_procs @ [sel,pro];                        
                      | OT_channel ch ->
                        let ao' = ch.ch_ao in
                        if not (List.mem (sel,pro) ao'.ao_procs) then
                          ao'.ao_procs <- ao'.ao_procs @ [sel,pro];                        
                      | _ -> error 206587 "";
                      ) at.at_objs;
                  | _ -> error 518797 "";
                end;
            | PI_block (il,bf) -> List.iter iter il;
            | _ -> ();
            in
        List.iter iter pro.pro_instr

