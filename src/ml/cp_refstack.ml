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
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     24.6.2007
**    $MODIFIED:    
**    $VERSION:     2.03
**
**    $INFO:
**  
**
**  Reference stack implementation for expression expansion and
**  substitution. For each data object (register,variable,...) exists one
**  (nested) reference stack. With the reference stack, registers handled as
**  values as much as possible, rather than memory objects. All assignments
**  to registers or variables are transformed into temporary expressions.
**  The assignment is queued with ALAP scheduling behaviour, that means that
**  an assigment is delayed as last as possible.
**
**    $ENDINFO
*)

open Cp_types
open Cp_utils
open Cp_data
open Cp_block_frame
open Cp_expr
open Hashtbl
open Printf
open Cp_common
open Cp_printtypes
open Cp_analysis

type rs_expr = {
  (*
  ** RHS of assignment
  *)
  mutable rse_instr: instr;
  mutable rse_src: source;
}
and rs_branch = {
  (*
  ** Branch expression
  *)
  mutable rsb_expr: instr;
  (*
  ** Different branches block stack
  *)
  mutable rsb_stacks: stack list;
  (*
  ** Current number of stacks in this branch
  *)
  mutable rsb_depth: int;

  (* 
  ** == Depth of nested branches
  *)
  rsb_id: int;
 
  (*
  ** All cases covered?
  *)
  rsb_compl : bool;
   
}
and rs_loop = {
  (*
  ** Loop expression; either counting loop index
  ** or conditional loop expression
  *)
  mutable rsl_expr: instr;
  (*
  ** Loop body stack
  *)
  mutable rsl_stack: stack;

  (* 
  ** == Depth of nested loops
  *)
  rsl_id: int;
 
}
and stack_element = 
  | RS_self of instr
  | RS_expr of rs_expr
  | RS_branch of rs_branch
  | RS_loop of rs_loop 
  | RS_ref of (int list)
and stack = {
  st_obj : object_type;
  st_params: object_params list;
  st_name : string;
  mutable st_depth: int;
  mutable st_stack : stack_element list;
  mutable st_time: int;
}
and stack_control =
  (*
  ** complete * depth * expr list * referenced objects list * modified objects list
  *)
  | ST_branch of (bool*int*instr list*string list*string list)
  (*
  ** depth * expr * referenced objects list * modified objects list
  *)
  | ST_loop of (int*instr*string list*string list)
 
(*
** There are three kinds of stack elements:
**  Expressions (RS_expr)
**  Self object (unknown value)
**  Control (RS_branch,RS_loop)
*)

type stack_element_kind =
  | SE_expr
  | SE_self
  | SE_ref
  | SE_control 
  
let reduced = ref 0

(*
** Evaluate and build the reference stack for a process, do 
** reference substitution in expressions.
** 
*)

let eval_pro_ref_stack pro =


  ind_incr ();
  let stacks = Hashtbl.create 100 in


  (*
  ** Schedule time keeps order of assignments.
  *)
  let sched_time = ref 0 in

  (*
  ** Keep track of nested control statements like branches
  ** and loops. 
  *)
  let control = ref [] in


  let rec obj_name opl ot =
    let name = ref (name_of_ot ot) in
    (*
    ** Subrange or array selectors extend the name! Here treated like
    ** different objects
    *) 
    List.iter (fun op ->
        match op with
        | OD_sub (a,b) -> 
            name := sprintf "%s_BITS_%d_%d" !name a b;
        | OD_sel selar ->
             name := sprintf "%s_ARSEL" !name;
            Array.iter (fun s -> 
                 name := sprintf "_%d" s) selar;
        | OD_sel_obj _ -> 
            name := sprintf "%s_ARSEL_DYN%d" !name (Random.int 10000000);
        | _ -> ();
        ) opl;
    !name in

  let self rso = RS_self (PI_obj (rso.st_params,rso.st_obj)) in

  let rec print_depth dl =
    match dl with
    | hd::[] -> sprintf "%d" (hd-1);
    | hd::tl -> sprintf "%d,%s" (hd-1) (print_depth tl);
    | [] -> "" in

  let rec print_expr expr =
    match expr with
    | PI_obj (opl,ot) ->
    begin
        match ot with
        | OT_reference (name,depthl) ->
            out (sprintf "REF<%s(%s)>" name (print_depth depthl));
        | _ -> 
            if is_const_expr expr then
            begin
                let i = get_const_expr expr in
                out (sprintf "VAL<%s>" (Int64.to_string i));
            end
            else
                out (sprintf "OBJ<%s>" (obj_name opl ot));
    end;
    | PI_arithm (op,op1,op2) ->
        out "ARITHM:";
        ind_incr ();
        out "OP1:";
        ind_incr ();
        print_expr op1;
        ind_decr ();
        out "OP2:";
        ind_incr ();
        print_expr op2;
        ind_decr ();
        ind_decr ();
    | PI_bool (kind,op,op1,op2) ->
        out "BOOL:";
        ind_incr ();
        out "OP1:";
        ind_incr ();
        print_expr op1;
        ind_decr ();
        out "OP2:";
        ind_incr ();
        print_expr op2;
        ind_decr ();
        ind_decr ();
    | _ -> () in

    let print_stack st =
        out (sprintf "STACK <%s,depth=%d,time=%d>:" 
                     st.st_name st.st_depth st.st_time);
        let rec print_stack st =
            List.iter (fun rso ->
              match rso with
              | RS_expr rse  ->
                 out "RS_EXPR:";
                 ind_incr ();
                 print_expr rse.rse_instr;
                 ind_decr ();
              | RS_self instr -> 
                 out "RS_SELF:";
                 ind_incr ();
                 print_expr instr;
                 ind_decr ();
              | RS_branch rsb -> 
                 out (sprintf "RS_BRANCH#%d:" rsb.rsb_id);
                 List.iter (fun st' ->
                     ind_incr ();
                     out "COND:";
                     ind_incr ();
                     print_stack st';
                     ind_decr ();
                     ind_decr ();
                  ) rsb.rsb_stacks;
              | RS_loop rsl -> 
                 out (sprintf "RS_LOOP#%d:" rsl.rsl_id);
                 ind_incr ();
                 print_stack rsl.rsl_stack;
                 ind_decr ();
              | RS_ref dl -> 
                 out "RS_REF:";
                 out (print_depth dl);
              ) st.st_stack;
            out ("--------------------------------------------------------");
          in
        print_stack st in

    let print_stack_el ste =
      match ste with
      | RS_expr rse  ->
         out "RS_EXPR:";
         ind_incr ();
         print_expr rse.rse_instr;
         ind_decr ();
      | RS_self instr -> 
         out "RS_SELF:";
         ind_incr ();
         print_expr instr;
         ind_decr ();
      | RS_branch rsb -> 
         out (sprintf "RS_BRANCH#%d:" rsb.rsb_id);
         List.iter (fun st' ->
             ind_incr ();
             out "COND:";
             ind_incr ();
             print_stack st';
             ind_decr ();
             ind_decr ();
          ) rsb.rsb_stacks;
      | RS_loop rsl -> 
         out (sprintf "RS_LOOP#%d:" rsl.rsl_id);
         ind_incr ();
         print_stack rsl.rsl_stack;
         ind_decr ();
      | RS_ref dl -> 
         out "RS_REF:";
         out (print_depth dl);
          in
 
  let print_stacks () =
      Hashtbl.iter (fun name st ->
          print_stack st;
        ) stacks;
    in


  (*
  ************************************************************************
  **                          Stack operations                           *
  ************************************************************************
  **
  ** Three kinds of stack elements must be distinguished:
  **    (C) Control stacks: RS_branch, RS_loop
  **    (E) Expressions: RS_expr,RS_ref
  **    (S) Self object RS_self
  *)

  let se_kind se =
    match se with
    | RS_expr _ -> SE_expr;
    | RS_self _ -> SE_self;
    | RS_ref _ -> SE_ref;
    | _ -> SE_control in


  (*
  ** Get referenced stack element. Perhaps nested stacks must be
  ** iterated...
  *)
  let rec get_stack_el sl dl =
    let sl' = List.rev sl in
    match dl with
    | [n] -> 
    begin
        (*
        ** Single stack element list reference
        *)
        let se = List.nth sl' (n-1) in
        se
    end;    
    | n ::tl ->
    begin
        (*
        ** Select actual stack element
        *)
        let se' = List.nth sl' (n-1) in
        match se' with
        | RS_branch rsb ->
        begin
          (*
          ** Select now branch stack...
          *)
          let stl' = List.rev rsb.rsb_stacks in 
          match tl with
          | sn :: tl' ->
            let st' = List.nth stl' (sn-1) in
            (*
            ** Select next stack element...
            *)
            get_stack_el st'.st_stack tl'; 
          | [] -> error 623749 ""; 
        end;
        | RS_loop rsl ->
            let st' = rsl.rsl_stack in
            get_stack_el st'.st_stack tl;
        | _ -> error 669805 "";
    end;
    | [] -> error 479356 "" in

  (*
  ** Get and relocate referenced stack element.
  *)
  let rec get_ref_stack_el st dl =
    let el = get_stack_el st dl in
    match el with
    | RS_ref dl' -> get_ref_stack_el st dl';
    | _ -> el in
    
  (*
  ** Push new stack element se on the top of actual stack,
  ** either root or actual control environment (RS_loop,RS_branch),
  ** in the case of nested control environments the
  ** deepest stack.
  *)
  let push root se =
    let rec iter st = 
      match st.st_stack with
      | (RS_self _) :: _
      | (RS_expr _) :: _ 
      | (RS_ref _ ) :: _ -> 
        st.st_stack <- se :: st.st_stack;
        st.st_depth <- st.st_depth + 1;
      | (RS_loop rsl) :: _ ->
        let st' = rsl.rsl_stack in
        iter st';
      | (RS_branch rsb) :: _ -> 
        let st' = List.hd rsb.rsb_stacks in  
        iter st';
      | [] -> 
        st.st_stack <- [se];
        st.st_depth <- 1; in
    iter root in

  (*
  ** Push new stack element to this stack st
  *)
  let push_this st se =
    st.st_stack <- se :: st.st_stack;
    st.st_depth <- st.st_depth + 1 in
    
  (*
  ** Remove and return stack element [E,S] from the top of actual stack,
  ** either root or actual control environment (RS_loop,RS_branch),
  ** in the case of nested control environments the
  ** deepest stack.
  *)
  let pop root  =
    let rec iter st =
      match st.st_stack with
      | (RS_self _) :: tl
      | (RS_expr _) :: tl ->
        let se = List.hd st.st_stack in
        st.st_stack <- tl;
        se
      | (RS_ref dl) :: tl -> 
        let se = get_stack_el root.st_stack dl in
        st.st_stack <- tl;
        se
      | (RS_loop rsl) :: _ ->
        let st' = rsl.rsl_stack in
        iter st';
      | (RS_branch rsb) :: _ -> 
        let st' = List.hd rsb.rsb_stacks in  
        iter st';
      | [] ->  error 0 "Pop: Empty reference stack" in
    iter root in

  (*
  ** Remove and return stack element from this stack.
  *)
  let pop_this st =
    match st.st_stack with
    | se :: tl ->
    st.st_stack <- tl;
    se
    | [] ->  error 0 "Pop_this: Empty reference stack"  in
      
  (*
  ** Get stack element [E,S] from the top of actual stack,
  ** either root or actual control environment (RS_loop,RS_branch),
  ** in the case of nested control environments the
  ** deepest stack.
  *)
  let get root =
    let rec iter st = 
      match st.st_stack with
      | (RS_self _) :: tl
      | (RS_expr _) :: tl ->
        List.hd st.st_stack
      | (RS_ref dl) :: _ -> get_stack_el root.st_stack dl;
      | (RS_loop rsl) :: _ ->
        let st' = rsl.rsl_stack in
        iter st';
      | (RS_branch rsb) :: _ -> 
        let st' = List.hd rsb.rsb_stacks in  
        iter st';
      | [] ->  error 0 "Get: Empty reference stack" in
    iter root in

  (*
  ** Get top element from this stack
  *)
  let get_this st =
    List.hd st.st_stack in
  (*
  ** Get previous stack element [E,S] from actual stack.
  *)
  let rec get_prev root =
    let rec iter st = 
      match st.st_stack with
      | _ :: ((RS_self _) :: tl)
      | _ :: ((RS_expr _) :: tl) ->
        List.hd (List.tl st.st_stack)
      | _ :: ((RS_ref dl ) :: tl) -> 
        get_stack_el root.st_stack dl;
      | (RS_loop rsl) :: _ ->
        let st' = rsl.rsl_stack in
        iter st';
      | (RS_branch rsb) :: _ -> 
        let st' = List.hd rsb.rsb_stacks in  
        iter st';
      | _ ->  error 0 "Get_prev: Not found." in
    iter root in
    
  let get_this_prev st =
    match st.st_stack with
    | _ :: (se :: tl) -> se;
    | _ -> error 0 "Get_this_prev: Not found." in
  
  (*
  ** Get actual stack.
  *)
  let get_stack root =
    let rec iter st = 
      match st.st_stack with
      | (RS_self _) :: tl
      | (RS_expr _) :: tl
      | (RS_ref _ ) :: tl -> 
        st
      | (RS_loop rsl) :: _ ->
        let st' = rsl.rsl_stack in
        iter st';
      | (RS_branch rsb) :: _ -> 
        let st' = List.hd rsb.rsb_stacks in  
        iter st';
      | [] ->  st in
    iter root in
    
  (*
  ** Get parent stack from actual stack. Without
  ** control environment it's the root stack itself, else.
  ** actual control stack.
  *)
  let get_parent_stack root =
    let rec iter st = 
      match st.st_stack with
      | (RS_self _) :: tl
      | (RS_expr _) :: tl
      | (RS_ref _ ) :: tl -> 
        st
      | (RS_loop rsl) :: _ ->
        let st' = rsl.rsl_stack in
        if se_kind (get_this st') = SE_control then
          iter st'
        else
          st
      | (RS_branch rsb) :: _ -> 
        let st' = List.hd rsb.rsb_stacks in  
        if se_kind (get_this st') = SE_control then
          iter st'
        else
          st
      | [] ->  st in
    iter root in
    
  (*
  ** Replace top element of actual stack.
  *)
  let replace root se =
    let st' = get_stack root in
    match st'.st_stack with
    | _ :: tl' ->
      st'.st_stack <- se :: tl';
    | [] -> 
      st'.st_stack <- [se];
      st'.st_depth <- 1; in

  (*
  ** Replace top element of parent stack of actual stack.
  *)
  let replace_parent root se =
    let st' = get_parent_stack root in
    match st'.st_stack with
    | _ :: tl' ->
      st'.st_stack <- se :: tl';
    | [] -> 
      st'.st_stack <- [se];
      st'.st_depth <- 1; in
 
  let replace_this st se =
    match st.st_stack with
    | _ :: tl' ->
      st.st_stack <- se :: tl';
    | [] -> 
      st.st_stack <- [se];
      st.st_depth <- 1; in
 
  (*
  ** Replace all RS_self stack elements with new se.
  *)
  let rec replace_self st se =
    let rec iter sl =
      match sl with
      | se' :: tl ->
      begin 
        match se' with
        | RS_self _ ->
          se :: (iter tl);
        | RS_expr _
        | RS_ref _  ->  
          se' :: (iter tl);
        | RS_loop rsl ->
          let st' = rsl.rsl_stack in
          replace_self st' se;
          se' :: (iter tl);
        | RS_branch rsb -> 
          List.iter (fun st' ->
              replace_self st' se
            ) rsb.rsb_stacks;
          se' :: (iter tl);
      end;
      | [] -> [] in          
    st.st_stack <- iter st.st_stack in  
    
       

  (*
  ** Get previous (last) expression BEFORE actual control stack, if any, 
  ** return depth list of reference path to previous expression
  ** starting with the root stack.
  *)
  let get_prev_expr root =
    let rec iter st dl expro =
      match get_this st with
      | RS_branch rsb->
      begin
        let st' = List.hd rsb.rsb_stacks in
        let top' = get_this st' in
        match get_this_prev st with
        | RS_expr prev ->
          if (se_kind top') = SE_control then
            iter st' (dl @ [st.st_depth;rsb.rsb_depth]) (Some (dl @ [st.st_depth-1],RS_expr prev))
          else
            Some (dl@[st.st_depth-1],RS_expr prev);
        | RS_ref prev ->
          if (se_kind top') = SE_control then
            iter st' (dl @ [st.st_depth;rsb.rsb_depth]) (Some (dl @ [st.st_depth-1],RS_ref prev))
          else
            Some (dl@[st.st_depth-1],RS_ref prev);
        | _ ->
          if (se_kind top') = SE_control then
            iter st' (dl @ [st.st_depth;rsb.rsb_depth]) expro
          else
            expro;
      end;
      | RS_loop rsl ->
      begin
        let st' = rsl.rsl_stack in
        let top' = get_this st' in
        match get_this_prev st with
        | RS_expr prev ->
          if (se_kind top') = SE_control then
            iter st' (dl @ [st.st_depth]) (Some (dl @ [st.st_depth-1],RS_expr prev))
          else
            Some (dl@[st.st_depth-1],RS_expr prev);
        | RS_ref prev ->
          if (se_kind top') = SE_control then
            iter st' (dl @ [st.st_depth]) (Some (dl @ [st.st_depth-1],RS_ref prev))
          else
            Some (dl@[st.st_depth-1],RS_ref prev);
        | _ ->
          if (se_kind top') = SE_control then
            iter st' (dl @ [st.st_depth]) expro
          else
            expro;
      end;
      | _  ->
      begin
        match get_this_prev st with
        | RS_expr prev ->
          Some (dl@[st.st_depth-1],RS_expr prev);
        | RS_ref prev ->
          Some (dl@[st.st_depth-1],RS_ref prev);
        | _ -> None
      end in
    iter root [] None in

  (*
  ** Get reference path to actual top stack element
  *)
  let rec get_path st =
    let rec iter st dl =
      match get_this st with
      | RS_self _
      | RS_expr _
      | RS_ref _ -> dl @ [st.st_depth];
      | RS_loop rsl ->
      begin
        let st' = rsl.rsl_stack in
        iter st' (dl @ [st.st_depth]);
      end;
      | RS_branch rsb->
      begin
        let st' = List.hd rsb.rsb_stacks in
        iter st' (dl @ [st.st_depth;rsb.rsb_depth]);
      end;
      in
    iter st [] in

  (*
  ** Create new branch element. Stack is initialized with
  ** stack element list sel.
  *)
  let new_branch con rso sel compl =
    let st = rso in                    
    let con_depth,con_exprl = con in
    let n = List.length con_exprl in
    debug "eval_pro_ref_stack" with  (sprintf "new_branch #c=%d compl=%b for <%s>..." n compl rso.st_name);
    let rec make_stacks n =
        let st =  {st_obj=rso.st_obj;
                   st_params=rso.st_params;
                   st_name=rso.st_name;
                   st_stack=sel;
                   st_depth=List.length sel;
                   st_time= 0} in
        if n > 1 then st :: (make_stacks (n-1)) else [st] in
    let rsb = {
          rsb_expr=List.hd con_exprl;
          rsb_stacks=make_stacks n;
          rsb_depth=n;
          rsb_id=con_depth;
          rsb_compl=compl;
          } in
    let st_top = List.hd rsb.rsb_stacks in
    RS_branch rsb in

  (*
  ** Create new loop element. Stack is initialized with
  ** stack element list sel.
  *)
  let new_loop con rso sel =                    
    debug "eval_pro_ref_stack" with (sprintf "new_loop for <%s>..." rso.st_name);
    let con_depth,con_exprl = con in
    let make_stack () =
        {st_obj=rso.st_obj;
         st_params=rso.st_params;
         st_name=rso.st_name;
         st_stack=sel;
         st_depth=List.length sel;
         st_time= 0} in
    let rsl = {
         rsl_expr=con_exprl;
         rsl_stack=make_stack ();
         rsl_id=con_depth;
         } in
    let st_top = rsl.rsl_stack in
    RS_loop rsl in
    
  (*
  ** Create a new control stack element.
  ** Because control environments can be nested,
  ** and actual top of rso is not complete (first occurence
  ** of object deep in control tree), create
  ** a nested stack element, too.
  ** Pushes stack element se onto new actual control stack.
  *)
  let new_control rso se =
    let st = rso in 
    let check_push_this st se =
      (*
      ** Don't add RS_self - is already on the top...
      *)
      match se with
      | RS_self _ -> ();
      | _ -> push_this st se in
       
    debug "eval_pro_ref_stack" with (sprintf "new_control for object <%s>..." rso.st_name);
      
    let rec iter dl cl st =
      match cl with
      | (ST_branch (compl,d,el,rl,ml)) :: tl_cl ->
      begin
        match get_this st with
        | RS_branch rsb ->
          let n = List.length el in
          let n' = List.length rsb.rsb_stacks in
          let st_0 = List.hd rsb.rsb_stacks in
          let se_0 = List.hd (List.rev st_0.st_stack) in
          let copy_st () = 
                  {st_0 with
                   st_depth=1;
                   st_stack=[se_0];
                   st_time=0} in
          for i = 1 to (n-n')
          do
            rsb.rsb_stacks <- (copy_st ()) :: rsb.rsb_stacks;
            rsb.rsb_depth <- rsb.rsb_depth + 1;
          done;
          
          let st' = List.hd rsb.rsb_stacks in
          iter (dl @ [st.st_depth;rsb.rsb_depth]) tl_cl st';
        | _ ->
          (*
          ** Initial top of stack is either RS_self or
          ** a previous expression reference.
          *)
          let se_0 = 
            let se = get_this st in
            match se with
            | RS_expr _ -> RS_ref (dl @ [st.st_depth]);
            | RS_ref _ -> se;
            | RS_self _ -> RS_ref (dl @ [st.st_depth]);
            | _ -> self rso in
          let se' = new_branch  (d,el) rso [se_0] compl in
          let st' =
            match se' with
            | RS_branch rsb' -> List.hd rsb'.rsb_stacks;
            | _ -> error 240396 "" in
          let se'' = iter (dl @ [st.st_depth+1;0]) tl_cl st' in
          check_push_this st' se'';
          se'
      end;
      | (ST_loop (d,el,rl,ml)) :: tl_cl ->
      begin
        match get_this st with
        | RS_loop rsl ->
          let st' = rsl.rsl_stack in
          iter (dl @ [st.st_depth]) tl_cl st';
        | _ ->
          (*
          ** In the case of a loop se_0 must be RS_self and
          ** can only be changed to RS_ref after the loop
          ** was examined.
          *)

          let se_0 = self rso in
          let se' = new_loop (d,el) rso [se_0] in
          let st' =
            match se' with
            | RS_loop rsl' -> rsl'.rsl_stack;
            | _ -> error 240397 "" in
          let se'' = iter (dl @ [st.st_depth+1]) tl_cl st' in
          check_push_this st' se'';
          se'
      end;
      | [] -> se in
    let se' = iter [] (List.rev !control) st in   
    if debug_it "eval_pro_ref_stack" then print_stack_el se';
    se' in

    
  (*
  ** LHS & RHS
  ** Return ONLY object iff
  **    1. it is a register or variable
  **    2. it is used only in a local context (one reader/writer process, readdep/writedep = [P])
  *)
  let get_obj instr = 
      match instr with
      | PI_obj (opl,ot) ->
      begin
        (*
        ** New object?
        *)
        let name = obj_name opl ot in
        let is_ref_obj = 
          match ot with
          | OT_reg co
          | OT_var co ->    (* cond. 1. *)
            co.co_writer=(match co.co_process with Some p -> [p]|None -> []) &&
            co.co_reader=(match co.co_process with Some p -> [p]|None -> []) (* cond. 2. *)
          | _ -> false in
        match co_of_ot ot with
        | Some co when is_ref_obj ->
          (*
          ** It's a Core object. Check for already existing reference
          ** stack...
          *)
            
          if not (Hashtbl.mem stacks name) then
          begin
            let st =  {st_obj=ot;
                 st_params=opl;
                 st_name=name;
                 st_stack=[RS_self instr];
                 st_depth=1;
                 st_time= 0} in
            debug "eval_pro_ref_stack" with (sprintf "get_obj: Created new stack for object <%s>..." name);
            Hashtbl.add stacks name st;
            Some st
          end
          else Some (Hashtbl.find stacks name);
        | _ -> None;
      end;
      | _ -> error 642047 "" in
 
  (*
  ** Return all object names found in expression
  *)
  let rec expr_names expr =
    match expr with
    | PI_obj (opl,ot) ->
      if not (is_value ot) then
        [obj_name opl ot]
      else
        [];
    | PI_arithm (op,op1,op2) ->
      (expr_names op1) @ (expr_names op2)
    | PI_bool (kind,op,op1,op2) ->
      (expr_names op1) @ (expr_names op2)
    | _ -> [] in
    
  let rec expr_lhs_names expr =
    match expr with
    | PI_obj (opl,ot) ->
      if not (is_value ot) then
        [obj_name opl ot]
      else
        [];
    | PI_arithm (op,op1,op2) ->
      (expr_names op1) @ (expr_names op2)
    | PI_bool (kind,op,op1,op2) ->
      (expr_names op1) @ (expr_names op2)
    | PI_assign (_,lhs,_) ->
      expr_lhs_names lhs;
    | _ -> [] in

  (*
  ** RHS
  *)
  let rec reference lhs rhs =
    let lhs_names =
      let nl = ref [] in
      List.iter (fun expr ->
        nl := !nl @ (expr_names expr);
        ) lhs;
      !nl in
      
    let add_ref name =
      (*
      ** Only add real references of RHS, discard self
      ** references of objects contained both in LHS and RHS.
      *)
      match !control with
      | ST_branch (compl,d,el,rl,ml) :: tl ->
        if not (List.mem name rl) &&
           not (List.mem name lhs_names) then
          control := (ST_branch (compl,d,el,name::rl,ml)) :: tl;
      | _ -> () in
    let expr = rhs in
    match expr with
    | PI_obj (opl,ot) ->
    begin
        let name = obj_name opl ot in
        match co_of_ot ot with
        | Some co ->
          (*
          ** It's a Core object. Check for already existing reference
          ** stack...
          *)
          if Hashtbl.mem stacks name then
          begin
            let rso = Hashtbl.find stacks name in

            let st = 
                (*
                ** Perhaps first RHS occurence of object in control
                ** statement, and no RS_branch on the top (required!).
                *)
                match !control with
                | (ST_branch (compl',depth',exprl,rl,ml))::_ ->
                begin 
                    match get_this (get_parent_stack rso) with
                    | (RS_branch rsb) when (rsb.rsb_id = depth') ->   
                        let n = List.length exprl in
                        let n' = List.length rsb.rsb_stacks in
                        let st_0 = List.hd rsb.rsb_stacks in
                        let se_0 = List.hd (List.rev st_0.st_stack) in
                        let copy_st () = 
                                {st_0 with
                                 st_depth=1;
                                 st_stack=[se_0];
                                 st_time=0} in
                        for i = 1 to (n-n')
                        do
                          rsb.rsb_stacks <- (copy_st ()) :: rsb.rsb_stacks;
                          rsb.rsb_depth <- rsb.rsb_depth + 1;
                        done;
                        rso
                    | _ -> 
                    begin
                      let ste = new_control rso (self rso) in
                      push rso ste;
                      rso
                    end;
                end;
                | (ST_loop (depth',exprl,rl,ml))::_ ->
                begin 
                    match get_this (get_parent_stack rso) with
                    | (RS_loop rsl) when (rsl.rsl_id = depth') ->   
                        rso
                    | _ -> 
                    begin
                      let ste = new_control rso (self rso) in
                      push rso ste;
                      rso
                    end;
                end;
                | [] -> rso
                in

            let rec get_ref st depthl =
              match get_this st with
              | RS_expr rse-> 
                PI_obj(opl,OT_reference (name,depthl@[st.st_depth]));
              | RS_branch rsb ->
              begin
                let st' = List.hd rsb.rsb_stacks in
                get_ref st' (depthl@[st.st_depth;rsb.rsb_depth]); 
              end;
              | RS_loop rsl  ->
              begin
                let st' = rsl.rsl_stack in
                get_ref st' (depthl@[st.st_depth]); 
              end;
              | RS_self _->
                PI_obj(opl,OT_reference (name,depthl@[st.st_depth]));
              | RS_ref dl ->
                PI_obj(opl,OT_reference (name,dl)) in


            let expr' = get_ref st [] in
            add_ref name;  

            if debug_it "eval_pro_ref_stack"  then
            begin
                match expr' with
                | PI_obj(opl,OT_reference (name,depthl)) ->
                   debug "eval_pro_ref_stack"  with
                         (sprintf "reference: RHS reference of <%s:%s>..." name
                                  (print_depth depthl));
                   print_stack st;
                | _ ->();
            end;
            expr'
          end
          else 
          begin
            (*
            ** Perhaps new data object on RHS found?
            *)
            let rso = get_obj expr in
            let expr' = 
              match rso with
              | Some rso ->
              begin
                debug "eval_pro_ref_stack" with (sprintf "reference: RHS reference of <%s:new>..." name);
                match !control with
                | (ST_branch (compl',depth',exprl,rl,ml))::_ ->
                  let ste = new_control rso (self rso) in
                  push rso ste;
                  expr;
                | (ST_loop (depth',exprl,rl,ml))::_ ->
                  let ste = new_control rso (self rso) in
                  push rso ste;
                  expr;
                | [] ->
                  expr;
                end;
                | None -> expr in
            add_ref name; 
            expr' 
          end;
        | None -> expr;
    end;
    | PI_arithm (op,op1,op2) ->
        PI_arithm(op,reference lhs op1,reference lhs op2);
    | PI_bool (kind,op,op1,op2) ->
        PI_bool(kind,op,reference lhs op1,reference lhs op2);
    | _ -> expr in

  (*
  ** Substitute stack references in expressions...
  *) 
  let rec relocate_expr expr =
    match expr with
    | PI_obj (opl,ot) ->
    begin
        match ot with
        | OT_reference (name,depthl) ->
        begin
            debug "eval_pro_ref_stack" with (sprintf "relocate_expr: %s[%s]..." name (print_depth depthl));
            let rso = Hashtbl.find stacks name in
            let se = get_stack_el rso.st_stack depthl in
            let rec get_expr se =
              match se with
              | RS_self expr' -> relocate_expr expr';
              | RS_expr rse -> relocate_expr rse.rse_instr;
              | RS_ref dl -> get_expr (get_stack_el rso.st_stack dl);
              | _ -> error 325551 "" in
            get_expr se;
        end;
        | _ -> expr;
    end;
    | PI_arithm (op,op1,op2) ->
        PI_arithm(op,relocate_expr op1,relocate_expr op2);
    | PI_bool (kind,op,op1,op2) ->
        PI_bool(kind,op,relocate_expr op1,relocate_expr op2);
    | _ -> expr in

  let relocate_expr expr =
    let expr' = relocate_expr expr in
    debug "eval_pro_ref_stack" with "relocate_expr: expr -> expr':";
    if debug_it "eval_pro_ref_stack" then
    begin
      print_instr expr;
      print_instr expr';  
    end;
    expr' in
    
  (*
  ** Check if object name (LHS) is contained in RHS of stack top expression
  *)
  let contained name root =
    match root.st_stack with
    | ste::_ -> 
    begin
      let rec expro se =
        match se with
        | RS_expr rse -> Some (relocate_expr rse.rse_instr);
        | RS_self expr -> Some expr;
        | RS_ref dl -> expro (get_stack_el root.st_stack dl);
        | _ -> None in
      match expro ste with
      | Some expr ->
        let rec iter expr =
          match expr with
          | PI_obj (opl,ot) ->
            let name' = obj_name opl ot in
            name' = name
          | PI_arithm (op,op1,op2) ->
            (iter op1) || (iter op2);
          | PI_bool (kind,op,op1,op2) ->
            (iter op1) || (iter op2);
          | _ -> false in
        iter expr;
      | None -> false;
    end;
    | [] -> false in

  (*
  ** Object was modified either if it contains
  ** an expression or/and RS_self is top of stack
  ** and stack <> [RS_self]!
  *)
  let is_modified st =
    let rec iter sl =
      match sl with
      | (RS_expr _) :: _ -> true;
      | _ :: tl -> iter tl;
      | [] -> false in
    match st.st_stack with
    | [RS_self _] -> false;
    | (RS_self _) :: _ -> true;
    | _ -> iter st.st_stack in

  (*
  ** Compare two RS_expr elements for
  ** equality.
  *)
  let is_equal st st' =
    let e1 =
      match get_this st with
      | RS_expr rse -> Some (pi_sprint_instr rse.rse_instr);
      | _ -> None in
    let e2 =
      match get_this st' with
      | RS_expr rse -> Some (pi_sprint_instr rse.rse_instr);
      | _ -> None in
    (e1 <> None && e2 <> None && e1 = e2) in
    


 
  let rec instr_names il =
    match il with
    | i :: tl ->
    begin
    (
      match i with
      | PI_assign (_,lhs,rhs) -> (expr_names lhs) @ (expr_names rhs);
      | PI_list il
      | PI_block (il,_) -> instr_names il
      | PI_forloop (_,e,_,l1,l2,b) -> (expr_names e) @ (expr_names l1) @ (expr_names l2) @ (instr_names [b]);
      | PI_loop (_,_,e,b) -> (expr_names e) @ (instr_names [b]);
      | PI_waitfor (_,e,_,_,b1,b2) -> (expr_names e) @ (instr_names [b1])  @ (instr_names [b2]);
      | PI_branch (_,e,b1,b2) -> (expr_names e) @ (instr_names [b1])  @ (instr_names [b2]);
      | PI_select (_,e,c) -> (expr_names e) @ (instr_names [c]); 
      | PI_case (_,il,b) -> instr_names (il@[b]);
      | PI_try (b,c) -> (instr_names [b]) @ (instr_names [c]); 
      | PI_fun (_,_,_,el) -> instr_names el; 
      | _ -> expr_names i;  
    ) @ (instr_names tl);
    end;
    | [] -> [] in     
  (*
  ** Return ordered list of all stack objects. Ordered with respect to
  ** their data depenencies to each other.
  *)
  let get_syms () =
    debug "eval_pro_ref_stack" with (sprintf "get_syms...");
      
    let l = ref [] in
    Hashtbl.iter (fun name sym ->
            l := !l @ [name,sym];
            ) stacks;
    let syms = 
        (*
        ** Apperas s on RHS of expressions from stack list li?
        *)
        let rec contained_in_rhs_list li s =
            match li with
            | s' :: tl ->
                let n1,rso1 = s in
                let n2,rso2 = s' in
                if contained n1 rso2 then true else
                    contained_in_rhs_list tl s;
            | [] -> false in
        (*
        ** Apperas RHS elements of s1 in LHS of stack list li?
        *)
        let rec contained_in_lhs_list li s =
            match li with
            | s' :: tl ->
                let n1,rso1 = s in
                let n2,rso2 = s' in
                if contained n2 rso1 then true else
                    contained_in_lhs_list tl s;
            | [] -> false in
        (*
        ** Add new element s1 in sorted list concerning two conditions:
        **
        **  LPRE @ [s1] @ LPOST
        **
        **  1. s1 not element on RHS in LPOST
        **  2. RHS of s1 contains no elements from LHS in LPRE
        *)

        let rec add lpre lpost s1 =
          let post = contained_in_rhs_list lpost s1 in  (* 1 *)
          let pre = contained_in_lhs_list lpre s1 in    (* 2 *)
          if not pre && not post then
            (*
            ** no conflicts found - the right place for new element
            *)
            lpre @ [s1] @ lpost     
          else if not pre && post then
          begin
            (*
            ** Move next element from post to pre list.
            *)
            let hd_post,tl_post=List.hd lpost,List.tl lpost in
            add (lpre@[hd_post]) tl_post s1
          end
          else
          begin
            print_stacks ();
            error 0 "Found cross dependency in reference stack?";
          end in
        let ls = ref [] in
        List.iter (fun s ->
            ls := add [] !ls s;
            ) !l;
        !ls in
    syms in
  
  (*
  *****************************************
  **            Stack FLUSH               *
  *****************************************
  *)
 
     
  (*
  ** Flush all (namel=[]) pending (previsouly delayed) data object assignments (real
  ** load statement) or only partial selected (namel <> []). 
  *)
  let flush_objs namel =
    debug "eval_pro_ref_stack" with (sprintf "flush_objs [%s]..."
                                (let str = ref "" in
                                 List.iter (fun name ->
                                    str := !str ^ (sprintf "<%s>" name);
                                        ) namel;
                                 !str));
                                
    let instrl = ref [] in

    (*
    ** Flush all pending expressions.  Append assignments in right order due
    ** to their data dependencies (resolved in get_syms).
    *)
    let syms = get_syms () in

    List.iter (fun (name,rso) ->
        debug "eval_pro_ref_stack" with (sprintf "flush_objs: %s testing obj=<%s>, stack is:" 
              (
                match !control with
                | (ST_branch (compl',depth',exprl,rl,ml)) :: _ -> 
                  sprintf "actual BRANCH#%d" depth'
                | (ST_loop (depth',expr,rl,ml)) :: _ -> 
                  sprintf "actual LOOP#%d" depth'
                | [] -> ""
              )
              rso.st_name);
        if debug_it "eval_pro_ref_stack" then print_stack rso;
        let flush_expr st =
          match get st with
          | RS_expr rse ->
            let expr = rse.rse_instr in (* must be relocated later *)
            debug "eval_pro_ref_stack" with (sprintf "flush_objs: flushing expression for object <%s>:"
                                        rso.st_name);
            if debug_it "eval_pro_ref_stack" then
              print_instr expr;
            decr reduced;
            instrl := !instrl @ [PI_assign (rse.rse_src,
                                            PI_obj(rso.st_params,
                                                       rso.st_obj),
                                            expr)];
            let self = RS_self (PI_obj (rso.st_params,rso.st_obj)) in
            push st self;
          | _ -> () in
        if namel = [] || (List.mem rso.st_name namel) then
          flush_expr rso;
        ) syms;
    !instrl in

  let flush_all () = 
    debug "eval_pro_ref_stack" with (sprintf "flush_all: ...");
    flush_objs [] in
 
  let rec flush_immed_check expr =
    (*
    ** Check it: top of stack must be flushed immediately?
    **
    **  1. Top: RS_expr and RS_expr contains special objects: queues and channels
    **  2. Top: RS_expr and RS_expr contains global data objects
    *)
    match expr with
    | PI_obj (opl,ot) ->
    begin
      match ot with
      | OT_queue _
      | OT_channel _ -> true;
      | OT_reg co
      | OT_var co ->    (* cond. 2. *)
        co.co_writer<>(match co.co_process with Some p -> [p]|None -> []) ||
        co.co_reader<>(match co.co_process with Some p -> [p]|None -> []) 
      | _ -> false;
    end;
    | PI_arithm (op,op1,op2) ->
      (flush_immed_check op1) || (flush_immed_check op2)
    | PI_bool (kind,op,op1,op2) ->
      (flush_immed_check op1) || (flush_immed_check op2)
    | PI_assign (_,_,rhs) ->
      flush_immed_check rhs;
    | _ -> false in
 
  (*
  ** Flush only pending (previsouly delayed) data object assignments 
  ** related with actual control head (loop).
  **
  **  There can be a flush
  **    1. before the actual control environment (prel)
  *+       due to write activity in body block and previously delayed expression,
  **    2. and/or at the end of the control body block (loop)
  **       due to write activity in body block. 
  **
  ** Returns: prel,loop,modl
  **    prel: PI_assign instruction list, preloop flush
  **    loop: PI_assign instruction list, loop body flush 
  **    modl: stack list of all modified but unflushed objects
  *)
  let flush_loop () =

    let lid =
      match !control with
      | (ST_loop (depth',expr,rl,ml)) :: _ -> 
            depth'
      | _ -> error 46909 "" in
    debug "eval_pro_ref_stack" with (sprintf "flush_loop: actual LOOP#%d..." lid);

    (*
    ** Pre loopblock instructions
    *)
    let prel = ref [] in
    (*
    ** Instruction list (appended to loop block).
    *)
    let loop = ref [] in

    (*
    ** All modified objects NOT flushed (T(x)=[RS_self]
    *)
    let modl = ref [] in

    let syms = get_syms () in


    let expr rso rse =
        let expr = rse.rse_instr in (* must be relocated later *)
        decr reduced;
        PI_assign (rse.rse_src,
                    PI_obj(rso.st_params,
                           rso.st_obj),
                    expr);
        in

    let expr_ref rso ndl =
        let expr = 
              (* must be relocated later *)
              PI_obj ([],OT_reference ndl) in
        decr reduced;
        PI_assign (nilsrc (),
                    PI_obj(rso.st_params,
                           rso.st_obj),
                    expr);
        in
                                                                                                                                   
    List.iter (fun (name,rso) ->
        if debug_it "eval_pro_ref_stack" then
        begin
            debug "eval_pro_ref_stack" with
                 (sprintf "  flush_loop: actual LOOP#%d, checking object <%s> with stack:"
                          lid rso.st_name);
            print_stack rso;
        end;
        let flush_expr st =
          (* st = root stack *)
          (*
          ** Flush expression on top of stack only if it was modified within
          ** the actual loop (lid), therefore actual  top stack must be
          ** a RS_loop#lid with rsl.st_stack <> [RS_self] element.
          *)
          let st' = get_parent_stack st in
          match get_this st' with
          | RS_loop rsl when (rsl.rsl_id = lid) ->
            let st'' = rsl.rsl_stack in

            let modified = is_modified st'' in

            let prel_flush =
              modified && 
              se_kind (get_this_prev st') = SE_expr in (* delayed expression *)

            let loop_flush =
              modified in

            if modified then
            begin
              debug "eval_pro_ref_stack" with (sprintf "  flush_loop: actual LOOP#%d, object <%s> was modified, flushing assignment for..."
                                          lid rso.st_name);
              if not prel_flush then modl := !modl @ [name,rso];
            end;
              
            if prel_flush then
            begin
              match get_this_prev st' with
              | RS_expr rse' ->
                if debug_it "eval_pro_ref_stack" then
                begin
                  debug "eval_pro_ref_stack" with
                        (sprintf "  flush_loop: actual LOOP#%d, preloop flush for object <%s> with expression:"
                                 lid rso.st_name);
                  print_expr rse'.rse_instr;
                end;
                prel := !prel @ [expr rso rse'];
              | RS_ref dl' ->
              begin
                  debug "eval_pro_ref_stack" with
                      (sprintf "  flush_loop: preloop flush for object <%s> with reference: REF%s"
                               rso.st_name (print_depth dl'));
                  match get_ref_stack_el rso.st_stack dl' with
                  | RS_self _ -> 
                    debug "eval_pro_ref_stack" with (sprintf 
                            "  flush_loop: actual LOOP#%d, <%s> preloop flush with reference ignored: REF[%s]=RS_self"
                            lid rso.st_name (print_depth dl'));
                  | _ ->
                    prel := !prel @ [expr_ref rso (rso.st_name,dl')];
              end;
              | _ -> ();              
            end;
            if loop_flush then
            begin
              match get_this st'' with
              | RS_expr rse''  -> 
                if debug_it "eval_pro_ref_stack" then
                begin
                  debug "eval_pro_ref_stack"  with
                       (sprintf "  flush_loop: actual LOOP#%d, loop flush for object <%s> with expression:"
                                lid rso.st_name);
                  print_expr rse''.rse_instr;
                end;
                loop := (expr rso rse'') :: !loop;
              | RS_ref dl'' -> 
              begin
                  debug "eval_pro_ref_stack" with
                      (sprintf "  flush_loop: actual LOOP#%d, loop flush for object <%s> with reference: REF%s"
                               lid rso.st_name (print_depth dl''));
                  match get_ref_stack_el rso.st_stack dl'' with
                  | RS_self _ -> 
                    debug "eval_pro_ref_stack" with (sprintf 
                            "  flush_loop: actual LOOP#%d, <%s> loop flush with reference ignored: REF[%s]=RS_self"
                            lid rso.st_name (print_depth dl''));
                  | _ ->
                    loop := (expr_ref rso (rso.st_name,dl'')) :: !loop;
              end;
              | _ -> ();              
            end;
            if not modified then
            begin
              (*
              ** This object was only referenced inside
              ** loop block. Replace RS_self inside loop with
              ** reference to last expression, if any.
              *) 
              let prev = get_prev_expr st in
              match prev with
              | Some (dl',expr') ->
              begin
                replace_self st' (RS_ref dl');
                debug "eval_pro_ref_stack" with (sprintf 
                                    "  flush_loop: actual LOOP#%d, object <%s> not modified, only referenced, replaced with RS_ref[%s],stack is now:" 
                                    lid rso.st_name (print_depth dl'));
                if debug_it "eval_pro_ref_stack" then print_stack st;
              end;    
              | _ -> ();
            end;            
          | _ -> () in
        flush_expr rso;
        ) syms;
    (*
    ** Finally, modify loop stack.
    ** 1. X was not modified inside loop, but referenced:
    **      -> convert loop stack [RS_self] to reference to prev_expr,
    **      -> push prev_expr to actual top of stack
    ** 2. X was modified inside loop:
    **      -> push RS_self to actual top of stack
    *)
    List.iter (fun (name,rso) ->
        let st = rso in
        let st' = get_parent_stack st in
        match  get_this st'  with
        | RS_loop rsl when (rsl.rsl_id = lid) -> 
        begin
          let st'' = rsl.rsl_stack in
          let modified = is_modified st'' in

          if not modified then
          begin
            match get_prev_expr st with
            | Some (dl',expr') ->
               (*
               ** RS_loop itself was already modified above.
               *)
               push_this st' expr';
            | _ -> push_this st' (self rso);
          end
          else
          begin
            push_this st' (self rso);
          end;
        end;
        if debug_it "eval_pro_ref_stack" then
        begin
          debug "eval_pro_ref_stack" with
                (sprintf "  flush_loop: actual LOOP#%d, modified stack for <%s>, stack is now:" 
                        lid rso.st_name);
          print_stack rso;
        end;
        | _ -> ();
        ) syms;
    
    !prel,!loop,!modl in



  (*
  ** Flush only pending (previsouly delayed) data object assignments (real
  ** load statement) related with actual control environment (branch).
  **
  ** Returns: prel,condl,modl
  **    prel: PI_assign instruction list, preconditional flush
  **    prel: PI_assign instruction list, postconditional flush
  **    condl: PI_assign instruction list list, inside conditional branch flush 
  **    modl: stack list of all modified but unflushed objects
  *)
  let flush_cond () =
    let bid,depth,rl,ml,rl_expr =
      match !control with
      | (ST_branch (compl',depth',exprl,rl,ml)) :: _ -> 
            depth',
            (List.length exprl),
            rl,ml,
            (expr_names (List.hd exprl));
      | _ -> error 46908 "" in

    debug "eval_pro_ref_stack" with (sprintf "flush_cond: actual BRANCH#%d, depth=%d ..." bid depth);
    (*
    ** Pre- and post conditional flush/move instructions
    *)
    let prel,postl = ref [], ref [] in
    (*
    ** Block flush instructions (appended at the end of each cond. block).
    *)
    let rec empty_lists n =
        if n > 1 then [] :: (empty_lists (n-1)) else [[]] in
    let condl = ref (empty_lists depth) in
    (*
    ** All modified objects NOT flushed (T(x)=[RS_self]
    *)
    let modl = ref [] in

    let syms = get_syms () in

    let expr rso rse =
        let expr = rse.rse_instr in (* must be relocated later *)
        decr reduced;
        PI_assign (rse.rse_src,
                    PI_obj(rso.st_params,
                           rso.st_obj),
                    expr);
        in

    let expr_ref rso ndl =
        let expr = 
              (* must be relocated later *)
              PI_obj ([],OT_reference ndl) in
        decr reduced;
        PI_assign (nilsrc (),
                    PI_obj(rso.st_params,
                           rso.st_obj),
                    expr);
        in

    List.iter (fun (name,rso) ->        
        if debug_it "eval_pro_ref_stack" then
        begin
            debug "eval_pro_ref_stack" with (sprintf "  flush_cond: actual BRANCH#%d, checking object <%s> with stack:"
                                         bid rso.st_name);
            print_stack rso;
        end;
        let flush_expr st =          
          (*
          ** Flush expression on top of stack only if it was modified within
          ** the actual branch (bid), therefore top of stack must be
          ** a RS_branch element.
          ** Distinguish two cases:
          **    
          **    0. The substack of each conditional block was intialized
          **       with a RS_self element!
          **
          **    1. The object was modified only in one of several
          **       possible conditional blocks. In this case, the
          **       object assigment must be flushed with the previous
          **       (if any) expression before the conditional branch,
          **       and at the end of the respective conditional blocks.
          **
          **    2. The object was modified in all possible conditional 
          **       blocks. Here the pre-flush is not necessary. The
          **       object is only flushed at the end of each conditional
          **       block. The bottom element of each substack must be
          **       changed to the previous stack element before the
          **       branch occurs.
          **
          **    3. The object was modified in all possible conditional 
          **       blocks with same expression. 
          **       The expression assignment is moved before the branch
          **       only one time iff not referenced in branch expression,
          **       not inside conditional block, and expression contains
          **       no objects modified in actual control context! (referenced1)
          **
          **    4. The object was modified in all possible conditional 
          **       blocks with same expression. 
          **       The expression assignment is moved after the branch
          **       only one time iff referenced in branch expression, but
          **       not inside conditional block, and expression contains
          **       no objects modified in actual control context! (referenced2)
          *)
          
          (*
          ** Flush expression on top of stack only if it was modified within
          ** the actual branch (bid), therefore actual  top stack must be
          ** a RS_branch#bid with rsb.rsb_stacks contains RS_expr element.
          *)
          let st' = get_parent_stack st in
          match get_this st' with
          | RS_branch rsb when (rsb.rsb_id = bid) ->
            let stl'' = rsb.rsb_stacks in
            let modified = ref 0 in
            let equal = ref 0 in
            let last = ref None in
            List.iter (fun st'' ->
                if is_modified st'' then incr modified;
                match !last with
                | Some st''' -> 
                  if is_equal st'' st''' then incr equal;
                  last := Some st'';
                | None ->
                  last := Some st'';
                  incr equal;
              ) stl'';
                
            let n' = List.length rsb.rsb_stacks in
            let complete = rsb.rsb_compl && depth = !modified in
            let equal = depth = !equal in
            let modified = !modified > 0 in
            
            let referenced1 = (List.mem rso.st_name rl) in
            let referenced2 = (List.mem rso.st_name rl_expr) in
            
                
            if depth > n' then
            begin
              (*
              ** Extend stack list. Initial top of stack is
              ** initial stack element of first conditional stack.
              *)
              let st_0 = List.hd rsb.rsb_stacks in
              let se_0 = List.hd (List.rev st_0.st_stack) in
              let copy_st () = 
                      {st_0 with
                       st_depth=1;
                       st_stack=[se_0];
                       st_time=0} in
              for i = 1 to (depth-n')
              do
                rsb.rsb_stacks <- (copy_st ()) :: rsb.rsb_stacks;
                rsb.rsb_depth <- rsb.rsb_depth + 1;  
              done;
            end;

            (*
            ** Preconditional move of expression common
            ** to all conditional and complete branches possible?
            *)
            let prec_move = 
              modified && complete && equal && not referenced1 && not referenced2 &&
              (
                (*
                ** preconditional move candidate may not be dependent on
                ** modified objects in actual control context
                *)
                match get_this (List.hd rsb.rsb_stacks) with
                | RS_expr rse' ->
                    let namel = expr_names (expr st rse') in
                    let dep = ref false in
                    List.iter (fun name ->
                      dep := !dep || (List.mem name ml)) namel;
                    not !dep
                | RS_ref dl' ->
                begin
                    match get_ref_stack_el rso.st_stack dl' with
                    | RS_self _ -> false
                    | _ ->
                      let namel = expr_names (expr_ref st (rso.st_name,dl')) in
                      let dep = ref false in
                      List.iter (fun name ->
                        dep := !dep || (List.mem name ml)) namel;
                      not !dep
                end;
                | _ -> false              
              ) in

            (*
            ** Postconditional move of expression common
            ** to all conditional and complete branches possible?
            *)
            let postc_move = 
              modified && complete && equal && not referenced1 && referenced2 &&
              (
                (*
                ** postconditional move candidate may not be dependent on
                ** modified objects in actual control context
                *)
                match get_this (List.hd rsb.rsb_stacks) with
                | RS_expr rse' ->
                    let namel = expr_names (expr st rse') in
                    let dep = ref false in
                    List.iter (fun name ->
                      dep := !dep || (List.mem name ml)) namel;
                    not !dep
                | RS_ref dl' ->
                begin
                    match get_ref_stack_el rso.st_stack dl' with
                    | RS_self _ -> false
                    | _ ->
                      let namel = expr_names (expr_ref st (rso.st_name,dl')) in
                      let dep = ref false in
                      List.iter (fun name ->
                        dep := !dep || (List.mem name ml)) namel;
                      not !dep
                end;
                | _ -> false              
              ) in

            (*
            ** Preconditional flush required ?
            *)
            let prec_flush =
              modified &&
              not complete &&
              se_kind (get_this_prev st') = SE_expr in (* delayed expression *)
              
            (*
            ** FLush inside each conditional required ?
            *)
            let cond_flush =
              modified &&
              (not prec_move) &&
              (not postc_move)  in

            debug "eval_pro_ref_stack" with (sprintf "flush_cond: actual BRANCH#%d, <%s>: modified=%b complete=%b equal=%b referenced1=%b referenced2=%b"
                                        bid rso.st_name modified complete equal referenced1 referenced2); 
            debug "eval_pro_ref_stack" with (sprintf "flush_cond: actual BRANCH#%d, <%s>: prec_move=%b postc_move=%b prec_flush=%b cond_flush=%b"
                                        bid rso.st_name prec_move postc_move prec_flush cond_flush); 
            
            if modified then
            begin
              debug "eval_pro_ref_stack" with (sprintf "  flush_cond: actual BRANCH#%d, object <%s> was modified, checking flush..."
                                          bid rso.st_name);
              modl := !modl @ [name,rso];
            end;
            
            (*
            ** PRE conditional flush required? 
            *)
            if prec_flush then
            begin
              match get_this_prev st' with
              | RS_expr rse' ->
                  if debug_it "eval_pro_ref_stack" then
                  begin
                    debug "eval_pro_ref_stack" with (sprintf "  flush_cond: actual BRANCH#%d, <%s> preconditional flush with expression:"
                                                bid rso.st_name);
                    print_expr rse'.rse_instr;
                  end;
                  prel := !prel @ [expr st rse'];
              | RS_ref dl' ->
              begin
                  debug "eval_pro_ref_stack" with (sprintf 
                          "  flush_cond: actual BRANCH#%d, <%s> preconditional flush with reference: REF[%s]"
                          bid rso.st_name (print_depth dl'));
                  match get_ref_stack_el rso.st_stack dl' with
                  | RS_self _ -> 
                    debug "eval_pro_ref_stack" with (sprintf 
                            "  flush_cond: actual BRANCH#%d, <%s> preconditional flush with reference ignored: REF[%s]=RS_self"
                            bid rso.st_name (print_depth dl'));
                  | _ ->
                    prel := !prel @ [expr_ref st (rso.st_name,dl')];
              end;
              | _ -> ();
            end;
            
            if cond_flush then
            begin
              (*
              ** Now for each conditional block (stack)...
              *)
              let condl_i = ref [] in
              List.iter (fun rso' ->
                let cn = depth - (List.length !condl_i) - 1 in
                match get_this rso' with
                | RS_expr rse'  -> 
                    if debug_it "eval_pro_ref_stack" then
                    begin
                      debug "eval_pro_ref_stack" with  
                            (sprintf "  flush_cond: actual BRANCH#%d, <%s> conditional #%d flush with expression:"
                                     bid rso.st_name cn);
                      print_expr rse'.rse_instr;
                    end;
                    condl_i := [expr rso' rse'] :: !condl_i;
                | RS_ref dl' when ((List.length rso'.st_stack) > 1) -> 
                begin
                    (*
                    ** Really required?
                    *)
                    debug "eval_pro_ref_stack"  with (sprintf 
                            "  flush_cond: actual BRANCH#%d, <%s> conditional #%d flush with reference: REF[%s]"
                            bid rso.st_name cn (print_depth dl'));
                  match get_ref_stack_el rso.st_stack dl' with
                  | RS_self _ -> 
                    debug "eval_pro_ref_stack" with (sprintf 
                            "  flush_cond: actual BRANCH#%d, <%s> conditional flush with reference ignored: REF[%s]=RS_self"
                            bid rso.st_name (print_depth dl'));
                    condl_i := [] :: !condl_i;
                  | _ ->
                    condl_i := [expr_ref rso' (rso'.st_name,dl')] :: !condl_i;
                end;
                | _ ->
                    condl_i := [] :: !condl_i;
                ) rsb.rsb_stacks;
              condl := 
                List.map2 (fun cl ci->
                        cl @ ci ) !condl !condl_i;
            end;
            
            if prec_move or postc_move then
            begin
              (*
              ** Can we move complete and allconditional equal expression if any before branch?
              *)
              match get_this (List.hd rsb.rsb_stacks) with
              | RS_expr rse' ->
                  if debug_it "eval_pro_ref_stack" then
                  begin
                    debug "eval_pro_ref_stack" with (sprintf "  flush_cond: actual BRANCH#%d, <%s> %sconditional flush with complete expression:"
                                                bid rso.st_name (if prec_move then "pre" else "post"));
                    print_expr rse'.rse_instr;
                  end;
                  if prec_move then prel := !prel @ [expr st rse']
                               else postl := !postl @ [expr st rse'];
              | RS_ref dl' ->
              begin
                  debug "eval_pro_ref_stack"  with (sprintf 
                          "  flush_cond: actual BRANCH#%d, <%s> %sconditional flush with complete reference: REF[%s]"
                          bid rso.st_name (if prec_move then "pre" else "post") (print_depth dl'));
                  match get_ref_stack_el rso.st_stack dl' with
                  | RS_self _ -> 
                    debug "eval_pro_ref_stack" with (sprintf 
                            "  flush_cond: actual BRANCH#%d, <%s> %sconditional flush with complete reference ignored: REF[%s]=RS_self"
                            bid rso.st_name (if prec_move then "pre" else "post") (print_depth dl'));
                  | _ ->
                    if prec_move then prel := !prel @ [expr_ref st (rso.st_name,dl')]
                                 else postl := !postl @ [expr_ref st (rso.st_name,dl')];
              end;
              | _ -> ();              
            end;
            
          | _ -> () in
        flush_expr rso;
        ) syms;
    (*
    ** Finally, modify stack.
    ** 1. X was not modified inside any branch, but referenced:
    **      -> push prev_expr to actual top of stack
    ** 2. X was modified inside at least one branch:
    **      -> push RS_self to actual top of stack
    *)
    List.iter (fun (name,rso) ->
        let st = rso in
        let st' = get_parent_stack st in
        match  get_this st'  with
        | RS_branch rsb when (rsb.rsb_id = bid) -> 
        begin
          let stl'' = rsb.rsb_stacks in
          let modified = ref false in
          List.iter (fun st'' ->
              modified := !modified || (is_modified st'');
            ) stl'';
          if not !modified then
          begin
            match get_prev_expr st with
            | Some (dl',expr') ->
                (*
                ** RS_branch itself was already modified above.
                *)
                push_this st' (RS_ref dl');
            | _ -> push_this st' (self rso);
          end
          else
          begin
            push_this st' (self rso);
          end;
        end;
        if debug_it "eval_pro_ref_stack" then
        begin
          debug "eval_pro_ref_stack" with (sprintf "  flush_cond: actual BRANCH#%d, stack for <%s> is now:" 
                                      bid rso.st_name);
          print_stack rso;
        end;
        | _ -> ();
        ) syms;
    
    !prel,!condl,!modl,!postl in




       
  (*
  ** Replace OT_reference objects in expressions with the actual
  ** expression in reference stack.
  *)
  let rec relocate_instr pi =
    match pi with
    | PI_assign (src,lhs,rhs) ->
        line src;
        PI_assign (src,lhs,relocate_expr rhs);
    | PI_waitfor (src,expr,n,tu,i1,i2) ->
        line src;
        PI_waitfor (src,relocate_instr expr,
                        n,tu,
                        relocate_instr i1,
                        relocate_instr i2);
    | PI_branch (src,expr,i1,i2) ->
        line src;
        PI_branch (src,relocate_expr expr,
                       relocate_instr i1,
                       relocate_instr i2)
    | PI_select (src,expr,cl) ->
        line src;
        PI_select (src,relocate_expr expr,
                       relocate_instr cl)
    | PI_case (src,exprl,i) ->
        line src;
        PI_case (src,List.map relocate_expr exprl,
                     relocate_instr i);
    | PI_try (b,cl) ->
        PI_try (relocate_instr b,
                relocate_instr cl)
    | PI_fun (src,(opl,ot),sel,argl) ->
        line src;
        PI_fun (src,(opl,ot),sel,
                    List.map relocate_expr argl);
    | PI_loop (src,kind,expr,i) ->
        line src;
        PI_loop (src,kind,
                     relocate_expr expr,
                     relocate_instr i)
    | PI_forloop (src,expr,dir,l1,l2,i) ->
        line src;
        PI_forloop (src,relocate_expr expr,
                        dir,
                        relocate_expr l1,
                        relocate_expr l2,
                        relocate_instr i)
    | PI_block (il,bf) ->
        PI_block (List.map relocate_instr il,bf);
    | PI_list il ->
        PI_list (List.map relocate_instr il);
    | _ -> pi in

  (*
  ** Objects [modl] were modified (in branches or loops), but were not pre flushed
  ** because there were no delayed expressions, T(x)=[RS_self;...].
  ** But other objects referencing these objects in delayed expressions
  ** must be flushed before the [modl] objects are!
  *)
  let flush_expr modl =
    (*
    ** Get previous control context if any -
    ** the parent stack of actual control stack
    *)
    let prev_bid,prev_lid = 
      match !control with
      | _ :: (ST_branch (compl',depth',exprl,rl,ml)) :: _ -> 
        Some (depth',List.length exprl), None
      | _ :: (ST_loop (depth',expr,rl,ml)) :: _ -> 
        None, Some (depth',1)
      | _ -> None,None in
      
    debug "eval_pro_ref_stack" with (sprintf "flush_expr: %s"
        (
          match !control with
          | (ST_branch (compl',depth',exprl,rl,ml)) :: _ -> 
            sprintf "actual BRANCH#%d" depth'
          | (ST_loop (depth',expr,rl,ml)) :: _ -> 
            sprintf "actual LOOP#%d" depth'
          | [] -> ""
        )
      );
    let syms = get_syms () in
    let prel = ref [] in
    let expr rso rse =
        let expr = rse.rse_instr in (* must be relocated later *)
        decr reduced;
        PI_assign (rse.rse_src,
                    PI_obj(rso.st_params,
                           rso.st_obj),
                    expr);
        in

    let expr_ref rso ndl =
        let expr = 
              (* must be relocated later *)
              PI_obj ([],OT_reference ndl) in
        decr reduced;
        PI_assign (nilsrc (),
                    PI_obj(rso.st_params,
                           rso.st_obj),
                    expr);
        in
    List.iter (fun (name,rso) ->
      debug "eval_pro_ref_stack" with (sprintf "  modified: <%s>..."  name);
      List.iter (fun (name',rso') ->
        let st' = rso' in
        let top' = get st' in
        let rec check se' =
          match se' with
          | RS_expr rse' ->
          begin
            (*
            ** Actually delayed expression. Check for ocuurence 
            ** of rso in rso'
            *)
            let expr' = relocate_expr rse'.rse_instr in
            let names_in_expr' = expr_names expr' in
            if (List.mem name names_in_expr') then
            begin
              (*
              ** Must be flushed iff rso and rso' have SAME parent stack (with same depth).
              *)
              let p' = get_parent_stack rso' in
              let flush_it =
                match get_this p' with
                | RS_branch rsb ->
                begin
                  match prev_bid with
                  | Some (bid,depth) -> bid = rsb.rsb_id &&  depth = rsb.rsb_depth
                  | None -> false 
                end;
                | RS_loop rsl ->
                begin
                  match prev_lid with
                  | Some (lid,_) -> lid = rsl.rsl_id
                  | None -> false
                end;
                | _ -> 
                begin
                  prev_bid = None && prev_lid = None
                end  in
                
              if flush_it then
              begin
                prel := !prel @ [expr rso' rse'];
                (*
                ** Mark it flushed...
                *)
                push st' (self rso');
                debug "eval_pro_ref_stack" with (sprintf "  flush_expr: object <%s> was flushed due to delayed expression with object <%s> dep."
                                    name' name);
              end
              else
                debug "eval_pro_ref_stack" with (sprintf "  flush_expr: object <%s> still delayed though <%s> dep. exist"
                                    name' name);
                
            end;
          end;
          | RS_ref dl' ->
          begin
            let se'' = get_stack_el rso'.st_stack dl' in
            check se''
          end;
          | _ -> () in
        check top';
        ) syms;
      ) modl;
    !prel in

  let _flush_expr expr flushed_list =    
    (*
    ** If there are referenced objects inside expression flushed in [prel] due to branch dependency, 
    ** flush expression related object, too.
    *)
    let names_in_expr = expr_names expr in
    let expr' = reference [] expr in (* must be relocated later *)
    let names_in_expr' = expr_names (relocate_expr expr') in
    debug "eval_pro_ref_stack" with (sprintf "PI_branch: names_in_expr=[%d] names_in_expr'=[%d]" 
                         (List.length names_in_expr) (List.length names_in_expr'));
    let flush_names = ref [] in

    List.iter (fun flushed ->
      match expr_lhs_names flushed with
      | flushed_obj_name :: _ -> 
        if (List.mem flushed_obj_name names_in_expr') then
        begin
          let list_index e l =
            let rec iter l n =
              match l with
              | hd::tl -> if hd = e then n else iter tl (n+1);
              | [] -> error  683477 "" in
            iter l 0 in

          let n = list_index flushed_obj_name names_in_expr' in
          if not (List.mem (List.nth names_in_expr n) !flush_names) then
               flush_names := !flush_names @ [(List.nth names_in_expr n)];
               (*
               ** Reverse relocation again, the original object must be flushed, not the referenced!
               *)
        end;
      | _ -> ();
      ) flushed_list;
    let expr',flushed_list' = if !flush_names <> [] then expr,(flush_objs !flush_names)
                                           else expr',[] in
    expr',flushed_list' in

  (*
  ** Evaluate list of process instructions...
  *)
  let in_sched = ref [] in
  let rec iter instrl =
    match instrl with
    | instr::tl ->
    begin
      match instr with
      | PI_assign (src,lhs,rhs) when (!in_sched <> [])->
      begin
        line src;
        debug "eval_pro_ref_stack" with (sprintf "instr=PI_assign(%s)..." (print_src src));
        incr sched_time;

        let obj = get_obj lhs in

        match obj with
        | Some rso when not (flush_immed_check rhs) ->
        begin
            let st = rso in
            let st' = get_parent_stack st in
            
            rso.st_time <- !sched_time;
            match !control with
            | (ST_branch (compl,con_depth,con_exprl,rl,ml))::_ ->
            begin
              match get_this st' with
              | RS_branch rsb ->
              begin
                if rsb.rsb_id = con_depth then
                begin
                  (*
                  ** Actual branch.
                  *)
                  let n = List.length con_exprl in

                  if rsb.rsb_stacks = [] then
                    error 351128 "";
                                
                  let st_0 = List.hd rsb.rsb_stacks in
                  let se_0 = List.hd (List.rev st_0.st_stack) in

                  let n' = List.length rsb.rsb_stacks in
                  (*
                  ** Conditional block changed? 
                  *)
                  if n > n' then
                  begin
                    (*
                    ** Extend stack list.
                    *)
                    let copy_st () = 
                      {(List.hd rsb.rsb_stacks) with
                       st_depth=1;
                       st_stack=[se_0];
                       st_time=0} in
                    for i = 1 to (n-n')
                    do
                      rsb.rsb_stacks <- (copy_st ()) :: rsb.rsb_stacks;
                      rsb.rsb_depth <- rsb.rsb_depth + 1;
                    done;
                  end;
                  let rhs'= reference [lhs] rhs in
                  let stexpr = RS_expr {rse_instr=rhs';
                                        rse_src=src;} in
                  push st stexpr;
                  incr reduced;
                  iter tl;
                end
                else
                begin
                  (*
                  ** New (nested) branch found.
                  ** Delay assignment. First occurence of data object
                  ** in LHS of assignment.
                  *)
                  let rhs'= reference [lhs] rhs in
                  let stexpr = RS_expr {rse_instr=rhs';
                                        rse_src=src;} in
                  let ste = new_control rso stexpr in
                  push st ste;
                  incr reduced;
                  iter tl;
                end;
              end;
              | RS_loop rsl ->
              begin
                (*
                ** New (nested) branch found.
                ** Delay assignment. First occurence of data object
                ** in LHS of assignment.
                *)
                let rhs'= reference [lhs] rhs in
                let stexpr = RS_expr {rse_instr=rhs';
                                      rse_src=src;} in
                let ste = new_control rso stexpr in
                push st ste;
                incr reduced;
                iter tl;
              end;
              | _ ->
              begin
                    (*
                    ** New branch found.
                    ** Delay assignment. First occurence of data object
                    ** in LHS of assignment.
                    *)
                  let rhs'= reference [lhs] rhs in
                  let stexpr = RS_expr {rse_instr=rhs';
                                        rse_src=src;} in
                  let ste = new_control rso stexpr in
                  push st ste;
                  incr reduced;
                  iter tl;
              end;
            end;
            | (ST_loop (con_depth,con_exprl,rl,ml))::_ ->
            begin
              match get_this st' with
              | RS_loop rsl ->
              begin
                if rsl.rsl_id = con_depth then
                begin
                  (*
                  ** Actual loop.
                  *)
                  let rhs'= reference [lhs] rhs in
                  let stexpr = RS_expr {rse_instr=rhs';
                                        rse_src=src;} in
                  push st stexpr;
                  incr reduced;
                  iter tl;
                end
                else
                begin
                  (*
                  ** New (nested) loop found.
                  ** Delay assignment. First occurence of data object
                  ** in LHS of assignment.
                  *)
                  let rhs'= reference [lhs] rhs in
                  let stexpr = RS_expr {rse_instr=rhs';
                                        rse_src=src;} in
                  let ste = new_control rso stexpr in
                  push st ste;
                  incr reduced;
                  iter tl;
                end;
              end;
              | RS_branch rsb ->
              begin
                (*
                ** New (nested) branch found.
                ** Delay assignment. First occurence of data object
                ** in LHS of assignment.
                *)
                let rhs'= reference [lhs] rhs in
                let stexpr = RS_expr {rse_instr=rhs';
                                      rse_src=src;} in
                let ste = new_control rso stexpr in
                push st ste;
                incr reduced;
                iter tl;
              end;
              | _ ->
              begin
                    (*
                    ** New loop found.
                    ** Delay assignment. First occurence of data object
                    ** in LHS of assignment.
                    *)
                  let rhs'= reference [lhs] rhs in
                  let stexpr = RS_expr {rse_instr=rhs';
                                        rse_src=src;} in
                  let ste = new_control rso stexpr in
                  push st ste;
                  incr reduced;
                  iter tl;
              end;
            end;
            | [] ->
            begin
              match get_this st' with
              | RS_self _ ->
                (*
                ** Delay assignment. First occurence of data object
                ** in LHS of assignment.
                *)
                let rhs'= reference [lhs] rhs in
                let stexpr = RS_expr {rse_instr=rhs';
                                      rse_src=src;} in
                push st stexpr;
                incr reduced;
                iter tl;
              | RS_expr _ -> 
                (*
                ** Delay assignment.
                *)
                let rhs'= reference [lhs] rhs in
                let stexpr = RS_expr {rse_instr=rhs';
                                      rse_src=src;} in
                push st stexpr;
                incr reduced;
                iter tl;
              | _ -> error 839538 "";
            end;
        end;
        | _ -> 
            let rhs'= reference [lhs] rhs in
            let instr' = PI_assign (src,lhs,rhs') in
            instr' :: (iter tl);
      end;
      
      | PI_branch (src,expr,b1,b2) when (!in_sched <> [])->
      begin
        line src;
        debug "eval_pro_ref_stack" with (sprintf "instr=PI_branch(%s)..." (print_src src));

        let old_control = !control in
        let depth = List.length !control in

        let expr' = reference [] expr in
        
        let compl = b2 <> PI_nop in
        
        control := (ST_branch (compl,depth,[expr'],[],[])) :: old_control;
        let b1' = List.hd (iter [b1]) in
        (*
        ** All referenced and modified objects in branch 1...
        *)
        let rl',ml' = 
          match !control with
          | ST_branch (_,_,_,rl',ml') :: _ -> rl',ml';
          | _ -> [],[] in
          
        control := (ST_branch (compl,depth,[expr;expr],rl',ml')) :: old_control;
        let b2' = List.hd (iter [b2]) in
        
        let prel,condll,modl,postl = flush_cond () in
        (*
        ** FLush still delayed expression of objects 
        ** not modified inside branch, but depending
        ** on branch modified objects BEFORE [prel]!
        *)
        let prel' = flush_expr modl in
        
        let b1'' = 
           match b1' with
           | PI_block (il,bf) when (condll <> []) ->
                PI_block (il @ (List.nth condll 0),bf);
           | _ -> b1' in
        let b2'' = 
           match b2' with
           | PI_block (il,bf) when ((List.length condll) = 2)->
                PI_block (il @ (List.nth condll 1),bf);
           | _ -> b2' in

        (*
        ** All referenced objects in branch 1 & 2
        ** -> propagate rl and ml up to next control environment if any
        *)
        control := (
          match !control with
          | ST_branch (_,_,_,rl'',ml'') :: tl ->  
          begin
            match tl with
            | ST_branch (c,d,e,rl''',ml''') :: tl' -> 
              ST_branch (c,d,e,rl'''@rl'',ml'''@ml'') :: tl';
            | ST_loop (d,e,rl''',ml''') :: tl' ->
              ST_loop (d,e,rl'''@rl'',ml'''@ml'') :: tl';
            | _ -> tl;
          end;
          | _  :: tl -> tl 
          | [] -> []
        );
        
        (* let expr',prel' = flush_expr expr prel in *)
        
        prel' @ prel @ [PI_branch (src,expr',b1'',b2'')] @ postl @ (iter tl);
      end;
	  
      | PI_select (src,expr,casel) when (!in_sched <> [])->
      begin
        line src;
        debug "eval_pro_ref_stack" with (sprintf "instr=PI_select(%s)..." (print_src src));

        let expr' = reference [] expr in (* must be relocated later *)
        let old_control = !control in
        let depth = List.length !control in
        let cl,cl_bf =
          match casel with
          | PI_block (l,bf) -> l,bf;
          | _ -> error 994952 "" in
          
        let exprl = ref [] in 
        let prel' = [] in
        let cl' = ref [] in

        let compl = 
          let others = ref false in
          let cl =
            match casel with
            | PI_block (l,_) -> l;
            | PI_list l -> l;
            | _ -> error 440874 "" in 
          List.iter (fun case ->
            match case with
            | PI_case (_,[],_) -> others := true;
            | PI_block ([PI_case (_,[],_)],_) -> others := true;
            | _ -> ()
            ) cl;
          !others in
        
        let first = ref true in
        List.iter (fun c ->
          match c with
          | PI_block ([PI_case (src,expr'',b)],bf) ->
            exprl := !exprl @ [expr'];
            let rl',ml' =
              if !first then [],[]
              else
                match !control with
                | ST_branch (_,_,_,rl',ml') :: _ -> rl',ml';
                | _ -> [],[] in
            control := (ST_branch (compl,depth, !exprl, rl',ml')) :: old_control;
            let b' = List.hd (iter [b]) in
            cl' := !cl' @ [bf,src,expr'',b'];
            first := false;			
          | _ -> error 381614 "" 
          ) cl;

        let prel,condll,modl,postl = flush_cond () in
        (*
        ** FLush still delayed expression of objects 
        ** not modified inside branch, but depending
        ** on branch modified objects BEFORE [prel]!
        *)
        let prel' = flush_expr modl in
        let casel' = 
          let n = ref 0 in
          PI_block ((List.map (fun (bf,s,e,b) ->
                       incr n;
                       PI_block ([PI_case (s,e,(match b with
                                     | PI_block (il,bf) when (List.length condll >= !n) ->
                                       PI_block (il @ (List.nth condll (!n-1)),bf);
                                     | _ -> b))],bf)
                         ) !cl'),cl_bf) in

        (*
        ** All referenced objects in branches
        ** -> propagate rl and ml up to next control environment if any
        *)
        control := (
          match !control with
          | ST_branch (_,_,_,rl'',ml'') :: tl ->  
          begin
            match tl with
            | ST_branch (c,d,e,rl''',ml''') :: tl' -> 
              ST_branch (c,d,e,rl'''@rl'',ml'''@ml'') :: tl';
            | ST_loop (d,e,rl''',ml''') :: tl' ->
              ST_loop (d,e,rl'''@rl'',ml'''@ml'') :: tl';
            | _ -> tl;
          end;
          | _  :: tl -> tl 
          | [] -> []
        );
        (* let expr',prel' = flush_expr expr prel in *)
        
        prel' @ prel @ [PI_select (src,expr',casel')] @ postl @ (iter tl);
      end;
	  
      | PI_loop (src,kind,expr,b) ->
      begin
        line src;
        debug "eval_pro_ref_stack" with (sprintf "instr=PI_loop(%s)..." (print_src src));

        let namel = expr_names expr in
        let prel_expr = if namel <> [] then flush_objs namel else [] in 

        let old_control = !control in
        let depth = List.length !control in
        control := (ST_loop (depth,expr,[],[])) :: old_control;
        let b' = List.hd (iter [b]) in
        let prel,condl,modl = flush_loop () in
        (*
        ** FLush still delayed expression of objects 
        ** not modified inside branch, but depending
        ** on branch modified objects BEFORE [prel]!
        *)
        let prel' = flush_expr modl in
        let b'' = 
           match b' with
           | PI_block (il,bf) ->
                PI_block (il @ condl,bf);
           | _ -> b' in

        (*
        ** All referenced objects in loop
        ** -> propagate rl and ml up to next control environment if any
        *)
        control := (
          match !control with
          | ST_loop (_,_,rl'',ml'') :: tl ->  
          begin
            match tl with
            | ST_branch (c,d,e,rl''',ml''') :: tl' -> 
              ST_branch (c,d,e,rl'''@rl'',ml'''@ml'') :: tl';
            | ST_loop (d,e,rl''',ml''') :: tl' ->
              ST_loop (d,e,rl'''@rl'',ml'''@ml'') :: tl';
            | _ -> tl;
          end;
          | _  :: tl -> tl 
          | [] -> []
        );
        prel' @ prel_expr @ prel @ ((PI_loop (src,kind,expr,b'')) :: (iter tl));                        
      end;

      | PI_forloop (src,expr,dir,lim1,lim2,b) ->
      begin
        line src;
        debug "eval_pro_ref_stack" with (sprintf "instr=PI_forloop(%s)..." (print_src src));

        let namel = expr_names expr in
        let prel_expr = if namel <> [] then flush_objs namel else [] in 

        let old_control = !control in
        let depth = List.length !control in
        control := (ST_loop (depth,expr,[],[])) :: old_control;
        let b' = List.hd (iter [b]) in
        let prel,condl,modl = flush_loop () in
        let b'' = 
           match b' with
           | PI_block (il,bf) ->
                PI_block (il @ condl,bf);
           | _ -> b' in

        (*
        ** All referenced objects in loop
        ** -> propagate rl and ml up to next control environment if any
        *)
        control := (
          match !control with
          | ST_loop (_,_,rl'',ml'') :: tl ->  
          begin
            match tl with
            | ST_branch (c,d,e,rl''',ml''') :: tl' -> 
              ST_branch (c,d,e,rl'''@rl'',ml'''@ml'') :: tl';
            | ST_loop (d,e,rl''',ml''') :: tl' ->
              ST_loop (d,e,rl'''@rl'',ml'''@ml'') :: tl';
            | _ -> tl;
          end;
          | _  :: tl -> tl 
          | [] -> []
        );
        debug "eval_pro_ref_stack" with (sprintf "#prel_expr=%d #prel=%d #condl=%d" 
                        (List.length prel_expr) (List.length prel) (List.length condl));
        prel_expr @ prel @ ((PI_forloop (src,expr,dir,lim1,lim2,b'')) :: (iter tl));                        
      end;
        
      | PI_block (il,bf) ->
        let do_sched =
          let rec iter pl =
            match pl with
            | (BP_locked) :: tl -> false;
            | (BP_schedule Sched_auto) :: tl -> true;
            | (BP_schedule (Sched_custom sl)) :: tl ->
              let rec iter2 sl =
                match sl with
                | Sched_refstack :: tl -> true;
                | hd :: tl -> iter2 tl;
                | [] -> iter tl in
              iter2 sl
            | hd::tl -> iter tl;
            | [] -> false in
          iter bf.bf_params in  

        if do_sched then
        begin
           if !in_sched = [] then
              out "Expanding block instructions...";
           let flush = !in_sched = [] in
           in_sched := bf.bf_name :: !in_sched;
           let il' = iter il in
           (*
           ** Schedule delayed assignments (ALAP behaviour).
           *)
           let il'' = 
            if flush then il' @ (flush_all ())
            else il' in
           let instr' = PI_block(il'',bf) in
           in_sched := List.tl !in_sched;

           (*
           ** Maybe we must move flushed assignments BEFORE this block
           ** if it's a functional wrapper block with only one
           ** PI_block list element allowed (the functional one).
           *)
           let rec split_branch il prel =
            match il with
            | i :: tl -> 
            begin
              match i with
              | PI_branch _ | PI_select _ -> prel,i,tl;
              | _ -> split_branch tl (prel@[i]);
            end;
            | [] -> error 988717 "" in
           let rec split_loop il prel =
            match il with
            | i :: tl -> 
            begin
              match i with
              | PI_loop _ | PI_forloop _ -> prel,i,tl;
              | _ -> split_loop tl (prel@[i]);
            end;
            | [] -> error 988718 "" in
            
           match bf.bf_type with
           | BF_compound -> instr' :: (iter tl);
           | BF_data ->
               if il' <> [] then
                   instr' :: (iter tl)
               else
                   iter tl;
           | BF_branch ->
               let prel,func,postl = split_branch il' []  in
               prel @ [PI_block([func],bf)] @ postl @ (iter tl); 
           | BF_loop ->
               let prel,func,postl = split_loop il' [] in
               prel @ [PI_block([func],bf)] @ postl @ (iter tl);
           | _ -> instr' :: (iter tl);
        end
        else
        begin
          if !in_sched <> [] then
          begin
            warning (sprintf "Found block with default or locked scheduling in custom scheduling environment! Ignoring block in scheduling.");
            (*
            ** Flush all objects contained in block
            *)
            let namel = instr_names il in
            let flushed = if namel <> [] then flush_objs namel else [] in
            let il' = flushed @ il in
            (PI_block (il',bf)) :: (iter tl);            
          end
          else
            (PI_block (iter il,bf)) :: (iter tl);
        end;
      | PI_list il -> 
           let instr' = PI_list (iter il) in
           instr' :: (iter tl);
      (*
      ** All these instructions require flush of inquired objects
      *)
      | PI_try (b,cl) ->
        let namel1 = expr_names b in
        let namel2 = expr_names cl in
        let namel = namel1 @ namel2 in
		let prel = if namel <> [] then flush_objs namel else [] in
        prel @ (instr :: (iter tl));		
        
      | PI_fun (src,(opl,ot),sel,argl) ->
      begin
        let namel = ref [] in
        List.iter (fun arg -> namel := !namel @ (expr_names arg);
          ) argl;
        let prel = if !namel <> [] then flush_objs !namel else [] in
        prel @ (instr :: (iter tl));
      end;
	  | PI_waitfor (src,expr,cyc,tmu,e_false,e_true) ->
        let namel = expr_names expr in
		let prel = if namel <> [] then flush_objs namel else [] in
        prel @ (instr :: (iter tl));		
      | _ -> instr :: (iter tl);
    end;
    | [] -> [] in
  
  let ok = 
    if !debug_it_select_list = [] then
      protects (pro.pro_instr <- iter pro.pro_instr)
    else 
    begin
      pro.pro_instr <- iter pro.pro_instr; 
      true
    end;
    in
  if not ok then
  begin
    print_stacks ();
    error 0 "Reference stack scheduling failed.";   
  end;
  pro.pro_instr <- List.map relocate_instr pro.pro_instr;
  pro.pro_instr <- instr_frame pro.pro_instr None;  

  if debug_it "eval_pro_ref_stack" then 
  begin
    List.iter (fun i -> out (cp_sprint_instr i)) pro.pro_instr;
    List.iter (fun i -> out (pi_sprint_instr i)) pro.pro_instr;
  end;
  ind_decr ()

  
let eval_ref_stack main =
    out "Reference Stack Scheduler: Performing expression expansions...";
    ind_incr ();
    let all_reduced = ref 0 in
    let eval_pro md =
      List.iter (fun pro ->
          reduced := 0;
          out (sprintf "in process <%s.%s>" 
                       md.mod_name 
                       pro.pro_name);
          eval_pro_ref_stack pro;
          ind_incr ();
          out (sprintf "Removed %d instructions(s)." !reduced); 
          ind_decr ();
          all_reduced := !all_reduced + !reduced; 
        ) md.mod_procs in
    eval_pro main;
    List.iter (fun md -> eval_pro md) main.mod_external;
    ind_decr ();
    !all_reduced
