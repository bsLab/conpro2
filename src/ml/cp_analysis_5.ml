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
**    $CREATED:     4.1.2007
**    $VERSION:     2.10
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
open Cp_block_frame

open Unix
open Printf

open Cp_analysis_1
open Cp_analysis_2
open Cp_analysis_3
open Cp_analysis_4

(*
** Mark (lock) all temporary objects actually not locked found in instruction.
** Returns all lockec temporary objects.
*)
let rec lock_temps instr =
  let check_temp co =
    let prefix = "TEMPS_" in
    let l = String.length prefix in
    let name = co.co_name in
    let lr = String.length name in
    if lr > l && 
        (String.sub name 0 l) = prefix &&
        not (List.mem Obj_inuse co.co_flags) then 
    begin
      co.co_flags <- co.co_flags @ [Obj_inuse];
      [co];
    end
    else [] in
    
  match instr with
  | PI_obj (opl,ot) ->
  begin
    match co_of_ot ot with
    | Some co -> 
      (check_temp co)@
      (
        if is_sel_obj opl then
          lock_temps (obj_sel_obj opl)
        else []
      )
    | None -> []
  end;
  | PI_list il ->
    List.flatten (List.map lock_temps il)
  | _ -> []

let unlock_temps temps =
  List.iter (fun co ->
    co.co_flags <- List.filter (fun f -> f <> Obj_inuse) co.co_flags;
    ) temps
    
(*
** 1. Resolve array object selector expressions and use temporary register instead.
** 2. Resolve non block (register) array access. If there is more than one object access 
**    in assignement or arithm./bool. expression, replace them with temporary expression, too.
*)
let expand_sel_expr pro instrl =
  let to_be_replaced = ref [] in
  let already_replaced = ref [] in
  
  let temps = ref [] in
  let release_temps () =
    List.iter (fun co ->
        co.co_flags <- List.filter (fun f -> f <> Obj_inuse) co.co_flags;
        ) !temps;
    temps := [] in

  let expand_sel_obj pro sel =
    let modu = pro.pro_module in
    debug "expand_sel_expr"  with (sprintf ">>>> expand_sel_obj in %s: %s" (print_src (source ())) (Cp_printtypes.pi_sprint_instr sel));
    match sel with
    | PI_arithm (op,op1,op2) ->
      if not (is_const_expr sel) then
      begin
        let _,dt = expr_type sel true in
        let temp_sel = tmp_reg pro dt in
        temp_sel.co_rules <- !core_rules;
        temp_sel.co_reader <- [pro];
        temp_sel.co_writer <- [pro];
        temps := !temps @ [temp_sel];
        let sel' = PI_obj ([],OT_reg temp_sel) in
        let expr' = PI_assign(nilsrc (),sel',sel) in
        [expr'],sel'
      end
      else
        [],expr_fold sel;

    | _ -> [],sel in

  let replace_obj pro obj =
    let modu = pro.pro_module in
    let _,dt = expr_type obj false in
    let temp = tmp_reg pro dt in
    temp.co_rules <- !core_rules;
    temp.co_reader <- [pro];
    temp.co_writer <- [pro];
    temps := !temps @ [temp];
    let obj' = PI_obj ([],OT_reg temp) in
    let expr' = PI_assign(nilsrc (),obj',obj) in
    [expr'],obj' in
        
  let aux = ref [] in
  let post_aux = ref [] in
  
  let rec resolve bound lhs pro il =

    let resolve1 lhs i =
        List.hd (resolve bound lhs pro [i]) in
    let resolve2 lhs i =
        match resolve bound lhs pro [i] with
        | [hd] -> hd;
        | [] -> error 517261 "";
        | li -> PI_block (li,nilbf) in

    (*
    ** Find object groups (multiple occurence) within instruction
    *)
    let find_groups instr =
        let objs = pi_get_objs instr in
        let obj_names = List.map name_of_pi objs in
        let obj_names' = List.sort (fun a b -> if a > b then 1 else if a = b then 0 else -1) obj_names in
        let rec groups li curli = 
          match li with
          | o :: tl -> 
          begin
            match curli with
            | o' :: _ -> if o = o' then groups tl (o' :: curli) else [curli] @ (groups tl [o]);
            | [] -> groups tl [o];
          end;
          | [] -> [curli] in
        let groups = groups obj_names' [] in
        let groups' = List.filter (fun ol -> (List.length ol) > 1) groups in
        let l = ref [] in
        List.iter (fun g -> l := !l @ g) groups';
        !l in
        
    match il with
    | instr::tl ->
    begin
      match instr with
      | PI_assign (src,lhs_,rhs) ->
      begin
        line src;
        debug "expand_sel_expr" with (sprintf ">>>> Assign: expand_sel_expr in %s" (print_src (source ())));
        (*
        ** Check for occurance of non-block arrays with dynamic access, both on LHS and RHS.
        *)
        to_be_replaced := find_groups instr;
        already_replaced := [];
        let lhs' = resolve1 true lhs_ in
        let rhs' = resolve1 false rhs in
        let instr' = PI_assign(src,lhs',rhs') in
        
        if !already_replaced <> [] then
          info (sprintf "Replaced %d array object(s) with temporary expression(s)." 
                        (List.length !already_replaced));
        if bound = 0 then
        begin
          let instrl' = !aux @ [instr'] @ !post_aux in
          aux := []; 
          post_aux := [];
          release_temps ();
          instrl' @ (resolve bound lhs pro tl)
        end
        else [instr'] @ (resolve bound lhs pro tl);
      end;
      | PI_arithm (op,op1,op2) ->
        [PI_arithm (op,resolve1 lhs op1,resolve1 lhs op2)];
      | PI_bool (kind,op,op1,op2) ->
        [PI_bool (kind,op,resolve1 lhs op1,resolve1 lhs op2)];
      | PI_concat (op1,op2) ->
        [PI_concat (resolve1 lhs op1,resolve1 lhs op2)];
      | PI_obj (opl,ot) ->
      begin
        let ot_name = name_of_ot ot in
        debug "expand_sel_expr" with (sprintf ">>>> Obj: expand_sel_expr of <%s> in %s [is_sel_obj=%b is_index_obj=%b]" 
                                             ot_name (print_src (source ()))
                                             (is_sel_obj opl)
                                             (is_index_obj opl));
        let obj' = 
          if is_sel_obj opl then
            PI_obj (List.map (fun pa ->
                        match pa with
                        | OD_sel_obj selobj -> 
                            let aux',selobj' = expand_sel_obj pro selobj in
                            aux := !aux @ aux';
                            OD_sel_obj selobj';
                        | _ -> pa;
                        ) opl,
                    ot)
          else if is_index_obj opl then
            PI_obj (List.map (fun pa ->
                        match pa with
                        | OD_index selobj -> 
                            let aux',selobj' = expand_sel_obj pro selobj in
                            aux := !aux @ aux';
                            OD_index selobj';
                        | _ -> pa;
                        ) opl,
                    ot)
            
          else
            instr in

        let obj'' =
          if (is_array ot) then
          begin
            match ot with
            | OT_array at -> 
              let is_dyn,is_block =
                List.mem AT_dyn at.at_flags,
                List.mem AT_block at.at_flags in
              if is_dyn && not is_block then
              begin
                (*
                ** Multiple occurence of non-block-dynamic  array access must be scattered to 
                ** temporary expression, replacing array object with temporary register!
                *)
                if List.mem ot_name !to_be_replaced then
                begin
                  let removed = ref false in
                  to_be_replaced := List.filter (fun o -> 
                                        let eq = o = ot_name in
                                        let remove_it = eq && not !removed in
                                        if eq then removed := true;
                                        not remove_it) !to_be_replaced;
                  if List.mem ot_name !already_replaced then
                  begin
                    already_replaced := ot_name :: !already_replaced;
                    (*
                    ** Temporary expression required
                    *)
                    let assign,temp= replace_obj pro obj' in
                    if not lhs 
                      then aux := !aux @ assign
                      else post_aux := !post_aux @ assign;
                    temp
                  end
                  else
                  begin
                    already_replaced := ot_name :: !already_replaced;
                    (*
                    ** First occurence may not be replaced.
                    *)
                    obj'    
                  end;
                end
                else obj'
              end
              else obj';
            | _ -> error 339637 "";  
          end else obj' in
        
        obj'' :: (resolve bound lhs pro tl);
      end;
      | PI_block (il,bf) ->
        let bound' = bound + (if (List.mem BP_bind bf.bf_params) then 1 else 0) in
        if bound' <> 1 then
        begin
          let il' = resolve bound' lhs pro il in        
          PI_block (il',bf) :: (resolve bound lhs pro tl);
        end
        else 
        begin
          let il' = resolve bound' lhs pro il in        
          let instrl' = !aux @ [PI_block (il',bf)] @ !post_aux in
          aux := []; 
          post_aux := [];
          release_temps (); 
          instrl' @  (resolve bound lhs pro tl);        
        end;
      | PI_list il ->
        let il' = resolve bound lhs pro il in
        PI_list il' :: (resolve bound lhs pro tl);
      | PI_branch (src,i1,i2,i3) ->
        line src;
        let instr' = resolve1 lhs i1 in
        let instrl' = !aux @ !post_aux in
        aux := []; 
        post_aux := [];
        release_temps ();
        
        instrl' @ [
        PI_branch (src,
                     instr',
                     resolve2 lhs i2,
                     resolve2 lhs i3)] @ (resolve bound lhs pro tl);
      | PI_forloop (src,i1,dir,i2,i3,i4) ->
        line src;
        PI_forloop (src,resolve1 lhs i1,
                        dir,
                        resolve1 lhs i2,
                        resolve1 lhs i3,
                        resolve2 lhs i4) :: (resolve bound lhs pro tl);
      | PI_loop (src,k,i1,i2) ->
        line src;
        let instr' = resolve1 lhs i1 in
        let instrl' = !aux @ !post_aux in
        aux := []; 
        post_aux := [];
        release_temps ();
        instrl' @ [PI_loop (src,k,
                     instr',
                     resolve2 lhs i2)] @ (resolve bound lhs pro tl);
      | PI_select (src,i1,i2) ->
        line src;
        let instr' = resolve1 lhs i1 in
        let instrl' = !aux @ !post_aux in
        aux := []; 
        post_aux := [];
        release_temps ();
        instrl' @ [PI_select (src,
                     instr',
                     resolve1 lhs i2)] @ (resolve bound lhs pro tl);
      | PI_case (src,il1,i2) ->
        line src;
        PI_case (src,
                     resolve bound lhs pro il1,
                     resolve2 lhs i2) :: (resolve bound lhs pro tl);
      | PI_try (i1,i2) ->
        PI_try (resolve1 lhs i1,
                resolve1 lhs i2) :: (resolve bound lhs pro tl);
      (*
      ** Only EMI/ADTO function=method calls
      *)
      | PI_fun (src,(opl,OT_object ao),sel,args) ->
      begin
        line src;
        debug "expand_sel_expr" with (sprintf "expand_sel_expr: PI_fun(ao) in %s" (print_src src));
        let temps' = lock_temps (PI_list args) in
        let args_flags = ref [] in
        List.iter (fun (sel',args_flags') ->
            if sel = sel' then args_flags := args_flags';
            ) ao.ao_type.ta_rules.rl_methods;
        let args' = List.map2 (fun arg arg_desc ->
                let lhs = arg_desc.arg_type = Arg_lhs in
                resolve1 lhs arg
            ) args !args_flags in
        let instrl' = !aux @ !post_aux in
        aux := []; 
        post_aux := [];
        release_temps ();
        unlock_temps temps';
        instrl' @ [PI_fun (src,(opl,OT_object ao),sel,args')] @ (resolve bound false pro tl);
      end;
      | PI_fun (src,(opl,ot),sel,args) ->
        line src;
        debug "expand_sel_expr" with (sprintf "expand_sel_expr: PI_fun(%s) in %s" 
                                              (Cp_printtypes.pi_sprint_instr (PI_obj(opl,ot))) (print_src src));
        let opl',ot' =
          match resolve1 lhs (PI_obj (opl,ot)) with
          | PI_obj (opl',ot') -> opl',ot'
          | _ -> progerr "PI_obj (opl',ot')" in
        let args' = List.map (resolve1 lhs) args in
        let instrl' = !aux @ !post_aux in
        aux := []; 
        post_aux := [];
        release_temps ();
        instrl' @ [PI_fun (src,(opl',ot'),sel,args')] @ (resolve bound false pro tl);
      | _ -> instr :: (resolve bound lhs pro tl);
    end;
    | [] -> [] in

  match pro with
  | Some pro ->
    resolve 0 false pro instrl;
  | None -> instrl
            
let expand_sel_expr pro il =
    let pro_name =
      match pro with
      | Some p -> p.pro_name;
      | None -> "" in
    debug "expand_sel_expr"  with (sprintf ">> expand_sel_expr <%s>" pro_name);
    let il' = if not compiler.t_C && not compiler.t_ml then expand_sel_expr pro il else il in
    if debug_it "expand_sel_expr" then List.iter print_instr il';
    debug "expand_sel_expr" with (sprintf "<< expand_sel_expr <%s>" pro_name);
    il'

(*
** Resolve guarded objects and expressions in function arguments and replace them
** with temporary registers if required. Requires information about object guards.
** called from Cp_synthesis.post_transform.
**
**
**    fun(arg1,arg2...) =>
**
**    temp_read
**    fun(temp_read,temp_write,...)
**    temp_write
*)

let expand_fun_arg pro instrl =
  let temps = ref [] in
  let release_temps () =
    List.iter (fun co ->
        co.co_flags <- List.filter (fun f -> f <> Obj_inuse) co.co_flags;
        ) !temps;
    temps := [] in
  let pre_aux = ref [] in
  let post_aux = ref [] in
  let in_expr = ref false in

  let rec resolve lhs pro il =

    let resolve1 lhs i =
        List.hd (resolve lhs pro [i]) in
    let resolve2 lhs i =
        match resolve lhs pro [i] with
        | [hd] -> hd;
        | [] -> error 317261 "";
        | li -> PI_block (li,nilbf) in

    match il with
    | instr::tl ->
    begin
      match instr with
      | PI_arithm (op,op1,op2) ->
      begin
        let instr' = expr_fold instr in
        if not !in_expr && not (is_const_expr instr') then
        begin
            in_expr := true;
            let expr' = PI_arithm (op,resolve1 true op1,
                                      resolve1 false op2) in
            let _,dt = expr_type expr' true in
            let temp_obj = tmp_reg pro dt in
            temp_obj.co_rules <- !core_rules;
            temp_obj.co_reader <- [pro];
            temp_obj.co_writer <- [pro];
            temps := !temps @ [temp_obj];
            let obj' = PI_obj ([],OT_reg temp_obj) in
            let expr'' = PI_assign(source (),obj',expr') in
            pre_aux := !pre_aux @ [expr''];
            in_expr := false; 
            [obj']
        end
        else
            [instr]
      end;
      | PI_bool (kind,op,op1,op2) ->
      begin
        if not !in_expr then
        begin
            in_expr := true;
            let expr' = PI_bool (kind,op,resolve1 false op1,
                                         resolve1 false op2) in
            let _,dt = expr_type expr' false in
            let temp_obj = tmp_reg pro dt in
            temp_obj.co_rules <- !core_rules;
            temp_obj.co_reader <- [pro];
            temp_obj.co_writer <- [pro];
            temps := !temps @ [temp_obj];
            let obj' = PI_obj ([],OT_reg temp_obj) in
            let expr'' = PI_assign(source (),obj',expr') in
            pre_aux := !pre_aux @ [expr''];
            in_expr := false; 
            [obj']
        end
        else
            [instr]
      end;
      | PI_obj (opl,ot) ->
      begin
        debug "expand_fun_arg" with (sprintf ">>>> Obj: expand_fun_arg of <%s> in %s" 
                                            (name_of_ot ot) (print_src (source ())));
        let gd_wr,gd_rd =
          match co_of_ot ot with
          | Some co ->
          begin 
            match co.co_guard with
            | Some gd ->
              List.mem GD_wr gd.gd_req,
              List.mem GD_rd gd.gd_req;
            | None -> false,false
          end;
          | None -> false,false in
        
        if lhs && gd_wr then
        begin
            let obj = instr in
            let dt = 
                match dt_of_ot ot with
                | Some dt -> dt;
                | None -> error 0 "expand_fun_arg: Unexpected dynamic selector of abstract obejct."
                in
            let temp_obj = tmp_reg pro dt in
            temp_obj.co_rules <- !core_rules;
            temp_obj.co_reader <- [pro];
            temp_obj.co_writer <- [pro];
            temps := !temps @ [temp_obj];
            let obj' = PI_obj ([],OT_reg temp_obj) in
            let expr' = PI_assign(source (),obj,obj') in
            post_aux := !post_aux @ [expr'];
            [obj']
          
        end
        else if not lhs && gd_rd then
        begin
          let obj = instr in
          let dt = 
              match dt_of_ot ot with
              | Some dt -> dt;
              | None -> error 0 "expand_fun_arg: Unexpected dynamic selector of abstract obejct."
              in
          let temp_obj = tmp_reg pro dt in
          temp_obj.co_rules <- !core_rules;
          temp_obj.co_reader <- [pro];
          temp_obj.co_writer <- [pro];
          temps := !temps @ [temp_obj];
          let obj' = PI_obj ([],OT_reg temp_obj) in
          let expr' = PI_assign(source (),obj',obj) in
          pre_aux := !pre_aux @ [expr'];
          [obj']
        end
        else [instr] 
      end;
      (*
      ** Only EMI/ADTO function=method calls
      *)
      | PI_fun (src,(opl,OT_object ao),sel,args) ->
      begin
        line src;
        let args_flags = ref [] in
        (*
        ** Mark all temporary registers already
        ** used in arguments as used
        *)
        let temps' = lock_temps (PI_list args) in
        
        List.iter (fun (sel',args_flags') ->
            if sel = sel' then args_flags := args_flags';
            ) ao.ao_type.ta_rules.rl_methods;
        let args' = List.map2 (fun arg arg_desc ->
                let lhs = arg_desc.arg_type = Arg_lhs in
                resolve1 lhs arg
            ) args !args_flags in
        unlock_temps temps';
        PI_fun (src,(opl,OT_object ao),sel,args') :: (resolve lhs pro tl);
      end;
      | _ -> instr :: (resolve lhs pro tl);
    end;
    | [] -> [] in

  match pro with
  | Some pro ->
    let instrl' = resolve false pro instrl in
    release_temps ();
    !pre_aux @ instrl' @ !post_aux
  | None -> instrl

let expand_fun_arg pro il =
    let pro_name =
      match pro with
      | Some p -> p.pro_name;
      | None -> "" in
    debug "expand_fun_arg" with (sprintf ">> expand_fun_arg <%s>" pro_name);
    let il' = if not compiler.t_C && not compiler.t_ml then expand_fun_arg pro il else il in
    if debug_it "expand_fun_arg" then List.iter print_instr il';
    debug "expand_fun_arg" with (sprintf "<< expand_fun_arg <%s>" pro_name);
    il'


(*
** Function argument expansion: main entry
**
** 1a. Resolve objects with dynamic array selectors. Replace dynamic selected
**     arrays with temporary register expressions. Required for function
**     arguments.
** 1b. Resolve array object selector expressions and use temporary register instead.
** 1c. Resolve non block (register) array access. If there is more than one object access 
**     in assignement or arithm./bool. expression, replace them with temporary expression, too.
**
** 1d. Resolve expression, replace with temporary register expressions.
**
**      expr => 
**      temp <- expr
**      ..temp..
*)

let expand_expr pro instrl =
  let temps = ref [] in
  let release_temps () =
    List.iter (fun co ->
        co.co_flags <- List.filter (fun f -> f <> Obj_inuse) co.co_flags;
        ) !temps;
    temps := [] in
  let aux = ref [] in
  let post_aux = ref [] in
  let in_expr = ref false in

  let rec resolve lhs pro il =

    let resolve1 lhs i =
        List.hd (resolve lhs pro [i]) in
    let resolve2 lhs i =
        match resolve lhs pro [i] with
        | [hd] -> hd;
        | [] -> error 317261 "";
        | li -> PI_block (li,nilbf) in

    match il with
    | instr::tl ->
    begin
      match instr with
      | PI_arithm (op,op1,op2) ->
      begin
        let instr' = expr_fold instr in
        if not !in_expr && not (is_const_expr instr') then
        begin
            in_expr := true;
            let expr' = PI_arithm (op,resolve1 false op1,
                                      resolve1 false op2) in
            let _,dt = expr_type expr' true in
            let temp_obj = tmp_reg pro dt in
            temp_obj.co_rules <- !core_rules;
            temp_obj.co_reader <- [pro];
            temp_obj.co_writer <- [pro];
            temps := !temps @ [temp_obj];
            let obj' = PI_obj ([],OT_reg temp_obj) in
            let expr'' = PI_assign(source (),obj',expr') in
            aux := !aux @ [expr''];
            in_expr := false; 
            [obj']
        end
        else
            [instr]
      end;
      | PI_bool (kind,op,op1,op2) ->
      begin
        if not !in_expr then
        begin
            in_expr := true;
            let expr' = PI_bool (kind,op,resolve1 false op1,
                                         resolve1 false op2) in
            let _,dt = expr_type expr' false in
            let temp_obj = tmp_reg pro dt in
            temp_obj.co_rules <- !core_rules;
            temp_obj.co_reader <- [pro];
            temp_obj.co_writer <- [pro];
            temps := !temps @ [temp_obj];
            let obj' = PI_obj ([],OT_reg temp_obj) in
            let expr'' = PI_assign(source (),obj',expr') in
            aux := !aux @ [expr''];
            in_expr := false; 
            [obj']
        end
        else
            [instr]
      end;
      | PI_obj (opl,ot) ->
      begin
        debug "expand_expr" with (sprintf ">>>> Obj: expand_expr of <%s> in %s" 
                                          (name_of_ot ot) (print_src (source ())));
        if is_sel_obj opl && not !in_expr && not lhs then
        begin
            let obj = instr in
            let dt = 
                match dt_of_ot ot with
                | Some dt -> dt;
                | None -> error 0 "expand_expr: Unexpected dynamic selector of abstract object."
                in
            let temp_obj = tmp_reg pro dt in
            temp_obj.co_rules <- !core_rules;
            temp_obj.co_reader <- [pro];
            temp_obj.co_writer <- [pro];
            temps := !temps @ [temp_obj];
            let obj' = PI_obj ([],OT_reg temp_obj) in
            let expr' = PI_assign(source (),obj',obj) in
            aux := !aux @ [expr'];
            [obj']
        end
        else if is_sel_obj opl && lhs then
        begin
            let obj = instr in
            let dt = 
                match dt_of_ot ot with
                | Some dt -> dt;
                | None -> error 0 "expand_expr: Unexpected dynamic selector of abstract obejct."
                in
            let temp_obj = tmp_reg pro dt in
            temp_obj.co_rules <- !core_rules;
            temp_obj.co_reader <- [pro];
            temp_obj.co_writer <- [pro];
            temps := !temps @ [temp_obj];
            let obj' = PI_obj ([],OT_reg temp_obj) in
            let expr' = PI_assign(source (),obj,obj') in
            post_aux := !post_aux @ [expr'];
            [obj']
          
        end
        else [instr]
      end;
      (*
      ** Only EMI/ADTO function=method calls
      *)
      | PI_fun (src,(opl,OT_object ao),sel,args) ->
        line src;
        let temps' = lock_temps (PI_list args) in
        let args_flags = ref [] in
        List.iter (fun (sel',args_flags') ->
            if sel = sel' then args_flags := args_flags';
            ) ao.ao_type.ta_rules.rl_methods;
        let args' = List.map2 (fun arg arg_desc ->
                let lhs = arg_desc.arg_type = Arg_lhs in
                resolve1 lhs arg
            ) args !args_flags in
        unlock_temps temps';
        PI_fun (src,(opl,OT_object ao),sel,args') :: (resolve false pro tl);

      (*
      ** user defined functions
      *)
      | PI_fun (src,(opl,ot),sel,args) ->
        line src;
        let args' = List.map (resolve1 false) args in
        (PI_fun (src,(opl,ot),sel,args')) :: (resolve false pro tl);

      | _ -> instr :: (resolve lhs pro tl);
    end;
    | [] -> [] in

  match pro with
  | Some pro ->
    let instrl' = resolve false pro (expand_sel_expr (Some pro) instrl) in
    !aux @ instrl' @ !post_aux
  | None -> instrl

let expand_expr pro il =
    let pro_name =
      match pro with
      | Some p -> p.pro_name;
      | None -> "" in
    debug "expand_expr" with (sprintf ">> expand_expr <%s>" pro_name);
    let il' = if not compiler.t_C && not compiler.t_ml then expand_expr pro il else il in
    if debug_it "expand_expr" then List.iter print_instr il';
    debug "expand_expr" with (sprintf "<< expand_expr <%s>" pro_name);
    il'

(*
** Resolve certain arithmetic operations like shifting and
** move them from original to temporary expressions if necessary.
**
** 1. Shift operations (both static and dynamic selectors)
**  x <- a+(b lsl 1)
**      =>
**  t <- b lsl 1
**  x <- a+t
*)

let expand_arith_expr pro instrl =
  let moved = ref 0 in
  let temps = ref [] in
  let release_temps () =
    List.iter (fun co ->
        co.co_flags <- List.filter (fun f -> f <> Obj_inuse) co.co_flags;
        ) !temps;
    temps := [] in

  let is_obj pi =
    match pi with
    | PI_obj _ -> true;
    | _ -> false in

  let make_tmp_expr pro expr =
    incr moved;
    let _,dt = expr_type expr true in
    let temp = tmp_reg pro dt in
    temp.co_rules <- !core_rules;
    temp.co_reader <- [pro];
    temp.co_writer <- [pro];
    temps := !temps @ [temp];
    let temp_obj = PI_obj ([],OT_reg temp) in
    let assign = [PI_assign (source (),temp_obj,expr)] in
    assign,temp_obj in

  let merge_block pro exprl =
    let bf = create_frame exprl in
    (*
    ** Avoid scheduling again...
    *)
    bf.bf_params <- bf.bf_params @ [BP_locked];
    [PI_block (exprl, bf)] in
    

  let resolve_obj pro expr =
    let expr' = expr_fold expr in
    match expr' with
    | PI_obj _ -> [],expr';
    | PI_arithm _ ->
        make_tmp_expr pro expr';
    | _ -> error 404493 "" in

  let rec resolve pro il =
    let resolve1 i =
        List.hd (resolve pro [i]) in
    let resolve2 i =
        match resolve pro [i] with
        | [hd] -> hd;
        | [] -> error 517262 "";
        | li -> PI_block (li,nilbf) in

    let shift_extract expr =
      (*
      ** Shift operation may only consist of objects, without nested
      ** expressions, and may not be nested in other expression itself.
      *)
      let aux = ref [] in
      let first = ref true in
      let rec iter expr =
        match expr with
        | PI_arithm (op,op1,op2) ->
        begin
          match op with
          | OP_lsl
          | OP_lsr
          | OP_asl
          | OP_asr when not !first ->
            let aux1,op1' = resolve_obj pro (iter op1) in
            let aux2,op2' = resolve_obj pro (iter op2) in
            let aux3,temp = make_tmp_expr pro (PI_arithm (op,op1',op2')) in
            aux := !aux @ aux1 @ aux2 @ aux3;
            temp
          | OP_lsl
          | OP_lsr
          | OP_asl
          | OP_asr when not (is_obj op1) && (is_obj op2) ->
            let aux1,temp = make_tmp_expr pro (iter op1) in
            let op2' = iter op2 in
            aux := !aux @ aux1;
            PI_arithm (op,temp,op2');
          | OP_lsl
          | OP_lsr
          | OP_asl
          | OP_asr when not (is_obj op2) && (is_obj op1) ->
            let aux1,temp = make_tmp_expr pro (iter op2) in
            let op1' = iter op1 in
            aux := !aux @ aux1;
            PI_arithm (op,op1',temp);
          | OP_lsl
          | OP_lsr
          | OP_asl
          | OP_asr when not (is_obj op2) && not (is_obj op1) ->
            let aux1,temp1 = make_tmp_expr pro (iter op1) in
            let aux2,temp2 = make_tmp_expr pro (iter op2) in
            aux := !aux @ aux1 @ aux2;
            PI_arithm (op,temp1,temp2);
          | _ -> 
            first := false;
            let op1' = iter op1 in
            let op2' = iter op2 in
            PI_arithm (op,op1',op2');
        end;
        | PI_bool  (kind,op,op1,op2) ->
        begin
          first := false;
          PI_bool  (kind,op,iter op1,iter op2);
        end;
        | _ -> expr in
      let expr' = iter expr in
      !aux, expr' in

    match il with
    | instr::tl ->
    begin
      match instr with
      | PI_assign (src,lhs,rhs) ->
      begin
        line src;
        moved := 0;
        let aux,rhs' = shift_extract rhs in
        release_temps ();
        if !moved > 0 then
            out (sprintf "Expanded [+%d] arithemetic expression(s) <%s>."
                         !moved (print_src src));
        (if aux <> [] then merge_block pro (aux @ [PI_assign (src,lhs,rhs')])
                      else [PI_assign (src,lhs,rhs')]) @ 
        (resolve pro tl);
      end;
      | PI_block (il,bf) ->
        let il' = resolve pro il in        
        PI_block (il',bf) :: (resolve pro tl);
      | PI_list il ->
        let il' = resolve pro il in
        PI_list il' :: (resolve pro tl);
      | PI_branch (src,i1,i2,i3) ->
        line src;
        PI_branch (src,
                     resolve1 i1,
                     resolve2 i2,
                     resolve2 i3) :: (resolve pro tl);
      | PI_forloop (src,i1,dir,i2,i3,i4) ->
        line src;
        PI_forloop (src,resolve1 i1,
                        dir,
                        resolve1 i2,
                        resolve1 i3,
                        resolve2 i4) :: (resolve pro tl);
      | PI_loop (src,k,i1,i2) ->
        line src;
        PI_loop (src,k,
                     resolve1 i1,
                     resolve2 i2) :: (resolve pro tl);
                     
      | PI_try (i1,i2) ->
        PI_try (resolve1 i1,
                resolve1 i2) :: (resolve  pro tl);
      | PI_select (src,i1,i2) ->
        line src;
        PI_select (src,
                     resolve1 i1,
                     resolve1 i2) :: (resolve pro tl);
      | PI_case (src,il1,i2) ->
        line src;
        PI_case (src,
                     resolve pro il1,
                     resolve2 i2) :: (resolve pro tl);
      | _ -> instr :: (resolve pro tl);
    end;
    | [] -> [] in

  match pro with
  | Some pro ->
    resolve pro instrl 
                     
  | None -> instrl

let expand_arith_expr pro il =
    let pro_name =
      match pro with
      | Some p -> p.pro_name;
      | None -> "" in
    debug "expand_arith_expr" with (sprintf ">> expand_arith_expr <%s>" pro_name);
    let il' = expand_arith_expr pro il in
    if debug_it "expand_arith_expr" then List.iter print_instr il';
    debug "expand_arith_expr" with (sprintf "<< expand_arith_expr <%s>" pro_name);
    il'


(*
** Do array object and ADTO transformations in assignments; replace
** object access with init, read and write methods for objects:
**
**      ar <- val; 
**   =>
**      ar.init val;
*)

let assign_method_transform pro pi =
  let pi_obj pi =
    match pi with
    | PI_obj (opl,ot) ->
      opl,ot;
    | _ -> error 204206 "" in
  let ao_of_ot ot =
    match ot with
    | OT_array at -> 
      (*
      ** Array itself is ADTO!
      *)
      at.at_ao;
    | OT_object ao -> ao;
    | _ -> error 335401 "" in
  let is_adto pi =
    match pi with
    | PI_obj (opl,OT_array at) ->
      not (is_sel opl) && 
      not (is_sel_obj opl)
    | PI_obj (_,OT_object ao) -> true;
    | _ -> false in
    
  debug "assign_method_transform" with "assign_method_transform";

  match pi with
  | PI_assign (src,lhs,rhs) ->
  line src;
  if is_adto lhs then
  begin
    match lhs with
    | PI_obj (opl,OT_array at) ->
      if not (is_sel opl) && 
         not (is_sel_obj opl) then
      begin
        let sub = 
          if is_sub opl then
            Some (obj_sub opl)
          else None in
          
        match rhs with
        | PI_obj (_,OT_value _)
        | PI_obj (_,OT_reg _)
        | PI_obj (_,OT_var _) ->
        let is_vec = 
          match rhs with
          | PI_obj (_,OT_value (V_list _))
          | PI_obj (_,OT_value (V_string _)) -> true;
          | _ -> false in
        if not is_vec then
        begin
          (*
          ** ar <- val; => ar.init val | ar.set a b val;
          *)
          let opl,ot = pi_obj lhs in
          let ao = ao_of_ot ot in
          match sub with
          | Some (a,b) ->
            let args = [
              PI_obj([],OT_value (V_int (Int64.of_int (a))));
              PI_obj([],OT_value (V_int (Int64.of_int (b))));
              rhs;
              ] in      
            info (sprintf
                 "Found %s.set method in assignment."
                 at.at_name);      
            let pi' = PI_fun(src,(opl,OT_object ao),"set",args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi' an.a_toplevel;  
            [pi'];
          | None ->
            let args = [rhs] in 
            info (sprintf
                 "Found %s.init method in assignment."
                 at.at_name);      
            let pi' = PI_fun(src,(opl,OT_object ao),"init",args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi' an.a_toplevel;  
            [pi'];
        end
        else 
        begin
          match sub with
          | Some (a,b) ->
          begin
            (*
            ** ar.[a to b] <- "xbxbxb"; => ar.copyn a 0 (b-a+1) "xbvxby";
            *)
            let opl,ot = pi_obj lhs in
            let ao = ao_of_ot ot in
            let src_off =
                match rhs with
                | PI_obj (opl,_) ->
                    if is_sub opl then
                    begin
                        let a,b= obj_sub opl in
                        a
                    end
                    else 0 
                | _ -> 0 in
            let args = [
                PI_obj([],OT_value (V_int (Int64.of_int a)));
                PI_obj([],OT_value (V_int (Int64.of_int src_off)));
                PI_obj([],OT_value (V_int (Int64.of_int (b-a+1))));
                rhs
                ] in 
            info (sprintf
                 "Found %s.copyn method in assignment."
                 at.at_name);      
            let pi' = PI_fun(src,(opl,OT_object ao),"copyn",args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi' an.a_toplevel;  
            [pi'];
          end;
          | None ->
          begin
            (*
            ** ar <- "xbxbxb"; => ar.copy "xbvxby";
            *)
            let opl,ot = pi_obj lhs in
            let ao = ao_of_ot ot in
            let args = [rhs] in 
            info (sprintf
                 "Found %s.copy method in assignment."
                 at.at_name);      
            let pi' = PI_fun(src,(opl,OT_object ao),"copy",args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi' an.a_toplevel;  
            [pi'];
          end;
        end;
        | PI_obj (_,OT_array at') ->
        begin
          match sub with
          | Some (a,b) ->
            (*
            ** ar.[a to b] <- ar2; => ar.copyn a 0 (b-a+1) ar2;
            *)
            let opl,ot = pi_obj lhs in
            let ao = ao_of_ot ot in
            let src_off =
                match rhs with
                | PI_obj (opl,_) ->
                    if is_sub opl then
                    begin
                        let a,b= obj_sub opl in
                        a
                    end
                    else 0 
                | _ -> 0 in
            let args = [
                PI_obj([],OT_value (V_int (Int64.of_int a)));
                PI_obj([],OT_value (V_int (Int64.of_int src_off)));
                PI_obj([],OT_value (V_int (Int64.of_int (b-a+1))));
                rhs
                ] in 
            info (sprintf
                 "Found %s.copyn method in assignment."
                 at.at_name);      
            let pi' = PI_fun(src,(opl,OT_object ao),"copyn",args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi' an.a_toplevel;  
            [pi'];
          | None ->
            (*
            ** ar1 <- ar2; => ar1.copy ar2;
            *)
            let opl,ot = pi_obj lhs in
            let ao = ao_of_ot ot in
            let args = [rhs] in 
            info (sprintf
                 "Found %s.copy method in assignment."
                 at.at_name);      
            let pi' = PI_fun(src,(opl,OT_object ao),"copy",args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi' an.a_toplevel;  
            [pi'];      
        end;
        | _ -> error 0 "\nFound unexepected RHS object in array assignment";
      end 
      else
        [pi];
    | PI_obj (opl,OT_object ao) ->
        let rlm = ao.ao_type.ta_rules.rl_methods in
        if not (meth_check rlm "write" 
                          (match rhs with
                           | PI_obj (opl,ot) when (is_value ot) ->
                             [new_arg_desc Arg_rhs]
                           | _ -> 
                             let _,dt = expr_type rhs false in 
                             [new_arg_desc  Arg_rhs ])
                           ) then
            error 0 (sprintf "No write method found for object <%s> in assignment or type incompatibility."
                      ao.ao_name);
        info (sprintf "Found %s.write method in assignment."
                      ao.ao_name);      
        
        (*
        ** Write operation  
        **  adto <- val; 
        **  <=>
        **  adto.write {val};
        *)
        [PI_fun(source (),(opl,OT_object ao),"write",[rhs])];
    | _ -> [pi];
  end
  else
  begin
    match rhs with
    | PI_obj (opl,OT_object ao) ->
        let rlm = ao.ao_type.ta_rules.rl_methods in

        if not (meth_check rlm "read" (let _,dt = expr_type lhs false in [new_arg_desc Arg_lhs])) then
            error 0 (sprintf "No read method found for object <%s> in assignment or type incompatibility."
                      ao.ao_name);
        info (sprintf "Found %s.read method in assignment."
                      ao.ao_name);      
        
        (*
        ** Write operation  
        **  val <- adto; 
        **  <=>
        **  adto.read {val};
        *)
        [PI_fun(source (),(opl,OT_object ao),"read",[lhs])];
    | _ -> [pi];
  end;
  | _ -> [pi]

(*
** Transform structure access:
**
**  s1 <- s2; => s1.e1 <- s2.e2; ....
**
*)
  
let assign_struct_transform pro pi =
  let is_obj pi = 
    match pi with
    | PI_obj (opl,ot) -> true
    | _ -> false in
    
  let pi_obj pi =
    match pi with
    | PI_obj (opl,ot) ->
      opl,ot;
    | _ -> progerr "pi_obj" in
    
  if debug_it "assign_struct_transform" then info "assign_struct_transform";

  match pi with
  | PI_assign (src,lhs,rhs) ->
  begin
    line src;
    match lhs with
    | PI_obj (opl,ot) ->
    begin
      match rhs with
      | PI_obj (opl',ot') ->
      begin
        match ot with 
        | OT_struct st ->
        begin
          match ot' with
          | OT_struct st' ->
          begin
            if st.st_type.ts_name <> st'.st_type.ts_name then
              error 0 (sprintf "Assignment with different structure types found.");
            List.map2 (fun l r ->
                PI_assign (src,PI_obj(opl,l),PI_obj(opl',r));
                ) st.st_objs st'.st_objs;
          end;
          | OT_array at' ->
          begin
            match at'.at_objs.(0) with
            | OT_struct st' ->
            begin
              if st.st_type.ts_name <> st'.st_type.ts_name then
                error 0 (sprintf "Assignment with different structure types found.");
              List.map2 (fun l r ->
                  PI_assign (src,PI_obj(opl,l),PI_obj(opl',r));
                  ) st.st_objs st'.st_objs;              
            end;
            | _ -> [pi];
          end;
          | _ -> error 0 (sprintf "Unexpected structure assignment with unexpected object on RHS found.");
        end;
        
        | OT_array at -> 
        begin
          match at.at_objs.(0) with
          | OT_struct st ->
          begin
            match ot' with
            | OT_struct st' ->
            begin
              if st.st_type.ts_name <> st'.st_type.ts_name then
                error 0 (sprintf "Assignment with different structure types found.");
              List.map2 (fun l r ->
                  PI_assign (src,PI_obj(opl,l),PI_obj(opl',r));
                  ) st.st_objs st'.st_objs;
            end;
            | _-> [pi];
          end;
          | _ -> [pi]
        end;
        | _ -> [pi];
      end;
      | _ -> [pi];
    end;
    | _ -> [pi];  
  end;
  | _ -> [pi]
  
let rec apply_struct_transform pro pi =
  let rec instrl_map f li =
    match li with
    | hd::tl ->
        let li' = f hd in
        li' @ (instrl_map f tl);
    | [] -> [] in

  let apply_and_block pi =
    let instrl = apply_struct_transform pro pi in
    match instrl with
    | pi'::[] -> pi';
    | pi'::aux ->
        PI_block (instrl,nilbf);
    | [] -> error 973668 "" in

  match pi with
  | PI_assign (src,_,_) -> 
    line src;
    assign_struct_transform pro pi
  | PI_branch (src,expr,b1,b2) ->
  begin
    line src;
    let b1' = apply_and_block b1 in
    let b2' = apply_and_block b2 in
    [PI_branch (src,expr,b1',b2')]; 
  end;
  | PI_select (src,expr,csl) ->
  begin
    line src;
    let csl' = List.hd (apply_struct_transform pro csl) in
    [PI_select (src,expr,csl')]; 
  end; 
  | PI_case (src,exprl,bl) ->
  begin
    line src;
    let bl' = apply_and_block bl in
    [PI_case (src,exprl,bl')];
  end;

  | PI_block (il,pl) ->
    [PI_block (instrl_map (apply_struct_transform pro) il,
              pl)];
  | PI_list il -> 
    [PI_list (instrl_map (apply_struct_transform pro) il)];
  | PI_forloop (src,expr,dir,lim1,lim2,bl) ->
  begin
    line src;
    let bl' = apply_and_block bl in
    [PI_forloop (src,expr,dir,lim1,lim2,bl')];  
  end;
  | PI_loop (src,kind,expr,bl) ->
  begin
    line src;
    let bl' = apply_and_block bl in
    [PI_loop (src,kind,expr,bl')];  
  end;
  | PI_try (i1,i2) ->
    [PI_try (apply_and_block i1,
            apply_and_block i2)]
  | _ -> [pi]



(*
** Transformation:
**
**      <ADTO> = <VAL> ->
**
**      <ADTO>.cmp_<op> ADTO_bool <VAL>;
**      ADTO_bool = true
*)

let rec compare_method_transform pro pi =
  let sel_of_op n op =
    match op with
    | OP_eq -> sprintf "cmp%s_eq" n;
    | OP_neq -> sprintf "cmp%s_neq" n;
    | _ -> error 397293 "" in

  let pi_obj pi =
    match pi with
    | PI_obj (opl,ot) ->
      opl,ot;
    | _ -> error 204207 "" in
  let ao_of_ot ot =
    match ot with
    | OT_array at -> 
      (*
      ** Array itself is ADTO!
      *)
      at.at_ao;
    | OT_object ao -> ao;
    | _ -> error 335402 "" in
  let is_ao ot =
    match ot with
    | OT_array _ 
    | OT_object _ -> true;
    | _ -> false in
  let is_pi_ao pi =
    match pi with
    | PI_obj (opl,ot) -> 
        not (is_sel opl) &&
        not (is_sel_obj opl) && 
        (is_ao ot);
    | _ -> false in

  let pi_ao_name pi =
    match pi with
    | PI_obj (_,ot) ->
        let ao = ao_of_ot ot in 
        ao.ao_name
    | _ -> "" in

  let pi_ao pi =
    match pi with
    | PI_obj (_,ot) ->
        let ao = ao_of_ot ot in 
        ao
    | _ -> error 742278 "" in
   
  let is_pi_sub pi =
    match pi with
    | PI_obj (opl,_) ->
      is_sub opl;
    | _ -> false in
      

  let adto_bool () =
        match pro with
        | Some pro ->
            if sym_check_obj pro.pro_objs "ADTO_bool" then
                sym_get_obj pro.pro_objs "ADTO_bool"
            else
            begin
                let obj = 
                  let co = new_obj "ADTO_bool" (DT_logic 1) in
                  co.co_flags <- [Obj_local];
                  OT_reg co in
                assign_rule obj;
                sym_add  pro.pro_objs (Sym_obj obj);
                obj;
            end;
        | None -> error 0 
                    (sprintf "\nAbstract object in relational expression without process.");
        in        

  match pi with
  | PI_bool (kind,op,op1,op2) ->
  begin
   match kind with
   | Relational ->
    if is_pi_ao op1 then
    begin
      if not (is_pi_sub op1) && not (is_pi_sub op2) then
      begin
        let op1' = PI_obj ([],adto_bool ()) in
        let op2' = PI_obj ([],OT_value (V_int Int64.one)) in
        let op' = OP_eq in
        let sel = sel_of_op "" op in
        info (sprintf "Found %s.%s method call in expression."
                      (pi_ao_name op1) sel);
        let aux =
            let ao = pi_ao op1 in
            let args = [op1';op2] in
            let  pi = 
                PI_fun (source (),([],OT_object ao),
                               sel,
                               args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi an.a_toplevel;
            [pi] in

        [PI_bool (kind,op',op1',op2')]@aux;
      end
      else
      begin
        let op1' = PI_obj ([],adto_bool ()) in
        let op2' = PI_obj ([],OT_value (V_int Int64.one)) in
        let op' = OP_eq in
        let sel = sel_of_op "n" op in

        let a1,b1 =
          match op1 with
          | PI_obj (opl,_) -> obj_sub opl;
          | _ -> error 782919 "" in

        let a2,b2 =
          match op2 with
          | PI_obj (opl,_) -> 
                if is_sub opl then obj_sub opl else 0,0;
          | _ -> 0,0 in

        info (sprintf "Found %s.%s method call in expression op1[%d to %d],op2[%d to %d]."
                      (pi_ao_name op1) sel
                      a1 b1 a2 b2);

        let ind = [
                PI_obj([],OT_value (V_int (Int64.of_int a1)));
                PI_obj([],OT_value (V_int (Int64.of_int a2)));
                PI_obj([],OT_value (V_int (Int64.of_int (b1-a1+1))));
              ] in
        let aux =
            let ao = pi_ao op1 in
            let args = [op1'] @ ind @ [op2] in
            let pi = PI_fun (source (),([],OT_object ao),
                               sel,
                               args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi an.a_toplevel;
            [pi] in        
        [PI_bool (kind,op',op1',op2')]@aux;
      end;
    end
    else if is_pi_ao op2 then
    begin
      if not (is_pi_sub op1) && not (is_pi_sub op2) then
      begin
        let op1' = PI_obj ([],adto_bool ()) in
        let op2' = PI_obj ([],OT_value (V_int Int64.one)) in
        let op' = OP_eq in
        let sel = sel_of_op "" op in
        info (sprintf "Found %s.%s method call in expression."
                      (pi_ao_name op2) sel);
        let aux =
            let ao = pi_ao op2 in
            let args = [op1';op1] in
            let pi = PI_fun (source (),([],OT_object ao),
                               sel,
                               args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi an.a_toplevel;
            [pi] in            
        [PI_bool (kind,op',op1',op2')]@aux;
      end
      else
      begin
        let op1' = PI_obj ([],adto_bool ()) in
        let op2' = PI_obj ([],OT_value (V_int Int64.one)) in
        let op' = OP_eq in
        let sel = sel_of_op "n" op in
        let a2,b2 =
          match op2 with
          | PI_obj (opl,_) -> obj_sub opl;
          | _ -> error 781920 "" in

        let a1,b1 =
          match op1 with
          | PI_obj (opl,_) -> 
                if is_sub opl then obj_sub opl else 0,0;
          | _ -> 0,0 in

        info (sprintf "Found %s.%s method call in expression op1[%d to %d],op2[%d to %d]."
                      (pi_ao_name op2) sel
                      a1 b1 a2 b2);

        let ind = [
                PI_obj([],OT_value (V_int (Int64.of_int a1)));
                PI_obj([],OT_value (V_int (Int64.of_int a2)));
                PI_obj([],OT_value (V_int (Int64.of_int (b1-a1+1))));
              ] in
        let aux =
            let ao = pi_ao op2 in
            let args = [op1'] @ ind @ [op1] in
            let pi = PI_fun (source (),([],OT_object ao),
                               sel,
                               args) in
            ao.ao_type.ta_rules.rl_fun_compile an.a_modu 
                                               an.a_pro
                                               pi an.a_toplevel;
            [pi] in
        [PI_bool (kind,op',op1',op2')]@aux;
      end
    end
    else [pi];
   | Bool ->
    let op1l' = compare_method_transform pro op1 in
    let op2l' = compare_method_transform pro op2 in
    let op1',aux1 = List.hd op1l',List.tl op1l' in
    let op2',aux2 = List.hd op2l',List.tl op2l' in
    
    [PI_bool (kind,op,op1',op2')]@aux1@aux2
  end;
  | _ -> [pi]

(*
** Do ADTO method transformations extracted from instructions.
*)

let rec apply_method_transform pro pi =
  let rec instrl_map f li =
    match li with
    | hd::tl ->
        let li' = f hd in
        li' @ (instrl_map f tl);
    | [] -> [] in

  let apply_and_block pi =
    let instrl = apply_method_transform pro pi in
    match instrl with
    | pi'::[] -> pi';
    | pi'::aux ->
        PI_block (instrl,nilbf);
    | [] -> error 973668 "" in

  match pi with
  | PI_assign (src,_,_) -> 
    line src;
    assign_method_transform pro pi
  | PI_branch (src,expr,b1,b2) ->
  begin
    line src;
    let b1' = apply_and_block b1 in
    let b2' = apply_and_block b2 in
    match compare_method_transform pro expr with
    | expr'::aux ->
        aux @ 
            [PI_branch (src,expr',b1',b2')]; 
    | [] -> []
  end;
  | PI_select (src,expr,csl) ->
  begin
    line src;
    let csl' = List.hd (apply_method_transform pro csl) in
    match compare_method_transform pro expr with
    | expr'::aux ->
        aux @ 
            [PI_select (src,expr',csl')];  
    | [] -> []
  end; 
  | PI_case (src,exprl,bl) ->
  begin
    line src;
    let bl' = apply_and_block bl in
    [PI_case (src,exprl,bl')];
  end;

  | PI_block (il,pl) ->
    [PI_block (instrl_map (apply_method_transform pro) il,
              pl)];
  | PI_list il -> 
    [PI_list (instrl_map (apply_method_transform pro) il)];
  | PI_forloop (src,expr,dir,lim1,lim2,bl) ->
  begin
    line src;
    let bl' = apply_and_block bl in
    match compare_method_transform pro expr with
    | expr'::aux ->
        aux @ 
            [PI_forloop (src,expr',dir,lim1,lim2,bl')];  
    | [] -> []
  end;
  | PI_loop (src,kind,expr,bl) ->
  begin
    line src;
    let bl' = apply_and_block bl in
    match compare_method_transform pro expr with
    | expr'::aux ->
        aux @ 
            [PI_loop (src,kind,expr',bl')];  
    | [] -> []
  end;
  | PI_try (i1,i2) ->
    let b1' = apply_and_block i1 in
    let b2' = apply_and_block i2 in
    [PI_try (b1',b2')]; 
  | _ -> [pi]

