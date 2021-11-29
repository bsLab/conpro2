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
**    $CREATED:     19.3.2006
**    $VERSION:     2.14
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
open Cp_block_frame
open Cp_printtypes
open Cp_symbol

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

  let next_is_jump li =
      match li with
      | ui::_ ->
      begin
          match ui.ui_code with
          | Jump _ -> true;
          | _ -> false;
      end;
      | _ -> false in

  let get_next_jump_label li =
      match li with
      | ui::_ ->
      begin
          match ui.ui_code with
          | Jump l -> l;
          | _ -> error 688700 ""
      end;
      | _ -> error 688701 "" in

  let next_is_label li =
    match li with
    | ui::_ ->
    begin
        match ui.ui_code with
        | Label _ -> true;
        | _ -> false;
    end;
    | _ -> false in

  let next_is_end li =
    match li with
    | ui::_ ->
    begin
        match ui.ui_code with
        | Label l -> l = "%END";
        | _ -> false;
    end;
    | [] -> true in

  let rec search_label li =
    match li with
    | hd::tl ->
    begin
      match hd.ui_code with
      | Label label -> 
          if not (next_is_jump tl) && not (next_is_label tl) then
              UC_label label
          else if next_is_jump tl then
              get_next_jump_label tl
          else
              search_label tl;
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
                  | Fun _ -> true;
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
      let l = {
        ui_code=Label (sprintf "i%d_bind_to_%d_end" id1 id2);
        ui_frame=List.hd pro.pro_frame;
      } in
      let j = {
          ui_code = Jump UC_next;
          ui_frame=List.hd pro.pro_frame;
      } in
      [l;j]
      in
    let ul = bind ucll in
    let bind_instr = 
      {
        ui_code=Bind (List.length ul);
        ui_frame=List.hd pro.pro_frame;
      } in
    [label_start;bind_instr] @ ul @ label_end;
    in

  incr id;
  let id1 = !id in
  let label name src = 
      {
          ui_code = Label (sprintf "i%d_%s" id1 name);
          ui_frame=List.hd pro.pro_frame;
      } in
  let labelo name = 
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
  | PI_assign (src,lhs,rhs) ->
      line src;
      let _,expr_dt = expr_type instr false in
      let dst = 
          match lhs with
          | PI_obj (opl,ot) -> 
              let flags =  if is_ot_local (Some pro) ot 
                            then [UO_lhs;UO_loc]  
                            else [UO_lhs] in
              ud_of_ot ot opl flags expr_dt;
          | _ -> error 463517 "";
          in
      debug "instr_ucode" with (sprintf "lhs=%s expr_dt=%s" 
                                    (name_of_pi lhs)
                                    (sprint_dt expr_dt));

      let dst_tmp_req,dst_range =
        match dst with
        | UC_var uo ->
          (*
          ** Special case!!
          *)
          uo.uo_range <> None,uo.uo_range 
        | _ -> false,None in

      let res,expr = expr_synth pro rhs (sprintf "i%d_assign" id1) (Some expr_dt) in

      debug "instr_ucode" with (sprintf "res=%s" (ui_sprint_uc res));

      let gd_dst_rd,gd_dst_wr = ud_guard dst in
      let gd_res_rd,gd_res_wr = ud_guard res in

      let frag_dst = ud_frag dst in
      let frag_src = 
          match rhs with
          | PI_obj _ ->
              pi_frag rhs;
          | _ -> false in

      if dst_tmp_req then
      begin
        (*
        ** Only for type logic!
        **
        ** VAR[sub] <- <expr> =>
        ** TEMP <- VAR;
        ** TEMP[sub] <- <expr>;
        ** VAR <- TEMP;
        *)
        let co = 
            match co_of_ud dst with
            | Some co -> co;
            | None -> error 0 (sprintf "Can't derive core object <%s> in expression." 
                                       (name_of_ud res)); in

        let dst_dt' = co.co_type in 
        let tempx =
          let name = co.co_name in
          {
            ut_type = uo_type dst_dt' expr_dt;
            ut_range = None;
            ut_name = name;
            ut_frame = Some (UC_label (sprintf "i%d_assign" !id ));
            ut_obj = None;
            ut_flags = [UO_lhs;UO_loc];
          } in
        let tempx' = {tempx with ut_range=dst_range;
                                 ut_type = {tempx.ut_type with
                                      uo_data_type=expr_dt;
                                      uo_expr_type=expr_dt;}} in
        let dst' =
          match dst with
          | UC_var uo -> 
            UC_var {uo with uo_range=None;
                            uo_type = {uo.uo_type with
                                          uo_expr_type=dst_dt'}};
          | _ -> error 251643 "" in
        let dst'' =
          match dst with
          | UC_var uo -> 
            UC_var {uo with uo_range=None;
                            uo_type = {uo.uo_type with
                                          uo_expr_type=dst_dt';
                                          uo_conv=None;}};
          | _ -> error 251643 "" in
        let temp1 = {
              ui_code = Move (UC_temp tempx,ud_fix_rhs dst'');
              ui_frame=List.hd pro.pro_frame;
            } in
        let temp2 = {
              ui_code = Move (UC_temp tempx',res);
              ui_frame=List.hd pro.pro_frame;
            } in
        let move = {
              ui_code = Move (dst',
                          UC_temp {tempx with ut_flags=[UO_rhs;UO_loc]});
              ui_frame=List.hd pro.pro_frame;
        } in
        [label "assign" src] @ [temp1] @ 
                 expr @ [temp2;move] @ (next "assign");
      end
      else if gd_dst_wr && gd_res_rd then
      begin
        (*
        ** Simple assignment:
        ** REG <- VAR
        ** VAR <- VAR
        *)
        let tempx,tempx_dt =
          let co = 
            match co_of_ud res with
            | Some co -> co;
            | None -> error 0 (sprintf "Can't derive core object <%s> in expression." 
                                       (name_of_ud res)); in

          let dt = co.co_type in


          let name = co.co_name in
          {
            ut_type = uo_type dt dt;
            ut_range = None;
            ut_name = name;
            ut_frame = Some (UC_label (sprintf "i%d_assign" !id ));
            ut_obj = None;
            ut_flags = [UO_lhs;UO_loc];
          },dt in

        let res' =
            match res with
            | UC_var uo -> 
              UC_var {uo with 
                            uo_type = {uo.uo_type with
                                          uo_expr_type=tempx_dt;
                                          uo_conv = None;
                                      }};
            | _ -> error 251644 "" in
        if not frag_src then
        begin  
            let temp = {
              ui_code = Move (UC_temp tempx,res');
              ui_frame=List.hd pro.pro_frame;
            } in
            let move = {
              ui_code = Move (dst,
                          UC_temp {tempx with ut_flags=[UO_rhs;UO_loc];
                                              ut_type={tempx.ut_type with
                                                  uo_expr_type=expr_dt}});
              ui_frame=List.hd pro.pro_frame;
            } in
            [label "assign" src] @ expr @ 
                    [temp;move] @ (next "assign");
        end
        else
        begin
            let temps = frag_src_split pro (UC_temp tempx) 
                                       res in
            if not frag_dst then  
            begin
              let move = {
                ui_code = Move (dst,
                          UC_temp {tempx with ut_flags=[UO_rhs;UO_loc];
                                              ut_type={tempx.ut_type with
                                                  uo_expr_type=expr_dt}});
                ui_frame=List.hd pro.pro_frame;
              } in
              [label "assign" src] @ expr @ 
                      temps @ [move] @ (next "assign");
            end
            else
            begin
                let moves = frag_dst_split pro dst 
                                           (UC_temp {tempx with
                                                     ut_flags=[UO_rhs;UO_loc];
                                                     ut_type={tempx.ut_type with
                                                        uo_expr_type=expr_dt}})
                                           in
                [label "assign" src] @ expr @ 
                       temps @ moves @ (next "assign");
            end;
        end
      end
      else if frag_dst then
      begin
        (*
        ** VAR <- EXPR[not read guarded]
        *)
        debug "instr_ucode" with "assign: Fragmented destination object";

        let tempx =
          let co = 
            match co_of_ud dst with
            | Some co -> co
            | None -> error 0 (sprintf "Can't derive core object <%s> in expression." 
                                       (name_of_ud res)); in
          let dt = co.co_type in
          let name = co.co_name in
          {
            ut_type = uo_type dt expr_dt;
            ut_range = None;
            ut_name = name;
            ut_frame = Some (UC_label (sprintf "i%d_assign" !id ));
            ut_obj = None;
            ut_flags = [UO_lhs;UO_loc];
          } in

        let temp = {
          ui_code = Move (UC_temp tempx,res);
          ui_frame=List.hd pro.pro_frame;
        } in
        let umove = frag_dst_split pro dst 
                                   (UC_temp {tempx with
                                             ut_flags=[UO_rhs;UO_loc]})
                                   in
        [label "assign" src] @ expr @ [temp] @ 
                umove @ (next "assign");
      end
      else if frag_src then
      begin
        (*
        ** REG[not write guarded]<-VAR
        *)
        let moves = frag_src_split pro dst
                                   res in
        [label "assign" src] @ moves @ (next "assign");
      end
      else
      begin
        let move = {
          ui_code = Move (dst,ud_fix_rhs res);
          ui_frame=List.hd pro.pro_frame;
        } in
        debug "instr_ucode" with (ui_sprint_instr move);
        [label "assign" src] @ expr @ [move] @ (next "assign");
      end;

  | PI_branch (src,cond,s1,s2) ->
      line src;
      let res,expr = expr_synth pro cond (sprintf "i%d_branch" id1) 
                                None in
      let label_end = sprintf "i%d_branch_end" id1 in
      let falsejump = {
          ui_code = Falsejump (res,UC_label label_end);
          ui_frame=List.hd pro.pro_frame;
        } in
      let exit_label = UC_label label_end in
      (*
      ** Synthesize TRUE and FALSE instruction(s) (blocks)
      *)
      let sl1 = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> if l = UC_next then
                                      {uc with ui_code=
                                            Falsejump (uo,exit_label)}
                                   else uc;
                  | Jump l -> if l = UC_next then  
                                      {uc with ui_code=
                                            Jump exit_label}
                                   else uc;
                  | _ -> uc;
            ) ((get_mod instr).rl_instr_ucode
                                s1
                                id  
                                pro) in
      let sl2 = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> if l = UC_next then
                                      {uc with ui_code=
                                            Falsejump (uo,exit_label)}
                                   else uc;
                  | Jump l -> if l = UC_next then  
                                      {uc with ui_code=
                                            Jump exit_label}
                                   else uc;
                  | _ -> uc;
            ) ((get_mod instr).rl_instr_ucode s2 id pro) in
      if s2 <> PI_nop then
      begin
        (*
        ** Fix Falsejmp target
        *)
        let s2_next = search_label sl2 in
        falsejump.ui_code <- Falsejump (res,s2_next);
      end;
      [label "branch" src] @ expr @ [falsejump] @ sl1 @ sl2 @ (next "branch");

  | PI_raise ex ->
      let label_raise = sprintf "i%d_raise" id1 in
      let label_raise' = UC_label label_raise in
      let exn = sprintf "PRO_%s_EXCEPTION" pro.pro_name in
      let r_ex = sym_get_obj modu.mod_objs exn  in
      let r_ex' = PI_obj ([],r_ex) in
      let expr = PI_assign (nilsrc (),r_ex',PI_obj([],OT_value (V_int (Int64.of_int ex)))) in
      let expr' = if ex <> 0 then remove_label_and_jump (instr_ucode expr id pro) 
                  else [] (* re-raise, r_ex already keeps exception *) in
      let jump = {
              ui_code = Jump (UC_label exn);
              ui_frame=List.hd pro.pro_frame;
            } in
      let bind = {
          ui_code = Bind 2;
          ui_frame=List.hd pro.pro_frame;
          } in
      [label "raise" (nilsrc())] @ [bind] @ expr' @ [jump] @ (next "raise");
      
  | PI_try (block,csl) ->
      let bf =
        match block with
        | PI_block (_,bf) -> bf;
        | _ -> nilbf in
      let label_try = sprintf "i%d_try_end" id1 in
      let label_try' = UC_label label_try in
      let exn = sprintf "PRO_%s_EXCEPTION" pro.pro_name in
      let r_ex = sym_get_obj modu.mod_objs exn  in
      let r_ex' = PI_obj ([],r_ex) in
      let expr = PI_assign (nilsrc (),r_ex',PI_obj([],OT_value (V_int (Int64.of_int 0)))) in
      let has_others = 
        match csl with
        | PI_block (il,bf) ->
        begin
          (List.filter (fun cs ->
            match cs with
            | PI_block ([PI_case (src,exprl,block)],bf') ->
            begin
              exprl = []
            end;
            | _ -> error 0 "Invalid case in try-with found.";
            ) il) <> [];
        end;
        | _ -> error 0 "Invalid try-with case list found." in
      let complete = ref true in
      let rec find_catch ctl = 
        match ctl with
        | [ct] -> 
          if ct.catch_env = bf 
            then ct
            else find_catch ct.catch_ctl.pro_catch
        | ct :: tl -> 
          if ct.catch_env = bf 
            then ct
            else find_catch tl 
        | [] -> error 0 (sprintf "Can't find catch environment.") in
      let catch = find_catch pro.pro_control.pro_catch in
      
      List.iter (fun pro ->
        if pro.pro_control.pro_raise <> [] then complete := false;
        ) catch.catch_ctl.pro_call;
       
      List.iter (fun ex ->
          if not (List.mem ex catch.catch_exl) then complete := false;
        ) catch.catch_ctl.pro_raise;
        
      let csl' = 
        match csl with
        | PI_block (il,bf) ->
        begin
          PI_block ((List.map (fun cs ->
            match cs with
            | PI_block ([PI_case (src,exprl,block)],bf') ->
            begin
              PI_block ([PI_case (src,exprl,(
                match block with
                | PI_block (il'',bf'') -> PI_block (il''@[expr],bf'');
                | _ -> 
                  let il'' = [block;expr] in
                  let bf'' = create_frame il in
                  PI_block (il'',bf'');
                ))],bf');
            end;
            | _ -> error 0 "Invalid case in try-with found.";
            ) il) @
            (if not has_others && not !complete then
             begin
               [PI_block ([PI_case (nilsrc(),[],
                  (
                    let il'' = [PI_raise 0] in
                    let bf'' = create_frame il in
                    PI_block (il'',bf'');
                  ))],bf)]
             end
             else [])
            ,bf);
        end;
        | _ -> error 0 "Invalid try-with case list found." in
      let try_with = PI_select (nilsrc (),r_ex',csl') in
      let try_with_csl = instr_ucode try_with id pro in
      let label_with' = search_label try_with_csl in
      let sl = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> 
                    if l = UC_next then
                      {uc with ui_code = Falsejump (uo,label_try')}
                    else if l = (UC_label exn) then
                      {uc with ui_code = Falsejump (uo,label_with')}
                    else
                      uc;
                  | Jump l -> 
                    if l = UC_next then  
                      {uc with ui_code = Jump label_try'}
                    else if l = (UC_label exn) then
                      {uc with ui_code = Jump label_with'}
                    else
                      uc;
                  | _ -> uc;
            ) ((get_mod instr).rl_instr_ucode block id pro) in
       let nop = {
              ui_code = Nop;
              ui_frame=List.hd pro.pro_frame;
            } in

      [label "try" (nilsrc())] @ [nop] @ sl @ try_with_csl @ (next "try");

  | PI_forloop (src,index,dir,expr1,expr2,block) ->
      (*
      **  For loop boundaries: 
      **    index = expr1 TO expr2
      **    index = expr1 DOWNTO expr2
      *)
      line src;
      let bpl = get_block_params block in
      let inline = List.mem BP_inline bpl in 

      let label_end = sprintf "i%d_for_loop_end" id1 in
      let exit_label = UC_label label_end in
      let label_cond = sprintf "i%d_for_loop_cond" id1 in
      let label_loop = sprintf "i%d_for_loop_incr" id1 in
      let label_loop' = UC_label label_loop in
      let label_cond' = UC_label label_cond in

      let expr_init = PI_assign (src,index,expr1) in
      decr id;
      let initl = remove_label_and_jump (instr_ucode expr_init id pro) in
      let expr_cond = PI_bool (Cp_syntax.Relational,
                               (if dir = '+' then OP_ge else OP_le),
                               expr2,
                               index) in
      let res,condl = expr_synth pro expr_cond (sprintf "i%d_forloop" id1) None in
      let falsejump = {
              ui_code = Falsejump (res,UC_label label_end);
              ui_frame=List.hd pro.pro_frame;
            } in
      let sl = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> 
                    if l = UC_next then
                      {uc with ui_code = Falsejump (uo,label_loop')}
                    else uc;
                  | Jump l -> 
                    if l = UC_next then  
                      {uc with ui_code = Jump label_loop'}
                    else uc;
                  | _ -> uc;
            ) ((get_mod instr).rl_instr_ucode block id pro) in

      let jump = {
              ui_code = Jump label_cond';
              ui_frame=List.hd pro.pro_frame;
            } in
      let expr_incr = PI_assign(src,index,
                                PI_arithm((if dir = '+' then OP_add else OP_sub),
                                          index,
                                          PI_obj ([],OT_value (V_int (i64 1))))) in
      decr id;
      let incrl = remove_label_and_jump (instr_ucode expr_incr id pro) in
      let incrl =
          (*
          ** Bind last three instructions: EXPR + MOVE + JUMP
          *)
          match List.rev (incrl @ [jump]) with
          | j::(m::(e::tl)) ->
              let b = {
                  ui_code = Bind 3;
                  ui_frame=List.hd pro.pro_frame;
                  } in
              (List.rev tl)@[b;e;m;j];
          | _ -> error 587975 "";
          in

      [label "for_loop" src] @ initl @ 
              [label "for_loop_cond" src] @ condl @
              [falsejump] @
              sl @ 
              [label "for_loop_incr" src] @
              incrl @
              (next "for_loop");

  | PI_loop (src,kind,expr,block) ->
  begin
    line src;
    match kind with
    | Loop_while ->
      let loop_name = 
        match kind with
        | Loop_while -> "while";
        | Loop -> "loop";
        in

      let bpl = get_block_params block in
      let inline = List.mem BP_inline bpl in 

      let label_end = sprintf "i%d_%s_loop_end" id1 loop_name in
      let exit_label = UC_label label_end in
      let label_cond = sprintf "i%d_%s_loop" id1 loop_name  in
      let label' = UC_label label_cond in

      let expr_cond = expr in
      let res,condl = expr_synth pro expr_cond 
                                (sprintf "i%d_%sloop" 
                                         id1 loop_name)
                                None in
      let falsejump = {
              ui_code = Falsejump (res,UC_label label_end);
              ui_frame=List.hd pro.pro_frame;
            } in
      let sl = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> if l = UC_next then
                                      {uc with ui_code=
                                            Falsejump (uo,label')}
                                   else uc;
                  | Jump l -> if l = UC_next then  
                                      {uc with ui_code=
                                            Jump label'}
                                   else uc;
                  | _ -> uc;
            ) ((get_mod instr).rl_instr_ucode
                                block
                                id  
                                pro) in

      if sl = [] then error 0 "Conditional loop with empty instruction block found!";

      [label (sprintf "%s_loop" loop_name) src] @ condl @
              [falsejump] @
              sl @ 
              (next (sprintf "%s_loop" loop_name));
    | Loop ->
      let sl = (get_mod instr).rl_instr_ucode
                                block
                                id  
                                pro in

      if sl = [] then error 0 "Unconditional loop with empty instruction block found!";
      let label_start = 
          match (List.hd sl).ui_code with
          | Label str -> UC_label str;
          | _ -> error 566161 ""; in
      let label_end = 
          match (label "loop" src).ui_code with
          | Label str -> UC_label str;
          | _ -> error 566161 ""; in
      let sl' = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> if l = UC_next then
                                      {uc with ui_code=
                                            Falsejump (uo,label_end)}
                                   else uc;
                  | Jump l -> if l = UC_next then  
                                      {uc with ui_code=
                                            Jump label_end}
                                   else uc;
                  | _ -> uc;
            ) sl in

      let jump = {
          ui_code = Jump label_start;
          ui_frame=List.hd pro.pro_frame;
          } in
      [label "loop" src] @ [jump] @
              sl' @
              (next "loop");
  end;
  | PI_select (src,expr1,block) ->
      let case_num = ref 0 in

      pro.pro_constr <- (constr_of_block "select" 
                              [BP_expr EXPR_flat;BP_bind]
                              pro.pro_constr) :: pro.pro_constr;
      line src;

      let is_obj =
          match expr1 with
          | PI_obj _ -> true;
          | _ -> false in
      let is_expr = not is_obj in
      let op1,expr_aux =
          if is_expr then
          begin
            let _,expr_dt = expr_type expr1 false in
            let temp_co = new_tmp pro expr_dt in
            temp_co.co_rules <- !core_rules;
            let temp = PI_obj ([],OT_reg temp_co) in
            let expr' = PI_assign(src,temp,expr1) in
            temp,[expr']
          end
          else
              expr1,[] in
      let csl =
          match block with
          | PI_block (csl,bf) -> csl;
          | _ -> error 399694 ""; in

      let jumps = ref [] in
      let blocks = ref [] in
      let label_end = sprintf "i%d_select_end" id1 in
      let exit_label = UC_label label_end in
      let label_select = sprintf "i%d_select" id1 in


      let n = ref (List.length csl) in
      let tmp_move = ref [] in
      List.iter (fun pi_expr -> tmp_move := !tmp_move @
                      (remove_label_and_jump (instr_ucode pi_expr id pro))) expr_aux;

      let tmp = ref None in

      (*
      ** Sort case list; move default case if any to the end of the list.
      *)
      let csl = List.sort (fun a b ->
              let is_def case =
                match case with
                | PI_case (_,[],_) -> true;
                | _ -> false in           
              if (is_def a) then 1 else 0) csl in     

      List.iter (fun case ->
        decr n;
        match case with
        | PI_block ([PI_case (src,expr2l,il)],bf) ->
        line src;
        pro.pro_frame <- bf :: pro.pro_frame;
        begin
         match expr2l with
         | [PI_list [expr_a;expr_b]] ->
          let sl = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> if l = UC_next then
                                      {uc with ui_code=
                                            Falsejump (uo,exit_label)}
                                   else uc;
                  | Jump l -> if l = UC_next then  
                                      {uc with ui_code=
                                            Jump exit_label}
                                   else uc;
                  | _ -> uc;
            ) ((get_mod instr).rl_instr_ucode
                                il
                                id  
                                pro) in

          if sl = [] then
          begin
            pi_print_instr_list (sprintf "%s" pro.pro_name) pro.pro_instr;
            error 0 (sprintf "Found empty ucode instruction list in multi-case of match environment!");
          end;

          let label_code =
              match (List.hd sl).ui_code with
              | Label str -> str;
              | _ -> error 976683 ""; in
          let expr_a' = PI_bool (Cp_syntax.Relational,OP_gt,op1,expr_a) in
          incr case_num;

          let label_case = sprintf "case_%d" !case_num in
          let label_case' = sprintf "i%d_%s" id1 label_case in
          let label_case_next = sprintf "case_%d" (!case_num+1) in
          let label_case_next' = sprintf "i%d_%s" id1 label_case_next in
          let res_a,ucl_a = expr_synth pro expr_a' label_case' None in

          let ucl_fix ucl' =
              (*
              ** Extract temporary register transfer...
              ** Change temporary register to the one of the
              ** first case...
              *)
              let fix_tmp ucl'' =
                List.map (fun ui ->
                  match ui.ui_code with
                  | Expr (op,dst,op1,op2) ->
                  begin
                      match op1 with
                      | UC_temp ut -> 
                        {ui with ui_code=Expr (op,dst,(get_some !tmp),op2)};
                      | _ -> 
                      begin
                        match op2 with
                        | UC_temp ut ->
                          {ui with ui_code=Expr (op,dst,op1,(get_some !tmp))};
                        | _ -> ui;
                      end;
                  end;
                  | _ -> ui;  
                  ) ucl'' in

              match ucl' with
              | hd::tl ->
              begin
                  match hd.ui_code with
                  | Move (dst,src) ->
                  begin
                      match dst with
                      | UC_temp ut ->
                          ut.ut_frame <- Some (UC_label label_select);
                          if !tmp = None then
                          begin
                            tmp := Some dst;
                            tmp_move := [hd];
                          end;
                          fix_tmp tl;
                      | _ -> ucl';
                  end;
                  | _ -> ucl';                    
              end;
              | [] ->  ucl' in

          let ucl_a = ucl_fix ucl_a in

          let falsejump_a = {
              ui_code = Falsejump (res_a,
                              if !n = 0 then UC_label label_end
                                 else UC_label label_case_next');
              ui_frame=List.hd pro.pro_frame;
            } in
          let expr_b' = PI_bool (Cp_syntax.Relational,OP_lt,op1,expr_b) in
          let res_b,ucl_b = expr_synth pro expr_b' label_case'
                                       None in
          let ucl_b = ucl_fix ucl_b in

          let falsejump_b = {
              ui_code = Falsejump (res_a,
                              if !n = 0 then UC_label label_end
                                 else UC_label label_case_next');
              ui_frame=List.hd pro.pro_frame;
            } in

          let jump = {
              ui_code = Jump (UC_label label_code);
              ui_frame=List.hd pro.pro_frame;
            } in

          blocks := !blocks @ sl;
          jumps := !jumps @ [label label_case src] @ ucl_a @ 
                            [falsejump_a] @ ucl_b @ 
                            [falsejump_b;jump];


         | [expr2] ->
          let expr' = PI_bool (Cp_syntax.Relational,OP_eq,op1,expr2) in
          incr case_num;
          let label_case = sprintf "case_%d" !case_num in
          let label_case' = sprintf "i%d_%s" id1 label_case in
          let label_case_next = sprintf "case_%d" (!case_num+1) in
          let label_case_next' = sprintf "i%d_%s" id1 label_case_next in
          let res,ucl = expr_synth pro expr' label_case' None in
          let ucl_fix ucl' =
              (*
              ** Extract temporary register transfer...
              ** Change temporary register to the one of the
              ** first case...
              *)
              let fix_tmp ucl'' =
                List.map (fun ui ->
                  match ui.ui_code with
                  | Expr (op,dst,op1,op2) ->
                  begin
                      match op1 with
                      | UC_temp ut -> 
                        {ui with ui_code=Expr (op,dst,(get_some !tmp),op2)};
                      | _ -> 
                      begin
                        match op2 with
                        | UC_temp ut ->
                          {ui with ui_code=Expr (op,dst,op1,(get_some !tmp))};
                        | _ -> ui;
                      end;
                  end;
                  | _ -> ui;  
                  ) ucl'' in

              match ucl' with
              | hd::tl ->
              begin
                  match hd.ui_code with
                  | Move (dst,src) ->
                  begin
                      match dst with
                      | UC_temp ut ->
                          ut.ut_frame <- Some (UC_label label_select);
                          if !tmp = None then
                          begin
                            tmp := Some dst;
                            tmp_move := [hd];
                          end;
                          fix_tmp tl;
                      | _ -> ucl';
                  end;
                  | _ -> ucl';                    
              end;
              | [] ->  ucl' in

          let ucl' = ucl_fix ucl in

          let falsejump = {
              ui_code = Falsejump (res,
                              if !n = 0 then UC_label label_end
                                 else UC_label label_case_next');
              ui_frame=List.hd pro.pro_frame;
            } in
          let sl = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> if l = UC_next then
                                      {uc with ui_code=
                                            Falsejump (uo,exit_label)}
                                   else uc;
                  | Jump l -> if l = UC_next then  
                                      {uc with ui_code=
                                            Jump exit_label}
                                   else uc;
                  | _ -> uc;
            ) ((get_mod instr).rl_instr_ucode
                                il
                                id  
                                pro) in

          if sl = [] then
          begin
            pi_print_instr_list (sprintf "%s" pro.pro_name) pro.pro_instr;
            error 0 (sprintf "Found empty ucode instruction list in single case of match environment!");
          end;
          let label_code =
              match (List.hd sl).ui_code with
              | Label str -> str;
              | _ -> error 976681 ""; in
          let jump = {
              ui_code = Jump (UC_label label_code);
              ui_frame=List.hd pro.pro_frame;
            } in

          blocks := !blocks @ sl;
          jumps := !jumps @ [label label_case src] @ ucl' @ 
                            [falsejump;jump];
         | [] ->
          (*
          ** Default case.
          *)
          incr case_num;
          let label_case = sprintf "case_%d" !case_num in
          let label_case' = sprintf "i%d_%s" id1 label_case in
          let label_case_next = sprintf "case_%d" (!case_num+1) in
          let label_case_next' = sprintf "i%d_%s" id1 label_case_next in
          let sl = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> if l = UC_next then
                                      {uc with ui_code=
                                            Falsejump (uo,exit_label)}
                                   else uc;
                  | Jump l -> if l = UC_next then  
                                      {uc with ui_code=
                                            Jump exit_label}
                                   else uc;
                  | _ -> uc;
            ) ((get_mod instr).rl_instr_ucode
                                il
                                id  
                                pro) in
          if sl = [] then
          begin
            pi_print_instr_list (sprintf "%s" pro.pro_name) pro.pro_instr;
            error 0 (sprintf "Found empty ucode instruction list in default case of match environment!");
          end;
          let label_code =
              match (List.hd sl).ui_code with
              | Label str -> str;
              | _ -> error 976682 ""; in
          let jump = {
              ui_code = Jump (UC_label label_code);
              ui_frame=List.hd pro.pro_frame;
            } in

          blocks := !blocks @ sl;
          jumps := !jumps @ [label label_case src] @ [jump];

         | exprli ->
         let sl = List.map (fun uc ->
                  match uc.ui_code with
                  | Falsejump (uo,l) -> if l = UC_next then
                                      {uc with ui_code=
                                            Falsejump (uo,exit_label)}
                                   else uc;
                  | Jump l -> if l = UC_next then  
                                      {uc with ui_code=
                                            Jump exit_label}
                                   else uc;
                  | _ -> uc;
            ) ((get_mod instr).rl_instr_ucode
                                il
                                id  
                                pro) in
         List.iter (fun expr2 -> 
          let expr' = PI_bool (Cp_syntax.Relational,OP_eq,op1,expr2) in
          incr case_num;
          let label_case = sprintf "case_%d" !case_num in
          let label_case' = sprintf "i%d_%s" id1 label_case in
          let label_case_next = sprintf "case_%d" (!case_num+1) in
          let label_case_next' = sprintf "i%d_%s" id1 label_case_next in
          let res,ucl = expr_synth pro expr' label_case'
                                   None in
          let ucl =
              (*
              ** Extract temporary register transfer...
              ** Update temporary register frames to select label...
              *)
              List.iter (fun ui ->
                  match ui.ui_code with
                  | Expr (op,dst,op1,op2) ->
                  begin
                      match op1 with
                      | UC_temp ut ->
                          ut.ut_frame <- Some (UC_label label_select);
                      | _ -> ();
                  end;
                  begin
                      match op2 with
                      | UC_temp ut ->
                          ut.ut_frame <- Some (UC_label label_select);
                      | _ -> ();
                  end;
                  | _ -> ();  
                ) ucl;
              match ucl with
              | hd::tl ->
              begin
                  match hd.ui_code with
                  | Move (dst,src) ->
                  begin
                      match dst with
                      | UC_temp ut ->
                          ut.ut_frame <- Some (UC_label label_select);
                          tmp := Some dst;
                          tmp_move := [hd];
                          tl;
                      | _ -> ucl;
                  end;
                  | _ -> ucl;                    
              end;
              | [] ->  ucl in

          let falsejump = {
              ui_code = Falsejump (res,
                              if !n = 0 then UC_label label_end
                                 else UC_label label_case_next');
              ui_frame=List.hd pro.pro_frame;
            } in
          let label_code =
              match (List.hd sl).ui_code with
              | Label str -> str;
              | _ -> error 976681 ""; in
          let jump = {
              ui_code = Jump (UC_label label_code);
              ui_frame=List.hd pro.pro_frame;
            } in
          jumps := !jumps @ [label label_case src] @ ucl @ [falsejump;jump];
         ) exprli;
         blocks := !blocks @ sl;
        end;
        pro.pro_frame <- List.tl pro.pro_frame;
        | _ -> error 0 "Invalid case list found. Maybe incomplete case or missing block.";
        ) csl;
      pro.pro_constr <- List.tl pro.pro_constr;
      let bind = 
          {
              ui_code=Bind (List.length !jumps);
              ui_frame=List.hd pro.pro_frame;
          } in
      [label "select" src] @ !tmp_move @ [bind] @ 
                             !jumps @ !blocks @ 
                             (next "select");
  | PI_case _ -> error 666931 "";
  | PI_nop -> decr id; [];

  | PI_block (il,bf) when not (List.mem BP_bind bf.bf_params) ->
      let pl = bf.bf_params in
      pro.pro_frame <- bf :: pro.pro_frame;

      decr id;
      let ucll = ref [] in
      if pl <> [] then
          pro.pro_constr <- (constr_of_block "block" pl 
                                             pro.pro_constr) :: 
                            pro.pro_constr;
      List.iter (fun instr ->
          let instr_ucl = (get_mod instr).rl_instr_ucode
                                               instr
                                               id   
                                               pro in
          ucll := !ucll @ [instr_ucl];
        ) il;
    if pl <> [] then
          pro.pro_constr <- List.tl pro.pro_constr;
    let res = unroll !ucll in
    pro.pro_frame <- List.tl pro.pro_frame;
    res

  | PI_block (il,bf) when (List.mem BP_bind bf.bf_params) ->
      let pl = bf.bf_params in
      pro.pro_frame <- bf :: pro.pro_frame;
      pro.pro_constr <- (constr_of_block "select" 
                              [BP_expr EXPR_flat;BP_bind]
                              pro.pro_constr) :: pro.pro_constr;

      decr id;
      let id1 = !id + 1 in
      let ucll = ref [] in
      List.iter (fun instr ->
          let instr_ucl = (get_mod instr).rl_instr_ucode
                                               instr
                                               id   
                                               pro in
          ucll := !ucll @ [instr_ucl];

        ) il;
    pro.pro_constr <- List.tl pro.pro_constr;
    let id2= !id in

    let res = bind !ucll id1 id2 in
    pro.pro_frame <- List.tl pro.pro_frame;  
    res
  | PI_waitfor (src,cond,time,unit,i_false,i_true) ->
  begin
    line src;
    if time = 0 then
    begin
      line src;
      if cond = PI_nop then
        error 0 "Found zero delay without condition in waitfor statement.";

      let is_waitfor = cond = PI_nop in
      if is_waitfor then error 0 "Waitfor with zero cycles found.";

      let label_start = UC_label (sprintf "i%d_waitfor" id1) in
      let is_extended = 
        match i_false with
        | PI_nop -> false;
        | PI_assign _ -> true;
        | PI_block _ -> true;
        | _ -> error 0 "Extended waitfor only allowed with simple assignments.";
        in         

      let uil = ref [label "waitfor" src] in

      let res,expr = expr_synth pro cond (sprintf "i%d_waitfor" id1) 
                                None in
      if not is_extended then
        uil := !uil @ expr
      else
      begin
        let expr_f = 
          let uil_f = instr_ucode i_false id pro in
          List.filter (fun ui ->
              match ui.ui_code with
              | Move (dst,src) -> 
                let gd_rd_d,gd_wr_d = ud_guard dst in
                let gd_rd_s,gd_wr_s = ud_guard src in
                if gd_rd_s or gd_wr_s or gd_wr_d or gd_rd_d then
                  error 0 "Guarded objects in waitfor wait block found.";
                true;
              | Label _ -> false;
              | Jump _ -> false;
              | _ -> error 0 "Unexpected statement inside waitfor wait block found." 
              ) uil_f;
          in
        (*
        ** Default block
        *)
        let uil_t = instr_ucode i_true id pro in
        List.iter (fun ui ->
              match ui.ui_code with
              | Move (dst,src) -> 
                let dst_co =
                  match dst with
                  | UC_reg uo
                  | UC_sig uo -> 
                  begin
                    match uo.uo_obj with
                    | Some co -> co;
                    | None -> error 731139 "";
                  end;
                  | _ -> error 0 "Unexpected LHS in waitfor default block found."; in
                let src_val =
                  match src with
                  | UC_val uv -> uv.uv_val;
                  | _ -> error 0 "Unexpected RHS in waitfor default block found."; in
                dst_co.co_default <- src_val;
              | Label _ -> ();
              | Jump _ -> ();
              | _ -> error 0 "Unexpected statement inside waitfor default block found." 
              ) uil_t;

        let n = (List.length expr) + 
                (List.length expr_f) + 1 in
        let bind = {
            ui_code = Bind n;
            ui_frame=List.hd pro.pro_frame;
          } in
        uil := !uil @ [bind] @ expr_f @ expr;
      end;

      let falsejump = {
          ui_code = Falsejump (res,label_start);
          ui_frame=List.hd pro.pro_frame;
        } in
      uil := !uil @ [falsejump];
      !uil @ (next "waitfor");
    end
    else 
    begin
      (*
      ** Timing
      *)
      line src;
      let is_extended = 
        match i_false with
        | PI_nop -> false;
        | PI_assign _ -> true;
        | PI_block _ -> true;
        | _ -> error 0 "Extended waitfor only allowed with simple assignments.";
        in
      let is_delay = cond = PI_nop in
      if not is_delay then error 0 "Waitfor with delay and condition found.";
      let uil = ref ([label "delay" src]) in

      let n = Int64.to_int (cyc_of_time (Int64.of_int time) unit) in  (* TODO *)
      let expr_f = 
        if is_extended then
        begin
          let uil_f = instr_ucode i_false id pro in
          List.filter (fun ui ->
              match ui.ui_code with
              | Move (dst,src) -> 
                let gd_rd_d,gd_wr_d = ud_guard dst in
                let gd_rd_s,gd_wr_s = ud_guard src in
                if gd_rd_s or gd_wr_s or gd_wr_d or gd_rd_d then
                  error 0 "Guarded objects in waitfor block found.";
                true;
              | Label _ -> false;
              | Jump _ -> false;
              | _ -> error 0 "Unexpected statement inside waitfor block found." 
              ) uil_f;
        end
        else [] in
      if is_extended then
      begin
        (*
        ** Default block
        *)

        let uil_t = instr_ucode i_true id pro in
        List.iter (fun ui ->
              match ui.ui_code with
              | Move (dst,src) -> 
                let dst_co =
                  match dst with
                  | UC_reg uo
                  | UC_sig uo -> 
                  begin
                    match uo.uo_obj with
                    | Some co -> co;
                    | None -> error 731149 "";
                  end;
                  | _ -> error 0 "Unexpected LHS in waitfor default block."; in
                let src_val =
                  match src with
                  | UC_val uv -> uv.uv_val;
                  | _ -> error 0 "Unexpected RHS in waitfor default block."; in
                dst_co.co_default <- src_val;
              | Label _ -> ();
              | Jump _ -> ();
              | _ -> error 0 "Unexpected statement inside waitfor default block found." 
              ) uil_t;
      end;

      let n' = List.length expr_f in

      if n > 2 then
      begin
        let label_start = sprintf "i%d_delay" id1 in
        let label_loop = sprintf "i%d_delay_loop" id1 in
        let dt = DT_logic (const_width (V_int (i64 (n*2)))) in
        let temp = {
              ut_type = uo_type dt dt;
              ut_range = None;
              ut_name = "delay_count";
              ut_frame = Some (UC_label label_start);
              ut_obj = None;
              ut_flags = [];
          } in
        let dst_a = immed () in
        let dst_b = dst_a in
        let zero = UC_val { uv_type= uo_type dt dt;
                            uv_val=(V_int Int64.zero)} in
        let one = UC_val {uv_type= uo_type dt dt;
                          uv_val=(V_int Int64.one)} in
        let expr = [
          {
              ui_code = Move (
                        UC_temp {temp with ut_flags = [UO_lhs;UO_loc]},
                        UC_val {uv_type= uo_type dt dt;
                                uv_val=(V_int (i64 (n-2)))};
                        );
              ui_frame=List.hd pro.pro_frame;
          };
          {
              ui_code = Label label_loop;
              ui_frame=List.hd pro.pro_frame;
          };
          {
              ui_code = Bind (4+n');
              ui_frame=List.hd pro.pro_frame;
          }] @
              expr_f @
          [
          {
              ui_code = Expr (
                        [OP_sub],
                        dst_a,
                        UC_temp {temp with ut_flags = [UO_rhs;UO_loc]},
                        one);
              ui_frame=List.hd pro.pro_frame;
          };
          {
              ui_code = Move (
                        UC_temp {temp with ut_flags = [UO_lhs;UO_loc]},
                        dst_a
                        );
              ui_frame=List.hd pro.pro_frame;
          };
          {
              ui_code = Expr (
                        [OP_eq],
                        dst_b,
                        UC_temp {temp with ut_flags = [UO_rhs;UO_loc]},
                        zero);
              ui_frame=List.hd pro.pro_frame;
          };
          ] in
        let falsejump = {
          ui_code = Falsejump (dst_b,UC_label label_loop);
          ui_frame=List.hd pro.pro_frame;
        } in
        [label "delay" src] @ 
                expr @ [falsejump] @
                (next "delay_loop")@
                (next "delay");
      end
      else
      begin
        (*
        ** Unrolled wait states...
        ** To create exact n states, use dummy register moves... 
        *)
        let label_start = (sprintf "i%d_delay" id1) in
        let dt = DT_logic 2 in
        let temp = {
              ut_type = uo_type dt dt;
              ut_range = None;
              ut_name = "delay_count";
              ut_frame = Some (UC_label label_start);
              ut_obj = None;
              ut_flags = [];
          } in

        let expr = 
          (if n > 0 then [{
              ui_code = Move (
                        UC_temp {temp with ut_flags = [UO_lhs;UO_loc]},
                        UC_val {uv_type= uo_type dt dt;
                                uv_val=(V_int (i64 1))};
                        );
              ui_frame=List.hd pro.pro_frame;
          }] else [])@
          (if n > 1 then 
          [
              {
                  ui_code = Move (
                        UC_temp {temp with ut_flags = [UO_lhs;UO_loc]},
                        UC_val {uv_type= uo_type dt dt;
                                uv_val=(V_int (i64 2))};
                        );
                  ui_frame=List.hd pro.pro_frame;
              }
          ] else []
          ) 
         in
        [label "delay" src] @ 
                expr @ 
                (next "delay");

    end;
  end;

      end;
  | PI_map _ 
  | PI_monitor _ ->
      [];
  | PI_fun (src,(opl,ot),sel,il) ->
      line src; 
      let rules = get_mod instr in
      if rules.rl_name <> "Core"  then
      begin
        (*
        ** Direct function call to appropiate module
        *) 
        let instr_ucl = rules.rl_instr_ucode
                                 instr
                                 id   
                                 pro in
        instr_ucl
      end
      else
      begin
        (*
        ** A function from Core module must be handled here!
        *)
        match sel with
        | "copy"
        | "init" ->
          let dt = 
            match ot with
            | OT_object ao ->
            begin
              match ao.ao_array with
              | at :: _ -> dt_of_ot (OT_array at);
              | [] -> None;
            end;
            | _ -> error 476293 "";
            in
          let arg = fun_get_ud (Some pro) "Core" sel il 1 dt true in
          [label "fun" src;
                   {ui_code=Fun ((opl,ot),sel,[UA_data arg]);
                    ui_frame=List.hd pro.pro_frame;}] @ 
                  (next "fun");
        | "set" ->
          let dt = 
            match ot with
            | OT_object ao ->
            begin
              match ao.ao_array with
              | at :: _ -> dt_of_ot (OT_array at);
              | [] -> None;
            end;
            | _ -> error 476293 "";
            in
          let a = fun_get_ud (Some pro) "Core" sel il 1 None true in
          let b = fun_get_ud (Some pro) "Core" sel il 2 None true in
          let arg = fun_get_ud (Some pro) "Core" sel il 3 dt true in
          [label "fun" src;{ui_code=Fun ((opl,ot),sel,
                                           [UA_data a;
                                            UA_data b;
                                            UA_data arg  
                                           ]);
                                           ui_frame=List.hd pro.pro_frame;}] @ 
                  (next "fun");
        | "copyn" ->
          let dt = 
            match ot with
            | OT_object ao ->
            begin
              match ao.ao_array with
              | at :: _ -> dt_of_ot (OT_array at);
              | [] -> None;
            end;
            | _ -> error 476293 "";
            in
          let dst_a = fun_get_ud (Some pro) "Core" sel il 1 None true in
          let src_a = fun_get_ud (Some pro) "Core" sel il 2 None true in
          let len = fun_get_ud (Some pro) "Core" sel il 3 None true in
          let arg = fun_get_ud (Some pro) "Core" sel il 4 dt true in
          [label "fun" src;{ui_code=Fun ((opl,ot),sel,
                                      [UA_data dst_a;
                                       UA_data src_a;
                                       UA_data len;
                                       UA_data arg  
                                       ]);
                                       ui_frame=List.hd pro.pro_frame;}] @ 
                  (next "fun");
        | "unlock" ->
        begin
          let rec get_ao ot = 
              match ot with
              | OT_queue qu -> qu.qu_ao,ot;
              | OT_channel ch ->  ch.ch_ao,ot;
              | OT_object ao -> ao,ot;
              | OT_array at -> 
                if is_sel opl then
                begin
                  let n = at_index at (obj_sel opl) in
                  get_ao at.at_objs.(n);
                end
                else
                    error 0 (sprintf "Array <%s> access without selector found." at.at_name);
              | _ -> error 750397 "" in
          let ao,ao_ot = get_ao ot in
          let ot' =
              match ao.ao_obj with
              | Some ot' ->  ot';
              | None -> ao.ao_obj <- Some ao_ot; ao_ot in
          match ot' with
          | OT_queue qu -> 
              [label "fun" src;{ui_code=Fun ((opl,ot),sel,[]);
                                ui_frame=List.hd pro.pro_frame;}] @ 
                      (next "fun");

          | OT_channel ch -> 
              [label "fun" src;{ui_code=Fun ((opl,ot),sel,[]);
                                ui_frame=List.hd pro.pro_frame;}] @ 
                      (next "fun");

          | _ -> error 0 (sprintf "Core: unknown object <%s> with method unlock found."         
                                  ao.ao_name);
        end;
        | "cmp_eq"
        | "cmp_neq" ->
        begin
          let ao_dt = 
            match ot with
            | OT_object ao ->
            begin
              match ao.ao_array with
              | at :: _ -> dt_of_ot (OT_array at);
              | [] -> None;
            end;
            | _ -> error 276293 "";
            in
            let res = fun_get_ud (Some pro) "Core" sel il 1 None  false in 
            let op2 = fun_get_ud (Some pro) "Core" sel il 2 ao_dt true in
            [label "fun" src;{ui_code=Fun ((opl,ot),sel,
                                        [UA_data res;
                                         UA_data op2]);
                                      ui_frame=List.hd pro.pro_frame;}] @
                    (next "fun");
        end;
        | "cmpn_eq"
        | "cmpn_neq" ->
        begin
          let ao_dt = 
            match ot with
            | OT_object ao ->
            begin
              match ao.ao_array with
              | at :: _ -> dt_of_ot (OT_array at);
              | [] -> None;
            end;
            | _ -> error 276293 "";
            in
            let res = fun_get_ud (Some pro) "Core" sel il 1 None  false in 
            let op1_a = fun_get_ud (Some pro) "Core" sel il 2 None  true in
            let op2_a = fun_get_ud (Some pro) "Core" sel il 3 None  true in
            let len = fun_get_ud (Some pro) "Core" sel il 4 None  true in
            let op2 = fun_get_ud (Some pro) "Core" sel il 5 ao_dt true in
            [label "fun" src;{ui_code=Fun ((opl,ot),sel,
                                        [UA_data res;
                                         UA_data op1_a;
                                         UA_data op2_a;
                                         UA_data len;
                                         UA_data op2]);
                                      ui_frame=List.hd pro.pro_frame;}] @
                    (next "fun");
        end;
        | "guard" -> [];
        | "empty" | "full" -> 
        begin
          let ao = 
              match ot with
              | OT_object ao -> ao;
              | _ -> error 750398 "" in
          let ot' =
              match ao.ao_obj with
              | Some ot' ->  ot';
              | None -> error 864665 "" in
          let res = fun_get_ud (Some pro) "Core" sel il 1 None  false in 
          match ot' with
          | OT_queue qu -> 
              [label "fun" src;{ui_code=Fun ((opl,ot),sel,[UA_data res]);
                                ui_frame=List.hd pro.pro_frame;}] @ 
                      (next "fun");

          | OT_channel ch -> 
              [label "fun" src;{ui_code=Fun ((opl,ot),sel,[UA_data res]);
                                ui_frame=List.hd pro.pro_frame;}] @ 
                      (next "fun");

          | _ -> error 0 (sprintf "Core: unknown object <%s> with method %s found."         
                                  ao.ao_name sel);
        end;

        | _ -> error 0 (sprintf "Core: unknown object method <%s>"         
                                sel);
      end;
  | _ -> [labelo "unknown"] @ (next "unknwon")
