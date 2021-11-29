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
**    $CREATED:     29.11.2007
**    $VERSION:     2.02
**
**    $INFO:
**
**  uCode expression constraint scheduler.
**  This scheduler is the counterpart to the reference stack module which tries to compact 
**  and merge single expressions in assignments to one large meta expression/assignment,
**  which is actually already optimized (constant folding...).
**  A meta expression is scheduled initially within one time step, but can violate
**  timing constraints. Therefore, the scheduler must inspect EACH expression and tries
**  to predict the calculation time. There is a timing constrain which limits the 
**  calculation time to the time of one time step. This leads to a split of expressions
**  into subexpression, and each subexpression requires (at least) one time step with
**  a calculation time not exceeding this time step value. This scheduling inferres new
**  temporary registers (or uses already inferred ones).
**  
**    $ENDOFINFO
**
*)
open Cp_print
open Cp_utils
open Cp_data
open Cp_types
open Cp_alu
open Printf
open Cp_symbol
open Cp_block_frame
open Cp_syntax
open Cp_resource
open Cp_common
open Cp_printtypes

(*
** Simple ASAP expression scheduler
*)
let ui_schedule_expr pro uil =
  out (sprintf "Expression Scheduler: Performing ASAP time constrained scheduling for process <%s>..." pro.pro_name);
  ind_incr ();
  let scheduled = ref 0 in
  let expanded = ref 0 in
  let last_label = ref None in
  let last_frame = ref None in
  let last_expr_dt = ref DT_bool in
  let label_reloc = ref [] in
  
  (*
  ** Schedule bounded expression instruction list
  ** with timing constraints.
  *)
  let sched uil =
    scheduled := !scheduled + (List.length uil);
    
    let rec expr_dt udl =
      match udl with
      | ud :: tl ->
      begin
        match ud with
        | UC_immed _ -> expr_dt tl;
        | _ ->
          let uo_type = type_of_ud ud in
          last_expr_dt := uo_type.uo_expr_type;
          uo_type.uo_expr_type;
      end;
      | [] -> !last_expr_dt in
      
    (*
    ** Relocate temporary register replacements
    ** required for splitted immediate destination
    ** boundaries.
    *)
    let reloc = ref [] in
    let expr_relocate uil =
      let relocate_immed ud =
        match ud with
        | UC_immed n -> 
        begin
          let replace = ref None in
          List.iter (fun (n',tmp) ->
            if n = n' then replace := Some tmp;
            ) !reloc;
          match !replace with
          | Some tmp -> tmp;
          | None -> ud;
        end;
        | _ -> ud in
        
      List.iter (fun ui ->
        match ui.ui_code with
        | Expr (opl,dst,op1,op2) ->
        begin
          ui.ui_code <- Expr (opl,dst,
                              relocate_immed op1,
                              relocate_immed op2); 
        end;
        | _ -> ();
        ) uil in
        
    let add_label_n n =
      match !last_label with
      | Some (UC_label s) -> 
        if n = 0 then label_reloc := !label_reloc @ [s,sprintf "%s_S%d" s n];
        {
          ui_code = Label (sprintf "%s_S%d" s n);
          ui_frame = (
            match !last_frame with
            | Some bf -> bf;
            | None -> error 664399 "");
        }
      | _ -> error 664398 "" in   
      
    let last_frame_n n =
      match !last_frame with
      | Some bf -> 
      begin
        let bf' = {
          bf with
            bf_name= sprintf "%s_S%d" bf.bf_name n;
            bf_childs = [];
          } in
        match bf.bf_parent with
        | Some bf'' -> 
          bf''.bf_childs <- bf''.bf_childs @ [bf'];                        
          bf'
        | None -> bf' (* ? *) 
      end;
      | _ -> error 664397 "" in 

    let remove_last_frame () =
      match !last_frame with
      | Some bf -> 
      begin
        match bf.bf_parent with
        | Some bf' ->
          bf'.bf_childs <- List.filter (fun bf'' ->
                                bf.bf_name <> bf''.bf_name)
                                  bf'.bf_childs;
        | None -> () (* ? *) 
      end;   
      | None -> error 520041 "" in
        
    let bind n =
        {
            ui_code = Bind n;
            ui_frame = (
            match !last_frame with
            | Some bf -> bf;
            | None -> error 664399 "");     
        } in

    (*
    ** New instruction list
    *)
    let uil' = ref [] in
    (*
    ** Actual bounded expression instruction list
    *)
    let bul = ref [] in
    let last = ref (List.hd uil) in
    (*
    ** Actual time consumed...
    *)
    let bul_time = ref 0.0 in
    let sec = ref 0 in
    
    List.iter (fun ui ->
      match ui.ui_code with
      | Expr (opl,dst,op1,op2) ->
        let op = List.hd opl in
        let dt = 
          if ((op_mode op) = OP_bool) ||
             ((op_mode op) = OP_relat)  then DT_bool 
          else expr_dt [dst;op1;op2] in
        
        let t = op_time op dt in
        if debug_it "ui_schedule_simple" then 
          debug "ui_schedule_simple" with (sprintf "Expr(%s): t=%f" (op_name op) t);
          
        bul_time := !bul_time +. t;
        if !bul_time <= 1.0 || !bul = [] then
        begin
          (*
          ** Add to cummulated expression list, either if
		  ** total time weight is less than value 1.0
		  ** (== 1 clock cycle time), or there is one
		  ** operation with estimated processing time weight
		  ** larger than 1.0.
          *)
          bul := !bul @ [ui];
          last := ui;
        end
        else
        begin
          (*
          ** Flush actual cummulated bounded bul list:
          ** 1. Bound all instructions from bul, last
          **    one requires change from IMMED_/TEMPS_
          **    to real temporary register destination.
          ** 2. Create new block frame, insert it in
          **    actual parent frame child list, remove
          **    original ASSIGN frame, if any.
          *)
          let bf' = last_frame_n !sec in
          if !sec = 0 then remove_last_frame ();
          
          let dst = 
            match !last.ui_code with
            | Expr (opl',dst',op1',op2') ->
            begin
              match dst' with
              | UC_immed n ->
                let name1 = sprintf "%%I%d" n in
                let op' = List.hd opl' in
                let dt' = 
                  if ((op_mode op') = OP_bool) ||
                     ((op_mode op') = OP_relat)  then DT_bool 
                  else expr_dt [dst';op1';op2'] in
                let ct' = if (op_mode op') = OP_bool ||
                             (op_mode op') = OP_relat  then Some (DT_logic 1) else None in
                let dst'' = {
                        ut_type = uo_type dt' dt'; 
                        ut_range = None;
                        ut_name = name1;
                        ut_frame = !last_label;
                        ut_obj = None;
                        ut_flags = [UO_lhs;UO_loc];
                    } in
                reloc := !reloc @ [n,UC_temp {dst'' with ut_type=copy_uo_type dst''.ut_type;
                                                         ut_flags=[UO_rhs;UO_loc]}];
                !last.ui_code <- Expr(opl',UC_temp dst'',op1',op2');
				incr expanded;
                UC_temp dst''
              | _ -> dst';
            end;
            | _ -> error 392044 "" in
          
          let bul' =
            [add_label_n !sec;] @ 
            (
              let n = List.length !bul in
              if n > 1 then [bind n] else []
            ) @ !bul in
          List.iter (fun ui ->
            (*
            ** Replace new blockframe bf'...
            *)
            ui.ui_frame <- bf';
            ) bul';        
          uil' := !uil' @ bul';
          incr sec;
          bul := [ui];
          bul_time := t;
		  last := ui;
        end; 
      | Nop
      | Jump _
      | Falsejump _ ->
        bul := !bul @ [ui];
        last := ui;
      | _ -> List.iter (fun ui -> out (ui_sprint_instr ui)) uil; error 415541 "";
      ) uil;
      
  if !bul <> [] then
  begin
    let bul' = 
      [add_label_n !sec;] @ 
      (
        let n = List.length !bul in
        if n > 1 then [bind n] else []
      ) @ !bul in
    if !sec > 0 then
    begin
      let bf' = last_frame_n !sec in
      List.iter (fun ui ->
            (*
            ** Replace new blockframe bf'...
            *)
            ui.ui_frame <- bf';
            ) bul';    
    end;    
    uil' := !uil' @ bul';
  end;
  expr_relocate !uil';
  !uil' in
      
  let get_n n li =
    let tail = ref [] in
    let rec iter n li =
      match li with
      | hd::tl -> if n > 1 then hd :: (iter (n-1) tl) else 
                  begin
                    tail := tl;
                    [hd];
                  end;
      | [] -> [] in
    let head = iter n li in
    head, !tail in
  let get_last l = List.hd (List.rev l) in
   
  let rec jump_reloc uil =
    let rec rel ul rl =
      match ul with
      | UC_next -> ul;
      | UC_label str ->
      begin 
        match rl with
        | (s,s') :: tl ->
          if str = s then UC_label s'
            else rel ul tl;
        | [] -> UC_label str;
      end;
      in
    match uil with
    | ui :: tl ->
    begin
      let ui' = 
        match ui.ui_code with
        | Jump ul -> 
          {ui with ui_code=Jump (rel ul !label_reloc)}
        | Falsejump (expr,ul) -> 
          {ui with ui_code=Falsejump (expr,rel ul !label_reloc)}
        | _ -> ui in
      ui' :: (jump_reloc tl); 
    end;
    | [] -> [] in
    
  (*
  ** Remember block_frame of last label; bind has no complete block frame!
  *)
  let rec iter uil =
    match uil with
    | ui :: tl -> 
    begin
      match ui.ui_code with
      | Label s -> 
        last_label := Some (UC_label s);
        last_frame := Some ui.ui_frame;
        ui :: (iter tl);
      | Bind n -> 
      begin
        match !last_frame with
        | Some bf -> 
          let do_sched =
            let rec iter pl =
              match pl with
              | (BP_schedule Sched_auto) :: tl -> true;
              | (BP_schedule (Sched_custom sl)) :: tl ->
                let rec iter2 sl =
                  match sl with
                  | Sched_expr :: tl -> true;
                  | hd :: tl -> iter2 tl;
                  | [] -> iter tl in
                iter2 sl
              | hd::tl -> iter tl;
              | [] -> false in
            iter bf.bf_params in  
 
          let is_branch_list =
            if n > 3 then
            begin
              let jumps = ref 0 in
              let uil',_ = get_n n tl in
              List.iter (fun ui ->
                match ui.ui_code with
                | Jump _
                | Falsejump _ -> incr jumps;
                | _ -> ();
                ) uil';
              !jumps > 1
            end
            else
                false in 
            
          if do_sched && not is_branch_list then
          begin
            let head,tail = get_n n tl in
            let scheduled = sched head in 
            scheduled @ (iter tail)
          end
          else ui :: (iter tl);
        | None -> ui :: (iter tl);
      end;
      | _ ->
      begin
        ui :: (iter tl);
      end;
    end;
    | [] -> [] in
  let uil' = jump_reloc (iter uil) in
  out (sprintf "Scheduled %d expression(s), expanded %d time step(s)." !scheduled !expanded);
  ind_decr ();
  uil'
  
