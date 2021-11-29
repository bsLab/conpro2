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
**    $CREATED:     14.1.2008
**    $VERSION:     2.02
**
**    $INFO:
**
**  uCode basicblock scheduler.
** 
**  Basic blocks are extended instruction blocks without side effect in the control path, 
**  that means blocks with only one entry point at the beginning and one exit point at
**  the end.
**  Independent expressions inside the basicblock can be scheduled concurrently
**  in the same time step.   
**  Here, basicblocks are named major blocks,
**  containing a list of atomic expressions, named minor blocks!
**  Dependencies are explored between minor blocks of each major block. Independent
**  minor blocks are scheduled in the same time step.
**  Minor blocks are bound to data dependency graphs. This graph explores data 
**  dependendencies: F: Forward, B: Backward, O: Order dep.
**  Each minor block can have several successor blocks. Scheduling is done
**  by longest path first rule.
**
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
open Cp_ucode
open Cp_common

type majorblock_kind =
  (*
  ** JUMP/FALSEJUMP/FUN
  *)
  | MJB_control
  (*
  ** MOVE/EXPR
  *)
  | MJB_data
  (*
  ** Keep unscheduled
  *)
  | MJB_fixed

(*
** Node of expression data dependency
** graph within a basicblock, or just generic 
** instruction list in control path, too. A control
** path always consists only of one minorblock.
*)
type minorblock_flags =
  | MIB_scheduled
  | MIB_jump
  
type minorblock = {
  mutable mib_id: int;
  mutable mib_instr: uc_instr list;
  mutable mib_guards: guard_type list;
  (*
  ** Data dependencies
  *)
  mutable mib_lhs: string list;
  mutable mib_rhs: string list;
  (*
  ** DDG
  *)
  mutable mib_childs: mib_edge;
  mutable mib_parents: mib_edge;
  mutable mib_next: minorblock list;
  mutable mib_flags: minorblock_flags list;
  mutable mib_load: int;
 }
and mib_edge = {
  (*
  ** Minor block dependency: Forward RHS data dep.
  *)
  mutable mie_forward_dep: minorblock list;
  (*
  ** Minor block dependency: LHS control order dep.
  *)
  mutable mie_order_dep: minorblock list;
  (*
  ** Minor block dependency: Backward RHS control order dep.
  *)
  mutable mie_backward_dep: minorblock list;
  (*
  ** RHS=[e] LHS=[e]!
  *)
  mutable mie_recursive: bool;
 }
and ddg_forest = {
  mutable ddg_root: minorblock;
  mutable ddg_schedule: minorblock list;
 }
and majorblock = {
  mutable mjb_id: int;
  mutable mjb_minorblocks: minorblock list;
  mutable mjb_minorblocks_head: minorblock;
  mutable mjb_kind: majorblock_kind;
  mutable mjb_prev: majorblock option;
  mutable mjb_next: majorblock option; 
  (*
  ** All independent DDGs (starting minor
  ** block without a parent).
  *)
  mutable mjb_ddg_forest: ddg_forest list;
 }


(*
** Super/basicblock scheduler
*)
let ui_schedule_basicblock pro uil =
  let uil' = ref [] in
  out (sprintf "Block Scheduler: Exploring concurrency in basic blocks for process <%s>..." pro.pro_name);
  ind_incr ();
  let major_splitted = ref 0 in

  (*
  ** Starting with initial basicblock partitioning, just collecting expressions in
  ** data and instructions into control blocks.
  ** ...
  *)
  let new_mib id = {
      mib_id = id;
      mib_instr = [];
      mib_guards = [];
      mib_lhs = [];
      mib_rhs = [];
      mib_childs = {mie_forward_dep=[];mie_order_dep=[];mie_backward_dep=[];mie_recursive=false};
      mib_parents = {mie_forward_dep=[];mie_order_dep=[];mie_backward_dep=[];mie_recursive=false};
      mib_next = [];
      mib_flags = [];
      mib_load = 0;
      } in
    
  let new_minorblock mjb =
    let mib = new_mib 0 in
    mjb.mjb_minorblocks <- mib :: mjb.mjb_minorblocks;
    mjb.mjb_minorblocks_head <- mib in

  (*
  ** Major block...
  *)
  let new_mjb kind =
    let mib = {
      mib_id = 0;
      mib_instr = [];
      mib_guards = [];
      mib_lhs = [];
      mib_rhs = [];
      mib_childs = {mie_forward_dep=[];mie_order_dep=[];mie_backward_dep=[];mie_recursive=false};
      mib_parents = {mie_forward_dep=[];mie_order_dep=[];mie_backward_dep=[];mie_recursive=false};
      mib_next = [];
      mib_flags = [];
      mib_load = 0;
      } in
    let mjb = {
      mjb_id = 0;
      mjb_minorblocks = [mib];
      mjb_minorblocks_head = mib;         
      mjb_kind = kind;
      mjb_prev = None;
      mjb_next = None;
      mjb_ddg_forest = [];
    } in
    mjb in

  let mjb = new_mjb MJB_fixed in
  let mjb_head = ref mjb in

  let add_uc mjb uc =
    mjb.mjb_minorblocks_head.mib_instr <- uc :: mjb.mjb_minorblocks_head.mib_instr; in
  let set_next mjb next =
    mjb.mjb_next <- Some next in
  let set_prev mjb prev =
    mjb.mjb_prev <- Some prev in
  let first_uc mjb =
    match mjb.mjb_minorblocks with
    | mib::_ ->
    begin
          match mib.mib_instr with
          | uc :: _ -> uc;
          | [] -> error 0 "empty basicblock found?";
    end;
    | [] -> error 0 "empty basicblock found?"; in

  let rec add_prev_label mjb uil =
    match uil with
    | uc::tl ->
    begin
      match uc.ui_code with
      | Label _ -> 
        add_uc mjb uc; 
        tl;
      | Nop ->
        add_uc mjb uc;
        add_prev_label mjb tl;
      | _ -> uil;
    end;
    | [] -> uil in
  let rec next_label_end add uil =
    match uil with
    | uc::tl ->
    begin
      match uc.ui_code with
      | Label str ->
        let len = String.length str in
        if len > 4 && (String.sub str (len-4) 4) = "_end" then (add@[uc]),tl
        else add,uil 
      | Nop ->
        next_label_end (add@[uc]) tl;
      | _ -> add,uil;
    end;
    | [] -> add,uil in

  let rec get_next_label uil =
    match uil with
    | uc::tl ->
    begin
      match uc.ui_code with
      | Label str -> str;
      | _ -> get_next_label tl;
    end;
    | [] -> "%END" in
  let print_mib mib = 
      out_ (sprintf "BEGIN<(%d)%s%s%s%s%s%s%s%s%s>" mib.mib_id
                  (if List.mem GD_rd mib.mib_guards then
                    "[GD_RD]"
                   else "")
                  (if List.mem GD_wr mib.mib_guards then
                    "[GD_WR]"
                   else "")
                  (if List.mem MIB_jump mib.mib_flags then
                    "[JUMP]"
                   else "")
                  (if List.mem MIB_scheduled mib.mib_flags then
                    "[SCHED]"
                   else "")
                  (
                    let s = ref "" in
                    let first = ref true in
                    List.iter (fun lhs -> 
                      if !first then
                        s := lhs
                      else
                        s := sprintf "%s,%s" !s lhs;
                      first := false;) mib.mib_lhs;
                    sprintf "LHS=[%s]" !s;
                  )
                  (
                    let s = ref "" in
                    let first = ref true in
                    List.iter (fun rhs -> 
                      if !first then
                        s := rhs
                      else
                        s := sprintf "%s,%s" !s rhs;
                      first := false;) mib.mib_rhs;
                    sprintf "RHS=[%s]" !s;                  
                  )
                  (
                    let s = ref "" in
                    let first = ref true in
                    List.iter (fun child -> 
                      if !first then
                        s := sprintf "F%d" child.mib_id
                      else
                        s := sprintf "%s,F%d" !s child.mib_id;
                      first := false;) mib.mib_childs.mie_forward_dep;
                    List.iter (fun child -> 
                      if !first then
                        s := sprintf "O%d" child.mib_id
                      else
                        s := sprintf "%s,O%d" !s child.mib_id;
                      first := false;) mib.mib_childs.mie_order_dep;
                    List.iter (fun child -> 
                      if !first then
                        s := sprintf "B%d" child.mib_id
                      else
                        s := sprintf "%s,B%d" !s child.mib_id;
                      first := false;) mib.mib_childs.mie_backward_dep;
                    if mib.mib_childs.mie_recursive && !first 
                      then s := sprintf "%sR%d" !s mib.mib_id else
                    if mib.mib_childs.mie_recursive 
                      then s := sprintf "%s,R%d" !s mib.mib_id;
                    sprintf "CHILDS=[%s]" !s;
                  )
                  (
                    let s = ref "" in
                    let first = ref true in
                    List.iter (fun parent -> 
                      if !first then
                        s := sprintf "F%d" parent.mib_id
                      else
                        s := sprintf "%s,F%d" !s parent.mib_id;
                      first := false;) mib.mib_parents.mie_forward_dep;
                    List.iter (fun parent -> 
                      if !first then
                        s := sprintf "O%d" parent.mib_id
                      else
                        s := sprintf "%s,O%d" !s parent.mib_id;
                      first := false;) mib.mib_parents.mie_order_dep;
                    List.iter (fun parent -> 
                      if !first then
                        s := sprintf "B%d" parent.mib_id
                      else
                        s := sprintf "%s,B%d" !s parent.mib_id;
                      first := false;) mib.mib_parents.mie_backward_dep;
                    sprintf "PARENTS=[%s]" !s;
                  )
                  (
                    let s = ref "" in
                    let first = ref true in
                    List.iter (fun mib' ->
                      if !first then s:= sprintf "NEXT=[%d" mib'.mib_id
                      else s := sprintf "%s,%d" !s mib'.mib_id;
                      first := false;
                      ) mib.mib_next;
                    sprintf "%s]" !s
                  ));
      ind_incr ();
      List.iter (fun uc ->
        out_ (ui_sprint_uc uc)) mib.mib_instr;
      ind_decr (); 
      out_ "END"; in

  let print_ddg ddg =
    let s = ref "[" in
    let first = ref true in
    if ddg.ddg_root.mib_id = (-1) then
      List.iter (fun next ->
        if !first then s := sprintf "%s%d" !s next.mib_id
        else s := sprintf "%s,%d" !s next.mib_id;
        first := false;
        ) ddg.ddg_root.mib_next
    else s := sprintf "%d" ddg.ddg_root.mib_id;
    s := sprintf "%s]" !s;
    !s in

  let rec print_mjb mjb  =
    let print_mjb_kind mjb =
      match mjb.mjb_kind with
      | MJB_data -> "DATA";
      | MJB_control -> "CTRL";
      | MJB_fixed -> "FIXED"; in
    out_ (sprintf "BEGIN<%s#%d%s%s>" 
                (print_mjb_kind mjb) 
                mjb.mjb_id
                (if mjb.mjb_kind = MJB_data then
                  sprintf ",DDG forests=[%s]"
                          (
                            let s = ref "" in
                            List.iter (fun ddg ->
                              s := sprintf "%s%s" !s (print_ddg ddg);
                               ) mjb.mjb_ddg_forest;
                            !s
                          )
                 else "")
                 (match mjb.mjb_next with
                  | Some next -> sprintf ",NEXT=%d" next.mjb_id;
                  | None -> ",NEXT=None"));
    ind_incr ();
    List.iter (fun mib -> print_mib mib;) mjb.mjb_minorblocks;
    ind_decr ();
    out_ (sprintf "END<%s>" (print_mjb_kind mjb));
    match mjb.mjb_next with
    | Some mjb' -> print_mjb mjb';
    | None -> () in

  let guards uil =
    let rec search gds uil =
      match uil with
      | uc :: tl ->
        let gds' = 
          match uc.ui_code with
          | Expr (_,dst,op1,op2) -> 
            let gd_rd1,gd_wr1 = ud_guard dst in
            let gd_rd2,gd_wr2 = ud_guard op1 in
            let gd_rd3,gd_wr3 = ud_guard op2 in
            (if gd_rd1 || gd_rd2 || gd_rd3 &&
                not (List.mem GD_rd gds) then [GD_rd] else [])@ 
            (if gd_wr1 &&
                not (List.mem GD_wr gds) then [GD_wr] else [])@gds 
          | Move (dst,src) -> 
            let gd_rd1,gd_wr1 = ud_guard dst in
            let gd_rd2,gd_wr2 = ud_guard src in
            (if gd_rd1 || gd_rd2 &&
                not (List.mem GD_rd gds) then [GD_rd] else [])@ 
            (if gd_wr1 &&
                not (List.mem GD_wr gds) then [GD_wr] else [])@gds 
          | _ -> gds in
        search gds' tl;
      | [] -> gds in
    search [] uil in

  (*
  ** Return two lists of all read (obj. appearing on RHS) and write (obj. app. on LHS) guarded objects respectively
  *)
  let get_guarded_objs uil =
    let rec search gds_rd gds_wr uil =
      match uil with
      | uc :: tl ->
        let gds_rd',gds_wr' = 
          match uc.ui_code with
          | Expr (_,dst,op1,op2) -> 
            let gd_rd1,gd_wr1 = ud_guard dst in
            let gd_rd2,gd_wr2 = ud_guard op1 in
            let gd_rd3,gd_wr3 = ud_guard op2 in
            (if gd_rd2 then [ud_name op1] else [])@
            (if gd_rd3 then [ud_name op2] else [])@gds_rd,
            (if gd_wr1 then [ud_name dst] else [])@gds_wr
          | Move (dst,src) -> 
            let gd_rd1,gd_wr1 = ud_guard dst in
            let gd_rd2,gd_wr2 = ud_guard src in
            (if gd_rd2 then [ud_name src] else [])@gds_rd,
            (if gd_wr1 then [ud_name dst] else [])@gds_wr
          | _ -> gds_rd,gds_wr in
        search gds_rd' gds_wr' tl;
      | [] -> gds_rd,gds_wr in
    search [] [] uil in

  let rec update_majorblocks mjb =
    begin
      match mjb.mjb_minorblocks with
      | [] ->
      begin
        (*
        ** No more minor block. Remove major block (?).
        ** Adjust link in next block...
        *)
        match mjb.mjb_next with
        | Some mjb' -> 
          mjb'.mjb_prev <- mjb.mjb_prev;
          if mjb.mjb_prev = None then
          begin
            warning "Empty head major block found. Will be removed...";
            mjb_head := mjb';
          end
          else
            warning (sprintf "Empty major block %d found. Will be removed..."
                             mjb.mjb_id);
        | None -> ();
      end; 
      begin
        (*
        ** and previous block...
        *)
        match mjb.mjb_prev with
        | Some mjb' -> 
          mjb'.mjb_next <- mjb.mjb_next;
        | None -> ();
      end; 
      | _ -> ()
    end;
    match mjb.mjb_next with
    | Some mjb' -> update_majorblocks mjb';
    | None -> () in


  let rec update_minorblocks mjb =
    if mjb.mjb_minorblocks_head.mib_instr = [] then
    begin
      mjb.mjb_minorblocks <- List.tl mjb.mjb_minorblocks;
      match mjb.mjb_minorblocks with
      | [] ->
      begin
        (*
        ** No more minor block. Remove major block (?).
        ** Adjust link in next block...
        *)
        match mjb.mjb_next with
        | Some mjb' -> 
          mjb'.mjb_prev <- mjb.mjb_prev;
          if mjb.mjb_prev = None then
          begin
            warning "Empty head major block found. Will be removed...";
            mjb_head := mjb';
          end
          else
            warning (sprintf "Empty major block %d found. Will be removed..."
                             mjb.mjb_id);
        | None -> ();
      end; 
      begin
        (*
        ** and previous block...
        *)
        match mjb.mjb_prev with
        | Some mjb' -> 
          mjb'.mjb_next <- mjb.mjb_next;
        | None -> ();
      end; 
      | _ -> 
        mjb.mjb_minorblocks_head <- List.hd mjb.mjb_minorblocks;  
    end;
    List.iter (fun mib ->
      mib.mib_guards <- guards mib.mib_instr;
      ) mjb.mjb_minorblocks;
    match mjb.mjb_next with
    | Some mjb' -> update_minorblocks mjb';
    | None -> () in

  let is_immed ud =
    match ud with
    | UC_immed _ -> true;
    | _ -> false in

  (*
  ** Get all jump labels (JUMP/FALSEJUMP)
  ** from instruction list.
  *)    
  let rec get_jumps jumps uil =
    match uil with
    | uc::tl ->
    begin
      match uc.ui_code with
      | Jump ulabel
      | Falsejump (_,ulabel) ->
      begin
        let label =
          match ulabel with
          | UC_label label -> label;
          | UC_next -> get_next_label tl in
        let jumps' = 
          if not (List.mem label jumps) then
            (label::jumps) else jumps in

        get_jumps jumps' tl;
      end;
      | _ -> get_jumps jumps tl;
    end;
    | [] -> jumps in

  (*
  ** Return expression object name
  ** and boolean value "data object"
  *) 
  let rec get_arg_name arg =
    match arg with
    | UC_reg uo
    | UC_var uo
    | UC_sig uo
    | UC_chan uo
    | UC_queue uo -> 
        true,uo.uo_name,
        (let auxl = ref [] in
         List.iter (fun ud -> 
          let _,name,auxl'=get_arg_name ud in
          auxl := !auxl @ [name] @ auxl')uo.uo_sel;
         !auxl)
    | UC_temp ut -> true,sprintf "TEMP_%s" ut.ut_name,[];
    | UC_immed _ 
    | UC_bool _
    | UC_val _ -> false,"",[]
    | UC_alu ua -> true,sprintf "ALU%s" (sprint_dt ua.ua_type),[];  
    | _ -> true,ud_name arg,[] in

  let add_lhs mib arg =
    let is_obj,name,auxl = get_arg_name arg in
    if is_obj && not (List.mem name mib.mib_lhs) then
      mib.mib_lhs <- mib.mib_lhs @ [name];
    List.iter (fun rhs -> 
      if not (List.mem rhs mib.mib_lhs) then
        mib.mib_rhs <- mib.mib_rhs @ [rhs]) auxl;
    in
  let add_rhs mib arg =
    let is_obj,name,auxl = get_arg_name arg in
    if is_obj && not (List.mem name mib.mib_rhs) then
      mib.mib_rhs <- mib.mib_rhs @ [name];      
    List.iter (fun rhs -> 
      if not (List.mem rhs mib.mib_lhs) then
        mib.mib_rhs <- mib.mib_rhs @ [rhs]) auxl;
    in

  (*
  ** Find out from instruction list 
  ** which basic block type comes next..
  *)
  let rec next_kind uil =
    match uil with
    | ui :: tl ->
    begin
      let sched =
        let rec iter pl =
          match pl with
          | (BP_schedule Sched_auto) :: tl -> true;
          | (BP_schedule (Sched_custom sl)) :: tl ->
            let rec iter2 sl =
              match sl with
              | Sched_basicblock :: tl -> true;
              | hd :: tl -> iter2 tl;
              | [] -> iter tl in
            iter2 sl
          | hd::tl -> iter tl;
          | [] -> false in
        iter ui.ui_frame.bf_params in  
      if not sched then MJB_fixed else
      match ui.ui_code with
      | Jump _
      | Falsejump _ -> MJB_control;
      | Move (dst,_)
      | Expr (_,dst,_,_) ->
        if not (is_immed dst) then MJB_data
        else next_kind tl;
      | _ -> next_kind tl;  
    end;
    | [] -> MJB_control in
 
  (*
  ** Check if 'hd' instruction (previous to tl) is in bounded environment...
  **  (hd ::) tl
  *)
  let is_bound tl =
    let rec is_bound uil n =
      match uil with
      | ui :: tl ->
      begin
        match ui.ui_code with
        | Bind n' -> if n' >= n then true else false  
        | _ -> is_bound tl (n+1);  
      end;
      | [] -> false in
    is_bound tl 1 in
   
  let first_frame mib =
    let rec find uil =
      match uil with
      | uc :: tl ->
      begin
        uc.ui_frame
      end;
      | [] -> nilbf in 
    find mib.mib_instr in 
    
  let first_label mib =
    let rec find uil =
      match uil with
      | uc :: tl ->
      begin
        match uc.ui_code with
        | Label l -> l;
        | _ -> find tl;
      end;
      | [] -> "" in 
    find mib.mib_instr in 
  (*
  ** Check for RHS data dependencies of mib from
  ** LHSs of previous minor blocks (mibl) and
  ** insert mib in mibl DDG if required.
  ** Data Dependency Graph Forest:
  **
  **  X: object, E: expression 
  **
  **  I. Forward RHS dependency: (X1<-E1) -> (X2<-{X1,E2})      x<-a+1; ... y<-x-1; 
  **  II. LHS order dependency/sequence: (X1<-E1) -> (X1<-E2)   z<-a; ... z<-b;
  **  III. Backward RHS dependency: (X1<-{X2,E1}) -> (X2<-E2)   y<-x+a; ... x <- 0;
  **  IV. Recursive data dependency: x<-x+1
  **  
  *)
  let link_minor mib mibl =
    debug "basicblock"  with (sprintf "link_minor: #mib=%d" mib.mib_id);
    let lhs = ref mib.mib_lhs in
    let rhs = ref mib.mib_rhs in

    (*
    ** IV.
    *)
    let is_dep4 = ref false in
    List.iter (fun dep ->
          if List.mem dep !lhs then
          begin
            is_dep4 := true;
          end;
          ) !rhs;
    if !is_dep4 then
      mib.mib_childs.mie_recursive <- true;

    let rec iter mibl =
      match mibl with
      | mib' :: tl ->
      begin
        debug "basicblock" with (sprintf "  #mib=%d #mib'=%d" mib.mib_id mib'.mib_id);
        let lhs' = mib'.mib_lhs in
        let rhs' = mib'.mib_rhs in
        let is_dep1 = ref false in
        
        (*
        ** I.
        *)
        List.iter (fun dep ->
          if List.mem dep !lhs then
          begin
            is_dep1 := true;
          end;
          ) rhs';
        if !is_dep1 then
        begin
          if not (List.mem mib' mib.mib_childs.mie_forward_dep) then
            mib.mib_childs.mie_forward_dep <- mib.mib_childs.mie_forward_dep @ [mib'];
          if not (List.mem mib mib'.mib_parents.mie_forward_dep) then
            mib'.mib_parents.mie_forward_dep <- mib'.mib_parents.mie_forward_dep @ [mib];
        end;
        (*
        ** II.
        *)
        let is_dep2 = ref false in
        List.iter (fun dep ->
          if List.mem dep !lhs then
          begin
            is_dep2 := true;
            lhs := List.filter (fun dep' -> dep <> dep') !lhs;
          end;
          ) lhs';
        if !is_dep2 then
        begin
          if not (List.mem mib' mib.mib_childs.mie_order_dep) then
            mib.mib_childs.mie_order_dep <- mib.mib_childs.mie_order_dep @ [mib'];
          if not (List.mem mib mib'.mib_parents.mie_order_dep) then
            mib'.mib_parents.mie_order_dep <- mib'.mib_parents.mie_order_dep @ [mib];
        end;
        (*
        ** III.
        *)
        let is_dep3 = ref false in
        List.iter (fun dep ->
          if List.mem dep !rhs then
          begin
            is_dep3 := true;
            rhs := List.filter (fun dep' -> dep <> dep') !rhs;
          end;
          ) lhs';
        if !is_dep3 then
        begin
          if not (List.mem mib' mib.mib_childs.mie_backward_dep) then
            mib.mib_childs.mie_backward_dep <- mib.mib_childs.mie_backward_dep @ [mib'];
          if not (List.mem mib mib'.mib_parents.mie_backward_dep) then
            mib'.mib_parents.mie_backward_dep <- mib'.mib_parents.mie_backward_dep @ [mib];
        end;        
        iter tl;
      end;
      | [] -> () 
     in
    iter mibl in

  let jumps = get_jumps [] uil in
  
  if debug_it "basicblock" then
  begin
    out_ "JUMPS:";
    List.iter (fun j -> out_ j;) jumps;
  end;

  let next_jump mjb =
    match mjb.mjb_next with
    | Some next ->
      (*
      ** Recognize control block structure like this:
      **   label:
      **    jump label'
      *)
      let n = ref 0 in
      let is_jump = ref false in
      List.iter (fun mib ->
        List.iter (fun uc ->
          if not !is_jump then incr n;
          match uc.ui_code with
          | Jump _ -> is_jump := true; incr n;
          | Nop | Label _ -> is_jump := !is_jump && true;
          | _ -> is_jump := false; 
          ) mib.mib_instr;
        ) next.mjb_minorblocks;
      if !is_jump then !n else 0
    | None -> 0 in
   
   (*
   ** Do MJB contains any jump instruction?
   *)
   let is_jump mjb =
     let is_jump = ref false in
     List.iter (fun mib ->
       List.iter (fun uc ->
          match uc.ui_code with
          | Jump _ | Falsejump _ -> is_jump := true; 
          | _ -> ();
         ) mib.mib_instr;
       ) mjb.mjb_minorblocks;
     !is_jump in
   let jump_info mjb =
     let jn,cjn = ref 0,ref 0 in
     List.iter (fun mib ->
       List.iter (fun uc ->
          match uc.ui_code with
          | Jump _ -> incr jn;
          | Falsejump _ -> incr cjn; 
          | _ -> ();
         ) mib.mib_instr;
       ) mjb.mjb_minorblocks;
     !jn,!cjn in
          
  (*
  ** Do major block partitioning
  *)
  let rec partition_major uil =
    match uil with
    | uc::tl ->
    begin
      let sched =
        let rec iter pl =
          match pl with
          | (BP_schedule Sched_auto) :: tl -> true;
          | (BP_schedule (Sched_custom sl)) :: tl ->
            let rec iter2 sl =
              match sl with
              | Sched_basicblock :: tl -> true;
              | hd :: tl -> iter2 tl;
              | [] -> iter tl in
            iter2 sl
          | hd::tl -> iter tl;
          | [] -> false in
        iter uc.ui_frame.bf_params in 
        
      if sched then
      begin
        match !mjb_head.mjb_kind with
        | MJB_fixed -> 
        begin
          let mjb = !mjb_head in
          let mjb' = new_mjb (next_kind uil) in
          mjb'.mjb_id <- mjb.mjb_id + 1;
          set_next mjb' mjb;
          set_prev mjb mjb';
          mjb_head := mjb';
          incr major_splitted;          
        end;
        | _ -> ();
      end
      else
      begin
        if uc.ui_frame.bf_id <> 0 then
        match !mjb_head.mjb_kind with
        | MJB_control 
        | MJB_data -> 
        begin
          let mjb = !mjb_head in
          let mjb' = new_mjb MJB_fixed in
          mjb'.mjb_id <- mjb.mjb_id + 1;
          set_next mjb' mjb;
          set_prev mjb mjb';
          mjb_head := mjb';
          incr major_splitted;          
        end; 
        | MJB_fixed -> ();      
      end; 
      
      match uc.ui_code with
      | Bind n ->
      begin
        match !mjb_head.mjb_kind with
        | MJB_fixed -> 
          add_uc !mjb_head uc;
          partition_major tl;          
        | MJB_data -> 
          (*
          ** Remove binding of expression with following jump control block if any !
          *)
          let jump_n = next_jump !mjb_head in
          if (n-jump_n) > 1 then
            add_uc !mjb_head {uc with ui_code=Bind (n-jump_n)};
          partition_major tl;
        | MJB_control ->
        begin
          (*
          ** Beginning of control block. Open new block, close
          ** actual control block.
          ** Try to get label before bind instruction, too.
          *)
          add_uc !mjb_head uc;
          let tl' = add_prev_label !mjb_head tl in
          if tl' <> [] then
          begin
            let mjb = !mjb_head in
            let mjb' = new_mjb (next_kind tl') in
            mjb'.mjb_id <- mjb.mjb_id + 1;
            set_next mjb' mjb;
            set_prev mjb mjb';
            mjb_head := mjb';
            partition_major tl';
          end;
        end;
      end;
      | Label label ->
      begin
        if (List.mem label jumps) &&
           !mjb_head.mjb_minorblocks_head.mib_instr <> [] &&
           !mjb_head.mjb_kind <> MJB_fixed then
        begin
          (*
          ** Split major block...
          *)
          let mjb = !mjb_head in
          let mjb' = new_mjb (next_kind tl) in
          mjb'.mjb_id <- mjb.mjb_id + 1;
          add_uc mjb uc;
          set_next mjb' mjb;
          set_prev mjb mjb';
          mjb_head := mjb';
          incr major_splitted;
          partition_major tl;
        end
        else
        begin
          add_uc !mjb_head uc;
          partition_major tl;          
        end;
      end;
      | Nop -> 
      begin
        add_uc !mjb_head uc;
        partition_major tl;
      end;
      | Expr (_,dst,_,_) 
      | Move (dst,_) ->
      begin
        if is_immed dst then
        begin
          add_uc !mjb_head uc;
          partition_major tl;
        end
        else
        begin
          match !mjb_head.mjb_kind with
          | MJB_fixed when not sched -> 
            add_uc !mjb_head uc;
            partition_major tl; 
          | MJB_fixed when sched ->          
            let mjb = !mjb_head in
            let mjb' = new_mjb MJB_data in
            mjb'.mjb_id <- mjb.mjb_id + 1;
            add_uc mjb' uc;
            set_next mjb' mjb;
            set_prev mjb mjb';
            mjb_head := mjb';
            partition_major tl;
          | MJB_control ->
            (*
            ** We must check for a data expression bound to
            ** a control block!!!
            *)
            if not (is_bound tl) then
            begin
              let mjb = !mjb_head in
              let mjb' = new_mjb MJB_data in
              mjb'.mjb_id <- mjb.mjb_id + 1;
              add_uc mjb' uc;
              set_next mjb' mjb;
              set_prev mjb mjb';
              mjb_head := mjb';
              partition_major tl;
            end
            else
            begin
              add_uc !mjb_head uc;
              partition_major tl;
            end;
          | MJB_data ->
            add_uc !mjb_head uc;
            partition_major tl;
        end;
      end;
      | Special _
      | Jump _
      | Falsejump _ 
      | Fun _ ->
      begin
        match !mjb_head.mjb_kind with
        | MJB_fixed when not sched -> 
          add_uc !mjb_head uc;
          partition_major tl;          
        | MJB_control ->
        begin
          let jumps,condjumps = jump_info !mjb_head in 
          let new_ctrl = 
            match uc.ui_code with
            | Jump _ -> jumps > 0 
            | Falsejump _ -> condjumps > 0
            | _ -> false in
          if not new_ctrl then
          begin
            add_uc !mjb_head uc; 
            partition_major tl;
          end
          else
          begin
            (*
            ** mjb_head already contain jumps, open new control major block!
            *)
            let mjb = !mjb_head in
            let mjb' = new_mjb MJB_control in
            mjb'.mjb_id <- mjb.mjb_id + 1;
            add_uc mjb' uc;
            set_next mjb' mjb;
            set_prev mjb mjb';
            mjb_head := mjb';
            partition_major tl;
            
          end;
        end;
        | MJB_fixed when sched ->          
          let mjb = !mjb_head in
          let mjb' = new_mjb MJB_control in
          mjb'.mjb_id <- mjb.mjb_id + 1;
          add_uc mjb' uc;
          set_next mjb' mjb;
          set_prev mjb mjb';
          mjb_head := mjb';
          partition_major tl;
        | MJB_data ->
          let mjb = !mjb_head in
          let mjb' = new_mjb MJB_control in
          mjb'.mjb_id <- mjb.mjb_id + 1;
          add_uc mjb' uc;
          set_next mjb' mjb;
          set_prev mjb mjb';
          mjb_head := mjb';
          partition_major tl;
      end; 
    end;
    | [] -> () in

  (*
  ** Initial basicblock partitioning into data and
  ** control blocks (or fixed without any scheduling)
  *)
  out "Parititioning major blocks...";
  partition_major (List.rev uil);

  (*
  ** Partitioning of basicblocks (major  blocks) into
  ** minor blocks (expression paritioning). Initially
  ** a basicblock only consists of one minor block with
  ** multiple instructions, which
  ** must be splitted into several independent minor
  ** blocks each containing only one expression (or
  ** an explicit bounded expression list).
  ** Additionally each minor block is appended to
  ** the data dependency graph(s).
  *) 
  let rec partition_minor mjb =   
    let rec minor mibl_in mibl_out =
      match mibl_out with
      | mib :: mib_tl ->
      begin
        let rec bind n ucl_in ucl_out =
          match ucl_out with
          | uc :: uc_tl ->
          begin 
            match uc.ui_code with
            | Label _
            | Nop -> 
               bind (max 1 (n-1)) (ucl_in@[uc]) uc_tl;
            | Expr (_,dst,src1,src2) ->
              add_lhs mib dst;    
              add_rhs mib src1;
              add_rhs mib src2;
              
              if is_immed dst then
              begin
                bind (max 1 (n-1)) (ucl_in@[uc]) uc_tl
              end
              else
              begin
                if n = 1 then
                begin
                 let ucl_in',uc_tl' = next_label_end (ucl_in@[uc]) uc_tl in
                 mib.mib_instr <- ucl_in';
                 uc_tl'
                end
                else
                  bind (n-1) (ucl_in@[uc]) uc_tl;   
              end;              
            | Move (dst,src) ->
              add_lhs mib dst;    
              add_rhs mib src;

              if is_immed dst then
              begin
                bind (max 1 (n-1)) (ucl_in@[uc]) uc_tl
              end
              else
              begin
                if n = 1 then
                begin
                  let ucl_in',uc_tl' = next_label_end (ucl_in@[uc]) uc_tl in
                  mib.mib_instr <- ucl_in';
                  uc_tl'
                end
                else
                  bind (n-1) (ucl_in@[uc]) uc_tl;   
              end;
            | Bind n' ->
              bind (n-1+n') (ucl_in@[uc]) uc_tl
            | _ -> error 654565 ""
          end;
          | [] -> [] in
        let ucl_tl = bind 1 [] mib.mib_instr in
        if ucl_tl = [] then minor (mibl_in@[mib]) mib_tl else
        begin
          let mib' = {
            mib_id = mib.mib_id + 1;
            mib_instr = ucl_tl;
            mib_guards = [];
            mib_lhs = [];
            mib_rhs = [];
            mib_childs = {mie_forward_dep=[];mie_order_dep=[];mie_backward_dep=[];mie_recursive=false};
            mib_parents = {mie_forward_dep=[];mie_order_dep=[];mie_backward_dep=[];mie_recursive=false};
            mib_next = [];
            mib_flags = [];
            mib_load = 0;
            } in
          minor (mibl_in@[mib]) (mib'::mib_tl); 
        end;
      end;
      | [] -> mibl_in in
      
    let rec minor_dep mibl =
      match mibl with
      | mib :: mib_tl ->
      begin
        let rec dep ucl =
          match ucl with
          | uc :: uc_tl ->
          begin 
            match uc.ui_code with
            | Expr (_,dst,src1,src2) ->
              add_lhs mib dst;    
              add_rhs mib src1;
              add_rhs mib src2;              
            | Move (dst,src) ->
              add_lhs mib dst;    
              add_rhs mib src;
            | Bind n' -> ();
            | Falsejump (expr,_) ->
              add_rhs mib expr;
            | Fun (_,_,argl) ->
              List.iter (fun arg ->
                match arg with
                | UA_data ud -> 
                  add_rhs mib ud;
                | UA_expr ul ->
                  dep ul) argl;
            | _ -> ();
          end;
          dep uc_tl;
          | [] -> () in
        dep mib.mib_instr; 
        minor_dep mib_tl;     
      end;
      | [] -> () in
    if mjb.mjb_kind = MJB_data then
    begin
      mjb.mjb_minorblocks <- minor [] mjb.mjb_minorblocks;
      mjb.mjb_minorblocks_head <- List.hd mjb.mjb_minorblocks;
    end
    else
    begin
      (*
      ** Only resolve data dependencies...
      *)
      minor_dep mjb.mjb_minorblocks;
    end;
    let rec link_minors mibl =
      match mibl with
      | mib :: mibl_tl ->
        link_minor mib mibl_tl;
        link_minors mibl_tl;
      | [] -> () in
    link_minors mjb.mjb_minorblocks;
    
    match mjb.mjb_next with
    | Some next -> partition_minor next;
    | None -> ();
    in 
    
  out "Parititioning minor blocks...";
  
  partition_minor !mjb_head;
  update_minorblocks !mjb_head;
  
  let rec merge_jumps mjb =
    let next = mjb.mjb_next in
    match mjb.mjb_next with
    | Some next ->
      if (mjb.mjb_kind = MJB_data) && not (is_jump mjb) then
      begin
        if next.mjb_kind = MJB_control then
        begin
          let is_uncond_jump,is_cond_jump,is_fun,is_expr, is_side_jump, is_first = 
            ref false, ref false, ref false, ref false, ref false, ref true in
          List.iter (fun mib ->
            List.iter (fun uc ->
              match uc.ui_code with
              | Jump _ -> is_uncond_jump := true;
              | Falsejump _ -> is_cond_jump := true;
              | Nop ->  ();
              | Label label -> if (List.mem label jumps) then is_side_jump := true;
              | Fun _ -> is_fun := true;
              | _ -> is_expr := true;
              ) mib.mib_instr;
            ) next.mjb_minorblocks;
          if !is_uncond_jump && not !is_cond_jump && not !is_fun && not !is_side_jump then
          begin
            (*
            ** Merge this jump mjb with previous data mjb.
            *)
            let jump_mib = List.hd next.mjb_minorblocks in
            jump_mib.mib_id <- List.length mjb.mjb_minorblocks;
            jump_mib.mib_flags <- jump_mib.mib_flags @ [MIB_jump];
            mjb.mjb_minorblocks <- mjb.mjb_minorblocks @ next.mjb_minorblocks;
            match next.mjb_next with
            | Some next' ->  
              next'.mjb_prev <- Some mjb;
              mjb.mjb_next <- Some next';
              merge_jumps next';
            | None -> 
              mjb.mjb_next <- None;
          end
          else
            merge_jumps next;
        end
        else
          merge_jumps next; 
      end
      else 
        merge_jumps next;
    | None -> ();    
    in
    
  out "Merging unconditional jumps...";
  merge_jumps !mjb_head;   

  (*
  ** Return sorted list of all minorblocks
  ** found in one forest
  *)
  let ddg_get_mibl root =
    let mibl = ref [] in
    let rec iter mib =
      if not (List.mem mib !mibl) then
        mibl := !mibl @ [mib];
      List.iter iter (mib.mib_childs.mie_forward_dep@
                      mib.mib_childs.mie_order_dep@
                      mib.mib_childs.mie_backward_dep) in
    iter root;
    List.sort (fun a b -> if a > b then 1 else -1) !mibl in 

  let ddg_get_childs mib =
    let mibl = ref [] in
    let add mib =
      if not (List.mem mib !mibl) then
        mibl := !mibl @ [mib] in
    
    List.iter add (mib.mib_childs.mie_forward_dep@
                   mib.mib_childs.mie_order_dep@
                   mib.mib_childs.mie_backward_dep);
    List.sort (fun a b -> if a > b then 1 else -1) !mibl in 

  (*
  ** Return #id list of all path nodes starting
  ** from root
  *)
  let ddg_paths root =
    let idl = ref [root.mib_id] in
    let rec iter nextl = 
      match nextl with
      | next :: tl ->
        if not (List.mem next.mib_id !idl) then 
          idl := !idl @ [next.mib_id]; 
        iter next.mib_next;
        iter tl;
      | [] -> () in
    iter root.mib_next;
    !idl in
    
  (*
  ** Examine two path lists for interconnections
  *)  
  let ddg_connect pl1 pl2 =
    let rec iter1 pl1 =
      match pl1 with
      | p :: tl -> 
        if List.mem p pl2 then true
        else iter1 tl;
      | [] -> false in
    iter1 pl1 in
    
  let rec link_minors mjb =
    debug "basicblock"  with (sprintf "link_minros MJB=%d" mjb.mjb_id);
    (*
    ** Now link all minorblock within one forest in
    ** default order...
    *)
    let rec iter mibl =
        match mibl with
        | mib :: tl ->
          mib.mib_next <- ddg_get_childs mib;
          iter tl;
        | _ -> () in
    iter mjb.mjb_minorblocks;
    match mjb.mjb_next with
    | Some next -> link_minors next;
    | None -> ();
    in 
   

  let new_ddg mjb =
    let root = {(new_mib (-1)) with mib_flags=[MIB_scheduled]} in
    {
      ddg_root = root;
      ddg_schedule = [root];
    } in      
    
  (*
  ** Returns (longest) path length for path next-target, returns 0 if there is
  ** no path.
  *)
  let rec ddg_path_length next target =
    let reached = ref false in
    let nextnext = next.mib_next in
    let longestpath = ref 0 in
    List.iter (fun next' ->
      longestpath := max !longestpath
                      (if next'.mib_id = target.mib_id then 
                        begin
                          reached := true; 
                          1
                        end
                        else
                        begin
                         let l' = ddg_path_length next' target in
                         if l' > 0 then 1+l' else 0
                        end;
                      );
      ) nextnext;
    if not !reached then 0 else !longestpath in
    
  let ddg_path_length next target =
      let l = ddg_path_length next target in
      debug "basicblock" with (sprintf "ddg_path_length: %d -> %d = %d" 
            next.mib_id target.mib_id l);
      l in

  let ddg_first last =
    match last.mib_next with
    | hd :: tl -> [hd];
    | [] -> [] in

  let ddg_rest last =
    match last.mib_next with
    | hd :: tl -> tl;
    | [] -> [] in
        

  link_minors !mjb_head;
  
  if debug_it "basicblock" then print_mjb !mjb_head;  

  (*
  ** Extract DDG forest for each major
  ** block.
  *)
  let rec build_forest mjb =
    debug "basicblock" with (sprintf "build_forest: MJB=%d" mjb.mjb_id);
    
    let root mib =
      mib.mib_parents.mie_forward_dep = [] &&
      mib.mib_parents.mie_order_dep = [] &&
      mib.mib_parents.mie_backward_dep = [] in
    let empty mib =
      let nonsens = ref true in
      List.iter (fun uc ->
         match uc.ui_code with
         | Nop | Label _ -> nonsens := !nonsens & true;
         | _ -> nonsens := false;
        ) mib.mib_instr;
      !nonsens in
      
    if mjb.mjb_kind = MJB_data then
    begin
      mjb.mjb_minorblocks <- 
        List.filter (fun mib ->
          if (root mib)&& (empty mib)  then
            false (* means: empty MIB, only nops and labels! Kick it out  *)
          else
            true
          ) mjb.mjb_minorblocks;
      (*
      ** Now build the forest(s).
      ** Filter out root mibs. Roots without any interconnections
      ** build up a new forest!
      *)
      let roots = List.filter (fun mib -> root mib) mjb.mjb_minorblocks in
      let paths = List.map (fun root -> root,ddg_paths root) roots in
      let rec join paths =
        match paths with
        | (root,path) :: tl -> 
          let ddg = new_ddg mjb in
          mjb.mjb_ddg_forest <- mjb.mjb_ddg_forest @ [ddg];
          ddg.ddg_root.mib_next <- [root];
          let same,indep = List.partition (fun (root',path') -> ddg_connect path path') tl in
          ddg.ddg_root.mib_next <- ddg.ddg_root.mib_next @
            (List.map (fun (root',path') -> root') same);
          join indep;
        | [] -> () in
      join paths;
    end
    else
      List.iter (fun mib ->
        if (root mib) then
        begin
            mjb.mjb_ddg_forest <- mjb.mjb_ddg_forest @ [
              let ddg = new_ddg mjb in
              ddg.ddg_root.mib_next <- [mib];
              ddg];
        end
        ) mjb.mjb_minorblocks;
    if debug_it "basicblock" then
    begin
      List.iter (fun ddg ->
        debug "basicblock" with (sprintf "MJB <%d>: DDG=[%s]"
          mjb.mjb_id
          (
            let s = ref "" in
            let first = ref true in
            if ddg.ddg_root.mib_id = (-1) then
              List.iter (fun next ->
                if !first then s := sprintf "%d" next.mib_id
                else s := sprintf "%s,%d" !s next.mib_id;
                first := false;
                ) ddg.ddg_root.mib_next
            else s := sprintf "%d" ddg.ddg_root.mib_id;
            !s
          ));
        ) mjb.mjb_ddg_forest; 
    end;
  
    match mjb.mjb_next with
    | Some next -> build_forest next;
    | None -> ();
    in 

  build_forest !mjb_head;
  update_majorblocks !mjb_head; 
  
  let major_c,major_d,major_f=ref 0,ref 0, ref 0 in

  let length head =
    let rec iter mjb minor_n major_n =
      match mjb.mjb_next with
      | Some mjb' -> 
      begin
        match mjb.mjb_kind with
        | MJB_fixed -> incr major_f;
        | MJB_control -> incr major_c;
        | MJB_data -> incr major_d;  
      end;
      iter mjb' (minor_n + (List.length mjb.mjb_minorblocks)) (major_n+1);
      | None -> 
      begin
        match mjb.mjb_kind with
        | MJB_fixed -> incr major_f;
        | MJB_control -> incr major_c;
        | MJB_data -> incr major_d;          
      end;
      (minor_n + (List.length mjb.mjb_minorblocks)),(major_n+1) in
    iter head 0 0 in
  let minor_n,major_n = length !mjb_head in
  out (sprintf "Found %d major (%d splitted, CDF=%d,%d,%d) and %d minor block(s) with %d jump(s)." 
                major_n !major_splitted !major_c !major_d !major_f minor_n (List.length jumps));
    
  (*
  ** Schedule as much as possible independent assignments into one time step.
  ** Maximal one guarderd expression may appear in one time step.
  ** Scheduling algorithm:
  **
  **  Rule 1: longest path first
  **        -> there may be several paths from one node A to another node B
  **        -> schedule the longest paths first/only
  **        -> vice versa: if actually path L1(A,B) is scheduled,
  **           a second path L2(A,C) can be also scheduled iff
  **           there is no path from L1 to L2(B,C)!
  **
  **  Rule 2: Time step n has a set of schedulable mibs from one ddg
  **  Rule 3: Time step n+1 has a successor set of schedulable mibs
  **          derived from all mib_next lists of previous scheduled mibs.
  **          Rule 1 must be satisfied.
  **
  **  Rule 4: There may be only one guarded expression inside bounded block
  **  Rule 5: If there is a guarded expression, no other expression with
  **          object recursion may be present
  **
  **  Rule 6: All forbidden (Rule 4/5) objects are delayed to the next
  **          time unit and scheduled only in this time unit
  **
  **  Example:          Dep.          Next
  **    1: a <- ...     O4 F3         3,4
  **    2: b <- ...     O5 F3         3,5
  **    3: c <- a+b     B4 B5 F6      4,5,6
  **    4: a <- ...     F6            6
  **    5: b:<- ...     F6            6
  **    6: d <- a+b+c
  **    
  **
  **  Time      Scheduled mib 
  **  1         1,2             
  **  2         3               Path 1-3/2-3 are the same
  **  3         4,5             Path 3-6 has length 1, Paths 3-4-6/3-5-6 has length 2!
  **  4         6
  **
  ** After block bounding, all jumps must be relocated to new block
  ** start label BLOCKBOUNDxx!
  *)            
  let relocate = ref [] in
  
  let tot_blocks = ref 0 in
  let tot_instr = ref 0 in
  
  let rec schedule mjb =
    let forest = mjb.mjb_ddg_forest in
    let bound = ref 1 in
    if mjb.mjb_kind = MJB_data then
    begin
      
      debug "basicblock" with (sprintf "schedule: MJB=%d" mjb.mjb_id);
      let is_scheduled mib =
        List.mem MIB_scheduled mib.mib_flags in
      
      (*
      ** Get set of next schedulable mibs from list of previously
      ** scheduled set. The first mib of mib_next list is used always
      ** because it's the next in original mib order (lowest mib_id in mib_next list).
      *)
      let ddg_next ddg =
        let is_forbidden mib = List.mem MIB_jump mib.mib_flags in
        
        let last_set = ddg.ddg_schedule in
        let to_sched = ref [] in
        List.iter (fun mib ->
          let first,rest = ddg_first mib,ddg_rest mib in
          
          List.iter (fun mib' ->
              if not (List.mem mib' !to_sched) && not (is_forbidden mib') then
                to_sched := !to_sched @ [mib'];) first;
          let rec iter nextl' =
            match nextl' with
            | mib' :: tl ->
              (*
              ** There may be no path from already scheduled mibs
              ** to this new candidate!
              *)
              let pathl = List.filter (fun sched ->  (ddg_path_length sched mib') <> 0 ) !to_sched in
              if pathl = [] && not (is_forbidden mib') && not (List.mem mib' !to_sched) then
                to_sched := !to_sched @ [mib'];
              iter tl;
            | [] -> ()
            in
          iter rest;
          ) last_set;
        ddg.ddg_schedule <- !to_sched in
         
      (*
      ** Group mibs to scheduling blocks...
      *)
      let start_label = ref "" in
      let block = ref [] in
      let start = ref true in
      let scheduled = ref 0 in
      let mib_total = List.length mjb.mjb_minorblocks in
      let mib_first = List.hd mjb.mjb_minorblocks in
      let mib_last = List.hd (List.rev mjb.mjb_minorblocks) in
      let mib_last_is_jump = List.mem MIB_jump mib_last.mib_flags in
  
      let add mib =
        debug "basicblock" with (sprintf "add mib=%d mjb=%d" mib.mib_id mjb.mjb_id);
        block := !block @ [mib];
        mib.mib_flags <- MIB_scheduled :: mib.mib_flags;
        tot_instr := !tot_instr + mib.mib_load;
        incr scheduled;
        in

      let ddg_unscheduled mib_root =
        debug "basicblock"  with (sprintf "ddg_unscheduled: mjb=%d" mjb.mjb_id);
        let to_sched = ref [] in
        let rec find_next mib =
          let scheduled = List.mem MIB_scheduled mib.mib_flags in
          if not scheduled then
            to_sched := !to_sched @ [mib];

          debug "basicblock" with (sprintf "  ddg_unscheduled: mib=%d scheduled=%b" mib.mib_id scheduled);

          match mib.mib_next with
          | next :: tl ->
            find_next next; 
          | [] -> () in
        find_next mib_root;
        debug "basicblock" with (sprintf "  ddg_unscheduled: found=%d" (List.length !to_sched));
        !to_sched in
      
      let ddg_next ddg =
        debug "basicblock" with (sprintf "ddg_next: MJB=%d root=%s" mjb.mjb_id (print_ddg ddg));
        ddg_next ddg; (* returns only allowed scheduleable mibs *)  
        let nl = ddg.ddg_schedule in      
        if debug_it "basicblock" then
          List.iter (fun n -> debug "basicblock" with (sprintf "  ddg_next: got %d" n.mib_id)) nl;
        nl in
             
        
      while !block <> [] || !start 
      do
        let guarded = ref 0 in
        let guarded_mib = ref None in
        block := [];
        (*
        ** Maybe some remains from previous pass? Must be scheduled NOW and only those!
        *)
        let unscheduled = List.filter (fun mib -> not (List.mem MIB_scheduled mib.mib_flags))
                            (List.concat (List.map (fun ddg -> ddg.ddg_schedule) forest)) in  
        if unscheduled <> [] then
          List.iter (fun mib -> add mib) unscheduled
        else
        List.iter (fun ddg ->
          (*
          ** Find next unscheduled mib(s) in ddg not violating control dependencies (guards)
          *)
          let schedulable = ddg_next ddg in
          let rec iter ddg_mibl =
            match ddg_mibl with
            | ddg_mib :: ddg_tl ->
              (*
              ** Fullfill condition: rule 4
              *)
              let is_guarded = ddg_mib.mib_guards <> [] in
              (*
              ** Fullfill condition: rule 5
              *)
              let pre_recursion =
                let r = ref false in
                List.iter (fun mib' ->
                  r := !r || mib'.mib_childs.mie_recursive ) !block;
                !r in
                
              if is_guarded && !guarded = 0 && not pre_recursion then 
              begin
                add ddg_mib;
                incr guarded;
                guarded_mib := Some ddg_mib;
              end 
              else if not is_guarded && !guarded = 1 && not ddg_mib.mib_childs.mie_recursive then
                add ddg_mib                
              else if not is_guarded && !guarded = 0 then
                add ddg_mib;
                
              iter ddg_tl;
            | [] -> () in
          iter schedulable;
          ) forest; 

         
        if (!scheduled = mib_total-1) && mib_last_is_jump then
          add mib_last;
          
        if !block = [] then
        begin
          (*
          ** Maybe there is one final (JUMP) mib to be scheduled. Get it.
          *)
          List.iter (fun ddg ->
            let schedulable = ddg_unscheduled ddg.ddg_root in
            List.iter add schedulable ) forest;
          match !block with
          | [hd] -> ();
          | [] -> ();
          | _ -> error 0 (sprintf "schedule: MJB=%d: more than one not allowed but unscheduled mib found." mjb.mjb_id);
        end;
        
        if !block <> [] then
        begin
          (*
          ** Merge mibs to one bounded block, and apply new bounded block frame to all
          ** included subblocks...
          *)
          incr tot_blocks;
          let bfl = ref [] in
          let src_start,src_end = {s_file="";s_line=999999;s_cpos=0},
                                  {s_file="";s_line=0;s_cpos=0} in
        
          (*
          ** Create linear list of all independent data and compound blocks
          ** used from instruction list in this block.
          *)
          List.iter (fun mib -> 
            List.iter (fun uc ->
              let bf = uc.ui_frame in
(*
              match bf.bf_type with
              | BF_data | BF_compound ->
                if not (List.mem uc.ui_frame !bfl) then
                begin
                  if src_start.s_line > uc.ui_frame.bf_src_start.s_line then
                    src_copy uc.ui_frame.bf_src_start src_start; 
                  if src_end.s_line < uc.ui_frame.bf_src_end.s_line && 
                     src_end.s_line > 0 then
                    src_copy uc.ui_frame.bf_src_end src_end; 
                  bfl := !bfl @ [uc.ui_frame];
                end;
              | _ -> 
*)
                (*
                ** Must be splitted to renamed blockframe!
                *)
                let bf' = {bf with bf_type=BF_data; 
                                   bf_name = sprintf "%s_BLOCK%d" bf.bf_name !tot_blocks;} in
                let rec add bfl'' =
                  match bfl'' with
                  | bf'' :: tl ->
                    if bf' = bf'' then
                    begin
                      uc.ui_frame <- bf'';  (* use physical original blockframe! *)
                    end 
                    else add tl; 
                  | [] ->
                    if src_start.s_line > uc.ui_frame.bf_src_start.s_line &&
                       uc.ui_frame.bf_src_start.s_line > 0 then
                      src_copy uc.ui_frame.bf_src_start src_start; 
                    if src_end.s_line < uc.ui_frame.bf_src_end.s_line && 
                       uc.ui_frame.bf_src_end.s_line > 0 then
                      src_copy uc.ui_frame.bf_src_end src_end;
                    uc.ui_frame <- bf';
                    bfl := !bfl @ [uc.ui_frame]; in
                add !bfl;
              ) mib.mib_instr
            ) !block;
          if src_end.s_line = 0 then src_copy src_start src_end;
          
          let bfl = 
            let comp = List.filter (fun bf -> bf.bf_type = BF_compound) !bfl in
            (*
            ** Filter out only blockframes with parent blockframe not contained in bfl.
            *)
            (List.filter (fun bf ->
              let rec found bfl =
                match bfl with
                | bf' :: tl ->
                begin
                  match bf.bf_parent with
                  | Some bfp -> 
                    if bf' <> bfp then
                      found tl
                    else false;
                  | None -> true;
                end;
                | [] -> true in
              found comp;
              ) !bfl) 
            in
          let rec bf_parent bfl = 
            match bfl with
            | bf :: tl ->
            begin
              match bf.bf_parent with
              | Some bf -> bf;
              | None -> bf_parent tl;
            end;
            | [] ->
              error 650581 "Bounded blockframe without parent?" 
            in
          let bf_parent = bf_parent bfl in
          let bf_bound = {bf_id= !tot_blocks;
                          bf_name=sprintf "BLOCKBOUND%d_%d" mjb.mjb_id !bound;
                          bf_src_start=src_start;
                          bf_src_end=src_end;
                          bf_parent=Some bf_parent;
                          bf_childs= bfl;
                          bf_time=FT_0;
                          bf_loop=FT_0,FT_0;
                          bf_type=BF_compound;
                          bf_params=[BP_bind]}  in
                          
          if (debug_it "basicblock") then
          begin
            debug "basicblock" with (sprintf "new BLOCKBOUND%d_%d" mjb.mjb_id !bound);
            debug "basicblock" with  (sprintf "PARENT: %s#%d" bf_parent.bf_name bf_parent.bf_id);
            
            List.iter (fun bf ->
              debug "basicblock" with (sprintf "BOUNDBLOCK member: %s#%d" bf.bf_name bf.bf_id);
              ) bfl;
          end;
          List.iter (fun bf ->
            bf.bf_parent <- Some bf_bound;
            ) bfl;
          bf_parent.bf_childs <- List.filter (fun bf ->
             not (List.mem bf bfl)
            ) bf_parent.bf_childs;
          bf_parent.bf_childs <- bf_parent.bf_childs @ [bf_bound];

          let label_name = sprintf "BLOCKBOUND%d_%d" mjb.mjb_id !bound in
          if !start then relocate := !relocate @ [first_label mib_first,label_name];
          let label = {ui_code=Label label_name;
                       ui_frame=bf_bound} in
          let bil = ref [] in
          (*
          ** Filter out and flatten mib list to instruction list.
          ** Last mib can be an unconditional jump (not scheduled/forbidden!); must be merged
          ** with last data block!
          *)
          debug "basicblock" with (sprintf "BLOCKBOUND%d_%d: creating flattend instruction list in bounded block: scheduled=%d mib_total=%d mib_last_is_jump=%b" 
                                  mjb.mjb_id !bound !scheduled mib_total mib_last_is_jump);
          List.iter (fun mib -> 
            List.iter (fun ui ->
              match ui.ui_code with
              | Bind _ -> ();
              | _ -> bil := !bil @ [ui];
              ) mib.mib_instr;
            ) !block;
            (*
              (!block @ (if !scheduled = mib_total-1 && mib_last_is_jump
                         then [mib_last] else []));
            *)
          let n = List.length !bil in
          let bind = {ui_code=Bind n;
                      ui_frame=bf_bound} in
          incr bound;
          uil' := !uil' @ [label;bind];
          uil' := !uil' @ !bil;
        end;
        start := false;
      done;     
    end
    else
    begin
      List.iter (fun mib ->
        uil' := !uil' @ mib.mib_instr;
        ) mjb.mjb_minorblocks;
    end;
    match mjb.mjb_next with
    | Some next -> schedule next;
    | None -> ();
    in 
  schedule !mjb_head;
  out (sprintf "Created %d bounded block(s)." !tot_blocks);
  
  let jump_relocated = ref 0 in
  let rec jump_relocate mjb =
    let rec relocate_ul rl ul =
      match rl with
      | (l,l') :: tl ->
        if (UC_label l) = ul then (incr jump_relocated; UC_label l')
        else relocate_ul tl ul;
      | [] -> ul in
    List.iter (fun mib ->
        List.iter (fun uc ->
            match uc.ui_code with
            | Jump ul -> 
              uc.ui_code <- Jump (relocate_ul !relocate ul);
            | Falsejump (ud,ul) ->
              uc.ui_code <- Falsejump (ud,relocate_ul !relocate ul);
            | _ -> (); 
            ) mib.mib_instr;
      ) mjb.mjb_minorblocks;
    match mjb.mjb_next with
    | Some next -> jump_relocate next;
    | None -> ();
    in 
    
  if !relocate <> [] then
  begin
    out "Relocating jumps to new bounded blocks...";
    if debug_it "basicblock" then
    begin
      debug "basicblock" with "Relocation table:";
      List.iter (fun (l1,l2) -> out (sprintf "  %s => %s" l1 l2)) !relocate; 
    end;
    jump_relocate !mjb_head;
    out (sprintf "%d jump(s) relocated." !jump_relocated);
  end;
  
  if debug_it "basicblock" then print_mjb !mjb_head;  

  ind_decr ();
  !uil'

