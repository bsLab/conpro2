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
**    $CREATED:     1.3.2006
**    $VERSION:     2.08
**
**    $INFO:
**
**  Program and MicroCode Optimization 
**  
**
**    $ENDOFINFO
**
*)

open Cp_common
open Cp_symbol
open Cp_types
open Cp_utils
open Cp_data
open Cp_analysis
open Cp_syntax
open Cp_expr
open Cp_print
open Cp_printtypes
open Printf
open Cp_stat

  
let optimize_expr main =    
    out "Optimizer: folding constants in expressions...";
    ind_incr ();

    let fold_pro md =
      List.iter (fun pro ->
          reduced := 0;
          out (sprintf "in process <%s.%s>" 
                       md.mod_name 
                       pro.pro_name);
          pro.pro_instr <- List.map (fun pi ->
                expr_fold pi;
                ) pro.pro_instr;
          ind_incr ();
          out (sprintf "Removed %d operand(s)." !reduced); 
          ind_decr ();
        ) md.mod_procs in
    fold_pro main;
    List.iter (fun md -> fold_pro md) main.mod_external;
    ind_decr ();
    !reduced


(*
** Code motion and optimization (mainly movement of loop invariant
** instructions)
*)
let moved = ref 0 

(*
** All instruction are encapsulated into (nested) frames. Each frame
** contains data dependencies.
*)
type f_state =
  | F_original
  | F_moved
  | F_removed
type f_kind =
  | F_fixed
  | F_floating
  | F_block
  | F_instr
    
type frame = {
    mutable f_name: string;
    mutable f_source: source;
    mutable f_state: f_state;
    mutable f_prev: frame option;
    mutable f_next: frame option;
    mutable f_childs: frame list;
    mutable f_parent: frame option;
    mutable f_instr: instr;
    mutable f_readdep: string list;
    mutable f_writedep: string list;
    (*
    ** Kind of frame:
    ** Enables or prevents code motion beyond this barrier.
    *)
    mutable f_kind: f_kind;
}

let rec to_frame parent prev pil =
  let extend_source f =
    (*
    ** Get source from child
    *)
    match f.f_childs with
    | [f'] -> f'.f_source
    | _ -> f.f_source in
    
  let rec dep right pi =
      match pi with
      | PI_obj (opl,ot) -> 
          if not (is_value ot) then 
          begin
            let pi_name = name_of_pi pi in
            if right then
              (
                if is_sel_obj opl then
                  [name_of_pi (obj_sel_obj opl)]
                else
                  []
              )
              @ [pi_name]
            else
              [pi_name] 
          end
            else [];
      | PI_bool (_,_,op1,op2) 
      | PI_arithm (_,op1,op2) -> 
          if right then 
          begin
              (dep right op1) @
              (dep right op2)
          end
          else [];
      | PI_assign (src,lhs,rhs) ->
          line src;
          if right then dep right rhs
                   else dep right lhs;
      | PI_map (src,lhs,rhs) ->
          line src;
          if right then dep right rhs
                   else dep right lhs;
      | _ -> []
      in
      
  (*
  ** Only expressions with registers and variables can
  ** be moved.
  *)
  let rec fixed pi =
      match pi with
      | PI_obj (opl,ot) -> 
        if not (is_value ot) then 
        begin
          match ot with
          | OT_signal _ -> true;
          | _ -> false;
        end
        else
          false;
      | PI_bool (_,_,op1,op2) 
      | PI_arithm (_,op1,op2) -> 
        (fixed op1) ||
        (fixed op2)
      | PI_assign (src,lhs,rhs) ->
        line src;
        (fixed lhs) ||
        (fixed rhs)
      | PI_map (src,lhs,rhs) ->
        line src;
        false;
      | _ -> false
      in
    
  match pil with
  | pi::tl ->
  begin
      match pi with
      | PI_forloop (src,expr,dir,lim1,lim2,block) ->
        line src; 
          let f = {
              f_name = "PI_forloop";
              f_source = source ();
              f_state = F_original;
              f_prev=prev;
              f_next=None;
              f_childs=[];
              f_parent=parent;
              f_instr=pi;
              f_readdep=(dep true lim1) @
                        (dep true lim2);
              f_writedep=(dep false expr);
              f_kind=F_floating;
              } in
          f.f_childs <- (
              match to_frame (Some f) None [block] with
              | Some f -> [f];
              | None -> []);
          f.f_next <- to_frame parent (Some f) tl;
          Some f
      | PI_loop (src,kind,expr,block) -> 
        line src; 
          let f = {
              f_name = "PI_loop";
              f_source = source ();
              f_state = F_original;
              f_prev=prev;
              f_next=None;
              f_childs=[];
              f_parent=parent;
              f_instr=pi;
              f_readdep=(dep true expr);
              f_writedep=[];
              f_kind=F_floating;
              } in
          f.f_childs <- (
              match to_frame (Some f) None [block] with
              | Some f -> [f];
              | None -> []);
          f.f_next <- to_frame parent (Some f) tl;
          Some f
      | PI_branch (src,expr,b1,b2) ->
        line src; 
          let f = {
              f_name = "PI_branch";
              f_source = source ();
              f_state = F_original;
              f_prev=prev;
              f_next=None;
              f_childs=[];
              f_parent=parent;
              f_instr=pi;
              f_readdep=(dep true expr);
              f_writedep=[];
              f_kind=F_fixed;
              } in
          f.f_childs <- (
              match to_frame (Some f) None [b1] with
              | Some f -> [f];
              | None -> []) @
              (
              match to_frame (Some f) None [b2] with
              | Some f -> [f];
              | None -> []);
          f.f_next <- to_frame parent (Some f) tl;
          Some f
      | PI_select (src,expr,case) ->
        line src; 
          let f = {
              f_name = "PI_select";
              f_source = source ();
              f_state = F_original;
              f_prev=prev;
              f_next=None;
              f_childs=[];
              f_parent=parent;
              f_instr=pi;
              f_readdep=(dep true expr);
              f_writedep=[];
              f_kind=F_fixed;
              } in
          let casel =
            match case with
            | PI_list cl -> cl;
            | _ -> error 11544 "" in
          f.f_childs <- List.map (fun case ->
              match to_frame (Some f) None [case] with
              | Some f -> f;
              | None -> error 11545 "") casel;
          f.f_next <- to_frame parent (Some f) tl;
          Some f
      | PI_case (src,exprl,b) ->
        line src; 
        let rd = ref [] in
        List.iter (fun expr -> rd := !rd @ (dep true expr)) exprl;
          let f = {
              f_name = "PI_case";
              f_source = source ();
              f_state = F_original;
              f_prev=prev;
              f_next=None;
              f_childs=[];
              f_parent=parent;
              f_instr=pi;
              f_readdep= !rd;
              f_writedep=[];
              f_kind=F_fixed;
              } in
          f.f_childs <-
            (
              match to_frame (Some f) None [b] with
              | Some f' -> [f'];
              | None -> error 355694 "";
            );
          f.f_next <- to_frame parent (Some f) tl;
          Some f
      | PI_try (b,case) ->
          let f = {
              f_name = "PI_try";
              f_source = source ();
              f_state = F_original;
              f_prev=prev;
              f_next=None;
              f_childs=[];
              f_parent=parent;
              f_instr=pi;
              f_readdep=[];
              f_writedep=[];
              f_kind=F_fixed;
              } in
          let casel =
            match case with
            | PI_list cl -> cl;
            | _ -> error 11545 "" in
          f.f_childs <- (
              match to_frame (Some f) None [b] with
              | Some f -> [f];
              | None -> []) @ (List.map (fun case ->
              match to_frame (Some f) None [case] with
              | Some f -> f;
              | None -> error 11546 "") casel);
          f.f_next <- to_frame parent (Some f) tl;
          Some f
      | PI_block (pil',bf') -> 
          let f = {
              f_name = "PI_block";
              f_source = source ();
              f_state = F_original;
              f_prev=prev;
              f_next=None;
              f_childs=[];
              f_parent=parent;
              f_instr=pi;
              f_readdep=[];
              f_writedep=[];                
              f_kind=F_block;
              } in
          f.f_childs <-
            (
              match to_frame (Some f) None pil' with
              | Some f' -> [f'];
              | None -> error 355693 "";
            );

          if List.mem BP_bind bf'.bf_params then
              f.f_kind <- F_fixed;
          f.f_source <- extend_source f;

          f.f_next <- to_frame parent (Some f) tl;
          Some f

      | _ -> 
        let rd,wd = dep true pi,
                    dep false pi in
        let f = {
              f_name = "Others";
              f_source = source ();
              f_state = F_original;
              f_prev=prev;
              f_next=None;
              f_childs=[];
              f_parent=parent;
              f_instr=pi;
              f_readdep=rd;
              f_writedep=wd;                
              f_kind=if (fixed pi) then F_fixed else F_instr;
              } in
          f.f_next <- to_frame parent (Some f) tl;
          Some f
  end;
  | [] -> None

let rec of_frame frame = 
  let rec fix_instr f =
    (*
    ** Rebuild block/instruction list 
    *)
    line f.f_source;
    match f.f_instr with
    | PI_forloop (src,expr,dir,lim1,lim2,block) ->  
        let block' = fix_instr (List.hd f.f_childs) in
        PI_forloop (src,expr,dir,lim1,lim2,block');
    | PI_loop (src,kind,expr,block) ->
        let block' = fix_instr (List.hd f.f_childs) in
        PI_loop (src,kind,expr,block');
    | PI_branch (src,expr,b1,b2) ->
        let b1' = fix_instr (List.nth f.f_childs 0) in
        let b2' = fix_instr (List.nth f.f_childs 1) in
        PI_branch (src,expr,b1',b2');
    | PI_select (src,expr,case) ->
        let case' = List.map fix_instr f.f_childs in
        PI_select (src,expr,PI_list case');
    | PI_try (b,case) ->
        let b' = fix_instr (List.hd f.f_childs) in
        let case' = List.map fix_instr (List.tl f.f_childs) in
        PI_try (b',PI_list case');
    | PI_case (src,expr,block) ->
        let block' = fix_instr (List.hd f.f_childs) in
        PI_case (src,expr,block');
    | PI_block (pil,bf) ->
        let pil' = of_frame (Some (List.hd f.f_childs)) in
        PI_block (pil',bf);
    | _ -> f.f_instr in

  match frame with
  | Some f ->
    f.f_instr <- fix_instr f;
    [f.f_instr] @ (of_frame f.f_next); 
  | None -> []


let rec frame_fix frame = 
  let rec adjust_top f =
    match f.f_prev with
    | Some f' -> adjust_top f';
    | None -> f; in

  let rec fix_instr f =
    (*
    ** Adjust frame lists to new top frame if code motion 
    ** occurs before the actual top frame.
    *)
    match f.f_instr with
    | PI_forloop _
    | PI_loop _
    | PI_block _ ->
        let b = List.hd f.f_childs in
        frame_fix (Some b);
        f.f_childs <- [adjust_top b];
    | PI_branch _ ->
        let b1 = List.nth f.f_childs 0 in
        let b2 = List.nth f.f_childs 1 in
        frame_fix (Some b1);
        frame_fix (Some b2);
        f.f_childs <- [adjust_top b1;adjust_top b2];
    | _ -> () in

  match frame with
  | Some f ->
    line f.f_source;
    fix_instr f;
    frame_fix f.f_next;
  | None -> ()


(*
** Extend read and write dependencies up to
** frame boundaries: F_float/F_fixed
*)
let rec frame_extend frame = 
  let rec extend f rd wd =
     match f.f_parent with
      | Some p ->
      begin
        p.f_readdep <- p.f_readdep @ rd;
        p.f_writedep <- p.f_writedep @ wd;
        extend p rd wd;
      end;
      | None -> () in
  
  let rec find_leafes f =
    match f with
    | Some f ->
    begin
      line f.f_source;
      match f.f_kind with
      | F_instr -> 
        [f] @ (find_leafes f.f_next)
      | _ -> 
        let lfl = ref [] in
        List.iter (fun c ->
            lfl := !lfl @ (find_leafes (Some c))) f.f_childs;
        !lfl @ (find_leafes f.f_next)
    end
    | None -> [] in

  match frame with
  | Some f ->
    line f.f_source;
    let leafes = find_leafes frame in
    List.iter (fun f ->
      extend f f.f_readdep f.f_writedep
      ) leafes;
  | None -> ()

let rec frame_loop_motion frame =
  let move f p = 
   (*
   ** Move instruction/block frame f before p...
   *)
   line f.f_source;
   let next = f.f_next in
   let prev = f.f_prev in
   if next <> None then
   begin
     match next with
     | Some n -> n.f_prev <- prev;
     | None -> error 612741 "";
   end;
   if prev <> None then
   begin
     match prev with
     | Some p -> p.f_next <- next;
     | None -> error 612742 "";
   end;
   let f' = {f with f_prev=None} in
   f.f_state <- F_removed;
   f'.f_state <- F_moved;
   let prev = p.f_prev in
   if prev <> None then
   begin
     match prev with
     | Some p -> 
       p.f_next <- Some f';
       f'.f_prev <- Some p;
     | None -> error 612742 "";
   end
   else f'.f_prev <- None;
   p.f_prev <- Some f';
   f'.f_next <- Some p;
   f'.f_parent <- p.f_parent;
   f.f_instr <- PI_nop;
   incr moved;
   info "Instruction moved." in

  let move_cond f p =
    (*
    **   f: instruction under test
    **   p: F_floating frame
    **
    ** Condition for instruction movement for LHS=y:
    ** Ndep(write,y) = 1
    ** Condition for instruction movement for RHS(x_i):
    ** Ndep(write,x_i) = 0
    *)  
    line f.f_source;
    let rec n dl s =
      match dl with
      | d::tl -> 
        (if d = s then 1 else 0)+(n tl s);
      | [] -> 0 in
    let rec check nl n =
      match nl with
      | n' :: tl -> (n' = n) && (check tl n);
      | [] -> true in
        
    match p with
    | Some p -> 
    begin
      let wd_p = p.f_writedep in
      let lhs = f.f_writedep in
      let rhs = f.f_readdep in
      let n1l = List.map (fun x -> n wd_p x) lhs in
      let n2l = List.map (fun x -> n wd_p x) rhs in
      (check n1l 1) && (check n2l 0)
    end;
    | None -> false in

  let rec find_move f p b =
    match f with
    | Some f ->
    begin
      line f.f_source;
      match f.f_kind with
      | F_floating ->
      begin
        let p' = Some f in
        let next = f.f_next in
        List.iter (fun f' -> find_move (Some f') p' b) f.f_childs;
        find_move next p b
      end;
      | F_block ->
        let next = f.f_next in
        (*
        ** Maybe moveable instruction are embedded in this
        ** basic block
        *)
        let b' = Some f in
        List.iter (fun f' -> find_move (Some f') p b') f.f_childs;
        find_move next p b;
      | F_instr ->
      begin
        let next = f.f_next in
        let move_it = move_cond f p in
        if move_it then
        begin
          let to_move = 
            match b with
            | Some b when (Some b)=f.f_parent -> b;
            | _ -> f; in
          let before =
            match p with
            | Some p -> p;
            | None -> error 331433 "" in
          move to_move before;
        end;
        find_move next p b;
      end;
      | F_fixed ->
      begin
        let next = f.f_next in
        List.iter (fun f' -> find_move (Some f') None b) f.f_childs;
        find_move next p b;        
      end;
    end;
    | None -> () in
        
  match frame with
  | Some f ->
    line f.f_source;
    let next = f.f_next in
    find_move frame None None;
    frame_loop_motion next;
  | None -> ()

let rec frame_branch_motion frame =
  let maybe_moves = ref [] in
  (*
  ** Get string representation of instruction (block)
  *)
  let rec str_of_pi pi =
    match pi with
    | PI_block (il,_) ->
      let str = ref "" in
      List.iter (fun pi' -> str := sprintf "%s/%s" !str (str_of_pi pi')) il;
      !str
    | PI_assign (_,lhs,rhs) ->
      sprintf "%s/%s" (str_of_pi lhs) (str_of_pi rhs);
    | PI_obj _ | PI_bool _ | PI_arithm _ -> pi_sprint_instr pi;
    | _ -> pi_sprint_instr pi; in

  let move f p = 
   (*
   ** Move instruction/block frame f before p...
   *)
   line f.f_source;
   let next = f.f_next in
   let prev = f.f_prev in
   if next <> None then
   begin
     match next with
     | Some n -> n.f_prev <- prev;
     | None -> error 612741 "";
   end;
   if prev <> None then
   begin
     match prev with
     | Some p -> p.f_next <- next;
     | None -> error 612742 "";
   end;
   let f' = {f with f_prev=None} in
   f.f_state <- F_removed;
   f'.f_state <- F_moved;
   let prev = p.f_prev in
   if prev <> None then
   begin
     match prev with
     | Some p -> 
       p.f_next <- Some f';
       f'.f_prev <- Some p;
     | None -> error 612742 "";
   end
   else f'.f_prev <- None;
   p.f_prev <- Some f';
   f'.f_next <- Some p;
   f'.f_parent <- p.f_parent;
   f.f_instr <- PI_nop;
   incr moved;
   info "Branch instruction moved." in

  let remove f p = 
   line f.f_source;
   let next = f.f_next in
   let prev = f.f_prev in
   if next <> None then
   begin
     match next with
     | Some n -> n.f_prev <- prev;
     | None -> error 612741 "";
   end;
   if prev <> None then
   begin
     match prev with
     | Some p -> p.f_next <- next;
     | None -> error 612742 "";
   end;
   info "Branch instruction removed." in
   
  let move_cond f c =
    (*
    **   f: instruction under test
    **   c: one partial branch case of F_fixed (branch) frame
    **
    ** Condition for instruction movement for LHS=y:
    ** Ndep(write,y) = 1
    ** Condition for instruction movement for RHS(x_i):
    ** Ndep(write,x_i) = 0
    *)  
    line f.f_source;
    let rec n dl s =
      match dl with
      | d::tl -> 
        (if d = s then 1 else 0)+(n tl s);
      | [] -> 0 in
    let rec check nl n =
      match nl with
      | n' :: tl -> (n' = n) && (check tl n);
      | [] -> true in
        
    match c with
    | Some c -> 
    begin
      let wd_c = c.f_writedep in
      let lhs = f.f_writedep in
      let rhs = f.f_readdep in
      let n1l = List.map (fun x -> n wd_c x) lhs in
      let n2l = List.map (fun x -> n wd_c x) rhs in
      (check n1l 1) && (check n2l 0)
    end;
    | None -> false in

  let rec find_move f p c b =
    match f with
    | Some f ->
    begin
      line f.f_source;
      match f.f_kind with
      | F_fixed when (f.f_name = "PI_branch") || (f.f_name = "PI_select") ->
      begin
        (*
        ** Try to find candidates for out-of-branch moves.
        ** Branches can be nested!
        *)
        let p' = Some f in
        let next = f.f_next in
        let orig_moves = !maybe_moves in
        maybe_moves := [];
        List.iter (fun f' -> 
          find_move (Some f') p' (Some f') b;
          ) f.f_childs;
        let branch_n = List.length f.f_childs in
        let common ml =
          (*
          ** Each instruction in each branch must have equal RHS...
          *) 
          List.iter (fun (to_move,before) ->
            let e1 = str_of_pi to_move.f_instr in
            let moves = ref [] in
            
            List.iter (fun (to_move',before') ->
              if to_move.f_writedep = to_move'.f_writedep then
              begin
                (*
                ** Compare both expression - they must be identical!
                *)
                let e2 = str_of_pi to_move'.f_instr in
                if e1 = e2 then
                  moves := !moves @ [to_move',before'];
              end;
              ) ml;
            if (List.length !moves) = branch_n then
            begin
              let first = ref true in
              List.iter (fun (to_move,before) ->
                if !first then
                begin
                  move to_move before;
                  first := false;
                end
                else 
                begin
                  remove to_move before;
                end;
                ) !moves;
            end;
            ) ml;
          [] in
        let to_moves = common !maybe_moves in
        maybe_moves := orig_moves;
        find_move next p c b
      end;
      | F_block ->
        let next = f.f_next in
        (*
        ** Maybe moveable instruction are embedded in this
        ** basic block
        *)
        let b' = Some f in
        List.iter (fun f' -> find_move (Some f') p c b') f.f_childs;
        find_move next p c b;
      | F_instr ->
      begin
        let next = f.f_next in
        let move_it = move_cond f c in
        if move_it then
        begin
          let to_move = 
            match b with
            | Some b when (Some b)=f.f_parent -> b;
            | _ -> f; in
          let before =
            match p with
            | Some p -> p;
            | None -> error 331433 "" in
          maybe_moves := !maybe_moves @ [to_move,before];
        end;
        find_move next p c b;
      end;
      | _ ->
      begin
        let next = f.f_next in
        List.iter (fun f' -> find_move (Some f') None c b) f.f_childs;
        find_move next p c b;        
      end;
    end;
    | None -> () in
        
  match frame with
  | Some f ->
    line f.f_source;
    let next = f.f_next in
    find_move frame None None None;
    frame_loop_motion next;
  | None -> ()

let rec sprint_frames frame =
  match frame with
  | Some f ->
  begin
    box 2 "Frame(%s)\n" (sprintf "{f_name=%s;f_kind=%s;\nf_source=%s;\nf_readdep=[%s];\nf_writedep=[%s];\nf_childs=[\n%s]\n}\n" 
                                 f.f_name 
                                 (
                                  match f.f_kind with
                                  | F_fixed -> "FIXED"
                                  | F_floating -> "FLOAT"
                                  | F_block -> "BLOCK"
                                  | F_instr -> "INSTR"
                                 )
                                 (print_src f.f_source)
                                 (align 11 (vlist (fun d -> d) f.f_readdep))
                                 (align 12 (vlist (fun d -> d) f.f_writedep))
                                 (vlist (fun f -> sprint_frames (Some f)) f.f_childs)                                 
                         )^
    (sprint_frames f.f_next) 
  end;
  | None -> "" 
  
let print_frames frame =
  out (sprintf "\n%s" (sprint_frames frame))
  
let optimize_motion main =    
    out "Optimizer: code motion optimization...";
    let all_moved = ref 0 in
    ind_incr ();
    let loop md =
      List.iter (fun pro ->
          moved := 0;
          out (sprintf "in process <%s.%s>" 
                       md.mod_name 
                       pro.pro_name);
          let frames = to_frame None None pro.pro_instr in 
          frame_extend frames;
          
          if debug_it "optimize_motion" then print_frames frames;
          frame_loop_motion frames; 
          frame_branch_motion frames; 
          frame_fix frames; 
          pro.pro_instr <- of_frame frames;
          ind_incr ();
          out (sprintf "Moved %d instruction(s)." !moved);
          all_moved := !all_moved + !moved; 
          ind_decr ();
        ) md.mod_procs in
    loop main;
    List.iter (fun md -> loop md) main.mod_external;
    ind_decr ();
    !all_moved

           
(*
** Program level optimization:
** Optimize and remove dead code, for example registers with only read
** or write access.
*)

let optimize_dead_objs main =
    an.a_curpos <- nilsrc();
    (*
    ** Find dead objects in process instructions. Only LHS occurunce of 
    ** dead objects in instructions are allowed!
    *)
    let get_dead_instr pro instr =
        let desc = ref "" in
        let left_dead = ref [] in
        let right_dead = ref [] in
        let is_value obt = 
            match obt with  
            | OT_named_value _
            | OT_value _ -> true;
            | _ -> false in

        let rec get instr =
            match instr with
            | PI_assign (src,lhs,rhs) ->
                line src;
                left_dead := !left_dead @ (get lhs);
                right_dead := !right_dead @ (get rhs);
                desc := "<ASSIGN>";
                [];
            | PI_map (src,lhs,rhs) ->
                line src;
                left_dead := !left_dead @ (get lhs);
                right_dead := !right_dead @ (get rhs);
                desc := "<MAP>";
                [];
            | PI_concat (i1,i2) -> 
                right_dead := !right_dead @ (get i1) @ (get i2);
                [];                
            | PI_obj (opl,ot) ->
(*****************
                let ot =
                  if is_sel opl then
                  begin
                    let i = obj_sel opl in
                    match ot with
                    | OT_array at ->  
                        let is_block = List.mem AT_block at.at_flags in
                        if not is_block then at.at_objs.(i)
                        else at.at_objs.(0);
                    | _ -> error 9848 "";
                  end
                  else ot in
********************)
                let ot_name = name_of_ot ot in
                let c1 = sym_check_sym pro.pro_objs (Sym_obj ot) in
                let c2 = sym_check_sym pro.pro_import (Sym_obj ot) in
                let c3 = sym_check_sym pro.pro_export (Sym_obj ot) in
                let c4 = is_value ot in
                let tmp_names = List.map (fun co -> co.co_name)
                                    pro.pro_temps in
                let c5 = List.mem ot_name tmp_names in
                if not c1 && not c2 && not c3 && not c4 && 
                   not c5 then
                    [ot]
                else
                    []
            | PI_arithm (_,i1,i2) ->
                right_dead := !right_dead @ (get i1);        
                right_dead := !right_dead @ (get i2);
                []
            | PI_bool (_,_,i1,i2) ->
                right_dead := !right_dead @ (get i1);        
                right_dead := !right_dead @ (get i2);
                []
            | PI_fun (_,(opl,(OT_object ao)),sel,args) -> 
                let args_flags = ref [] in
                List.iter (fun (sel',args_flags') ->
                    if sel = sel' then args_flags := args_flags';
                    ) ao.ao_type.ta_rules.rl_methods;
                left_dead := !left_dead @ (get (PI_obj (opl,(OT_object ao))));
                let rec iter args args' =
                    match args with
                    | arg::tl ->
                    begin
                        match args' with
                        | arg' :: tl' ->
                            begin
                              match arg'.arg_type with
                              | Arg_lhs  ->
                                left_dead := !left_dead @ (get arg)
                              | _ ->
                                right_dead := !right_dead @ (get arg);
                            end;
                            iter tl tl';
                        | [] -> error 457376 "";
                    end;
                    | [] -> () in
                iter args !args_flags;
                [];
            | _ -> []
            in
        __(get instr);
        !left_dead, !right_dead, !desc
        in


    let rem = ref [] in
    let rem_blocks = ref [] in
    let warned = ref  [] in

    (*
    ** Remove not used processes.
    *)
    let remove_pro modu =
        modu.mod_procs <- List.filter (fun pro ->
            let pro_main = pro.pro_name = "main" in
            (*
            ** First check process itself: at least one start or
            ** call must be done outside. Else it's a dead process.
            *)
            let ao =pro.pro_ao in
            let pstart,pstop,pcall=ref [],ref [], ref [] in
            List.iter (fun (sel,pro) ->
                match sel with
                | "start" -> pstart := !pstart @ [pro];
                | "stop" -> pstop := !pstop @ [pro];
                | "call" -> pcall := !pcall @ [pro];
                | _ -> error 0 (sprintf "Unknown method <%s> found for process <%s>."
                                         sel ao.ao_name);
                ) ao.ao_procs;
            if !pstart = [] && !pcall = [] && not pro_main then
            begin
                optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                             (sprintf "Removing process <%s> because never started nor called."
                                      pro.pro_name); 
                sym_delete modu.mod_objs (Sym_pro pro);
                (*
                ** Remove process dependency in all imported objects, too.
                *)
                let arg_pref = sprintf "ARG_%s_" pro.pro_name in
                let ret_pref = sprintf "RET_%s_" pro.pro_name in
                let lock_pref = sprintf "LOCK_%s" pro.pro_name in

                let match_pref name p =
                    let pn = String.length p in 
                    let nn = String.length name in
                    (nn >= pn) && ((String.sub name 0 pn) = p) in
 
                let syms = list_of_sym pro.pro_objs in
                List.iter (fun sym ->
                    match sym with
                    | Sym_obj ot ->                    
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing local object <%s>."
                                               (name_of_ot ot));
                        sym_delete pro.pro_objs sym;
                    | Sym_block db -> 
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing local block <%s>."
                                              db.db_name);
                        sym_delete pro.pro_objs sym;
                    | _ -> ();
                    ) syms;
                (*
                ** Remove process dependencies in imported objects (reader/writer/access)...
                *)
                let syms = list_of_sym pro.pro_import in
                List.iter (fun sym ->
                    match sym with
                    | Sym_obj ot ->   
                    begin
                      match co_of_ot ot with
                      | Some co -> 
                        if List.mem pro co.co_reader then
                          co.co_reader <- List.filter (fun pro' -> pro' <> pro) co.co_reader; 
                        if List.mem pro co.co_writer then
                          co.co_writer <- List.filter (fun pro' -> pro' <> pro) co.co_writer; 
                      | None -> ();
                    end;    
                    | Sym_block db ->              
                      List.iter (fun ot ->
                        match co_of_ot ot with
                        | Some co -> 
                          if List.mem pro co.co_reader then
                            co.co_reader <- List.filter (fun pro' -> pro' <> pro) co.co_reader; 
                          if List.mem pro co.co_writer then
                            co.co_writer <- List.filter (fun pro' -> pro' <> pro) co.co_writer; 
                        | None -> ();                        
                        ) db.db_objs;
                    | _ -> ();
                    ) syms;
                
                let syms = list_of_sym modu.mod_objs in
                let procs = modu.mod_procs in
                List.iter (fun sym ->
                    match sym with
                    | Sym_obj ot ->                    
                        let name = name_of_ot ot in
                        if match_pref name arg_pref then
                        begin
                            optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                         (sprintf "Removing global argument object <%s> and import dependencies..." 
                                                  name);
                            sym_delete modu.mod_objs sym;
                            List.iter (fun pro' ->
                              if pro' <> pro then
                              begin
                                let ok = protects (sym_delete pro'.pro_import sym) in
                                if ok then out (sprintf "    ... in process <%s>." pro'.pro_name);
                              end;
                              ) procs;                           
                        end;
                        if match_pref name ret_pref then
                        begin
                            optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                         (sprintf "Removing global return object <%s> and import dependencies..." 
                                                  name);
                            sym_delete modu.mod_objs sym;
                            List.iter (fun pro' ->
                              if pro' <> pro then
                              begin
                                let ok = protects (sym_delete pro'.pro_import sym) in
                                if ok then out (sprintf "    ... in process <%s>." pro'.pro_name);
                              end;
                              ) procs;                           
                        end;
                        if match_pref name lock_pref then
                        begin
                            optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                         (sprintf "Removing global lock object <%s> and import dependencies..." 
                                                  name);
                            sym_delete modu.mod_objs sym;
                            List.iter (fun pro' ->
                              if pro' <> pro then
                              begin
                                let ok = protects (sym_delete pro'.pro_import sym) in
                                if ok then out (sprintf "    ... in process <%s>." pro'.pro_name);
                              end;
                              ) procs;                           
                        end;
                    | _ -> ();
                    ) syms;
                            
                false
            end
            else
                true;
            ) modu.mod_procs;
        in
    
    (*
    ** Remove not used objects
    *)
    let rec remove_it symhash pro =
        let syms = list_of_sym symhash in
        List.iter (fun sym ->
          if not (is_mon sym) then
          match sym with
          | Sym_obj obj -> 
          begin
            match obj with
            | OT_reg co ->
                let p = List.length (List.filter (fun f -> f=Obj_port) co.co_flags)  in
                let r = (List.length co.co_reader)+p in
                let w = List.length co.co_writer in

                (*
                ** Exported object in top module or monitor?
                *)
                let e = (sym_check (get_some !main_module).mod_export 
                                  co.co_name) in
                let m = (sym_check (get_some !main_module).mod_export
                                  ("MON_"^co.co_name))  in

                if (r > 0 && w = 0 && not e) then
                    error 0 (sprintf 
                            "Found Register <%s.%s> with %d reader(s) and %d writer(s)." 
                             co.co_module.mod_name co.co_name r w)
                else if (r = 0 && not e && not m) or (w = 0) then
                begin
                    (*
                    ** Kick it out.
                    *)
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Register <%s.%s> with %d reader(s) and %d writer(s)." 
                                              co.co_module.mod_name co.co_name r w);
                    end;
                    sym_delete symhash sym;
                end;
            | OT_channel ch ->
                let co = ch.ch_obj in
                let r = List.length co.co_reader in
                let w = List.length co.co_writer in
                (*
                ** Exported object in top module or monitor?
                *)
                let e = (sym_check (get_some !main_module).mod_export 
                                  co.co_name) in
                let m = (sym_check (get_some !main_module).mod_export
                                  ("MON_"^co.co_name))  in

                if (r > 0 && w = 0 && not e) then
                    error 0 (sprintf 
                            "Found Channel <%s.%s> with %d reader(s) and %d writer(s)." 
                             co.co_module.mod_name co.co_name r w)
                else if (r = 0 && not e && not m) or (w = 0) then
                begin
                    (*
                    ** Kick it out.
                    *)
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Channel <%s.%s> with %d reader(s) and %d writer(s)." 
                                              co.co_module.mod_name co.co_name r w);
                    end;
                    sym_delete symhash sym;
                end;
            | OT_queue qu ->
                let co = qu.qu_obj in
                let r = List.length co.co_reader in
                let w = List.length co.co_writer in
                (*
                ** Exported object in top module or monitor?
                *)
                let e = (sym_check (get_some !main_module).mod_export 
                                  co.co_name) in
                let m = (sym_check (get_some !main_module).mod_export
                                  ("MON_"^co.co_name))  in

                if (r > 0 && w = 0 && not e) then
                    error 0 (sprintf 
                            "Found Queue <%s.%s> with %d reader(s) and %d writer(s)." 
                             co.co_module.mod_name co.co_name r w)
                else if (r = 0 && not e && not m) or (w = 0) then
                begin
                    (*
                    ** Kick it out.
                    *)
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Queue <%s.%s> with %d reader(s) and %d writer(s)." 
                                              co.co_module.mod_name co.co_name r w);
                    end;
                    sym_delete symhash sym;
                end;
            | OT_var co ->
                let r = List.length co.co_reader in
                let w = List.length co.co_writer in
                let e = sym_check (get_some !main_module).mod_export 
                                  co.co_name in
                if (r = 0 && not e) or (w = 0) then
                begin
                    (*
                    ** Kick it out.
                    *)
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Variable <%s.%s> with %d reader(s) and %d writer(s)." 
                                              co.co_module.mod_name co.co_name r w);
                    end;
                    sym_delete symhash sym;
                    (*
                    ** Remove object from data_block list, too.
                    *)
                    match co.co_block with
                    | Some db ->
                        db.db_objs <- List.filter (fun obj' ->
                                            if obj = obj' then
                                                db.db_size <- 
                                                db.db_size - co.co_size;
                                            if obj = obj' then false
                                                          else true)
                                            db.db_objs;
                    | None -> error 801368 "optimize_dead_code: co_block";
                end;
            | OT_array_sel (at,sel) ->
                let is_co =
                    match at.at_objs.(sel) with
                    | OT_reg _
                    | OT_channel _
                    | OT_queue _
                    | OT_signal _
                    | OT_var _ -> true;
                    | _ -> false;
                    in
                if is_co then
                begin
                  let co =
                    match at.at_objs.(sel) with
                    | OT_reg co
                    | OT_signal co
                    | OT_var co -> co;
                    | OT_channel ch -> ch.ch_obj;
                    | OT_queue qu -> qu.qu_obj;
                    | _ -> error 759279 "";
                    in
                
                  let r = List.length co.co_reader in
                  let w = List.length co.co_writer in
                  let m = sym_check (get_some !main_module).mod_export 
                                    co.co_name in
                  if (r = 0 && not m) or (w = 0) then
                  begin
                    (*
                    ** Kick it out.
                    *)
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Array <%s.%s> with %d reader(s) and %d writer(s)." 
                                              co.co_module.mod_name co.co_name r w);
                    end;
                    sym_delete symhash sym;
                    (*
                    ** Remove object from data_block list, too.
                    *)
                    match co.co_block with
                    | Some db ->
                        db.db_objs <- List.filter (fun obj' ->
                                            if obj = obj' then
                                                db.db_size <- 
                                                db.db_size - co.co_size;
                                            if obj = obj' then false
                                                          else true)
                                            db.db_objs;
                    | None -> ();
                  end;
                end;
            | OT_array at ->
            begin
              let e = (sym_check (get_some !main_module).mod_export 
                                  at.at_name) in
              match at.at_objs.(0) with
              | OT_var co  ->
                let obj = at.at_objs.(0) in
                let r = List.length co.co_reader in
                let w = List.length co.co_writer in
                let m = sym_check (get_some !main_module).mod_export 
                                  co.co_name in
                if (r = 0 && not m && not e) or (w = 0 && not m) then
                begin
                    (*
                    ** Kick it out.
                    *)
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Array <%s.%s> with %d reader(s) and %d writer(s)." 
                                              co.co_module.mod_name co.co_name r w);
                    end;
                    sym_delete symhash sym;
                    (*
                    ** Remove object from data_block list, too.
                    *)
                    match co.co_block with
                    | Some db ->
                        db.db_objs <- List.filter (fun obj' ->
                                            if obj = obj' then
                                                db.db_size <- 
                                                db.db_size - co.co_size;
                                            if obj = obj' then false
                                                          else true)
                                            db.db_objs;
                    | None -> error 33589 "optimize_dead_code: co_block";
                end;
              | OT_reg _ ->
              begin
                (*
                ** Kick out only totally unused array
                *)
                let remove = ref 0 in
                let mn = ref "" in
                let size = Array.length at.at_objs in
                Array.iter (fun ot ->
                  match ot with
                  | OT_reg co ->
                    mn := co.co_module.mod_name;
                    let r = List.length co.co_reader in
                    let w = List.length co.co_writer in
                    let m = sym_check (get_some !main_module).mod_export 
                                       co.co_name in
                    if (r = 0 && not m && not e) or (w = 0 && not m) then
                        incr remove;
                  | _ -> error 59894 "";
                  ) at.at_objs;
                if !remove = size then
                begin
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Array <%s.%s> with %d unused cells from total %d."
                                              !mn at.at_name
                                              !remove size);
                    end;
                    sym_delete symhash sym;
                end
                else if not (List.mem at.at_name !warned) && !remove > 0 then
                begin
                  out (sprintf 
                            "Found %d unused from total %d cells in Array <%s.%s>."
                            !remove size
                            !mn at.at_name);
                  warned := !warned @ [at.at_name];
                end;
              end;
              | OT_channel _ ->
              begin
                (*
                ** Kick out only totally unused array
                *)
                let remove = ref 0 in
                let mn = ref "" in
                let size = Array.length at.at_objs in
                Array.iter (fun ot ->
                  match ot with
                  | OT_channel ch ->
                    let co = ch.ch_obj in
                    mn := co.co_module.mod_name;
                    let r = List.length co.co_reader in
                    let w = List.length co.co_writer in
                    let m = sym_check (get_some !main_module).mod_export 
                                       co.co_name in
                    if (r = 0 && not m) or (w = 0 && not m) then
                        incr remove;
                  | _ -> error 59894 "";
                  ) at.at_objs;
                if !remove = size then
                begin
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Array <%s.%s> with %d unused cells from total %d."
                                              !mn at.at_name
                                              !remove size);
                    end;
                    sym_delete symhash sym;
                end
                else if not (List.mem at.at_name !warned) && !remove > 0 then
                begin
                  out (sprintf 
                            "Found %d unused from total %d cells in Array <%s.%s>."
                            !remove size
                            !mn at.at_name);
                  warned := !warned @ [at.at_name];
                end;
              end;
              | OT_queue _ ->
              begin
                (*
                ** Kick out only totally unused array
                *)
                let remove = ref 0 in
                let mn = ref "" in
                let size = Array.length at.at_objs in
                Array.iter (fun ot ->
                  match ot with
                  | OT_queue qu ->
                    let co = qu.qu_obj in
                    mn := co.co_module.mod_name;
                    let r = List.length co.co_reader in
                    let w = List.length co.co_writer in
                    let m = sym_check (get_some !main_module).mod_export 
                                       co.co_name in
                    if (r = 0 && not m) or (w = 0 && not m) then
                        incr remove;
                  | _ -> error 59895 "";
                  ) at.at_objs;
                if !remove = size then
                begin
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Array <%s.%s> with %d unused cells from total %d."
                                              !mn at.at_name
                                              !remove size);
                    end;
                    sym_delete symhash sym;
                end
                else if not (List.mem at.at_name !warned) && !remove > 0 then
                begin
                  out (sprintf 
                            "Found %d unused from total %d cells in Array <%s.%s>."
                            !remove size
                            !mn at.at_name);
                  warned := !warned @ [at.at_name];
                end;
              end;
              | _ -> ();
            end;
            | OT_signal co ->
                let r = (List.length co.co_reader)+
                        (if List.mem Obj_port_in co.co_flags then 1 else 0) in
                let w = (List.length co.co_writer)+
                        (if List.mem Obj_port_out co.co_flags then 1 else 0) in
                (*
                ** Exported object in top module or monitor or port signal?
                *)
                let e = (sym_check (get_some !main_module).mod_export 
                                  co.co_name) in
                let m = (sym_check (get_some !main_module).mod_export
                                  ("MON_"^co.co_name)) in
                let p = (List.mem Obj_port co.co_flags)  in

                if (r > 0 && w = 0 && not e && not p) then
                    error 0 (sprintf 
                            "Found Signal <%s.%s> with %d reader(s) and %d writer(s)." 
                             co.co_module.mod_name co.co_name r w)
                else if (r = 0 && not m && not e && not p) or 
                   (w = 0 && not m && not e && not p) then
                begin
                    (*
                    ** Kick it out.
                    *)
                    if not (List.mem obj !rem) then 
                    begin
                        rem := !rem @ [obj];
                        optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                     (sprintf "Removing Signal <%s.%s> with %d reader(s) and %d writer(s)." 
                                              co.co_module.mod_name co.co_name r w);
                    end;
                    sym_delete symhash sym;
                end;
            | _ -> ();
          end;
          | Sym_pro pro -> 
            out (sprintf "... in process <%s> ..." pro.pro_name);

            remove_it pro.pro_objs pro;
            remove_it pro.pro_import pro ;
            remove_it pro.pro_export pro;
            (*
            ** Remove instructions:
            ** assignements: LHS = <deadobj>
            **               RHS = <deadobj> => error!!!
            ** mappings: LHS = <deadobj>
            **           RHS = <deadobj> => error!!!
            *)

            let n = ref 0 in
            let lines = ref [] in
            an.a_curpos <- nilsrc ();
            let examine instr = 
                debug "optimize_dead_objs" with (sprintf "optimize_dead_objs: examine [%d]" an.a_curpos.s_line);
                let lhs,rhs,desc = get_dead_instr pro instr in
                if lhs <> [] then
                begin
                    optimize_log (sprintf "[%s.%s]" pro.pro_module.mod_name pro.pro_name)
                                 (sprintf "Removing instruction %s."
                                           (if an.a_curpos <> (nilsrc ()) then
                                              sprintf "[file %s, line %d]"
                                                      an.a_curpos.s_file an.a_curpos.s_line
                                            else
                                              sprintf "[Auxiliary instruction]"));
                    false       (* remove it *)
                end
                else if rhs <> [] then
                begin
                    error 0 (sprintf "optimize_dead_objs: process %s.%s has dead object(s) on RHS in instruction %d!" 
                             pro.pro_module.mod_name pro.pro_name !n);
                end
                else
                    true
                in

            let rec iter il =
              match il with
              | instr::tl ->
              begin
                match instr with
                | PI_branch (src,expr,b1,b2) ->
                    line src;
                    let keep_expr = examine expr in
                    let b1' = 
                        match b1 with
                        | PI_block (il,bf) -> 
                            PI_block (iter il,bf);
                        | _ -> 
                            let pl' = iter [b1] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                    let b2' = 
                        match b2 with
                        | PI_block (il,bf) -> 
                            PI_block (iter il,bf);
                        | _ -> 
                            let pl' = iter [b2] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                    (if keep_expr then
                        [PI_branch (src,expr,b1',b2')] else []) @
                    (iter tl);
                | PI_waitfor (src,expr,time,unit,b1,b2) ->
                    line src;
                    let keep_expr = examine expr in
                    let b1' = 
                        match b1 with
                        | PI_block (il,bf) -> 
                            PI_block (iter il,bf);
                        | _ -> 
                            let pl' = iter [b1] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                    let b2' = 
                        match b2 with
                        | PI_block (il,bf) -> 
                            PI_block (iter il,bf);
                        | _ -> 
                            let pl' = iter [b2] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                    (if keep_expr then
                        [PI_waitfor (src,expr,time,unit,b1',b2')] else []) @
                    (iter tl);
                | PI_forloop (src,expr,dir,i1,i2,b) ->
                    line src;
                    let b' = 
                        match b with
                        | PI_block (il,bf) -> 
                            PI_block (iter il,bf);
                        | _ -> 
                            let pl' = iter [b] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                     [PI_forloop (src,expr,dir,i1,i2,b')] @
                    (iter tl);
                | PI_loop (src,kind,expr,b) ->
                    line src;
                    let b' = 
                        match b with
                        | PI_block (il,bf) -> 
                            PI_block (iter il,bf);
                        | _ -> 
                            let pl' = iter [b] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                     [PI_loop (src,kind,expr,b')] @
                     (iter tl);
                | PI_select (src,expr,csl) ->
                    line src;
                    let csl' = 
                        match csl with
                        | PI_list li -> PI_list (iter li);
                        | _ -> 
                            let pl' = iter [csl] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                     [PI_select (src,expr,csl')] @
                    (iter tl);
                | PI_case (src,il,b) ->
                    line src;
                    let b' = 
                        match b with
                        | PI_block (il,bf) -> 
                            PI_block (iter il,bf);
                        | _ -> 
                            let pl' = iter [b] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                     [PI_case (src,il,b')] @
                     (iter tl);
                | PI_block (il,bf) when not (List.mem BP_bind bf.bf_params)->
                     [PI_block (iter il,bf)] @ (iter tl);
                | PI_list li -> [PI_list (iter li)] @ (iter tl);
                | PI_fun (src,_,_,_)
                | PI_map (src,_,_)
                | PI_monitor (src,_,_)
                | PI_assign (src,_,_) ->
                    line src;
                    let keep = examine instr in
                    (if keep then 
                        [instr] else []) @ (iter tl);
                | PI_nop -> [PI_nop] @ (iter tl);
                | PI_block (il,bf) when (List.mem BP_bind bf.bf_params) -> 
                    instr :: (iter tl);
                | PI_raise ex -> [PI_raise ex] @ (iter tl);
                | PI_try (b,csl) ->
                    let b' = 
                        match b with
                        | PI_block (il,bf) -> 
                            PI_block (iter il,bf);
                        | _ -> 
                            let pl' = iter [b] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                    let csl' = 
                        match csl with
                        | PI_list li -> PI_list (iter li);
                        | _ -> 
                            let pl' = iter [csl] in
                            if pl' <> [] then
                                List.hd pl'
                            else PI_nop in
                     [PI_try (b',csl')] @
                    (iter tl);

                | _ -> error 0 "Found unexpected instruction.";
              end;
              | [] -> []  in
            pro.pro_instr <- iter pro.pro_instr;
            an.a_curpos <- nilsrc ();
          | Sym_block db -> 
            if db.db_objs = [] then
            begin
                if not (List.mem db !rem_blocks) then 
                begin
                    rem_blocks := !rem_blocks @ [db];
                    out (sprintf "Removing block <%s> with 0 objects."
                                 db.db_name);
                end;
                sym_delete symhash sym;
            end;
          | _ -> ();
          ) syms;
        in
    out "Optimizer: removing dead objects and instructions...";
    ind_incr ();
    (*
    ** Remove unsued processes first and remove process dependencies
    ** of objects referenced by these removed processes.
    *)
    remove_pro main;
    let rec iter mll =
            List.iter (fun ml ->
                remove_pro ml;
                iter ml.mod_external;
                ) mll;
            in
    iter main.mod_external;

    (*
    ** Examine first all objects from the main module.
    *)
    let pro = module_pro_main main in
    remove_it main.mod_objs pro;  
    remove_it main.mod_export pro;  
    
    let rec iter mll =
            List.iter (fun ml ->
                let pro = module_pro_main ml in
                remove_it ml.mod_objs pro;
                remove_it ml.mod_export pro;
                remove_it ml.mod_import pro;
                iter ml.mod_external;
                ) mll;
            in
    (*
    ** Now the included module tree.
    *)
    iter main.mod_external;
    let remn = (List.length !rem) + 
               (List.length !rem_blocks) in
    out (sprintf "Removed %d object(s)." remn);
    ind_decr ();
    remn

        
