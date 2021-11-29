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
**    $INITIAL:     (C) 2006 BSSLAB
**    $CREATED:     19.3.2006
**    $VERSION:     1.93
**
**    $INFO:
**
**  Core module. Head part.
**
**    $ENDOFINFO
**
*)

open Cp_common
open Cp_types
open Cp_symbol
open Cp_utils
open Cp_analysis
open Cp_ucode
open Cp_expr
open Cp_syntax
open Printf


let (self:(rl_module option) ref) = ref None

(*
** Return true if object is handled by this rule module.
*)
let rec my sym =
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_signal _
    | OT_reg _
    | OT_var _
    | OT_channel _
    | OT_queue _
    | OT_array _
    | OT_named_value _ 
    | OT_const _ ->
           true       
    | OT_struct st ->
      if st.st_objs <> [] then my (Sym_obj (List.hd st.st_objs))
      else error 135662 "";
    | OT_component st ->
      if st.st_objs <> [] then my (Sym_obj (List.hd st.st_objs))
      else error 135663 "";
    | _ -> false
  end;
  | Sym_block _ -> true;
  | _ -> false


let get_procs co =
  let blocked = ref [] in
  let all = ref [] in  
  let pgd = 
    match co.co_guard with    
    | Some gd -> 
        (if List.mem GD_rd gd.gd_req then co.co_reader else []) @
        (if List.mem GD_wr gd.gd_req then co.co_writer else []);
    | None -> [] in

  List.iter (fun pro ->
    if not (List.mem pro !blocked) then
      blocked := !blocked @ [pro]
      ) pgd;
  List.iter (fun pro ->
    if not (List.mem pro !all) then
      all := !all @ [pro]
      ) (co.co_reader @ co.co_writer);
  co.co_reader,
  co.co_writer,        
  !blocked,                            
  !all                    
