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
**    $INITIAL:     (C) 2006-2007 BSSLAB
**    $CREATED:     16.4.2006
**    $VERSION:     1.126
**
**    $INFO:
**
**  Utils
**
**    $ENDOFINFO
**
*)

open Unix
open Printf

open Cp_syntax
open Cp_types
open Cp_symbol
open Cp_common
open Cp_utils_1
open Cp_utils_2
open Cp_data_core

let range_dt dt range =
  let a,b = range in
  let n = b-a+1 in
  dt_of_td (td_of_dt dt) n


(*
** Split fragmented variable access into several states.
**  ud_dst: fragmented variable UC_var, 
**  ud_src: temporary register UC_temp
*)
let frag_dst_split pro ud_dst ud_src =
    let uo_dst = 
        match ud_dst with
        | UC_var ud -> ud;
        | _ -> error 114440 "";
        in
    let ut_src = 
        match ud_src with
        | UC_temp ut -> ut;
        | _ -> error 169917 "";
        in

    let co_dst =
        match uo_dst.uo_obj with
        | Some co -> co;
        | None -> error 420330 "";
        in
    let n = co_dst.co_subsize in
    let w = size_of_dt co_dst.co_type in
    let f = w/n in

        
    let moves = ref [] in
    for i = 0 to n-1
    do
        let a,b = (i*f),((i+1)*f)-1 in
        let ud_dst' = UC_var {uo_dst with
                              uo_type = {uo_dst.uo_type with
                                         uo_expr_type = DT_logic f;};
                              uo_addr = (fun off ->
                                            uo_dst.uo_addr (i+off));
                              } in
        
        let ud_src' = UC_temp {ut_src with
                               ut_range = Some (a,b)} in
        let move = {
            ui_code = Move (ud_dst',ud_src');
            ui_frame=List.hd pro.pro_frame;
            } in
        moves := !moves @ [move];
    done;
    !moves

(*
** Split fragmented variable access into several states.
**  ud_src: fragmented variable UC_var, 
**  ud_dst: temporary register UC_temp or register (not write guarded)
*)
let frag_src_split pro ud_dst ud_src =
  let uo_src = 
        match ud_src with
        | UC_var ud -> ud;
        | _ -> error 188846 "";
        in
  let co_src =
        match uo_src.uo_obj with
        | Some co -> co;
        | None -> error 467907 "";
        in
  let n = co_src.co_subsize in
  let w = size_of_dt co_src.co_type in
  let f = w/n in
  let moves = ref [] in
  match ud_dst with
  | UC_temp ut_dst ->
    for i = 0 to n-1
    do
        let a,b = (i*f),((i+1)*f)-1 in
        let ud_src' = UC_var {uo_src with
                              uo_type = {uo_src.uo_type with 
                                            uo_expr_type = DT_logic f;};
                              uo_range = Some (a,b);
                              uo_addr = (fun off ->
                                    uo_src.uo_addr (i+off)) } in
        let ud_dst' = UC_temp {ut_dst with
                               ut_range = Some (a,b);
                               } in
        if i = 0 then
        begin
            let move = {
                ui_code = Move (ud_dst',ud_src');
                ui_frame=List.hd pro.pro_frame;
                } in
            moves := !moves @ [move];
        end
        else
        begin
            let expr = {
                ui_code = Expr ([OP_lor],ud_dst',ud_src',ud_fix_rhs ud_dst);
                ui_frame=List.hd pro.pro_frame;
                } in
            moves := !moves @ [expr];
        end;
    done;
    !moves
  | UC_reg uo_dst ->
    let gd_Rd,gd_wr = ud_guard ud_dst in
    if gd_wr then error 217672 "";
    for i = 0 to n-1
    do
        let a,b = (i*f),((i+1)*f)-1 in
        let ud_src' = UC_var {uo_src with
                              uo_type = {uo_src.uo_type with
                                            uo_expr_type = DT_logic f;};
                              uo_range = Some (a,b);
                              uo_addr = (fun off ->
                                    uo_src.uo_addr (i+off)) } in
        let ud_dst' = UC_reg {uo_dst with
                               uo_range = Some (a,b);
                               } in
        if i = 0 then
        begin
            let move = {
                ui_code = Move (ud_dst',ud_src');
                ui_frame=List.hd pro.pro_frame;
                } in
            moves := !moves @ [move];
        end
        else
        begin
            let expr = {
                ui_code = Expr ([OP_lor],ud_dst',ud_src',ud_fix_rhs ud_dst);
                ui_frame=List.hd pro.pro_frame;
                } in
            moves := !moves @ [expr];
        end;
    done;
    !moves
  | _ -> error 788907 ""
