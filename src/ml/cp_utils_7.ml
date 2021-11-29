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
**    $INITIAL:     (C) 2006-2013 BSSLAB
**    $CREATED:     23.7.2006
**    $VERSION:     1.10
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
open Cp_utils_3
open Cp_utils_4
open Cp_utils_5
open Cp_utils_6
open Cp_data_core

(*
** Test for valid VHDL value 
*)
let is_vhdl_val str =
  let valid = ref true in
  let is_hex = ref false in
  String.iter (fun c ->
    match c with
    | 'x' | 'X' -> is_hex := true; 
    | 'b' | 'o' -> ();
    | ' ' -> is_hex := false;
    | '"' -> is_hex := false;
    | '0' .. '9' -> ();
    | 'a' .. 'f' -> valid := !valid && !is_hex = false;
    | 'A' .. 'F' -> valid := !valid && !is_hex = false;
    | ',' | ';' | '(' | ')' -> ();
    | _ -> valid := false; is_hex := false;
    ) str;
  !valid

let log2 x =
  let rec log2 x y = 
    if x < 1 then y 
    else log2 (x / 2) (y+1) in
  log2 (x-1) 0
let log2_64 x =
  let rec log2 x y = 
    if x < 0L1 then y 
    else log2 (Int64.div x 0L2) (Int64.add y 0L1) in
  log2 (Int64.sub x 0L1) 0L0
  
  
(*
** Find rl method "meth_name" in method description list ml and
** check argument list (rhs/lhs and type correctness)
** argl: method parameters
** argl': method application arguments
*)
let rec meth_check_args argl argl' =
  match argl with
  | arg :: tl ->
  begin
    match argl' with
    | arg' :: tl' ->
    begin
      let desc arg =
        match arg.arg_type with
        | Arg_lhs -> 1,arg.arg_data_type;
        | Arg_rhs -> 2,arg.arg_data_type;
        | Arg_lrhs -> 3,arg.arg_data_type in
        
      let cmp_dt dt dt' = (td_of_dt dt) = (td_of_dt dt') in
      let kind,dt = desc arg in
      let kind',dt' = desc arg' in
      ((kind = kind') || (kind = 3)) &&
      (cmp_dt dt dt')
    end;
    | [] -> false (* inconsistent argument list *)
  end;
  | [] -> true
  
let rec meth_check ml meth_name argl =
  match ml with
  | (meth_name',argl') :: tl ->
    if meth_name = meth_name' then
    begin
      meth_check_args argl argl'
    end
    else
      meth_check tl meth_name argl 
  | [] -> false 
  
(*
** Function parameter descriptor
*)
let arg_desc label kind datatype =
  {
    arg_label=label;
    arg_type=kind;
    arg_data_type=datatype; 
  }
  
let new_arg_desc kind =
  (*
  ** Actually uninitialized data type...
  *)
  arg_desc "new" kind (DT_object "'a")

let new_arg_desc_dt kind dt =
  (*
  ** Actually uninitialized data type...
  *)
  arg_desc "new" kind dt
  
let set_arg_data_type arg dt =
  arg.arg_data_type <- dt
 
let get_arg_data_type arg  =
  arg.arg_data_type 
   
let find_str search instr =
  protects (__(Str.search_forward (Str.regexp search) instr 0))
   
let is_num str =
  if str = "" then false
  else match str.[0] with
  | '0' .. '9' -> true;
  | _ -> false
  
let pro_of_ot ot = 
  match co_of_ot ot with
  | Some co -> co.co_process 
  | None -> None 

let pro_of_uo uo = 
  match uo.uo_obj with
  | Some co -> 
  begin
    match co.co_process with
    | Some pro -> pro;
    | None -> module_pro_main co.co_module;
  end;
  | None ->
    error 0 (sprintf "pro_of_uo: Process required, but object <%s> has no associated process." uo.uo_name)
    
let pro_of_ud ud =
  match uo_of_ud ud with
  | Some uo -> pro_of_uo uo;
  | None -> error 0 (sprintf "pro_of_ud: Process required, but object <%s> has no associated process." (ud_name ud))
     
let remove_attr_from_obj attr obj =
  match obj with
  | OT_signal co 
  | OT_reg co 
  | OT_var co -> 
    co.co_flags <- List.filter (fun f -> f <> attr) co.co_flags
  | _ -> ()
