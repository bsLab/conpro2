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
**    $INITIAL:     (C) 2008 BSSLAB
**    $CREATED:     17.10.2008
**    $VERSION:     2.07
**
**    $INFO:
**
**  Tool Description Interface - Types
**
**    $ENDOFINFO
**
*)
open Cp_common
open Cp_types
open Cp_syntax
open Cp_symbol
open Cp_utils
open Cp_data
open Cp_printtypes
open Cp_data_core
open Unix
open Printf
open Cp_fun

(*
** First this is module class is generated one time at opening of module.
** An instance is created using the #instance method each time an object
** is created.
*)
type func_builtin = 
  |  F_PRINT 
  |  F_PRINT_LINE 
  |  F_PRINT_FILE
  |  F_WRITE 
  |  F_WRITE_LINE
  |  F_OPEN_FILE
  |  F_CREATE_FILE 
  |  F_APPEND_FILE 
  |  F_REMOVE_FILE 
  |  F_RENAME_FILE 
  |  F_COPY_FILE 
  |  F_MOVE_FILE 
  |  F_LIST
  |  F_REMOVE_DIR 
  |  F_MAKE_DIR 
  |  F_CHANGE_DIR 
  |  F_EXIT 
  |  F_EXEC 
  |  F_EXEC_LOG
  |  F_EXEC_WRITE 
  |  F_BASENAME
  |  F_CHOPEXT
  |  F_DIRNAME
  |  F_GETENV
  |  F_GETOPT
  |  F_GETOPTS
  |  F_DATE
  |  F_TIME
  |  F_EXPORT
  |  F_PORT
  |  F_EXIST
  |  F_REV
  |  F_USER of string

type target_lang =
  | L_BASH
  | L_MAKE
  | L_TCL


type tde_range =
  | TDE_point of int64
  | TDE_line of (int64*int64)

and tdi_expr =
  | TDE_int of int
  | TDE_str of string
  | TDE_op of (tdi_expr * string * tdi_expr)
  | TDE_var of string
  | TDE_sel of (string * tdi_expr)
  | TDE_fun of (func_builtin * tdi_expr list)

and tdi_env = {
  (*
  ** Name of environment parameter
  *)
  mutable tde_name : string;
  (*
  ** Default or actual value (required).
  ** Scalar type: [hd]
  ** Array type: [e1;e2;e3;...]
  *)
  mutable tde_val  : tdi_expr list;
  (*
  ** Optional specification of parameter value range
  *)
  mutable tde_range: tde_range list;
  (*
  ** Type specifier
  *)
  mutable tde_type : char;
  (*
  ** Environment parameter or variable?
  *)
  mutable tde_var : bool;
  
  (*
  ** Array? If true, above tde_val holds array elements.
  *)
  mutable tde_array: bool;
}

type tdi_fun = {
  mutable tdf_name: string;
  mutable tdf_code: string list;
  mutable tdf_env: (string,tdi_env) Hashtbl.t;
}

