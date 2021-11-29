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
**    $CREATED:     4.11.2008
**    $VERSION:     2.02
**
**    $INFO:
**
**  Micro Code Interface - Types
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



type uci_env = {
  (*
  ** Name of environment parameter
  *)
  mutable uce_name : string;
  (*
  ** Default or actual value (required)
  *)
  mutable uce_val  : string;
  (*
  ** Type specifier
  *)
  mutable uce_type : char;
  (*
  ** Environment parameter or variable?
  *)
  mutable uce_var : bool;
  
  (*
  ** Array?
  *)
  mutable uce_array: bool;
}

type uci_type = {
  uct_name : string;
  uct_kind: char;
  uct_def: string list;
}
