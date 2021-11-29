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
**    $CREATED:     8.7.2008
**    $VERSION:     2.05
**
**    $INFO:
**
**  External Module Interface - Types
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

type emi_env = env

type emi_method = {
  mutable emm_name : string;
  (*
  ** Method parameter descriptor
  *)
  mutable emm_args : Cp_types.arg_desc list;
  mutable emm_procs: Cp_types.process list;
} 
type emi_signal = {
  mutable emi_sig_name: string;
  mutable emi_sig_type: data_type;
  mutable emi_sig_dir: port_type option;
  mutable emi_procs: string list;
  mutable emi_kind: char; (* signal/variable *)
}
type emi_const = {
  mutable emi_const_name: string;
  mutable emi_const_type: data_type;
  mutable emi_const_val: string;
}
type emi_type = {
  mutable emt_type_name: string;
  mutable emt_type_type: data_type option;
  mutable emt_type_range: env_range option;
  mutable emt_type_cont: string list;
  mutable emt_kind: char; (* enumeration/array *)
}
type emi_mapping = {
  mutable ema_lhs: string;
  mutable ema_rhs: string;
  mutable ema_procs: string list;
}
type emi_arg_context = {
  mutable eac_pro: Cp_types.process;
  mutable eac_ao: Cp_types.abstract_object;
  mutable eac_uargs: Cp_types.uc_arg list ;
}
type emi_access = {
  mutable emc_name: string;
  (*
  ** Generic data object access during runtime
  *)
  
  (*
  ** Synthesis data generator functions using
  ** arg_context: {actual method argument descriptor, process and abstract object context}.
  ** arg_data_type (=ud.phys_type/data_type) can differ from above emm_args definition => 
  ** type conversion required!
  *)
  mutable emc_data: ( emi_arg_context option -> Cp_types.synth_data list) list;
  mutable emc_control: (emi_arg_context option -> Cp_types.synth_data list) list;
  (*
  ** For non runtime methods
  *)
  
  (*
  ** Set parameters 
  *)
  mutable emc_set: (emi_arg_context option -> Cp_types.env_expr * Cp_types.env_expr) list;
}
type emi_process = {
  mutable emp_name: string;
  mutable emp_decl: string list;
  mutable emp_code: string list;
  mutable emp_sens: string list;
}

