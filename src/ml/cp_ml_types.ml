open Cp_common
open Cp_types
open Cp_symbol
open Hashtbl

type ml_obj_desc =
  {
    mutable desc_name: string;
    mutable desc_obj: ml_env_obj;
    mutable desc_num_sel: int list;
    mutable desc_sel: string list;
    mutable desc_array: bool;
    mutable desc_struct: bool;
    mutable desc_mutable: bool;
    mutable desc_export: bool;
    mutable desc_subst: string;
    mutable desc_ref: bool;
  }
(*
*)
and ml_struct =
  {
    mutable strut_name: string;
    mutable strut_elems: (string*data_type) list;
    (*
    ** ConPro: structure element names have type local context
    ** ML: structure element names have type global context
    ** subst: unique element names <st_name*elem_name>
    *) 
    mutable strut_names: string list;
  }  
and ml_array =
  {
    mutable array_name: string;
    mutable array_dims: int array;
    mutable array_type: ml_env_obj;
  }
and ml_enum =
  {
    mutable enum_name: string;
    mutable enum_elems: string list;
    mutable enum_numbers: int list;
  }
(*
*)
and ml_storage =
  {
    mutable sto_name: string;
    mutable sto_type: ml_env_obj;
    mutable sto_mutable: bool;  (* For example loop identifiers are not... *)
    mutable sto_export: bool;   (* Exported storage object *)
    mutable sto_ref: bool;      (* Dereferenced Pointer access required? *)
    mutable sto_subst: string;
    mutable sto_def: value option;
    mutable sto_sym: bool;
  }
and ml_abstract =
  {
    mutable abstr_name: string;
    mutable abstr_env: (string*string) list;
    mutable abstr_type: ml_env_obj;
    mutable abstr_code : string list;
    mutable abstr_class: string;
    mutable abstr_class_type: string;
    mutable abstr_subst: string;
    mutable abstr_cond: string;
  }
and ml_exception = 
  {
    mutable exc_name: string;
    mutable exc_id: int;
  }
and ml_fun = 
  {
    mutable fn_name: string;
    mutable fn_args: (string*core_object) list;
    mutable fn_rets: (string*core_object) list;
    mutable fn_inline: bool;
  }
and ml_env_obj =
  | ML_struct of ml_struct
  | ML_array of ml_array
  | ML_enum of ml_enum
  | ML_type of data_type
  | ML_abstract_type of abstract_object
  | ML_exception of ml_exception
  | ML_storage of ml_storage
  | ML_abstract of ml_abstract
  | ML_fun of ml_fun
  | ML_unknown
and ml_level =
  | ML_level_high
  | ML_level_mid
  | ML_level_low
and ml_env = 
  {
    env_glob: (string , ml_env_obj) Hashtbl.t ;
    env_loc: (string , ml_env_obj) Hashtbl.t;
    env_level: ml_level;
  }
