open Cp_common
open Cp_types
open Cp_symbol
open Hashtbl

type c_obj_desc =
  {
    mutable desc_name: string;
    mutable desc_obj: c_env_obj;
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
and c_struct =
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
and c_array =
  {
    mutable array_name: string;
    mutable array_dims: int array;
    mutable array_type: c_env_obj;
  }
and c_enum =
  {
    mutable enum_name: string;
    mutable enum_elems: string list;
    mutable enum_numbers: int list;
  }
(*
*)
and c_storage =
  {
    mutable sto_name: string;
    mutable sto_type: c_env_obj;
    mutable sto_mutable: bool;  (* For example loop identifiers are not... *)
    mutable sto_export: bool;   (* Exported storage object *)
    mutable sto_ref: bool;      (* Dereferenced Pointer access required? *)
    mutable sto_subst: string;
    mutable sto_def: value option;
    mutable sto_sym: bool;
  }
and c_abstract =
  {
    mutable abstr_name: string;
    mutable abstr_env: (string*string) list;
    mutable abstr_type: c_env_obj;
    mutable abstr_code : string list;
    mutable abstr_class: string;
    mutable abstr_class_type: string;
    mutable abstr_subst: string;
  }
and c_exception = 
  {
    mutable exc_name: string;
    mutable exc_id: int;
  }
and c_fun = 
  {
    mutable fn_name: string;
    mutable fn_args: (string*core_object) list;
    mutable fn_rets: (string*core_object) list;
    mutable fn_inline: bool;
  }
and c_env_obj =
  | C_struct of c_struct
  | C_array of c_array
  | C_enum of c_enum
  | C_type of data_type
  | C_abstract_type of abstract_object
  | C_exception of c_exception
  | C_storage of c_storage
  | C_abstract of c_abstract
  | C_fun of c_fun
  | C_unknown
and c_level =
  | C_level_high
  | C_level_mid
  | C_level_low
and c_env = 
  {
    env_glob: (string , c_env_obj) Hashtbl.t ;
    env_loc: (string , c_env_obj) Hashtbl.t;
    env_level: c_level;
  }
