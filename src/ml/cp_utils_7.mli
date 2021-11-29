val is_vhdl_val : string -> bool
val log2 : int -> int
val log2_64 : int64 -> int64
val meth_check_args :
  Cp_types.arg_desc list -> Cp_types.arg_desc list -> bool
val meth_check :
  ('a * Cp_types.arg_desc list) list -> 'a -> Cp_types.arg_desc list -> bool
val arg_desc :
  string -> Cp_types.arg_type -> Cp_types.data_type -> Cp_types.arg_desc
val new_arg_desc : Cp_types.arg_type -> Cp_types.arg_desc
val new_arg_desc_dt : Cp_types.arg_type -> Cp_types.data_type -> Cp_types.arg_desc
val set_arg_data_type : Cp_types.arg_desc -> Cp_types.data_type -> unit
val get_arg_data_type : Cp_types.arg_desc -> Cp_types.data_type
val find_str : string -> string -> bool
val is_num : string -> bool
val pro_of_ot : Cp_types.object_type -> Cp_types.process option
val pro_of_uo : Cp_types.uc_object -> Cp_types.process
val pro_of_ud : Cp_types.uc_data -> Cp_types.process
val remove_attr_from_obj: Cp_types.obj_flags -> Cp_types.object_type -> unit
