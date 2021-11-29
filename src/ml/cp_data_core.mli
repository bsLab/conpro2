val uo_type : Cp_types.data_type -> Cp_types.data_type -> Cp_types.uo_type
val copy_uo_type : Cp_types.uo_type -> Cp_types.uo_type
val obj_decl_type : Cp_types.data_type -> string
val obj_size : Cp_types.data_type -> int
val dt_of_td : Cp_types.data_id -> int -> Cp_types.data_type
val td_of_dt : Cp_types.data_type -> Cp_types.data_id
val id_of_td : Cp_types.data_id -> int
val id_of_dt : Cp_types.data_type -> int
val to_of_ot : Cp_types.object_type -> Cp_types.object_id
val name_of_ot : Cp_types.object_type -> string
val dt_of_ot : Cp_types.object_type -> Cp_types.data_type option
val co_of_ot : Cp_types.object_type -> Cp_types.core_object option
val size_of_dt : Cp_types.data_type -> int
val sprint_dt : Cp_types.data_type -> string
val sprint_td : Cp_types.data_id -> string
val print_dt : Cp_types.data_type -> unit
val sprint_ot : Cp_types.object_type -> string
val sprint_opl : Cp_types.object_params list -> string
val is_value : Cp_types.object_type -> bool
val get_value : Cp_types.object_type -> Cp_types.value
val is_const : Cp_types.object_type -> bool
val is_sub : Cp_types.object_params list -> bool
val is_index : Cp_types.object_params list -> bool
val is_index_obj : Cp_types.object_params list -> bool
val is_conv : Cp_types.object_params list -> bool
val is_neg : Cp_types.object_params list -> bool
val is_sel : Cp_types.object_params list -> bool
val is_sel_obj : Cp_types.object_params list -> bool
val is_array : Cp_types.object_type -> bool
val obj_sub : Cp_types.object_params list -> int * int
val obj_index : Cp_types.object_params list -> int
val obj_index_obj : Cp_types.object_params list -> Cp_types.instr
val obj_conv : Cp_types.object_params list -> Cp_types.data_type
val obj_neg : Cp_types.object_params list -> Cp_types.data_type
val obj_sel : Cp_types.object_params list -> int array
val obj_sel_obj : Cp_types.object_params list -> Cp_types.instr
val obj_dt : Cp_types.instr -> Cp_types.data_type
val ot_of_pi : Cp_types.instr -> Cp_types.object_type
val rot_of_pi : Cp_types.instr -> (int * int) option * Cp_types.object_type
val dt_of_pi : Cp_types.instr -> Cp_types.data_type
val conv_dt_of_pi : Cp_types.instr -> Cp_types.data_type option
val name_of_pi : Cp_types.instr -> string
val type_of_ud : Cp_types.uc_data -> Cp_types.uo_type
val to_of_ud : Cp_types.uc_data -> Cp_types.object_id
val name_of_ud : Cp_types.uc_data -> string
val pi_get_objs : Cp_types.instr -> Cp_types.instr list
val sprint_vhdl_data : Cp_types.vhdl_data -> string
val print_vhdl_data : Cp_types.vhdl_data -> unit
