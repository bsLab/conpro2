val at_index : Cp_types.array_type -> int array -> int
val at_dimi : Cp_types.array_type -> int -> int -> string
val at_dims : Cp_types.array_type -> string -> int -> string
val at_dimsv : Cp_types.array_type -> string -> int -> string
val ao_of_ot :
  Cp_types.object_params list ->
  Cp_types.object_type -> Cp_types.abstract_object
val ao_of_sym :
  Cp_types.cp_module -> Cp_types.sym_type -> Cp_types.object_type
val co_of_ud : Cp_types.uc_data -> Cp_types.core_object option
val uo_of_ud : Cp_types.uc_data -> Cp_types.uc_object option
val dt_of_ud : Cp_types.uc_data -> Cp_types.data_type
val dt_of_udl : Cp_types.uc_data list -> Cp_types.data_type
val ud_conv : Cp_types.uc_data -> Cp_types.data_type -> unit
val array_guard : Cp_types.array_type -> bool * bool
val ud_guard : Cp_types.uc_data -> bool * bool
val ud_frag : Cp_types.uc_data -> bool
val ud_temp : Cp_types.uc_data -> bool
val ud_local : Cp_types.uc_data -> bool
val ud_name : Cp_types.uc_data -> string
val ud_val : Cp_types.uc_data -> Cp_types.value
val ud_fix_rhs : Cp_types.uc_data -> Cp_types.uc_data
val ud_fix_lhs : Cp_types.uc_data -> Cp_types.uc_data
val ud_default : Cp_types.uc_data -> Cp_types.value
val ud_change_sign :
  Cp_types.uc_data ->
  Cp_types.data_type option -> Cp_types.uc_data * Cp_types.data_type option
val pi_frag : Cp_types.instr -> bool
val is_value : Cp_types.object_type -> bool
val get_value : Cp_types.object_type -> Cp_types.value
val is_pi_value : Cp_types.instr -> bool
val is_const : Cp_types.object_type -> bool
val is_core : Cp_types.object_type -> bool
val print_timeunit : Cp_syntax.timeunit -> string
val print_frequnit : Cp_syntax.frequnit -> string
val is_mod_pro : string -> bool
val set_addr_sel :
  Cp_types.array_type ->
  Cp_types.uc_data -> Cp_types.object_type list -> unit
