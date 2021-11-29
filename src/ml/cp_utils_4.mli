val op_type : string -> Cp_syntax.operator
val op_name : Cp_syntax.operator -> string
val alu_state_str : Cp_syntax.operator -> string
val op_vhdl : Cp_syntax.operator -> string
val op_mode : Cp_syntax.operator -> Cp_syntax.operator_group
val op_level : Cp_syntax.operator -> int
val op_special : Cp_syntax.operator -> bool
val new_tmp' :
  Cp_types.core_object list ref -> Cp_types.data_type -> Cp_types.core_object
val new_tmp : Cp_types.process -> Cp_types.data_type -> Cp_types.core_object
val release_tmp : Cp_types.process -> Cp_types.core_object -> unit
val get_tmp : Cp_types.process -> Cp_types.data_type -> Cp_types.core_object
val tmp_reg : Cp_types.process -> Cp_types.data_type -> Cp_types.core_object
val new_sig :
  Cp_types.cp_module ->
  Cp_types.process option ->
  string -> Cp_types.data_type -> Cp_types.core_object
val new_reg :
  Cp_types.cp_module ->
  Cp_types.process option ->
  string -> Cp_types.data_type -> Cp_types.core_object
val alu_thres : Cp_types.process -> int
val alu_use : Cp_types.process -> bool
val check_alu :
  Cp_types.process -> Cp_syntax.operator list -> Cp_types.data_type -> bool
val add_alu :
  Cp_types.process -> Cp_syntax.operator list -> Cp_types.data_type -> unit
val find_alu : Cp_types.process -> Cp_types.data_type -> Cp_types.pro_alu
