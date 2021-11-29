val ui_sprint_range : (int * int) option -> string
val ui_sprint_index : Cp_types.uc_arg option -> string
val ui_sprint_dt : Cp_types.data_type -> string
val ui_sprint_conv : Cp_types.data_type option -> string
val ui_sprint_flags : Cp_types.uo_flags list -> string
val ui_sprint_type : Cp_types.uo_type -> string
val ui_sprint_uo : Cp_types.uc_object -> string
val ui_sprint_uat : Cp_types.uc_array -> string
val ui_sprint_ut : Cp_types.uc_temp -> string
val ui_sprint_val : Cp_types.value -> string
val ui_sprint_ul : Cp_types.uc_list -> string
val ui_sprint_ua : Cp_types.uc_alu -> string
val ui_sprint_ud : Cp_types.uc_data -> string
val ui_sprint_uc : Cp_types.uc_instr -> string
val ui_emit_pro_ucode : Cp_types.cp_module -> Cp_types.process -> unit
val ui_emit_ucode : Cp_types.cp_module -> unit
val ui_compact_ucode : Cp_types.uc_instr list -> Cp_types.uc_instr list
val ui_bind_ucode :
  Cp_types.process -> Cp_types.uc_instr list -> Cp_types.uc_instr list
val ui_infer_alu : Cp_types.process -> unit
val ui_resolve_temps : Cp_types.process -> unit
val ui_resolve_bool : Cp_types.process -> unit
val ui_expr_type_align : Cp_types.process -> unit
