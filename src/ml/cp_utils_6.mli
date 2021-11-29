val get_rules : Cp_types.object_type -> Cp_types.rl_module
val get_state_names : Cp_types.process -> string list
val is_mon : Cp_types.sym_type -> bool
val get_mon : Cp_types.sym_type -> bool * bool * Cp_types.sym_type
val to_default : string -> string
val default_by_name : Cp_types.process -> string -> string
val default_by_obj : Cp_types.instr -> string
val default_by_dt : Cp_types.data_type -> string
val to_lower : string -> string
val ieee : unit -> string list
val get_block_params : Cp_types.instr -> Cp_types.block_params list
val name_of_modgen : Cp_types.modgen -> string
val find_modgen : string -> string
val is_local : Cp_types.uc_data -> bool
val is_ot_local : Cp_types.process option -> Cp_types.object_type -> bool
val fix_range : string -> string
val fix_data_type : Cp_types.uc_data -> Cp_types.uc_data -> unit
val fix_val_type : Cp_types.uc_data -> Cp_types.uc_data -> unit
val is_val : Cp_types.uc_data -> bool
val sort_data_path : Cp_types.synth_data list -> Cp_types.synth_data list
val prefix : string -> string
val module_pro_main : Cp_types.cp_module -> Cp_types.process
val get_int_of_str : string -> int
val logic_range : string -> string -> string list
val int_range : string -> string -> string list
val constr_of_block :
  string ->
  Cp_types.block_params list ->
  Cp_types.block_constraints list -> Cp_types.block_constraints
val print_block_constr : Cp_types.block_constraints -> unit
val expr_model : Cp_types.process -> Cp_types.expr_type
val add_module : string -> bool
