val expr_type_of_str : string -> Cp_types.expr_type
val str_of_expr_type : Cp_types.expr_type -> string
val sprint_dt : Cp_types.data_type -> string
val print_dt : Cp_types.data_type -> unit
val sum : int list -> int
val suma : int array -> int
val prod : int list -> int
val proda : int array -> int
val source : unit -> Cp_types.source
val line : Cp_types.source -> unit
val i64 : int -> int64
val neg_i64 : int64 -> bool
val pos_i64 : int64 -> bool
val cyc_of_time : int64 -> Cp_syntax.timeunit -> int64
val per_of_freq : int64 -> Cp_syntax.frequnit -> int64
val const_width : Cp_types.value -> int
val dt_of_val : Cp_types.value -> Cp_types.data_type
val int_of_val : Cp_types.value -> int
val str_of_val : Cp_types.value -> string
val zeros : int -> string
val ones : int -> string
val val_str : Cp_types.data_type -> Cp_types.value -> string
val sel_width : int -> int
val is_uo_struct : Cp_types.uc_object -> bool
val get_uo_struct : Cp_types.uc_object -> Cp_types.object_type
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
val range_dt : Cp_types.data_type -> int * int -> Cp_types.data_type
val frag_dst_split :
  Cp_types.process ->
  Cp_types.uc_data -> Cp_types.uc_data -> Cp_types.uc_instr list
val frag_src_split :
  Cp_types.process ->
  Cp_types.uc_data -> Cp_types.uc_data -> Cp_types.uc_instr list
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
val sig_guard : Cp_types.vhdl_data -> string
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
