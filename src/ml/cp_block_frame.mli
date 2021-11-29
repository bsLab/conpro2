val nilsrc_v : Cp_types.source
val nilbf : Cp_types.block_frame
val src_copy : Cp_types.source -> Cp_types.source -> unit
val inherit_params : Cp_types.block_frame -> unit
val create_frame : Cp_types.instr list -> Cp_types.block_frame
val frame_of_block : Cp_types.instr -> Cp_types.block_frame
val instr_frame :
  Cp_types.instr list -> Cp_types.block_frame option -> Cp_types.instr list
val sprint_time : Cp_types.frame_time -> string
val add_time :
  Cp_types.frame_time -> Cp_types.frame_time -> Cp_types.frame_time
val compact_time : Cp_types.frame_time -> Cp_types.frame_time
val print_frame : Cp_types.block_frame -> string
val print_frame_tree : Cp_types.block_frame -> unit
val bf_add_time : Cp_types.block_frame -> Cp_types.frame_time -> unit
type minmax = Min | Max
val frame_time_calc : Cp_types.process -> unit
val print_frame_trees : Cp_types.cp_module -> unit
