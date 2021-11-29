val self : Cp_types.rl_module option ref
val my : Cp_types.sym_type -> bool
val get_procs :
  Cp_types.core_object ->
  Cp_types.process list * Cp_types.process list * Cp_types.process list *
  Cp_types.process list
val obj_port :
  Cp_types.sym_type ->
  Cp_types.cp_module -> Cp_types.process option -> string list * string list
val obj_map :
  Cp_types.sym_type ->
  Cp_types.cp_module -> Cp_types.process option -> string list
val obj_decl :
  Cp_types.sym_type ->
  Cp_types.cp_module -> Cp_types.process option -> string list * string list
val obj_code :
  Cp_types.sym_type ->
  Cp_types.cp_module -> Cp_types.process option -> string list
val instr_ucode :
  Cp_types.instr -> int ref -> Cp_types.process -> Cp_types.uc_instr list
val emit_mon :
  Cp_types.instr ->
  Cp_types.cp_module ->
  Cp_types.process option -> Cp_types.sym_type -> Cp_types.synth_data list
val emit_top_code :
  Cp_types.instr ->
  Cp_types.cp_module -> Cp_types.process option -> string list * string list
val top_code :
  Cp_types.cp_module -> Cp_types.process option -> string list * string list
val simple_assign :
  Cp_types.process ->
  Cp_types.uc_data ->
  Cp_types.uc_data ->
  Cp_types.synth_data list * Cp_types.synth_data list *
  Cp_types.synth_data list
val fun_scode :
  Cp_types.uc_instr ->
  string -> string -> 'a -> Cp_types.process -> Cp_types.state list
val fun_compile :
  'a -> Cp_types.process option -> Cp_types.instr -> 'b -> unit
val bf_time : 'a -> 'b -> 'c -> Cp_types.frame_time
val rules : Cp_types.rl_module
val init : unit -> unit
