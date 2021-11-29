val emit_mon :
  Cp_types.instr ->
  Cp_types.cp_module ->
  Cp_types.process option -> Cp_types.sym_type -> Cp_types.synth_data list
val emit_top_code :
  Cp_types.instr ->
  Cp_types.cp_module -> Cp_types.process option -> (string list) * (string list)
val top_code : Cp_types.cp_module -> Cp_types.process option -> (string list) * (string list)
