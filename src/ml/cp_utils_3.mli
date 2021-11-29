val range_dt : Cp_types.data_type -> int * int -> Cp_types.data_type
val frag_dst_split :
  Cp_types.process ->
  Cp_types.uc_data -> Cp_types.uc_data -> Cp_types.uc_instr list
val frag_src_split :
  Cp_types.process ->
  Cp_types.uc_data -> Cp_types.uc_data -> Cp_types.uc_instr list
