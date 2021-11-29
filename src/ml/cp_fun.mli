val fun_get_time :
  string ->
  string -> Cp_types.instr list -> int -> int64 * Cp_syntax.timeunit
val fun_get_str : string -> string -> Cp_types.instr list -> int -> string
val fun_get_str2 : string -> string -> Cp_types.instr list -> int -> string
val fun_get_int : string -> string -> Cp_types.instr list -> int -> int
val fun_get_bool : string -> string -> Cp_types.instr list -> int -> bool
val fun_get_freq :
  string ->
  string -> Cp_types.instr list -> int -> int64 * Cp_syntax.frequnit
val fun_get_arg :
  Cp_types.process ->
  string ->
  string ->
  Cp_types.instr list ->
  Cp_types.uo_flags list ->
  int -> Cp_types.data_type option -> Cp_types.uc_arg
val fun_get_arg_ot :
  string -> string -> Cp_types.instr list -> int -> Cp_types.object_type
