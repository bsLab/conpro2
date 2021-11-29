val ud_of_ot :
  Cp_types.object_type ->
  Cp_types.object_params list ->
  Cp_types.uo_flags list -> Cp_types.data_type -> Cp_types.uc_data
val ud_of_ot :
  Cp_types.object_type ->
  Cp_types.object_params list ->
  Cp_types.uo_flags list -> Cp_types.data_type -> Cp_types.uc_data
val fun_get_ud :
  Cp_types.process option ->
  string ->
  string ->
  Cp_types.instr list ->
  int -> Cp_types.data_type option -> bool -> Cp_types.uc_data
val vhdl_rhs_rules :
  (Cp_types.data_type ->
   Cp_types.data_type option ->
   Cp_types.data_type ->
   Cp_types.data_type option ->
   Cp_types.data_type option ->
   Cp_types.data_type option ->
   (int * int) option ->
   Cp_types.uc_arg option ->
   Cp_types.object_id ->
   ((Cp_types.data_id * Cp_types.data_id option * Cp_types.data_id *
     Cp_types.data_id option * bool * Cp_types.data_id option) *
    (string -> Cp_types.vd_conv))
   list)
  ref
val vhdl_lhs_rules :
  (Cp_types.data_type ->
   Cp_types.data_type option ->
   Cp_types.data_type ->
   Cp_types.data_type option ->
   Cp_types.data_type option ->
   Cp_types.data_type option ->
   (int * int) option ->
   Cp_types.uc_arg option ->
   Cp_types.object_id ->
   ((Cp_types.data_id * Cp_types.data_id option * Cp_types.data_id *
     Cp_types.data_id option * bool * Cp_types.data_id option) *
    (string -> Cp_types.vd_conv))
   list)
  ref
val type_name : Cp_types.data_type -> string
val vhdl_convert :
  Cp_types.data_type -> Cp_types.data_type -> string * string
val vhdl_conv_neg :
  Cp_types.data_type -> Cp_types.data_type option -> string -> string
val vhdl_conv_range :
  (int * int) option ->
  Cp_types.uc_arg option ->
  string -> Cp_types.data_type -> string * Cp_types.data_type
val vhdl_conv_lhs_range :
  (int * int) option ->
  Cp_types.uc_arg option ->
  string -> Cp_types.data_type -> string * Cp_types.data_type
val vhdl_conv_dt :
  string ->
  Cp_types.data_type -> Cp_types.data_type -> string * Cp_types.data_type
val vhdl_convert_ud :
  string ->
  Cp_types.uo_type ->
  Cp_types.uo_flags list ->
  (int * int) option ->
  Cp_types.uc_arg option -> Cp_types.object_id -> Cp_types.vd_conv
val vhdl_of_val : Cp_types.data_type -> Cp_types.value -> Cp_types.vhdl_data
val vhdl_of_ud :
  Cp_types.process ->
  Cp_types.pro_alu option ->
  Cp_types.uc_data -> (string * string) option -> Cp_types.vhdl_data
val vhdl_arithm_aux_n : int ref
val vhdl_of_expr :
  Cp_types.process ->
  Cp_types.uc_data ->
  Cp_types.uc_data -> Cp_syntax.operator -> Cp_types.vhdl_data
val vhdl_sig_of_ud : Cp_types.process -> Cp_types.uc_data -> string
val vhdl_sens_of_ud : Cp_types.process -> Cp_types.uc_data -> string list
