
val _vhdl_rhs_rules :
  Cp_types.data_type ->
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
  list
val _vhdl_lhs_rules :
  Cp_types.data_type ->
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
  list

val init : unit -> unit
