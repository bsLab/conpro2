val op_time_table : Cp_types.resource_timing list ref
val op_add_time_table :
  Cp_syntax.operator list -> Cp_types.data_id -> string -> unit
val op_time : Cp_syntax.operator -> Cp_types.data_type -> float
val defaults : Cp_types.device -> unit
val get_dev_info : string -> Cp_types.device
