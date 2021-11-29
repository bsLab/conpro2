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
