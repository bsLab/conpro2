type stat_env = {
  stat_name : string;
  stat_desc : (string, stat_env) Hashtbl.t;
  mutable stat_count : int;
} 
val stat_env : (string, stat_env) Hashtbl.t
val lookup_env : (string, 'a) Hashtbl.t -> string -> 'a
val try_lookup_env : ('a, 'b) Hashtbl.t -> 'a -> 'b option
val get_env : ('a, 'b) Hashtbl.t -> 'b list
val stat : string -> string -> unit
val stat_set : string -> string -> int -> int -> unit
val print_stat : unit -> unit
val analysed : (string * string) list ref
val analyse_log : string -> string -> unit
val analyse_summary : unit -> unit
val optimized : (string * string) list ref
val optimize_log : string -> string -> unit
val optimize_summary : unit -> unit
val ucoded : (string * string) list ref
val ucode_log : string -> string -> unit
val ucode_summary : unit -> unit
val rtled : (string * string) list ref
val rtl_log : string -> string -> unit
val rtl_summary : unit -> unit
val called : (string * string) list ref
val call_log : Cp_types.process -> unit
val call_exc_log : Cp_types.core_object -> unit
val call_summary : unit -> unit
