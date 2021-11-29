class system :
  (string * string) list ->
  object
    method clock : int -> unit
    method clock_level : int -> unit
    method expr_type : string -> unit
    method reset_internal : bool -> unit
    method reset_level : int -> unit
    method simu_cycles : int -> unit
    method simu_res : int -> unit
    method target : string -> unit
end
