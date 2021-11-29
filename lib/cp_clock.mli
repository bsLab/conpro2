class clock :
  (string * string) list ->
  object
    method action : int -> unit
    method add : int -> unit
    method clock : int -> unit
    method config : (string * string) list -> unit
    method source : string -> unit
    val version : string
  end
