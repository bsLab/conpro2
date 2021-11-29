class random1 :
  (string * string) list ->
  object
    method config : (string * string) list -> unit
    method init : unit -> unit
    method read : unit -> int
    method seed : int -> unit
    method selector : bool
    val mutable datatype : string
    val mutable datawidth : int
    val mutable seed_v : int
    val version : string
  end
class random2 :
  (string * string) list ->
  object
    method config : (string * string) list -> unit
    method init : unit -> unit
    method read : unit -> int
    method seed : int -> unit
    method selector : bool
    val mutable datatype : string
    val mutable datawidth : int
    val mutable seed_v : int
    val version : string
  end
