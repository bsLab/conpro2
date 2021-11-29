class timer :
  (string * string) list ->
  object
    method await : unit -> unit
    method config : (string * string) list -> unit
    method init : unit -> unit
    method start : unit -> unit
    method stop : unit -> unit
    method time : int -> unit
    val mutable id : int
    val t : Process.timers_t
    val version : string
  end
