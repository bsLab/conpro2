class ioport :
  (string * string) list ->
  object
    method config : (string * string) list -> unit
    method dir : int -> unit
    method init : unit -> unit
    method interface  : int -> unit    
    method read : unit -> int 
    method write : int -> unit
    val mutable datawidth : int
    val version : string
  end
