class uart :
  (string*string) list ->
  object ('a)
    method config : (string*string) list -> unit
    method init : unit -> unit
    method stop  : unit -> unit
    method start  : unit -> unit
    method baud : int -> unit
    method interface  : int -> int -> unit
    method read : unit -> int*bool
    method write : int -> bool
end
