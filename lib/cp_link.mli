class link :
  (string*string) list ->
  object ('a)
    method selector : bool
    method config : (string*string) list -> unit
    method init : unit
    method stop  : unit
    method start  : unit
    method interface  : int -> int -> int -> int -> unit
    method read : int*bool
    method write : int -> bool
end
