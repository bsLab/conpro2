class event :
  (string * string) list ->
  object
    method await : unit -> unit
    method config : (string * string) list -> unit
    method init : unit -> unit
    method wakeup : unit -> unit
end
