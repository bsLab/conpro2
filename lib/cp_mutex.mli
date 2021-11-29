type mutex_t = {
  mutable qawait : int array;
  mutable owner : int;
  mutable qawait_head : int;
  mutable qawait_tail : int;
} 
class mutex :
  (string * string) list ->
  object
    method config : (string * string) list -> unit
    method init : unit -> unit
    method lock : unit -> unit
    method unlock : unit -> unit
    val mutable id : int
    val m : mutex_t
    val mutable model : string
    val version : string
  end
