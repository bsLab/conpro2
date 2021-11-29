type semaphore_t = {
  mutable qawait : int array;
  mutable count : int;
  mutable qawait_head : int;
  mutable qawait_tail : int;
} 
class semaphore :
  (string * string) list ->
  object
    method config : (string * string) list -> unit
    method down : unit -> unit
    method init : int -> unit
    method level : unit -> int
    method unlock : unit -> unit
    method up : unit -> unit
    val mutable id : int
    val s : semaphore_t
    val version : string
  end
