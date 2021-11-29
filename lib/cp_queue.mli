type queue_t = {
  mutable size : int;
  mutable head : int;
  mutable tail : int;
  mutable full : bool;
  mutable empty : bool;
  mutable qawait : int array;
  mutable timeout : int array;
  mutable waiting : int;
  mutable qawait_head : int;
  mutable qawait_tail : int;
} 
class ['a] queue :
  (string * string) list ->
  'a ->
  object
    method config : (string * string) list -> unit
    method init : unit -> unit
    method read : unit -> 'a
    method unlock : unit -> unit
    method write : 'a -> unit
    val mutable data : 'a array
    val mutable id : int
    val q : queue_t
    val version : string
  end
