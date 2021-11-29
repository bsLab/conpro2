val max_PROC : int
val nilPID : int
val nilFD : 'a option
val nilEXC : int
val nilTMO : float
val nilPROC : 'a option
type process_states =
    PROC_FREE
  | PROC_READY
  | PROC_RUN
  | PROC_AWAIT
  | PROC_EXCEP
  | PROC_START
  | PROC_STOP
and process_t = {
  mutable id : int;
  mutable name : string;
  mutable state : process_states;
  mutable pend_excep : int;
  mutable handler : process_t -> int -> unit;
  mutable arg : int;
  mutable read : Unix.file_descr option;
  mutable write : Unix.file_descr option;
  mutable excep : Unix.file_descr option;
  mutable join: int;
  mutable context : process_context;
  mutable func : int -> unit;
  mutable fun_arg : int;
  mutable next : process_t option;
  mutable timeout : float;
  mutable signal: int;
} 
and process_context = {
  mutable uc_thread : Thread.t option;
  mutable uc_ev : bool;
} 
and timers_t = {
  qawait : int array;
  mutable on : bool;
  mutable timeout : float;
  mutable interval : int;
  mutable once : bool;
  mutable waiting : int;
  mutable next : timers_t option;
} 
val nilT : timers_t option variable
val _handler : 'a -> 'b -> unit
val process_table : process_t array
val process_top : int variable
val process_running : int variable
val q_timer : timers_t option ref
val q_process_ready : process_t option ref
val q_process_await : process_t option ref
val q_process_start : process_t option ref
val q_process_stop : process_t option ref
val next : process_t option -> process_t option
val get : 'a option -> 'a
val mem : process_t option ref -> process_t -> bool
val head : process_t option ref -> process_t
val add : process_t option ref -> process_t -> unit
val rem : process_t option ref -> process_t -> unit
val empty : 'a option ref -> bool
val fd_id : 'a option -> int
val process_await :
  process_t -> int -> (process_t -> int -> unit) -> int -> unit
val process_await_io :
  process_t -> int -> char -> Unix.file_descr -> unit
val process_wakeup : process_t -> unit
val process_self : unit -> process_t 
val process_dump : unit -> unit
val process_io : unit -> int
val process_init : unit -> unit
val process_new : string -> (int -> unit) -> int -> process_t
val process_schedule : unit -> unit
val process_start : process_t -> unit
val process_stop : process_t -> unit
val process_call : process_t -> unit
val process_end : unit -> unit
val process_delay : int -> unit
class process :
  (string * string) list ->
  (int -> unit) ->
  object
    method call : unit -> unit
    method config : (string * string) list -> unit
    method start : unit -> unit
    method stop : unit -> unit
    val mutable pro : process_t option
    val mutable pro_id : int
    val mutable pro_name : string
    val version : string
  end
