open Thread
open Event
open Com_types
open Com_const
open Com_models
open Com_objs
open Com_send
open Com_recv
type process_state = PROC_init | PROC_start | PROC_run | PROC_end
class process_main () =
  object (self)
  val mutable process_id = None
  val process_ev = Event.create ()
  val mutable process_state = PROC_init
  val process_lock = Mutex.create ()

  val process_impl  = fun () ->
    rnd#init;
    ln#init;
    ln#start;
    recv#start;
    send#start;

  method process () =
    process_state <- PROC_start;
    while true do try 
    begin
      match process_state with
      | PROC_start ->
        Event.await process_ev;
        process_state <- PROC_run;
      | PROC_run -> 
        process_impl ();
        process_state <- PROC_end;
      | PROC_end ->
        (* main never ends! *)
        process_state <- PROC_start;
      | PROC_init -> 
        progerr "process_state";
    end
    with Thread_signal n -> process_state <- PROC_end; done

  method start = 
    (match process_id with
     | Some t -> self#stop; 
     | None -> process_id <- Some (Thread.create self#process ()););
    while process_state <> PROC_start do Thread.yield() done;
    Event.wakeup process_ev

  method stop = 
    (match process_id with
    | Some t -> if process_state <> PROC_end then Thread.signal t 1;
    | None -> ());
    while process_state <> PROC_end do Thread.yield() done;

  method call = 
    Mutex.lock process_lock;
    (match process_id with
     | Some t -> self#stop;
     | None -> ());
    self#start;
    if process_state <> PROC_end then Event.await process_ev;
    Mutex.unlock process_lock
end
let main = new process_main ()
