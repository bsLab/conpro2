(*
** ConPro ML implementation library
** SEMAPHORE
*)
open Conpro
open Process

type semaphore_t = {
  mutable qawait: int array;
  mutable count: int;
  mutable qawait_head: int;
  mutable qawait_tail: int;
}

let mutable semas = []
let sema_handler p id =
  let s = List.nth semas id in
  try for i = 0 to max_PROC-1 
  do
    if s.qawait.(i) = p.id then
    begin
      s.qawait.(i) <- nilPID;
      raise Exit;
    end;  
  done with Exit -> ()

class semaphore env =
  object (self)
  
  val version = "1.5"
  val s = {
    qawait = Array.create max_PROC nilPID;
    qawait_head=0;
    qawait_tail=0;
    count=0;
  }
  
  val mutable id = 0
  
  initializer 
    self#config env;
    id <- List.length semas;
    semas <- semas @ [s];
    
    
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | "init" -> s.count <- int_of_string v;
      | _ -> () ) env
    
      
  method init n = 
    s.count <- n;
    s.qawait_head <- 0;
    s.qawait_tail <- 0
    
  method down () = 
    let p = process_self () in
    if s.count = 0 then
    begin
      process_await p 0 sema_handler id;

      s.qawait.(s.qawait_head) <- p.id;
      s.qawait_head <- s.qawait_head + 1; 
      if s.qawait_head = max_PROC then s.qawait_head <- 0;
      process_schedule ();
      self#down ()
    end
    else
      s.count <- s.count - 1
      
  method up () = 
    let p = process_self () in
    (*
    ** Find next waiting process in queue, if any. Processes can be already
    ** removed from await queue!
    *)
    s.count <- s.count + 1;
    while s.qawait.(s.qawait_tail) = nilPID && 
          s.qawait_tail <> s.qawait_head
    do
      s.qawait_tail <- s.qawait_tail + 1; 
      if s.qawait_tail = max_PROC then s.qawait_tail <- 0;
    done;
    if s.qawait_tail <> s.qawait_head then
    begin
      process_wakeup process_table.(s.qawait.(s.qawait_tail));
      s.qawait_tail <- s.qawait_tail + 1; 
      if s.qawait_tail = max_PROC then s.qawait_tail <- 0;
      process_schedule();
    end;
    
  method level () =
    s.count
    
  method unlock () =
    ()
end
