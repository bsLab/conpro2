(*
** ConPro ML implementation library
** MUTEX
*)
open Conpro
open Process

type mutex_t = {
  mutable qawait: int array;
  mutable owner: int;
  mutable qawait_head: int;
  mutable qawait_tail: int;
}

let mutable mutexes = []
let mutex_handler p id =
  let mu = List.nth mutexes id in
  try for i = 0 to max_PROC-1 
  do
    if mu.qawait.(i) = p.id then
    begin
      mu.qawait.(i) <- nilPID;
      raise Exit;
    end;  
  done with Exit -> ()

class mutex env =
  object (self)
  val version = "1.4"
  val m = {
    qawait = Array.create max_PROC nilPID;
    qawait_head=0;
    qawait_tail=0;
    owner=nilPID;
  }
  val mutable id = 0
  val mutable model = ""
  
  initializer 
    self#config env;
    id <- List.length mutexes;
    mutexes <- mutexes @ [m];
    
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | "model" -> model <- v;
      | _ -> () ) env
      
  method init () = 
    m.qawait_head <- 0;
    m.qawait_tail <- 0;
    m.owner <- nilPID
    
  method lock () =
    let p = process_self () in
    if m.owner <> nilPID then
    begin
      process_await p 0 mutex_handler id;

      m.qawait.(m.qawait_head) <- p.id;
      m.qawait_head <- m.qawait_head + 1; 
      if m.qawait_head = max_PROC then m.qawait_head <- 0;
      process_schedule ();
      self#lock ()
    end
    else
      m.owner <- p.id;

  method unlock () =
    let p = process_self () in
    (*
    ** Find next waiting process in queue, if any. Processes can be already
    ** removed from await queue!
    *)
    m.owner <- nilPID;
    while m.qawait.(m.qawait_tail) = nilPID && 
          m.qawait_tail <> m.qawait_head
    do
      m.qawait_tail <- m.qawait_tail + 1; 
      if m.qawait_tail = max_PROC then m.qawait_tail <- 0;
    done;
    if m.qawait_tail <> m.qawait_head then
    begin
      process_wakeup process_table.(m.qawait.(m.qawait_tail));
      m.qawait_tail <- m.qawait_tail + 1; 
      if m.qawait_tail = max_PROC then m.qawait_tail <- 0;
      process_schedule();
    end;

end



