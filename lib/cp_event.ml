(*
** ConPro ML implementation library
** EVENT
*)
open Conpro
open Process

type event_t = {
  mutable qawait: int array;
  mutable waiting: int;
  mutable latched: bool;
  mutable latch: int;
}
let mutable events = []
let event_handler p id =
  let ev = List.nth events id in
  try for i = 0 to max_PROC-1 
  do
    if ev.qawait.(i) = p.id then
    begin
      ev.qawait.(i) <- nilPID;
      raise Exit;
    end;  
  done with Exit -> ()
    
class event env =
  object (self)
  
  val version = "1.4"
  val ev = {
        qawait=Array.create max_PROC nilPID;
        waiting=0; 
        latched = false;
        latch=0;
      }
    
  val mutable id = 0
  
  initializer 
    self#config env;
    id <- List.length events;
    events <- events @ [ev]
    
    
  method config (env: (string*string) list) = 
    List.iter (fun (e,v) ->
      match e with
      | "latch" -> if v = "1" then ev.latched <- true 
      | _ -> () ) env
      
  method init () = 
    ev.waiting <- 0;
    for i = 0 to max_PROC-1 do  ev.qawait.(i) <- nilPID; done
    
  method await () = 
    let p = process_self () in
    if not ev.latched || (ev.latched && ev.latch = 0) then
    begin
      process_await p 0 event_handler id;
      ev.qawait.(ev.waiting) <- p.id;
      ev.waiting <- ev.waiting + 1;
      process_schedule ();
    end
    else ev.latch <- 0
    
  method wakeup () = 
    if ev.waiting = 0 && ev.latched then ev.latch <- 1
    else
    begin
      for i = 0 to ev.waiting-1
      do
        let pid = ev.qawait.(i) in
        if pid <> nilPID then process_wakeup process_table.(pid);
      done;
      ev.waiting <- 0;
    end;
    process_schedule ()
    
end
