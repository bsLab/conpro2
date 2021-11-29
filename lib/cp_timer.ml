(*
** ConPro ML implementation library
** TIMER
*)
open Conpro
open Process


let mutable timers = ([]:timers_t list)
let timer_handler p id =
  let t = List.nth timers id in
  try for i = 0 to max_PROC-1 
  do
    if t.qawait.(i) = p.id then
    begin
      t.qawait.(i) <- nilPID;
      raise Exit;
    end;  
  done with Exit -> ()

class timer env =
  object (self)
  val version = "1.4"
  
  val t = {
        qawait=Array.create max_PROC nilPID;
        waiting=0; 
        timeout = nilTMO;
        interval = 0;
        once = false;
        on=false;
        next=None;
      }:timers_t
    
  val mutable id = 0
  
  initializer 
    self#config env;
    id <- List.length timers;
    timers <- timers @ [t];
    if !q_timer = nilT then q_timer := Some t
    else 
    begin
      (*
      ** Put on top of the queue
      *)
      t.next <- !q_timer;
      q_timer := Some t;
    end;
    
    
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | "time" -> t.interval <- int_of_string v;
      | "mode" -> t.once <- (int_of_string v) = 1;
      | _ -> () ) env
      
  method init () = 
    t.on <- false
    
    
  method time v = 
    t.interval <- v
    
  method await () =
    let p = process_self () in
    process_await p 0 timer_handler id;
    t.qawait.(t.waiting) <- p.id;
    t.waiting <- t.waiting + 1;
    process_schedule ()
    
  method start () =
    let date = Unix.gettimeofday () in
    t.on <- true;
    t.timeout <- date +. ((float_of_int t.interval) *. 1e-6)
    
      
  method stop () =
    t.on <- false;
    t.timeout <- nilTMO
    
end



