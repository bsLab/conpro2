(*
** ConPro ML implementation library
** QUEUE
*)
open Conpro
open Process


type queue_t = {
  mutable size: int;
  mutable head: int;
  mutable tail: int;
  mutable full: bool;
  mutable empty: bool;
  
  mutable qawait: int array;
  mutable timeout: int array;
  mutable waiting: int;
  mutable qawait_head: int;
  mutable qawait_tail: int;
}

let mutable queues = []
let queue_handler p id =
  let q = List.nth queues id in
  try for i = 0 to max_PROC-1 
  do
    if q.qawait.(i) = p.id then
    begin
      q.qawait.(i) <- nilPID;
      raise Exit;
    end;  
  done with Exit -> ()

class ['a] queue env t =
  object (self)
  val version = "1.8"
  val mutable data = ([|t|]: 'a array)
  
  val q = {
        size=16;
        head=0;
        tail=0;
        full=false;
        empty=true; 
        
        qawait=Array.create max_PROC nilPID;
        waiting=0;
        timeout=Array.create max_PROC 0;
        qawait_head=0;
        qawait_tail=0;
      }
    
  val mutable id = 0
  
  initializer 
    self#config env;
    id <- List.length queues;
    queues <- queues @ [q];
    data<- Array.create q.size t
    
    
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | "size" -> q.size <- int_of_string v;
      | _ -> () ) env
      
  method init () = 
    q.head <- 0; 
    q.tail <- 0;
    q.full <- false;
    q.empty <- true;
    q.waiting <- 0;
    q.qawait_head <- 0;
    q.qawait_tail <- 0
    
  method read () = 
    let p = process_self () in
    if not q.empty then
    begin
      if q.full then
      begin
        (*
        ** Find next waiting process in queue, if any. Processes can be already
        ** removed from await queue!
        *)
        while q.qawait.(q.qawait_tail) = nilPID && 
              q.qawait_tail <> q.qawait_head
        do
          q.qawait_tail <- q.qawait_tail + 1; 
          if q.qawait_tail = max_PROC then q.qawait_tail <- 0;       
        done;
        if q.qawait_tail <> q.qawait_head then 
        begin
          process_wakeup process_table.(q.qawait.(q.qawait_tail));
          q.qawait_tail <- q.qawait_tail + 1; 
          if q.qawait_tail = max_PROC then q.qawait_tail <- 0;       
        end;
      end;
      q.full <- false;

      let d = data.(q.tail) in
      q.tail <- q.tail + 1;
      if q.tail = q.size then q.tail <- 0; 
      if q.head = q.tail then q.empty <- true;
      d;
    end
    else
    begin
      process_await p 0 queue_handler id;
      q.qawait.(q.qawait_head) <- p.id;
      q.timeout.(q.qawait_head) <- 0;
      q.qawait_head <- q.qawait_head + 1;
      if q.qawait_head = max_PROC then q.qawait_head <- 0;
      process_schedule ();      
      if p.signal = 0 then self#read () else 
      begin
        p.signal <- 0;
        t
      end;
    end
        
  method write d =
    let p = process_self () in
    if not q.full then
    begin
      if q.empty then
      begin
        (*
        ** Find next waiting process in queue, if any. Processes can be already
        ** removed from await queue!        
        *)
        while q.qawait.(q.qawait_tail) = nilPID &&
              q.qawait_tail <> q.qawait_head
        do
          q.qawait_tail <- q.qawait_tail + 1; 
          if q.qawait_tail = max_PROC then q.qawait_tail <- 0;       
        done;
        if q.qawait_tail <> q.qawait_head then
        begin
          process_wakeup process_table.(q.qawait.(q.qawait_tail));
          q.qawait_tail <- q.qawait_tail + 1; 
          if q.qawait_tail = max_PROC then q.qawait_tail <- 0;
        end;
      end;
      q.empty <- false;
      data.(q.head) <- d;
      q.head <- q.head + 1;
      if q.head = q.size then q.head <- 0;
      if q.head = q.tail then q.full <- true;
    end
    else
    begin
      process_await p 0 queue_handler id;
      q.qawait.(q.qawait_head) <- p.id;
      q.timeout.(q.qawait_head) <- 0;
      q.qawait_head <- q.qawait_head + 1;
      if q.qawait_head = max_PROC then q.qawait_head <- 0;
      process_schedule ();      
      if p.signal = 0 then
        self#write d
      else
        p.signal <- 0;
    end
    
  method unlock () =
    let p = process_self () in
    while q.qawait_tail <> q.qawait_head
    do
      let pid=q.qawait.(q.qawait_tail) in
      if pid <> nilPID then
      begin
        process_table.(pid).signal<-process_table.(pid).signal+1;
        process_wakeup process_table.(pid);
      end;
      q.qawait_tail<-q.qawait_tail+1; if q.qawait_tail = max_PROC then q.qawait_tail <- 0;       
    done;
    process_schedule ()
    
end
