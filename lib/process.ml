open Unix
open Conpro
open Thread
open Printf
  
  
(*
** ConPro ML implementation library
** PROCESS 
** Version 1.5
*)
let max_PROC = 64
let proDBG = false

let nilPID = -1
let nilFD = None
let nilEXC = -1
let nilTMO = 0.0
let nilPROC = None

type process_states =
  | PROC_FREE
  | PROC_READY
  | PROC_RUN
  | PROC_AWAIT
  | PROC_EXCEP
  | PROC_START
  | PROC_STOP  

type process_t = {
  (*
  ** Unique process identifier PID
  *)
  mutable id: int;
  (*
  ** Descriptive name of process
  *)
  mutable name: string;
  (*
  ** Actual process state (type process_states)
  *)
  mutable state: process_states;  
  
  (*
  ** Pending exception signal?
  *)
  mutable pend_excep: int;
  
  (*
  ** Blocked processes (PROC_AWAIT) must
  ** have an interrupt handler which cleans
  ** up the blocked resource
  *)
  mutable handler: process_t -> int -> unit;
  
  (*
  ** Blocked resources passed to handler
  *)
  mutable arg: int;
  (*
  ** PROC_AWAIT on IO: read,write,execeptio
  *)
  mutable read: Unix.file_descr option;
  mutable write: Unix.file_descr option;
  mutable excep: Unix.file_descr option;
  (*
  ** PROC_AWAIT on process termination: call
  *)
  mutable join: int;
  (*
  ** Process context, process function and optional argument 
  *)
  mutable context: process_context;
  mutable func : int -> unit;
  mutable fun_arg: int;
  
  (*
  ** Linked list (queues)
  *)
  mutable next: process_t option;
  (*
  ** PROC_AWAIT with timeout? [abs. time, sec]
  *)
  mutable timeout: float;
  (*
  ** Signal pending?
  *)
  mutable signal: int;  
} 
and

process_context = {
  mutable uc_thread: Thread.t option;
  mutable uc_ev: bool;
}

and
(*
** from timer.h, but required here for
** process scheduling
*)
timers_t = {
  qawait: int array;
  mutable on: bool;
  (*
  ** [abs. time, sec]
  *)
  mutable timeout: float;
  (*
  ** [microsec]
  *)
  mutable interval: int;
  mutable once : bool;
  mutable waiting : int;
  mutable next: timers_t option;
  
  
}

let mutable nilT = None

    
let _handler p arg =
  ()

let process_table = 
  Array.init max_PROC (fun i -> 
    {
      id=i;
      name="";
      state= PROC_FREE;
      pend_excep=nilEXC;
      handler=_handler;
      arg=0;
      read=None;
      write=None;
      excep=None;
      join=nilPID;
      context=(fun i -> {uc_thread=None;uc_ev=false}) i;
      func=(fun _ -> ());
      fun_arg=0;
      next=nilPROC;
      timeout=0.0;
      signal=0;
    })
    
let mutable process_top = 0
let mutable process_running = 0
let q_timer = ref nilT
let q_process_ready = ref nilPROC
let q_process_await = ref nilPROC
let q_process_start = ref nilPROC
let q_process_stop  = ref nilPROC


(*
** Process Queue management
*)
let next qe =
  match qe with
  | Some q -> q.next
  | None -> None
let get qe =
  match qe with
  | Some q -> q
  | None -> progerr "get (Some q)"
    
let mem q p =
  let mutable qe = !q in
  while qe <> nilPROC && qe <> (Some p)
  do
    qe <- next qe;
  done;
  qe <> nilPROC

let head q =
  let head = get !q in
  q := head.next;
  head.next <- nilPROC;
  head
  
let add q p =
  if !q = nilPROC then q := Some p
  else
  begin
    let mutable qe = !q in
    while (next qe) <> nilPROC 
    do
      qe <- next qe;
    done;
    (get qe).next <- Some p;
  end
  
let rem q p =
  let mutable qe = !q in
  if !q = (Some p)  then q := next !q else 
  begin
    while qe <> nilPROC && (next qe) <> nilPROC && (next qe) <> (Some p)
    do
      qe <- next qe;
    done;
    if qe = nilPROC then conpro_err "[REM]: empty queue";
    if (next qe) = nilPROC then  conpro_err "[REM]: process not found";
    (get qe).next  <- p.next;
  end;
  p.next <- nilPROC

let empty q = !q = None

let print q =
  let mutable qe = !q in
  let mutable s = "[" in
  while qe <> nilPROC
  do
    let p = get qe in
    s <- sprintf "%s%d:%s%s%s%s%s" s p.id p.name
      (if p.read <> nilFD then "|R" else "")
      (if p.write <> nilFD then "|W" else "")
      (if p.excep <> nilFD then "|X" else "")
      (if p.timeout <> nilTMO then "|T" else "");
    if p.next <> nilPROC then  s <- sprintf "%s," s;
    qe <- p.next;
  done;
  s^"]"
  
  
let fd_id fd =
  match fd with
  | Some fd ->
    ((Obj.magic fd):int)
  | None -> (-1)
  
  
let process_await p timeout handler arg =
  let date = Unix.gettimeofday() in
  p.state <- PROC_AWAIT;
  p.handler <- handler;
  p.arg <- arg;
  if timeout > 0 then  p.timeout <- date +. ((float_of_int timeout) *. 1e-6)
  else p.timeout <- nilTMO;
  add q_process_await p

  
let process_wakeup p =
  p.state <- PROC_READY;
  p.handler p p.arg;
  (*
  ** Reset to default handler
  *)
  p.handler <- _handler;
  p.timeout <- nilTMO;
  rem q_process_await p;
  add q_process_ready p


(*
** Return actual process identifier
*) 
let process_self() = process_table.(process_running)

  
let process_dump () = 
  let date = Unix.gettimeofday () in
  conpro_print (sprintf "q_process_ready:%s"
                  (print q_process_ready));
  conpro_print (sprintf "q_process_await:%s"
                  (print q_process_await));
  conpro_print (sprintf "q_process_start:%s"
                  (print q_process_start));
  conpro_print (sprintf "q_process_stop:%s"
                  (print q_process_stop));
  conpro_print (sprintf "timer: (date=%f)" date);
  if !q_timer <> nilT then
  begin
    let mutable qt = !q_timer in
    while qt <> nilT
    do
      let t = get_some qt in
      conpro_print (sprintf 
                "[interval=%d,once=%b,timeout=%f,waiting=%d,on=%b,qawait=(%s)]"
                  t.interval t.once t.timeout t.waiting t.on (
      let mutable s = "" in
      for i = 0 to t.waiting-1 
      do
        let pid = t.qawait.(i) in
        if pid <> nilPID then
        begin
          let p = process_table.(pid) in
          s <- sprintf "%s%d:%s" s p.id p.name;  
        end;
      done; s));
      qt <- t.next;
    done;
  end

let process_io () =
  let mutable date = Unix.gettimeofday () in
  let mutable need_select, need_wait = 0,0 in
  let mutable retcode = 0 in
  let mutable ready = 0 in
  let mutable pid = 0 in
  let mutable again = true in
  
  let mutable readfds = [] in
  let mutable writefds = [] in
  let mutable exceptfds = [] in
  let mutable timeout = nilTMO in
  let mutable delay = nilTMO in
  
  while again = true
  do
    date <- Unix.gettimeofday ();
    if proDBG then conpro_print (sprintf "[process_io] @ %f [q_timer=%b]" date (!q_timer <> nilT));    
    (*
    ** Check for IO and timeout events
    *)
    pid <- 0;
    ready <- 0;

    if not (empty q_process_await) then
    begin
      let mutable qp = !q_process_await in
      while qp <> nilPROC
      do
        let p = get qp in 
        (*
        ** Check timeout event
        *)
        if p.timeout <> nilTMO && date > p.timeout then
        begin
          p.timeout <- nilTMO;
          if proDBG then conpro_print (sprintf "[process_io]: wakeup of timed out process %d" p.id);
          process_wakeup p;
          (*
          ** Start again from beginning of queue
          *)
          qp <- !q_process_await;
          ready <- ready + 1;
        end;
        (*
        ** Check IO event
        *)
        if p.read <> nilFD then
        begin
          if List.mem (get_some p.read) readfds then
          begin
            p.timeout <- nilTMO;
            p.read <- nilFD;
            process_wakeup p;
            ready <- ready + 1; 
          end;
        end;
        if p.write <> nilFD then
        begin
          if List.mem (get_some p.write) writefds then
          begin
            p.timeout <- nilTMO;
            p.write <- nilFD;
            process_wakeup p;
            ready <- ready + 1;              
          end;
        end;
        if p.excep <> nilFD then
        begin
          if List.mem (get_some p.excep) exceptfds then
          begin
            p.timeout <- nilTMO;
            p.excep <- nilFD;
            process_wakeup p;
            ready <- ready + 1;              
          end;
        end;
        qp <- p.next;
      done;
    end;
    
    
    
    
    (*
    ** Check timers
    *)
    if !q_timer <> nilT then
    begin
      let mutable qt = !q_timer in
      while qt <> nilT 
      do
        let (t:timers_t) = get_some qt in
        if t.timeout <> nilTMO && date > t.timeout then
        begin
          if t.once = false then t.timeout <- date +. ((float_of_int t.interval) *. 1e-6)
          else 
          begin
            t.timeout <- nilTMO;
            t.on <- false;
          end;
          for i=0 to t.waiting
          do
            let pid = t.qawait.(i) in
            if pid <> nilPID then
            begin
              (*
              ** Wakeup
              *)
              let p = process_table.(pid) in
              if proDBG then conpro_print (sprintf "[process_io]: timer wakeup of process %d" pid);
              process_wakeup p;
              t.qawait.(i) <- nilPID;
              ready <- ready + 1;
            end;
          done;
          t.waiting <- 0;
        end;
        qt <- t.next;
      done;
    end;

    if proDBG then conpro_print (sprintf "[process_io] ready=%d" ready);
    
    if ready>0 then 
    begin
      again <- false;
      retcode <- ready;
    end 
    else
    begin
      
      
      
      readfds <- [];
      writefds <- [];
      exceptfds <- [];
      need_select <- 0;
      need_wait <- 0;
      timeout <- nilTMO;
      
      (*
      ** Check timers
      *)
      if !q_timer <> nilT then
      begin
        let mutable qt = !q_timer in
        while qt <> nilT
        do
          let t = get qt in
          if t.timeout <> nilTMO && t.timeout > date then
          begin
            need_wait <- need_wait + 1;
            if t.timeout < timeout || timeout=nilTMO then 
            begin
              timeout <- t.timeout;
            end;
          end;
          qt <- t.next;
        done;
      end;
      (*
      ** Check awaiting processes...
      *)
      if !q_process_await <> nilPROC then
      begin
        let mutable qp = !q_process_await in
        while qp <> nilPROC
        do
          let p = get qp in
          (*
          ** Check timeout event
          *)
          if p.timeout <> nilTMO && p.timeout > date then
          begin
            need_wait <- need_wait + 1;
            if p.timeout < timeout || timeout=nilTMO then
            begin
              timeout <- p.timeout;
            end;
          end;
          (*
          ** Check IO event
          *)
          if p.read <> nilFD then
          begin
            readfds <- readfds @ [get_some p.read];
            if proDBG then conpro_print (sprintf "[process_io] readfd=%s" p.name); 
            need_select <- max need_select (fd_id p.read);
          end;
          if p.write <> nilFD then
          begin
            writefds <- writefds @ [get_some p.write];
            if proDBG then conpro_print (sprintf "[process_io] writefd=%s" p.name); 
            need_select <- max need_select (fd_id p.write);
          end;
          if p.excep <> nilFD then
          begin
            exceptfds <- exceptfds @ [get_some p.excep];
            if proDBG then conpro_print (sprintf "[process_io] excepfd=%s" p.name); 
            need_select <- max need_select (fd_id p.excep);
          end;
          qp <- p.next;
        done;
      end;

      if proDBG then conpro_print (sprintf "[process_io] need_select=%d need_wait=%d" need_select need_wait);
      
      if need_select > 0 || need_wait > 0 then
      begin
        if need_wait > 0 then
        begin
          delay <- timeout -. date;
          if proDBG then conpro_print (sprintf "[process_io]: waiting %f s..." delay);
        end
        else
          delay <- -1.0;
        let readfds',writefds',exceptfds' = 
          Unix.select readfds writefds exceptfds delay in
        readfds <- readfds';
        writefds <- writefds';
        exceptfds <- exceptfds';
        retcode <- (List.length readfds) + (List.length writefds) + (List.length exceptfds);
        if proDBG then conpro_print (sprintf "[process_io] select->retcode=%d" retcode);
      end
      else
      begin
        again <- false;
        retcode <- 0;
      end;  
    end;    
  done;
  retcode

let mutable process_init_done = false
  
let process_init () =
  if not process_init_done then
  begin
    for i = 0 to max_PROC-1
    do
      process_table.(i).id <- i;      
      if i = 0 then 
      begin
        process_table.(i).state <- PROC_RUN;
        process_table.(i).name <- "root";
        process_table.(i).context.uc_thread <- Some (Thread.self ());
      end
      else
      begin
        process_table.(i).state <- PROC_FREE; 
      end;
      process_table.(i).handler <- _handler;
      process_table.(i).arg <- 0;
      process_table.(i).pend_excep <- nilEXC;
      process_table.(i).next <- nilPROC;
      process_table.(i).timeout <- nilTMO;
      process_table.(i).read <- nilFD;
      process_table.(i).write <- nilFD;
      process_table.(i).excep <- nilFD;    
      process_table.(i).signal <- 0;    
    done;

    (*
    ** Root/main process
    *)
    process_top <- 1;
    q_process_ready := nilPROC;
    q_process_start := nilPROC;
    q_process_stop := nilPROC;
    q_process_await := nilPROC;
    (* q_timer := nilT; *)
    process_init_done <- true;
  end 
  
(*
** The process scheduler
*)
let process_schedule () =
  let mutable  pid=0 in
  let mutable pid_run = process_running in
  let mutable date = nilTMO in
 
  let mutable again = true in
  
  let swapcontext next =
    if proDBG then conpro_print (sprintf 
                    "[process_schedule]: swapcontext to %d:%s [thr=%b]..."
                                  next.id next.name
                                  (next.context.uc_thread <> None));
    try 
    begin
      Thread.yield ();
      Thread.wakeup (get_some next.context.uc_thread);    
      Thread.sleep ();
    end
    with 
     _ -> 
      process_dump ();
      conpro_print (sprintf "[process_schedule]: swapcontext failed: process thread %s #%d not suspended."
                              next.name next.id);
      Pervasives.exit (-1);
    in
    
  let check_signal p = 
      if process_table.(p).signal < 0 then 
      begin
        if proDBG then conpro_print (sprintf 
                      "[process_schedule]: %d:%s: raising signal %d..."
                            pid_run process_table.(p).name 
                            process_table.(p).signal );
        (*
        ** One final run before stopping...
        *)
        let found = mem q_process_stop process_table.(p) in
        if not found then add q_process_stop process_table.(p);
        let signal = process_table.(p).signal in
        process_table.(p).signal <- 0;             
        raise (Thread_signal signal);
      end in
    
  try while again 
  do
    date <- Unix.gettimeofday();
    if proDBG then
      conpro_print (sprintf "[process_schedule] @%f: %d [run=%b], QREADY=%s, QAWAIT=%s" 
                    date process_running (process_table.(process_running).state = PROC_RUN)
                    (print q_process_ready) (print q_process_await));
    (*
    ** Check for starting processes first...
    *)
    if not (empty q_process_start) then
    begin
      let p = head q_process_start in
      pid <- p.id;
      if proDBG then conpro_print (sprintf 
                        "[process_schedule]: %d:%s: starting process %d:%s..."
                             pid_run process_table.(pid_run).name pid
                             process_table.(pid).name);
      
      process_running <- pid;
      process_table.(pid).state <- PROC_START;

      if process_table.(pid_run).state = PROC_RUN then
      begin
         process_table.(pid_run).state <- PROC_READY;
         add q_process_ready process_table.(pid_run); 
      end;
      
      swapcontext process_table.(pid);
                  
      process_running <- pid_run;
      if process_table.(pid).state = PROC_RUN then
      begin
        process_table.(pid).state <- PROC_READY;
        add q_process_ready process_table.(pid); 
      end;
      if proDBG then conpro_print (sprintf 
                      "[process_schedule]: %d:%s: after starting process %d:%s..."
                            pid_run process_table.(pid_run).name pid
                            process_table.(pid).name);
      raise Exit;
    end;
    (*
    ** Check for ready processes, schedule oldest first (FIFO order)...
    *)
    pid <- 0;
    if not (empty q_process_ready) then
    begin
      let p = head q_process_ready in
      pid <- p.id;
      process_running <- pid;
      process_table.(pid).state <- PROC_RUN;
      if pid <> pid_run then
      begin
        if process_table.(pid_run).state = PROC_RUN then
        begin
           process_table.(pid_run).state <- PROC_READY;
           add q_process_ready process_table.(pid_run); 
        end;
        if proDBG then conpro_print (sprintf 
                      "[process_schedule]: %d:%s: resuming process %d:%s..."
                          pid_run process_table.(pid_run).name pid
                          process_table.(pid).name);
        swapcontext process_table.(pid);
        process_running <- pid_run;
        if process_table.(pid).state = PROC_RUN then
        begin
          process_table.(pid).state <- PROC_READY;
          add q_process_ready process_table.(pid); 
        end;
        check_signal pid_run;
      end;
      if proDBG then conpro_print (sprintf 
                        "[process_schedule]: %d:%s: after scheduling process %d:%s..."
                            pid_run process_table.(pid_run).name pid
                            process_table.(pid).name);
      
      raise Exit;
    end;
    if process_table.(process_running).state = PROC_RUN then raise Exit
    else 
    begin
      let ready = process_io () in
      if ready = 0 then
      begin
        process_dump();
        conpro_err "[process_schedule]: no processes ready";
      end;
    end;
  done with Exit -> ()
  
  

(*
** Process control
*)
let process_start p =
  p.state <- PROC_START;
  p.timeout <- nilTMO;
  let found = mem q_process_stop p in
  if found then  rem q_process_stop p;
  add q_process_start p;
  process_schedule ()
 
let process_stop p =
  let p' = process_self () in
  if proDBG then conpro_print (Printf.sprintf "process_stop %s #%d: stopping %s #%d " p'.name p'.id p.name p.id);
  p.state <- PROC_STOP;
  (*
  ** If blocked, cleanup blocked resource
  *)
  p.handler p p.arg;

  
  (*
  ** Process is either ready or awaiting.
  *)
  let found = mem q_process_ready p in
  if found && p=p' then rem q_process_await p
  else if found then
  begin
    match p.context.uc_thread with
    | Some t ->
      if proDBG then conpro_print (Printf.sprintf "process_stop #%d: sending signal to %s #%d, thread #%d" p'.id p.name p.id (Thread.id t));
      (*
      ** Process must be kept on ready queue and is
      ** finally removed from the ready queue in the scheduler
      *)
      p.signal <- (-100);
    | None -> rem q_process_ready p
  end;
  let found = mem q_process_await p in
  if found && p=p' then rem q_process_await p
  else if found then
  begin
    match p.context.uc_thread with
    | Some t ->
      if proDBG then conpro_print (Printf.sprintf "process_stop #%d: sending signal to %s #%d, thread #%d" p'.id p.name p.id (Thread.id t));
      (*
      ** Process must be moved to ready queue and is
      ** finally removed from the ready queue in the scheduler
      *)
      rem q_process_await p;
      add q_process_ready p;
      p.signal <- (-100);
    | None -> rem q_process_await p
  end;
  
  let found = mem q_process_stop p in
  if found then rem q_process_stop p;
  add q_process_stop p;

  if p.join <> nilPID then
  begin
    (*
    ** There is a calling process waiting
    ** for this process termination
    *)
    let id = p.join in
    let pj = process_table.(id) in
    let found = mem q_process_await pj in
    if found then
    begin
      rem q_process_await pj;
      add q_process_ready pj;
    end;
    p.join <- nilPID;
  end;
  
  
  process_schedule()

(*     
** Return actual process identifier
*)
let process_call p =
  let pr = process_self () in

  p.state <- PROC_START;
  p.timeout <- nilTMO;
  p.join <- pr.id;
  let found = mem q_process_stop p in
  if found then rem q_process_stop p;
  add q_process_start p;
  
  pr.state <- PROC_AWAIT;
  pr.handler <- _handler;
  pr.timeout <- nilTMO;
  add q_process_await pr;
  
  process_schedule()

let process_end () =
  let p = process_self () in
  if proDBG then conpro_print (Printf.sprintf "process_end: %s #%d" p.name p.id);
  p.state <- PROC_STOP;
  (*
  ** If blocked, cleanup blocked resource
  *)
  p.handler p p.arg;
  
  (*
  ** Process is either ready or awaiting.
  *)
  let found = mem q_process_ready p in
  if found then rem q_process_ready p;
  let found = mem q_process_await p in
  if found then rem q_process_await p;
  let found = mem q_process_stop p in
  if found then rem q_process_stop p;
  add q_process_stop p;

  if p.join <> nilPID then
  begin
    (*
    ** There is a calling process waiting
    ** for this process termination
    *)
    let id = p.join in
    let pj = process_table.(id) in
    let found = mem q_process_await pj in
    if found then
    begin
        rem q_process_await pj;
        add q_process_ready pj;
    end;
    p.join <- nilPID;
  end;
  process_schedule()
  

(*
** Delay process (timeout: micro seconds)
*)  
let process_delay tmo =
  let p = process_self () in
  let mutable date = Unix.gettimeofday () in
  p.state <- PROC_AWAIT;
  p.handler <- _handler;
  if tmo > 0 then  p.timeout <- date +. ((float_of_int tmo) *. 1e-6)
  else p.timeout <- nilTMO;
  add q_process_await p;
  process_schedule()

(*
** Wait for ready IO...
*)
let process_await_io p timeout kind fd =
  let date = Unix.gettimeofday() in
  p.state <- PROC_AWAIT;
  p.handler <- _handler;
  if timeout > 0 then  p.timeout <- date +. ((float_of_int timeout) *. 1e-6)
  else p.timeout <- nilTMO;
  (
    match kind with
    | 'r' -> p.read <- Some fd;
    | 'w' -> p.write <- Some fd;
    | 'x' -> p.excep <- Some fd;
    | _ -> conpro_err("process_await_io");
  );
  add q_process_await p;
  process_schedule ()

(*
** Create a new process
*)
let process_new name f arg =
  process_init ();
  let mutable i = 0 in
  if proDBG then conpro_print (sprintf "process_new %s" name);
  while (i < max_PROC && process_table.(i).state <> PROC_FREE)
  do
    i <- i +1;
  done;
  if i=max_PROC then conpro_err("Process table full");
  process_table.(i).state <- PROC_STOP;
  process_table.(i).pend_excep <- nilEXC;
  process_table.(i).next <- nilPROC;
  process_table.(i).timeout <- nilTMO;
  process_table.(i).read <- nilFD;
  process_table.(i).write <- nilFD;
  process_table.(i).excep <- nilFD;
  process_table.(i).signal <- 0;
  (*
  ** Set to default resource handler (dummy)
  *)
  process_table.(i).handler <- _handler;
  

  process_table.(i).name <- name;
  
  (*
  ** Start process control thread.
  *)
  process_table.(i).func <- f;
  process_table.(i).fun_arg <- arg;
  let t = Thread.create (fun pid ->
        let p = process_table.(pid) in
        if proDBG then conpro_print (sprintf 
                       "process_new %d:%s started." i name);
        while true 
        do
          try
          begin
            match p.state with
            | PROC_STOP -> 
              if proDBG then conpro_print (sprintf 
                              "process %d:%s sleeping..." i name);
              Thread.sleep (); 
            | PROC_START | PROC_RUN -> 
              if proDBG then conpro_print (sprintf 
                              "process %d:%s running..." i name);
              p.state <- PROC_RUN;
              p.func p.fun_arg;
              if proDBG then conpro_print (sprintf 
                              "process %d:%s finished..." i name);
              if p.state = PROC_RUN then process_end (); 
            | _ -> Thread.exit ();
          end
          with Thread_signal n -> 
            if proDBG then conpro_print (sprintf 
                              "process %d:%s got signal %d..." i name n);
            process_end ();
            | _ -> 
              conpro_print (sprintf "[process]: stopping process %s #%d on uncaught exception."
                                    p.name p.id);
              process_end ();
        done;
        ) i in
  process_table.(i).context.uc_thread <- Some t;
  Thread.yield ();
  process_top <- i+1;
  if proDBG then conpro_print (sprintf "process_new %d:%s [thr=%b] created." i name
      (process_table.(i).context.uc_thread <> None));
  process_table.(i)
   
  
class process env pro_fun =
  object (self)
  val version = "1.5"
  (*
  ** Name and id# of process
  *)
  val mutable pro_name = ""
  val mutable pro_id = 0
  
  val mutable pro = nilPROC
  
  initializer 
    self#config env;
    pro <- Some (process_new pro_name pro_fun pro_id)
    
    
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | "name" -> pro_name <-  v;
      | "#" -> pro_id <- int_of_string v;
      | _ -> () ) env
              
  method start () =
    process_start (get pro)
    
  method stop () =
    process_stop (get pro)

  method call () =
    process_call (get pro)
    
end

