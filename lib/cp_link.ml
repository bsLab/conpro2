(*
** ConPro ML implementation library
** LINK
*)

(*
** Establish a bidirectional link
** between two processes using a socket, negotiated by
** those two processe.
** 
**
** Connect Sequence:
**
**    1. Try to bind an unbound socket, listen to this socket and wait
**       for second process connect.
**    2. If 1. failed (EOpen socket for reading and writing
**
*)

class link env  =
  object (self)
  val version = "1.5"
  val mutable datawidth = 8
  val mutable dev = "/tmp/link"
  val dev_init = Event.create ()
  val mutable fd = None 
  val mutable in_q = [] 
  val mutable out_q = []
  val mutable in_thr = None
  val mutable out_thr = None
  val in_lock = Mutex.create ()
  val out_lock = Mutex.create ()
  val in_ev = Event.create ()
  val out_ev = Event.create ()
  val mutable in_stat =  false
  val mutable out_stat = false
  
  method selector = datawidth < 31
  
  method private cc_thr () =
    let cc = Unix.socket PF_UNIX SOCK_STREAM 0 in
    try
    begin
      bind cc (ADDR_UNIX dev);
      listen cc 1;
      (*
      ** Wait for other side to connect
      *)
      print "Waiting for connection to link %s..." dev;
      let fd',sa' = accept cc in
      fd <- Some fd';
      Event.wakeup dev_init;
    end
    with
      Unix.Unix_error (EADDRINUSE, "bind", "") -> 
      begin
        (*
        ** Other side has already created the bound socket
        *)
        try
        begin
          connect cc (ADDR_UNIX dev);
          fd <- Some cc;
          Event.wakeup dev_init;
        end
        with _ -> err (Printf.sprintf "Can't connect to link device %s." dev)
      end
      | _ -> err (Printf.sprintf "Can't bind link device %s." dev)
     
    
  method private rx_thr () = 
    try
    begin 
      let buf = "  " in
      let rec ic () = 
        match fd with
        | Some fd -> fd
        | None -> Event.await dev_init; ic () in
      let ic = ic () in
      let input_binary_int ic =
        let d = ref 0 in
        for i = 1 to datawidth/8 
        do
          let n = 
            Thread.wait_read ic; 
            Unix.recv ic buf 0 1 [] in
          if n <= 0 then
            err (Printf.sprintf "Read failed on link %s." dev);
          d := bitw ((i-1)*8) (i*8-1) !d (int_of_char buf.[0])
        done;
        !d in
      
      print "Input link %s connected." dev;
      while true
      do
        let d = input_binary_int ic in
        Mutex.mu_lock in_lock;
        in_q <- in_q @ [d];
        Event.wakeup in_ev;
        Mutex.mu_unlock in_lock;
      done
    end
    with Thread.Thread_signal _ -> 
    begin
        match fd with
        | Some fd' -> fd <- None; Unix.shutdown fd' SHUTDOWN_RECEIVE; 
        | None -> ()
    end
    
  method private tx_thr () =
    try
    begin 
      Mutex.mu_lock out_lock;
      let rec oc () = 
        match fd with
        | Some fd -> fd
        | None -> Event.await dev_init; oc () in
      let oc = oc () in
      let buf = String.create 10 in
      let output_binary_int oc d =
        let d = ref d in
        for i = 1 to datawidth/8 
        do
          buf.[0] <- char_of_int (!d land 0xff);
          let n = 
            Thread.wait_write oc; 
            Unix.send oc buf 0 1 [] in
          if n <= 0 then
            err (Printf.sprintf "Write failed on link %s." dev);
          d := !d lsr 8;
        done in

      print "Output link %s connected." dev;
      Mutex.mu_unlock out_lock;
      while true
      do
         if out_q = [] then Event.await out_ev;
         Mutex.mu_lock out_lock;
         let d = List.hd out_q in
         out_q <- List.tl out_q;
         Mutex.mu_unlock out_lock;
         output_binary_int oc d;
      done
    end
    with Thread.Thread_signal _ -> 
    begin
        match fd with
        | Some fd' -> fd <- None; Unix.shutdown fd' SHUTDOWN_SEND; 
        | None -> ()
    end
     
  initializer 
    self#config env
      
      
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | "datawidth" -> datawidth <- int_of_string v;
      | "dev" -> dev <- v;
      | _ -> () ) env;
    
  method init () = 
    __(Thread.create self#cc_thr ())
    
  method start () = 
    out_thr <- Some (Thread.create self#tx_thr ());
    in_thr <- Some (Thread.create self#rx_thr ());
    
  method stop () = 
    match in_thr,out_thr with
    | Some in_thr,Some out_thr -> 
      Thread.signal in_thr 1;
      Thread.signal out_thr 1;
    | _ -> ()
    
  method interface (data_in:int) (data_in_ack:int) 
                   (data_out:int) (data_out_ack:int) =
    ()
    
  method read () = 
    if in_q = [] then Event.await in_ev;
    Mutex.mu_lock in_lock;
    let d = List.hd in_q in
    in_q <- List.tl in_q;
    Mutex.mu_unlock in_lock;
    d,in_stat
          
  method write d =
    Mutex.mu_lock out_lock;
    out_q <- out_q @ [d];
    Event.wakeup out_ev;
    Mutex.mu_unlock out_lock;    
    out_stat
    
end
