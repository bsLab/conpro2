open Com_types
open Unix
let err msg = print_string msg; print_newline (); exit 1
let print fmt = 
  let print chan fmt =
      let fmt = (Obj.magic fmt : string) in
      let len = String.length fmt in
      let fl () = Pervasives.flush chan in
      let rec doprn i =
          if i >= len then fl ();
          if i >= len then 
          begin
            output_char chan '\n';
            Pervasives.flush chan;            
            Obj.magic ();
          end else
          match String.unsafe_get fmt i with
          | '%'  -> Printf.scan_format fmt i cont_s cont_a cont_t
          | '\n' -> output_char chan '\n';
                    Pervasives.flush chan;
                    doprn (succ i);
          |  c   -> output_char chan c; 
                    doprn (succ i)
        and cont_s s i =
          output_string chan s; doprn i
        and cont_a printer arg i =
          printer chan arg; doprn i
        and cont_t printer i =
          printer chan; doprn i
        in 
        doprn 0 in
  print Pervasives.stdout fmt
      
  
let bit a b o =
  let m = String.make (b+1) '0' in
  for i = a to b 
  do
    m.[b-i] <- '1';
  done;
  (o land (int_of_string ("0b"^m))) lsr a
  

let bit64 a b o =
  let m = String.make (b+1) '0' in
  for i = a to b 
  do
    m.[b-i] <- '1';
  done;
  Int64.shift_right (Int64.logand o (Int64.of_string ("0b"^m))) a
  

let bitw a b o e =
  let m = String.make (b+1) '0' in
  let m' = String.make (b-a+1) '1' in
  for i = a to b 
  do
    m.[b-i] <- '1';
  done;
  let m = int_of_string ("0b"^m) in
  let m' = int_of_string ("0b"^m') in
  (o land (lnot m)) lor ((e land m') lsl a)
  
let bitw64 a b o e =
  let m = String.make (b+1) '0' in
  let m' = String.make (b-a+1) '1' in
  for i = a to b 
  do
    m.[b-i] <- '1';
  done;
  let m = Int64.of_string ("0b"^m) in
  let m' = Int64.of_string ("0b"^m') in
  Int64.logor (Int64.logand o (Int64.lognot m)) (Int64.shift_left (Int64.logand e m') a)
    
let delay millisec = Thread.mdelay millisec
class random env  =
  object (self)
  val mutable seed_v = 0
  val mutable datawidth = 8
  val mutable datatype = "int"
  
  initializer 
    List.iter (fun (e,v) ->
      match e with
      | "seed" -> seed_v <- int_of_string v;
      | "datawidth" -> datawidth <- int_of_string v;
      | "datatype" -> datatype <- v;
      | _ -> () ) env
      
  method init = 
    Random.init seed_v
  method seed v = 
    seed_v <- v;
    Random.init seed_v
  method read =
    match datawidth with
    | 8 -> Random.int 255;    
    | 10 -> Random.int 1023; 
    | 12 -> Random.int 4095;     
    | 14 -> Random.int 16383;
    | 16 -> Random.int 65535;
    | _ -> 0
end
(*
** Establish a bidirectional link
** between two processes using a socket, negotiated by
** those two processe.
** 
**
** Connect Sequence:
**
**    1. Try to bind an unbound socket, listen on this socket and wait
**       for second process connect.
**    2. If 1. failed (EOpen socket for reading and writing
**
*)
class link env  =
  object (self)
  val version = "1.3"
  val mutable datawidth = 8
  val mutable dev = "/tmp/link"
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
  
  method cc_thr () =
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
        end
        with _ -> err (Printf.sprintf "Can't connect to link device %s." dev)
      end
      | _ -> err (Printf.sprintf "Can't bind link device %s." dev)
     
    
  method rx_thr () = 
    try
    begin 
      let buf = "  " in
      let ic = 
        match fd with
        | Some fd -> fd
        | None -> err ("No socket filedescriptor found.") in
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
    
  method tx_thr () =
    try
    begin 
      Mutex.mu_lock out_lock;
      let oc = 
        match fd with
        | Some fd -> fd
        | None -> err ("No socket filedescriptor found.") in
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
    List.iter (fun (e,v) ->
      match e with
      | "datawidth" -> datawidth <- int_of_string v;
      | "dev" -> dev <- v;
      | _ -> () ) env;
      
  method init = 
    self#cc_thr ()
    
  method start = 
    out_thr <- Some (Thread.create self#tx_thr ());
    in_thr <- Some (Thread.create self#rx_thr ());
    
  method stop = 
    match in_thr,out_thr with
    | Some in_thr,Some out_thr -> 
      Thread.signal in_thr 1;
      Thread.signal out_thr 1;
    | _ -> ()
    
  method interface =
    ()
    
  method read = 
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
