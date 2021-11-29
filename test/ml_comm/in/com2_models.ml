open Com2_types
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
class uart env  =
  object (self)
  val version = "1.5"
  val mutable datawidth = 8
  val mutable stop_bits = 1
  val mutable parity = 0
  val mutable baud = 9600
  val mutable dev = "/tmp/link"
  val mutable dev_kind = S_SOCK
  val mutable fd = None
  val mutable fd_tio = None
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
  
  method get_fd =
    match fd with
    | Some fd -> fd
    | None ->
      match dev_kind with
      | S_SOCK ->
      begin
        let cc = Unix.socket PF_UNIX SOCK_STREAM 0 in
        try
        begin
          bind cc (ADDR_UNIX dev);
          listen cc 1;
          (*
          ** Wait for other side to connect
          *)
          print "Waiting for connection to link %s..." dev;
          let fd_,sa = accept cc in
          fd <- Some fd_;
          fd_
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
              cc
            end
            with _ -> err (Printf.sprintf "Can't connect to link device %s." dev)
          end
          | _ -> err (Printf.sprintf "Can't bind link device %s." dev)
      end
      | _ ->
        let fd_ = 
          try 
            Unix.openfile dev [Unix.O_RDWR;Unix.O_NONBLOCK] 0o666;
          with _ -> err (Printf.sprintf "Can't open link device %s." dev) in
        fd <- Some fd_;
        fd_
        
      
  method get_tio =
    match fd_tio with
    | Some tio -> tio;
    | None ->
      let fd = self#get_fd  in
      let tio = Unix.tcgetattr fd in
      tio.c_icanon <- false;
      tio.c_echo   <- false;
      tio.c_echoe  <- false;
      tio.c_echok  <- false;
      tio.c_echonl <- false;
      tio.c_noflsh <- false; 
      tio.c_vmin <- 1; 
      tio.c_ignbrk <- true;
      tio.c_brkint <- false;
      tio.c_igncr <- true;
      tio.c_ixon <- false;
      tio.c_ixoff <- false;
      tio.c_opost <- false;
      tio.c_clocal <- true;
      tio.c_isig <- false;
      tio.c_istrip <- false;
      tio.c_hupcl <- false;   
      tio.c_vintr <- '\000';
      tio.c_vquit <- '\000';
      tio.c_verase <- '\000';
      tio.c_vkill <- '\000';
      tio.c_veof <- '\000';
      tio.c_veol <- '\000';
      fd_tio <- Some tio;
      tio
      
  method set_tio =
    if dev_kind = S_CHR then
      match fd_tio with
      | Some tio -> 
        let fd = self#get_fd in
        Unix.tcsetattr fd TCSANOW tio;
      | None -> 
        err (Printf.sprintf "Can't set termnial IO descriptor (empty) for device %s." dev)
    
  method update_tio =
    let tio = self#get_tio  in
    tio.c_ibaud <- baud;
    tio.c_obaud <- baud;
    tio.c_csize <- datawidth;
    tio.c_cstopb <- stop_bits;
    tio.c_parenb <- parity > 0;
    tio.c_parodd <- parity = 1;
    tio.c_vmin <- 1;
    (* tio.c_vtime <- ? *)
    self#set_tio
      
  method rx_thr () = 
    let fd = self#get_fd in
    try
    begin 
      let buf = "  " in
      let input_binary_int ic =
        match dev_kind with
        | S_SOCK ->
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
          !d 
        | _  ->
          let d = ref 0 in
          for i = 1 to datawidth/8 
          do
            let n = 
              Thread.wait_read ic;
              Unix.read ic buf 0 1 in
            if n <= 0 then
              err (Printf.sprintf "Read failed on link %s." dev);
            d := bitw ((i-1)*8) (i*8-1) !d (int_of_char buf.[0])
          done;
          !d in
      while true
      do
        let d = input_binary_int fd in
        Mutex.mu_lock in_lock;
        in_q <- in_q @ [d];
        Event.wakeup in_ev;
        Mutex.mu_unlock in_lock;
      done
    end
    with Thread.Thread_signal _ -> 
    begin
      match dev_kind with
      | S_SOCK -> Unix.shutdown (self#get_fd) SHUTDOWN_RECEIVE
      | _ -> Unix.close (self#get_fd)
    end
    
  method tx_thr () =
    let fd = self#get_fd in
    try
    begin 
      Mutex.mu_lock out_lock;
      let buf = String.create 10 in
      let output_binary_int oc d =
        match dev_kind with
        | S_SOCK ->
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
          done 
        | _ ->
          let d = ref d in
          for i = 1 to datawidth/8 
          do
            buf.[0] <- char_of_int (!d land 0xff);
            let n = 
              Thread.wait_write oc;
              Unix.write oc buf 0 1 in
            if n <= 0 then
              err (Printf.sprintf "Write failed on link %s." dev);
            d := !d lsr 8;
          done in
          
      Mutex.mu_unlock out_lock;
      while true
      do
         if out_q = [] then Event.await out_ev;
         Mutex.mu_lock out_lock;
         let d = List.hd out_q in
         out_q <- List.tl out_q;
         Mutex.mu_unlock out_lock;
         output_binary_int fd d;
      done
    end
    with  _ -> 
    begin
      match dev_kind with
      | S_SOCK -> Unix.shutdown (self#get_fd) SHUTDOWN_SEND
      | _ -> Unix.close (self#get_fd)
    end
     
  initializer 
    List.iter (fun (e,v) ->
      match e with
      | "datawidth" -> datawidth <- int_of_string v;
      | "dev" -> dev <- v;
      | "baud" -> baud <- int_of_string v;
      | _ -> () ) env;
      
  method init = 
    protect(let st = Unix.stat dev in dev_kind <- st.st_kind);
    __(self#get_fd);
    if dev_kind = S_CHR then
      __(self#update_tio);
    
  method start = 
    out_thr <- Some (Thread.create self#tx_thr ());
    in_thr <- Some (Thread.create self#rx_thr ());
    
  method stop = 
    match in_thr,out_thr with
    | Some in_thr,Some out_thr -> 
    begin
      Thread.signal in_thr 1;
      Thread.signal out_thr 1;
      Thread.yield ();
      match fd with
      | Some fd -> Unix.close fd;
      | None -> ()
    end;
    | _ -> ()
  
  method baud n =
    baud <- n;
    if fd <> None then self#update_tio
      
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
