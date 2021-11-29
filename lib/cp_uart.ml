
(*
** ConPro ML implementation library
** UART
*)
open Conpro
open Process
open Printf

open Unix
let linkDBG = false
external unsafe_recv :
  file_descr -> string -> int -> int -> msg_flag list -> int
  = "unix_recv"
external unsafe_send :
  file_descr -> string -> int -> int -> msg_flag list -> int
  = "unix_send"
external unsafe_read : file_descr -> string -> int -> int -> int = "unix_read"
external unsafe_write : file_descr -> string -> int -> int -> int = "unix_write"

type uart_t = {
  mutable datawidth: int;
  mutable stop_bits: int;
  mutable parity: int;
  mutable baud: int;
  mutable dev : string;
  mutable dev_kind : Unix.file_kind;
  mutable fd : Unix.file_descr option;
  mutable tio : Unix.terminal_io option;
  mutable in_stat: bool;
  mutable out_stat: bool;
  mutable qawait: int array;
  mutable waiting: int;  
  mutable connecting: bool;
  mutable pro_aux: process option;
}
let mutable uarts = []
let uart_handler p id =
  let s = List.nth uarts id in
  try for i = 0 to max_PROC-1 
  do
    if s.qawait.(i) = p.id then
    begin
      s.qawait.(i) <- nilPID;
      raise Exit;
    end;  
  done with Exit -> ()

class uart env  =
  object (self)
  val version = "1.8"
  val u = {
        datawidth=8;
        stop_bits = 1;
        parity = 0;
        baud = 9600;
        dev = "";
        dev_kind = S_SOCK;
        fd = None;
        tio = None;
        in_stat = true;
        out_stat = true;
        qawait=Array.create max_PROC nilPID;
        waiting=0; 
        connecting=false;
        pro_aux=None;
      }
    
  val mutable id = 0
  
  initializer 
    self#config env;
    id <- List.length uarts;
    uarts <- uarts @ [u]
  
  method private await  = 
    let p = process_self () in
    process_await p 0 uart_handler id;
    u.qawait.(u.waiting) <- p.id;
    u.waiting <- u.waiting + 1;
    process_schedule ();
    
  method private wakeup  = 
    for i = 0 to u.waiting-1
    do
      let pid = u.qawait.(i) in
      if pid <> nilPID then process_wakeup process_table.(pid);
    done;
    u.waiting <- 0;
    process_schedule ()

  method private get_fd =
    match u.fd with
    | Some fd -> fd
    | None -> 
      self#await; 
      u.in_stat <- true; u.out_stat <- true;
      self#get_fd 
            
  method private connect process_id =
      u.connecting<- true;
      if u.dev = "" then self#await;
      match u.dev_kind with
      | S_SOCK ->
      begin
        let cc = Unix.socket PF_UNIX SOCK_STREAM 0 in
        try
        begin
          bind cc (ADDR_UNIX u.dev);
          listen cc 1;
          (*
          ** Wait for other side to connect
          *)
          conpro_print (Printf.sprintf "[LINK INFO]: Waiting for connection to link %s..." u.dev);
          process_await_io (process_self ()) 0 'r' cc;
          let fd_,sa = accept cc in
          conpro_print (Printf.sprintf "[LINK INFO]: Other side connected to link %s..." u.dev);
          Unix.close cc;
          u.fd <- Some fd_;
          u.connecting <- false;
          self#wakeup;
          process_end ()
        end
        with
          Unix.Unix_error (EADDRINUSE, "bind", "") -> 
          begin
            (*
            ** Other side has already created and bound the socket
            *)
            try
            begin
              connect cc (ADDR_UNIX u.dev);
              conpro_print (Printf.sprintf "[LINK INFO]: Connected to link %s..." u.dev);
              u.fd <- Some cc;
              u.connecting <- false;
              self#wakeup;
              process_end ()
            end
            with _ -> 
            begin
              (*
              ** Orphan? Last try: delete file and bind again
              *)
              unlink(u.dev);
              try
              begin
                bind cc (ADDR_UNIX u.dev);
                listen cc 1;
                (*
                ** Wait for other side to connect
                *)
                conpro_print (Printf.sprintf "[LINK INFO]: Again waiting for connection to link %s..." u.dev);
                process_await_io (process_self ()) 0 'r' cc;
                let fd_,sa = accept cc in
                Unix.close cc;
                conpro_print (Printf.sprintf "[LINK INFO]: Other side connected to link %s..." u.dev);
                u.fd <- Some fd_;
                u.connecting <- false;
                self#wakeup;
                process_end ();
              end
              with
                _ -> 
                  conpro_err (Printf.sprintf "[LINK ERROR]: Can't connect to link device %s." u.dev)
            end;
          end
          | _ -> conpro_err (Printf.sprintf "[LINK ERROR]: Can't bind link device %s." u.dev)
      end
      | _ ->
        let fd_ = 
          try 
            Unix.openfile u.dev [Unix.O_RDWR;Unix.O_NONBLOCK] 0o666;
          with _ -> conpro_err (Printf.sprintf "[LINK ERROR]: Can't open link device %s." u.dev) in
        conpro_print (Printf.sprintf "[LINK INFO]: Connected to link %s..." u.dev);
        u.fd <- Some fd_;
        u.connecting <- false;
        __(self#update_tio);
        self#wakeup;
        process_end ()
        
      
  method private get_tio =
    match u.tio with
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
      u.tio <- Some tio;
      tio
      
  method private set_tio =
    if u.dev_kind = S_CHR then
      match u.tio with
      | Some tio -> 
        let fd = self#get_fd in
        Unix.tcsetattr fd TCSANOW tio;
      | None -> 
        conpro_err (Printf.sprintf "[LINK ERROR]: Can't set termnial IO descriptor (empty) for device %s." u.dev)
    
  method private update_tio =
    let tio = self#get_tio  in
    tio.c_ibaud <- u.baud;
    tio.c_obaud <- u.baud;
    tio.c_csize <- u.datawidth;
    tio.c_cstopb <- u.stop_bits;
    tio.c_parenb <- u.parity > 0;
    tio.c_parodd <- u.parity = 1;
    tio.c_vmin <- 1;
    (* tio.c_vtime <- ? *)
    self#set_tio
 
     
  initializer 
    self#config env
    
  method config env = 
    List.iter (fun (e,v) ->
      match e with
      | "datawidth" -> u.datawidth <- int_of_string v;
      | "dev" -> u.dev <- v;
      | "baud" -> u.baud <- int_of_string v;
      | _ -> () ) env
      
  method init () = 
    conpro_print (Printf.sprintf "[LINK INFO] %s: initializing." u.dev);
    protect(let st = Unix.stat u.dev in u.dev_kind <- st.st_kind);
    match u.pro_aux with
    | Some pro ->
      pro#start ();
    | None -> 
      let connect_pro = new process ["name",Printf.sprintf "link_connect:%s" u.dev] self#connect in
      u.pro_aux <- Some connect_pro;
      connect_pro#start()

    
  method start () = 
    conpro_print (Printf.sprintf "[LINK INFO] %s: starting." u.dev);
    match u.fd with
    | Some fd -> 
      if u.dev_kind = S_CHR then
        __(self#update_tio);
      u.in_stat <- true; u.out_stat <- true
    | None -> ()
      
    
  method stop () = 
    conpro_print (Printf.sprintf "[LINK INFO] %s: stopping." u.dev);
    match u.fd with
    | Some fd -> 
    begin
      match u.dev_kind with
      | S_SOCK -> 
        Unix.close fd; 
        Unix.unlink u.dev;
      | _ -> Unix.close fd      
    end;
    u.fd <- None;
    u.in_stat <- false; u.out_stat <- false
    | None -> ()

  method baud n =
    u.baud <- n;
    if u.fd <> None then self#update_tio
      
  method interface (rx:int) (tx:int) =
    ()
    
  method private read_byte fd buf ifs len =
    try
    begin
      match u.dev_kind with
      | S_SOCK -> unsafe_recv fd buf ifs len []
      | _ -> unsafe_read fd buf ifs len
    end
    with 
    | Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    begin
      process_await_io (process_self ()) 0 'r' fd;
      self#read_byte fd buf ifs len
    end
    | _ ->
    begin
      u.in_stat <- false;
      (-1)
    end;

  method private write_byte fd buf ofs len =
    try
    begin
      match u.dev_kind with
      | S_SOCK -> unsafe_send fd buf ofs len []
      | _ -> unsafe_write fd buf ofs len
    end
    with 
    | Unix_error((EAGAIN | EWOULDBLOCK), _, _) ->
    begin
      process_await_io (process_self ()) 0 'w' fd;
      self#write_byte fd buf ofs len
    end
    | _ ->
    begin
      u.out_stat <- false;
      (-1)
    end;
      
  method read () = 
    let buf = " " in
    let mutable d = 0 in
    if linkDBG then conpro_print "read>>>";
    let fd = self#get_fd in
    try 
    begin
      for i = 1 to u.datawidth/8 
      do
        let n = self#read_byte fd buf 0 1 in
        if linkDBG then conpro_print (sprintf "  read = %d: %x" n (int_of_char buf.[0])); 
        if n <= 0 then
        begin
          conpro_err (Printf.sprintf "[LINK INFO]: Read failed on link %s." u.dev);
        end
        else
          d <- bitw ((i-1)*8) (i*8-1) d (int_of_char buf.[0]);
      done;
      d,not u.in_stat
    end with Exit -> 
      let p = process_self () in
      p.read <- None;
      p.write <- None;
      u.in_stat <- false;
      self#stop ();
      self#init ();
      0,true
      
          
  method write d =
    if u.fd <> None then
    begin
      let buf = " " in
      if linkDBG then conpro_print "write>>>";
      let fd = self#get_fd in
      let mutable d = d in
      try
      begin
        for i = 1 to u.datawidth/8 
        do
          buf.[0] <- char_of_int (d land 0xff);
          let n = self#write_byte fd buf 0 1 in
          if linkDBG then conpro_print (sprintf "  write = %d : %x" n (int_of_char buf.[0]));
          if n <= 0 then
          begin
            conpro_err (Printf.sprintf "[LINK INFO]: Write failed on link %s." u.dev);
            raise Exit
          end
          else d <- d lsr 8;
        done;
        not u.out_stat
      end with Exit ->
        let p = process_self () in
        p.read <- None;
        p.write <- None;
        u.out_stat <- false;
        self#stop ();
        self#init ();
        true        
    end
    else false

end
