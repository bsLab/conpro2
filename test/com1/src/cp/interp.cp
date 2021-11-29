queue tty_stdout: char with depth=16;
queue tty_stdin: char with depth=16;
array line: reg[32] of char;

const NL: value := 10;


object tty_link: uart;
  tty_link.interface(DEV.link_tty_rx,DEV.link_tty_tx);
  tty_link.baud(38400);
  
function tty_print():
begin
  reg i: int[5];
  
  i <- 0;
  while line.[i] <> '.' do
  begin
    diag[0] <- 1;
    tty_stdout <- line.[i];
    i <- i + 1;
  end;
  tty_stdout <- NL;
end;

function str_to_int() return (y:int[16]):
begin
  reg i,c : int[8];
  reg sign: bool;
  i <- 0;
  y <- 0;
  sign <- false;
  if line.[i] = '-' then
  begin
    sign <- true;
    i <- i + 1;
  end;
  while line.[i] <> '.' do
  begin
    y <- y * 10;
    c <- to_int(line.[i]) - '0';
    y <- y + c;
    i <- i + 1;
  end;
  if sign = true then
    y <- -y;
end;

function str_of_int(x:int[16]):
begin
  reg i : int[8];
  reg r,z,q: logic[div_n];
  reg suppress: bool;
  
  i <- 0;
 
  if x < 0 then
  begin
    line.[i] <- '-';
    i <- i + 1;
    x <- -x;
  end;
  z <- to_logic(x);
  
  q <- 10000;
  suppress <- true;
  
  while q <> 0 do
  begin
    r <- div(z,q);
      
    if (suppress = true and r <> 0) or
       (suppress = false) then
    begin
      line.[i] <- to_char(r) + '0';
      suppress <- false;
      i <- i + 1;
    end;
    z <- z - (q * r);
    q <- div(q,10);
  end;
  line.[i] <- '.';
end;


process tty_tx:
begin
  reg d:logic[8];
  reg err:bool;
  
  err <- false;
  always do
  begin
    d <- to_logic(tty_stdout);
    diag[2] <- 1;
    tty_link.write(d,err);
    if err = false then err <- true;
    diag[1] <- 1;
  end;
end;

reg tty_stdin_tmo: logic;

process tty_rx_tmo:
begin
  tty_stdin_tmo <- 0;
  wait for 10 millisec;
  tty_stdin_tmo <- 1;
  tty_stdin.unlock ();
end;

process tty_rx:
begin
  reg d:logic[8];
  reg err:bool;
  
  
  always do
  begin
    tty_link.read(d,err);
    if err = false then
      tty_stdin <- to_char(d);
    -- if err = false then diag[1] <- 1 else diag[2] <- 1;
  end;
end;

function tty_read(c,tmo):
begin
  tty_rx_tmo.start ();
  c <- tty_stdin;
  tty_rx_tmo.stop ();
  if tty_stdin_tmo = 1 then
    tmo <- true
  else
    tmo <- false;
end with inline;


process interp:
begin
  reg d: int[16];
  reg tmo:bool;
  reg c:char;
  reg pos:int[8];
  reg buf_ind: int[4];
  
  reg token_state:int[8];
  const TOKEN_ERR: value := 31;
    
  line <- "Interpreter Ready;.";
  tty_print ();
  
  token_state <- 0;
  buf_ind <- 0;
  
  always do
  begin
    match token_state with
    begin
      when 0:
      begin
        c <- tty_stdin;
      end;
      when 1:
      begin
        pos <- pos + 1;
        tty_read(c,tmo);
        if tmo = true then
          token_state <- TOKEN_ERR;            
      end;
    end;

    match token_state with
    begin
      when 0,1:
      begin
        if c >= '0' and c <= '9' then
        begin
          line.[buf_ind] <- c;
          buf_ind <- buf_ind + 1;
          token_state <- 1;
        end
        else
        match c with
        begin
          when ';': 
          begin
            if buf_ind > 1 then 
              token_state <- 10
            else
              token_state <- TOKEN_ERR;
          end;
          when ' ': 
          begin
            if token_state <> 0 then 
              token_state <- TOKEN_ERR;
          end;
          when '-':
          begin
            if token_state = 0 then
            begin
              line.[0] <- '-';
              buf_ind <- 1;
              token_state <- 1;
            end
            else
              token_state <- TOKEN_ERR;
          end;
          when others: token_state <- TOKEN_ERR;
        end;              
      end;
      when 10:
      begin
        line.[buf_ind] <- '.';
        d <- str_to_int();
        d <- d + 1;
        str_of_int(d);
        tty_print ();  
        tty_stdout <- ';';
        token_state <- 0; 
        buf_ind <- 0;     
      end;
      when TOKEN_ERR:
      begin
        line <- "ERR;.";
        tty_print ();        
        token_state <- 0; 
        buf_ind <- 0;     
      end;
    end;
  end;
end;

