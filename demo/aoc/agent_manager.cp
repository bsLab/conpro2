open Core;
open Process;
open Event;


exception Agent_error;

queue managing: int[8] with depth=8;

type direction : {
  SOUTH;
  WEST;
  EAST;
  NORTH;
};


--
-- Agent state (>0: activity state)
-- 
const FREE : value := 0;
const START : value := -1;
const DYING : value := -2;
const PASSED : value := -3;
const GONE : value := -4;
const RESERVED : value := -5;

type agent_control : {
  ac: char;
  lid: int[8];
  dx: int[8];
  dy: int[8];
  dir: int[4];
  live: int[4];
  state: int[8];    
};


array agent_table: var[TABLE_SIZE2] of agent_control;
function linkQ(d:int[4]) return (b:bool):
begin
  b <- false;
  match d with
  begin
    when NORTH: if links_alive[3] = 1 then b <- true;
    when SOUTH: if links_alive[0] = 1 then b <- true;
    when WEST: if links_alive[1] = 1 then b <- true;
    when EAST: if links_alive[2] = 1 then b <- true;
  end;
end;

function migrate(t:int[8],d:int[4],s:int[8]):
begin
  agent_table.[t].dir <- d;  
  agent_table.[t].state <- s;  
  managing <- t;
end;

function kill(t:int[8]):
begin
  agent_table.[t].state <- FREE;  
end;

function new(acc:char) return (id:int[8]):
begin
  reg i: int[6];
  i <- 0;
  while i < TABLE_SIZE and agent_table.[i].state <> FREE do
    i<-i+1;
  if i = TABLE_SIZE then raise Agent_error;  
  agent_table.[i].ac <- acc;
  agent_table.[i].dx <- 0;
  agent_table.[i].dy <- 0;
  agent_table.[i].live <- MAX_LIVE;
  agent_table.[i].state <- START;
  id <- i;
end;

array link_rx_proc: process[NUM_LINKS] of
begin
  reg d,v: logic[8];
  reg d32: logic[32];
  reg ac: char;
  reg err: bool;
  reg i: int[8];
  reg n: int[8];
  always do
  begin
    try
    begin
      d <- rx_queue.[#];
      
      if d = MSG_ALV then tx_queue.[#] <- MSG_ACK
      else if d = MSG_ACK then links_alive.[#] <- 1
      else if d <> MSG_REQ then raise Agent_error;
      
      i <- TABLE_SIZE,n <- 1;
      while i < TABLE_SIZE2 and agent_table.[i].state <> FREE do
        i<-i+1;
      if i = TABLE_SIZE2 then raise Agent_error;
      agent_table.[i].state <- RESERVED;  
      --
      -- Message:
      --    AC[8]
      --    LID[8]
      --    DX[8]
      --    DY[8]
      --    STATE[8]
      --    DATA
      --      (name:char) (value)
      --      ..
      --    END
      --
      while n <> 0 do
      begin
        d <- rx_queue.[#];
        --
        -- GID
        --
        match n with
        begin
          when 1:
          begin
            ac <- to_char(d);
            agent_table.[i].ac <- ac;
          end;
          when 2: agent_table.[i].lid <- to_int(d);
          when 3: agent_table.[i].dx <- to_int(d);
          when 4: agent_table.[i].dy <- to_int(d);
          when 5: 
          begin
            agent_table.[i].state <- to_int(d); 
            n <- -1;
          end;
        end;
        n <- n + 1;
      end;
      match ac with
      begin
        when 'a':
        begin
          v <- 1;
          while v <> 0 do
          begin
            v <- rx_queue.[#];
            d <- rx_queue.[#];
            match v with
            begin
              when 'a': agents_a_data.[i].dx0 <- to_int(d);
              when 'b': agents_a_data.[i].dy0 <- to_int(d);
              when 'c': agents_a_data.[i].len <- to_int(d);
              when 'd': 
              begin
                d32 <- d;
                for j = 1 to 3 do
                begin
                  d <- rx_queue.[#];
                  d32 <- d32 lsl 8;
                  d32 <- d32 lor d;
                end;
                agents_a_data.[i].data <- to_int(d32);
              end;
              when 'e': agents_a_data.[i].dx <- to_int(d);
              when 'f': agents_a_data.[i].dx <- to_int(d);
              when 'g': agents_a_data.[i].gamma_x <- to_int(d);
              when 'h': agents_a_data.[i].gamma_y <- to_int(d);
              when 'i': agents_a_data.[i].dir <- to_int(d);
              when 'j': if d = 0 then agents_a_data.[i].processed <- false
                        else agents_a_data.[i].processed <- true;
            end;
          end;
          match agent_table.[i].state with
          begin
            when START: transition_1 <- i;
            when 1: transition_2 <- i;
            when 2: transition_3 <- i;
            when 3: transition_4 <- i;
            when 4: transition_5 <- i;
            when 5: transition_6 <- i;
            when 6: transition_7 <- i;
          end;
        end;
        when others:
        begin
          agent_table.[i].state <- FREE;          
        end;
      end;
    end
    with 
    begin
      when Agent_error: i <- 0;
    end;
  end;
end;

process manager:
begin
  reg t:int[8];
  reg ln: int[4];
  reg d:logic[8];
  reg d32:logic[32];
  reg n: int[8];
  reg v,ac: char;
    
  always do
  begin
    --
    -- we handle only migration requests
    --
    t <- managing;
    match agent_table.[t].dir with
    begin
      when SOUTH: 
      begin
        ln <- 0;
        agent_table.[t].dy <- agent_table.[t].dy - 1;
      end;
      when WEST: 
      begin
        ln <- 1;
        agent_table.[t].dx <- agent_table.[t].dx - 1;
      end;
      when EAST: 
      begin
        ln <- 2;
        agent_table.[t].dx <- agent_table.[t].dx + 1;
      end;
      when NORTH: 
      begin
        ln <- 3;
        agent_table.[t].dy <- agent_table.[t].dy + 1;
      end;
    end;
    ac <- agent_table.[t].ac;
    n <- 1;
    while n <> 0 do
    begin
      --
      -- GID
      --
      match n with
      begin
        when 1: d <- to_logic(agent_table.[t].ac);
        when 2: d <- to_logic(agent_table.[t].lid);
        when 3: d <- to_logic(agent_table.[t].dx);
        when 4: d <- to_logic(agent_table.[t].dy);
        when 5: 
        begin
          d <- to_logic(agent_table.[t].state); 
          n <- -1;
        end;
      end;
      n <- n + 1;
      tx_queue.[ln] <- d;
    end;

    match ac with
    begin
      when 'a':
      begin
        v <- 'a';
        while v <> 0 do
        begin
          tx_queue.[ln] <- to_logic(v);
          match v with
          begin
            when 'a': d <- to_logic(agents_a_data.[t].dx0);
            when 'b': d <- to_logic(agents_a_data.[t].dy0);
            when 'c': d <- to_logic(agents_a_data.[t].len);
            when 'd': 
            begin
              d32 <- to_logic(agents_a_data.[t].data);
              for j = 1 to 3 do
              begin
                tx_queue.[ln] <- d32[24 to 31];
                d32 <- d32 lsr 8;
              end;
              d <- d32[24 to 31];
            end;
            when 'e': d <- to_logic(agents_a_data.[t].dx);
            when 'f': d <- to_logic(agents_a_data.[t].dx);
            when 'g': d <- to_logic(agents_a_data.[t].gamma_x);
            when 'h': d <- to_logic(agents_a_data.[t].gamma_y);
            when 'i': d <- to_logic(agents_a_data.[t].dir);
            when 'j': if agents_a_data.[t].processed = true then d <- 1
                      else d <- 0;
          end;
          tx_queue.[ln] <- d;
          v <- v + 1;
        end;
      end;
    end;
    agent_table.[t].state <- GONE;          
  end;
end;

function agent_init():
begin
  for i = 0 to TABLE_SIZE2-1 do
  begin
    agent_table.[i].ac <- agent_table.[i].ac;
    agent_table.[i].lid <- agent_table.[i].lid;
    agent_table.[i].dx <- agent_table.[i].dx;
    agent_table.[i].dy <- agent_table.[i].dy;
    agent_table.[i].live <- agent_table.[i].live;
    agent_table.[i].state <- agent_table.[i].state;
    agent_table.[i].dir <- agent_table.[i].dir;
    agent_table.[i].ac <- agent_table.[i].ac;
    agent_table.[i].lid <- agent_table.[i].lid;
    agent_table.[i].dx <- agent_table.[i].dx;
    agent_table.[i].dy <- agent_table.[i].dy;
    agent_table.[i].live <- agent_table.[i].live;
    agent_table.[i].state <- agent_table.[i].state;
  end;

  for i = 0 to TABLE_SIZE2-1 do
  begin
    agent_table.[i].lid <- i;
    agent_table.[i].state <- FREE;
    agent_table.[i].lid <- (-1);
    agent_table.[i].state <- FREE;
  end;
  for i = 0 to NUM_LINKS-1 do
  begin
    link_rx_proc.[i].start();
  end;
  manager.start();
end;
