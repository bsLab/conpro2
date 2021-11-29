open Event;
--
-- Pipelined processes model patterns
--
exception Signal,Await;
queue qin,qout1,qou2,qs: int[8];
event agent with latch;

process p_comp:
begin
  reg t: int[8];
  always do 
  begin
    try
    begin
      t <- qin;
      if check_signal(t) = true then raise Signal;
      -- compute --
      -- state transitions --
      if cond1 then qout1 <- t;
      else if cond2 then qout2 <- t;
    end
    with
    begin
      when Signal : qs <- t;
    end;
  end;
end;


--
-- IO events
--
process p_ev_io:
begin
  reg first: bool;
  reg t,t0: int[8];
  always do
  begin
    try
    begin
      t0 <- (-1),first <- true;
      always do
      begin
        t <- qin;
        if check_signal(t) = true then raise Signal;
        if t = t0 then 
        begin
          -- at beginning of queue, wait for IO event
          qin <- t;
          raise Await;
        end;
        {x,b} <- try_out('A');
        if b = true then
        begin
          -- post computations --
          -- state transitions --
          if cond1 then qout1 <- t;
          else if cond2 then qout2 <- t;
        end
        else
        begin
          if first = true then 
          begin
            t0 <- t, 
            first <- false;
          end;
          qin <- t;
        end;
      end;
    end;
    with
    begin
      when Signal : qs <- t;
      when Await : 
      begin
        agent.await (); 
        -- to avoid race conditions: signal event again for other waiters!
        agent.wakeup();
      end;
    end;
  end;
end;

--
-- Timer interval delays (slazy delay: at least time interval behaviour)
--
process p_ev_timer: begin
  reg first: bool;
  reg t: int[8];
  queue qtd: int[8];
  always do
  begin
    try
    begin
      always do
      begin
        while qin.empty () <> false do begin
           t <- qin;
           if check_signal(t) = true then raise Signal;
           -- pre computations --
           tqd <- t;
        end;
        wait for DELAY;
        while qtd.empty () <> false do begin
           t <- qtd;
           if check_signal(t) = true then raise Signal;
           -- post computations --
           -- state transitions --
           if cond1 then qout1 <- t;
           else if cond2 then qout2 <- t;
        end;
      end;
    end;
    with
    begin
      when Signal : qs <- t;
    end;
  end;
end;

process main:
begin
  --
  -- start agent t in state x
  --
  qx <- t;
end;
