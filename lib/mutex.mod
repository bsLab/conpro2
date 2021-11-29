--
--      ==================================
--      OOOO   OOOO OOOO  O      O   OOOO
--      O   O  O    O     O     O O  O   O
--      O   O  O    O     O     O O  O   O
--      OOOO   OOOO OOOO  O     OOO  OOOO
--      O   O     O    O  O    O   O O   O
--      O   O     O    O  O    O   O O   O
--      OOOO   OOOO OOOO  OOOO O   O OOOO
--      ==================================
--      BSSLAB, Dr. Stefan Bosse, http://www.bsslab.de
--
--      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
--                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
--                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
--                 OUTSIDE OF THE SOFTWARE SYSTEM.
--
--    $AUTHORS:     Stefan Bosse
--    $INITIAL:     (C) 2006-2009 BSSLAB
--    $CREATED:     5.12.2008
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements mutex object
--
--
--    $ENDOFINFO
--

#version "2.11";

--
-- parameters used, allowed and default values
--
#parameter
begin
  --
  -- 1: static priority scheduling
  -- 2: dynamic fifo scheduling
  --
  $scheduler["static","fifo"] <= "static";
  $arch001 <= 1;
  --
  -- Mutex Model:
  --  owner: only the owner process of a mutex can unlock the mutex
  --  group: each member of a process group can unlock the mutex
  --
  $model["owner","group"] <= "owner";
  $fsm["moore","mealy"] <= "moore";
  $ARRAY [0,1]<= 0;
end;

--
-- Supported object methods
--
#methods
begin
  init();
  lock();
  unlock();
end;

#assert
begin
  size($P.init) >= 1;
  size($P.lock) >= 1; 
  size($P.unlock) >= 1;
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal MUTEX_$O_INIT: out std_logic;
  end;
  foreach $p in $P.lock do
  begin
    signal MUTEX_$O_LOCK: out std_logic;
  end;
  foreach $p in $P.unlock do
  begin
    signal MUTEX_$O_UNLOCK: out std_logic;
  end;
  foreach $p in $P do
  begin
    signal MUTEX_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    MUTEX_$O_INIT => MUTEX_$O_$p_INIT;
  end;
  foreach $p in $P.lock do
  begin
    MUTEX_$O_LOCK => MUTEX_$O_$p_LOCK;
  end;
  foreach $p in $P.unlock do
  begin
    MUTEX_$O_UNLOCK => MUTEX_$O_$p_UNLOCK;
  end;
  foreach $p in $P do
  begin
    MUTEX_$O_GD => MUTEX_$O_$p_GD;
  end;
end;

--
-- Object method access (local process context)
-- for each method ...
--
init: #access ($fsm = "moore")
begin
  #data
  begin
    MUTEX_$O_INIT <= MUTEX_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for MUTEX_$O_GD = '0';
  end;
end;

lock: #access ($fsm = "moore")
begin
  #data
  begin
    MUTEX_$O_LOCK <= MUTEX_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for MUTEX_$O_GD = '0';
  end;
end;


unlock: #access ($fsm = "moore")
begin
  #data
  begin
    MUTEX_$O_UNLOCK <= MUTEX_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for MUTEX_$O_GD = '0';
  end;
end;
init: #access ($fsm = "mealy")
begin
  #data
  begin
    MUTEX_$O_INIT <= '1' when $ACC else '0';
  end;
  #control
  begin
    wait for MUTEX_$O_GD = '0';
  end;
end;

lock: #access ($fsm = "mealy")
begin
  #data
  begin
    MUTEX_$O_LOCK <= '1' when $ACC else '0';
  end;
  #control
  begin
    wait for MUTEX_$O_GD = '0';
  end;
end;


unlock: #access ($fsm = "mealy")
begin
  #data
  begin
    MUTEX_$O_UNLOCK <= '1' when $ACC else '0';
  end;
  #control
  begin
    wait for MUTEX_$O_GD = '0';
  end;
end;

--
-- Implementation (global module context)
-- VHDL signals required, both for mapping processes
-- and auxilliary signals.
--
#signals
begin
  --
  -- Implementation signals
  --

  foreach $p in $P.lock do
  begin
    signal MUTEX_$O_$p_LOCK: std_logic;
  end;
  foreach $p in $P.unlock do
  begin
    signal MUTEX_$O_$p_UNLOCK: std_logic;
  end;
  foreach $p in $P.init do
  begin
    signal MUTEX_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P do
  begin
    signal MUTEX_$O_$p_GD: std_logic;    
    signal MUTEX_$O_$p_LOCKed: std_logic;
  end;
end;

#signals ($scheduler="static")
begin
  signal MUTEX_$O_LOCKed: std_logic;
end;

#signals ($scheduler="fifo")
begin
  signal MUTEX_$O_LOCKed: std_logic;
  foreach $p in $P do
  begin
    signal MUTEX_$O_$p_QUEUED: std_logic;
    signal MUTEX_$O_$p_PRIO: std_logic_vector[index_width($P)];
  end;
  signal MUTEX_$O_HEAD: std_logic_vector[index_width($P)];
  signal MUTEX_$O_TAIL: std_logic_vector[index_width($P)];
  signal MUTEX_$O_OWNER: std_logic_vector[index_width($P)];
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
MUTEX_$O_SCHED: #process ($scheduler="static" and $fsm="moore")
begin
  if $CLK then
  begin
    if $RES then
    begin
      MUTEX_$O_LOCKed <= '0';
      foreach $p in $P do
      begin
        MUTEX_$O_$p_GD <= '1';
      end;
      foreach $p in $P.lock do
      begin
        MUTEX_$O_$p_LOCKed <= '0';
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        MUTEX_$O_$p_GD <= '1';
      end;
      sequence
      begin
        foreach $p in $P.init do
        begin
          if MUTEX_$O_$p_INIT = '1' then
          begin
            MUTEX_$O_LOCKed <= '0';
            MUTEX_$O_$p_GD <= '0';
            foreach $l in $P.lock do
            begin
              if MUTEX_$O_$l_LOCKed = '1' then
              begin
                MUTEX_$O_$l_LOCKed <= '0';
                MUTEX_$O_$l_GD <= '0';
              end;
            end;
          end;
        end;
        foreach $p in $P.lock do
        begin
          if MUTEX_$O_$p_LOCK = '1' and MUTEX_$O_LOCKed  = '0' then
          begin
            MUTEX_$O_LOCKed <= '1';
            MUTEX_$O_$p_LOCKed <= '1';
            MUTEX_$O_$p_GD <= '0';
          end;
        end;
        if $model = "owner" then
          foreach $p in $P.unlock do
          begin
            if MUTEX_$O_$p_UNLOCK = '1' then
            begin
              MUTEX_$O_LOCKed <= '0';
              MUTEX_$O_$p_LOCKed <= '0';
              MUTEX_$O_$p_GD <= '0';
            end;
          end;
        if $model = "group" then
        begin
          foreach $p in $P.unlock do
          begin
            if MUTEX_$O_$p_UNLOCK = '1' then
            begin
              MUTEX_$O_LOCKed <= '0';
              if member($P.lock,$p) = true then
                MUTEX_$O_$p_LOCKed <= '0';
              MUTEX_$O_$p_GD <= '0';
            end;
          end;
        end;
      end;
    end;
  end;
end;

MUTEX_$O_SCHED: #process ($scheduler="fifo" and $fsm="moore")
begin
  if $CLK then
  begin
    if $RES then
    begin
      MUTEX_$O_LOCKed <= '0';
      foreach $p in $P do
      begin
        MUTEX_$O_$p_GD <= '1';
        MUTEX_$O_$p_QUEUED <= '0';
        MUTEX_$O_$p_PRIO <= to_logic(0,index_width($P));
      end;
      foreach $p in $P.lock do
      begin
        MUTEX_$O_$p_LOCKed <= '0';
      end;
      MUTEX_$O_HEAD <= to_logic(0,index_width($P));
      MUTEX_$O_TAIL <= to_logic(0,index_width($P));
      MUTEX_$O_OWNER <= to_logic(0,index_width($P));
    end
    else
    begin
      foreach $p in $P do
      begin
        MUTEX_$O_$p_GD <= '1';
      end;
      sequence
      begin
        foreach $p in $P.init do
        begin
          if MUTEX_$O_$p_INIT = '1' then
          begin
            MUTEX_$O_LOCKed <= '0';
            MUTEX_$O_$p_GD <= '0';
            foreach $pa in $P do
            begin
              MUTEX_$O_$pa_QUEUED <= '0';
              MUTEX_$O_$pa_PRIO <= to_logic(0,index_width($P));
            end;
            foreach $pl in $P.lock do
            begin
              MUTEX_$O_$pl_LOCKed <= '0';
            end;
            MUTEX_$O_HEAD <= to_logic(0,index_width($P));
            MUTEX_$O_TAIL <= to_logic(0,index_width($P));
            MUTEX_$O_OWNER <= to_logic(0,index_width($P));
          end;
        end;
        foreach $p in $P.lock do
        begin
          if MUTEX_$O_$p_LOCK = '1' and MUTEX_$O_LOCKed = '0' then
          begin
            MUTEX_$O_$p_GD <= '0';
            MUTEX_$O_LOCKed <= '1';
            MUTEX_$O_$p_LOCKed <= '1';
            MUTEX_$O_OWNER <= to_logic(index($P,$p)+1,index_width($P));
          end;
        end;
        foreach $p in $P.lock do
        begin
          if MUTEX_$O_$p_LOCK = '1' and MUTEX_$O_$p_QUEUED = '0' and MUTEX_$O_OWNER /= to_logic(index($P,$p)+1,index_width($P)) then
          begin
            MUTEX_$O_$p_PRIO <= MUTEX_$O_HEAD + 1;
            MUTEX_$O_HEAD <= MUTEX_$O_HEAD + 1;
            MUTEX_$O_$p_QUEUED <= '1';
          end;
        end;
        foreach $p in $P.lock do
        begin
          if MUTEX_$O_$p_LOCK = '1' and MUTEX_$O_$p_QUEUED = '1' and MUTEX_$O_$p_PRIO = MUTEX_$O_TAIL then
          begin
            MUTEX_$O_$p_GD <= '0';
            MUTEX_$O_$p_QUEUED <= '0';
            MUTEX_$O_OWNER <= to_logic(index($P,$p)+1,index_width($P));
          end;
        end;
        if $arch001 = 1 then
        begin
          foreach $p in $P.unlock do
          begin
            if MUTEX_$O_$p_UNLOCK = '1' then
            begin
              MUTEX_$O_$p_GD <= '0';
              MUTEX_$O_$p_LOCKed <= '0';
              if MUTEX_$O_HEAD = MUTEX_$O_TAIL then
              begin
                MUTEX_$O_LOCKed <= '0';
                MUTEX_$O_OWNER <= to_logic(0,index_width($P));
              end
              else
              begin
                MUTEX_$O_TAIL <= MUTEX_$O_TAIL + 1;
              end;
            end;
          end;
        end;
        if $arch001 = 2 then
        begin
          if expand($P.lock,$p,or,MUTEX_$O_$p_UNLOCK = '1') then
          begin
            foreach $pl in $P.unlock do
            begin
              if MUTEX_$O_$pl_UNLOCK = '1' then
              begin
                MUTEX_$O_$pl_GD <= '0';
                MUTEX_$O_$pl_LOCKed <= '0';
              end;
            end;
            if MUTEX_$O_HEAD = MUTEX_$O_TAIL then
            begin
              MUTEX_$O_LOCKed <= '0';
              MUTEX_$O_OWNER <= to_logic(0,index_width($P));
            end
            else
            begin
              MUTEX_$O_TAIL <= MUTEX_$O_TAIL + 1;
            end;
          end;
        end;
      end; 
    end;
  end;
end;

MUTEX_$O_SCHED: #process ($scheduler="static" and $fsm="mealy")
begin
  foreach $p in $P do
  begin
    MUTEX_$O_$p_GD <= '1';
  end;
  sequence
  begin
    foreach $p in $P.init do
    begin
      if MUTEX_$O_$p_INIT = '1' then
      begin
        MUTEX_$O_$p_GD <= '0';
        foreach $l in $P.lock do
        begin
          if MUTEX_$O_$l_LOCKed = '1' then
          begin
            MUTEX_$O_$l_GD <= '0';
          end;
        end;
      end;
    end;
    foreach $p in $P.lock do
    begin
      if MUTEX_$O_$p_LOCK = '1' and MUTEX_$O_LOCKed  = '0' then
      begin
        MUTEX_$O_$p_GD <= '0';
      end;
    end;
    if $model = "owner" then
      foreach $p in $P.unlock do
      begin
        if MUTEX_$O_$p_UNLOCK = '1' then
        begin
          MUTEX_$O_$p_GD <= '0';
        end;
      end;
    if $model = "group" then
    begin
      foreach $p in $P.unlock do
      begin
        if MUTEX_$O_$p_UNLOCK = '1' then
        begin
          MUTEX_$O_$p_GD <= '0';
        end;
      end;
    end;
  end;

  if $CLK then
  begin
    if $RES then
    begin
      MUTEX_$O_LOCKed <= '0';
      foreach $p in $P.lock do
      begin
        MUTEX_$O_$p_LOCKed <= '0';
      end;
    end
    else
    begin
      sequence
      begin
        foreach $p in $P.init do
        begin
          if MUTEX_$O_$p_INIT = '1' then
          begin
            MUTEX_$O_LOCKed <= '0';
            foreach $l in $P.lock do
            begin
              if MUTEX_$O_$l_LOCKed = '1' then
              begin
                MUTEX_$O_$l_LOCKed <= '0';
              end;
            end;
          end;
        end;
        foreach $p in $P.lock do
        begin
          if MUTEX_$O_$p_LOCK = '1' and MUTEX_$O_LOCKed  = '0' then
          begin
            MUTEX_$O_LOCKed <= '1';
            MUTEX_$O_$p_LOCKed <= '1';
          end;
        end;
        if $model = "owner" then
          foreach $p in $P.unlock do
          begin
            if MUTEX_$O_$p_UNLOCK = '1' then
            begin
              MUTEX_$O_LOCKed <= '0';
              MUTEX_$O_$p_LOCKed <= '0';
            end;
          end;
        if $model = "group" then
        begin
          foreach $p in $P.unlock do
          begin
            if MUTEX_$O_$p_UNLOCK = '1' then
            begin
              MUTEX_$O_LOCKed <= '0';
              if member($P.lock,$p) = true then
                MUTEX_$O_$p_LOCKed <= '0';
            end;
          end;
        end;
      end;
    end;
  end;
end;
