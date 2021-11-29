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
--    $CREATED:     10.12.2008
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements event object
--
--
--    $ENDOFINFO
--

#version "2.06";

--
-- parameters used, allowed and default values
--
#parameter
begin
  $NAME <= "EVENT";
  --
  -- 1: static priority scheduling
  -- 2: dynamic fifo scheduling
  --
  $arch001 <= 1;
  
  --
  --  Event with Latch Behaviour
  --
  $latch[0,1] <= 0;
  $ARRAY [0,1]<= 0;
end;

--
-- Supported object methods
--
#methods
begin
  init();
  await();
  wakeup();
end;

#assert
begin
  size($P.init) >= 1;
  size($P.await) >= 1; 
  size($P.wakeup) >= 1;
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal EVENT_$O_INIT: out std_logic;
  end;
  foreach $p in $P.await do
  begin
    signal EVENT_$O_AWAIT: out std_logic;
  end;
  foreach $p in $P.wakeup do
  begin
    signal EVENT_$O_WAKEUP: out std_logic;
  end;
  foreach $p in $P do
  begin
    signal EVENT_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    EVENT_$O_INIT => EVENT_$O_$p_INIT;
  end;
  foreach $p in $P.await do
  begin
    EVENT_$O_AWAIT => EVENT_$O_$p_AWAIT;
  end;
  foreach $p in $P.wakeup do
  begin
    EVENT_$O_WAKEUP => EVENT_$O_$p_WAKEUP;
  end;
  foreach $p in $P do
  begin
    EVENT_$O_GD => EVENT_$O_$p_GD;
  end;
end;

--
-- Object method access (local process context)
-- for each method ...
--
init: #access
begin
  #data
  begin
    EVENT_$O_INIT <= EVENT_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for EVENT_$O_GD = '0';
  end;
end;

await: #access
begin
  #data
  begin
    EVENT_$O_AWAIT <= EVENT_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for EVENT_$O_GD = '0';
  end;
end;


wakeup: #access
begin
  #data
  begin
    EVENT_$O_WAKEUP <= EVENT_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for EVENT_$O_GD = '0';
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

  foreach $p in $P.await do
  begin
    signal EVENT_$O_$p_AWAIT: std_logic;
    signal EVENT_$O_$p_LOCKed: std_logic;
  end;
  foreach $p in $P.wakeup do
  begin
    signal EVENT_$O_$p_WAKEUP: std_logic;
  end;
  foreach $p in $P.init do
  begin
    signal EVENT_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P do
  begin
    signal EVENT_$O_$p_GD: std_logic;    
  end;
end;

#signals ($latch=1)
begin
  signal EVENT_$O_LATCH: std_logic;
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
EVENT_$O_SCHED: #process 
begin
  if $CLK then
  begin
    if $RES then
    begin
      foreach $p in $P do
      begin
        EVENT_$O_$p_GD <= '1';
      end;
      foreach $p in $P.await do
      begin
        EVENT_$O_$p_LOCKed <= '0';
      end;
      if $latch = 1 then
        EVENT_$O_LATCH <= '0';
    end
    else
    begin
      foreach $p in $P do
      begin
        EVENT_$O_$p_GD <= '1';
      end;
      sequence
      begin
        foreach $p in $P.init do
        begin
          if EVENT_$O_$p_INIT = '1' then
          begin
            EVENT_$O_$p_GD <= '0';
            foreach $l in $P.await do
            begin
              if EVENT_$O_$l_LOCKed = '1' then
              begin
                EVENT_$O_$l_LOCKed <= '0';
                EVENT_$O_$l_GD <= '0';
              end;
            end;
            if $latch = 1 then
              EVENT_$O_LATCH <= '0';
          end;
        end;
        foreach $p in $P.await do
        begin
          if EVENT_$O_$p_AWAIT = '1' and EVENT_$O_$p_LOCKed  = '0' then
          begin
            EVENT_$O_$p_LOCKed <= '1';
          end;
        end;
        if $latch = 0 then
        begin
          if expand($P.wakeup,$p,or,EVENT_$O_$p_WAKEUP = '1') then
          begin
            foreach $p in $P.wakeup do
            begin
              if EVENT_$O_$p_WAKEUP = '1' then 
                EVENT_$O_$p_GD <= '0';          
            end;
            foreach $p in $P.await do
            begin
              if EVENT_$O_$p_LOCKed = '1' then
              begin
                EVENT_$O_$p_LOCKed <= '0';
                EVENT_$O_$p_GD <= '0';
              end;
            end;
          end;
        end;
        if $latch = 1 then
        begin
          if expand($P.wakeup,$p,or,EVENT_$O_$p_WAKEUP = '1') or EVENT_$O_LATCH = '1' then
          begin
            foreach $p in $P.wakeup do
            begin
              if EVENT_$O_$p_WAKEUP = '1' then 
              begin
                EVENT_$O_$p_GD <= '0';
                EVENT_$O_LATCH <= '1';
              end;          
            end;
            foreach $p in $P.await do
            begin
              if EVENT_$O_$p_LOCKed = '1' then
              begin
                EVENT_$O_$p_LOCKed <= '0';
                EVENT_$O_$p_GD <= '0';
                EVENT_$O_LATCH <= '0';
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

