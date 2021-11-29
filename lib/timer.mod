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
--    $INITIAL:     (C) 2006-2010 BSSLAB
--    $CREATED:     10.12.2008
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements timer object
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
  -- await method call implementation 
  --  1: one-by-one
  --  2: all-by-one
  --
  $arch001[1,2] <= 1;
  --
  -- Timer implementation
  -- 1: sequenced in scheduler
  -- 2: concurrent to scheduler 
  --
  $arch002[1,2] <= 2; 
  --
  --  Event with Latch Behaviour
  --
  $latch[0,1] <= 0;

  $time with filtered and sorted;
  
  $sig;
  --
  -- 0: '0'
  -- 1: '1'
  -- 2: 'Z'
  --
  $sig_action_level[0,1,2];
  $sig_def_level[0,1,2];
  
  $mode[0,1] <= 0;
  $ARRAY [0,1]<= 0;
end;

--
-- Supported object methods
--
#methods
begin
  init();
  time (#rhs:natural);
  await();
  wakeup();
  start ();
  stop ();
  --
  -- 0: continously (default)
  -- 1: one-shot
  --
  mode (#rhs:logic);
  --
  --  optional output signal:
  --  sig,action_level(timer active),def_level (timer inactive)
  --
  sig_action(#lhs:logic,#rhs:natural,#rhs:natural);
end;

#assert
begin
  size($P.init) >= 1;
  size($P.time) >= 1; 
  size($P.await) >= 1; 
  size($P.start) >= 1; 
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal TIMER_$O_INIT: out std_logic;
  end;
  foreach $p in $P.await do
  begin
    signal TIMER_$O_AWAIT: out std_logic;
  end;
  foreach $p in $P.wakeup do
  begin
    signal TIMER_$O_WAKEUP: out std_logic;
  end;
  foreach $p in $P.start do
  begin
    signal TIMER_$O_START: out std_logic;
  end;
  foreach $p in $P.stop do
  begin
    signal TIMER_$O_STOP: out std_logic;
  end;
  foreach $p in $P.mode do
  begin
    signal TIMER_$O_MODE_SET: out std_logic;
    signal TIMER_$O_MODE: out std_logic;
  end;
  foreach $p in $P.time do
  begin
    signal TIMER_$O_TIME_SET: out std_logic;
    signal TIMER_$O_TIME: out std_logic_vector[index_width($time)];
  end;
  foreach $p in $P do
  begin
    signal TIMER_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    TIMER_$O_INIT => TIMER_$O_$p_INIT;
  end;
  foreach $p in $P.await do
  begin
    TIMER_$O_AWAIT => TIMER_$O_$p_AWAIT;
  end;
  foreach $p in $P.wakeup do
  begin
    TIMER_$O_WAKEUP => TIMER_$O_$p_WAKEUP;
  end;
  foreach $p in $P.start do
  begin
    TIMER_$O_START => TIMER_$O_$p_START;
  end;
  foreach $p in $P.stop do
  begin
    TIMER_$O_STOP => TIMER_$O_$p_STOP;
  end;
  foreach $p in $P.time do
  begin
    TIMER_$O_TIME_SET => TIMER_$O_$p_TIME_SET;
    TIMER_$O_TIME => TIMER_$O_$p_TIME;
  end;
  foreach $p in $P.mode do
  begin
    TIMER_$O_MODE_SET => TIMER_$O_$p_MODE_SET;
    TIMER_$O_MODE => TIMER_$O_$p_MODE;
  end;
  foreach $p in $P do
  begin
    TIMER_$O_GD => TIMER_$O_$p_GD;
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
    TIMER_$O_INIT <= TIMER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for TIMER_$O_GD = '0';
  end;
end;

await: #access
begin
  #data
  begin
    TIMER_$O_AWAIT <= TIMER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for TIMER_$O_GD = '0';
  end;
end;

wakeup: #access
begin
  #data
  begin
    TIMER_$O_WAKEUP <= TIMER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for TIMER_$O_GD = '0';
  end;
end;

start: #access
begin
  #data
  begin
    TIMER_$O_START <= TIMER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for TIMER_$O_GD = '0';
  end;
end;

stop: #access
begin
  #data
  begin
    TIMER_$O_STOP <= TIMER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for TIMER_$O_GD = '0';
  end;
end;



time: #access
begin
  #set
  begin
    $time <= $ARG1;
  end;
  #data
  begin
    TIMER_$O_TIME_SET <= TIMER_$O_GD when $ACC else '0';
    TIMER_$O_TIME <= index($time,$ARG1) when $ACC else 0;
  end;
  #control
  begin
    wait for TIMER_$O_GD = '0';
  end;
end;

mode: #access
begin
  #set
  begin
    $mode <= $ARG1;
  end;
  #data
  begin
    TIMER_$O_MODE_SET <= TIMER_$O_GD when $ACC else '0';
    TIMER_$O_MODE <= $ARG1 when $ACC else '0';
  end;
  #control
  begin
    wait for TIMER_$O_GD = '0';
  end;
end;

sig_action: #access
begin
  #set
  begin
    $sig <= $ARG1;
    $sig_action_level <= $ARG2;
    $sig_def_level <= $ARG3;
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
  
  signal TIMER_$O_COUNTER: std_logic_vector[width((max($time)*$clock)/1000000000)];
  signal TIMER_$O_COUNT: std_logic_vector[width((max($time)*$clock)/1000000000)];
  signal TIMER_$O_ENABLED: std_logic;
  signal TIMER_$O_MODE: std_logic;
  
  foreach $p in $P.await do
  begin
    signal TIMER_$O_$p_AWAIT: std_logic;
    signal TIMER_$O_$p_LOCKed: std_logic;
  end;
  foreach $p in $P.init do
  begin
    signal TIMER_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P.wakeup do
  begin
    signal TIMER_$O_$p_WAKEUP: std_logic;
  end;
  foreach $p in $P.time do
  begin
    signal TIMER_$O_$p_TIME_SET: std_logic;
    signal TIMER_$O_$p_TIME: std_logic_vector[index_width($time)];
  end;
  foreach $p in $P.mode do
  begin
    signal TIMER_$O_$p_MODE_SET: std_logic;
    signal TIMER_$O_$p_MODE: std_logic;
  end;
  foreach $p in $P.start do
  begin
    signal TIMER_$O_$p_START: std_logic;
  end;
  foreach $p in $P.stop do
  begin
    signal TIMER_$O_$p_STOP: std_logic;
  end;
  foreach $p in $P do
  begin
    signal TIMER_$O_$p_GD: std_logic;    
  end;
end;

#signals ($latch=1)
begin
  signal TIMER_$O_LATCH: std_logic;
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
TIMER_$O_SCHED: #process 
begin
  if $CLK then
  begin
    if $RES then
    begin
      TIMER_$O_ENABLED <= '0';
      TIMER_$O_MODE <= '0';
      TIMER_$O_COUNTER <= to_logic(0,width((max($time)*$clock)/1000000000));
      TIMER_$O_COUNT <= to_logic((nth($time,1)*$clock)/1000000000,
                                 width((max($time)*$clock)/1000000000));
      
      foreach $p in $P do
      begin
        TIMER_$O_$p_GD <= '1';
      end;
      foreach $p in $P.await do
      begin
        TIMER_$O_$p_LOCKed <= '0';
      end;

      if $latch = 1 then
        TIMER_$O_LATCH <= '0';
    end
    else
    begin
      foreach $p in $P do
      begin
        TIMER_$O_$p_GD <= '1';
      end;
    
      if $arch002 = 2 then
      begin
        if TIMER_$O_ENABLED = '1' then
        begin
          if TIMER_$O_COUNTER = to_logic(0,width((max($time)*$clock)/1000000000)) then
          begin
            foreach $p in $P.await do
            begin
              if TIMER_$O_$p_LOCKed = '1' then
              begin
                TIMER_$O_$p_LOCKed <= '0';
                TIMER_$O_$p_GD <= '0';
              end;
            end;
            if TIMER_$O_MODE = '0' then
            begin
              TIMER_$O_COUNTER <= TIMER_$O_COUNT;
            end
            else
              TIMER_$O_ENABLED <= '0';
          end
          else
          begin
            TIMER_$O_COUNTER <= TIMER_$O_COUNTER - 1;
          end;
        end;
      end;
      
      sequence
      begin
        foreach $p in $P.init do
        begin
          if TIMER_$O_$p_INIT = '1' then
          begin
            TIMER_$O_COUNTER <= to_logic(0,width((max($time)*$clock)/1000000000));
            TIMER_$O_COUNT <= to_logic((nth($time,1)*$clock)/1000000000,
                                       width((max($time)*$clock)/1000000000));            
            TIMER_$O_ENABLED <= '0';
            TIMER_$O_MODE <= to_logic($mode,1);
            
            TIMER_$O_$p_GD <= '0';
            foreach $l in $P.await do
            begin
              if TIMER_$O_$l_LOCKed = '1' then
              begin
                TIMER_$O_$l_LOCKed <= '0';
                TIMER_$O_$l_GD <= '0';
              end;
            end;
            if $latch = 1 then
              TIMER_$O_LATCH <= '0';
          end;
        end;
        if $arch001 = 1 and $latch =  0 then
        begin
          foreach $p in $P.await do
          begin
            if TIMER_$O_$p_AWAIT = '1' and TIMER_$O_$p_LOCKed  = '0' then
            begin
              TIMER_$O_$p_LOCKed <= '1';
            end;
          end;
        end;
        if $arch001 = 2 and $latch = 0 then
        begin
          if expand($P.await,$p,or,TIME_$O_$p_AWAIT = '1' and TIMER_$O_$p_LOCKed  = '0') then
          begin
            foreach $p in $P.await do
            begin
              if TIMER_$O_$p_AWAIT = '1' then
              begin
                TIMER_$O_$p_LOCKed <= '1';
              end;
            end;            
          end;
        end;
        if $arch001 = 1 and $latch =  1 then
        begin
          foreach $p in $P.await do
          begin
            if TIMER_$O_$p_AWAIT = '1' and TIMER_$O_$p_LOCKed  = '0' then
            begin
              if TIMER_$O_LATCH = '0' then
                TIMER_$O_$p_LOCKed <= '1'
              else
              begin
                TIMER_$O_$p_GD <= '0';
                TIMER_$O_LATCH <= '0';
              end;
            end;
          end;
        end;
        if $arch001 = 2 and $latch = 1 then
        begin
          if expand($P.await,$p,or,TIME_$O_$p_AWAIT = '1' and TIMER_$O_$p_LOCKed  = '0') then
          begin
            foreach $p in $P.await do
            begin
              if TIMER_$O_$p_AWAIT = '1' then
              begin
                if TIMER_$O_LATCH = '0' then
                  TIMER_$O_$p_LOCKed <= '1'
                else
                begin
                  TIMER_$O_$p_GD <= '0';
                  TIMER_$O_LATCH <= '0';
                end;                  
              end;
            end;            
          end;
        end;
        foreach $p in $P.start do
        begin
          if TIMER_$O_$p_START = '1'  then
          begin
            TIMER_$O_COUNTER <= TIMER_$O_COUNT;
            TIMER_$O_ENABLED <= '1';
            TIMER_$O_$p_GD <= '0';
            if $latch = 1 then
              TIMER_$O_LATCH <= '0';
          end;
        end;
        foreach $p in $P.stop do
        begin
          if TIMER_$O_$p_STOP = '1'  then
          begin
            TIMER_$O_COUNTER <= to_logic(0,width((max($time)*$clock)/1000000000));
            TIMER_$O_ENABLED <= '0';
            TIMER_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.time do
        begin
          if TIMER_$O_$p_TIME_SET = '1'  then
          begin
            TIMER_$O_$p_GD <= '0';
            sequence
            begin
              foreach $this_time in $time do
              begin
                if TIMER_$O_$p_TIME = index($time,$this_time) then
                  TIMER_$O_COUNT <= to_logic(($this_time*$clock)/1000000000,
                                              width((max($time)*$clock)/1000000000));
              end;
            end;
          end;
        end;
        foreach $p in $P.mode do
        begin
          if TIMER_$O_$p_MODE_SET = '1'  then
          begin
            TIMER_$O_$p_GD <= '0';
            TIMER_$O_MODE <= TIMER_$O_$p_MODE;
          end;
        end;
        if $arch002 = 1 then
        begin
          if others then
          begin
            if TIMER_$O_ENABLED = '1' then
            begin
              if TIMER_$O_COUNTER = to_logic(0,width((max($time)*$clock)/1000000000)) then
              begin
                if $latch = 1 then
                  TIMER_$O_LATCH <= '1';
                  
                foreach $p in $P.await do
                begin
                  if TIMER_$O_$p_LOCKed = '1' then
                  begin
                    TIMER_$O_$p_LOCKed <= '0';
                    TIMER_$O_$p_GD <= '0';
                    if $latch = 1 then
                      TIMER_$O_LATCH <= '0';
                  end;
                end;
                if TIMER_$O_MODE = '0' then
                 begin
                   TIMER_$O_COUNTER <= TIMER_$O_COUNT;
                 end
                 else
                   TIMER_$O_ENABLED <= '0';
              end
              else
              begin
                TIMER_$O_COUNTER <= TIMER_$O_COUNTER - 1;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

#top
begin
  foreach $s in $sig do
  begin
    $s <= to_logic(nth($sig_action_level,index($sig,$s)+1),1) when TIMER_$O_ENABLED = '1' else
          to_logic(nth($sig_def_level,index($sig,$s)+1),1); 
  end;
end;
