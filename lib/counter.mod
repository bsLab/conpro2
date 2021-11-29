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
--    $CREATED:     18.6.2010
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements counter object (triggered timer)
--
--  This object implements a gated period counter.
--  1. start/stop methods provide internal time measurement (in time ticks)
--  2. start/stop methods and external signal provide external time measurement (in time ticks)
--  3. last acquired period can be accessed by read method
--  4. counter tick time can be set independently from system clock (calculated)
--
--
--    $ENDOFINFO
--

#version "2.2";

--
-- parameters used, allowed and default values
--
#parameter
begin
  $datawidth[4 to 32] <= 8;
  $time with filtered and sorted;
  
  $mode["internal","external"] <= "internal";
  $sig;
  --
  -- 0: '0'
  -- 1: '1'
  -- 2: 'Z'
  --
  $sig_action_level[0,1,2];
  $sig_def_level[0,1,2];
  
  $ARRAY [0,1]<= 0;
end;

--
-- Supported object methods
--
#methods
begin
  init();
  time (#rhs:natural);
  start ();
  stop ();
  read (#lhs: logic[$datawidth]);
  --
  --  optional output signal:
  --  sig,action_level(timer start),def_level (timer stop)
  --
  sig_action(#rhs:logic,#rhs:natural,#rhs:natural);
end;

#assert
begin
  size($P.init) >= 1;
  size($P.time) >= 1; 
  size($P.start) >= 1; 
  size($P.read) >= 1; 
end;
#assert ($mode = "internal")
begin
  size($P.stop) >= 1; 
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal COUNTER_$O_INIT: out std_logic;
  end;
  foreach $p in $P.read do
  begin
    signal COUNTER_$O_RE: out std_logic;
    signal COUNTER_$O_RD: in std_logic_vector[$datawidth];
  end;
  foreach $p in $P.start do
  begin
    signal COUNTER_$O_START: out std_logic;
  end;
  foreach $p in $P.stop do
  begin
    signal COUNTER_$O_STOP: out std_logic;
  end;
  foreach $p in $P.time do
  begin
    signal COUNTER_$O_TIME_SET: out std_logic;
    signal COUNTER_$O_TIME: out std_logic_vector[index_width($time)];
  end;
  foreach $p in $P do
  begin
    signal COUNTER_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    COUNTER_$O_INIT => COUNTER_$O_$p_INIT;
  end;
  foreach $p in $P.read do
  begin
    COUNTER_$O_RE => COUNTER_$O_$p_RE;
    COUNTER_$O_RD => COUNTER_$O_$p_RD;
  end;
  foreach $p in $P.start do
  begin
    COUNTER_$O_START => COUNTER_$O_$p_START;
  end;
  foreach $p in $P.stop do
  begin
    COUNTER_$O_STOP => COUNTER_$O_$p_STOP;
  end;
  foreach $p in $P.time do
  begin
    COUNTER_$O_TIME_SET => COUNTER_$O_$p_TIME_SET;
    COUNTER_$O_TIME => COUNTER_$O_$p_TIME;
  end;
  foreach $p in $P do
  begin
    COUNTER_$O_GD => COUNTER_$O_$p_GD;
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
    COUNTER_$O_INIT <= COUNTER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for COUNTER_$O_GD = '0';
  end;
end;

read: #access
begin
  #data
  begin
    COUNTER_$O_RE <= COUNTER_$O_GD when $ACC else '0';
    $ARG1 <= COUNTER_$O_RD when $ACC else 0;    
  end;
  #control
  begin
    wait for COUNTER_$O_GD = '0';
  end;
end;


start: #access
begin
  #data
  begin
    COUNTER_$O_START <= COUNTER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for COUNTER_$O_GD = '0';
  end;
end;

stop: #access
begin
  #data
  begin
    COUNTER_$O_STOP <= COUNTER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for COUNTER_$O_GD = '0';
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
    COUNTER_$O_TIME_SET <= COUNTER_$O_GD when $ACC else '0';
    COUNTER_$O_TIME <= index($time,$ARG1) when $ACC else 0;
  end;
  #control
  begin
    wait for COUNTER_$O_GD = '0';
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
  
  signal COUNTER_$O_COUNTER: std_logic_vector[$datawidth];
  signal COUNTER_$O_COUNT: std_logic_vector[$datawidth];
  signal COUNTER_$O_TIMER_COUNTER: std_logic_vector[width((max($time)*$clock)/1000000000)];
  signal COUNTER_$O_TIMER_COUNT: std_logic_vector[width((max($time)*$clock)/1000000000)];
  signal COUNTER_$O_COUNTER_ENABLED: std_logic;
  signal COUNTER_$O_TIMER_ENABLED: std_logic;
  signal COUNTER_$O_TIMER_EVENT: std_logic;
  
  foreach $p in $P.read do
  begin
    signal COUNTER_$O_$p_RE: std_logic;
    signal COUNTER_$O_$p_RD: std_logic_vector[$datawidth];
    signal COUNTER_$O_$p_LOCKed: std_logic;
  end;
  foreach $p in $P.init do
  begin
    signal COUNTER_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P.time do
  begin
    signal COUNTER_$O_$p_TIME_SET: std_logic;
    signal COUNTER_$O_$p_TIME: std_logic_vector[index_width($time)];
  end;
  foreach $p in $P.start do
  begin
    signal COUNTER_$O_$p_START: std_logic;
  end;
  foreach $p in $P.stop do
  begin
    signal COUNTER_$O_$p_STOP: std_logic;
  end;
  foreach $p in $P do
  begin
    signal COUNTER_$O_$p_GD: std_logic;    
  end;
end;

#signals ($mode = "external")
begin
  signal COUNTER_$O_COUNTER_EXTERNAL: std_logic;
  signal COUNTER_$O_COUNTER_EXTERNAL_IO: std_logic;
end;

--
-- Timer generator
--
COUNTER_$O_TIMER_GEN: #process
begin
  if $CLK then
  begin
    COUNTER_$O_TIMER_EVENT <= '0';
    if $RES then
    begin
      COUNTER_$O_TIMER_COUNTER <= to_logic(0,width((max($time)*$clock)/1000000000));
    end
    else
    begin
      if COUNTER_$O_TIMER_ENABLED = '1' then
      begin
        if COUNTER_$O_TIMER_COUNTER = to_logic(0,width((max($time)*$clock)/1000000000)) then
        begin
          COUNTER_$O_TIMER_EVENT <= '1';
          COUNTER_$O_TIMER_COUNTER <= COUNTER_$O_TIMER_COUNT;
        end
        else
        begin
          COUNTER_$O_TIMER_COUNTER <= COUNTER_$O_TIMER_COUNTER - 1;
        end;
      end;
    end;  
  end;
end;
--
-- Counter acquisition control (start/stop)
--
COUNTER_$O_ACQ: #process ($mode = "internal")
begin
  if $CLK then
  begin
    if $RES then
    begin
      COUNTER_$O_COUNTER <= to_logic(0,$datawidth);
      COUNTER_$O_COUNT <= to_logic(0,$datawidth);  
    end
    else
    begin
      if COUNTER_$O_COUNTER_ENABLED = '1' and COUNTER_$O_TIMER_EVENT = '1' then
      begin
        COUNTER_$O_COUNTER <= COUNTER_$O_COUNTER + 1;
      end
      else
      begin
        COUNTER_$O_COUNT <= COUNTER_$O_COUNTER;
      end;
    end;
  end;     
end;

COUNTER_$O_ACQ: #process ($mode = "external")
begin
  if $CLK then
  begin
    if $RES then
    begin
      COUNTER_$O_COUNTER <= to_logic(0,$datawidth);
      COUNTER_$O_COUNT <= to_logic(0,$datawidth);  
    end
    else
    begin
      if COUNTER_$O_COUNTER_EXTERNAL = '1' and COUNTER_$O_TIMER_EVENT = '1' then
      begin
        COUNTER_$O_COUNTER <= COUNTER_$O_COUNTER + 1;
      end
      else if COUNTER_$O_COUNTER_EXTERNAL = '0' and COUNTER_$O_COUNTER /= to_logic(0,$datawidth) then
      begin
        COUNTER_$O_COUNT <= COUNTER_$O_COUNTER;
        COUNTER_$O_COUNTER <= to_logic(0,$datawidth);
      end;
    end;
  end;     
end;

COUNTER_$O_SAMPLER: #process ($mode = "external")
begin
  variable sample: logic[4];
  if $CLK then
  begin
    if $RES then
    begin
      sample := to_logic(0,4);
    end
    else
    begin
      if COUNTER_$O_COUNTER_EXTERNAL_IO = to_logic($sig_action_level,1) then
      begin
        if sample < to_logic(15,4) then
        begin
          sample := sample + 1;
        end;
      end
      else
      begin
        if sample > to_logic(0,4) then
        begin
          sample := sample - 1;
        end;
      end;
      --
      -- Sampler above threshold?
      --
      if sample > to_logic(10,4) then COUNTER_$O_COUNTER_EXTERNAL <= '1' 
      else COUNTER_$O_COUNTER_EXTERNAL <= '0';
    end;  
  end;
end;

#top ($mode = "external")
begin
  COUNTER_$O_COUNTER_EXTERNAL_IO <= $sig;
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
COUNTER_$O_SCHED: #process 
begin
  if $CLK then
  begin
    if $RES then
    begin
      COUNTER_$O_COUNTER_ENABLED <= '0';
      COUNTER_$O_TIMER_ENABLED <= '0';
      COUNTER_$O_TIMER_COUNT <= to_logic((nth($time,1)*$clock)/1000000000,
                                        width((max($time)*$clock)/1000000000));
      
      foreach $p in $P do
      begin
        COUNTER_$O_$p_GD <= '1';
      end;
      foreach $p in $P.await do
      begin
        COUNTER_$O_$p_LOCKed <= '0';
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        COUNTER_$O_$p_GD <= '1';
      end;
    
      
      sequence
      begin
        foreach $p in $P.init do
        begin
          if COUNTER_$O_$p_INIT = '1' then
          begin
            COUNTER_$O_TIMER_COUNT <= to_logic((nth($time,1)*$clock)/1000000000,
                                               width((max($time)*$clock)/1000000000));            
            COUNTER_$O_COUNTER_ENABLED <= '0';
            COUNTER_$O_TIMER_ENABLED <= '0';
            
            COUNTER_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.start do
        begin
          if COUNTER_$O_$p_START = '1'  then
          begin
            COUNTER_$O_TIMER_ENABLED <= '1';
            COUNTER_$O_COUNTER_ENABLED <= '1';
            COUNTER_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.stop do
        begin
          if COUNTER_$O_$p_STOP = '1'  then
          begin
            COUNTER_$O_TIMER_ENABLED <= '0';
            COUNTER_$O_COUNTER_ENABLED <= '0';
            COUNTER_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.read do
        begin
          if COUNTER_$O_$p_RE = '1'  then
          begin
            COUNTER_$O_$p_RD <= COUNTER_$O_COUNT;
            COUNTER_$O_$p_GD <= '0';
          end;
        end;
        foreach $p in $P.time do
        begin
          if COUNTER_$O_$p_TIME_SET = '1'  then
          begin
            COUNTER_$O_$p_GD <= '0';
            sequence
            begin
              foreach $this_time in $time do
              begin
                if COUNTER_$O_$p_TIME = index($time,$this_time) then
                  COUNTER_$O_TIMER_COUNT <= to_logic(($this_time*$clock)/1000000000,
                                                      width((max($time)*$clock)/1000000000));
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

