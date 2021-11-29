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
-- Implements barrier object
--
--
--    $ENDOFINFO
--

#version "2.04";

--
-- parameters used, allowed and default values
--
#parameter
begin
  --
  -- 1: static priority scheduling
  -- 2: dynamic fifo scheduling
  --
  $arch001 <= 1;
  $ARRAY [0,1]<= 0;

end;

--
-- Supported object methods
--
#methods
begin
  init();
  await();
end;

#assert
begin
  size($P.init) >= 1;
  size($P.await) >= 2; 
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal BARRIER_$O_INIT: out std_logic;
  end;
  foreach $p in $P.await do
  begin
    signal BARRIER_$O_AWAIT: out std_logic;
  end;
  foreach $p in $P do
  begin
    signal BARRIER_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    BARRIER_$O_INIT => BARRIER_$O_$p_INIT;
  end;
  foreach $p in $P.await do
  begin
    BARRIER_$O_AWAIT => BARRIER_$O_$p_AWAIT;
  end;
  foreach $p in $P do
  begin
    BARRIER_$O_GD => BARRIER_$O_$p_GD;
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
    BARRIER_$O_INIT <= BARRIER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for BARRIER_$O_GD = '0';
  end;
end;

await: #access
begin
  #data
  begin
    BARRIER_$O_AWAIT <= BARRIER_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for BARRIER_$O_GD = '0';
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
  signal BARRIER_$O_LOCKed: std_logic;
  signal BARRIER_$O_LEVEL: std_logic_vector[index_width($P.await)];
  
  foreach $p in $P.await do
  begin
    signal BARRIER_$O_$p_AWAIT: std_logic;
    signal BARRIER_$O_$p_LOCKed: std_logic;
  end;
  foreach $p in $P.init do
  begin
    signal BARRIER_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P do
  begin
    signal BARRIER_$O_$p_GD: std_logic;    
  end;
end;


--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
BARRIER_$O_SCHED: #process 
begin
  if $CLK then
  begin
    if $RES then
    begin
      BARRIER_$O_LOCKed <= '1';
      BARRIER_$O_LEVEL <= to_logic(size($P.await),index_width($P.await));
      
      foreach $p in $P do
      begin
        BARRIER_$O_$p_GD <= '1';
      end;
      foreach $p in $P.await do
      begin
        BARRIER_$O_$p_LOCKed <= '0';
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        BARRIER_$O_$p_GD <= '1';
      end;
      sequence
      begin
        foreach $p in $P.init do
        begin
          if BARRIER_$O_$p_INIT = '1' then
          begin
            BARRIER_$O_LOCKed <= '1';
            BARRIER_$O_LEVEL <= to_logic(size($P.await),index_width($P.await));
            
            BARRIER_$O_$p_GD <= '0';
            foreach $l in $P.await do
            begin
              if BARRIER_$O_$l_LOCKed = '1' then
              begin
                BARRIER_$O_$l_LOCKed <= '0';
                BARRIER_$O_$l_GD <= '0';
              end;
            end;
          end;
        end;
        foreach $p in $P.await do
        begin
          if BARRIER_$O_$p_AWAIT = '1' and BARRIER_$O_$p_LOCKed  = '0' then
          begin
            BARRIER_$O_$p_LOCKed <= '1';
            BARRIER_$O_LEVEL <= BARRIER_$O_LEVEL - 1;
          end;
        end;
        if others then
        begin
          if BARRIER_$O_LEVEL = to_logic(0,index_width($P.await)) then
          begin
            BARRIER_$O_LEVEL <= to_logic(size($P.await),index_width($P.await));
            BARRIER_$O_LOCKed <= '0';
            foreach $p in $P.await do
            begin
              BARRIER_$O_$p_LOCKed <= '0';
              BARRIER_$O_$p_GD <= '0';
            end;
          end
          else
            BARRIER_$O_LOCKed <= '1';
        end;
      end;
    end;
  end;
end;

