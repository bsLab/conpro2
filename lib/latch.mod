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
--    $CREATED:     31.3.2009
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements latch object
--
--
--    $ENDOFINFO
--

#version "2.03";

--
-- parameters used, allowed and default values
--
#parameter
begin
  $set_act[0,1] <= 1;
  $set_sig;
  $ARRAY [0,1]<= 0;
end;

--
-- Supported object methods
--
#methods
begin
  init();
  set(#rhs:logic);
  reset();
  read(#lhs:logic);
  set_act(#rhs:nat);
end;

#assert
begin
  size($P.init) >= 1;
  size($P.set) >= 1; 
  size($P.read) >= 1;
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal LATCH_$O_INIT: out std_logic;
  end;
  foreach $p in $P.reset do
  begin
    signal LATCH_$O_RESET: out std_logic;
  end;
  foreach $p in $P.read do
  begin
    signal LATCH_$O_RE: out std_logic;
    signal LATCH_$O_RD: in std_logic;
  end;
  foreach $p in $P.read or $P.init or $P.reset do
  begin
    signal LATCH_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    LATCH_$O_INIT => LATCH_$O_$p_INIT;
  end;
  foreach $p in $P.reset do
  begin
    LATCH_$O_RESET => LATCH_$O_$p_RESET;
  end;
  foreach $p in $P.read do
  begin
    LATCH_$O_RE => LATCH_$O_$p_RE;
    LATCH_$O_RD => LATCH_$O_$p_RD;
  end;
  foreach $p in $P.read or $P.init or $P.reset do
  begin
    LATCH_$O_GD => LATCH_$O_$p_GD;
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
    LATCH_$O_INIT <= LATCH_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for LATCH_$O_GD = '0';
  end;
end;

reset: #access
begin
  #data
  begin
    LATCH_$O_RESET <= LATCH_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for LATCH_$O_GD = '0';
  end;
end;

set: #access
begin
  #set
  begin
    $set_sig <= $ARG1;
  end;
end;

set_act: #access
begin
  #set
  begin
    $set_act <= $ARG1;
  end;
end;

read: #access
begin
  #data
  begin
    LATCH_$O_RE <= LATCH_$O_GD when $ACC else '0';
    $ARG1 <= LATCH_$O_RD when $ACC else 0;
  end;
  #control
  begin
    wait for LATCH_$O_GD = '0';
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

  foreach $p in $P.read do
  begin
    signal LATCH_$O_$p_RD: std_logic;
    signal LATCH_$O_$p_RE: std_logic;
  end;
  foreach $p in $P.reset do
  begin
    signal LATCH_$O_$p_reset: std_logic;
  end;
  foreach $p in $P.init do
  begin
    signal LATCH_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P.read or $P.reset or $P.init do
  begin
    signal LATCH_$O_$p_GD: std_logic;    
  end;
  signal LATCH_$O_LATCH_S: std_logic;
  signal LATCH_$O_LATCH_R: std_logic;
  signal LATCH_$O_LATCH_RS: std_logic;
  
end;

LATCH_$O_LATCH: #process
begin
  if LATCH_$O_LATCH_S = to_logic($set_act,1) then
    LATCH_$O_LATCH_RS <= '1'
  else if LATCH_$O_LATCH_R = '1' then
    LATCH_$O_LATCH_RS <= '0';
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
LATCH_$O_SCHED: #process 
begin
  if $CLK then
  begin
    if $RES then
    begin
      foreach $p in $P.read or $P.reset or $P.init do
      begin
        LATCH_$O_$p_GD <= '1';
      end;
      LATCH_$O_LATCH_R <= '0';
    end
    else
    begin
      foreach $p in $P.read or $P.reset or $P.init do
      begin
        LATCH_$O_$p_GD <= '1';
      end;
      LATCH_$O_LATCH_R <= '0';

      sequence
      begin
        foreach $p in $P.init do
        begin
          if LATCH_$O_$p_INIT = '1' then
          begin
            LATCH_$O_$p_GD <= '0';
            LATCH_$O_LATCH_R <= '1';
          end;
        end;
        foreach $p in $P.reset do
        begin
          if LATCH_$O_$p_RESET = '1' then
          begin
            LATCH_$O_$p_GD <= '0';
            LATCH_$O_LATCH_R <= '1';
          end;
        end;
        foreach $p in $P.read do
        begin
          if LATCH_$O_$p_RE = '1' then
          begin
            LATCH_$O_$p_GD <= '0';
            LATCH_$O_$p_RD <= LATCH_$O_LATCH_RS;
            LATCH_$O_LATCH_R <= '1';
          end;
        end;
      end;
    end;
  end;
end;

#top
begin
  LATCH_$O_LATCH_S <= $set_sig;
end;
