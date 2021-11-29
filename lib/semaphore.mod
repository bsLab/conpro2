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
--    $CREATED:     9.12.2008
--    $VERSION:     2.02
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements semaphore object
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
  $NAME <= "SEMA";
  --
  -- 1: static priority scheduling
  -- 2: dynamic fifo scheduling
  --
  $scheduler["static","fifo"] <= "static";
  $arch001[1,2] <= 2;
  $depth[4 to 16] <= 8;
  $init <= 0;
  
  $ARRAY [0,1]<= 0;
end;

--
-- Supported object methods
--
#methods
begin
  init(#rhs:logic[$depth]);
  down();
  up();
  read();
  write();
end;

#assert
begin
  size($P.up) >= 1;
  size($P.down) >= 1; 
  size($P.init) >= 1;
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal SEMA_$O_INIT: out std_logic;
  end;
  foreach $p in $P.read do
  begin
    signal SEMA_$O_RE: out std_logic;
    signal SEMA_$O_RD: in std_logic_vector[$depth];
  end;
  foreach $p in $P.write do
  begin
    signal SEMA_$O_WE: out std_logic;
  end;
  foreach $p in $P.write or $P.init do
  begin
    signal SEMA_$O_WR: out std_logic_vector[$depth];  
  end;
  foreach $p in $P.down do
  begin
    signal SEMA_$O_DOWN: out std_logic;
  end;
  foreach $p in $P.up do
  begin
    signal SEMA_$O_UP: out std_logic;
  end;
  foreach $p in $P do
  begin
    signal SEMA_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    SEMA_$O_INIT => SEMA_$O_$p_INIT;
  end;
  foreach $p in $P.read do
  begin
    SEMA_$O_RE => SEMA_$O_$p_RE;
    SEMA_$O_RD => SEMA_$O_$p_RD;
  end;
  foreach $p in $P.write do
  begin
    SEMA_$O_WE => SEMA_$O_$p_WE;
  end;
  foreach $p in $P.write or $P.init do
  begin
    SEMA_$O_WR => SEMA_$O_$p_WR;
  end;
  foreach $p in $P.down do
  begin
    SEMA_$O_DOWN => SEMA_$O_$p_DOWN;
  end;
  foreach $p in $P.up do
  begin
    SEMA_$O_UP => SEMA_$O_$p_UP;
  end;
  foreach $p in $P do
  begin
    SEMA_$O_GD => SEMA_$O_$p_GD;
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
    SEMA_$O_INIT <= SEMA_$O_GD when $ACC else '0';
    SEMA_$O_WR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for SEMA_$O_GD = '0';
  end;
end;

read: #access
begin
  #data
  begin
    SEMA_$O_RE <= SEMA_$O_GD when $ACC else '0';
    $arg1 <= SEMA_$O_RD when $ACC else 0;
  end;
  #control
  begin
    wait for SEMA_$O_GD = '0';
  end;
end;

write: #access
begin
  #data
  begin
    SEMA_$O_WE <= SEMA_$O_GD when $ACC else '0';
    SEMA_$O_WR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for SEMA_$O_GD = '0';
  end;
end;

down: #access
begin
  #data
  begin
    SEMA_$O_DOWN <= SEMA_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for SEMA_$O_GD = '0';
  end;
end;


up: #access
begin
  #data
  begin
    SEMA_$O_UP <= SEMA_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for SEMA_$O_GD = '0';
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

  foreach $p in $P.down do
  begin
    signal SEMA_$O_$p_DOWN: std_logic;
  end;
  foreach $p in $P.up do
  begin
    signal SEMA_$O_$p_UP: std_logic;
  end;
  foreach $p in $P.init do
  begin
    signal SEMA_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P.write or $P.init do
  begin
    signal SEMA_$O_$p_WR: std_logic_vector[$depth];    
  end;
  foreach $p in $P do
  begin
    signal SEMA_$O_$p_GD: std_logic;    
  end;
end;

#signals ($scheduler="static")
begin
  signal SEMA_$O_LEVEL: std_logic_vector[$depth];
end;

#signals ($scheduler="fifo")
begin
  signal SEMA_$O_LEVEL: std_logic_vector[$depth];
  foreach $p in $P do
  begin
    signal SEMA_$O_$p_QUEUED: std_logic;
    signal SEMA_$O_$p_PRIO: std_logic_vector[index_width($P)];
  end;
  signal SEMA_$O_HEAD: std_logic_vector[index_width($P)];
  signal SEMA_$O_TAIL: std_logic_vector[index_width($P)];
  signal SEMA_$O_OWNER: std_logic_vector[index_width($P)];
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
SEMA_$O_SCHED: #process ($scheduler="static")
begin
  if $CLK then
  begin
    if $RES then
    begin
      SEMA_$O_LEVEL <= to_logic($init,$depth);
      foreach $p in $P do
      begin
        SEMA_$O_$p_GD <= '1';
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        SEMA_$O_$p_GD <= '1';
      end;
      sequence
      begin
        foreach $p in $P.init do
        begin
          if SEMA_$O_$p_INIT = '1' then
          begin
            SEMA_$O_$p_GD <= '0';
            SEMA_$O_LEVEL <= SEMA_$O_$p_WR;
          end;
        end;
        foreach $p in $P.down do
        begin
          if SEMA_$O_$p_DOWN = '1' and not (SEMA_$O_LEVEL = to_logic(0,$depth))  then
          begin
            SEMA_$O_$p_GD <= '0';
            SEMA_$O_LEVEL <= SEMA_$O_LEVEL - 1;            
          end;
        end;
        if $arch001 = 1 then
        begin
          foreach $p in $P.up do
          begin
            if SEMA_$O_$p_UP = '1' then
            begin
              SEMA_$O_$p_GD <= '0';
              SEMA_$O_LEVEL <= SEMA_$O_LEVEL + 1;
            end;
          end;
        end;
        if $arch001 = 2 then
        begin
          if expand($P.up,$p,or,SEMA_$O_$p_UP = '1') then
          begin
            SEMA_$O_LEVEL <= SEMA_$O_LEVEL + 1;
            sequence
            begin
              foreach $pl in $P.up do
              begin
                if SEMA_$O_$pl_UP = '1' then
                begin
                  SEMA_$O_$pl_GD <= '0';
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

SEMA_$O_SCHED: #process ($scheduler="fifo")
begin
  if $CLK then
  begin
    if $RES then
    begin
      SEMA_$O_LEVEL <= to_logic($init,$depth);
      foreach $p in $P do
      begin
        SEMA_$O_$p_GD <= '1';
        SEMA_$O_$p_QUEUED <= '0';
        SEMA_$O_$p_PRIO <= to_logic(0,index_width($P));
      end;
      SEMA_$O_HEAD <= to_logic(0,index_width($P));
      SEMA_$O_TAIL <= to_logic(0,index_width($P));
      SEMA_$O_OWNER <= to_logic(0,index_width($P));
    end
    else
    begin
      foreach $p in $P do
      begin
        SEMA_$O_$p_GD <= '1';
      end;
      sequence
      begin
        foreach $p in $P.init do
        begin
          if SEMA_$O_$p_INIT = '1' then
          begin
            SEMA_$O_LEVEL <= SEMA_$O_$p_WR;
            SEMA_$O_$p_GD <= '0';
            foreach $pa in $P do
            begin
              SEMA_$O_$pa_QUEUED <= '0';
              SEMA_$O_$pa_PRIO <= to_logic(0,index_width($P));
            end;
            SEMA_$O_HEAD <= to_logic(0,index_width($P));
            SEMA_$O_TAIL <= to_logic(0,index_width($P));
            SEMA_$O_OWNER <= to_logic(0,index_width($P));
          end;
        end;
        foreach $p in $P.down do
        begin
          if SEMA_$O_$p_DOWN = '1' and not (SEMA_$O_LEVEL = to_logic(0,$depth)) then
          begin
            SEMA_$O_$p_GD <= '0';
            SEMA_$O_LEVEL <= SEMA_$O_LEVEL - 1;
            SEMA_$O_OWNER <= to_logic(index($P,$p)+1,index_width($P));
          end;
        end;
        foreach $p in $P.down do
        begin
          if SEMA_$O_$p_DOWN = '1' and SEMA_$O_$p_QUEUED = '0' and SEMA_$O_OWNER /= to_logic(index($P,$p)+1,index_width($P)) then
          begin
            SEMA_$O_$p_PRIO <= SEMA_$O_HEAD + 1;
            SEMA_$O_HEAD <= SEMA_$O_HEAD + 1;
            SEMA_$O_$p_QUEUED <= '1';
          end;
        end;
        foreach $p in $P.down do
        begin
          if SEMA_$O_$p_DOWN = '1' and SEMA_$O_$p_QUEUED = '1' and SEMA_$O_$p_PRIO = SEMA_$O_TAIL then
          begin
            SEMA_$O_$p_GD <= '0';
            SEMA_$O_$p_QUEUED <= '0';
            SEMA_$O_OWNER <= to_logic(index($P,$p)+1,index_width($P));
          end;
        end;
        if $arch001 = 1 then
        begin
          foreach $p in $P.up do
          begin
            if SEMA_$O_$p_UP = '1' then
            begin
              SEMA_$O_$p_GD <= '0';
              if SEMA_$O_HEAD = SEMA_$O_TAIL then
              begin
                SEMA_$O_OWNER <= to_logic(0,index_width($P));
                SEMA_$O_LEVEL <= SEMA_$O_LEVEL + 1;              
              end
              else
              begin
                SEMA_$O_TAIL <= SEMA_$O_TAIL + 1;
              end;
            end;
          end;
        end;
        if $arch001 = 2 then
        begin
          if expand($P.up,$p,or,SEMA_$O_$p_UP = '1') then
          begin
            sequence
            begin
              foreach $pl in $P.up do
              begin
                if SEMA_$O_$pl_UP = '1' then
                begin
                  SEMA_$O_$pl_GD <= '0';
                end;
              end;
            end;
            if SEMA_$O_HEAD = SEMA_$O_TAIL then
            begin
              SEMA_$O_OWNER <= to_logic(0,index_width($P));
              SEMA_$O_LEVEL <= SEMA_$O_LEVEL + 1;              
            end
            else
            begin
              SEMA_$O_TAIL <= SEMA_$O_TAIL + 1;
            end;
          end;
        end;
      end; 
    end;
  end;
end;
