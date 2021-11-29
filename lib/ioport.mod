
#version "1.9";

#parameter
begin
  $NAME <= "IOPORT";
  --
  -- Bit datawidth of port
  --
  $datawidth[1 to 64] <= 8;
  --
  -- Direction bit mask: 0=IN,1=OUT
  --
  $dir <= 0;
  $port_db;

  $fsm["moore","mealy"] <= "moore";

  $ARRAY [0,1]<= 0;
end;



-------------------------------------------------
-- ConPro Process Interface to Access Scheduler
-------------------------------------------------


--************************
-- Method Declaration
--************************

#methods
begin
  init();
  read(#lhs:logic[$datawidth]);
  write(#rhs:logic[$datawidth]);
  --
  -- Direction bit mask: 0=IN,1=OUT
  --
  dir(#rhs:logic[$datawidth]);
  interface(#lrhs:logic[$datawidth]);
end;

#assert
begin
  size($P.read) >= 1 or size($P.write) >= 1;
  size($P.init) >= 1;
end;

--************************
-- Process Port Interface
--************************

#interface
begin
  foreach $p in $P.init do
  begin
    signal PORT_$O_INIT: out std_logic;
  end;
  foreach $p in $P.read do
  begin
    signal PORT_$O_RE: out std_logic;
    signal PORT_$O_RD: in std_logic_vector[$datawidth];
  end;
  foreach $p in $P.write do
  begin
    signal PORT_$O_WE: out std_logic;
    signal PORT_$O_WR: out std_logic_vector[$datawidth];  
  end;
  foreach $p in $P.dir do
  begin
    signal PORT_$O_DE: out std_logic;
    signal PORT_$O_DR: out std_logic_vector[$datawidth];  
  end;
  foreach $p in $P do
  begin
    signal PORT_$O_GD: in std_logic;
  end;
end;

#mapping
begin
  foreach $p in $P.init do
  begin
    PORT_$O_INIT => PORT_$O_$p_INIT;
  end;
  foreach $p in $P.read do
  begin
    PORT_$O_RE => PORT_$O_$p_RE;
    PORT_$O_RD => PORT_$O_$p_RD;
  end;
  foreach $p in $P.write do
  begin
    PORT_$O_WE => PORT_$O_$p_WE;
    PORT_$O_WR => PORT_$O_$p_WR;
  end;
  foreach $p in $P.dir do
  begin
    PORT_$O_DE => PORT_$O_$p_DE;
    PORT_$O_DR => PORT_$O_$p_DR;
  end;
  foreach $p in $P do
  begin
    PORT_$O_GD => PORT_$O_$p_GD;
  end;
end;

--************************
-- Method Access
--************************

init: #access ($fsm="moore")
begin
  #data
  begin
    PORT_$O_INIT <= PORT_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for PORT_$O_GD = '0';
  end;
end;

read: #access ($fsm="moore")
begin
  #data
  begin
    PORT_$O_RE <= PORT_$O_GD when $ACC else '0';
    $ARG1 <= PORT_$O_RD when $ACC else 0;
  end;
  #control
  begin
    wait for PORT_$O_GD = '0';
  end;
end;

write: #access ($fsm="moore")
begin
  #data
  begin
    PORT_$O_WE <= PORT_$O_GD when $ACC else '0';
    PORT_$O_WR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for PORT_$O_GD = '0';
  end;
end;

dir: #access ($fsm="moore")
begin
  #data
  begin
    PORT_$O_DE <= PORT_$O_GD when $ACC else '0';
    PORT_$O_DR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for PORT_$O_GD = '0';
  end;
end;

init: #access ($fsm="mealy")
begin
  #data
  begin
    PORT_$O_INIT <= '1' when $ACC else '0';
  end;
  #control
  begin
    wait for PORT_$O_GD = '0';
  end;
end;

read: #access ($fsm="mealy")
begin
  #data
  begin
    PORT_$O_RE <= '1' when $ACC else '0';
    $ARG1 <= PORT_$O_RD when $ACC else 0;
  end;
  #control
  begin
    wait for PORT_$O_GD = '0';
  end;
end;

write: #access ($fsm="mealy")
begin
  #data
  begin
    PORT_$O_WE <= '1' when $ACC else '0';
    PORT_$O_WR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for PORT_$O_GD = '0';
  end;
end;

dir: #access ($fsm="mealy")
begin
  #data
  begin
    PORT_$O_DE <= '1' when $ACC else '0';
    PORT_$O_DR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for PORT_$O_GD = '0';
  end;
end;

interface: #access
begin
  #set
  begin
    $port_db <= $ARG1;
  end;
end;

--************************
-- Signals required for access
--************************

#signals
begin
  foreach $p in $P.init do
  begin
    signal PORT_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P.read do
  begin
    signal PORT_$O_$p_RE: std_logic;
    signal PORT_$O_$p_RD: std_logic_vector[$datawidth];
  end;
  foreach $p in $P.write do
  begin
    signal PORT_$O_$p_WE: std_logic;
    signal PORT_$O_$p_WR: std_logic_vector[$datawidth];
  end;
  foreach $p in $P.dir do
  begin
    signal PORT_$O_$p_DE: std_logic;
    signal PORT_$O_$p_DR: std_logic_vector[$datawidth];
  end;
  foreach $p in $P do
  begin
    signal PORT_$O_$p_GD: std_logic;    
  end;
end;

---------------------------------------------
-- Access Scheduler Implementation
---------------------------------------------

--#signals
--begin
--end;

PORT_$O_SCHED: #process ($fsm="moore")
begin
  if $CLK then
  begin
    if $RES then
    begin
      PORT_$O_DIR <= to_logic($dir,$datawidth);
      foreach $p in $P do
      begin
        PORT_$O_$p_GD <= '1';
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        PORT_$O_$p_GD <= '1';
      end;
      sequence
      begin
        foreach $p in $P.init do
        begin
          if PORT_$O_$p_INIT = '1' then
          begin
            PORT_$O_$p_GD <= '0';
            PORT_$O_DIR <= to_logic($dir,$datawidth);
          end;
        end;
        foreach $p in $P.read and $P.dir do
        begin
          if PORT_$O_$p_RE = '1' and PORT_$O_$p_DE = '1' then
          begin
            PORT_$O_$p_GD <= '0';
            PORT_$O_$p_RD <= (PORT_$O_IN and (not PORT_$O_DIR)) or (PORT_$O_OUT and PORT_$O_DIR);
            PORT_$O_DIR <= PORT_$O_$p_DR;            
          end;         
        end;
        foreach $p in $P.read do
        begin
          if PORT_$O_$p_RE = '1' then
          begin
            PORT_$O_$p_GD <= '0';
            PORT_$O_$p_RD <= (PORT_$O_IN and (not PORT_$O_DIR)) or (PORT_$O_OUT and PORT_$O_DIR);
          end;         
        end;
        foreach $p in $P.write and $P.dir do
        begin
          if PORT_$O_$p_WE = '1' and PORT_$O_$p_DE = '1' then
          begin
            PORT_$O_$p_GD <= '0';
            PORT_$O_OUT <= PORT_$O_$p_WR;            
            PORT_$O_DIR <= PORT_$O_$p_DR;            
          end;
        end;
        foreach $p in $P.write do
        begin
          if PORT_$O_$p_WE = '1' then
          begin
            PORT_$O_$p_GD <= '0';
            PORT_$O_OUT <= PORT_$O_$p_WR;            
          end;
        end;
        foreach $p in $P.dir do
        begin
          if PORT_$O_$p_DE = '1' then
          begin
            PORT_$O_$p_GD <= '0';
            PORT_$O_DIR <= PORT_$O_$p_DR;            
          end;
        end;
      end;
    end;
  end;
end;

PORT_$O_SCHED: #process ($fsm="mealy")
begin
  foreach $p in $P do
  begin
    PORT_$O_$p_GD <= '1';
  end;
  foreach $p in $P.read do
  begin
    PORT_$O_$p_RD <= to_logic(0,$datawidth);
  end;
  sequence
  begin
    foreach $p in $P.init do
    begin
      if PORT_$O_$p_INIT = '1' then
      begin
        PORT_$O_$p_GD <= '0';
      end;
    end;
    foreach $p in $P.read and $P.dir do
    begin
      if PORT_$O_$p_RE = '1' and PORT_$O_$p_DE = '1' then
      begin
        PORT_$O_$p_GD <= '0';
        PORT_$O_$p_RD <= (PORT_$O_IN and (not PORT_$O_DIR)) or (PORT_$O_OUT and PORT_$O_DIR);
      end;         
    end;
    foreach $p in $P.read do
    begin
      if PORT_$O_$p_RE = '1' then
      begin
        PORT_$O_$p_GD <= '0';
        PORT_$O_$p_RD <= (PORT_$O_IN and (not PORT_$O_DIR)) or (PORT_$O_OUT and PORT_$O_DIR);
      end;         
    end;
    foreach $p in $P.write and $P.dir do
    begin
      if PORT_$O_$p_WE = '1' and PORT_$O_$p_DE = '1' then
      begin
        PORT_$O_$p_GD <= '0';
      end;
    end;
    foreach $p in $P.write do
    begin
      if PORT_$O_$p_WE = '1' then
      begin
        PORT_$O_$p_GD <= '0';
      end;
    end;
    foreach $p in $P.dir do
    begin
      if PORT_$O_$p_DE = '1' then
      begin
        PORT_$O_$p_GD <= '0';
      end;
    end;
  end;

  if $CLK then
  begin
    if $RES then
    begin
      PORT_$O_DIR <= to_logic($dir,$datawidth);
    end
    else
    begin
      sequence
      begin
        foreach $p in $P.init do
        begin
          if PORT_$O_$p_INIT = '1' then
          begin
            PORT_$O_DIR <= to_logic($dir,$datawidth);
          end;
        end;
        foreach $p in $P.read and $P.dir do
        begin
          if PORT_$O_$p_RE = '1' and PORT_$O_$p_DE = '1' then
          begin
            PORT_$O_DIR <= PORT_$O_$p_DR;            
          end;         
        end;
        foreach $p in $P.write and $P.dir do
        begin
          if PORT_$O_$p_WE = '1' and PORT_$O_$p_DE = '1' then
          begin
            PORT_$O_OUT <= PORT_$O_$p_WR;            
            PORT_$O_DIR <= PORT_$O_$p_DR;            
          end;
        end;
        foreach $p in $P.write do
        begin
          if PORT_$O_$p_WE = '1' then
          begin
            PORT_$O_OUT <= PORT_$O_$p_WR;            
          end;
        end;
        foreach $p in $P.dir do
        begin
          if PORT_$O_$p_DE = '1' then
          begin
            PORT_$O_DIR <= PORT_$O_$p_DR;            
          end;
        end;
      end;
    end;
  end;
end;

------------------------------------
-- Functional Implementation
------------------------------------

#signals
begin
  signal PORT_$O_DIR: std_logic_vector[$datawidth];
  signal PORT_$O_IN: std_logic_vector[$datawidth];
  signal PORT_$O_OUT: std_logic_vector[$datawidth];
end;


PORT_$O_IO: #process
begin
  for i = 0 to $datawidth-1 do
  begin
    if PORT_$O_DIR[i] = '1' then
      $port_db[i] <= PORT_$O_OUT[i]
    else
      $port_db[i] <= 'Z';
  end;
end;

#top
begin
  PORT_$O_IN <= $port_db;
end;
