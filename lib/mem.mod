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
--    $CREATED:     30.1.2009
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements external and internal memory-mapped blocks and devices
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
  $arch001[0,1] <= 0;
  
  $model["singleport","duplex"] <= "singleport";
 
  --
  -- control signals
  --  0: low active, event: 0->1 transition
  --  1: high active, event: 1->0 transition
  --  2: not used
  --
  $ce[0,1,2] <= 2;
  $re[0,1,2] <= 2;
  $we[0,1,2] <= 2;
  $datawidth[1 to 256] <= 8;
  $addrwidth[1 to 256] <= 8;
  
  
  --
  -- load and hold phases, time in clock cycles 
  --
  --                                    |- delayhold          -|
  --                                    |- datahold -|
  --           |- addrload -|- cmdhold -|- addrhold   -|  
  --
  --  ADDR    _LLLLLLLLLLLLLAAAAAAAAAAAAHHHHHHHHHHHHHHHH__________
  --  DATA    _______________________RRRRRRR______________________      
  --  RE      ______________HHHHHHHHHHHHE_________________________
  --
  --  ADDR    _LLLLLLLLLLLLLAAAAAAAAAAAAHHHHHHHHHHHHHHHH__________
  --  DATA    ______________WWWWWWWWWWWWHHHHHHHHHHHHH____________        
  --  WE      ______________HHHHHHHHHHHHE_________________________
  --
  --                                    |- datahold -|
  --
  $addrload[0 to 15] <= 1;
  $dataload[0 to 15] <= 0;
  $addrhold[0 to 15] <= 0;
  $datahold[0 to 15] <= 0;
  $cmdhold[1 to 15] <= 1;
  $delayhold[0 to 15] <= 0;
  
  
  $ram_db;
  $ram_ab;
  $ram_we;
  $ram_re;
  $ram_cs;

  $ARRAY [0,1]<= 0;
end;

--
-- Supported object methods
--
#methods
begin
  --
  -- write(data,addr)
  --
  write(#rhs:logic[$datawidth],#rhs:logic[$addrwidth]);
  --
  -- read(data,addr)
  --
  read(#lhs:logic[$datawidth],#rhs:logic[$addrwidth]);
  --
  -- port(db,ab,re,we,cs)
  --
  interface(#lrhs:logic[$datawidth],#lhs:logic[$addrwidth],#lhs:logic,#lhs:logic,#lhs:logic);
end;

#assert
begin
  size($P.interface) >= 1;
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.read do
  begin
    signal MEM_$O_RE: out std_logic;
    signal MEM_$O_RD: in std_logic_vector[$datawidth];
  end;
  foreach $p in $P.write do
  begin
    signal MEM_$O_WE: out std_logic;
    signal MEM_$O_WR: out std_logic_vector[$datawidth];
  end;
  foreach $p in $P.read or $P.write do
  begin
    signal MEM_$O_ADDR: out std_logic_vector[$addrwidth];
  end;
  foreach $p in $P do
  begin
    signal MEM_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.read do
  begin
    MEM_$O_RE => MEM_$O_$p_RE;
    MEM_$O_RD => MEM_$O_$p_RD;
  end;
  foreach $p in $P.write do
  begin
    MEM_$O_WE => MEM_$O_$p_WE;
    MEM_$O_WR => MEM_$O_$p_WR;
  end;
  foreach $p in $P.write or $P.read do
  begin
    MEM_$O_ADDR => MEM_$O_$p_ADDR;  
  end;
  foreach $p in  $P do
  begin
    MEM_$O_GD => MEM_$O_$p_GD;
  end;
end;

--
-- Object method access (local process context)
-- for each method ...
--

read: #access
begin
  #data
  begin
    MEM_$O_RE <= MEM_$O_GD when $ACC else '0';
    $ARG1 <= MEM_$O_RD when $ACC else 0;
    MEM_$O_ADDR <=  $ARG2 when $ACC else 0;
  end;
  #control
  begin
    wait for MEM_$O_GD = '0';
  end;
end;

write: #access
begin
  #data
  begin
    MEM_$O_WE <= MEM_$O_GD when $ACC else '0';
    MEM_$O_WR <= $ARG1 when $ACC else 0;
    MEM_$O_ADDR <=  $ARG2 when $ACC else 0;
  end;
  #control
  begin
    wait for MEM_$O_GD = '0';
  end;
end;


interface: #access
begin
  #set
  begin
    $ram_db <= $ARG1;
    $ram_ab <= $ARG2;
    $ram_re <= $ARG3;
    $ram_we <= $ARG4;
    $ram_cs <= $ARG5;
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
    signal MEM_$O_$p_RE: std_logic;
    signal MEM_$O_$p_RD: std_logic_vector[$datawidth];
  end;
  foreach $p in $P.write do
  begin
    signal MEM_$O_$p_WE: std_logic;
    signal MEM_$O_$p_WR: std_logic_vector[$datawidth];
  end;
  foreach $p in $P do
  begin
    signal MEM_$O_$p_ADDR: std_logic_vector[$addrwidth];
    signal MEM_$O_$p_GD: std_logic;    
    signal MEM_$O_$p_LOCKED: std_logic;
  end;
end;


--
-- Bus control signals
--
#signals ($re = 0 or $re = 1)
begin
  signal MEM_$O_RE: std_logic;
end;
#signals ($we = 0 or $we = 1)
begin
  signal MEM_$O_WE: std_logic;
end;
#signals ($ce = 0 or $ce = 1)
begin
  signal MEM_$O_CE: std_logic;
end;



--
-- 3-process-FSM implementation
--

#signals($model="singleport")
begin
  type MEM_$O_STATES is {
    MEM_$O_S_REQ,
    MEM_$O_S_READ_LOAD,
    MEM_$O_S_WRITE_LOAD,
    MEM_$O_S_READ,
    MEM_$O_S_WRITE,
    MEM_$O_S_READ_HOLD,    
    MEM_$O_S_WRITE_HOLD
  };
  signal MEM_$O_STATE: MEM_$O_STATES;
  signal MEM_$O_NEXT_STATE: MEM_$O_STATES;
 
  signal MEM_$O_WRITE_ACT: logic;
  signal MEM_$O_ADDR_ACT: logic;
  signal MEM_$O_ADDR_WR: std_logic_vector[$addrwidth];
  signal MEM_$O_ADDR_WE: std_logic;
  signal MEM_$O_DATA_WR: std_logic_vector[$datawidth];
  signal MEM_$O_DATA_WE: std_logic;
  signal MEM_$O_LOCK_WE: logic;

  foreach $p in $P do
  begin
    signal MEM_$O_$p_LOCK_WR: std_logic;
  end;
  signal MEM_$O_WAIT_WE: logic;
  signal MEM_$O_WAIT_WR: logic[4];
  signal MEM_$O_WAIT: logic[4];

  --
  -- Bus signals
  --
  signal MEM_$O_DB_RD: std_logic_vector[$datawidth];
  signal MEM_$O_DB_WR: std_logic_vector[$datawidth];
  signal MEM_$O_AB_WR: std_logic_vector[$addrwidth];
  
end;

MEM_$O_SCHED_REG:#process($model="singleport")
begin
  if $CLK then
  begin
    if $RES then
    begin
      MEM_$O_STATE <= MEM_$O_S_REQ;
      foreach $p in $P do
        MEM_$O_$p_LOCKED <= '0';
      MEM_$O_WAIT <= to_logic(0,4);
    end
    else
    begin
      MEM_$O_STATE <= MEM_$O_NEXT_STATE;
      if MEM_$O_ADDR_WE = '1' then
        MEM_$O_AB_WR <= MEM_$O_ADDR_WR;
      if MEM_$O_DATA_WE = '1' then
        MEM_$O_DB_WR <= MEM_$O_DATA_WR;
      if MEM_$O_LOCK_WE = '1' then
      begin
        foreach $p in $P do
          MEM_$O_$p_LOCKED <= MEM_$O_$p_LOCK_WR;
      end;
      if MEM_$O_WAIT_WE = '1' then
        MEM_$O_WAIT <= MEM_$O_WAIT_WR;
    end;
  end;
end;

MEM_$O_CONTROL:#process($model="singleport")
begin
  MEM_$O_WRITE_ACT <= '0';
  MEM_$O_ADDR_ACT <= '0';
  
  if $ce = 0 then
    MEM_$O_CE <= '1';
  if $ce = 1 then
    MEM_$O_CE <= '0';
  if $re = 0 then
    MEM_$O_RE <= '1';
  if $re = 1 then
    MEM_$O_RE <= '0';
  if $we = 0 then
    MEM_$O_WE <= '1';
  if $we = 1 then
    MEM_$O_WE <= '0';

  case MEM_$O_STATE is
  begin
    when MEM_$O_S_REQ => 
    begin
      null;
    end;
    when MEM_$O_S_READ_LOAD =>
    begin
     if $ce = 0 then
        MEM_$O_CE <= '0';
     if $ce = 1 then
        MEM_$O_CE <= '1';
      MEM_$O_ADDR_ACT <= '1';
    end;
    when MEM_$O_S_WRITE_LOAD =>
    begin
     if $ce = 0 then
        MEM_$O_CE <= '0';
     if $ce = 1 then
        MEM_$O_CE <= '1';
      if $addrload /= 0 then
      begin
        if MEM_$O_WAIT < to_logic($addrload,4) then
          MEM_$O_ADDR_ACT <= '1';
      end;
      if $dataload /= 0 then
      begin
        if MEM_$O_WAIT < to_logic($dataload,4) then
          MEM_$O_WRITE_ACT <= '1';
      end;
    end;
    when MEM_$O_S_READ => 
    begin
      MEM_$O_ADDR_ACT <= '1';
      if $ce = 0 then
        MEM_$O_CE <= '0';
      if $ce = 1 then
        MEM_$O_CE <= '1';
      if $re = 0 then
        MEM_$O_RE <= '0';
      if $re = 1 then
        MEM_$O_RE <= '1';
    end;
    when MEM_$O_S_WRITE =>
    begin
      MEM_$O_WRITE_ACT <= '1';
      MEM_$O_ADDR_ACT <= '1';
      if $ce = 0 then
        MEM_$O_CE <= '0';
      if $ce = 1 then
        MEM_$O_CE <= '1';
      if $we = 0 then
        MEM_$O_WE <= '0';
      if $we = 1 then
        MEM_$O_WE <= '1';
    end;
    when MEM_$O_S_READ_HOLD =>
    begin
     if $ce = 0 then
        MEM_$O_CE <= '0';
     if $ce = 1 then
        MEM_$O_CE <= '1';
      MEM_$O_ADDR_ACT <= '1';
    end;
    when MEM_$O_S_WRITE_HOLD =>
    begin
     if $ce = 0 then
        MEM_$O_CE <= '0';
     if $ce = 1 then
        MEM_$O_CE <= '1';
      if $addrhold /= 0 then
      begin
        if MEM_$O_WAIT < to_logic($addrload,4) then
          MEM_$O_ADDR_ACT <= '1';
      end;
      if $datahold /= 0 then
      begin
        if MEM_$O_WAIT < to_logic($datahold,4) then
          MEM_$O_WRITE_ACT <= '1';
      end;
    end;
  end;

end;

MEM_$O_SCHED_STATE:#process($model="singleport")
begin
  MEM_$O_ADDR_WR <= to_logic(0,$addrwidth);
  MEM_$O_ADDR_WE <= '0';
  MEM_$O_DATA_WR <= to_logic(0,$datawidth);
  MEM_$O_DATA_WE <= '0';
  MEM_$O_WAIT_WR <= to_logic(0,4);  
  MEM_$O_WAIT_WE <= '0';
  MEM_$O_LOCK_WE <= '0';
  foreach $p in $P do
  begin
    MEM_$O_$p_LOCK_WR <= '0';
    MEM_$O_$p_GD <= '1';
  end;    
  foreach $p in $P.read do
  begin
    MEM_$O_$p_RD <= to_logic(0,$datawidth);
  end;
  
  case MEM_$O_STATE is
  begin
  
    when MEM_$O_S_REQ =>
    begin
      MEM_$O_NEXT_STATE <= MEM_$O_S_REQ;
      sequence
      begin
        foreach $p in $P.read do
        begin
          if MEM_$O_$p_RE = '1'  then
          begin
            if $addrload = 0 then
              MEM_$O_NEXT_STATE <= MEM_$O_S_READ
            else
            begin
              MEM_$O_NEXT_STATE <= MEM_$O_S_READ_LOAD;
              MEM_$O_WAIT_WR <= to_logic($addrload-1,4);
              MEM_$O_WAIT_WE <= '1';
            end;
            MEM_$O_ADDR_WR <= MEM_$O_$p_ADDR;
            MEM_$O_ADDR_WE <= '1';
            MEM_$O_$p_LOCK_WR <= '1';
            MEM_$O_LOCK_WE <= '1';
          end;
        end;
        foreach $p in $P.write do
        begin
          if MEM_$O_$p_WE = '1'  then
          begin
            if $addrload = 0 and $dataload = 0 then
              MEM_$O_NEXT_STATE <= MEM_$O_S_WRITE
            else
            begin
              MEM_$O_NEXT_STATE <= MEM_$O_S_WRITE_LOAD;
              MEM_$O_WAIT_WR <= to_logic(max($dataload,$addrload)-1,4);
              MEM_$O_WAIT_WE <= '1';
            end;
            MEM_$O_ADDR_WR <= MEM_$O_$p_ADDR;
            MEM_$O_ADDR_WE <= '1';
            MEM_$O_DATA_WR <= MEM_$O_$p_WR;
            MEM_$O_DATA_WE <= '1';
            MEM_$O_$p_LOCK_WR <= '1';
            MEM_$O_LOCK_WE <= '1';
          end;
        end;
      end;
    end;
    
    when MEM_$O_S_READ_LOAD =>
    begin
      MEM_$O_NEXT_STATE <= MEM_$O_S_READ_LOAD;
      if MEM_$O_WAIT = to_logic(0,4) then
      begin
        MEM_$O_NEXT_STATE <= MEM_$O_S_READ;
        MEM_$O_WAIT_WR <= to_logic(0,4);
        MEM_$O_WAIT_WE <= '1';
      end
      else
      begin
        MEM_$O_WAIT_WR <= MEM_$O_WAIT - 1;
        MEM_$O_WAIT_WE <= '1'; 
      end;      
    end;
    
    when MEM_$O_S_WRITE_LOAD =>
    begin
      MEM_$O_NEXT_STATE <= MEM_$O_S_WRITE_LOAD;
      if MEM_$O_WAIT = to_logic(0,4) then
      begin
        MEM_$O_NEXT_STATE <= MEM_$O_S_WRITE;
        MEM_$O_WAIT_WR <= to_logic(0,4);
        MEM_$O_WAIT_WE <= '1';
      end
      else
      begin
        MEM_$O_WAIT_WR <= MEM_$O_WAIT - 1;
        MEM_$O_WAIT_WE <= '1'; 
      end;      
    end;
    
    when MEM_$O_S_READ =>
    begin
      MEM_$O_NEXT_STATE <= MEM_$O_S_READ;
      if MEM_$O_WAIT = to_logic($cmdhold-1,4) then
      begin
        if $addrhold = 0 and $delayhold = 0 then
          MEM_$O_NEXT_STATE <= MEM_$O_S_REQ
        else 
        begin
          MEM_$O_WAIT_WR <= to_logic(0,4);
          MEM_$O_WAIT_WE <= '1';
          MEM_$O_NEXT_STATE <= MEM_$O_S_READ_HOLD;
        end;
        
        foreach $p in $P.read do
        begin
          if MEM_$O_$p_LOCKED = '1' then
          begin
            MEM_$O_$p_GD <= '0';
            MEM_$O_$p_RD <= MEM_$O_DB_RD;
            MEM_$O_$p_LOCK_WR <= '0';
            MEM_$O_LOCK_WE <= '1';
          end;
        end;        
      end
      else
      begin
        MEM_$O_WAIT_WR <= MEM_$O_WAIT + 1;
        MEM_$O_WAIT_WE <= '1'; 
      end;
    end;
    
    when MEM_$O_S_WRITE =>
    begin
      MEM_$O_NEXT_STATE <= MEM_$O_S_WRITE;
      if MEM_$O_WAIT = to_logic($cmdhold-1,4) then
      begin
        if $addrhold = 0 and $delayhold = 0 and $datahold = 0 then
          MEM_$O_NEXT_STATE <= MEM_$O_S_REQ
        else 
        begin
          MEM_$O_WAIT_WR <= to_logic(0,4);
          MEM_$O_WAIT_WE <= '1';
          MEM_$O_NEXT_STATE <= MEM_$O_S_WRITE_HOLD;
        end;
        foreach $p in $P.write do
        begin
          if MEM_$O_$p_LOCKED = '1' then
          begin
            MEM_$O_$p_GD <= '0';
            MEM_$O_$p_LOCK_WR <= '0';
            MEM_$O_LOCK_WE <= '1';
          end;
        end;        
      end
      else
      begin
        MEM_$O_WAIT_WR <= MEM_$O_WAIT + 1;
        MEM_$O_WAIT_WE <= '1'; 
      end;
    end;
    
    when MEM_$O_S_READ_HOLD =>
    begin
      MEM_$O_NEXT_STATE <= MEM_$O_S_READ_HOLD;
      if MEM_$O_WAIT = to_logic(max($delayhold,$addrhold)-1,4) then
        MEM_$O_NEXT_STATE <= MEM_$O_S_REQ
      else
      begin
        MEM_$O_WAIT_WR <= MEM_$O_WAIT + 1;
        MEM_$O_WAIT_WE <= '1'; 
      end;      
    end;
    
    when MEM_$O_S_WRITE_HOLD =>
    begin
      MEM_$O_NEXT_STATE <= MEM_$O_S_WRITE_HOLD;
      if MEM_$O_WAIT = to_logic(max($delayhold,$addrhold,$datahold)-1,4) then
          MEM_$O_NEXT_STATE <= MEM_$O_S_REQ
      else
      begin
        MEM_$O_WAIT_WR <= MEM_$O_WAIT + 1;
        MEM_$O_WAIT_WE <= '1'; 
      end;      
    end;
  end;
end;


#top($model="singleport")
begin
  MEM_$O_DB_RD <= $ram_db;
  $ram_db <= MEM_$O_DB_WR when MEM_$O_WRITE_ACT = '1' else (others => 'Z');
  $ram_ab <= MEM_$O_AB_WR when MEM_$O_ADDR_ACT = '1' else (others => '0');
  $ram_re <= MEM_$O_RE;
  $ram_we <= MEM_$O_WE;
  $ram_cs <= MEM_$O_CE;
end;
