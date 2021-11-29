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
--    $INITIAL:     (C) 2006-2011 BSSLAB
--    $CREATED:     30.1.2009
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements external and internal RAM/ROM memory blocks
--
--
--    $ENDOFINFO
--

#version "2.12";

--
-- parameters used, allowed and default values
--
#parameter
begin
  $class["external","internal"] <= "internal";
  $datawidth[1 to 256] <= 8;
  $addrwidth[1 to 256] <= 8;
 
  --
  -- INTERNAL
  --
  
  $arch["singleport","dualport"] <= "singleport";
  $mode["read-first","write-first","no-change","async-read","sync-read","sync-read2-write1"] <= "read-first";
  $scheduler["static","fifo"] <= "static";
 
  -- EXTERNAL
  --
  -- control signals
  --  0: low active, event: 0->1 transition
  --  1: high active, event: 1->0 transition
  --  2: not used
  --
  $ce[0,1,2] <= 2;
  $re[0,1,2] <= 2;
  $we[0,1,2] <= 2;

  
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
  $ram_addr;
  $ram_we;
  $ram_re;
  $ram_cs;
  
  -- INTERNAL
  
  --
  -- LOCAL=0: global shared access,
  -- LOCAL=1: local access by one process
  --
  $LOCAL[0,1] <= 0;
  $ARRAY [0,1]<= 0;
end;


-------------------
-- INTERNAL
-------------------

#assert($class="internal" and $arch="dualport")
begin
  size($P.read) = 2;
  size($P.write) = 2;
end;

--
-- Supported object methods
--

#methods($class="internal")
begin
  --
  -- write(data,addr)
  --
  write(#rhs:logic[$datawidth],#rhs:logic[$addrwidth]);
  --
  -- read(data,addr)
  --
  read(#lhs:logic[$datawidth],#rhs:logic[$addrwidth]);
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface($class="internal" and $LOCAL=0)
begin
  foreach $p in $P.read do
  begin
    signal RAM_$O_RE: out std_logic;
    signal RAM_$O_RD: in std_logic_vector[$datawidth];
  end;
  foreach $p in $P.write do
  begin
    signal RAM_$O_WE: out std_logic;
    signal RAM_$O_WR: out std_logic_vector[$datawidth];
  end;
  foreach $p in $P.read or $P.write do
  begin
    signal RAM_$O_ADDR: out std_logic_vector[$addrwidth+1];
  end;
  foreach $p in $P do
  begin
    signal RAM_$O_GD: in std_logic;
  end;
end;
#interface($class="internal" and $LOCAL=1)
begin
  null;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping($class="internal" and $LOCAL=0)
begin
  foreach $p in $P.read do
  begin
    RAM_$O_RE => RAM_$O_$p_RE;
    RAM_$O_RD => RAM_$O_$p_RD;
  end;
  foreach $p in $P.write do
  begin
    RAM_$O_WE => RAM_$O_$p_WE;
    RAM_$O_WR => RAM_$O_$p_WR;
  end;
  foreach $p in $P.write or $P.read do
  begin
    RAM_$O_ADDR => RAM_$O_$p_ADDR;  
  end;
  foreach $p in $P do
  begin
    RAM_$O_GD => RAM_$O_$p_GD;
  end;
end;
#mapping($class="internal" and $LOCAL=1)
begin
  null;
end;

--
-- Object method access (local process context)
-- for each method ...
--

read: #access($class="internal")
begin
  #data
  begin
    RAM_$O_ADDR <=  $ARG2 when $ACC else 0;
    RAM_$O_RE <= RAM_$O_GD when $ACC else '0';
    $ARG1 <= RAM_$O_RD when $ACC else 0;
  end;
  #control
  begin
    wait for RAM_$O_GD = '0';
  end;
end;

write: #access($class="internal")
begin
  #data
  begin
    RAM_$O_WE <= RAM_$O_GD when $ACC else '0';
    RAM_$O_WR <= $ARG1 when $ACC else 0;
    RAM_$O_ADDR <=  $ARG2 when $ACC else 0;
  end;
  #control
  begin
    wait for RAM_$O_GD = '0';
  end;
end;

--
-- Implementation (global module context)
-- VHDL signals required, both for mapping processes
-- and auxilliary signals.
--
#signals($class="internal" and $LOCAL=0)
begin
  --
  -- Implementation signals
  --

  foreach $p in $P.read do
  begin
    signal RAM_$O_$p_RE: std_logic;
    signal RAM_$O_$p_RD: std_logic_vector[$datawidth];
  end;
  foreach $p in $P.write do
  begin
    signal RAM_$O_$p_WE: std_logic;
    signal RAM_$O_$p_WR: std_logic_vector[$datawidth];
  end;
  foreach $p in $P do
  begin
    signal RAM_$O_$p_ADDR: std_logic_vector[$addrwidth+1];
    signal RAM_$O_$p_GD: std_logic;    
  end;
end;
#signals($class="internal" and $LOCAL=1)
begin
  --
  -- Implementation signals
  --

  foreach $p in $P.read do
  begin
    signal RAM_$O_RE: std_logic;
    signal RAM_$O_RD: std_logic_vector[$datawidth];
  end;
  foreach $p in $P.write do
  begin
    signal RAM_$O_WE: std_logic;
    signal RAM_$O_WR: std_logic_vector[$datawidth];
  end;
  foreach $p in $P do
  begin
    signal RAM_$O_ADDR: std_logic_vector[$addrwidth+1];
    signal RAM_$O_GD: std_logic;    
  end;
end;

#signals($class="internal" and $mode="read-first" and $arch="singleport")
begin
  signal RAM_$O_ENABLE: std_logic;
  signal RAM_$O_WRITE: std_logic;
  signal RAM_$O_COMPL: std_logic;
  signal RAM_$O_ADDRSEL: std_logic_vector[$addrwidth+1];
  signal RAM_$O_DATA_IN: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_OUT: std_logic_vector[$datawidth];
  type RAM_$O_TYPE is array[0 to ((2^$addrwidth)-1)] of std_logic_vector[$datawidth];
  signal RAM_$O: RAM_$O_TYPE;
end;

RAM_$O_RAM_IMPL:#process($class="internal" and $mode="read-first" and $arch="singleport")
begin
  if $CLK then
  begin
    if RAM_$O_ENABLE = '1' then
    begin
      if RAM_$O_WRITE = '1' then
        RAM_$O[to_nat(RAM_$O_ADDRSEL)] <= RAM_$O_DATA_IN;
      RAM_$O_DATA_OUT <= RAM_$O[to_nat(RAM_$O_ADDRSEL)];
      RAM_$O_COMPL <= '1';
    end;
    if RAM_$O_COMPL = '1' or $RES then
    begin
      RAM_$O_COMPL <= '0';
    end;
  end;
end;

#signals($class="internal" and $mode="write-first" and $arch="singleport")
begin
  signal RAM_$O_ENABLE: std_logic;
  signal RAM_$O_WRITE: std_logic;
  signal RAM_$O_COMPL: std_logic;
  signal RAM_$O_ADDRSEL: std_logic_vector[$addrwidth+1];
  signal RAM_$O_DATA_IN: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_OUT: std_logic_vector[$datawidth];
  type RAM_$O_TYPE is array[0 to ((2^$addrwidth)-1)] of std_logic_vector[$datawidth];
  signal RAM_$O: RAM_$O_TYPE;
end;

RAM_$O_RAM_IMPL:#process($class="internal" and $mode="write-first" and $arch="singleport")
begin
  if $CLK then
  begin
    if RAM_$O_ENABLE = '1' then
    begin
      if RAM_$O_WRITE = '1' then
      begin
        RAM_$O[to_nat(RAM_$O_ADDRSEL)] <= RAM_$O_DATA_IN;
        RAM_$O_DATA_OUT <= RAM_$O_DATA_IN;
      end
      else
        RAM_$O_DATA_OUT <= RAM_$O[to_nat(RAM_$O_ADDRSEL)];
      RAM_$O_COMPL <= '1';
    end;
    if RAM_$O_COMPL = '1' then
    begin
      RAM_$O_COMPL <= '0';
    end;
  end;
end;

#signals($class="internal" and $mode="no-change" and $arch="singleport")
begin
  signal RAM_$O_ENABLE: std_logic;
  signal RAM_$O_WRITE: std_logic;
  signal RAM_$O_COMPL: std_logic;
  signal RAM_$O_ADDRSEL: std_logic_vector[$addrwidth+1];
  signal RAM_$O_DATA_IN: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_OUT: std_logic_vector[$datawidth];
  type RAM_$O_TYPE is array[0 to ((2^$addrwidth)-1)] of std_logic_vector[$datawidth];
  signal RAM_$O: RAM_$O_TYPE;
end;

RAM_$O_RAM_IMPL:#process($class="internal" and $mode="no-change" and $arch="singleport")
begin
  if $CLK then
  begin
    if RAM_$O_ENABLE = '1' then
    begin
      if RAM_$O_WRITE = '1' then
        RAM_$O[to_nat(RAM_$O_ADDRSEL)] <= RAM_$O_DATA_IN
      else
        RAM_$O_DATA_OUT <= RAM_$O[to_nat(RAM_$O_ADDRSEL)];
      RAM_$O_COMPL <= '1';
    end;
    if RAM_$O_COMPL = '1' then
    begin
      RAM_$O_COMPL <= '0';
    end;
  end;
end;

#signals($class="internal" and $mode="async-read" and $arch="singleport")
begin
  signal RAM_$O_ENABLE: std_logic;
  signal RAM_$O_WRITE: std_logic;
  signal RAM_$O_COMPL: std_logic;
  signal RAM_$O_ADDRSEL: std_logic_vector[$addrwidth+1];
  signal RAM_$O_DATA_IN: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_OUT: std_logic_vector[$datawidth];
  type RAM_$O_TYPE is array[0 to ((2^$addrwidth)-1)] of std_logic_vector[$datawidth];
  signal RAM_$O: RAM_$O_TYPE;
end;

RAM_$O_RAM_IMPL:#process($class="internal" and $mode="async-read" and $arch="singleport")
begin
  if $CLK then
  begin
    if RAM_$O_WRITE = '1' then
    begin
      RAM_$O[to_nat(RAM_$O_ADDRSEL)] <= RAM_$O_DATA_IN;
      RAM_$O_COMPL <= '1';
    end;
    if RAM_$O_COMPL = '1' then
    begin
      RAM_$O_COMPL <= '0';
    end;
  end;
  RAM_$O_DATA_OUT <= RAM_$O[to_nat(RAM_$O_ADDRSEL)];
end;

#signals($class="internal" and $mode="sync-read" and $arch="singleport")
begin
  signal RAM_$O_ENABLE: std_logic;
  signal RAM_$O_WRITE: std_logic;
  signal RAM_$O_COMPL: std_logic;
  signal RAM_$O_ADDRSEL: std_logic_vector[$addrwidth+1];
  signal RAM_$O_ADDR_AUX: std_logic_vector[$addrwidth+1];
  signal RAM_$O_DATA_IN: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_OUT: std_logic_vector[$datawidth];
  type RAM_$O_TYPE is array[0 to ((2^$addrwidth)-1)] of std_logic_vector[$datawidth];
  signal RAM_$O: RAM_$O_TYPE;
end;

RAM_$O_RAM_IMPL:#process($class="internal" and $mode="sync-read" and $arch="singleport")
begin
  if $CLK then
  begin
    if RAM_$O_ENABLE = '1' then
    begin
      if RAM_$O_WRITE = '1' then
        RAM_$O[to_nat(RAM_$O_ADDRSEL)] <= RAM_$O_DATA_IN;
      RAM_$O_ADDR_AUX <= RAM_$O_ADDRSEL;
      RAM_$O_COMPL <= '1';
    end;
    if RAM_$O_COMPL = '1' then
    begin
      RAM_$O_COMPL <= '0';
    end;
  end;
  RAM_$O_DATA_OUT <= RAM_$O[to_nat(RAM_$O_ADDR_AUX)];
end;

#signals($class="internal" and $mode="sync-read2-write1" and $arch="singleport")
begin
  signal RAM_$O_ENABLE: std_logic;
  signal RAM_$O_WRITE: std_logic;
  signal RAM_$O_COMPL: std_logic;
  signal RAM_$O_ADDR_A: std_logic_vector[$addrwidth+1];
  signal RAM_$O_ADDR_B: std_logic_vector[$addrwidth+1];
  signal RAM_$O_ADDR_AUX_A: std_logic_vector[$addrwidth+1];
  signal RAM_$O_ADDR_AUX_B: std_logic_vector[$addrwidth+1];
  signal RAM_$O_DATA_IN: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_OUT_A: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_OUT_B: std_logic_vector[$datawidth];
  type RAM_$O_TYPE is array[0 to ((2^$addrwidth)-1)] of std_logic_vector[$datawidth];
  signal RAM_$O: RAM_$O_TYPE;
end;

RAM_$O_RAM_IMPL:#process($class="internal" and $mode="sync-read2-write1" and $arch="dualport")
begin
  if $CLK then
  begin
    if RAM_$O_ENABLE = '1' then
    begin
      if RAM_$O_WRITE = '1' then
        RAM_$O[to_nat(RAM_$O_ADDR_A)] <= RAM_$O_DATA_IN;
      RAM_$O_ADDR_AUX_A <= RAM_$O_ADDR_A;
      RAM_$O_ADDR_AUX_B <= RAM_$O_ADDR_B;
      RAM_$O_COMPL <= '1';
    end;
    if RAM_$O_COMPL = '1' then
    begin
      RAM_$O_COMPL <= '0';
    end;
  end;
  RAM_$O_DATA_OUT_A <= RAM_$O[to_nat(RAM_$O_ADDR_AUX_A)];
  RAM_$O_DATA_OUT_B <= RAM_$O[to_nat(RAM_$O_ADDR_AUX_B)];
end;


--
-- True Dual-Port Implementation requires global shared variable for RAM!
--
#signals($class="internal" and $mode="read-first" and $arch="dualport")
begin
  signal RAM_$O_EN_A: std_logic;
  signal RAM_$O_EN_B: std_logic;
  signal RAM_$O_WE_A: std_logic;
  signal RAM_$O_WE_B: std_logic;
  signal RAM_$O_COMPL_A: std_logic;
  signal RAM_$O_COMPL_B: std_logic;
  signal RAM_$O_ADDR_A: std_logic_vector[$addrwidth+1];
  signal RAM_$O_ADDR_B: std_logic_vector[$addrwidth+1];
  signal RAM_$O_DATA_IN_A: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_IN_B: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_OUT_A: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_OUT_B: std_logic_vector[$datawidth];
  type RAM_$O_TYPE is array[0 to ((2^$addrwidth)-1)] of std_logic_vector[$datawidth];
  shared variable RAM_$O: RAM_$O_TYPE;
end;

RAM_$O_RAM_IMPL_A:#process($class="internal" and $mode="read-first" and $arch="dualport")
begin
  if $CLK then
  begin
    if RAM_$O_EN_A = '1' then
    begin
      if RAM_$O_WE_A = '1' then
        RAM_$O[to_nat(RAM_$O_ADDR_A)] := RAM_$O_DATA_IN_A;
      RAM_$O_DATA_OUT_A <= RAM_$O[to_nat(RAM_$O_ADDR_A)];
      RAM_$O_COMPL_A <= '1';
    end;
    if RAM_$O_COMPL_A = '1' then
    begin
      RAM_$O_COMPL_A <= '0';
    end;
  end;
end;

RAM_$O_RAM_IMPL_B:#process($class="internal" and $mode="read-first" and $arch="dualport")
begin
  if $CLK then
  begin
    if RAM_$O_EN_B = '1' then
    begin
      if RAM_$O_WE_B = '1' then
        RAM_$O[to_nat(RAM_$O_ADDR_B)] := RAM_$O_DATA_IN_B;
      RAM_$O_DATA_OUT_B <= RAM_$O[to_nat(RAM_$O_ADDR_B)];
      RAM_$O_COMPL_B <= '1';
    end;
    if RAM_$O_COMPL_B = '1' then
    begin
      RAM_$O_COMPL_B <= '0';
    end;
  end;
end;

--
-- Signals for Scheduler process: access serialization
--
#signals($class="internal" and $scheduler="static" and $LOCAL=0)
begin
  --
  -- Implementation signals
  --
  signal RAM_$O_BUSY: std_logic;

  foreach $p in $P do
  begin
    signal RAM_$O_$p_LOCKed: std_logic;
  end;
  
end;

#signals($class="internal" and $scheduler="static" and $LOCAL=1)
begin
  --
  -- Implementation signals
  --
  signal RAM_$O_BUSY: std_logic;
  signal RAM_$O_LOCKed: std_logic;
  
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
RAM_$O_SCHED: #process($class="internal" and $arch="singleport" and $scheduler="static" and $LOCAL=0)
begin
  foreach $p in $P do
  begin
    RAM_$O_$p_GD <= '1';
  end;
  foreach $p in $P.read do
  begin
    RAM_$O_$p_RD <= to_logic(0,$datawidth);  
  end;
  
  RAM_$O_DATA_IN <= to_logic(0,$datawidth);
  RAM_$O_ADDRSEL <= to_logic(0,$addrwidth+1);
  RAM_$O_ENABLE <= '0';
  RAM_$O_WRITE <= '0';
  
  sequence
  begin
    foreach $p in $P.read do
    begin
      if RAM_$O_$p_LOCKed  = '1' and  RAM_$O_COMPL = '1' then
      begin
        RAM_$O_$p_RD <= RAM_$O_DATA_OUT;
        RAM_$O_$p_GD <= '0';
      end;
    end;
    foreach $p in $P.write do
    begin
      if RAM_$O_$p_LOCKed  = '1' and RAM_$O_COMPL = '1' then
      begin
        RAM_$O_$p_GD <= '0';
      end;
    end;
    foreach $p in $P.read do
    begin
      if RAM_$O_$p_RE = '1' and RAM_$O_$p_LOCKed  = '0' and  RAM_$O_COMPL = '0' then
      begin
        RAM_$O_ENABLE <= '1';
        RAM_$O_ADDRSEL <= RAM_$O_$p_ADDR;
      end;
    end;
    foreach $p in $P.write do
    begin
      if RAM_$O_$p_WE = '1' and RAM_$O_$p_LOCKed  = '0' and RAM_$O_COMPL = '0' then
      begin
        RAM_$O_DATA_IN <= RAM_$O_$p_WR;
        RAM_$O_ADDRSEL <= RAM_$O_$p_ADDR;
        RAM_$O_ENABLE <= '1';
        RAM_$O_WRITE <= '1';
      end;
    end;
  end;
  if $CLK then
  begin
    if $RES then
    begin
      foreach $p in $P do
      begin
        RAM_$O_$p_LOCKed <= '0';
      end;
    end
    else -- if RAM_$O_BUSY = '0' then
    begin      
      sequence
      begin
        foreach $p in $P.read do
        begin
          if RAM_$O_$p_LOCKed  = '1' and RAM_$O_COMPL = '1' then
          begin
            RAM_$O_$p_LOCKed <= '0';
          end;
        end;
        foreach $p in $P.write do
        begin
          if RAM_$O_$p_LOCKed  = '1' and RAM_$O_COMPL = '1' then
          begin
            RAM_$O_$p_LOCKed <= '0';
          end;
        end;
        foreach $p in $P.read do
        begin
          if RAM_$O_$p_RE = '1' and RAM_$O_$p_LOCKed  = '0' then
          begin
            RAM_$O_$p_LOCKed <= '1';
          end;
        end;
        foreach $p in $P.write do
        begin
          if RAM_$O_$p_WE = '1' and RAM_$O_$p_LOCKed  = '0' then
          begin
            RAM_$O_$p_LOCKed <= '1';
          end;
        end;
      end;
    -- end
    -- else
    -- begin
    --   RAM_$O_EN <= '0';
    --   RAM_$O_WE <= '0';
    --   RAM_$O_BUSY <= '0';
    end;
    
  end;
end;
RAM_$O_SCHED: #process($class="internal" and $arch="singleport" and $scheduler="static" and $LOCAL=1)
begin
  if $CLK then
  begin
    if $RES then
    begin
      RAM_$O_BUSY <= '0';
      RAM_$O_GD <= '1';
      RAM_$O_LOCKed <= '0';
      -- RAM_$O_RE <= '0';
      RAM_$O_WRITE <= '0';
      RAM_$O_ENABLE <= '0';
    end
    else -- if RAM_$O_BUSY = '0' then
    begin
      RAM_$O_GD <= '1';
      -- RAM_$O_RE <= '0';
      RAM_$O_WRITE <= '0';
      RAM_$O_ENABLE <= '0';
      
      sequence
      begin
        foreach $p in $P.read do
        begin
          if RAM_$O_RE = '1' and RAM_$O_LOCKed  = '1' and RAM_$O_COMPL = '1' then
          begin
            RAM_$O_RD <= RAM_$O_DATA_OUT;
            RAM_$O_GD <= '0';
            RAM_$O_LOCKed <= '0';
            RAM_$O_BUSY <= '0';
          end;
        end;
        foreach $p in $P.write do
        begin
          if RAM_$O_WE = '1' and RAM_$O_LOCKed  = '1' and RAM_$O_COMPL = '1' then
          begin
            RAM_$O_GD <= '0';
            RAM_$O_LOCKed <= '0';
            RAM_$O_BUSY <= '0';
          end;
        end;
        foreach $p in $P.read do
        begin
          if RAM_$O_RE = '1' and RAM_$O_LOCKed  = '0' and RAM_$O_BUSY = '0' then
          begin
            RAM_$O_ENABLE <= '1';
            RAM_$O_BUSY <= '1';
            RAM_$O_ADDRSEL <= RAM_$O_ADDR;
            RAM_$O_LOCKed <= '1';
          end;
        end;
        foreach $p in $P.write do
        begin
          if RAM_$O_WE = '1' and RAM_$O_LOCKed  = '0' and RAM_$O_BUSY = '0' then
          begin
            RAM_$O_DATA_IN <= RAM_$O_WR;
            RAM_$O_ADDRSEL <= RAM_$O_ADDR;
            RAM_$O_ENABLE <= '1';
            RAM_$O_WRITE <= '1';
            RAM_$O_BUSY <= '1';
            RAM_$O_LOCKed <= '1';
          end;
        end;
      end;
    -- end
    -- else
    -- begin
    --   RAM_$O_EN <= '0';
    --   RAM_$O_WE <= '0';
    --   RAM_$O_BUSY <= '0';
    end;
    
  end;
end;

RAM_$O_SCHED_A: #process($class="internal" and $arch="dualport" and $mode="read-first")
begin
  with $p = nth($P,1) do
  begin
    if $CLK then
    begin
      if $RES then
      begin
        -- RAM_$O_BUSY <= '0';
        RAM_$O_$p_GD <= '1';
        RAM_$O_$p_LOCKed <= '0';
        -- RAM_$O_RE_A <= '0';
        RAM_$O_WE_A <= '0';
        RAM_$O_EN_A <= '0';
      end
      else -- if RAM_$O_BUSY = '0' then
      begin
        RAM_$O_$p_GD <= '1';
        -- RAM_$O_RE_A <= '0';
        RAM_$O_WE_A <= '0';
        RAM_$O_EN_A <= '0';

        sequence
        begin
          if RAM_$O_$p_RE = '1' and RAM_$O_$p_LOCKed  = '1' and RAM_$O_COMPL_A = '1' then
          begin
            RAM_$O_$p_RD <= RAM_$O_DATA_OUT_A;
            RAM_$O_$p_GD <= '0';
            RAM_$O_$p_LOCKed <= '0';
          end;
          if RAM_$O_$p_WE = '1' and RAM_$O_$p_LOCKed  = '1' and RAM_$O_COMPL_A = '1' then
          begin
            RAM_$O_$p_GD <= '0';
            RAM_$O_$p_LOCKed <= '0';
          end;
          if RAM_$O_$p_RE = '1' and RAM_$O_$p_LOCKed  = '0'  then
          begin
            RAM_$O_EN_A <= '1';
            -- RAM_$O_BUSY <= '1';
            RAM_$O_ADDR_A <= RAM_$O_$p_ADDR;
            RAM_$O_$p_LOCKed <= '1';
          end;
          if RAM_$O_$p_WE = '1' and RAM_$O_$p_LOCKed  = '0'  then
          begin
            RAM_$O_DATA_IN_A <= RAM_$O_$p_WR;
            RAM_$O_ADDR_A <= RAM_$O_$p_ADDR;
            RAM_$O_EN_A <= '1';
            RAM_$O_WE_A <= '1';
            -- RAM_$O_BUSY <= '1';
            RAM_$O_$p_LOCKed <= '1';
          end;
        end;
      -- end
      -- else
      -- begin
      --   RAM_$O_EN <= '0';
      --   RAM_$O_WE <= '0';
      --   RAM_$O_BUSY <= '0';
      end;

    end;
  end;
end;

RAM_$O_SCHED_B: #process($class="internal" and $arch="dualport" and $mode="read-first")
begin
  with $p = nth($P,2) do
  begin
    if $CLK then
    begin
      if $RES then
      begin
        -- RAM_$O_BUSY <= '0';
        RAM_$O_$p_GD <= '1';
        RAM_$O_$p_LOCKed <= '0';
        -- RAM_$O_RE_B <= '0';
        RAM_$O_WE_B <= '0';
        RAM_$O_EN_B <= '0';
      end
      else -- if RAM_$O_BUSY = '0' then
      begin
        RAM_$O_$p_GD <= '1';
        -- RAM_$O_RE_B <= '0';
        RAM_$O_WE_B <= '0';
        RAM_$O_EN_B <= '0';

        sequence
        begin
          if RAM_$O_$p_RE = '1' and RAM_$O_$p_LOCKed  = '1' and RAM_$O_COMPL_B = '1' then
          begin
            RAM_$O_$p_RD <= RAM_$O_DATA_OUT_B;
            RAM_$O_$p_GD <= '0';
            RAM_$O_$p_LOCKed <= '0';
          end;
          if RAM_$O_$p_WE = '1' and RAM_$O_$p_LOCKed  = '1' and RAM_$O_COMPL_B = '1' then
          begin
            RAM_$O_$p_GD <= '0';
            RAM_$O_$p_LOCKed <= '0';
          end;
          if RAM_$O_$p_RE = '1' and RAM_$O_$p_LOCKed  = '0'  then
          begin
            RAM_$O_EN_B <= '1';
            -- RAM_$O_BUSY <= '1';
            RAM_$O_ADDR_B <= RAM_$O_$p_ADDR;
            RAM_$O_$p_LOCKed <= '1';
          end;
          if RAM_$O_$p_WE = '1' and RAM_$O_$p_LOCKed  = '0' then
          begin
            RAM_$O_DATA_IN_B <= RAM_$O_$p_WR;
            RAM_$O_ADDR_B <= RAM_$O_$p_ADDR;
            RAM_$O_EN_B <= '1';
            RAM_$O_WE_B <= '1';
            -- RAM_$O_BUSY <= '1';
            RAM_$O_$p_LOCKed <= '1';
          end;
        end;
      -- end
      -- else
      -- begin
      --   RAM_$O_EN <= '0';
      --   RAM_$O_WE <= '0';
      --   RAM_$O_BUSY <= '0';
      end;

    end;
  end;
end; 




-------------------
-- EXTERNAL
-------------------

--
-- Supported object methods
--
#methods($class="external")
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
  -- port(db,addr,re,we,cs)
  --
  interface(#lrhs:logic[$datawidth],#rhs:logic[$addrwidth],#lhs:logic,#lhs:logic,#lhs:logic);
end;

#assert($class="external")
begin
  size($P.interface) >= 1;
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface($class="external")
begin
  foreach $p in $P.read do
  begin
    signal RAM_$O_RE: out std_logic;
    signal RAM_$O_RD: in std_logic_vector[$datawidth];
  end;
  foreach $p in $P.write do
  begin
    signal RAM_$O_WE: out std_logic;
    signal RAM_$O_WR: out std_logic_vector[$datawidth];
  end;
  foreach $p in $P.read or $P.write do
  begin
    signal RAM_$O_ADDR: out std_logic_vector[$addrwidth];
  end;
  foreach $p in $P do
  begin
    signal RAM_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping($class="external")
begin
  foreach $p in $P.read do
  begin
    RAM_$O_RE => RAM_$O_$p_RE;
    RAM_$O_RD => RAM_$O_$p_RD;
  end;
  foreach $p in $P.write do
  begin
    RAM_$O_WE => RAM_$O_$p_WE;
    RAM_$O_WR => RAM_$O_$p_WR;
  end;
  foreach $p in $P.write or $P.read do
  begin
    RAM_$O_ADDR => RAM_$O_$p_ADDR;  
  end;
  foreach $p in $P do
  begin
    RAM_$O_GD => RAM_$O_$p_GD;
  end;
end;

--
-- Object method access (local process context)
-- for each method ...
--

read: #access($class="external")
begin
  #data
  begin
    RAM_$O_RE <= RAM_$O_GD when $ACC else '0';
    $ARG1 <= RAM_$O_RD when $ACC else 0;
    RAM_$O_ADDR <=  $ARG2 when $ACC else 0;
  end;
  #control
  begin
    wait for RAM_$O_GD = '0';
  end;
end;

write: #access($class="external")
begin
  #data
  begin
    RAM_$O_WE <= RAM_$O_GD when $ACC else '0';
    RAM_$O_WR <= $ARG1 when $ACC else 0;
    RAM_$O_ADDR <=  $ARG2 when $ACC else 0;
  end;
  #control
  begin
    wait for RAM_$O_GD = '0';
  end;
end;


interface: #access($class="external")
begin
  #set
  begin
    $ram_db <= $ARG1;
    $ram_addr <= $ARG2;
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
#signals($class="external")
begin
  --
  -- Implementation signals
  --

  foreach $p in $P.read do
  begin
    signal RAM_$O_$p_RE: std_logic;
    signal RAM_$O_$p_RD: std_logic_vector[$datawidth];
  end;
  foreach $p in $P.write do
  begin
    signal RAM_$O_$p_WE: std_logic;
    signal RAM_$O_$p_WR: std_logic_vector[$datawidth];
  end;
  foreach $p in $P do
  begin
    signal RAM_$O_$p_ADDR: std_logic_vector[$addrwidth];
    signal RAM_$O_$p_GD: std_logic;    
  end;
end;


--
-- Bus control signals
--
#signals ($class="external" and $re = 0 or $re = 1)
begin
  signal RAM_$O_RE: std_logic;
end;
#signals ($class="external" and $we = 0 or $we = 1)
begin
  signal RAM_$O_WE: std_logic;
end;
#signals ($class="external" and $ce = 0 or $ce = 1)
begin
  signal RAM_$O_CE: std_logic;
end;



--
-- 3-process-FSM implementation
--

#signals($class="external")
begin
  type RAM_$O_STATES is {
    RAM_$O_S_REQ,
    RAM_$O_S_READ_LOAD,
    RAM_$O_S_WRITE_LOAD,
    RAM_$O_S_READ,
    RAM_$O_S_WRITE,
    RAM_$O_S_READ_HOLD,    
    RAM_$O_S_WRITE_HOLD
  };
  signal RAM_$O_STATE: RAM_$O_STATES;
  signal RAM_$O_NEXT_STATE: RAM_$O_STATES;
 
  signal RAM_$O_WRITE_ACT: logic;
  signal RAM_$O_ADDR_ACT: logic;
  signal RAM_$O_ADDR_WR: std_logic_vector[$addrwidth];
  signal RAM_$O_ADDR_WE: std_logic;
  signal RAM_$O_DATA_WR: std_logic_vector[$datawidth];
  signal RAM_$O_DATA_WE: std_logic;
  signal RAM_$O_LOCK_WE: logic;

  foreach $p in $P do
  begin
    signal RAM_$O_$p_LOCK_WR: std_logic;
  end;
  signal RAM_$O_WAIT_WE: logic;
  signal RAM_$O_WAIT_WR: logic[4];
  signal RAM_$O_WAIT: logic[4];

  --
  -- Bus signals
  --
  signal RAM_$O_DB_RD: std_logic_vector[$datawidth];
  signal RAM_$O_DB_WR: std_logic_vector[$datawidth];
  signal RAM_$O_AB_WR: std_logic_vector[$addrwidth];
  
end;

#signals($class="external")
begin
  foreach $p in $P do
  begin
    signal RAM_$O_$p_LOCKED: std_logic;
  end;
end;

RAM_$O_SCHED_REG:#process($class="external")
begin
  if $CLK then
  begin
    if $RES then
    begin
      RAM_$O_STATE <= RAM_$O_S_REQ;
      foreach $p in $P do
        RAM_$O_$p_LOCKED <= '0';
      RAM_$O_WAIT <= to_logic(0,4);
    end
    else
    begin
      RAM_$O_STATE <= RAM_$O_NEXT_STATE;
      if RAM_$O_ADDR_WE = '1' then
        RAM_$O_AB_WR <= RAM_$O_ADDR_WR;
      if RAM_$O_DATA_WE = '1' then
        RAM_$O_DB_WR <= RAM_$O_DATA_WR;
      if RAM_$O_LOCK_WE = '1' then
      begin
        foreach $p in $P do
          RAM_$O_$p_LOCKED <= RAM_$O_$p_LOCK_WR;
      end;
      if RAM_$O_WAIT_WE = '1' then
        RAM_$O_WAIT <= RAM_$O_WAIT_WR;
    end;
  end;
end;

RAM_$O_CONTROL:#process($class="external")
begin
  RAM_$O_WRITE_ACT <= '0';
  RAM_$O_ADDR_ACT <= '0';
  
  if $ce = 0 then
    RAM_$O_CE <= '1';
  if $ce = 1 then
    RAM_$O_CE <= '0';
  if $re = 0 then
    RAM_$O_RE <= '1';
  if $re = 1 then
    RAM_$O_RE <= '0';
  if $we = 0 then
    RAM_$O_WE <= '1';
  if $we = 1 then
    RAM_$O_WE <= '0';

  case RAM_$O_STATE is
  begin
    when RAM_$O_S_REQ => 
    begin
      null;
    end;
    when RAM_$O_S_READ_LOAD =>
    begin
     if $ce = 0 then
        RAM_$O_CE <= '0';
      RAM_$O_ADDR_ACT <= '1';
    end;
    when RAM_$O_S_WRITE_LOAD =>
    begin
     if $ce = 0 then
        RAM_$O_CE <= '0';
      if $addrload /= 0 then
      begin
        if RAM_$O_WAIT < to_logic($addrload,4) then
          RAM_$O_ADDR_ACT <= '1';
      end;
      if $dataload /= 0 then
      begin
        if RAM_$O_WAIT < to_logic($dataload,4) then
          RAM_$O_WRITE_ACT <= '1';
      end;
    end;
    when RAM_$O_S_READ => 
    begin
      RAM_$O_ADDR_ACT <= '1';
      if $ce = 0 then
        RAM_$O_CE <= '0';
      if $ce = 1 then
        RAM_$O_CE <= '1';
      if $re = 0 then
        RAM_$O_RE <= '0';
      if $re = 1 then
        RAM_$O_RE <= '1';
    end;
    when RAM_$O_S_WRITE =>
    begin
      RAM_$O_WRITE_ACT <= '1';
      RAM_$O_ADDR_ACT <= '1';
      if $ce = 0 then
        RAM_$O_CE <= '0';
      if $ce = 1 then
        RAM_$O_CE <= '1';
      if $we = 0 then
        RAM_$O_WE <= '0';
      if $we = 1 then
        RAM_$O_WE <= '1';
    end;
    when RAM_$O_S_READ_HOLD =>
    begin
     if $ce = 0 then
        RAM_$O_CE <= '0';
      RAM_$O_ADDR_ACT <= '1';
    end;
    when RAM_$O_S_WRITE_HOLD =>
    begin
     if $ce = 0 then
        RAM_$O_CE <= '0';
      if $addrhold /= 0 then
      begin
        if RAM_$O_WAIT < to_logic($addrload,4) then
          RAM_$O_ADDR_ACT <= '1';
      end;
      if $datahold /= 0 then
      begin
        if RAM_$O_WAIT < to_logic($datahold,4) then
          RAM_$O_WRITE_ACT <= '1';
      end;
    end;
  end;

end;

RAM_$O_SCHED_STATE:#process($class="external")
begin
  RAM_$O_ADDR_WR <= to_logic(0,$addrwidth);
  RAM_$O_ADDR_WE <= '0';
  RAM_$O_DATA_WR <= to_logic(0,$datawidth);
  RAM_$O_DATA_WE <= '0';
  RAM_$O_WAIT_WR <= to_logic(0,4);  
  RAM_$O_WAIT_WE <= '0';
  RAM_$O_LOCK_WE <= '0';
  foreach $p in $P do
  begin
    RAM_$O_$p_LOCK_WR <= '0';
    RAM_$O_$p_GD <= '1';
  end;    
  foreach $p in $P.read do
  begin
    RAM_$O_$p_RD <= to_logic(0,$datawidth);
  end;
  
  case RAM_$O_STATE is
  begin
  
    when RAM_$O_S_REQ =>
    begin
      RAM_$O_NEXT_STATE <= RAM_$O_S_REQ;
      sequence
      begin
        foreach $p in $P.read do
        begin
          if RAM_$O_$p_RE = '1'  then
          begin
            if $addrload = 0 then
              RAM_$O_NEXT_STATE <= RAM_$O_S_READ
            else
            begin
              RAM_$O_NEXT_STATE <= RAM_$O_S_READ_LOAD;
              RAM_$O_WAIT_WR <= to_logic($addrload-1,4);
              RAM_$O_WAIT_WE <= '1';
            end;
            RAM_$O_ADDR_WR <= RAM_$O_$p_ADDR;
            RAM_$O_ADDR_WE <= '1';
            RAM_$O_$p_LOCK_WR <= '1';
            RAM_$O_LOCK_WE <= '1';
          end;
        end;
        foreach $p in $P.write do
        begin
          if RAM_$O_$p_WE = '1'  then
          begin
            if $addrload = 0 and $dataload = 0 then
              RAM_$O_NEXT_STATE <= RAM_$O_S_WRITE
            else
            begin
              RAM_$O_NEXT_STATE <= RAM_$O_S_WRITE_LOAD;
              RAM_$O_WAIT_WR <= to_logic(max($dataload,$addrload)-1,4);
              RAM_$O_WAIT_WE <= '1';
            end;
            RAM_$O_ADDR_WR <= RAM_$O_$p_ADDR;
            RAM_$O_ADDR_WE <= '1';
            RAM_$O_DATA_WR <= RAM_$O_$p_WR;
            RAM_$O_DATA_WE <= '1';
            RAM_$O_$p_LOCK_WR <= '1';
            RAM_$O_LOCK_WE <= '1';
          end;
        end;
      end;
    end;
    
    when RAM_$O_S_READ_LOAD =>
    begin
      RAM_$O_NEXT_STATE <= RAM_$O_S_READ_LOAD;
      if RAM_$O_WAIT = to_logic(0,4) then
      begin
        RAM_$O_NEXT_STATE <= RAM_$O_S_READ;
        RAM_$O_WAIT_WR <= to_logic(0,4);
        RAM_$O_WAIT_WE <= '1';
      end
      else
      begin
        RAM_$O_WAIT_WR <= RAM_$O_WAIT - 1;
        RAM_$O_WAIT_WE <= '1'; 
      end;      
    end;
    
    when RAM_$O_S_WRITE_LOAD =>
    begin
      RAM_$O_NEXT_STATE <= RAM_$O_S_WRITE_LOAD;
      if RAM_$O_WAIT = to_logic(0,4) then
      begin
        RAM_$O_NEXT_STATE <= RAM_$O_S_WRITE;
        RAM_$O_WAIT_WR <= to_logic(0,4);
        RAM_$O_WAIT_WE <= '1';
      end
      else
      begin
        RAM_$O_WAIT_WR <= RAM_$O_WAIT - 1;
        RAM_$O_WAIT_WE <= '1'; 
      end;      
    end;
    
    when RAM_$O_S_READ =>
    begin
      RAM_$O_NEXT_STATE <= RAM_$O_S_READ;
      if RAM_$O_WAIT = to_logic($cmdhold-1,4) then
      begin
        if $addrhold = 0 and $delayhold = 0 then
          RAM_$O_NEXT_STATE <= RAM_$O_S_REQ
        else 
        begin
          RAM_$O_WAIT_WR <= to_logic(0,4);
          RAM_$O_WAIT_WE <= '1';
          RAM_$O_NEXT_STATE <= RAM_$O_S_READ_HOLD;
        end;
        
        foreach $p in $P.read do
        begin
          if RAM_$O_$p_LOCKED = '1' then
          begin
            RAM_$O_$p_GD <= '0';
            RAM_$O_$p_RD <= RAM_$O_DB_RD;
            RAM_$O_$p_LOCK_WR <= '0';
            RAM_$O_LOCK_WE <= '1';
          end;
        end;        
      end
      else
      begin
        RAM_$O_WAIT_WR <= RAM_$O_WAIT + 1;
        RAM_$O_WAIT_WE <= '1'; 
      end;
    end;
    
    when RAM_$O_S_WRITE =>
    begin
      RAM_$O_NEXT_STATE <= RAM_$O_S_WRITE;
      if RAM_$O_WAIT = to_logic($cmdhold-1,4) then
      begin
        if $addrhold = 0 and $delayhold = 0 and $datahold = 0 then
          RAM_$O_NEXT_STATE <= RAM_$O_S_REQ
        else 
        begin
          RAM_$O_WAIT_WR <= to_logic(0,4);
          RAM_$O_WAIT_WE <= '1';
          RAM_$O_NEXT_STATE <= RAM_$O_S_WRITE_HOLD;
        end;
        foreach $p in $P.write do
        begin
          if RAM_$O_$p_LOCKED = '1' then
          begin
            RAM_$O_$p_GD <= '0';
            RAM_$O_$p_LOCK_WR <= '0';
            RAM_$O_LOCK_WE <= '1';
          end;
        end;        
      end
      else
      begin
        RAM_$O_WAIT_WR <= RAM_$O_WAIT + 1;
        RAM_$O_WAIT_WE <= '1'; 
      end;
    end;
    
    when RAM_$O_S_READ_HOLD =>
    begin
      RAM_$O_NEXT_STATE <= RAM_$O_S_READ_HOLD;
      if RAM_$O_WAIT = to_logic(max($delayhold,$addrhold)-1,4) then
        RAM_$O_NEXT_STATE <= RAM_$O_S_REQ
      else
      begin
        RAM_$O_WAIT_WR <= RAM_$O_WAIT + 1;
        RAM_$O_WAIT_WE <= '1'; 
      end;      
    end;
    
    when RAM_$O_S_WRITE_HOLD =>
    begin
      RAM_$O_NEXT_STATE <= RAM_$O_S_WRITE_HOLD;
      if RAM_$O_WAIT = to_logic(max($delayhold,$addrhold,$datahold)-1,4) then
          RAM_$O_NEXT_STATE <= RAM_$O_S_REQ
      else
      begin
        RAM_$O_WAIT_WR <= RAM_$O_WAIT + 1;
        RAM_$O_WAIT_WE <= '1'; 
      end;      
    end;
  end;
end;


#top($class="external")
begin
  RAM_$O_DB_RD <= $ram_db;
  $ram_db <= RAM_$O_DB_WR when RAM_$O_WRITE_ACT = '1' else (others => 'Z');
  $ram_ab <= RAM_$O_AB_WR when RAM_$O_ADDR_ACT = '1' else (others => '0');
  $ram_re <= RAM_$O_RE;
  $ram_we <= RAM_$O_WE;
  $ram_cs <= RAM_$O_CE;
end;
