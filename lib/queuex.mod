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
--    $CREATED:     31.3.2011
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements queue object
--
--
--    $ENDOFINFO
--

#version "deleop.1-1";

--
-- parameters used, allowed and default values
--
#parameter
begin
  
  --
  -- A signal can be used to monitor the 
  -- complete receive of a data word
  --
  $select[0,1] <= 0;

  --
  -- Object parameters, can be changed
  --
  
  --
  -- Datawidth and type of queue
  --
  $datawidth[2 to 64] <= 8;
  $datatype["logic","int"] <= "logic";
  --
  -- Number of cells
  --
  $depth [1 to 1024] <= 8;
  $ARRAY [0,1]<= 0;

  $avail;

end;

#methods ($datatype = "logic")
begin
  init();
  read(#rhs:logic[$datawidth]);
  write(#lhs:logic[$datawidth]);
end;

#methods ($datatype = "int")
begin
  init();
  read(#rhs:int[$datawidth]);
  write(#lhs:int[$datawidth]);
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal QUEUEX_$O_INIT: out std_logic;
  end;
  foreach $p in $P.read do
  begin
    signal QUEUEX_$O_RD: in std_logic_vector[$datawidth];
    signal QUEUEX_$O_RE: out std_logic;
  end;
  foreach $p in $P.write do
  begin
    signal QUEUEX_$O_WE: out std_logic;
    signal QUEUEX_$O_WR: out std_logic_vector[$datawidth];
  end;
  foreach $p in $P do
  begin
    signal QUEUEX_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    QUEUEX_$O_INIT => QUEUEX_$O_$p_INIT;
  end;
  foreach $p in $P.read do
  begin
    QUEUEX_$O_RD => QUEUEX_$O_$p_RD;
    QUEUEX_$O_RE => QUEUEX_$O_$p_RE;
  end;
  foreach $p in $P.write do
  begin
    QUEUEX_$O_WE => QUEUEX_$O_$p_WE;
    QUEUEX_$O_WR => QUEUEX_$O_$p_WR;
  end;
  foreach $p in $P do
  begin
    QUEUEX_$O_GD => QUEUEX_$O_$p_GD;
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
    QUEUEX_$O_INIT <= QUEUEX_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for QUEUEX_$O_GD = '0';
  end;
end;

read: #access
begin
  #data
  begin
    QUEUEX_$O_RE <= QUEUEX_$O_GD when $ACC else '0';
    $ARG1 <= QUEUEX_$O_RD when $ACC else 0;
  end;
  #control
  begin
    wait for QUEUEX_$O_GD = '0';
  end;
end;


write: #access
begin
  #data
  begin
    QUEUEX_$O_WE <= QUEUEX_$O_GD when $ACC else '0';
    QUEUEX_$O_WR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for QUEUEX_$O_GD = '0';
  end;
end;

select: #access
begin
  #set
  begin
    $avail  <= $ARG1;
  end;
end;

--
-- Implementation (global module context)
-- VHDL signals required, both for mapping processes
-- and auxilliary signals.
--
#signals ($datatype = "logic")
begin
  --
  -- Implementation signals
  --

  foreach $p in $P.init do
  begin
    signal QUEUEX_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P.read do
  begin
    signal QUEUEX_$O_$p_RD: std_logic_vector[$datawidth];
    signal QUEUEX_$O_$p_RE: std_logic;
  end;
  foreach $p in $P.write do
  begin
    signal QUEUEX_$O_$p_WE: std_logic;
    signal QUEUEX_$O_$p_WR: std_logic_vector[$datawidth];
  end;
  foreach $p in $P do
  begin
    signal QUEUEX_$O_$p_ID: std_logic_vector[8];    
    signal QUEUEX_$O_$p_GD: std_logic;    
    signal QUEUEX_$O_$p_LOCKed: std_logic;
  end;
end;

#signals
begin
  type QUEUEX_$O_QRAM_TYPE is array[0 to ($depth-1)] 
      of std_logic_vector[$datawidth];
  signal QUEUEX_$O_QRAM: QUEUEX_$O_QRAM_type;
  signal QUEUEX_$O_EMPTY: std_logic;
  signal QUEUEX_$O_FULL: std_logic;
  signal QUEUEX_$O_LOCKED: std_logic;
  signal QUEUEX_$O_ADDR_RD: std_logic_vector[width($depth)-1];
  signal QUEUEX_$O_ADDR_WR: std_logic_vector[width($depth)-1];
  signal QUEUEX_$O_QRAM_ADDR_AUX: std_logic_vector[width($depth)-1];
  signal QUEUEX_$O_QRAM_RD_ADDR: std_logic_vector[width($depth)-1];
  signal QUEUEX_$O_QRAM_DIN_AUX: std_logic_vector[$datawidth];
  signal QUEUEX_$O_QRAM_DOUT_AUX: std_logic_vector[$datawidth];
  signal QUEUEX_$O_QRAM_WE_AUX: std_logic;
end;

QUEUEX_$O_IMPL_QRAM: #process
begin
  if $CLK then
  begin
    if QUEUEX_$O_QRAM_WE_AUX = '1' then
    begin
      QUEUEX_$O_QRAM[to_nat(QUEUEX_$O_QRAM_ADDR_AUX)] <= QUEUEX_$O_QRAM_DIN_AUX;
    end;
    QUEUEX_$O_QRAM_RD_ADDR <= QUEUEX_$O_QRAM_ADDR_AUX;
  end;
end;

#top
begin
  QUEUEX_$O_QRAM_DOUT_AUX <= QUEUEX_$O_QRAM[to_nat(QUEUEX_$O_QRAM_RD_ADDR)];
end;

IMPL_QUEUE_$O_SCHED:#process
begin
  variable wr_next: std_logic_vector[width($depth)-1];
  variable rd_next: std_logic_vector[width($depth)-1];
  if $CLK then
  begin
    if $RES then
    begin
      QUEUEX_$O_FULL <= '0';
      QUEUEX_$O_EMPTY <= '1';
      QUEUEX_$O_ADDR_RD <= to_logic(0,(width($depth)-1));
      QUEUEX_$O_ADDR_WR <= to_logic(0,(width($depth)-1));
      QUEUEX_$O_LOCKED <= '0';
      QUEUEX_$O_QRAM_WE_AUX <= '0';
      QUEUEX_$O_QRAM_ADDR_AUX <= to_logic(0,(width($depth)-1));
      QUEUEX_$O_QRAM_DIN_AUX <= to_logic(0,$datawidth);
      foreach $p in $P do
      begin
        QUEUEX_$O_$p_ID <= to_logic(index($processes,$p),8);
        QUEUEX_$O_$p_GD <= '1';
      end;
    end
    else if QUEUEX_$O_LOCKED = '0' then
    begin
      QUEUEX_$O_QRAM_ADDR_AUX <= QUEUEX_$O_ADDR_RD;
      wr_next := QUEUEX_$O_ADDR_WR + 1;
      rd_next := QUEUEX_$O_ADDR_RD + 1;
      sequence
      begin
        foreach $p in $P.init do
        begin
          if QUEUEX_$O_$p_INIT = '1'  then
          begin
            QUEUEX_$O_$p_GD <= '0';
          end;
        end;      
        foreach $p in $P.write do
        begin
          if QUEUEX_$O_$p_WE = '1' and QUEUEX_$O_FULL = '0' then
          begin
            if wr_next = QUEUEX_$O_ADDR_RD then 
            begin
              QUEUEX_$O_FULL <= '1'; 
            end;
            QUEUEX_$O_QRAM_DIN_AUX <= QUEUEX_$O_$p_WR;
            QUEUEX_$O_QRAM_ADDR_AUX <= QUEUEX_$O_ADDR_WR;
            QUEUEX_$O_QRAM_WE_AUX <= '1';
            QUEUEX_$O_ADDR_WR <= wr_next;
            QUEUEX_$O_LOCKED <= '1';
            QUEUEX_$O_$p_GD <= '0';
            QUEUEX_$O_EMPTY <= '0';            
          end;
        end;
        foreach $p in $P.read do
        begin
          if QUEUEX_$O_$p_RE = '1' and QUEUEX_$O_EMPTY = '0' then
          begin
            if rd_next = QUEUEX_$O_ADDR_WR then 
            begin
              QUEUEX_$O_EMPTY <= '1'; 
            end;
            QUEUEX_$O_ADDR_RD <= rd_next;
            QUEUEX_$O_LOCKED <= '1';
            QUEUEX_$O_$p_GD <= '0';
            QUEUEX_$O_FULL <= '0';
          end;
        end;
      end;
    end
    else
    begin
      QUEUEX_$O_LOCKED <= '0';
      QUEUEX_$O_QRAM_WE_AUX <= '0';
      foreach $p in $P do
      begin
        QUEUEX_$O_$p_GD <= '1';
      end;
    end;
  end;
end;

#top
begin
  foreach $p in $P.read do
  begin
    QUEUEX_$O_$p_RD <= QUEUEX_$O_QRAM_DOUT_AUX;
  end;
end;

#top ($select=1)
begin
  $avail <= not QUEUEX_$O_EMPTY;
end;
