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
--    $CREATED:     1.12.2010
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- A 4-phase dual-rail protocol with 4-bit encoding supporting 4-bit tokens:
--
--    STATE   D  D' ACK  COMPL
--    ---------------------
--    EMPTY   0  0   -0   0
--    DATA 1  1  0   +1   1   (VALID)
--    DATA 0  0  1   +1   1   (VALID)
--    ...
--    --
--  
--    (+): rising, (-): falling event
--
--  |SENDER|                       |RECEIVER|
--                  => D1,D2,D3,D4    
--                  => D1',D2',...
--                  ACK <=
--
--  Basic behaviour with EMPTY/DATA tokens:
--  
--    1. Send EMPTY, wait for ACK=0       1. Wait for EMPTY, then set ACK := 0
--    2. Send DATA, wait for ACK=1        2. Wait for VALID, store DATA 
--    3. goto 1                           3. Set ACK := 1, goto 1
--
--      
--
--
--    $ENDOFINFO
--

#version "develop.4-2";

--
-- parameters used, allowed and default values
--
#parameter
begin
  $class["external","internal"] <= "external";
  
  --
  -- Object parameter, can be changed
  --
  $datawidth <= 8;
  
  --
  -- Implementation parameters, not changeable
  --
  
  --
  -- Up to 16 4-bit frames
  --
  $cntwidth <= 4;
  
  --
  -- Dual-rail encoded port width and token width
  --
  $portwidth <= 8;
  $tokenwidth <= 4;
  
  --
  -- External class signals
  --
  $link_data_in;
  $link_ack_in;
  $link_data_out;
  $link_ack_out;
  
  $ARRAY [0,1]<= 0;
end;

--
-- Supported object methods
--
#methods
begin
  init ();
  start ();
  stop ();
  --
  -- write(data,err)
  --
  write(#rhs:logic[$datawidth],#lhs:bool);
  --
  -- read(data,err)
  --
  read(#lhs:logic[$datawidth],#lhs:bool);
  --
  -- external port interface (data_in,data_in_ack,
  --                          data_out,data_out_ack)
  --
  interface(#rhs:logic[$portwidth],#lhs:logic,
            #lhs:logic[$portwidth],#rhs:logic);
end;

#assert
begin
  size($P.init) >= 1;
  size($P.start) >= 1;
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal LINK_$O_INIT: out std_logic;
  end;
  foreach $p in $P.start do
  begin
    signal LINK_$O_START: out std_logic;
  end;
  foreach $p in $P.stop do
  begin
    signal LINK_$O_STOP: out std_logic;
  end;
  foreach $p in $P.read do
  begin
    signal LINK_$O_RE: out std_logic;
    signal LINK_$O_RD: in std_logic_vector[$datawidth];
    signal LINK_$O_RD_ERR: in std_logic;
  end;
  foreach $p in $P.write do
  begin
    signal LINK_$O_WE: out std_logic;
    signal LINK_$O_WR: out std_logic_vector[$datawidth];
    signal LINK_$O_WR_ERR: in std_logic;
  end;
  foreach $p in $P do
  begin
    signal LINK_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    LINK_$O_INIT => LINK_$O_$p_INIT;
  end;
  foreach $p in $P.start do
  begin
    LINK_$O_START => LINK_$O_$p_START;
  end;
  foreach $p in $P.stop do
  begin
    LINK_$O_STOP => LINK_$O_$p_STOP;
  end;
  foreach $p in $P.read do
  begin
    LINK_$O_RE => LINK_$O_$p_RE;
    LINK_$O_RD => LINK_$O_$p_RD;
    LINK_$O_RD_ERR => LINK_$O_$p_RD_ERR;
  end;
  foreach $p in $P.write do
  begin
    LINK_$O_WE => LINK_$O_$p_WE;
    LINK_$O_WR => LINK_$O_$p_WR;
    LINK_$O_WR_ERR => LINK_$O_$p_WR_ERR;
  end;
  foreach $p in $P do
  begin
    LINK_$O_GD => LINK_$O_$p_GD;
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
    LINK_$O_INIT <= LINK_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for LINK_$O_GD = '0';
  end;
end;

read: #access
begin
  #data
  begin
    LINK_$O_RE <= LINK_$O_GD when $ACC else '0';
    $ARG1 <= LINK_$O_RD when $ACC else 0;
    $ARG2 <= LINK_$O_RD_ERR when $ACC else '0';
  end;
  #control
  begin
    wait for LINK_$O_GD = '0';
  end;
end;

write: #access
begin
  #data
  begin
    LINK_$O_WE <= LINK_$O_GD when $ACC else '0';
    LINK_$O_WR <= $ARG1 when $ACC else 0;
    $ARG2 <= LINK_$O_WR_ERR when $ACC else '0';
  end;
  #control
  begin
    wait for LINK_$O_GD = '0';
  end;
end;


interface: #access
begin
  #set
  begin
    $link_data_in  <= $ARG1;
    $link_ack_in   <= $ARG2;
    $link_data_out <= $ARG3;
    $link_ack_out  <= $ARG4;
  end;
end;


start: #access
begin
  #data
  begin
    LINK_$O_START <= LINK_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for LINK_$O_GD = '0';
  end;
end;

stop: #access
begin
  #data
  begin
    LINK_$O_STOP <= LINK_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for LINK_$O_GD = '0';
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

  foreach $p in $P.init do
  begin
    signal LINK_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P.start do
  begin
    signal LINK_$O_$p_START: std_logic;    
  end;
  foreach $p in $P.stop do
  begin
    signal LINK_$O_$p_STOP: std_logic;    
  end;
  foreach $p in $P.read do
  begin
    signal LINK_$O_$p_RE: std_logic;
    signal LINK_$O_$p_RD: std_logic_vector[$datawidth];
    signal LINK_$O_$p_RD_ERR: std_logic;
  end;
  foreach $p in $P.write do
  begin
    signal LINK_$O_$p_WE: std_logic;
    signal LINK_$O_$p_WR: std_logic_vector[$datawidth];
    signal LINK_$O_$p_WR_ERR: std_logic;
  end;
  foreach $p in $P do
  begin
    signal LINK_$O_$p_GD: std_logic;    
    signal LINK_$O_$p_LOCKed: std_logic;
  end;
end;


#signals
begin
  signal LINK_$O_EN: std_logic;
  signal LINK_$O_RE: std_logic;
  signal LINK_$O_WE: std_logic;
  signal LINK_$O_AVAIL: std_logic;
  signal LINK_$O_BUSY: std_logic;
  signal LINK_$O_DIN_ACK: std_logic;
  signal LINK_$O_DIN_COMPL: std_logic;
  signal LINK_$O_DIN_EMPTY: std_logic;
  signal LINK_$O_DOUT_EMPTY: std_logic;
  signal LINK_$O_DOUT_ACK: std_logic;
  signal LINK_$O_DOUT_ACK_SAMPLED: std_logic;
  signal LINK_$O_DIN_REG: std_logic_vector[$datawidth];
  signal LINK_$O_DIN: std_logic[$tokenwidth];
  signal LINK_$O_DIN_CNT: std_logic_vector[$cntwidth];
  signal LINK_$O_DIN2: std_logic_vector[$portwidth];
  signal LINK_$O_DIN_SAMPLED: std_logic_vector[$portwidth];
  signal LINK_$O_DOUT_REG: std_logic_vector[$datawidth];
  signal LINK_$O_DOUT_SHIFT: std_logic_vector[$datawidth];
  signal LINK_$O_DOUT_CNT: std_logic_vector[$cntwidth];
  signal LINK_$O_DOUT2: std_logic_vector[$portwidth];
  signal LINK_$O_LOCKED: std_logic;
  type LINK_$O_STATES is {
    LINK_$O_S_EMPTY,
    LINK_$O_S_REQ,
    LINK_$O_S_SET
  };
  signal LINK_$O_state_in: LINK_$O_STATES;
  signal LINK_$O_state_in_next: LINK_$O_STATES;
  signal LINK_$O_state_out: LINK_$O_STATES;
  signal LINK_$O_state_out_next: LINK_$O_STATES;
end;

LINK_$O_IN_PROC_S:#process
begin
  if $CLK then
  begin
    if $RES or LINK_$O_EN = '0' then
    begin
      LINK_$O_state_in <= LINK_$O_S_EMPTY;
    end
    else
    begin
      LINK_$O_state_in <= LINK_$O_state_in_next;
    end;
  end;
end;

LINK_$O_IN_PROC_T:#process
begin
  case LINK_$O_state_in is
  begin
    when LINK_$O_S_EMPTY =>
    begin
      if LINK_$O_DIN_EMPTY = '1' then
      begin
        --
        -- Wait for empty token - reset state event
        -- Return to zero phase.
        --
        if LINK_$O_DIN_CNT = to_logic($datawidth/4,$cntwidth) then
          LINK_$O_state_in_next <= LINK_$O_S_REQ
        else
          LINK_$O_state_in_next <= LINK_$O_S_SET;       
      end
      else
      begin
        LINK_$O_state_in_next <= LINK_$O_S_EMPTY;
      end;
    end;
    when LINK_$O_S_SET =>
    begin
      if LINK_$O_DIN_COMPL = '1' then
      begin
        --
        -- Valid data token received
        --

        LINK_$O_state_in_next <= LINK_$O_S_EMPTY;
      end
      else
      begin
        LINK_$O_state_in_next <= LINK_$O_S_SET;
      end;    
    end;
    when LINK_$O_S_REQ =>
    begin
      if LINK_$O_RE = '1' then
      begin
        LINK_$O_state_in_next <= LINK_$O_S_SET;
      end
      else
      begin
        LINK_$O_state_in_next <= LINK_$O_S_REQ;
      end;          
    end;
  end;
end;

LINK_$O_IN_PROC_U:#process
begin
  LINK_$O_DIN_ACK <= '0';
  LINK_$O_AVAIL <= '0';
  case LINK_$O_state_in is
  begin
    when LINK_$O_S_EMPTY =>
    begin
      LINK_$O_DIN_ACK <= '1';
      if LINK_$O_DIN_EMPTY = '1' then
      begin
        --
        -- Wait for empty token - reset state event
        -- Return to zero phase.
        --
        LINK_$O_DIN_ACK <= '0';
      end;
    end;
    when LINK_$O_S_SET =>
    begin
      if LINK_$O_DIN_COMPL = '1' then
      begin
        --
        -- Valid data token received
        --

        --
        -- Send acknowledge
        --
        LINK_$O_DIN_ACK <= '1';
      end;
    end;
    when LINK_$O_S_REQ =>
    begin
      --
      -- Wait for object read request...
      --
      LINK_$O_AVAIL <= '1';  -- read event
    end;
  end;
end;

LINK_$O_IN_PROC_V:#process
begin
  if $CLK then
  begin
    if $RES or LINK_$O_EN = '0' then
    begin
      LINK_$O_DIN_REG <= to_logic(0,$datawidth);
      LINK_$O_DIN_CNT <= to_logic(0,$cntwidth);
    end
    else
    begin
      case LINK_$O_state_in is
      begin
        when LINK_$O_S_SET =>
        begin
          if LINK_$O_DIN_COMPL = '1' then
          begin
            --
            -- Valid data token received
            --
            
            --
            -- Shift left, MSB first, LSB last
            --
            LINK_$O_DIN_REG[3 downto 0] <= LINK_$O_DIN;
            LINK_$O_DIN_REG[($datawidth-1) downto 4] <=
              LINK_$O_DIN_REG[($datawidth-5) downto 0];            
            LINK_$O_DIN_CNT <= LINK_$O_DIN_CNT + 1;
          end;
        end;
        when LINK_$O_S_REQ =>
        begin
          LINK_$O_DIN_CNT <= to_logic(0,$cntwidth);
        end;
        when others => null;
      end;
    end;
  end;
end;


LINK_$O_OUT_PROC_S:#process
begin
  if $CLK then
  begin
    if $RES or LINK_$O_EN = '0' then
    begin
      LINK_$O_state_out <= LINK_$O_S_EMPTY;
    end
    else
    begin
      LINK_$O_state_out <= LINK_$O_state_out_next;
    end;
  end;
end;

LINK_$O_OUT_PROC_T:#process
begin
  case LINK_$O_state_out is
  begin
    when LINK_$O_S_EMPTY =>
    begin
      if LINK_$O_DOUT_ACK_SAMPLED = '0' then
      begin
        if LINK_$O_DOUT_CNT = to_logic(0,$cntwidth) then
        begin
          LINK_$O_state_out_next <= LINK_$O_S_REQ;
        end
        else
        begin
          LINK_$O_state_out_next <= LINK_$O_S_SET;
        end;
      end
      else
      begin
        LINK_$O_state_out_next <= LINK_$O_S_EMPTY;
      end;
    end;
    when LINK_$O_S_REQ =>
    begin
      if LINK_$O_WE = '1' then
      begin
        LINK_$O_state_out_next <= LINK_$O_S_SET;
      end
      else
      begin
        LINK_$O_state_out_next <= LINK_$O_S_REQ;
      end;
    end;
    when LINK_$O_S_SET =>
    begin
      if LINK_$O_DOUT_ACK_SAMPLED = '1' then
      begin
        --
        -- Got acknowledge from receiver, send empty token
        --
        LINK_$O_state_out_next <= LINK_$O_S_EMPTY;
      end
      else
      begin
        LINK_$O_state_out_next <= LINK_$O_S_SET;
      end;
    end;
    when others => 
    begin
      LINK_$O_state_out_next <= LINK_$O_S_EMPTY;
    end;
  end;
end;


LINK_$O_OUT_PROC_U:#process
begin
  LINK_$O_DOUT_EMPTY <= '0';
  case LINK_$O_state_out is
  begin
    when LINK_$O_S_EMPTY =>
    begin
      LINK_$O_DOUT_EMPTY <= '1';
    end;
    when LINK_$O_S_REQ =>
    begin
      LINK_$O_DOUT_EMPTY <= '1';
    end;
    when others => null;
  end;
end;

LINK_$O_OUT_PROC_V:#process
begin
  if $CLK then
  begin
    if $RES or LINK_$O_EN = '0' then
    begin
      LINK_$O_DOUT_CNT <= to_logic(0,$cntwidth);
      LINK_$O_DOUT_SHIFT <= to_logic(0,$datawidth);
      LINK_$O_BUSY <= '0';
    end
    else
    begin
      case LINK_$O_state_out is
      begin
        when LINK_$O_S_REQ =>
        begin
          LINK_$O_BUSY <= '0';
          if LINK_$O_WE = '1' then
          begin
            LINK_$O_BUSY <= '1';
            LINK_$O_DOUT_SHIFT <= LINK_$O_DOUT_REG;
          end;
        end;
        when LINK_$O_S_SET =>
        begin
          if LINK_$O_DOUT_ACK_SAMPLED = '1' then
          begin
            LINK_$O_DOUT_CNT <= LINK_$O_DOUT_CNT + 1;
            --
            -- Shift left, MSB first, LSB last
            --
            LINK_$O_DOUT_SHIFT[3 downto 0] <= 0b0000;
            LINK_$O_DOUT_SHIFT[($datawidth-1) downto 4] <=
              LINK_$O_DOUT_SHIFT[($datawidth-5) downto 0];            
            if LINK_$O_DOUT_CNT = to_logic(($datawidth/4)-1,$cntwidth) then 
            begin
              LINK_$O_DOUT_CNT <= to_logic(0,$cntwidth);
            end;
          end;
        end;
        when others => null;
      end;
    end;
  end;
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
LINK_$O_SCHED: #process 
begin
  if $CLK then
  begin
    if $RES then
    begin
      foreach $p in $P do
      begin
        LINK_$O_$p_GD <= '1';
      end;
      foreach $p in $P do
      begin
        LINK_$O_$p_LOCKed <= '0';
      end;
      LINK_$O_EN <= '0';
      LINK_$O_RE <= '0';
      LINK_$O_WE <= '0';
      LINK_$O_LOCKED <= '0';
      LINK_$O_DOUT_REG <= to_logic(0,$datawidth);
    end
    else if LINK_$O_LOCKED = '0' then
    begin
      sequence
      begin
        foreach $p in $P.init do
        begin
          if LINK_$O_$p_INIT = '1'  then
          begin
            LINK_$O_$p_GD <= '0';
            LINK_$O_LOCKED <= '1';
            foreach $l in $P do
            begin
              if LINK_$O_$l_LOCKed = '1' then
              begin
                LINK_$O_$l_LOCKed <= '0';
                LINK_$O_$l_GD <= '0';
              end;
            end;
          end;
        end;
        foreach $p in $P.start do
        begin
          if LINK_$O_$p_START = '1' then
          begin
            LINK_$O_LOCKED <= '1';
            LINK_$O_$p_GD <= '0';
            LINK_$O_EN <= '1';
          end;
        end;
        foreach $p in $P.stop do
        begin
          if LINK_$O_$p_STOP = '1' then
          begin
            LINK_$O_LOCKED <= '1';
            LINK_$O_$p_GD <= '0';
            LINK_$O_EN <= '0';
          end;
        end;
        foreach $p in $P.read do
        begin
          if LINK_$O_$p_RE = '1' and LINK_$O_$p_LOCKed  = '1' and LINK_$O_AVAIL = '1' then
          begin
            LINK_$O_$p_RD <= LINK_$O_DIN_REG;
            LINK_$O_$p_RD_ERR <= '0';
            LINK_$O_$p_GD <= '0';
            LINK_$O_$p_LOCKed <= '0';
            LINK_$O_RE <= '1';
            LINK_$O_LOCKED <= '1';
          end;
        end;
        foreach $p in $P.write do
        begin
          if LINK_$O_$p_WE = '1' and LINK_$O_$p_LOCKed  = '1' and LINK_$O_BUSY = '0' then
          begin
            LINK_$O_$p_WR_ERR <= '0';
            LINK_$O_$p_GD <= '0';
            LINK_$O_$p_LOCKed <= '0';
            LINK_$O_LOCKED <= '1';
          end;
        end;
        foreach $p in $P.read do
        begin
          if LINK_$O_$p_RE = '1' and LINK_$O_$p_LOCKed  = '0'  then
          begin
            if LINK_$O_AVAIL = '1' then
            begin
              LINK_$O_$p_RD <= LINK_$O_DIN_REG;
              LINK_$O_$p_RD_ERR <= '0';
              LINK_$O_$p_GD <= '0';
              LINK_$O_RE <= '1'; 
              LINK_$O_LOCKED <= '1';         
            end
            else
            begin
              LINK_$O_$p_LOCKed <= '1';
            end;
          end;
        end;
        foreach $p in $P.write do
        begin
          if LINK_$O_$p_WE = '1' and LINK_$O_$p_LOCKed  = '0' and LINK_$O_BUSY = '0' then
          begin
            LINK_$O_DOUT_REG <= LINK_$O_$p_WR;
            LINK_$O_WE <= '1';
            LINK_$O_LOCKED <= '1';
            LINK_$O_$p_LOCKed <= '1';
          end;
        end;
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        LINK_$O_$p_GD <= '1';
      end;
      LINK_$O_RE <= '0';
      LINK_$O_WE <= '0';
      LINK_$O_LOCKED <= '0';
    end;
  end;
end;

LINK_$O_SAMPLER1:#process
begin
  variable samples1 : std_logic_vector[$portwidth];
  variable samples2 : std_logic_vector[$portwidth];
  if $CLK then
  begin
    if $RES then
    begin
      samples1 := to_logic(0,$portwidth);
      samples2 := to_logic(0,$portwidth);
      LINK_$O_DIN_SAMPLED <= to_logic(0,$portwidth);
    end
    else
    begin
      samples2 := samples1;
      samples1 := LINK_$O_DIN2;
    end;
    
    if samples1 = samples2  then
    begin
      LINK_$O_DIN_SAMPLED <= samples1;
    end;
  end;
end;

LINK_$O_SAMPLER2:#process
begin
  variable samples : std_logic_vector[2];
  if $CLK then
  begin
    if $RES then
    begin
      samples := 0b00;
      LINK_$O_DOUT_ACK_SAMPLED <= '0';
    end
    else
    begin
      samples[1] := samples[0];
      samples[0] := LINK_$O_DOUT_ACK;
    end;
    
    if samples = 0b00  then
      LINK_$O_DOUT_ACK_SAMPLED <= '0';
    if samples = 0b11 then
      LINK_$O_DOUT_ACK_SAMPLED <= '1';
  end;
end;

--
-- Token width = 4, portwidth = 8 bit
--
LINK_$O_DECODER:#process
begin
  LINK_$O_DIN_COMPL <= (LINK_$O_DIN_SAMPLED[0] xor LINK_$O_DIN_SAMPLED[1]) and
                       (LINK_$O_DIN_SAMPLED[2] xor LINK_$O_DIN_SAMPLED[3]) and 
                       (LINK_$O_DIN_SAMPLED[4] xor LINK_$O_DIN_SAMPLED[5]) and
                       (LINK_$O_DIN_SAMPLED[6] xor LINK_$O_DIN_SAMPLED[7]);
  LINK_$O_DIN_EMPTY <= not LINK_$O_DIN_SAMPLED[0] and not LINK_$O_DIN_SAMPLED[1] and
                       not LINK_$O_DIN_SAMPLED[2] and not LINK_$O_DIN_SAMPLED[3] and
                       not LINK_$O_DIN_SAMPLED[4] and not LINK_$O_DIN_SAMPLED[5] and
                       not LINK_$O_DIN_SAMPLED[6] and not LINK_$O_DIN_SAMPLED[7];
  LINK_$O_DIN[0] <= LINK_$O_DIN_SAMPLED[0];
  LINK_$O_DIN[1] <= LINK_$O_DIN_SAMPLED[2];
  LINK_$O_DIN[2] <= LINK_$O_DIN_SAMPLED[4];
  LINK_$O_DIN[3] <= LINK_$O_DIN_SAMPLED[6];
end;

LINK_$O_ENCODER:#process
begin
  if LINK_$O_DOUT_EMPTY = '1' then
  begin
    LINK_$O_DOUT2 <= to_logic(0,$portwidth);
  end
  else
  begin
    LINK_$O_DOUT2[0] <= LINK_$O_DOUT_SHIFT[$datawidth-4];
    LINK_$O_DOUT2[1] <= not LINK_$O_DOUT_SHIFT[$datawidth-4];
    LINK_$O_DOUT2[2] <= LINK_$O_DOUT_SHIFT[$datawidth-3];
    LINK_$O_DOUT2[3] <= not LINK_$O_DOUT_SHIFT[$datawidth-3];
    LINK_$O_DOUT2[4] <= LINK_$O_DOUT_SHIFT[$datawidth-2];
    LINK_$O_DOUT2[5] <= not LINK_$O_DOUT_SHIFT[$datawidth-2];
    LINK_$O_DOUT2[6] <= LINK_$O_DOUT_SHIFT[$datawidth-1];
    LINK_$O_DOUT2[7] <= not LINK_$O_DOUT_SHIFT[$datawidth-1];
  end;
end;

#top
begin
  LINK_$O_DIN2 <= $link_data_in;
  $link_data_out <= LINK_$O_DOUT2;
  $link_ack_in <= LINK_$O_DIN_ACK;
  LINK_$O_DOUT_ACK <= $link_ack_out;
end;
