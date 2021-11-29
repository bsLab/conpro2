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
--    $CREATED:     15.12.2008
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements serial communication (UART) object
--
--
--    $ENDOFINFO
--

#version "2.17";

--
-- parameters used, allowed and default values
--
#parameter
begin
  $baud with filtered and sorted;
  $datawidth <= 8;
  $rxsig;
  $txsig;
  $rxact[0,1] <= 1;
  $txact[0,1] <= 1;
  $wrmode["sync","async"] <= "sync";
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
  baud(#rhs:natural);
  --
  -- write(data,err)
  --
  write(#rhs:logic[$datawidth],#lhs:bool);
  --
  -- read(data,err)
  --
  read(#lhs:logic[$datawidth],#lhs:bool);
  --
  -- With additional timeout management (timeout => err=true)
  -- readtmo(data,err)
  --
  readtmo(#lhs:logic[$datawidth],#lhs:bool);
  --
  -- port(rx,tx)
  --
  interface(#rhs:logic,#lhs:logic);
  --
  -- Select activity level of rx/tx
  --
  level(#rhs:natural,#rhs:natural);
end;

#assert
begin
  size($P.init) >= 1;
  size($P.start) >= 1;
  size($P.baud) >= 1;
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal UART_$O_INIT: out std_logic;
  end;
  foreach $p in $P.start do
  begin
    signal UART_$O_START: out std_logic;
  end;
  foreach $p in $P.stop do
  begin
    signal UART_$O_STOP: out std_logic;
  end;
  foreach $p in $P.read or $P.readtmo do
  begin
    signal UART_$O_RD: in std_logic_vector[$datawidth];
    signal UART_$O_RD_ERR: in std_logic;
  end;
  foreach $p in $P.read do
  begin
    signal UART_$O_RE: out std_logic;
  end;
  foreach $p in $P.readtmo do
  begin
    signal UART_$O_RTE: out std_logic;
  end;
  foreach $p in $P.write do
  begin
    signal UART_$O_WE: out std_logic;
    signal UART_$O_WR: out std_logic_vector[$datawidth];
    signal UART_$O_WR_ERR: in std_logic;
  end;
  foreach $p in $P.baud do
  begin
    signal UART_$O_BAUD_SET: out std_logic;
    signal UART_$O_BAUD: out std_logic_vector[index_width($baud)];
  end;
  foreach $p in $P do
  begin
    signal UART_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    UART_$O_INIT => UART_$O_$p_INIT;
  end;
  foreach $p in $P.start do
  begin
    UART_$O_START => UART_$O_$p_START;
  end;
  foreach $p in $P.stop do
  begin
    UART_$O_STOP => UART_$O_$p_STOP;
  end;
  foreach $p in $P.read or $P.readtmo do
  begin
    UART_$O_RD => UART_$O_$p_RD;
    UART_$O_RD_ERR => UART_$O_$p_RD_ERR;
  end;
  foreach $p in $P.read do
  begin
    UART_$O_RE => UART_$O_$p_RE;
  end;
  foreach $p in $P.readtmo do
  begin
    UART_$O_RTE => UART_$O_$p_RTE;
  end;
  foreach $p in $P.write do
  begin
    UART_$O_WE => UART_$O_$p_WE;
    UART_$O_WR => UART_$O_$p_WR;
    UART_$O_WR_ERR => UART_$O_$p_WR_ERR;
  end;
  foreach $p in $P.baud do
  begin
    UART_$O_BAUD_SET => UART_$O_$p_BAUD_SET;
    UART_$O_BAUD => UART_$O_$p_BAUD;
  end;
  foreach $p in $P do
  begin
    UART_$O_GD => UART_$O_$p_GD;
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
    UART_$O_INIT <= UART_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for UART_$O_GD = '0';
  end;
end;

read: #access
begin
  #data
  begin
    UART_$O_RE <= UART_$O_GD when $ACC else '0';
    $ARG1 <= UART_$O_RD when $ACC else 0;
    $ARG2 <= UART_$O_RD_ERR when $ACC else '0';
  end;
  #control
  begin
    wait for UART_$O_GD = '0';
  end;
end;

readtmo: #access
begin
  #data
  begin
    UART_$O_RTE <= UART_$O_GD when $ACC else '0';
    $ARG1 <= UART_$O_RD when $ACC else 0;
    $ARG2 <= UART_$O_RD_ERR when $ACC else '0';
  end;
  #control
  begin
    wait for UART_$O_GD = '0';
  end;
end;

write: #access
begin
  #data
  begin
    UART_$O_WE <= UART_$O_GD when $ACC else '0';
    UART_$O_WR <= $ARG1 when $ACC else 0;
    $ARG2 <= UART_$O_WR_ERR when $ACC else '0';
  end;
  #control
  begin
    wait for UART_$O_GD = '0';
  end;
end;

baud: #access
begin
  #set
  begin
    $baud <= $ARG1;
    print("Achieved baud rate accuracy [bit/s]: ",
           "[actual = ",to_string($clock / (($clock / (16 * $ARG1))*16)),"] ",
           "[requested = ",to_string($ARG1),"] ",
           "[error = ",to_string((((($clock / (($clock / (16 * $ARG1))*16))*1000)/$ARG1)-1000)/10),".",
                       to_string((((($clock / (($clock / (16 * $ARG1))*16))*1000)/$ARG1)-1000)-
                                 (((((($clock / (($clock / (16 * $ARG1))*16))*1000)/$ARG1)-1000)/10)*10))," %]"
            );
  end;
  #data
  begin
    UART_$O_BAUD_SET <= UART_$O_GD when $ACC else '0';
    UART_$O_BAUD <= index($baud,$ARG1) when $ACC else 0;
  end;
  #control
  begin
    wait for UART_$O_GD = '0';
  end;
end;

interface: #access
begin
  #set
  begin
    $rxsig <= $ARG1;
    $txsig <= $ARG2;
  end;
end;

level: #access
begin
  #set
  begin
    $rxact <= $ARG1;
    $txact <= $ARG2;
  end;
end;

start: #access
begin
  #data
  begin
    UART_$O_START <= UART_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for UART_$O_GD = '0';
  end;
end;

stop: #access
begin
  #data
  begin
    UART_$O_STOP <= UART_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for UART_$O_GD = '0';
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
    signal UART_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P.start do
  begin
    signal UART_$O_$p_START: std_logic;    
  end;
  foreach $p in $P.stop do
  begin
    signal UART_$O_$p_STOP: std_logic;    
  end;
  foreach $p in $P.read or $P.readtmo do
  begin
    signal UART_$O_$p_RD: std_logic_vector[$datawidth];
    signal UART_$O_$p_RD_ERR: std_logic;
  end;
  foreach $p in $P.read do
  begin
    signal UART_$O_$p_RE: std_logic;
  end;
  foreach $p in $P.readtmo do
  begin
    signal UART_$O_$p_RTE: std_logic;
  end;
  foreach $p in $P.write do
  begin
    signal UART_$O_$p_WE: std_logic;
    signal UART_$O_$p_WR: std_logic_vector[$datawidth];
    signal UART_$O_$p_WR_ERR: std_logic;
  end;
  foreach $p in $P.baud do
  begin
    signal UART_$O_$p_BAUD_SET: std_logic;
    signal UART_$O_$p_BAUD: std_logic_vector[index_width($baud)];
  end;
  foreach $p in $P do
  begin
    signal UART_$O_$p_GD: std_logic;    
    signal UART_$O_$p_LOCKed: std_logic;
  end;
end;


#signals
begin
  signal UART_$O_RXD: std_logic;
  signal UART_$O_TXD: std_logic;
  signal UART_$O_RX_EN: std_logic;
  signal UART_$O_TX_EN: std_logic;
  signal UART_$O_TX_DATA: std_logic_vector[$datawidth];
  signal UART_$O_RX_DATA: std_logic_vector[$datawidth];  
  signal UART_$O_BAUD_COUNT: std_logic_vector[width(($clock / (16 * min($baud)))-1)];
  signal UART_$O_BAUD16: std_logic;
  signal UART_$O_RX_RE: std_logic;
  signal UART_$O_BUSY: std_logic;
  
  signal UART_$O_RX_SAMPLED: std_logic;
  signal UART_$O_RX_COMPL: std_logic;
  signal UART_$O_RX_ERR: std_logic;
  signal UART_$O_RX_BIT_PHASE: std_logic_vector[4];
  signal UART_$O_RX_BIT_COUNT: std_logic_vector[width($datawidth)+1];
  signal UART_$O_RX_SHIFT: std_logic_vector[$datawidth];
  
  signal UART_$O_TX_COMPL: std_logic;
  signal UART_$O_TX_DRE: std_logic;
  signal UART_$O_TX_WE: std_logic;
  signal UART_$O_TX_ERR: std_logic;
  signal UART_$O_TX_TICK: std_logic;
  signal UART_$O_TX_BIT_COUNT: std_logic_vector[width($datawidth)+1];
  signal UART_$O_TX_SHIFT: std_logic_vector[$datawidth+1];
  
  signal UART_$O_TIMEOUT: std_logic_vector[20]; 
end;

#signals ($wrmode="sync")
begin
  foreach $p in $P.write do
  begin
    signal UART_$O_$p_TX_AWAIT: std_logic;
  end;
end;

UART_$O_BAUD_GEN: #process
begin
  variable baud_cnt: std_logic_vector[width(($clock / (16 * min($baud)))-1)];
  if $CLK then
  begin
    if $RES then
    begin
      baud_cnt := to_logic(0,width(($clock / (16 * min($baud)))-1));
      UART_$O_BAUD16 <= '0';
    end
    else if baud_cnt = to_logic(0,width(($clock / (16 * min($baud)))-1)) then
    begin
      UART_$O_BAUD16 <= '1';
      baud_cnt := UART_$O_BAUD_COUNT;
    end
    else
    begin
      baud_cnt := baud_cnt - 1;
      UART_$O_BAUD16 <= '0';
    end;
  end;
end;

UART_$O_RX_SAMPLER: #process
begin
  variable samples : std_logic_vector[2];
  if $CLK then
  begin
    if $RES then
    begin
      samples := "11";
      UART_$O_RX_SAMPLED <= '1';
    end
    else if UART_$O_BAUD16 = '1' then
    begin
      samples[1] := samples[0];
      samples[0] := UART_$O_RXD;
    end;
    if samples = "00" then
        UART_$O_RX_SAMPLED <= '0';
    if samples = "11" then
        UART_$O_RX_SAMPLED <= '1';
  end;
end;

UART_$O_RX_CONTROLLER: #process
begin
  if $CLK then
  begin
    if $RES or UART_$O_RX_EN = '0' then
    begin
      UART_$O_RX_COMPL <= '0';
      UART_$O_RX_ERR <= '0';
      UART_$O_RX_BIT_PHASE <= to_logic(0,4);
      UART_$O_RX_SHIFT <= to_logic(0,$datawidth);
      UART_$O_RX_BIT_COUNT <= to_logic(0,width($datawidth)+1);
    end
    else 
    begin
      if UART_$O_RX_RE = '1' then
      begin
        UART_$O_RX_COMPL <= '0';
        UART_$O_RX_ERR <= '0';      
      end;

      if UART_$O_BAUD16 = '1' then
      begin
        if UART_$O_RX_BIT_COUNT = to_logic(0,width($datawidth)+1) and
           (UART_$O_RX_SAMPLED = '1' or
            UART_$O_RX_BIT_PHASE = to_logic(7,4))
        then
            UART_$O_RX_BIT_PHASE <= to_logic(0,4)
        else
            UART_$O_RX_BIT_PHASE <= UART_$O_RX_BIT_PHASE + 1;
        if UART_$O_RX_BIT_COUNT = to_logic(0,width($datawidth)+1) then
        begin
          if UART_$O_RX_BIT_PHASE = to_logic(7,4) then
              UART_$O_RX_BIT_COUNT <= UART_$O_RX_BIT_COUNT + 1;
        end
        else if UART_$O_RX_BIT_PHASE = to_logic(15,4) then
        begin
          UART_$O_RX_BIT_COUNT <= UART_$O_RX_BIT_COUNT + 1;
          if (UART_$O_RX_BIT_COUNT = to_logic($datawidth+1,width($datawidth)+1)) 
          then -- Stop bit
          begin
            UART_$O_RX_BIT_COUNT <= to_logic(0,width($datawidth)+1);
            if UART_$O_RX_EN = '1' then
            begin
              UART_$O_RX_COMPL <= '1'; -- Now UART Receive complete
              UART_$O_RX_ERR <= (not UART_$O_RX_SAMPLED) or UART_$O_RX_COMPL;
              if UART_$O_RX_COMPL = '0' or UART_$O_RX_RE = '1' then
              begin
                --
                -- Receiver in read out mode
                --
                UART_$O_RX_DATA <= UART_$O_RX_SHIFT[($datawidth-1) downto 0];
              end;
            end;
          end
          else
          begin
            UART_$O_RX_SHIFT[($datawidth-2) downto 0] <= UART_$O_RX_SHIFT[$datawidth-1 downto 1];
            UART_$O_RX_SHIFT[$datawidth-1] <= UART_$O_RX_SAMPLED;        
          end;
        end;
      end;
    end;
  end;
end;

UART_$O_TX_SAMPLER: #process
begin
  variable tx_cnt : std_logic_vector[4];
  if $CLK then
  begin
    if $RES then
    begin
      tx_cnt := to_logic(0,4);
      UART_$O_TX_TICK <= '0';
    end 
    else if UART_$O_BAUD16 = '1' then
    begin
      if tx_cnt = to_logic(15,4) then
        UART_$O_TX_TICK <= '1';
      tx_cnt := tx_cnt + 1;
    end
    else
      UART_$O_TX_TICK <= '0';
  end;
end;

UART_$O_TX_CONTROLLER: #process
begin
  if $CLK then
  begin
    if $RES or UART_$O_TX_EN = '0' then
    begin
      UART_$O_TX_COMPL <= '0';
      UART_$O_TX_DRE <= '1';
      UART_$O_TX_BIT_COUNT <= to_logic(0,width($datawidth)+1);
      UART_$O_TX_SHIFT <= to_logic(0,$datawidth+1);
      UART_$O_TXD <= '1';
      UART_$O_TX_ERR <= '0';
    end
    else
    begin
      if UART_$O_TX_WE = '1' and UART_$O_TX_EN = '1' then
      begin
        --
        -- Load data word and start TX-FSM.
        --
        UART_$O_TX_DRE <= '0';
        UART_$O_TX_COMPL <= '0';
        UART_$O_TX_SHIFT[($datawidth-1) downto 0] <= UART_$O_TX_DATA;
        UART_$O_TX_SHIFT[$datawidth] <= '0';
      end
      else if UART_$O_TX_WE = '1' and UART_$O_TX_EN = '0' then
      begin
        UART_$O_TX_ERR <= '1';
      end;

      if UART_$O_TX_TICK = '1' then
      begin
        case UART_$O_TX_BIT_COUNT is
        begin
          when to_logic(0,width($datawidth)+1) =>
          begin
            if UART_$O_TX_DRE = '0' then
              UART_$O_TX_BIT_COUNT <= to_logic(1,width($datawidth)+1);
            UART_$O_TXD <= '1';
          end;
          when to_logic(1,width($datawidth)+1) =>
          begin
            --
            -- Start bit
            --
            UART_$O_TXD <= '0';
            UART_$O_TX_BIT_COUNT <= to_logic(2,width($datawidth)+1);
          end;
          when others =>
          begin
            UART_$O_TX_BIT_COUNT <= UART_$O_TX_BIT_COUNT + 1;
            if UART_$O_TX_BIT_COUNT = to_logic(($datawidth+1),width($datawidth)+1) then
            begin
              --
              -- TX complete.
              -- Next data word can be sent.
              --
              UART_$O_TX_DRE <= '1';
              UART_$O_TX_BIT_COUNT <= to_logic(0,width($datawidth)+1);
              UART_$O_TX_COMPL <= '1';
            end;
            UART_$O_TXD <= UART_$O_TX_SHIFT[0];
            UART_$O_TX_SHIFT[($datawidth-1) downto 0] <= UART_$O_TX_SHIFT[$datawidth downto 1];
          end;
        end;
      end;
    end;
  end;
end;

--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
UART_$O_SCHED: #process 
begin
  if $CLK then
  begin
    if $RES then
    begin
      UART_$O_BUSY <= '0';
      foreach $p in $P do
      begin
        UART_$O_$p_GD <= '1';
      end;
      foreach $p in $P do
      begin
        UART_$O_$p_LOCKed <= '0';
      end;
      UART_$O_RX_EN <= '0';
      UART_$O_TX_EN <= '0';
      UART_$O_RX_RE <= '0';
      UART_$O_TX_WE <= '0';
      UART_$O_BAUD_COUNT <= to_logic(($clock / (16 * nth($baud,1)))-1,
                                     width(($clock / (16 * min($baud)))-1));
    end
    else if UART_$O_BUSY = '0' then
    begin
      sequence
      begin
        foreach $p in $P.init do
        begin
          if UART_$O_$p_INIT = '1'  then
          begin
            UART_$O_$p_GD <= '0';
            UART_$O_BUSY <= '1';
            foreach $l in $P do
            begin
              if UART_$O_$l_LOCKed = '1' then
              begin
                UART_$O_$l_LOCKed <= '0';
                UART_$O_$l_GD <= '0';
              end;
            end;
          end;
        end;
        foreach $p in $P.start do
        begin
          if UART_$O_$p_START = '1' then
          begin
            UART_$O_BUSY <= '1';
            UART_$O_$p_GD <= '0';
            UART_$O_RX_EN <= '1';
            UART_$O_TX_EN <= '1';
          end;
        end;
        foreach $p in $P.stop do
        begin
          if UART_$O_$p_STOP = '1' then
          begin
            UART_$O_BUSY <= '1';
            UART_$O_$p_GD <= '0';
            UART_$O_RX_EN <= '0';
            UART_$O_TX_EN <= '0';
          end;
        end;
        foreach $p in $P.baud do
        begin
          if UART_$O_$p_BAUD_SET = '1' then
          begin
            UART_$O_$p_GD <= '0';
            UART_$O_BUSY <= '1';
            foreach $b in $baud do
            begin
              if UART_$O_$p_BAUD = index($baud,$b) then
              begin
                UART_$O_BAUD_COUNT <= 
                  to_logic(($clock / (16 * $b))-1,
                           width(($clock / (16 * min($baud)))-1));
              end;
            end;
          end;
        end;
        foreach $p in $P.read do
        begin
          if UART_$O_$p_RE = '1' and UART_$O_$p_LOCKed  = '1' and UART_$O_RX_COMPL = '1' then
          begin
            UART_$O_$p_RD <= UART_$O_RX_DATA;
            UART_$O_$p_RD_ERR <= UART_$O_RX_ERR;
            UART_$O_$p_GD <= '0';
            UART_$O_$p_LOCKed <= '0';
            UART_$O_RX_RE <= '1';
            UART_$O_BUSY <= '1';
          end;
        end;
        foreach $p in $P.readtmo do
        begin
          if UART_$O_$p_RTE = '1' and UART_$O_$p_LOCKed  = '1'then
          begin
            if  UART_$O_RX_COMPL = '1' then
            begin
              UART_$O_$p_RD <= UART_$O_RX_DATA;
              UART_$O_$p_RD_ERR <= UART_$O_RX_ERR;
              UART_$O_$p_GD <= '0';
              UART_$O_$p_LOCKed <= '0';
              UART_$O_RX_RE <= '1';
              UART_$O_BUSY <= '1';
            end
            else
            begin
              UART_$O_TIMEOUT <= UART_$O_TIMEOUT - 1;
              if UART_$O_TIMEOUT = to_logic(0,20) then
              begin
                -- timeout occured, return with err=true
                UART_$O_$p_RD <= to_logic(0,$datawidth);
                UART_$O_$p_RD_ERR <= '1';
                UART_$O_$p_GD <= '0';
                UART_$O_$p_LOCKed <= '0';                
              end;
            end;
          end;
        end;
        foreach $p in $P.write do
        begin
          if UART_$O_$p_WE = '1' and UART_$O_$p_LOCKed  = '1' and UART_$O_TX_COMPL = '1' then
          begin
            UART_$O_$p_WR_ERR <= UART_$O_TX_ERR;
            UART_$O_$p_GD <= '0';
            UART_$O_$p_LOCKed <= '0';
            UART_$O_BUSY <= '1';
          end;
        end;
        foreach $p in $P.read do
        begin
          if UART_$O_$p_RE = '1' and UART_$O_$p_LOCKed  = '0'  then
          begin
            if UART_$O_RX_COMPL = '1' then
            begin
              UART_$O_$p_RD <= UART_$O_RX_DATA;
              UART_$O_$p_RD_ERR <= UART_$O_RX_ERR;
              UART_$O_$p_GD <= '0';
              UART_$O_RX_RE <= '1'; 
              UART_$O_BUSY <= '1';         
            end
            else
            begin
              UART_$O_$p_LOCKed <= '1';
            end;
          end;
        end;
        foreach $p in $P.readtmo do
        begin
          if UART_$O_$p_RTE = '1' and UART_$O_$p_LOCKed  = '0'  then
          begin
            if UART_$O_RX_COMPL = '1' then
            begin
              UART_$O_$p_RD <= UART_$O_RX_DATA;
              UART_$O_$p_RD_ERR <= UART_$O_RX_ERR;
              UART_$O_$p_GD <= '0';
              UART_$O_RX_RE <= '1'; 
              UART_$O_BUSY <= '1';         
            end
            else
            begin
              UART_$O_$p_LOCKed <= '1';
              --
              -- Fixed timeout, clk=10MHz => tmo=100ms
              --
              UART_$O_TIMEOUT <= to_logic(1000000,20);
            end;
          end;
        end;
        foreach $p in $P.write do
        begin
          if UART_$O_$p_WE = '1' and UART_$O_$p_LOCKed  = '0' and UART_$O_TX_DRE = '1' then
          begin
            UART_$O_TX_DATA <= UART_$O_$p_WR;
            UART_$O_TX_WE <= '1';
            UART_$O_BUSY <= '1';
            UART_$O_$p_LOCKed <= '1';
          end;
        end;
      end;
    end
    else
    begin
      foreach $p in $P do
      begin
        UART_$O_$p_GD <= '1';
      end;
      UART_$O_RX_RE <= '0';
      UART_$O_TX_WE <= '0';
      UART_$O_BUSY <= '0';
    end;
    
  end;
end;

#top
begin
  if $rxact = 1 then
    UART_$O_RXD <= $rxsig;
  if $rxact = 0 then
    UART_$O_RXD <= not $rxsig;
  if $txact = 1 then
    $txsig <= UART_$O_TXD;
  if $txact = 0 then
    $txsig <= not UART_$O_TXD;
end;
