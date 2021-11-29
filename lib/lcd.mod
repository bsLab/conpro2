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
--    $CREATED:     14.1.2009
--    $VERSION:     2.08
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements LCD Display object
--
--
--    $ENDOFINFO
--

#version "2.10";

--
-- parameters used, allowed and default values
--
#parameter
begin
  --
  -- character size
  --
  $datawidth <= 8;
  --
  -- 4 or 8 bit mode
  --
  $mode[4,8] <= 4;
  --
  -- LCD displays are slow devices, 
  -- access time in nanoseconds
  --
  $access_time <= 2000;
  
  $lines[1 to 8] <= 3;
  
  $dots["5x7","5x10"] <= "5x7";
    
  --
  -- Controller Type
  --
  $controller["ks0070","st7066","st7036"] <= "st7036"; 
  --
  -- Device signals
  --
  $lcd_db;
  $lcd_rs;
  $lcd_rw;
  $lcd_en;
  $lcd_res;
end;

--
-- Supported object methods
--
#methods
begin
  init ();
  --
  -- cmd(data)
  --
  cmd(#rhs:logic[$datawidth]);
  --
  -- write(data)
  --
  write(#rhs:logic[$datawidth]);
  --
  -- D(0)..D(N-1),RW,EN,RS,RESET
  interface(#lrhs:logic[$mode],#lhs:logic,#lhs:logic,#lhs:logic,#lhs:logic);
end;

#assert
begin
  size($P.init) >= 1;
end;

--
-- Interface for processes accessing methods of object (process port, local process context)
--
#interface
begin
  foreach $p in $P.init do
  begin
    signal LCD_$O_INIT: out std_logic;
  end;
  foreach $p in $P.write do
  begin
    signal LCD_$O_WE: out std_logic;
  end;
  foreach $p in $P.cmd do
  begin
    signal LCD_$O_CE: out std_logic;
  end;
  foreach $p in $P.write or $P.cmd do
  begin
    signal LCD_$O_WR: out std_logic_vector[$datawidth];
  end;
  foreach $p in $P do
  begin
    signal LCD_$O_GD: in std_logic;
  end;
end;

--
-- Process mapping of object signals (global module context)
--

#mapping
begin
  foreach $p in $P.init do
  begin
    LCD_$O_INIT => LCD_$O_$p_INIT;
  end;
  foreach $p in $P.write do
  begin
    LCD_$O_WE => LCD_$O_$p_WE;
  end;
  foreach $p in $P.cmd do
  begin
    LCD_$O_CE => LCD_$O_$p_CE;
  end;
  foreach $p in $P.write or $P.cmd do
  begin
    LCD_$O_WR => LCD_$O_$p_WR;
  end;
  foreach $p in $P do
  begin
    LCD_$O_GD => LCD_$O_$p_GD;
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
    LCD_$O_INIT <= LCD_$O_GD when $ACC else '0';
  end;
  #control
  begin
    wait for LCD_$O_GD = '0';
  end;
end;


write: #access
begin
  #data
  begin
    LCD_$O_WE <= LCD_$O_GD when $ACC else '0';
    LCD_$O_WR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for LCD_$O_GD = '0';
  end;
end;

cmd: #access
begin
  #data
  begin
    LCD_$O_CE <= LCD_$O_GD when $ACC else '0';
    LCD_$O_WR <= $ARG1 when $ACC else 0;
  end;
  #control
  begin
    wait for LCD_$O_GD = '0';
  end;
end;

interface: #access
begin
  #set
  begin
    $lcd_db <= $ARG1;
    $lcd_rw <= $ARG2;
    $lcd_en <= $ARG3;
    $lcd_rs <= $ARG4;
    $lcd_res <= $ARG5;
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
    signal LCD_$O_$p_INIT: std_logic;    
  end;
  foreach $p in $P.cmd do
  begin
    signal LCD_$O_$p_CE: std_logic;    
  end;
  foreach $p in $P.write do
  begin
    signal LCD_$O_$p_WE: std_logic;
  end;
  foreach $p in $P.write or $P.cmd do
  begin
    signal LCD_$O_$p_WR: std_logic_vector[$datawidth];
  end;
  foreach $p in $P do
  begin
    signal LCD_$O_$p_GD: std_logic;    
    signal LCD_$O_$p_LOCKed: std_logic;
  end;
end;

#signals
begin
  --
  -- Device interface
  --
  signal LCD_$O_RW: std_logic;
  signal LCD_$O_EN: std_logic;
  signal LCD_$O_RS: std_logic;
  signal LCD_$O_RES: std_logic;
  signal LCD_$O_DB_RD: std_logic_vector[$mode];
  signal LCD_$O_DB_WR: std_logic_vector[$mode];


  signal LCD_$O_TX_DATA: std_logic_vector[$datawidth];
  signal LCD_$O_TXD: std_logic;
  signal LCD_$O_TXC: std_logic;
  signal LCD_$O_TX_BUSY: std_logic;
  signal LCD_$O_TX_COMPL: std_logic;

  signal LCD_$O_TIMER_CNT: std_logic_vector[width(($access_time*$clock)/1000000000)];
  signal LCD_$O_TIMER_TICK: std_logic;
  signal LCD_$O_TIMER_BUSY: std_logic;
  signal LCD_$O_TIMER_START: std_logic;
    
  signal LCD_$O_INIT: std_logic;
  signal LCD_$O_BUSY: std_logic;
end;

#signals ($mode=4)
begin
  type LCD_$O_TX_STATES is {
    LCD_$O_TX_I0,
    LCD_$O_TX_I1,
    LCD_$O_TX_I2,
    LCD_$O_TX_I3,
    LCD_$O_TX_I4,
    LCD_$O_TX_B0,
    LCD_$O_TX_B1,
    LCD_$O_TX_S0,
    LCD_$O_TX_S1,
    LCD_$O_TX_S2,
    LCD_$O_TX_S3
  };
  signal LCD_$O_TX_STATE: LCD_$O_TX_STATES;
end;

#signals ($mode=8)
begin
  type LCD_$O_TX_STATES is {
    LCD_$O_TX_I0,
    LCD_$O_TX_I1,
    LCD_$O_TX_I2,
    LCD_$O_TX_I3,
    LCD_$O_TX_S0,
    LCD_$O_TX_S1,
    LCD_$O_TX_S2
  };
  signal LCD_$O_TX_STATE: LCD_$O_TX_STATES;
end;

LCD_$O_TIMER: #process
begin
  if $CLK then
  begin
    if $RES then
    begin
      LCD_$O_TIMER_TICK <= '0';
      LCD_$O_TIMER_BUSY <= '0';
      LCD_$O_TIMER_CNT <= to_logic(0,width(($access_time*$clock)/1000000000));
    end
    else if LCD_$O_TIMER_START = '1' then
    begin
      LCD_$O_TIMER_TICK <= '0';
      LCD_$O_TIMER_BUSY <= '1';
      LCD_$O_TIMER_CNT <= to_logic(
        ($access_time*$clock)/1000000000,
        width(($access_time*$clock)/1000000000)
        );
    end
    else if LCD_$O_TIMER_BUSY = '1' then
    begin
      if LCD_$O_TIMER_CNT = to_logic(0,width(($access_time*$clock)/1000000000)) then
      begin
        LCD_$O_TIMER_TICK <= '1';
        LCD_$O_TIMER_BUSY <= '0';
      end
      else
      begin
        LCD_$O_TIMER_CNT <= LCD_$O_TIMER_CNT - 1;
      end;
    end
    else
    begin
      LCD_$O_TIMER_TICK <= '0';
    end;
  end;
end;

#top
begin
  LCD_$O_DB_RD <= $lcd_db;
  $lcd_db <= LCD_$O_DB_WR when LCD_$O_RW = '0' else (others => 'Z');
end;

LCD_$O_TX_CONTROLLER: #process ($mode=4)
begin
  variable tx_cnt: std_logic;
  variable tx_cmd: std_logic;
  variable tx_busy: std_logic;
  variable tx_compl: std_logic;
  variable lcd_busy: std_logic;
  variable delay_cnt: logic[20];
  variable data_cnt: logic[4];
  
  if $CLK then
  begin
    if $RES then
    begin
      LCD_$O_TX_STATE <= LCD_$O_TX_I0;
      LCD_$O_TIMER_START <= '0';
      tx_cnt := '0';
      tx_cmd := '0';
      tx_busy := '0';
      tx_compl := '0';
      lcd_busy := '0';

      -- Initialization only
      delay_cnt := to_logic(0,20);
      data_cnt := to_logic(0,4);
    end
    else 
    begin
      case LCD_$O_TX_STATE is
      begin
        when LCD_$O_TX_I0 =>
        begin
          if LCD_$O_INIT = '1' then
          begin
            if $controller = "ks0070" then
            begin
              delay_cnt := to_logic((30000*1000)/$access_time,20);
            end;
            if $controller = "st7066" then
            begin
              delay_cnt := to_logic((40000*1000)/$access_time,20);
            end;
            if $controller = "st7036" then
            begin
              delay_cnt := to_logic((50000*1000)/$access_time,20);
            end;
            data_cnt := to_logic(0,4);
            LCD_$O_TIMER_START <= '1';
            LCD_$O_TX_STATE <= LCD_$O_TX_I1;
            tx_busy := '1';
          end;
        end;
        
        when LCD_$O_TX_I1 =>
        begin

          LCD_$O_TIMER_START <= '0';
          LCD_$O_TX_STATE <= LCD_$O_TX_I1;
          --
          -- Wait for delay_cnt us at least
          --
          if LCD_$O_TIMER_TICK = '1' then
          begin
            delay_cnt := delay_cnt - 1;
            if delay_cnt = to_logic(0,20) then
            begin
              LCD_$O_TIMER_START <= '1';
              LCD_$O_TX_STATE <= LCD_$O_TX_I2;
            end
            else
              LCD_$O_TIMER_START <= '1';         
          end;
        end;

        when LCD_$O_TX_I2 =>
        begin
          LCD_$O_TIMER_START <= '0';
          if LCD_$O_TIMER_TICK = '1' then
          begin
            if $controller = "ks0070" then
            begin
              case data_cnt is
              begin
                when to_logic(0,4) => delay_cnt := to_logic((50*1000)/$access_time,20);
                when to_logic(1,4) => delay_cnt := to_logic((50*1000)/$access_time,20);
                when to_logic(2,4) => delay_cnt := to_logic((2000*1000)/$access_time,20);
                when others => delay_cnt := to_logic(1,20);
              end;
            end;
            if $controller = "st7066" then
            begin
              case data_cnt is
              begin
                when to_logic(0,4) => delay_cnt := to_logic((50*1000)/$access_time,20);
                when to_logic(1,4) => delay_cnt := to_logic((1*1000)/$access_time,20);
                when to_logic(2,4) => delay_cnt := to_logic((2000*1000)/$access_time,20);
                when others => delay_cnt := to_logic(1,20);
              end;
            end;
            if $controller = "st7036" then
            begin
              case data_cnt is
              begin
                when to_logic(0,4) => delay_cnt := to_logic((2000*1000)/$access_time,20);
                when to_logic(1,4) => delay_cnt := to_logic((50*1000)/$access_time,20);
                when to_logic(2,4) => delay_cnt := to_logic((50*1000)/$access_time,20);
                when to_logic(3,4) => delay_cnt := to_logic((2000*1000)/$access_time,20);
                when others => delay_cnt := to_logic(1,20);
              end;
            end;
            
            LCD_$O_TX_STATE <= LCD_$O_TX_I3;
            LCD_$O_TIMER_START <= '1';
          end;
         end;

        when LCD_$O_TX_I3 =>
        begin

          LCD_$O_TIMER_START <= '0';

          --
          -- Wait for delay_cnt us at least
          --
          if LCD_$O_TIMER_TICK = '1' then
          begin
            delay_cnt := delay_cnt - 1;
            if delay_cnt = to_logic(0,20) then
            begin
              
              if $controller = "ks0070" then
              begin
                if data_cnt = to_logic(2,4) then
                  LCD_$O_TX_STATE <= LCD_$O_TX_I4
                else
                  LCD_$O_TX_STATE <= LCD_$O_TX_I2;
               end;
              if $controller = "st7066" then
              begin
                if data_cnt = to_logic(2,4) then
                  LCD_$O_TX_STATE <= LCD_$O_TX_I4
                else
                  LCD_$O_TX_STATE <= LCD_$O_TX_I2;
              end;
              if $controller = "st7036" then
              begin
                if data_cnt = to_logic(3,4) then
                  LCD_$O_TX_STATE <= LCD_$O_TX_I4
                else
                  LCD_$O_TX_STATE <= LCD_$O_TX_I2;
              end;
              
              data_cnt := data_cnt + 1;
              LCD_$O_TIMER_START <= '1';
            end
            else
              LCD_$O_TIMER_START <= '1';         
          end;
        end;

 
        when LCD_$O_TX_I4 =>
        begin
          --
          -- Wait for delay_cnt us at least
          --
          LCD_$O_TIMER_START <= '0';
          if LCD_$O_TIMER_TICK = '1' then
          begin
            tx_busy := '0';
            tx_compl := '1';
            LCD_$O_TX_STATE <= LCD_$O_TX_S0;
            LCD_$O_TIMER_START <= '0';
          end;
        end;

        --
        -- Busy Flag Check
        --
        when LCD_$O_TX_B0 =>
        begin
          LCD_$O_TIMER_START <= '0';
          if LCD_$O_TIMER_TICK = '1' then
          begin
            if tx_cnt = '0' then
              lcd_busy := LCD_$O_DB_RD[$mode-1];
            LCD_$O_TX_STATE <= LCD_$O_TX_B1;
            LCD_$O_TIMER_START <= '1';
          end;            
        end;
        
        when LCD_$O_TX_B1 =>
        begin
          LCD_$O_TIMER_START <= '0';
          if LCD_$O_TIMER_TICK = '1' then
          begin
            if tx_cnt = '0' then
            begin
              tx_cnt := '1';
              LCD_$O_TX_STATE <= LCD_$O_TX_B0;
              LCD_$O_TIMER_START <= '1';
            end
            else
            begin
              tx_cnt := '0';
              if lcd_busy = '0' then 
              begin
                LCD_$O_TIMER_START <= '1';
                LCD_$O_TX_STATE <= LCD_$O_TX_S1;
              end
              else
              begin
                LCD_$O_TX_STATE <= LCD_$O_TX_B0;
                LCD_$O_TIMER_START <= '1';                  
              end; 
            end;

          end;            
        end;

        when LCD_$O_TX_S0 =>
        begin
          LCD_$O_TIMER_START <= '0';
          tx_compl := '0';

          if LCD_$O_TXD = '1' then
          begin
            LCD_$O_TX_STATE <= LCD_$O_TX_B0;
            LCD_$O_TIMER_START <= '1';
            tx_busy := '1';
            tx_cnt := '0';
            tx_cmd := '0';
          end
          else if LCD_$O_TXC = '1' then
          begin
            LCD_$O_TX_STATE <= LCD_$O_TX_B0;
            LCD_$O_TIMER_START <= '1';
            tx_busy := '1';
            tx_cnt := '0';
            tx_cmd := '1';
          end;            
        end;
        
        when LCD_$O_TX_S1 =>
        begin
          LCD_$O_TIMER_START <= '0';

          if LCD_$O_TIMER_TICK = '1' then
          begin
            LCD_$O_TIMER_START <= '1';
            LCD_$O_TX_STATE <= LCD_$O_TX_S2;
          end;
        end;
        
        when LCD_$O_TX_S2 =>
        begin
          LCD_$O_TIMER_START <= '0';
          --
          -- Data write in progess or end
          --

          if LCD_$O_TIMER_TICK = '1' then
          begin
            if tx_cnt = '1' then
            begin
              if tx_cmd = '0' then
              begin
                tx_busy := '0';
                tx_compl := '1';
                LCD_$O_TX_STATE <= LCD_$O_TX_S0; 
              end
              else 
              begin
                delay_cnt := to_logic((2000*1000)/$access_time,20);
                LCD_$O_TIMER_START <= '1';
                LCD_$O_TX_STATE <= LCD_$O_TX_S3; 
              end;
            end            
            else
            begin
              tx_cnt := '1';
              LCD_$O_TIMER_START <= '1';
              LCD_$O_TX_STATE <= LCD_$O_TX_S1;
            end;
          end;
        end;
        
        when LCD_$O_TX_S3 =>
        begin
          -- Wait for delay_cnt us at least
          --
          LCD_$O_TIMER_START <= '0';
          if LCD_$O_TIMER_TICK = '1' then
          begin
            delay_cnt := delay_cnt - 1;
            if delay_cnt = to_logic(0,20) then
            begin
              tx_busy := '0';
              tx_compl := '1';
              LCD_$O_TX_STATE <= LCD_$O_TX_S0;
              LCD_$O_TIMER_START <= '0';
            end
            else
              LCD_$O_TIMER_START <= '1';         
          end;
        end;
        when others => LCD_$O_TX_STATE <= LCD_$O_TX_S0;
      end;
    end;
  end;     
 
  --
  -- BUSY must be immediately active after TXC/TXD request signal activation!
  --
  LCD_$O_TX_BUSY <=  tx_busy;
  LCD_$O_TX_COMPL <= tx_compl;

  LCD_$O_EN <= '0';
  LCD_$O_RW <= '0';
  LCD_$O_RS <= '0';
  LCD_$O_DB_WR <= 0b0000;
  LCD_$O_RES <= '1';
  
  case LCD_$O_TX_STATE is
  begin
    when LCD_$O_TX_I0 =>
    begin
      LCD_$O_RES <= '0';
    end;    
    when LCD_$O_TX_I1 =>
    begin
      null;
    end;    
    when LCD_$O_TX_I2 | LCD_$O_TX_I3 =>
    begin
      LCD_$O_RW <= '0';
      LCD_$O_RS <= '0';
      if LCD_$O_TX_STATE = LCD_$O_TX_I2 then
        LCD_$O_EN <= '1'
      else
        LCD_$O_EN <= '0';
      
      if $controller = "ks0070" then
      begin
        --
        --  delay=30ms
        --
        -- dc RS RW D7 D6 D5 D4
        -- 0  0  0  0  0  1  0
        -- 1  0  0  0  0  1  0
        -- 2  0  0  N  F  0  0
        -- N=0:1 line, N=1:2line
        -- F=0:5x7 dots, F=1:5x10 dots
        --
        case data_cnt is
        begin
          when to_logic(0,4) => LCD_$O_DB_WR <= 0b0010;
          when to_logic(1,4) => LCD_$O_DB_WR  <= 0b0010;
          when to_logic(2,4) => 
          begin
            if $dots = "5x10" then
            begin
              if $lines = 1 then
                LCD_$O_DB_WR  <= 0b0100
              else
                LCD_$O_DB_WR  <= 0b1100;
            end;
            if $dots = "5x7" then
            begin
              if $lines = 1 then
                LCD_$O_DB_WR  <= 0b0000
              else
                LCD_$O_DB_WR <= 0b1000;
            end;
          end;
          when others => LCD_$O_DB_WR <= 0b0000;
        end;
      end;


      if $controller = "st7066" then
      begin
        --
        --  delay=40ms
        --
        -- dc RS RW D7 D6 D5 D4
        -- 0  0  0  0  0  1  1
        --  delay 50 us
        --
        -- 1  0  0  0  0  1  0
        -- 2  0  0  N  F  0  0
        -- N=0:1 line, N=1:2line
        -- F=0:5x7 dots, F=1:5x10 dots
        --

        case data_cnt is
        begin
          when to_logic(0,4) => LCD_$O_DB_WR <= 0b0011;
          when to_logic(1,4) => LCD_$O_DB_WR <= 0b0010;
          when to_logic(2,4) => 
          begin
            if $dots = "5x10" then
            begin
              if $lines = 1 then
                LCD_$O_DB_WR <= 0b0100
              else
                LCD_$O_DB_WR <= 0b1100;
            end;
            if $dots = "5x7" then
            begin
              if $lines = 1 then
                LCD_$O_DB_WR <= 0b0000
              else
                LCD_$O_DB_WR <= 0b1000;
            end;
          end;
          when others => LCD_$O_DB_WR <= 0b0000;
        end;
      end;
      if $controller = "st7036" then
      begin
        --
        --  delay=40ms
        --
        -- dc D7 D6 D5 D4
        -- 0  0  0  1  1
        --  delay 2 ms
        -- 1  0  0  1  1
        --  delay 50 us
        -- 2  0  0  1  1
        --  delay 50 us
        -- 3  0  0  1  0
        --  delay 50 us
        --
 
        case data_cnt is
        begin
          when to_logic(0,4) => LCD_$O_DB_WR <= 0b0011;
          when to_logic(1,4) => LCD_$O_DB_WR <= 0b0011;
          when to_logic(2,4) => LCD_$O_DB_WR <= 0b0011;
          when to_logic(3,4) => LCD_$O_DB_WR <= 0b0010;
          when others => LCD_$O_DB_WR <= 0b0000;
        end;
      end;
    
    end;    
    
    
    when LCD_$O_TX_I4 =>
    begin
      null;
    end;
    
    when LCD_$O_TX_B0 =>
    begin
      LCD_$O_RW <= '1';
      LCD_$O_RS <= '0';
      LCD_$O_EN <= '1';    
    end;    
    when LCD_$O_TX_B1 =>
    begin
      LCD_$O_RW <= '1';
      LCD_$O_RS <= '0';
      LCD_$O_EN <= '0';    
    end;  
      
    when LCD_$O_TX_S0 =>
    begin
      null;
    end;   
     
    when LCD_$O_TX_S1 =>
    begin
     LCD_$O_RW <= '0';
     if tx_cmd = '0' then
       LCD_$O_RS <= '1'
     else
       LCD_$O_RS <= '0';
     LCD_$O_EN <= '1';
     
     if tx_cnt = '0' then
       LCD_$O_DB_WR <= LCD_$O_TX_DATA[7 downto 4]
     else
       LCD_$O_DB_WR <= LCD_$O_TX_DATA[3 downto 0];
    
    end;   
     
    when LCD_$O_TX_S2 =>
    begin
     LCD_$O_RW <= '0';
     if tx_cmd = '0' then
       LCD_$O_RS <= '1'
     else
       LCD_$O_RS <= '0';
     LCD_$O_EN <= '0';
     
     if tx_cnt = '0' then
       LCD_$O_DB_WR <= LCD_$O_TX_DATA[7 downto 4]
     else
       LCD_$O_DB_WR <= LCD_$O_TX_DATA[3 downto 0];
    
    end;  
      
    when LCD_$O_TX_S3 =>
    begin
      null;
    end;    
  end;
 end;



--
-- Implementation (global module context)
-- Scheduler process: access serialization
--
LCD_$O_SCHED: #process 
begin
  if $CLK then
  begin
    if $RES then
    begin
      foreach $p in $P do
      begin
        LCD_$O_$p_GD <= '1';
      end;
      foreach $p in $P do
      begin
        LCD_$O_$p_LOCKed <= '0';
      end;
      LCD_$O_TXD <= '0';
      LCD_$O_TXC <= '0';
      LCD_$O_TX_DATA <= to_logic(0,$datawidth);
      LCD_$O_BUSY <= '0';
    end
    else
    begin
      foreach $p in $P do
      begin
        LCD_$O_$p_GD <= '1';
      end;
            
      if LCD_$O_BUSY = '0' then
      begin
        sequence
        begin
          foreach $p in $P.init do
          begin
            if LCD_$O_$p_INIT = '1' then
            begin
             if LCD_$O_$p_LOCKed = '0' then
             begin
               LCD_$O_INIT <= '1';
               LCD_$O_$p_LOCKed <= '1';
               LCD_$O_BUSY <= '1';
             end
             else
             begin
               if LCD_$O_TX_BUSY = '0' then
               begin
                 LCD_$O_$p_LOCKed <= '0';
                 LCD_$O_$p_GD <= '0';
               end;
             end;
            end;
          end;
          foreach $p in $P.cmd do
          begin
            if LCD_$O_$p_CE = '1' and LCD_$O_$p_LOCKed  = '1' and LCD_$O_TX_COMPL = '1' then
            begin
              LCD_$O_$p_LOCKed <= '0';
              LCD_$O_$p_GD <= '0';
            end;
          end;
          foreach $p in $P.write do
          begin
            if LCD_$O_$p_WE = '1' and LCD_$O_$p_LOCKed  = '1' and LCD_$O_TX_COMPL = '1' then
            begin
              LCD_$O_$p_LOCKed <= '0';
              LCD_$O_$p_GD <= '0';
            end;
          end;
          foreach $p in $P.cmd do
          begin
            if LCD_$O_$p_CE = '1' and LCD_$O_$p_LOCKed  = '0' and LCD_$O_TX_BUSY = '0' then
            begin
              LCD_$O_TX_DATA <= LCD_$O_$p_WR;
              LCD_$O_TXC <= '1';
              LCD_$O_$p_LOCKed <= '1';
              LCD_$O_BUSY <= '1';
            end;
          end;
          foreach $p in $P.write do
          begin
            if LCD_$O_$p_WE = '1' and LCD_$O_$p_LOCKed  = '0' and LCD_$O_TX_BUSY = '0' then
            begin
              LCD_$O_TX_DATA <= LCD_$O_$p_WR;
              LCD_$O_TXD <= '1';
              LCD_$O_$p_LOCKed <= '1';
              LCD_$O_BUSY <= '1';
            end;
          end;
        end;
      end
      else
      begin
        LCD_$O_TXD <= '0';
        LCD_$O_TXC <= '0';
        LCD_$O_INIT <= '0';
        LCD_$O_BUSY <= '0';
      end;
    end;
  end;
end;

#top
begin
  $lcd_rw <= LCD_$O_RW;
  $lcd_en <= LCD_$O_EN;
  $lcd_rs <= LCD_$O_RS;
  $lcd_res <= LCD_$O_RES;
end;
