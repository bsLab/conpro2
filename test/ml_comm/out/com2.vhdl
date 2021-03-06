--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D115 Genetic size: 2534927
--         Compile date: Fri Oct 23 13:13:50 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Implementation of Module <Com2>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity MOD_com2 is
port(
  -- Connections to the outside world
  signal DEV_data_in: in std_logic;
  signal DEV_data_out: out std_logic;
  signal DEV_data: out std_logic_vector(7 downto 0);
  signal DEV_data_en: out std_logic;
  signal CLK: in std_logic;
  signal RESET: in std_logic
);
end MOD_com2;
architecture main of MOD_com2 is
  -- Process instances
  component com2_send
  port(
    -- Connections to external objects, components and the outside world
    signal REG_PRO_send_EXCEPTION_RD: in signed(7 downto 0);
    signal REG_PRO_send_EXCEPTION_WR: out signed(7 downto 0);
    signal REG_PRO_send_EXCEPTION_WE: out std_logic;
    signal RND_rnd_RE: out std_logic;
    signal RND_rnd_RD: in std_logic_vector(7 downto 0);
    signal RND_rnd_GD: in std_logic;
    signal UART_ln_STOP: out std_logic;
    signal UART_ln_WE: out std_logic;
    signal UART_ln_WR: out std_logic_vector(7 downto 0);
    signal UART_ln_WR_ERR: in std_logic;
    signal UART_ln_GD: in std_logic;
    signal PRO_send_ENABLE: in std_logic;
    signal PRO_send_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component com2_recv
  port(
    -- Connections to external objects, components and the outside world
    signal REG_PRO_recv_EXCEPTION_RD: in signed(7 downto 0);
    signal REG_PRO_recv_EXCEPTION_WR: out signed(7 downto 0);
    signal REG_PRO_recv_EXCEPTION_WE: out std_logic;
    signal DEV_data_WR: out std_logic_vector(7 downto 0);
    signal UART_ln_STOP: out std_logic;
    signal UART_ln_RE: out std_logic;
    signal UART_ln_RD: in std_logic_vector(7 downto 0);
    signal UART_ln_RD_ERR: in std_logic;
    signal UART_ln_GD: in std_logic;
    signal DEV_data_en_WR: out std_logic;
    signal PRO_recv_ENABLE: in std_logic;
    signal PRO_recv_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component com2_main
  port(
    -- Connections to external objects, components and the outside world
    signal PRO_send_START: out std_logic;
    signal PRO_send_GD: in std_logic;
    signal RND_rnd_INIT: out std_logic;
    signal RND_rnd_GD: in std_logic;
    signal UART_ln_INIT: out std_logic;
    signal UART_ln_START: out std_logic;
    signal UART_ln_BAUD_SET: out std_logic;
    signal UART_ln_BAUD: out std_logic;
    signal UART_ln_GD: in std_logic;
    signal PRO_recv_START: out std_logic;
    signal PRO_recv_GD: in std_logic;
    signal PRO_main_ENABLE: in std_logic;
    signal PRO_main_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  -- Local and temporary data objects
  signal REG_PRO_send_EXCEPTION: signed(7 downto 0);
  signal REG_PRO_send_EXCEPTION_RD: signed(7 downto 0);
  signal REG_PRO_send_EXCEPTION_send_WR: signed(7 downto 0);
  signal REG_PRO_send_EXCEPTION_send_WE: std_logic;
  signal REG_PRO_recv_EXCEPTION: signed(7 downto 0);
  signal REG_PRO_recv_EXCEPTION_RD: signed(7 downto 0);
  signal REG_PRO_recv_EXCEPTION_recv_WR: signed(7 downto 0);
  signal REG_PRO_recv_EXCEPTION_recv_WE: std_logic;
  signal PRO_send_ENABLE: std_logic;
  signal PRO_send_END: std_logic;
  signal PRO_send_main_START: std_logic;
  signal PRO_send_main_GD: std_logic;
  signal DEV_data_out_WR: std_logic;
  signal RND_rnd_send_RD: std_logic_vector(7 downto 0);
  signal RND_rnd_d_in: std_logic;
  signal RND_rnd_data_shift: std_logic_vector(7 downto 0);
  signal RND_rnd_data: std_logic_vector(7 downto 0);
  signal RND_rnd_shift: std_logic;
  signal RND_rnd_init: std_logic;
  signal RND_rnd_avail: std_logic;
  signal RND_rnd_send_RE: std_logic;
  signal RND_rnd_main_INIT: std_logic;
  signal RND_rnd_main_GD: std_logic;
  signal RND_rnd_send_GD: std_logic;
  signal RND_rnd_count: std_logic_vector(2 downto 0);
  signal PRO_main_ENABLE: std_logic;
  signal PRO_main_END: std_logic;
  signal DEV_data_WR: std_logic_vector(7 downto 0);
  signal UART_ln_main_INIT: std_logic;
  signal UART_ln_main_START: std_logic;
  signal UART_ln_recv_STOP: std_logic;
  signal UART_ln_send_STOP: std_logic;
  signal UART_ln_recv_RE: std_logic;
  signal UART_ln_recv_RD: std_logic_vector(7 downto 0);
  signal UART_ln_recv_RD_ERR: std_logic;
  signal UART_ln_send_WE: std_logic;
  signal UART_ln_send_WR: std_logic_vector(7 downto 0);
  signal UART_ln_send_WR_ERR: std_logic;
  signal UART_ln_main_BAUD_SET: std_logic;
  signal UART_ln_main_BAUD: std_logic;
  signal UART_ln_main_GD: std_logic;
  signal UART_ln_main_LOCKed: std_logic;
  signal UART_ln_recv_GD: std_logic;
  signal UART_ln_recv_LOCKed: std_logic;
  signal UART_ln_send_GD: std_logic;
  signal UART_ln_send_LOCKed: std_logic;
  signal UART_ln_RXD: std_logic;
  signal UART_ln_TXD: std_logic;
  signal UART_ln_RX_EN: std_logic;
  signal UART_ln_TX_EN: std_logic;
  signal UART_ln_TX_DATA: std_logic_vector(7 downto 0);
  signal UART_ln_RX_DATA: std_logic_vector(7 downto 0);
  signal UART_ln_BAUD_COUNT: std_logic_vector(6 downto 0);
  signal UART_ln_BAUD16: std_logic;
  signal UART_ln_RX_RE: std_logic;
  signal UART_ln_BUSY: std_logic;
  signal UART_ln_RX_SAMPLED: std_logic;
  signal UART_ln_RX_COMPL: std_logic;
  signal UART_ln_RX_ERR: std_logic;
  signal UART_ln_RX_BIT_PHASE: std_logic_vector(3 downto 0);
  signal UART_ln_RX_BIT_COUNT: std_logic_vector(4 downto 0);
  signal UART_ln_RX_SHIFT: std_logic_vector(7 downto 0);
  signal UART_ln_TX_COMPL: std_logic;
  signal UART_ln_TX_DRE: std_logic;
  signal UART_ln_TX_WE: std_logic;
  signal UART_ln_TX_ERR: std_logic;
  signal UART_ln_TX_TICK: std_logic;
  signal UART_ln_TX_BIT_COUNT: std_logic_vector(4 downto 0);
  signal UART_ln_TX_SHIFT: std_logic_vector(8 downto 0);
  signal UART_ln_send_TX_AWAIT: std_logic;
  signal DEV_data_in_RD: std_logic;
  signal PRO_recv_ENABLE: std_logic;
  signal PRO_recv_END: std_logic;
  signal PRO_recv_main_START: std_logic;
  signal PRO_recv_main_GD: std_logic;
  signal DEV_data_en_WR: std_logic;
  -- State Processing
  -- Aux. signals
  signal conpro_system_clk: std_logic;
  signal conpro_system_reset: std_logic;
begin
  -- Module implementation
  -- Register
  IMPL_REG_PRO_send_EXCEPTION: process(
          REG_PRO_send_EXCEPTION_send_WR,
          REG_PRO_send_EXCEPTION_send_WE,
          REG_PRO_send_EXCEPTION,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_PRO_send_EXCEPTION_RD <= REG_PRO_send_EXCEPTION;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_PRO_send_EXCEPTION <= to_signed(0,8);
     elsif REG_PRO_send_EXCEPTION_send_WE='1' then
      REG_PRO_send_EXCEPTION <= REG_PRO_send_EXCEPTION_send_WR;
     end if;
    end if;
  end process IMPL_REG_PRO_send_EXCEPTION;
  
  -- Register
  IMPL_REG_PRO_recv_EXCEPTION: process(
          REG_PRO_recv_EXCEPTION_recv_WR,
          REG_PRO_recv_EXCEPTION_recv_WE,
          REG_PRO_recv_EXCEPTION,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_PRO_recv_EXCEPTION_RD <= REG_PRO_recv_EXCEPTION;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_PRO_recv_EXCEPTION <= to_signed(0,8);
     elsif REG_PRO_recv_EXCEPTION_recv_WE='1' then
      REG_PRO_recv_EXCEPTION <= REG_PRO_recv_EXCEPTION_recv_WR;
     end if;
    end if;
  end process IMPL_REG_PRO_recv_EXCEPTION;
  
  -- Process control
  PRO_CONTROL_send: process(
          PRO_send_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_send_ENABLE <= '0';
        PRO_send_main_GD <= '1';
      elsif PRO_send_main_START = '1' then
        PRO_send_ENABLE <= '1';
        PRO_send_main_GD <= '0';
      else
        PRO_send_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_send;
  
  --
  --  ConPro V2.1.D115 EMI Random.random V2.07
  --
  
  --
  -- EMI <Object Random.random.rnd> Process
  --
  RANDOM_rnd_SCHED: process(conpro_system_clk,
    conpro_system_reset,
    RND_rnd_init,
    RND_rnd_main_INIT,
    RND_rnd_send_RE,
    RND_rnd_avail,
    RND_rnd_data)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        RND_rnd_shift <= '0';
        RND_rnd_init <= '0';
        RND_rnd_main_GD <= '1';
        RND_rnd_send_GD <= '1';
      else
        RND_rnd_main_GD <= '1';
        RND_rnd_send_GD <= '1';
        if (RND_rnd_init) = ('1') then
          RND_rnd_init <= '0';
        elsif (RND_rnd_main_INIT) = ('1') then
          RND_rnd_init <= '1';
          RND_rnd_main_GD <= '0';
        elsif (RND_rnd_send_RE) = ('1') then
          RND_rnd_shift <= '1';
          if (RND_rnd_avail) = ('1') then
            RND_rnd_send_RD <= RND_rnd_data;
            RND_rnd_send_GD <= '0';
            RND_rnd_shift <= '0';
          end if;
        end if;
      end if;
    end if;
  end process RANDOM_rnd_SCHED;
  --
  -- EMI <Object Random.random.rnd> Process
  --
  RANDOM_rnd: process(conpro_system_clk,
    conpro_system_reset,
    RND_rnd_init,
    RND_rnd_shift,
    RND_rnd_d_in,
    RND_rnd_data_shift,
    RND_rnd_count)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if ((conpro_system_reset) = ('1')) or ((RND_rnd_init) = ('1')) then
        RND_rnd_data_shift <= "11111111";
        RND_rnd_data <= "00000000";
        RND_rnd_count <= "000";
        RND_rnd_avail <= '0';
      elsif (RND_rnd_shift) = ('1') then
        RND_rnd_data_shift <= (RND_rnd_d_in) & (RND_rnd_data_shift(7 downto 1));
        RND_rnd_count <= (RND_rnd_count) + (1);
        if (RND_rnd_count) = ("111") then
          RND_rnd_avail <= '1';
          RND_rnd_data <= RND_rnd_data_shift;
          RND_rnd_count <= "000";
        else
          RND_rnd_avail <= '0';
        end if;
      end if;
    end if;
  end process RANDOM_rnd;
  --
  -- EMI <Object Random.random.rnd>
  --
  RND_rnd_d_in <= (RND_rnd_data_shift(0)) xor ((RND_rnd_data_shift(4)) xor ((RND_rnd_data_shift(5)) xor (RND_rnd_data_shift(7))));
  --
  -- End of <Object Random.random.rnd>
  --
  
  -- Process control
  PRO_CONTROL_main: process(
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_main_ENABLE <= '1'; -- main process activated on reset
      end if;
    end if;
  end process PRO_CONTROL_main;
  
  --
  --  ConPro V2.1.D115 EMI Uart.uart V2.14
  --
  
  --
  -- EMI <Object Uart.uart.ln> Process
  --
  UART_ln_BAUD_GEN: process(conpro_system_clk,
    conpro_system_reset,
    UART_ln_BAUD_COUNT)
    variable baud_cnt: std_logic_vector(6 downto 0);
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        baud_cnt := "0000000";
        UART_ln_BAUD16 <= '0';
      elsif (baud_cnt) = ("0000000") then
        UART_ln_BAUD16 <= '1';
        baud_cnt := UART_ln_BAUD_COUNT;
      else
        baud_cnt := (baud_cnt) - (1);
        UART_ln_BAUD16 <= '0';
      end if;
    end if;
  end process UART_ln_BAUD_GEN;
  --
  -- EMI <Object Uart.uart.ln> Process
  --
  UART_ln_RX_SAMPLER: process(conpro_system_clk,
    conpro_system_reset,
    UART_ln_BAUD16,
    UART_ln_RXD)
    variable samples: std_logic_vector(1 downto 0);
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        samples := "11";
        UART_ln_RX_SAMPLED <= '1';
      elsif (UART_ln_BAUD16) = ('1') then
        samples(1) := samples(0);
        samples(0) := UART_ln_RXD;
      end if;
      if (samples) = ("00") then
        UART_ln_RX_SAMPLED <= '0';
      end if;
      if (samples) = ("11") then
        UART_ln_RX_SAMPLED <= '1';
      end if;
    end if;
  end process UART_ln_RX_SAMPLER;
  --
  -- EMI <Object Uart.uart.ln> Process
  --
  UART_ln_RX_CONTROLLER: process(conpro_system_clk,
    conpro_system_reset,
    UART_ln_RX_RE,
    UART_ln_BAUD16,
    UART_ln_RX_BIT_COUNT,
    UART_ln_RX_SAMPLED,
    UART_ln_RX_BIT_PHASE,
    UART_ln_RX_SHIFT,
    UART_ln_RX_EN,
    UART_ln_RX_COMPL)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        UART_ln_RX_COMPL <= '0';
        UART_ln_RX_ERR <= '0';
        UART_ln_RX_BIT_PHASE <= "0000";
        UART_ln_RX_SHIFT <= "00000000";
        UART_ln_RX_BIT_COUNT <= "00000";
      elsif (UART_ln_RX_RE) = ('1') then
        UART_ln_RX_COMPL <= '0';
        UART_ln_RX_ERR <= '0';
      end if;
      if (UART_ln_BAUD16) = ('1') then
        if ((UART_ln_RX_BIT_COUNT) = ("00000")) and (((UART_ln_RX_SAMPLED) = ('1')) or ((UART_ln_RX_BIT_PHASE) = ("0111"))) then
          UART_ln_RX_BIT_PHASE <= "0000";
        else
          UART_ln_RX_BIT_PHASE <= (UART_ln_RX_BIT_PHASE) + (1);
        end if;
        if (UART_ln_RX_BIT_COUNT) = ("00000") then
          if (UART_ln_RX_BIT_PHASE) = ("0111") then
            UART_ln_RX_BIT_COUNT <= (UART_ln_RX_BIT_COUNT) + (1);
          end if;
        elsif (UART_ln_RX_BIT_PHASE) = ("1111") then
          UART_ln_RX_BIT_COUNT <= (UART_ln_RX_BIT_COUNT) + (1);
          if (UART_ln_RX_BIT_COUNT) = ("01001") then
            UART_ln_RX_BIT_COUNT <= "00000";
            if (UART_ln_RX_EN) = ('1') then
              UART_ln_RX_COMPL <= '1';
              UART_ln_RX_ERR <= (not (UART_ln_RX_SAMPLED)) or (UART_ln_RX_COMPL);
              if ((UART_ln_RX_COMPL) = ('0')) or ((UART_ln_RX_RE) = ('1')) then
                UART_ln_RX_DATA <= UART_ln_RX_SHIFT(7 downto 0);
              end if;
            end if;
          else
            UART_ln_RX_SHIFT(6 downto 0) <= UART_ln_RX_SHIFT(7 downto 1);
            UART_ln_RX_SHIFT(7) <= UART_ln_RX_SAMPLED;
          end if;
        end if;
      end if;
    end if;
  end process UART_ln_RX_CONTROLLER;
  --
  -- EMI <Object Uart.uart.ln> Process
  --
  UART_ln_TX_SAMPLER: process(conpro_system_clk,
    conpro_system_reset,
    UART_ln_BAUD16)
    variable tx_cnt: std_logic_vector(3 downto 0);
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        tx_cnt := "0000";
        UART_ln_TX_TICK <= '0';
      elsif (UART_ln_BAUD16) = ('1') then
        if (tx_cnt) = ("1111") then
          UART_ln_TX_TICK <= '1';
        end if;
        tx_cnt := (tx_cnt) + (1);
      else
        UART_ln_TX_TICK <= '0';
      end if;
    end if;
  end process UART_ln_TX_SAMPLER;
  --
  -- EMI <Object Uart.uart.ln> Process
  --
  UART_ln_TX_CONTROLLER: process(conpro_system_clk,
    conpro_system_reset,
    UART_ln_TX_WE,
    UART_ln_TX_EN,
    UART_ln_TX_DATA,
    UART_ln_TX_TICK,
    UART_ln_TX_BIT_COUNT,
    UART_ln_TX_DRE,
    UART_ln_TX_SHIFT)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        UART_ln_TX_COMPL <= '0';
        UART_ln_TX_DRE <= '1';
        UART_ln_TX_BIT_COUNT <= "00000";
        UART_ln_TX_SHIFT <= "000000000";
        UART_ln_TXD <= '1';
        UART_ln_TX_ERR <= '0';
      else
        if ((UART_ln_TX_WE) = ('1')) and ((UART_ln_TX_EN) = ('1')) then
          UART_ln_TX_DRE <= '0';
          UART_ln_TX_COMPL <= '0';
          UART_ln_TX_SHIFT(7 downto 0) <= UART_ln_TX_DATA;
          UART_ln_TX_SHIFT(8) <= '0';
        elsif ((UART_ln_TX_WE) = ('1')) and ((UART_ln_TX_EN) = ('0')) then
          UART_ln_TX_ERR <= '1';
        end if;
      end if;
      if (UART_ln_TX_TICK) = ('1') then
        case UART_ln_TX_BIT_COUNT is
          when "00000" =>
            if (UART_ln_TX_DRE) = ('0') then
              UART_ln_TX_BIT_COUNT <= "00001";
            end if;
            UART_ln_TXD <= '1';
          when "00001" =>
            UART_ln_TXD <= '0';
            UART_ln_TX_BIT_COUNT <= "00010";
          when others =>
            UART_ln_TX_BIT_COUNT <= (UART_ln_TX_BIT_COUNT) + (1);
            if (UART_ln_TX_BIT_COUNT) = ("01001") then
              UART_ln_TX_DRE <= '1';
              UART_ln_TX_BIT_COUNT <= "00000";
              UART_ln_TX_COMPL <= '1';
            end if;
            UART_ln_TXD <= UART_ln_TX_SHIFT(0);
            UART_ln_TX_SHIFT(7 downto 0) <= UART_ln_TX_SHIFT(8 downto 1);
        end case;
      end if;
    end if;
  end process UART_ln_TX_CONTROLLER;
  --
  -- EMI <Object Uart.uart.ln> Process
  --
  UART_ln_SCHED: process(conpro_system_clk,
    conpro_system_reset,
    UART_ln_BUSY,
    UART_ln_main_INIT,
    UART_ln_main_LOCKed,
    UART_ln_recv_LOCKed,
    UART_ln_send_LOCKed,
    UART_ln_main_START,
    UART_ln_recv_STOP,
    UART_ln_send_STOP,
    UART_ln_main_BAUD_SET,
    UART_ln_main_BAUD,
    UART_ln_recv_RE,
    UART_ln_RX_COMPL,
    UART_ln_RX_DATA,
    UART_ln_RX_ERR,
    UART_ln_send_WE,
    UART_ln_TX_COMPL,
    UART_ln_TX_ERR,
    UART_ln_TX_DRE,
    UART_ln_send_WR)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        UART_ln_BUSY <= '0';
        UART_ln_main_GD <= '1';
        UART_ln_recv_GD <= '1';
        UART_ln_send_GD <= '1';
        UART_ln_main_LOCKed <= '0';
        UART_ln_recv_LOCKed <= '0';
        UART_ln_send_LOCKed <= '0';
        UART_ln_RX_EN <= '0';
        UART_ln_TX_EN <= '0';
        UART_ln_RX_RE <= '0';
        UART_ln_TX_WE <= '0';
        UART_ln_BAUD_COUNT <= "1000000";
      elsif (UART_ln_BUSY) = ('0') then
        if (UART_ln_main_INIT) = ('1') then
          UART_ln_main_GD <= '0';
          UART_ln_BUSY <= '1';
          if (UART_ln_main_LOCKed) = ('1') then
            UART_ln_main_LOCKed <= '0';
            UART_ln_main_GD <= '0';
          end if;
          if (UART_ln_recv_LOCKed) = ('1') then
            UART_ln_recv_LOCKed <= '0';
            UART_ln_recv_GD <= '0';
          end if;
          if (UART_ln_send_LOCKed) = ('1') then
            UART_ln_send_LOCKed <= '0';
            UART_ln_send_GD <= '0';
          end if;
        elsif (UART_ln_main_START) = ('1') then
          UART_ln_BUSY <= '1';
          UART_ln_main_GD <= '0';
          UART_ln_RX_EN <= '1';
          UART_ln_TX_EN <= '1';
        elsif (UART_ln_recv_STOP) = ('1') then
          UART_ln_BUSY <= '1';
          UART_ln_recv_GD <= '0';
          UART_ln_RX_EN <= '0';
          UART_ln_TX_EN <= '0';
        elsif (UART_ln_send_STOP) = ('1') then
          UART_ln_BUSY <= '1';
          UART_ln_send_GD <= '0';
          UART_ln_RX_EN <= '0';
          UART_ln_TX_EN <= '0';
        elsif (UART_ln_main_BAUD_SET) = ('1') then
          UART_ln_main_GD <= '0';
          UART_ln_BUSY <= '1';
          if (UART_ln_main_BAUD) = ('0') then
            UART_ln_BAUD_COUNT <= "1000000";
          end if;
        elsif (((UART_ln_recv_RE) = ('1')) and ((UART_ln_recv_LOCKed) = ('1'))) and ((UART_ln_RX_COMPL) = ('1')) then
          UART_ln_recv_RD <= UART_ln_RX_DATA;
          UART_ln_recv_RD_ERR <= UART_ln_RX_ERR;
          UART_ln_recv_GD <= '0';
          UART_ln_recv_LOCKed <= '0';
          UART_ln_RX_RE <= '1';
          UART_ln_BUSY <= '1';
        elsif (((UART_ln_send_WE) = ('1')) and ((UART_ln_send_LOCKed) = ('1'))) and ((UART_ln_TX_COMPL) = ('1')) then
          UART_ln_send_WR_ERR <= UART_ln_TX_ERR;
          UART_ln_send_GD <= '0';
          UART_ln_send_LOCKed <= '0';
          UART_ln_BUSY <= '1';
        elsif ((UART_ln_recv_RE) = ('1')) and ((UART_ln_recv_LOCKed) = ('0')) then
          if (UART_ln_RX_COMPL) = ('1') then
            UART_ln_recv_RD <= UART_ln_RX_DATA;
            UART_ln_recv_RD_ERR <= UART_ln_RX_ERR;
            UART_ln_recv_GD <= '0';
            UART_ln_RX_RE <= '1';
            UART_ln_BUSY <= '1';
          else
            UART_ln_recv_LOCKed <= '1';
          end if;
        elsif (((UART_ln_send_WE) = ('1')) and ((UART_ln_send_LOCKed) = ('0'))) and ((UART_ln_TX_DRE) = ('1')) then
          UART_ln_TX_DATA <= UART_ln_send_WR;
          UART_ln_TX_WE <= '1';
          UART_ln_BUSY <= '1';
          UART_ln_send_LOCKed <= '1';
        end if;
      else
        UART_ln_main_GD <= '1';
        UART_ln_recv_GD <= '1';
        UART_ln_send_GD <= '1';
        UART_ln_RX_RE <= '0';
        UART_ln_TX_WE <= '0';
        UART_ln_BUSY <= '0';
      end if;
    end if;
  end process UART_ln_SCHED;
  --
  -- EMI <Object Uart.uart.ln>
  --
  UART_ln_RXD <= DEV_data_in_RD;
  DEV_data_out_WR <= UART_ln_TXD;
  --
  -- End of <Object Uart.uart.ln>
  --
  
  -- Process control
  PRO_CONTROL_recv: process(
          PRO_recv_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_recv_ENABLE <= '0';
        PRO_recv_main_GD <= '1';
      elsif PRO_recv_main_START = '1' then
        PRO_recv_ENABLE <= '1';
        PRO_recv_main_GD <= '0';
      else
        PRO_recv_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_recv;
  
  
  -- Process instantiations
  PRO_MAP_send: com2_send port map(
    REG_PRO_send_EXCEPTION_RD => REG_PRO_send_EXCEPTION_RD,
    REG_PRO_send_EXCEPTION_WR => REG_PRO_send_EXCEPTION_send_WR,
    REG_PRO_send_EXCEPTION_WE => REG_PRO_send_EXCEPTION_send_WE,
    RND_rnd_RE => RND_rnd_send_RE,
    RND_rnd_RD => RND_rnd_send_RD,
    RND_rnd_GD => RND_rnd_send_GD,
    UART_ln_STOP => UART_ln_send_STOP,
    UART_ln_WE => UART_ln_send_WE,
    UART_ln_WR => UART_ln_send_WR,
    UART_ln_WR_ERR => UART_ln_send_WR_ERR,
    UART_ln_GD => UART_ln_send_GD,
    PRO_send_ENABLE => PRO_send_ENABLE,
    PRO_send_END => PRO_send_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_recv: com2_recv port map(
    REG_PRO_recv_EXCEPTION_RD => REG_PRO_recv_EXCEPTION_RD,
    REG_PRO_recv_EXCEPTION_WR => REG_PRO_recv_EXCEPTION_recv_WR,
    REG_PRO_recv_EXCEPTION_WE => REG_PRO_recv_EXCEPTION_recv_WE,
    DEV_data_WR => DEV_data_WR,
    UART_ln_STOP => UART_ln_recv_STOP,
    UART_ln_RE => UART_ln_recv_RE,
    UART_ln_RD => UART_ln_recv_RD,
    UART_ln_RD_ERR => UART_ln_recv_RD_ERR,
    UART_ln_GD => UART_ln_recv_GD,
    DEV_data_en_WR => DEV_data_en_WR,
    PRO_recv_ENABLE => PRO_recv_ENABLE,
    PRO_recv_END => PRO_recv_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_main: com2_main port map(
    PRO_send_START => PRO_send_main_START,
    PRO_send_GD => PRO_send_main_GD,
    RND_rnd_INIT => RND_rnd_main_INIT,
    RND_rnd_GD => RND_rnd_main_GD,
    UART_ln_INIT => UART_ln_main_INIT,
    UART_ln_START => UART_ln_main_START,
    UART_ln_BAUD_SET => UART_ln_main_BAUD_SET,
    UART_ln_BAUD => UART_ln_main_BAUD,
    UART_ln_GD => UART_ln_main_GD,
    PRO_recv_START => PRO_recv_main_START,
    PRO_recv_GD => PRO_recv_main_GD,
    PRO_main_ENABLE => PRO_main_ENABLE,
    PRO_main_END => PRO_main_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  
  -- Toplevel assignments
  -- Monitors
  DEV_data_in_RD <= DEV_data_in;
  DEV_data_out <= DEV_data_out_WR;
  DEV_data <= DEV_data_WR;
  DEV_data_en <= DEV_data_en_WR;
  conpro_system_clk <= CLK;
  conpro_system_reset <= RESET;
end main;
