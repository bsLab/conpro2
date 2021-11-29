--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D175 Genetic size: 2714497
--         Compile date: Fri Apr  1 18:08:25 CEST 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Implementation of Module <Q>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity MOD_q is
port(
  -- Connections to the outside world
  signal d2_RD: out std_logic_vector(7 downto 0);
  signal d1_RD: out std_logic_vector(7 downto 0);
  signal CLK: in std_logic;
  signal RESET: in std_logic
);
end MOD_q;
architecture main of MOD_q is
  -- Process instances
  component q_p1
  port(
    -- Connections to external objects, components and the outside world
    signal QUEUEX_q_WE: out std_logic;
    signal QUEUEX_q_WR: out std_logic_vector(7 downto 0);
    signal QUEUEX_q_GD: in std_logic;
    signal REG_d1_WR: out std_logic_vector(7 downto 0);
    signal REG_d1_WE: out std_logic;
    signal PRO_p1_ENABLE: in std_logic;
    signal PRO_p1_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component q_p2
  port(
    -- Connections to external objects, components and the outside world
    signal QUEUEX_q_RD: in std_logic_vector(7 downto 0);
    signal QUEUEX_q_RE: out std_logic;
    signal QUEUEX_q_GD: in std_logic;
    signal REG_d2_WR: out std_logic_vector(7 downto 0);
    signal REG_d2_WE: out std_logic;
    signal PRO_p2_ENABLE: in std_logic;
    signal PRO_p2_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component q_main
  port(
    -- Connections to external objects, components and the outside world
    signal QUEUEX_q_INIT: out std_logic;
    signal QUEUEX_q_GD: in std_logic;
    signal PRO_p1_START: out std_logic;
    signal PRO_p1_GD: in std_logic;
    signal PRO_p2_START: out std_logic;
    signal PRO_p2_GD: in std_logic;
    signal PRO_main_ENABLE: in std_logic;
    signal PRO_main_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  -- Local and temporary data objects
  type QUEUEX_q_QRAM_TYPE is array (0 to 7) of std_logic_vector(7 downto 0);
  signal QUEUEX_q_main_INIT: std_logic;
  signal QUEUEX_q_p2_RD: std_logic_vector(7 downto 0);
  signal QUEUEX_q_p2_RE: std_logic;
  signal QUEUEX_q_p1_WE: std_logic;
  signal QUEUEX_q_p1_WR: std_logic_vector(7 downto 0);
  signal QUEUEX_q_main_ID: std_logic_vector(7 downto 0);
  signal QUEUEX_q_main_GD: std_logic;
  signal QUEUEX_q_main_LOCKed: std_logic;
  signal QUEUEX_q_p2_ID: std_logic_vector(7 downto 0);
  signal QUEUEX_q_p2_GD: std_logic;
  signal QUEUEX_q_p2_LOCKed: std_logic;
  signal QUEUEX_q_p1_ID: std_logic_vector(7 downto 0);
  signal QUEUEX_q_p1_GD: std_logic;
  signal QUEUEX_q_p1_LOCKed: std_logic;
  signal QUEUEX_q_QRAM: QUEUEX_q_QRAM_type;
  signal QUEUEX_q_EMPTY: std_logic;
  signal QUEUEX_q_FULL: std_logic;
  signal QUEUEX_q_LOCKED: std_logic;
  signal QUEUEX_q_ADDR_RD: std_logic_vector(2 downto 0);
  signal QUEUEX_q_ADDR_WR: std_logic_vector(2 downto 0);
  signal QUEUEX_q_QRAM_ADDR_AUX: std_logic_vector(2 downto 0);
  signal QUEUEX_q_QRAM_RD_ADDR: std_logic_vector(2 downto 0);
  signal QUEUEX_q_QRAM_DIN_AUX: std_logic_vector(7 downto 0);
  signal QUEUEX_q_QRAM_DOUT_AUX: std_logic_vector(7 downto 0);
  signal QUEUEX_q_QRAM_WE_AUX: std_logic;
  signal REG_d1: std_logic_vector(7 downto 0);
  signal REG_d1_p1_WR: std_logic_vector(7 downto 0);
  signal REG_d1_p1_WE: std_logic;
  signal REG_d2: std_logic_vector(7 downto 0);
  signal REG_d2_p2_WR: std_logic_vector(7 downto 0);
  signal REG_d2_p2_WE: std_logic;
  signal PRO_main_ENABLE: std_logic;
  signal PRO_main_END: std_logic;
  signal PRO_p1_ENABLE: std_logic;
  signal PRO_p1_END: std_logic;
  signal PRO_p1_main_START: std_logic;
  signal PRO_p1_main_GD: std_logic;
  signal PRO_p2_ENABLE: std_logic;
  signal PRO_p2_END: std_logic;
  signal PRO_p2_main_START: std_logic;
  signal PRO_p2_main_GD: std_logic;
  -- State Processing
  -- Aux. signals
  signal conpro_system_clk: std_logic;
  signal conpro_system_reset: std_logic;
begin
  -- Module implementation
  --
  --  ConPro V2.1.D175 EMI Queuex.queuex Vdeleop.1-1
  --
  
  --
  -- EMI <Object Queuex.queuex.q> Process
  --
  QUEUEX_q_IMPL_QRAM: process(conpro_system_clk,
    QUEUEX_q_QRAM_WE_AUX,
    QUEUEX_q_QRAM_DIN_AUX,
    QUEUEX_q_QRAM_ADDR_AUX)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (QUEUEX_q_QRAM_WE_AUX) = ('1') then
        QUEUEX_q_QRAM((L_to_N(QUEUEX_q_QRAM_ADDR_AUX))) <= QUEUEX_q_QRAM_DIN_AUX;
      end if;
      QUEUEX_q_QRAM_RD_ADDR <= QUEUEX_q_QRAM_ADDR_AUX;
    end if;
  end process QUEUEX_q_IMPL_QRAM;
  --
  -- EMI <Object Queuex.queuex.q> Process
  --
  IMPL_QUEUE_q_SCHED: process(conpro_system_clk,
    conpro_system_reset,
    QUEUEX_q_LOCKED,
    QUEUEX_q_ADDR_RD,
    QUEUEX_q_ADDR_WR,
    QUEUEX_q_main_INIT,
    QUEUEX_q_p1_WE,
    QUEUEX_q_FULL,
    QUEUEX_q_p1_WR,
    QUEUEX_q_p2_RE,
    QUEUEX_q_EMPTY)
    variable wr_next: std_logic_vector(2 downto 0);
    variable rd_next: std_logic_vector(2 downto 0);
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        QUEUEX_q_FULL <= '0';
        QUEUEX_q_EMPTY <= '1';
        QUEUEX_q_ADDR_RD <= "000";
        QUEUEX_q_ADDR_WR <= "000";
        QUEUEX_q_LOCKED <= '0';
        QUEUEX_q_QRAM_WE_AUX <= '0';
        QUEUEX_q_QRAM_ADDR_AUX <= "000";
        QUEUEX_q_QRAM_DIN_AUX <= "00000000";
        QUEUEX_q_main_ID <= "00000000";
        QUEUEX_q_main_GD <= '1';
        QUEUEX_q_p2_ID <= "00000010";
        QUEUEX_q_p2_GD <= '1';
        QUEUEX_q_p1_ID <= "00000001";
        QUEUEX_q_p1_GD <= '1';
      elsif (QUEUEX_q_LOCKED) = ('0') then
        QUEUEX_q_QRAM_ADDR_AUX <= QUEUEX_q_ADDR_RD;
        wr_next := (QUEUEX_q_ADDR_WR) + (1);
        rd_next := (QUEUEX_q_ADDR_RD) + (1);
        if (QUEUEX_q_main_INIT) = ('1') then
          QUEUEX_q_main_GD <= '0';
        elsif ((QUEUEX_q_p1_WE) = ('1')) and ((QUEUEX_q_FULL) = ('0')) then
          if (wr_next) = (QUEUEX_q_ADDR_RD) then
            QUEUEX_q_FULL <= '1';
          end if;
          QUEUEX_q_QRAM_DIN_AUX <= QUEUEX_q_p1_WR;
          QUEUEX_q_QRAM_ADDR_AUX <= QUEUEX_q_ADDR_WR;
          QUEUEX_q_QRAM_WE_AUX <= '1';
          QUEUEX_q_ADDR_WR <= wr_next;
          QUEUEX_q_LOCKED <= '1';
          QUEUEX_q_p1_GD <= '0';
          QUEUEX_q_EMPTY <= '0';
        elsif ((QUEUEX_q_p2_RE) = ('1')) and ((QUEUEX_q_EMPTY) = ('0')) then
          if (rd_next) = (QUEUEX_q_ADDR_WR) then
            QUEUEX_q_EMPTY <= '1';
          end if;
          QUEUEX_q_ADDR_RD <= rd_next;
          QUEUEX_q_LOCKED <= '1';
          QUEUEX_q_p2_GD <= '0';
          QUEUEX_q_FULL <= '0';
        end if;
      else
        QUEUEX_q_LOCKED <= '0';
        QUEUEX_q_QRAM_WE_AUX <= '0';
        QUEUEX_q_main_GD <= '1';
        QUEUEX_q_p2_GD <= '1';
        QUEUEX_q_p1_GD <= '1';
      end if;
    end if;
  end process IMPL_QUEUE_q_SCHED;
  --
  -- EMI <Object Queuex.queuex.q>
  --
  QUEUEX_q_QRAM_DOUT_AUX <= QUEUEX_q_QRAM((L_to_N(QUEUEX_q_QRAM_RD_ADDR)));
  --
  -- EMI <Object Queuex.queuex.q>
  --
  QUEUEX_q_p2_RD <= QUEUEX_q_QRAM_DOUT_AUX;
  --
  -- End of <Object Queuex.queuex.q>
  --
  
  -- Register
  IMPL_REG_d1: process(
          REG_d1_p1_WR,
          REG_d1_p1_WE,
          REG_d1,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_d1 <= "00000000";
     elsif REG_d1_p1_WE='1' then
      REG_d1 <= REG_d1_p1_WR;
     end if;
    end if;
  end process IMPL_REG_d1;
  
  -- Register
  IMPL_REG_d2: process(
          REG_d2_p2_WR,
          REG_d2_p2_WE,
          REG_d2,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_d2 <= "00000000";
     elsif REG_d2_p2_WE='1' then
      REG_d2 <= REG_d2_p2_WR;
     end if;
    end if;
  end process IMPL_REG_d2;
  
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
  
  -- Process control
  PRO_CONTROL_p1: process(
          PRO_p1_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_p1_ENABLE <= '0';
        PRO_p1_main_GD <= '1';
      elsif PRO_p1_main_START = '1' then
        PRO_p1_ENABLE <= '1';
        PRO_p1_main_GD <= '0';
      else
        PRO_p1_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_p1;
  
  -- Process control
  PRO_CONTROL_p2: process(
          PRO_p2_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_p2_ENABLE <= '0';
        PRO_p2_main_GD <= '1';
      elsif PRO_p2_main_START = '1' then
        PRO_p2_ENABLE <= '1';
        PRO_p2_main_GD <= '0';
      else
        PRO_p2_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_p2;
  
  
  -- Process instantiations
  PRO_MAP_p1: q_p1 port map(
    QUEUEX_q_WE => QUEUEX_q_p1_WE,
    QUEUEX_q_WR => QUEUEX_q_p1_WR,
    QUEUEX_q_GD => QUEUEX_q_p1_GD,
    REG_d1_WR => REG_d1_p1_WR,
    REG_d1_WE => REG_d1_p1_WE,
    PRO_p1_ENABLE => PRO_p1_ENABLE,
    PRO_p1_END => PRO_p1_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_p2: q_p2 port map(
    QUEUEX_q_RD => QUEUEX_q_p2_RD,
    QUEUEX_q_RE => QUEUEX_q_p2_RE,
    QUEUEX_q_GD => QUEUEX_q_p2_GD,
    REG_d2_WR => REG_d2_p2_WR,
    REG_d2_WE => REG_d2_p2_WE,
    PRO_p2_ENABLE => PRO_p2_ENABLE,
    PRO_p2_END => PRO_p2_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_main: q_main port map(
    QUEUEX_q_INIT => QUEUEX_q_main_INIT,
    QUEUEX_q_GD => QUEUEX_q_main_GD,
    PRO_p1_START => PRO_p1_main_START,
    PRO_p1_GD => PRO_p1_main_GD,
    PRO_p2_START => PRO_p2_main_START,
    PRO_p2_GD => PRO_p2_main_GD,
    PRO_main_ENABLE => PRO_main_ENABLE,
    PRO_main_END => PRO_main_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  
  -- Toplevel assignments
  -- Monitors
  d2_RD <= REG_d2;
  d1_RD <= REG_d1;
  conpro_system_clk <= CLK;
  conpro_system_reset <= RESET;
end main;
