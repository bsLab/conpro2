--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D138 Genetic size: 2566603
--         Compile date: Thu Feb 25 11:39:56 CET 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Implementation of Module <Mu>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity MOD_mu is
port(
  -- Connections to the outside world
  signal d_0_RD: out std_logic;
  signal d_1_RD: out std_logic;
  signal d_2_RD: out std_logic;
  signal d_3_RD: out std_logic;
  signal CLK: in std_logic;
  signal RESET: in std_logic
);
end MOD_mu;
architecture main of MOD_mu is
  -- Process instances
  component mu_p_0
  port(
    -- Connections to external objects, components and the outside world
    signal REG_d_0_WR: out std_logic;
    signal REG_d_0_WE: out std_logic;
    signal MUTEX_mu_LOCK: out std_logic;
    signal MUTEX_mu_UNLOCK: out std_logic;
    signal MUTEX_mu_GD: in std_logic;
    signal PRO_p_0_ENABLE: in std_logic;
    signal PRO_p_0_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component mu_p_1
  port(
    -- Connections to external objects, components and the outside world
    signal REG_d_1_WR: out std_logic;
    signal REG_d_1_WE: out std_logic;
    signal MUTEX_mu_LOCK: out std_logic;
    signal MUTEX_mu_UNLOCK: out std_logic;
    signal MUTEX_mu_GD: in std_logic;
    signal PRO_p_1_ENABLE: in std_logic;
    signal PRO_p_1_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component mu_p_2
  port(
    -- Connections to external objects, components and the outside world
    signal REG_d_2_WR: out std_logic;
    signal REG_d_2_WE: out std_logic;
    signal MUTEX_mu_LOCK: out std_logic;
    signal MUTEX_mu_UNLOCK: out std_logic;
    signal MUTEX_mu_GD: in std_logic;
    signal PRO_p_2_ENABLE: in std_logic;
    signal PRO_p_2_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component mu_p_3
  port(
    -- Connections to external objects, components and the outside world
    signal REG_d_3_WR: out std_logic;
    signal REG_d_3_WE: out std_logic;
    signal MUTEX_mu_LOCK: out std_logic;
    signal MUTEX_mu_UNLOCK: out std_logic;
    signal MUTEX_mu_GD: in std_logic;
    signal PRO_p_3_ENABLE: in std_logic;
    signal PRO_p_3_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component mu_main
  port(
    -- Connections to external objects, components and the outside world
    signal PRO_p_0_START: out std_logic;
    signal PRO_p_0_GD: in std_logic;
    signal PRO_p_1_START: out std_logic;
    signal PRO_p_1_GD: in std_logic;
    signal PRO_p_2_START: out std_logic;
    signal PRO_p_2_GD: in std_logic;
    signal PRO_p_3_START: out std_logic;
    signal PRO_p_3_GD: in std_logic;
    signal MUTEX_mu_INIT: out std_logic;
    signal MUTEX_mu_GD: in std_logic;
    signal PRO_main_ENABLE: in std_logic;
    signal PRO_main_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  -- Local and temporary data objects
  signal REG_d_0: std_logic;
  signal REG_d_0_p_0_WR: std_logic;
  signal REG_d_0_p_0_WE: std_logic;
  signal REG_d_1: std_logic;
  signal REG_d_1_p_1_WR: std_logic;
  signal REG_d_1_p_1_WE: std_logic;
  signal REG_d_2: std_logic;
  signal REG_d_2_p_2_WR: std_logic;
  signal REG_d_2_p_2_WE: std_logic;
  signal REG_d_3: std_logic;
  signal REG_d_3_p_3_WR: std_logic;
  signal REG_d_3_p_3_WE: std_logic;
  signal PRO_main_ENABLE: std_logic;
  signal PRO_main_END: std_logic;
  signal PRO_p_0_ENABLE: std_logic;
  signal PRO_p_0_END: std_logic;
  signal PRO_p_0_main_START: std_logic;
  signal PRO_p_0_main_GD: std_logic;
  signal PRO_p_1_ENABLE: std_logic;
  signal PRO_p_1_END: std_logic;
  signal PRO_p_1_main_START: std_logic;
  signal PRO_p_1_main_GD: std_logic;
  signal PRO_p_2_ENABLE: std_logic;
  signal PRO_p_2_END: std_logic;
  signal PRO_p_2_main_START: std_logic;
  signal PRO_p_2_main_GD: std_logic;
  signal PRO_p_3_ENABLE: std_logic;
  signal PRO_p_3_END: std_logic;
  signal PRO_p_3_main_START: std_logic;
  signal PRO_p_3_main_GD: std_logic;
  signal MUTEX_mu_p_3_LOCK: std_logic;
  signal MUTEX_mu_p_2_LOCK: std_logic;
  signal MUTEX_mu_p_1_LOCK: std_logic;
  signal MUTEX_mu_p_0_LOCK: std_logic;
  signal MUTEX_mu_p_3_UNLOCK: std_logic;
  signal MUTEX_mu_p_2_UNLOCK: std_logic;
  signal MUTEX_mu_p_1_UNLOCK: std_logic;
  signal MUTEX_mu_p_0_UNLOCK: std_logic;
  signal MUTEX_mu_main_INIT: std_logic;
  signal MUTEX_mu_main_GD: std_logic;
  signal MUTEX_mu_main_LOCKed: std_logic;
  signal MUTEX_mu_p_3_GD: std_logic;
  signal MUTEX_mu_p_3_LOCKed: std_logic;
  signal MUTEX_mu_p_2_GD: std_logic;
  signal MUTEX_mu_p_2_LOCKed: std_logic;
  signal MUTEX_mu_p_1_GD: std_logic;
  signal MUTEX_mu_p_1_LOCKed: std_logic;
  signal MUTEX_mu_p_0_GD: std_logic;
  signal MUTEX_mu_p_0_LOCKed: std_logic;
  signal MUTEX_mu_LOCKed: std_logic;
  -- State Processing
  -- Aux. signals
  signal conpro_system_clk: std_logic;
  signal conpro_system_reset: std_logic;
begin
  -- Module implementation
  -- Register
  IMPL_REG_d_0: process(
          REG_d_0_p_0_WR,
          REG_d_0_p_0_WE,
          REG_d_0,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_d_0 <= '0';
     elsif REG_d_0_p_0_WE='1' then
      REG_d_0 <= REG_d_0_p_0_WR;
     end if;
    end if;
  end process IMPL_REG_d_0;
  
  -- Register
  IMPL_REG_d_1: process(
          REG_d_1_p_1_WR,
          REG_d_1_p_1_WE,
          REG_d_1,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_d_1 <= '0';
     elsif REG_d_1_p_1_WE='1' then
      REG_d_1 <= REG_d_1_p_1_WR;
     end if;
    end if;
  end process IMPL_REG_d_1;
  
  -- Register
  IMPL_REG_d_2: process(
          REG_d_2_p_2_WR,
          REG_d_2_p_2_WE,
          REG_d_2,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_d_2 <= '0';
     elsif REG_d_2_p_2_WE='1' then
      REG_d_2 <= REG_d_2_p_2_WR;
     end if;
    end if;
  end process IMPL_REG_d_2;
  
  -- Register
  IMPL_REG_d_3: process(
          REG_d_3_p_3_WR,
          REG_d_3_p_3_WE,
          REG_d_3,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_d_3 <= '0';
     elsif REG_d_3_p_3_WE='1' then
      REG_d_3 <= REG_d_3_p_3_WR;
     end if;
    end if;
  end process IMPL_REG_d_3;
  
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
  PRO_CONTROL_p_0: process(
          PRO_p_0_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_p_0_ENABLE <= '0';
        PRO_p_0_main_GD <= '1';
      elsif PRO_p_0_main_START = '1' then
        PRO_p_0_ENABLE <= '1';
        PRO_p_0_main_GD <= '0';
      else
        PRO_p_0_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_p_0;
  
  -- Process control
  PRO_CONTROL_p_1: process(
          PRO_p_1_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_p_1_ENABLE <= '0';
        PRO_p_1_main_GD <= '1';
      elsif PRO_p_1_main_START = '1' then
        PRO_p_1_ENABLE <= '1';
        PRO_p_1_main_GD <= '0';
      else
        PRO_p_1_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_p_1;
  
  -- Process control
  PRO_CONTROL_p_2: process(
          PRO_p_2_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_p_2_ENABLE <= '0';
        PRO_p_2_main_GD <= '1';
      elsif PRO_p_2_main_START = '1' then
        PRO_p_2_ENABLE <= '1';
        PRO_p_2_main_GD <= '0';
      else
        PRO_p_2_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_p_2;
  
  -- Process control
  PRO_CONTROL_p_3: process(
          PRO_p_3_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_p_3_ENABLE <= '0';
        PRO_p_3_main_GD <= '1';
      elsif PRO_p_3_main_START = '1' then
        PRO_p_3_ENABLE <= '1';
        PRO_p_3_main_GD <= '0';
      else
        PRO_p_3_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_p_3;
  
  --
  --  ConPro V2.1.D138 EMI Mutex.mutex V2.11
  --
  
  --
  -- EMI <Object Mutex.mutex.mu> Process
  --
  MUTEX_mu_SCHED: process(MUTEX_mu_main_INIT,
    MUTEX_mu_p_3_LOCKed,
    MUTEX_mu_p_2_LOCKed,
    MUTEX_mu_p_1_LOCKed,
    MUTEX_mu_p_0_LOCKed,
    MUTEX_mu_p_3_LOCK,
    MUTEX_mu_LOCKed,
    MUTEX_mu_p_2_LOCK,
    MUTEX_mu_p_1_LOCK,
    MUTEX_mu_p_0_LOCK,
    MUTEX_mu_p_3_UNLOCK,
    MUTEX_mu_p_2_UNLOCK,
    MUTEX_mu_p_1_UNLOCK,
    MUTEX_mu_p_0_UNLOCK,
    conpro_system_clk,
    conpro_system_reset)
  begin
    MUTEX_mu_main_GD <= '1';
    MUTEX_mu_p_3_GD <= '1';
    MUTEX_mu_p_2_GD <= '1';
    MUTEX_mu_p_1_GD <= '1';
    MUTEX_mu_p_0_GD <= '1';
    if (MUTEX_mu_main_INIT) = ('1') then
      MUTEX_mu_main_GD <= '0';
      if (MUTEX_mu_p_3_LOCKed) = ('1') then
        MUTEX_mu_p_3_GD <= '0';
      end if;
      if (MUTEX_mu_p_2_LOCKed) = ('1') then
        MUTEX_mu_p_2_GD <= '0';
      end if;
      if (MUTEX_mu_p_1_LOCKed) = ('1') then
        MUTEX_mu_p_1_GD <= '0';
      end if;
      if (MUTEX_mu_p_0_LOCKed) = ('1') then
        MUTEX_mu_p_0_GD <= '0';
      end if;
    elsif ((MUTEX_mu_p_3_LOCK) = ('1')) and ((MUTEX_mu_LOCKed) = ('0')) then
      MUTEX_mu_p_3_GD <= '0';
    elsif ((MUTEX_mu_p_2_LOCK) = ('1')) and ((MUTEX_mu_LOCKed) = ('0')) then
      MUTEX_mu_p_2_GD <= '0';
    elsif ((MUTEX_mu_p_1_LOCK) = ('1')) and ((MUTEX_mu_LOCKed) = ('0')) then
      MUTEX_mu_p_1_GD <= '0';
    elsif ((MUTEX_mu_p_0_LOCK) = ('1')) and ((MUTEX_mu_LOCKed) = ('0')) then
      MUTEX_mu_p_0_GD <= '0';
    elsif (MUTEX_mu_p_3_UNLOCK) = ('1') then
      MUTEX_mu_p_3_GD <= '0';
    elsif (MUTEX_mu_p_2_UNLOCK) = ('1') then
      MUTEX_mu_p_2_GD <= '0';
    elsif (MUTEX_mu_p_1_UNLOCK) = ('1') then
      MUTEX_mu_p_1_GD <= '0';
    elsif (MUTEX_mu_p_0_UNLOCK) = ('1') then
      MUTEX_mu_p_0_GD <= '0';
    end if;
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        MUTEX_mu_LOCKed <= '0';
        MUTEX_mu_p_3_LOCKed <= '0';
        MUTEX_mu_p_2_LOCKed <= '0';
        MUTEX_mu_p_1_LOCKed <= '0';
        MUTEX_mu_p_0_LOCKed <= '0';
      else
        if (MUTEX_mu_main_INIT) = ('1') then
          MUTEX_mu_LOCKed <= '0';
          if (MUTEX_mu_p_3_LOCKed) = ('1') then
            MUTEX_mu_p_3_LOCKed <= '0';
          end if;
          if (MUTEX_mu_p_2_LOCKed) = ('1') then
            MUTEX_mu_p_2_LOCKed <= '0';
          end if;
          if (MUTEX_mu_p_1_LOCKed) = ('1') then
            MUTEX_mu_p_1_LOCKed <= '0';
          end if;
          if (MUTEX_mu_p_0_LOCKed) = ('1') then
            MUTEX_mu_p_0_LOCKed <= '0';
          end if;
        elsif ((MUTEX_mu_p_3_LOCK) = ('1')) and ((MUTEX_mu_LOCKed) = ('0')) then
          MUTEX_mu_LOCKed <= '1';
          MUTEX_mu_p_3_LOCKed <= '1';
        elsif ((MUTEX_mu_p_2_LOCK) = ('1')) and ((MUTEX_mu_LOCKed) = ('0')) then
          MUTEX_mu_LOCKed <= '1';
          MUTEX_mu_p_2_LOCKed <= '1';
        elsif ((MUTEX_mu_p_1_LOCK) = ('1')) and ((MUTEX_mu_LOCKed) = ('0')) then
          MUTEX_mu_LOCKed <= '1';
          MUTEX_mu_p_1_LOCKed <= '1';
        elsif ((MUTEX_mu_p_0_LOCK) = ('1')) and ((MUTEX_mu_LOCKed) = ('0')) then
          MUTEX_mu_LOCKed <= '1';
          MUTEX_mu_p_0_LOCKed <= '1';
        elsif (MUTEX_mu_p_3_UNLOCK) = ('1') then
          MUTEX_mu_LOCKed <= '0';
          MUTEX_mu_p_3_LOCKed <= '0';
        elsif (MUTEX_mu_p_2_UNLOCK) = ('1') then
          MUTEX_mu_LOCKed <= '0';
          MUTEX_mu_p_2_LOCKed <= '0';
        elsif (MUTEX_mu_p_1_UNLOCK) = ('1') then
          MUTEX_mu_LOCKed <= '0';
          MUTEX_mu_p_1_LOCKed <= '0';
        elsif (MUTEX_mu_p_0_UNLOCK) = ('1') then
          MUTEX_mu_LOCKed <= '0';
          MUTEX_mu_p_0_LOCKed <= '0';
        end if;
      end if;
    end if;
  end process MUTEX_mu_SCHED;
  --
  -- End of <Object Mutex.mutex.mu>
  --
  
  
  -- Process instantiations
  PRO_MAP_p_0: mu_p_0 port map(
    REG_d_0_WR => REG_d_0_p_0_WR,
    REG_d_0_WE => REG_d_0_p_0_WE,
    MUTEX_mu_LOCK => MUTEX_mu_p_0_LOCK,
    MUTEX_mu_UNLOCK => MUTEX_mu_p_0_UNLOCK,
    MUTEX_mu_GD => MUTEX_mu_p_0_GD,
    PRO_p_0_ENABLE => PRO_p_0_ENABLE,
    PRO_p_0_END => PRO_p_0_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_p_1: mu_p_1 port map(
    REG_d_1_WR => REG_d_1_p_1_WR,
    REG_d_1_WE => REG_d_1_p_1_WE,
    MUTEX_mu_LOCK => MUTEX_mu_p_1_LOCK,
    MUTEX_mu_UNLOCK => MUTEX_mu_p_1_UNLOCK,
    MUTEX_mu_GD => MUTEX_mu_p_1_GD,
    PRO_p_1_ENABLE => PRO_p_1_ENABLE,
    PRO_p_1_END => PRO_p_1_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_p_2: mu_p_2 port map(
    REG_d_2_WR => REG_d_2_p_2_WR,
    REG_d_2_WE => REG_d_2_p_2_WE,
    MUTEX_mu_LOCK => MUTEX_mu_p_2_LOCK,
    MUTEX_mu_UNLOCK => MUTEX_mu_p_2_UNLOCK,
    MUTEX_mu_GD => MUTEX_mu_p_2_GD,
    PRO_p_2_ENABLE => PRO_p_2_ENABLE,
    PRO_p_2_END => PRO_p_2_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_p_3: mu_p_3 port map(
    REG_d_3_WR => REG_d_3_p_3_WR,
    REG_d_3_WE => REG_d_3_p_3_WE,
    MUTEX_mu_LOCK => MUTEX_mu_p_3_LOCK,
    MUTEX_mu_UNLOCK => MUTEX_mu_p_3_UNLOCK,
    MUTEX_mu_GD => MUTEX_mu_p_3_GD,
    PRO_p_3_ENABLE => PRO_p_3_ENABLE,
    PRO_p_3_END => PRO_p_3_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_main: mu_main port map(
    PRO_p_0_START => PRO_p_0_main_START,
    PRO_p_0_GD => PRO_p_0_main_GD,
    PRO_p_1_START => PRO_p_1_main_START,
    PRO_p_1_GD => PRO_p_1_main_GD,
    PRO_p_2_START => PRO_p_2_main_START,
    PRO_p_2_GD => PRO_p_2_main_GD,
    PRO_p_3_START => PRO_p_3_main_START,
    PRO_p_3_GD => PRO_p_3_main_GD,
    MUTEX_mu_INIT => MUTEX_mu_main_INIT,
    MUTEX_mu_GD => MUTEX_mu_main_GD,
    PRO_main_ENABLE => PRO_main_ENABLE,
    PRO_main_END => PRO_main_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  
  -- Toplevel assignments
  -- Monitors
  d_0_RD <= REG_d_0;
  d_1_RD <= REG_d_1;
  d_2_RD <= REG_d_2;
  d_3_RD <= REG_d_3;
  conpro_system_clk <= CLK;
  conpro_system_reset <= RESET;
end main;
