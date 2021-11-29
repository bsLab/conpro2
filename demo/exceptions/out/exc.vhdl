--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D90 Genetic size: 2230944
--         Compile date: Fri Jul 17 13:28:54 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Implementation of Module <Exc>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity MOD_exc is
port(
  -- Connections to the outside world
  signal jg_RD: out std_logic_vector(7 downto 0);
  signal CLK: in std_logic;
  signal RESET: in std_logic
);
end MOD_exc;
architecture main of MOD_exc is
  -- Process instances
  component exc_p1
  port(
    -- Connections to external objects, components and the outside world
    signal REG_PRO_p1_EXCEPTION_RD: in signed(7 downto 0);
    signal REG_PRO_p1_EXCEPTION_WR: out signed(7 downto 0);
    signal REG_PRO_p1_EXCEPTION_WE: out std_logic;
    signal REG_jg_WR: out signed(7 downto 0);
    signal REG_jg_WE: out std_logic;
    signal REG_jg_GD: in std_logic;
    signal PRO_p1_ENABLE: in std_logic;
    signal PRO_p1_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component exc_p2
  port(
    -- Connections to external objects, components and the outside world
    signal REG_jg_WR: out signed(7 downto 0);
    signal REG_jg_WE: out std_logic;
    signal REG_jg_GD: in std_logic;
    signal REG_PRO_p2_EXCEPTION_WR: out signed(7 downto 0);
    signal REG_PRO_p2_EXCEPTION_WE: out std_logic;
    signal PRO_p2_ENABLE: in std_logic;
    signal PRO_p2_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component exc_main
  port(
    -- Connections to external objects, components and the outside world
    signal REG_jg_WR: out signed(7 downto 0);
    signal REG_jg_WE: out std_logic;
    signal REG_jg_GD: in std_logic;
    signal REG_PRO_p2_EXCEPTION_RD: in signed(7 downto 0);
    signal PRO_p1_START: out std_logic;
    signal PRO_p1_GD: in std_logic;
    signal PRO_p2_CALL: out std_logic;
    signal PRO_p2_GD: in std_logic;
    signal REG_PRO_main_EXCEPTION_RD: in signed(7 downto 0);
    signal REG_PRO_main_EXCEPTION_WR: out signed(7 downto 0);
    signal REG_PRO_main_EXCEPTION_WE: out std_logic;
    signal PRO_main_ENABLE: in std_logic;
    signal PRO_main_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  -- Local and temporary data objects
  signal REG_PRO_p1_EXCEPTION: signed(7 downto 0);
  signal REG_PRO_p1_EXCEPTION_RD: signed(7 downto 0);
  signal REG_PRO_p1_EXCEPTION_p1_WR: signed(7 downto 0);
  signal REG_PRO_p1_EXCEPTION_p1_WE: std_logic;
  signal REG_jg: signed(7 downto 0);
  signal REG_jg_LOCKED: std_logic;
  signal REG_jg_p1_WR: signed(7 downto 0);
  signal REG_jg_p1_WE: std_logic;
  signal REG_jg_p2_WR: signed(7 downto 0);
  signal REG_jg_p2_WE: std_logic;
  signal REG_jg_main_WR: signed(7 downto 0);
  signal REG_jg_main_WE: std_logic;
  signal REG_jg_p1_GD: std_logic;
  signal REG_jg_p2_GD: std_logic;
  signal REG_jg_main_GD: std_logic;
  signal PRO_main_ENABLE: std_logic;
  signal PRO_main_END: std_logic;
  signal REG_PRO_p2_EXCEPTION: signed(7 downto 0);
  signal REG_PRO_p2_EXCEPTION_RD: signed(7 downto 0);
  signal REG_PRO_p2_EXCEPTION_p2_WR: signed(7 downto 0);
  signal REG_PRO_p2_EXCEPTION_p2_WE: std_logic;
  signal PRO_p1_ENABLE: std_logic;
  signal PRO_p1_END: std_logic;
  signal PRO_p1_main_START: std_logic;
  signal PRO_p1_main_GD: std_logic;
  signal PRO_p2_ENABLE: std_logic;
  signal PRO_p2_END: std_logic;
  signal PRO_p2_main_CALL: std_logic;
  signal PRO_p2_main_GD: std_logic;
  signal REG_PRO_main_EXCEPTION: signed(7 downto 0);
  signal REG_PRO_main_EXCEPTION_RD: signed(7 downto 0);
  signal REG_PRO_main_EXCEPTION_main_WR: signed(7 downto 0);
  signal REG_PRO_main_EXCEPTION_main_WE: std_logic;
  -- State Processing
  -- Aux. signals
  signal conpro_system_clk: std_logic;
  signal conpro_system_reset: std_logic;
begin
  -- Module implementation
  -- Register
  IMPL_REG_PRO_p1_EXCEPTION: process(
          REG_PRO_p1_EXCEPTION_p1_WR,
          REG_PRO_p1_EXCEPTION_p1_WE,
          REG_PRO_p1_EXCEPTION,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_PRO_p1_EXCEPTION_RD <= REG_PRO_p1_EXCEPTION;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_PRO_p1_EXCEPTION <= to_signed(0,8);
     elsif REG_PRO_p1_EXCEPTION_p1_WE='1' then
      REG_PRO_p1_EXCEPTION <= REG_PRO_p1_EXCEPTION_p1_WR;
     end if;
    end if;
  end process IMPL_REG_PRO_p1_EXCEPTION;
  
  -- Guarded Register
  IMPL_REG_jg: process(
          REG_jg_p1_WR,
          REG_jg_p1_WE,
          REG_jg_p2_WR,
          REG_jg_p2_WE,
          REG_jg_main_WR,
          REG_jg_main_WE,
          REG_jg_LOCKED,
          REG_jg,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_jg <= to_signed(0,8);
      REG_jg_LOCKED <= '0';
      REG_jg_p1_GD <= '1';
      REG_jg_p2_GD <= '1';
      REG_jg_main_GD <= '1';
     else
      REG_jg_p1_GD <= '1';
      REG_jg_p2_GD <= '1';
      REG_jg_main_GD <= '1';
      if REG_jg_LOCKED = '0' and REG_jg_p1_WE='1' then
       REG_jg <= REG_jg_p1_WR;
       REG_jg_p1_GD <= '0';
       REG_jg_LOCKED <= '1';
      elsif REG_jg_LOCKED = '0' and REG_jg_p2_WE='1' then
       REG_jg <= REG_jg_p2_WR;
       REG_jg_p2_GD <= '0';
       REG_jg_LOCKED <= '1';
      elsif REG_jg_LOCKED = '0' and REG_jg_main_WE='1' then
       REG_jg <= REG_jg_main_WR;
       REG_jg_main_GD <= '0';
       REG_jg_LOCKED <= '1';
      elsif REG_jg_LOCKED = '1' then
       REG_jg_LOCKED <= '0';
       REG_jg_p1_GD <= '1';
       REG_jg_p2_GD <= '1';
       REG_jg_main_GD <= '1';
      end if;
     end if;
    end if;
  end process IMPL_REG_jg;
  
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
  
  -- Register
  IMPL_REG_PRO_p2_EXCEPTION: process(
          REG_PRO_p2_EXCEPTION_p2_WR,
          REG_PRO_p2_EXCEPTION_p2_WE,
          REG_PRO_p2_EXCEPTION,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_PRO_p2_EXCEPTION_RD <= REG_PRO_p2_EXCEPTION;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_PRO_p2_EXCEPTION <= to_signed(0,8);
     elsif REG_PRO_p2_EXCEPTION_p2_WE='1' then
      REG_PRO_p2_EXCEPTION <= REG_PRO_p2_EXCEPTION_p2_WR;
     end if;
    end if;
  end process IMPL_REG_PRO_p2_EXCEPTION;
  
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
          PRO_p2_main_CALL,
          PRO_p2_END,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_p2_ENABLE <= '0';
        PRO_p2_main_GD <= '1';
      elsif PRO_p2_main_CALL = '1' and PRO_p2_END = '1' then
        PRO_p2_main_GD <= '0';
        PRO_p2_ENABLE <= '0';
      elsif PRO_p2_main_CALL = '1' and PRO_p2_END = '0' then
        PRO_p2_ENABLE <= '1';
      else
        PRO_p2_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_p2;
  
  -- Register
  IMPL_REG_PRO_main_EXCEPTION: process(
          REG_PRO_main_EXCEPTION_main_WR,
          REG_PRO_main_EXCEPTION_main_WE,
          REG_PRO_main_EXCEPTION,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_PRO_main_EXCEPTION_RD <= REG_PRO_main_EXCEPTION;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_PRO_main_EXCEPTION <= to_signed(0,8);
     elsif REG_PRO_main_EXCEPTION_main_WE='1' then
      REG_PRO_main_EXCEPTION <= REG_PRO_main_EXCEPTION_main_WR;
     end if;
    end if;
  end process IMPL_REG_PRO_main_EXCEPTION;
  
  
  -- Process instantiations
  PRO_MAP_p1: exc_p1 port map(
    REG_PRO_p1_EXCEPTION_RD => REG_PRO_p1_EXCEPTION_RD,
    REG_PRO_p1_EXCEPTION_WR => REG_PRO_p1_EXCEPTION_p1_WR,
    REG_PRO_p1_EXCEPTION_WE => REG_PRO_p1_EXCEPTION_p1_WE,
    REG_jg_WR => REG_jg_p1_WR,
    REG_jg_WE => REG_jg_p1_WE,
    REG_jg_GD => REG_jg_p1_GD,
    PRO_p1_ENABLE => PRO_p1_ENABLE,
    PRO_p1_END => PRO_p1_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_p2: exc_p2 port map(
    REG_jg_WR => REG_jg_p2_WR,
    REG_jg_WE => REG_jg_p2_WE,
    REG_jg_GD => REG_jg_p2_GD,
    REG_PRO_p2_EXCEPTION_WR => REG_PRO_p2_EXCEPTION_p2_WR,
    REG_PRO_p2_EXCEPTION_WE => REG_PRO_p2_EXCEPTION_p2_WE,
    PRO_p2_ENABLE => PRO_p2_ENABLE,
    PRO_p2_END => PRO_p2_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_main: exc_main port map(
    REG_jg_WR => REG_jg_main_WR,
    REG_jg_WE => REG_jg_main_WE,
    REG_jg_GD => REG_jg_main_GD,
    REG_PRO_p2_EXCEPTION_RD => REG_PRO_p2_EXCEPTION_RD,
    PRO_p1_START => PRO_p1_main_START,
    PRO_p1_GD => PRO_p1_main_GD,
    PRO_p2_CALL => PRO_p2_main_CALL,
    PRO_p2_GD => PRO_p2_main_GD,
    REG_PRO_main_EXCEPTION_RD => REG_PRO_main_EXCEPTION_RD,
    REG_PRO_main_EXCEPTION_WR => REG_PRO_main_EXCEPTION_main_WR,
    REG_PRO_main_EXCEPTION_WE => REG_PRO_main_EXCEPTION_main_WE,
    PRO_main_ENABLE => PRO_main_ENABLE,
    PRO_main_END => PRO_main_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  
  -- Toplevel assignments
  -- Monitors
  jg_RD <= std_logic_vector(REG_jg);
  conpro_system_clk <= CLK;
  conpro_system_reset <= RESET;
end main;
