--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D161 Genetic size: 2703860
--         Compile date: Wed Jun 30 14:12:50 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Implementation of Module <C>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity MOD_c is
port(
  -- Connections to the outside world
  signal CLK: in std_logic;
  signal RESET: in std_logic
);
end MOD_c;
architecture main of MOD_c is
  -- Process instances
  component c_p1
  port(
    -- Connections to external objects, components and the outside world
    signal REG_x_RD: in signed(7 downto 0);
    signal REG_x_WR: out signed(7 downto 0);
    signal REG_x_WE: out std_logic;
    signal REG_x_GD: in std_logic;
    signal PRO_p1_ENABLE: in std_logic;
    signal PRO_p1_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component c_p2
  port(
    -- Connections to external objects, components and the outside world
    signal REG_x_RD: in signed(7 downto 0);
    signal REG_x_WR: out signed(7 downto 0);
    signal REG_x_WE: out std_logic;
    signal REG_x_GD: in std_logic;
    signal PRO_p2_ENABLE: in std_logic;
    signal PRO_p2_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component c_main
  port(
    -- Connections to external objects, components and the outside world
    signal REG_x_WR: out signed(7 downto 0);
    signal REG_x_WE: out std_logic;
    signal REG_x_GD: in std_logic;
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
  signal REG_x: signed(7 downto 0);
  signal REG_x_LOCKED: std_logic;
  signal REG_x_RD: signed(7 downto 0);
  signal REG_x_p1_WR: signed(7 downto 0);
  signal REG_x_p1_WE: std_logic;
  signal REG_x_p2_WR: signed(7 downto 0);
  signal REG_x_p2_WE: std_logic;
  signal REG_x_main_WR: signed(7 downto 0);
  signal REG_x_main_WE: std_logic;
  signal REG_x_p1_GD: std_logic;
  signal REG_x_p2_GD: std_logic;
  signal REG_x_main_GD: std_logic;
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
  -- Guarded Register
  IMPL_REG_x: process(
          REG_x_p1_WR,
          REG_x_p1_WE,
          REG_x_p2_WR,
          REG_x_p2_WE,
          REG_x_main_WR,
          REG_x_main_WE,
          REG_x_LOCKED,
          REG_x,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_x_RD <= REG_x;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_x <= to_signed(0,8);
      REG_x_LOCKED <= '0';
      REG_x_p1_GD <= '1';
      REG_x_p2_GD <= '1';
      REG_x_main_GD <= '1';
     else
      REG_x_p1_GD <= '1';
      REG_x_p2_GD <= '1';
      REG_x_main_GD <= '1';
      if REG_x_LOCKED = '0' and REG_x_p1_WE='1' then
       REG_x <= REG_x_p1_WR;
       REG_x_p1_GD <= '0';
       REG_x_LOCKED <= '1';
      elsif REG_x_LOCKED = '0' and REG_x_p2_WE='1' then
       REG_x <= REG_x_p2_WR;
       REG_x_p2_GD <= '0';
       REG_x_LOCKED <= '1';
      elsif REG_x_LOCKED = '0' and REG_x_main_WE='1' then
       REG_x <= REG_x_main_WR;
       REG_x_main_GD <= '0';
       REG_x_LOCKED <= '1';
      elsif REG_x_LOCKED = '1' then
       REG_x_LOCKED <= '0';
       REG_x_p1_GD <= '1';
       REG_x_p2_GD <= '1';
       REG_x_main_GD <= '1';
      end if;
     end if;
    end if;
  end process IMPL_REG_x;
  
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
  PRO_MAP_p1: c_p1 port map(
    REG_x_RD => REG_x_RD,
    REG_x_WR => REG_x_p1_WR,
    REG_x_WE => REG_x_p1_WE,
    REG_x_GD => REG_x_p1_GD,
    PRO_p1_ENABLE => PRO_p1_ENABLE,
    PRO_p1_END => PRO_p1_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_p2: c_p2 port map(
    REG_x_RD => REG_x_RD,
    REG_x_WR => REG_x_p2_WR,
    REG_x_WE => REG_x_p2_WE,
    REG_x_GD => REG_x_p2_GD,
    PRO_p2_ENABLE => PRO_p2_ENABLE,
    PRO_p2_END => PRO_p2_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_main: c_main port map(
    REG_x_WR => REG_x_main_WR,
    REG_x_WE => REG_x_main_WE,
    REG_x_GD => REG_x_main_GD,
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
  conpro_system_clk <= CLK;
  conpro_system_reset <= RESET;
end main;
