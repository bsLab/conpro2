--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D174 Genetic size: 2713135
--         Compile date: Fri Jan 21 09:49:21 CET 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Implementation of Module <L2_simu>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity MOD_l2_simu is
port(
  -- Connections to the outside world
  signal Lc2_x_RD: out std_logic_vector(11 downto 0);
  signal Lc2_xa_RD: out std_logic_vector(7 downto 0);
  signal Lc1_x_RD: out std_logic_vector(11 downto 0);
  signal Lc1_xa_RD: out std_logic_vector(7 downto 0);
  signal CLK1: in std_logic;
  signal CLK2: in std_logic;
  signal RESET: in std_logic
);
end MOD_l2_simu;
architecture main of MOD_l2_simu is
  signal Lc1_DEV_ln_din: std_logic_vector(1 downto 0);
  signal Lc1_DEV_ln_din_ack: std_logic;
  signal Lc1_DEV_ln_dout: std_logic_vector(1 downto 0);
  signal Lc1_DEV_ln_dout_ack: std_logic;
  signal Lc2_DEV_ln_din: std_logic_vector(1 downto 0);
  signal Lc2_DEV_ln_din_ack: std_logic;
  signal Lc2_DEV_ln_dout: std_logic_vector(1 downto 0);
  signal Lc2_DEV_ln_dout_ack: std_logic;
  component MOD_l2
  port(
    -- Connections to the outside world
    signal x_RD: out std_logic_vector(11 downto 0);
    signal xa_RD: out std_logic_vector(7 downto 0);
    signal DEV_ln_din: in std_logic_vector(1 downto 0);
    signal DEV_ln_din_ack: out std_logic;
    signal DEV_ln_dout: out std_logic_vector(1 downto 0);
    signal DEV_ln_dout_ack: in std_logic;
    signal CLK: in std_logic;
    signal RESET: in std_logic
  );
  end component;
  component l_connect
  port(
    signal Lc1_ln_din: out std_logic_vector(1 downto 0);
    signal Lc1_ln_din_ack: in std_logic;
    signal Lc1_ln_dout: in std_logic_vector(1 downto 0);
    signal Lc1_ln_dout_ack: out std_logic;
    signal Lc2_ln_din: out std_logic_vector(1 downto 0);
    signal Lc2_ln_din_ack: in std_logic;
    signal Lc2_ln_dout: in std_logic_vector(1 downto 0);
    signal Lc2_ln_dout_ack: out std_logic;
    signal CLK: in std_logic;
    signal RESET: in std_logic
  );
  end component;
begin
  -- Module instantiation
  Lc1: MOD_l2 port map(
    x_RD => Lc1_x_RD,
    xa_RD => Lc1_xa_RD,
    DEV_ln_din => Lc1_DEV_ln_din,
    DEV_ln_din_ack => Lc1_DEV_ln_din_ack,
    DEV_ln_dout => Lc1_DEV_ln_dout,
    DEV_ln_dout_ack => Lc1_DEV_ln_dout_ack,
    CLK => CLK1,
    RESET => RESET
  );
  Lc2: MOD_l2 port map(
    x_RD => Lc2_x_RD,
    xa_RD => Lc2_xa_RD,
    DEV_ln_din => Lc2_DEV_ln_din,
    DEV_ln_din_ack => Lc2_DEV_ln_din_ack,
    DEV_ln_dout => Lc2_DEV_ln_dout,
    DEV_ln_dout_ack => Lc2_DEV_ln_dout_ack,
    CLK => CLK2,
    RESET => RESET
  );
  L_c: l_connect port map(
    Lc1_ln_din => Lc1_DEV_ln_din,
    Lc1_ln_din_ack => Lc1_DEV_ln_din_ack,
    Lc1_ln_dout => Lc1_DEV_ln_dout,
    Lc1_ln_dout_ack => Lc1_DEV_ln_dout_ack,
    Lc2_ln_din => Lc2_DEV_ln_din,
    Lc2_ln_din_ack => Lc2_DEV_ln_din_ack,
    Lc2_ln_dout => Lc2_DEV_ln_dout,
    Lc2_ln_dout_ack => Lc2_DEV_ln_dout_ack,
    CLK => CLK1,
    RESET => RESET
  );
end main;
