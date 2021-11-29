--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D112-M4 Genetic size: 2326415
--         Compile date: Wed Sep 30 09:43:14 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Implementation of Module <L>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity MOD_l is
port(
  -- Connections to the outside world
  signal x_RD: out std_logic_vector(7 downto 0);
  signal CLK: in std_logic;
  signal RESET: in std_logic
);
-- ConPro VHDL Library
--
-- ConPro2 library
--
--  Version 2.1.4
--


function min(n,m:natural) return natural is
  begin
    if n < m then return n;
    elsif m < n then return m;
    else return n; end if;
  end;
function max(n,m:natural) return natural is
  begin
    if n > m then return n;
    elsif m > n then return m;
    else return n; end if;
  end;

--
-- multiplier with width_A=width_B=width_RES, type I
--
function mult(A,B: SIGNED) return SIGNED is
  variable BA: SIGNED((A'length+B'length-1) downto 0);
  variable BA_n: SIGNED((A'length-1) downto 0);
  begin
    BA := A * B;
    BA_n := resize(BA,A'length);
    return (BA_n);
  end;
--
-- multiplier with width_A=width_B=width_RES, type L
--
function mult(A,B: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
  variable BA: STD_LOGIC_VECTOR((A'length+B'length-1) downto 0);
  variable BA_n: STD_LOGIC_VECTOR((A'length-1) downto 0);
  begin
    BA := A * B;
    BA_n := BA((A'length-1) downto 0);
    return (BA_n);
  end;
--
-- Overloading of 1 bit adder, type L
--
function "+" (L: std_logic; R:integer) return std_logic is
  begin
    if R = 0 then return L; else return (L xor '1'); end if;
  end "+";

--
-- Type conversion
-- ConPro types! L: logic(std_logic), I:integer(signed), N:natural(integer)
-- Argument width relation: n <= m
--
function Ln_to_Lm(L_n:std_logic_vector;n,m:natural) return std_logic_vector is
  variable fill: std_logic_vector(max(1,m-n-1) downto 0) := (others => '0');
  variable L_m: std_logic_vector(m-1 downto 0);
  begin
    if (m-n) > 1 then L_m := fill & L_n;
    else L_m := '0' & L_n; end if;
    return L_m;
  end;
function Lm_to_Ln(L_m:std_logic_vector;m,n:natural) return std_logic_vector is
  variable L_n: std_logic_vector(n-1 downto 0);
  begin
    L_n := L_m(n-1 downto 0);
    return L_n;
  end;
function Ln_1_to_Lm(L_n:std_logic;n,m:natural) return std_logic_vector is
  variable fill: std_logic_vector(max(1,m-n-1) downto 0) := (others => '0');
  variable L_m: std_logic_vector(m-1 downto 0);
  begin
    if (m-n) > 1 then L_m := fill & L_n;
    else L_m := '0' & L_n; end if;
    return L_m;
  end;
function Lm_to_Ln_1(L_m:std_logic_vector;m,n:natural) return std_logic is
  variable L_n: std_logic;
  begin
    L_n := L_m(0);
    return L_n;
  end;
function Ln_to_Lm_1(L_n:std_logic_vector;n,m:natural) return std_logic_vector is
  variable L_m: std_logic_vector(m-1 downto 0);
  begin
    L_m := '1' & L_n;
    return L_m;
  end;
function In_to_Im(I_n:signed;n,m:natural) return signed is
  variable I_m: signed(m-1 downto 0);
  begin
    I_m := resize(I_n,m);
    return I_m;
  end;
function Im_to_In(I_m:signed;m,n:natural) return signed is
  variable I_n: signed(n-1 downto 0);
  begin
    I_n := resize(I_m,n);
    return I_n;
  end;
function Im_to_Ln_1(I_m:signed;m,n:natural) return std_logic is
  variable L_n: std_logic;
  begin
    L_n := std_logic(I_m(0));
    return L_n;
  end;
function I_to_L(I:signed) return std_logic_vector is
  variable L: std_logic_vector(I'length-1 downto 0);
  begin
    L := std_logic_vector(I);
    return L;
  end;
function In_to_Lm(I_n:signed;n,m:natural) return std_logic_vector is
  variable L_m: std_logic_vector(m-1 downto 0);
  begin
    L_m := std_logic_vector(resize(I_n,m));
    return L_m;
  end;
function Im_to_Ln(I_m:signed;m,n:natural) return std_logic_vector is
  variable L_n: std_logic_vector(n-1 downto 0);
  begin
    L_n := std_logic_vector(resize(I_m,n));
    return L_n;
  end;
function L_to_I(L:std_logic_vector) return signed is
  variable I: signed(L'length-1 downto 0);
  begin
    I := signed(L);
    return I;
  end;
function L_to_B(L:std_logic_vector) return boolean is
  variable comp: std_logic_vector(L'length-1 downto 0) := (others => '1');
  variable B: boolean;
  begin
    B := (L = comp);
    return B;
  end;
function Ln_to_B(L:std_logic_vector;n:natural) return boolean is
  variable comp: std_logic_vector(n-1 downto 0) := (others => '1');
  variable B: boolean;
  begin
    B := (L = comp);
    return B;
  end;
function I_to_B(I:signed) return boolean is
  variable comp: signed(I'length-1 downto 0) := (others => '1');
  variable B: boolean;
  begin
    B := (I = comp);
    return B;
  end;
function In_to_B(I:signed;n:natural) return boolean is
  variable comp: signed(n-1 downto 0) := (others => '1');
  variable B: boolean;
  begin
    B := (I = comp);
    return B;
  end;
function Ln_to_Im(L_n:std_logic_vector;n,m:natural) return signed is
  variable I_m: signed(m-1 downto 0);
  begin
    I_m := resize(signed(L_n),m);
    return I_m;
  end;
function Lm_to_In(L_m:std_logic_vector;m,n:natural) return signed is
  variable I_n: signed(n-1 downto 0);
  begin
    I_n := resize(signed(L_m),n);
    return I_n;
  end;
function N_to_Im(N:natural;m:natural) return signed is
  variable I_m: signed(m-1 downto 0);
  begin
    I_m := to_signed(N,m);
    return I_m;
  end;
function L_to_N(L:std_logic_vector) return integer is
  variable N: integer;
  begin
    N := conv_integer(L);
    return N;
  end;
function L_1_to_N(L:std_logic) return integer is
  variable N: integer;
  variable L_2: std_logic_vector(1 downto 0); 
  begin
    L_2 := '0' & L;
    N := conv_integer(L_2);
    return N;
  end;
function I_to_N(I:signed) return integer is
  variable N: integer;
  begin
    N := to_integer(I);
    return N;
  end;
end MOD_l;
architecture main of MOD_l is
  -- Process instances
  component l_p1
  port(
    -- Connections to external objects, components and the outside world
    signal REG_x_RD: in signed(7 downto 0);
    signal REG_x_WR: out signed(7 downto 0);
    signal REG_x_WE: out std_logic;
    signal PRO_p1_ENABLE: in std_logic;
    signal PRO_p1_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component l_main
  port(
    -- Connections to external objects, components and the outside world
    signal PRO_p1_START: out std_logic;
    signal PRO_p1_GD: in std_logic;
    signal PRO_main_ENABLE: in std_logic;
    signal PRO_main_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  -- Local and temporary data objects
  signal REG_x: signed(7 downto 0);
  signal REG_x_RD: signed(7 downto 0);
  signal REG_x_p1_WR: signed(7 downto 0);
  signal REG_x_p1_WE: std_logic;
  signal PRO_main_ENABLE: std_logic;
  signal PRO_main_END: std_logic;
  signal PRO_p1_ENABLE: std_logic;
  signal PRO_p1_END: std_logic;
  signal PRO_p1_main_START: std_logic;
  signal PRO_p1_main_GD: std_logic;
  -- State Processing
  -- Aux. signals
  signal conpro_system_clk: std_logic;
  signal conpro_system_reset: std_logic;
begin
  -- Module implementation
  -- Register
  IMPL_REG_x: process(
          REG_x_p1_WR,
          REG_x_p1_WE,
          REG_x,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_x_RD <= REG_x;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_x <= to_signed(0,8);
     elsif REG_x_p1_WE='1' then
      REG_x <= REG_x_p1_WR;
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
  
  
  -- Process instantiations
  PRO_MAP_p1: l_p1 port map(
    REG_x_RD => REG_x_RD,
    REG_x_WR => REG_x_p1_WR,
    REG_x_WE => REG_x_p1_WE,
    PRO_p1_ENABLE => PRO_p1_ENABLE,
    PRO_p1_END => PRO_p1_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_main: l_main port map(
    PRO_p1_START => PRO_p1_main_START,
    PRO_p1_GD => PRO_p1_main_GD,
    PRO_main_ENABLE => PRO_main_ENABLE,
    PRO_main_END => PRO_main_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  
  -- Toplevel assignments
  -- Monitors
  x_RD <= std_logic_vector(REG_x);
  conpro_system_clk <= CLK;
  conpro_system_reset <= RESET;
end main;
