--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D175 Genetic size: 2714497
--         Compile date: Fri Apr  1 18:08:25 CEST 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- ConPro VHDL Library.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
--
-- ConPro2 library
--
--  Version 2.1.8
--

package ConPRO is
  function min(n,m:natural) return natural;
  function max(n,m:natural) return natural;
  function cmult(A,B: SIGNED) return SIGNED;
  function cmult(A,B: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function "+" (L: std_logic; R:integer) return std_logic;
  function Ln_to_Lm(L_n:std_logic_vector;n,m:natural) return std_logic_vector;
  function Lm_to_Ln(L_m:std_logic_vector;m,n:natural) return std_logic_vector;
  function Ln_1_to_Lm(L_n:std_logic;n,m:natural) return std_logic_vector;
  function Lm_to_Ln_1(L_m:std_logic_vector;m,n:natural) return std_logic;
  function Ln_to_Lm_1(L_n:std_logic_vector;n,m:natural) return std_logic_vector;
  function In_to_Im(I_n:signed;n,m:natural) return signed;
  function Im_to_In(I_m:signed;m,n:natural) return signed;
  function Im_to_Ln_1(I_m:signed;m,n:natural) return std_logic ;
  function I_to_L(I:signed) return std_logic_vector;
  function In_to_Lm(I_n:signed;n,m:natural) return std_logic_vector;
  function Im_to_Ln(I_m:signed;m,n:natural) return std_logic_vector;
  function L_to_I(L:std_logic_vector) return signed;
  function L_to_B(L:std_logic_vector) return boolean;
  function Ln_1_to_B(L:std_logic) return boolean;
  function Ln_to_B(L:std_logic_vector;n:natural) return boolean;
  function I_to_B(I:signed) return boolean;
  function In_to_B(I:signed;n:natural) return boolean;
  function Ln_to_Im(L_n:std_logic_vector;n,m:natural) return signed;
  function Lm_to_In(L_m:std_logic_vector;m,n:natural) return signed;
  function N_to_Im(N:natural;m:natural) return signed;
  function L_to_N(L:std_logic_vector) return integer;
  function L_1_to_N(L:std_logic) return integer;
  function I_to_N(I:signed) return integer;
  function B_to_L(B: boolean) return std_logic;
  function "and" (A: signed; B: signed) return signed;
  function "or" (A: signed; B: signed) return signed;
  function "xor" (A: signed; B: signed) return signed;
end ConPRO;

package body ConPRO is

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
  -- cmultiplier with width_A=width_B=width_RES, type I
  --
  function cmult(A,B: SIGNED) return SIGNED is
    variable BA: SIGNED((A'length+B'length-1) downto 0);
    variable BA_n: SIGNED((A'length-1) downto 0);
    begin
      BA := A * B;
      BA_n := resize(BA,A'length);
      return (BA_n);
    end;
  --
  -- cmultiplier with width_A=width_B=width_RES, type L
  --
  function cmult(A,B: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
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
      L := std_logic_vector(unsigned(I));
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
      L_n := std_logic_vector(resize(unsigned(I_m),n));
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
  function Ln_1_to_B(L:std_logic) return boolean is
    variable comp: std_logic := '1';
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
  function B_to_L(B:boolean) return std_logic is
    variable L: std_logic;
    begin
      if B = true then L := '1'; else L := '0'; end if;
      return L;
    end;
  function "and" (A:signed; B:signed) return signed is
    begin
      return L_to_I(I_to_L(A) and I_to_L(B));
    end;
  function "or" (A:signed; B:signed) return signed is
    begin
      return L_to_I(I_to_L(A) or I_to_L(B));
    end;
  function "xor" (A:signed; B:signed) return signed is
    begin
      return L_to_I(I_to_L(A) xor I_to_L(B));
    end;
end ConPRO;
