--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D112-M4 Genetic size: 2326415
--         Compile date: Wed Sep 30 09:43:14 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <p1> from module <L>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity l_p1 is
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
end l_p1;
architecture main of l_p1 is
  -- Local and temporary data objects
  signal d1: std_logic;
  signal d2: std_logic;
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_p1_start, -- PROCESS0[:0]
    S_i1_assign, -- ASSIGN87757[l.cp:10]
    S_i2_assign, -- ASSIGN59662[l.cp:11]
    S_i3_while_loop, -- COND_LOOP44358[l.cp:12]
    S_i4_while_loop, -- COND_LOOP65705[l.cp:14]
    S_i5_assign, -- ASSIGN17429[l.cp:16]
    S_i6_assign, -- ASSIGN39314[l.cp:17]
    S_i7_assign, -- ASSIGN2877[l.cp:19]
    S_p1_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_p1_start;
  signal pro_state_next: pro_states := S_p1_start;
  -- Auxilliary toplevel definitions
  constant CONST_I8_1: signed(7 downto 0) := "00000001";
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
begin
  state_transition: process(
          PRO_p1_ENABLE,
          pro_state_next,
          conpro_system_clk,
          conpro_system_reset
  )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset='1' or PRO_p1_ENABLE='0' then
        pro_state <= S_p1_start;
      else
        pro_state <= pro_state_next;
      end if;
    end if;
  end process state_transition;
  -- Process implementation
  -- Instruction Controlpath Unit - The Leitwerk
  control_path: process(
          d1,
          d2,
          pro_state
          )
  begin
    PRO_p1_END <= '0';
    case pro_state is
      when S_p1_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_assign;
      when S_i1_assign => -- ASSIGN87757[l.cp:10]
        pro_state_next <= S_i2_assign;
      when S_i2_assign => -- ASSIGN59662[l.cp:11]
        pro_state_next <= S_i3_while_loop;
      when S_i3_while_loop => -- COND_LOOP44358[l.cp:12]
        if d1 = '1' then
          pro_state_next <= S_i4_while_loop;
        else
          pro_state_next <= S_p1_end;
        end if;
      when S_i4_while_loop => -- COND_LOOP65705[l.cp:14]
        if d2 = '1' then
          pro_state_next <= S_i5_assign;
        else
          pro_state_next <= S_i7_assign;
        end if;
      when S_i5_assign => -- ASSIGN17429[l.cp:16]
        pro_state_next <= S_i6_assign;
      when S_i6_assign => -- ASSIGN39314[l.cp:17]
        pro_state_next <= S_i4_while_loop;
      when S_i7_assign => -- ASSIGN2877[l.cp:19]
        pro_state_next <= S_i3_while_loop;
      when S_p1_end => -- PROCESS0[:0]
        pro_state_next <= S_p1_end;
        PRO_p1_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          REG_x_RD,
          pro_state
          )
  begin
    -- Default values
    REG_x_WR <= to_signed(0,8);
    REG_x_WE <= '0';
    case pro_state is
      when S_p1_start => -- PROCESS0[:0]
        null;
      when S_i1_assign => -- ASSIGN87757[l.cp:10]
        null;
      when S_i2_assign => -- ASSIGN59662[l.cp:11]
        null;
      when S_i3_while_loop => -- COND_LOOP44358[l.cp:12]
        null;
      when S_i4_while_loop => -- COND_LOOP65705[l.cp:14]
        null;
      when S_i5_assign => -- ASSIGN17429[l.cp:16]
        REG_x_WR <= REG_x_RD + CONST_I8_1;
        REG_x_WE <= '1';
      when S_i6_assign => -- ASSIGN39314[l.cp:17]
        null;
      when S_i7_assign => -- ASSIGN2877[l.cp:19]
        null;
      when S_p1_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        d1 <= '0';
        d2 <= '0';
      else
        case pro_state is
          when S_p1_start => -- PROCESS0[:0]
            null;
          when S_i1_assign => -- ASSIGN87757[l.cp:10]
            d1 <= '1';
          when S_i2_assign => -- ASSIGN59662[l.cp:11]
            d2 <= '1';
          when S_i3_while_loop => -- COND_LOOP44358[l.cp:12]
            null;
          when S_i4_while_loop => -- COND_LOOP65705[l.cp:14]
            null;
          when S_i5_assign => -- ASSIGN17429[l.cp:16]
            null;
          when S_i6_assign => -- ASSIGN39314[l.cp:17]
            d2 <= '0';
          when S_i7_assign => -- ASSIGN2877[l.cp:19]
            d1 <= '0';
          when S_p1_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;