--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D103 Genetic size: 2277029
--         Compile date: Mon Aug 24 17:06:07 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <p1> from module <P>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_unsigned.all;
entity p_p1 is
port(
  -- Connections to external objects, components and the outside world
  signal PORT_pt1_RE: out std_logic;
  signal PORT_pt1_RD: in std_logic_vector(7 downto 0);
  signal PORT_pt1_WE: out std_logic;
  signal PORT_pt1_WR: out std_logic_vector(7 downto 0);
  signal PORT_pt1_DE: out std_logic;
  signal PORT_pt1_DR: out std_logic_vector(7 downto 0);
  signal PORT_pt1_GD: in std_logic;
  signal PORT_pt2_RE: out std_logic;
  signal PORT_pt2_RD: in std_logic_vector(7 downto 0);
  signal PORT_pt2_WE: out std_logic;
  signal PORT_pt2_WR: out std_logic_vector(7 downto 0);
  signal PORT_pt2_DE: out std_logic;
  signal PORT_pt2_DR: out std_logic_vector(7 downto 0);
  signal PORT_pt2_GD: in std_logic;
  signal PRO_p1_ENABLE: in std_logic;
  signal PRO_p1_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end p_p1;
architecture main of p_p1 is
  -- Local and temporary data objects
  signal LOOP_i_0: signed(4 downto 0);
  signal d1: std_logic_vector(7 downto 0);
  signal d2: std_logic_vector(7 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_p1_start, -- PROCESS0[:0]
    S_i1_fun, -- FUN73170[p.cp:21]
    S_i2_fun, -- FUN83041[p.cp:22]
    S_i3_fun, -- FUN83331[p.cp:23]
    S_i4_fun, -- FUN95086[p.cp:24]
    S_i5_for_loop, -- COUNT_LOOP2877[p.cp:26]
    S_i5_for_loop_cond, -- COUNT_LOOP2877[p.cp:26]
    S_i6_assign, -- ASSIGN85449[p.cp:28]
    S_i7_assign, -- ASSIGN23890[p.cp:29]
    S_i8_assign, -- ASSIGN7669[p.cp:30]
    S_i9_fun, -- FUN19215[p.cp:31]
    S_i10_bind_to_11, -- FUN63052[p.cp:32]
    S_i12_assign, -- ASSIGN71138[p.cp:33]
    S_i13_bind_to_14, -- FUN19442[p.cp:34]
    S_i15_fun, -- FUN78627[p.cp:35]
    S_i16_while_loop, -- COND_LOOP48484[p.cp:36]
    S_i16_fun, -- FUN22215[p.cp:37]
    S_i5_for_loop_incr, -- COUNT_LOOP2877[p.cp:26]
    S_p1_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_p1_start;
  signal pro_state_next: pro_states := S_p1_start;
  -- Auxilliary toplevel definitions
  constant CONST_I5_1: signed(4 downto 0) := "00001";
  constant CONST_I5_8: signed(4 downto 0) := "01000";
  -- ConPro VHDL Library
  --
  -- ConPro2 library
  --
  --  Version 2.1.2
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
      BA_n := conv_signed(BA,A'length);
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
  function Ln_to_Lm_1(L_n:std_logic_vector;n,m:natural) return std_logic_vector is
    variable L_m: std_logic_vector(m-1 downto 0);
    begin
      L_m := '1' & L_n;
      return L_m;
    end;
  function In_to_Im(I_n:signed;n,m:natural) return signed is
    variable I_m: signed(m-1 downto 0);
    begin
      I_m := conv_signed(I_n,m);
      return I_m;
    end;
  function Im_to_In(I_m:signed;m,n:natural) return signed is
    variable I_n: signed(n-1 downto 0);
    begin
      I_n := conv_signed(I_m,n);
      return I_n;
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
      L_m := std_logic_vector(conv_signed(I_n,m));
      return L_m;
    end;
  function Im_to_Ln(I_m:signed;m,n:natural) return std_logic_vector is
    variable L_n: std_logic_vector(n-1 downto 0);
    begin
      L_n := std_logic_vector(conv_signed(I_m,n));
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
      I_m := conv_signed(signed(L_n),m);
      return I_m;
    end;
  function Lm_to_In(L_m:std_logic_vector;m,n:natural) return signed is
    variable I_n: signed(n-1 downto 0);
    begin
      I_n := conv_signed(signed(L_m),n);
      return I_n;
    end;
  function N_to_Im(N:natural;m:natural) return signed is
    variable I_m: signed(m-1 downto 0);
    begin
      I_m := conv_signed(N,m);
      return I_m;
    end;
  function L_to_N(L:std_logic_vector) return integer is
    variable N: integer;
    begin
      N := conv_integer(unsigned((L)));
      return N;
    end;
  function I_to_N(I:signed) return integer is
    variable N: integer;
    begin
      N := conv_integer(unsigned((I)));
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
          PORT_pt1_GD,
          PORT_pt2_GD,
          LOOP_i_0,
          d1,
          pro_state
          )
  begin
    PRO_p1_END <= '0';
    case pro_state is
      when S_p1_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_fun;
      when S_i1_fun => -- FUN73170[p.cp:21]
        if not((PORT_pt1_GD) = ('0')) then
          pro_state_next <= S_i1_fun;
        else
          pro_state_next <= S_i2_fun;
        end if;
      when S_i2_fun => -- FUN83041[p.cp:22]
        if not((PORT_pt2_GD) = ('0')) then
          pro_state_next <= S_i2_fun;
        else
          pro_state_next <= S_i3_fun;
        end if;
      when S_i3_fun => -- FUN83331[p.cp:23]
        if not((PORT_pt1_GD) = ('0')) then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_i4_fun;
        end if;
      when S_i4_fun => -- FUN95086[p.cp:24]
        if not((PORT_pt2_GD) = ('0')) then
          pro_state_next <= S_i4_fun;
        else
          pro_state_next <= S_i5_for_loop;
        end if;
      when S_i5_for_loop => -- COUNT_LOOP2877[p.cp:26]
        pro_state_next <= S_i5_for_loop_cond;
      when S_i5_for_loop_cond => -- COUNT_LOOP2877[p.cp:26]
        if CONST_I5_8 >= LOOP_i_0 then
          pro_state_next <= S_i6_assign;
        else
          pro_state_next <= S_p1_end;
        end if;
      when S_i6_assign => -- ASSIGN85449[p.cp:28]
        pro_state_next <= S_i7_assign;
      when S_i7_assign => -- ASSIGN23890[p.cp:29]
        pro_state_next <= S_i8_assign;
      when S_i8_assign => -- ASSIGN7669[p.cp:30]
        pro_state_next <= S_i9_fun;
      when S_i9_fun => -- FUN19215[p.cp:31]
        if not((PORT_pt1_GD) = ('0')) then
          pro_state_next <= S_i9_fun;
        else
          pro_state_next <= S_i10_bind_to_11;
        end if;
      when S_i10_bind_to_11 => -- FUN63052[p.cp:32]
        if (not((PORT_pt2_GD) = ('0'))) and (not((PORT_pt1_GD) = ('0'))) then
          pro_state_next <= S_i10_bind_to_11;
        else
          pro_state_next <= S_i12_assign;
        end if;
      when S_i12_assign => -- ASSIGN71138[p.cp:33]
        pro_state_next <= S_i13_bind_to_14;
      when S_i13_bind_to_14 => -- FUN19442[p.cp:34]
        if (not((PORT_pt2_GD) = ('0'))) and (not((PORT_pt1_GD) = ('0'))) then
          pro_state_next <= S_i13_bind_to_14;
        else
          pro_state_next <= S_i15_fun;
        end if;
      when S_i15_fun => -- FUN78627[p.cp:35]
        if not((PORT_pt1_GD) = ('0')) then
          pro_state_next <= S_i15_fun;
        else
          pro_state_next <= S_i16_while_loop;
        end if;
      when S_i16_while_loop => -- COND_LOOP48484[p.cp:36]
        if d1(0) = '0' then
          pro_state_next <= S_i16_fun;
        else
          pro_state_next <= S_i5_for_loop_incr;
        end if;
      when S_i16_fun => -- FUN22215[p.cp:37]
        if not((PORT_pt1_GD) = ('0')) then
          pro_state_next <= S_i16_fun;
        else
          pro_state_next <= S_i16_while_loop;
        end if;
      when S_i5_for_loop_incr => -- COUNT_LOOP2877[p.cp:26]
        pro_state_next <= S_i5_for_loop_cond;
      when S_p1_end => -- PROCESS0[:0]
        pro_state_next <= S_p1_end;
        PRO_p1_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          d1,
          d2,
          pro_state
          )
  begin
    -- Default values
    PORT_pt1_DE <= '0';
    PORT_pt1_DR <= "00000000";
    PORT_pt2_DE <= '0';
    PORT_pt2_DR <= "00000000";
    PORT_pt1_RE <= '0';
    PORT_pt2_RE <= '0';
    PORT_pt1_WE <= '0';
    PORT_pt1_WR <= "00000000";
    PORT_pt2_WE <= '0';
    PORT_pt2_WR <= "00000000";
    case pro_state is
      when S_p1_start => -- PROCESS0[:0]
        null;
      when S_i1_fun => -- FUN73170[p.cp:21]
        PORT_pt1_DE <= PORT_pt1_GD;
        PORT_pt1_DR <= "00000000";
      when S_i2_fun => -- FUN83041[p.cp:22]
        PORT_pt2_DE <= PORT_pt2_GD;
        PORT_pt2_DR <= "11111111";
      when S_i3_fun => -- FUN83331[p.cp:23]
        PORT_pt1_RE <= PORT_pt1_GD;
      when S_i4_fun => -- FUN95086[p.cp:24]
        PORT_pt2_RE <= PORT_pt2_GD;
      when S_i5_for_loop => -- COUNT_LOOP2877[p.cp:26]
        null;
      when S_i5_for_loop_cond => -- COUNT_LOOP2877[p.cp:26]
        null;
      when S_i6_assign => -- ASSIGN85449[p.cp:28]
        null;
      when S_i7_assign => -- ASSIGN23890[p.cp:29]
        null;
      when S_i8_assign => -- ASSIGN7669[p.cp:30]
        null;
      when S_i9_fun => -- FUN19215[p.cp:31]
        PORT_pt1_WE <= PORT_pt1_GD;
        PORT_pt1_WR <= d1;
      when S_i10_bind_to_11 => -- FUN63052[p.cp:32]
        PORT_pt2_WE <= PORT_pt2_GD;
        PORT_pt2_WR <= d2;
        PORT_pt1_DE <= PORT_pt1_GD;
        PORT_pt1_DR <= "11111000";
      when S_i12_assign => -- ASSIGN71138[p.cp:33]
        null;
      when S_i13_bind_to_14 => -- FUN19442[p.cp:34]
        PORT_pt2_WE <= PORT_pt2_GD;
        PORT_pt2_WR <= d2;
        PORT_pt1_DE <= PORT_pt1_GD;
        PORT_pt1_DR <= "00000000";
      when S_i15_fun => -- FUN78627[p.cp:35]
        PORT_pt1_RE <= PORT_pt1_GD;
      when S_i16_while_loop => -- COND_LOOP48484[p.cp:36]
        null;
      when S_i16_fun => -- FUN22215[p.cp:37]
        PORT_pt1_RE <= PORT_pt1_GD;
      when S_i5_for_loop_incr => -- COUNT_LOOP2877[p.cp:26]
        null;
      when S_p1_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          PORT_pt1_RD,
          PORT_pt2_RD,
          d1,
          LOOP_i_0,
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        d1 <= "00000000";
        d2 <= "00000000";
        LOOP_i_0 <= conv_signed(0,5);
      else
        case pro_state is
          when S_p1_start => -- PROCESS0[:0]
            null;
          when S_i1_fun => -- FUN73170[p.cp:21]
            null;
          when S_i2_fun => -- FUN83041[p.cp:22]
            null;
          when S_i3_fun => -- FUN83331[p.cp:23]
            d1 <= PORT_pt1_RD;
          when S_i4_fun => -- FUN95086[p.cp:24]
            d2 <= PORT_pt2_RD;
          when S_i5_for_loop => -- COUNT_LOOP2877[p.cp:26]
            LOOP_i_0 <= CONST_I5_1;
          when S_i5_for_loop_cond => -- COUNT_LOOP2877[p.cp:26]
            null;
          when S_i6_assign => -- ASSIGN85449[p.cp:28]
            d1 <= d1 and "00001111";
          when S_i7_assign => -- ASSIGN23890[p.cp:29]
            d1 <= ( Im_to_Ln((LOOP_i_0),5,4) ) & d1(3 downto 0);
          when S_i8_assign => -- ASSIGN7669[p.cp:30]
            d2 <= d2(7 downto 4) & ( '1' ) & d2(2 downto 0);
          when S_i9_fun => -- FUN19215[p.cp:31]
            null;
          when S_i10_bind_to_11 => -- FUN63052[p.cp:32]
            null;
          when S_i12_assign => -- ASSIGN71138[p.cp:33]
            d2 <= d2(7 downto 4) & ( '0' ) & d2(2 downto 0);
          when S_i13_bind_to_14 => -- FUN19442[p.cp:34]
            null;
          when S_i15_fun => -- FUN78627[p.cp:35]
            d1 <= PORT_pt1_RD;
          when S_i16_while_loop => -- COND_LOOP48484[p.cp:36]
            null;
          when S_i16_fun => -- FUN22215[p.cp:37]
            d1 <= PORT_pt1_RD;
          when S_i5_for_loop_incr => -- COUNT_LOOP2877[p.cp:26]
            LOOP_i_0 <= LOOP_i_0 + CONST_I5_1;
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