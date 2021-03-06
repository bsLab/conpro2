--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D100 Genetic size: 2261933
--         Compile date: Thu Aug 13 10:06:37 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <main> from module <Par>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity par_main is
port(
  -- Connections to external objects, components and the outside world
  signal REG_ARG_FUN_parity5_x_WR: out std_logic_vector(63 downto 0);
  signal REG_ARG_FUN_parity5_x_WE: out std_logic;
  signal REG_d_RD: in std_logic_vector(63 downto 0);
  signal REG_d_WR: out std_logic_vector(63 downto 0);
  signal REG_d_WE: out std_logic;
  signal REG_RET_FUN_parity3_p_RD: in std_logic;
  signal REG_p_WR: out std_logic;
  signal REG_p_WE: out std_logic;
  signal REG_ARG_FUN_parity2_x_WR: out std_logic_vector(63 downto 0);
  signal REG_ARG_FUN_parity2_x_WE: out std_logic;
  signal REG_RET_FUN_parity5_p_RD: in std_logic;
  signal REG_ARG_FUN_parity4_x_WR: out std_logic_vector(63 downto 0);
  signal REG_ARG_FUN_parity4_x_WE: out std_logic;
  signal REG_RET_FUN_parity2_p_RD: in std_logic;
  signal REG_ARG_FUN_parity1_x_WR: out std_logic_vector(63 downto 0);
  signal REG_ARG_FUN_parity1_x_WE: out std_logic;
  signal REG_ARG_FUN_parity6_x_WR: out std_logic_vector(63 downto 0);
  signal REG_ARG_FUN_parity6_x_WE: out std_logic;
  signal REG_RET_FUN_parity4_p_RD: in std_logic;
  signal REG_ARG_FUN_parity3_x_WR: out std_logic_vector(63 downto 0);
  signal REG_ARG_FUN_parity3_x_WE: out std_logic;
  signal REG_RET_FUN_parity1_p_RD: in std_logic;
  signal MUTEX_LOCK_FUN_parity1_INIT: out std_logic;
  signal MUTEX_LOCK_FUN_parity1_LOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity1_UNLOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity1_GD: in std_logic;
  signal MUTEX_LOCK_FUN_parity2_INIT: out std_logic;
  signal MUTEX_LOCK_FUN_parity2_LOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity2_UNLOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity2_GD: in std_logic;
  signal MUTEX_LOCK_FUN_parity3_INIT: out std_logic;
  signal MUTEX_LOCK_FUN_parity3_LOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity3_UNLOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity3_GD: in std_logic;
  signal MUTEX_LOCK_FUN_parity4_INIT: out std_logic;
  signal MUTEX_LOCK_FUN_parity4_LOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity4_UNLOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity4_GD: in std_logic;
  signal REG_RET_FUN_parity6_p_RD: in std_logic;
  signal PRO_FUN_parity1_CALL: out std_logic;
  signal PRO_FUN_parity1_GD: in std_logic;
  signal MUTEX_LOCK_FUN_parity5_INIT: out std_logic;
  signal MUTEX_LOCK_FUN_parity5_LOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity5_UNLOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity5_GD: in std_logic;
  signal PRO_FUN_parity2_CALL: out std_logic;
  signal PRO_FUN_parity2_GD: in std_logic;
  signal MUTEX_LOCK_FUN_parity6_INIT: out std_logic;
  signal MUTEX_LOCK_FUN_parity6_LOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity6_UNLOCK: out std_logic;
  signal MUTEX_LOCK_FUN_parity6_GD: in std_logic;
  signal PRO_FUN_parity3_CALL: out std_logic;
  signal PRO_FUN_parity3_GD: in std_logic;
  signal PRO_FUN_parity4_CALL: out std_logic;
  signal PRO_FUN_parity4_GD: in std_logic;
  signal PRO_FUN_parity5_CALL: out std_logic;
  signal PRO_FUN_parity5_GD: in std_logic;
  signal PRO_FUN_parity6_CALL: out std_logic;
  signal PRO_FUN_parity6_GD: in std_logic;
  signal PRO_main_ENABLE: in std_logic;
  signal PRO_main_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end par_main;
architecture main of par_main is
  -- Local and temporary data objects
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_main_start, -- PROCESS0[:0]
    S_i1_fun, -- FUN70032[:0]
    S_i2_fun, -- FUN33255[:0]
    S_i3_fun, -- FUN70514[:0]
    S_i4_fun, -- FUN11013[:0]
    S_i5_fun, -- FUN98554[:0]
    S_i6_fun, -- FUN12980[:0]
    S_i7_assign, -- ASSIGN58548[par.cp:96]
    S_i8_fun, -- FUN87043[par.cp:97]
    S_i9_assign, -- ASSIGN49948[par.cp:97]
    S_i10_fun, -- FUN67070[par.cp:97]
    S_i11_assign, -- ASSIGN93859[par.cp:97]
    S_i12_fun, -- FUN57960[par.cp:97]
    S_i13_fun, -- FUN22985[par.cp:98]
    S_i14_assign, -- ASSIGN98146[par.cp:98]
    S_i15_fun, -- FUN17116[par.cp:98]
    S_i16_assign, -- ASSIGN32752[par.cp:98]
    S_i17_fun, -- FUN80820[par.cp:98]
    S_i18_fun, -- FUN18440[par.cp:99]
    S_i19_assign, -- ASSIGN20818[par.cp:99]
    S_i20_fun, -- FUN17407[par.cp:99]
    S_i21_assign, -- ASSIGN62772[par.cp:99]
    S_i22_fun, -- FUN72635[par.cp:99]
    S_i23_fun, -- FUN34114[par.cp:100]
    S_i24_assign, -- ASSIGN95395[par.cp:100]
    S_i25_fun, -- FUN3002[par.cp:100]
    S_i26_assign, -- ASSIGN17812[par.cp:100]
    S_i27_fun, -- FUN38990[par.cp:100]
    S_i28_fun, -- FUN519[par.cp:101]
    S_i29_assign, -- ASSIGN69953[par.cp:101]
    S_i30_fun, -- FUN74204[par.cp:101]
    S_i31_assign, -- ASSIGN13353[par.cp:101]
    S_i32_fun, -- FUN38336[par.cp:101]
    S_i33_fun, -- FUN46088[par.cp:102]
    S_i34_assign, -- ASSIGN85514[par.cp:102]
    S_i35_fun, -- FUN83234[par.cp:102]
    S_i36_assign, -- ASSIGN76980[par.cp:102]
    S_i37_fun, -- FUN92681[par.cp:102]
    S_main_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_main_start;
  signal pro_state_next: pro_states := S_main_start;
  -- Auxilliary toplevel definitions
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
  function L_to_N(L:std_logic) return integer is
    variable N: integer;
    begin
      N := conv_integer(L);
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
          PRO_main_ENABLE,
          pro_state_next,
          conpro_system_clk,
          conpro_system_reset
  )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset='1' or PRO_main_ENABLE='0' then
        pro_state <= S_main_start;
      else
        pro_state <= pro_state_next;
      end if;
    end if;
  end process state_transition;
  -- Process implementation
  -- Instruction Controlpath Unit - The Leitwerk
  control_path: process(
          MUTEX_LOCK_FUN_parity1_GD,
          MUTEX_LOCK_FUN_parity2_GD,
          MUTEX_LOCK_FUN_parity3_GD,
          MUTEX_LOCK_FUN_parity4_GD,
          MUTEX_LOCK_FUN_parity5_GD,
          MUTEX_LOCK_FUN_parity6_GD,
          PRO_FUN_parity1_GD,
          PRO_FUN_parity2_GD,
          PRO_FUN_parity3_GD,
          PRO_FUN_parity4_GD,
          PRO_FUN_parity5_GD,
          PRO_FUN_parity6_GD,
          pro_state
          )
  begin
    PRO_main_END <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_fun;
      when S_i1_fun => -- FUN70032[:0]
        if not((MUTEX_LOCK_FUN_parity1_GD) = ('0')) then
          pro_state_next <= S_i1_fun;
        else
          pro_state_next <= S_i2_fun;
        end if;
      when S_i2_fun => -- FUN33255[:0]
        if not((MUTEX_LOCK_FUN_parity2_GD) = ('0')) then
          pro_state_next <= S_i2_fun;
        else
          pro_state_next <= S_i3_fun;
        end if;
      when S_i3_fun => -- FUN70514[:0]
        if not((MUTEX_LOCK_FUN_parity3_GD) = ('0')) then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_i4_fun;
        end if;
      when S_i4_fun => -- FUN11013[:0]
        if not((MUTEX_LOCK_FUN_parity4_GD) = ('0')) then
          pro_state_next <= S_i4_fun;
        else
          pro_state_next <= S_i5_fun;
        end if;
      when S_i5_fun => -- FUN98554[:0]
        if not((MUTEX_LOCK_FUN_parity5_GD) = ('0')) then
          pro_state_next <= S_i5_fun;
        else
          pro_state_next <= S_i6_fun;
        end if;
      when S_i6_fun => -- FUN12980[:0]
        if not((MUTEX_LOCK_FUN_parity6_GD) = ('0')) then
          pro_state_next <= S_i6_fun;
        else
          pro_state_next <= S_i7_assign;
        end if;
      when S_i7_assign => -- ASSIGN58548[par.cp:96]
        pro_state_next <= S_i8_fun;
      when S_i8_fun => -- FUN87043[par.cp:97]
        if not((MUTEX_LOCK_FUN_parity1_GD) = ('0')) then
          pro_state_next <= S_i8_fun;
        else
          pro_state_next <= S_i9_assign;
        end if;
      when S_i9_assign => -- ASSIGN49948[par.cp:97]
        pro_state_next <= S_i10_fun;
      when S_i10_fun => -- FUN67070[par.cp:97]
        if PRO_FUN_parity1_GD = '1' then
          pro_state_next <= S_i10_fun;
        else
          pro_state_next <= S_i11_assign;
        end if;
      when S_i11_assign => -- ASSIGN93859[par.cp:97]
        pro_state_next <= S_i12_fun;
      when S_i12_fun => -- FUN57960[par.cp:97]
        if not((MUTEX_LOCK_FUN_parity1_GD) = ('0')) then
          pro_state_next <= S_i12_fun;
        else
          pro_state_next <= S_i13_fun;
        end if;
      when S_i13_fun => -- FUN22985[par.cp:98]
        if not((MUTEX_LOCK_FUN_parity2_GD) = ('0')) then
          pro_state_next <= S_i13_fun;
        else
          pro_state_next <= S_i14_assign;
        end if;
      when S_i14_assign => -- ASSIGN98146[par.cp:98]
        pro_state_next <= S_i15_fun;
      when S_i15_fun => -- FUN17116[par.cp:98]
        if PRO_FUN_parity2_GD = '1' then
          pro_state_next <= S_i15_fun;
        else
          pro_state_next <= S_i16_assign;
        end if;
      when S_i16_assign => -- ASSIGN32752[par.cp:98]
        pro_state_next <= S_i17_fun;
      when S_i17_fun => -- FUN80820[par.cp:98]
        if not((MUTEX_LOCK_FUN_parity2_GD) = ('0')) then
          pro_state_next <= S_i17_fun;
        else
          pro_state_next <= S_i18_fun;
        end if;
      when S_i18_fun => -- FUN18440[par.cp:99]
        if not((MUTEX_LOCK_FUN_parity3_GD) = ('0')) then
          pro_state_next <= S_i18_fun;
        else
          pro_state_next <= S_i19_assign;
        end if;
      when S_i19_assign => -- ASSIGN20818[par.cp:99]
        pro_state_next <= S_i20_fun;
      when S_i20_fun => -- FUN17407[par.cp:99]
        if PRO_FUN_parity3_GD = '1' then
          pro_state_next <= S_i20_fun;
        else
          pro_state_next <= S_i21_assign;
        end if;
      when S_i21_assign => -- ASSIGN62772[par.cp:99]
        pro_state_next <= S_i22_fun;
      when S_i22_fun => -- FUN72635[par.cp:99]
        if not((MUTEX_LOCK_FUN_parity3_GD) = ('0')) then
          pro_state_next <= S_i22_fun;
        else
          pro_state_next <= S_i23_fun;
        end if;
      when S_i23_fun => -- FUN34114[par.cp:100]
        if not((MUTEX_LOCK_FUN_parity4_GD) = ('0')) then
          pro_state_next <= S_i23_fun;
        else
          pro_state_next <= S_i24_assign;
        end if;
      when S_i24_assign => -- ASSIGN95395[par.cp:100]
        pro_state_next <= S_i25_fun;
      when S_i25_fun => -- FUN3002[par.cp:100]
        if PRO_FUN_parity4_GD = '1' then
          pro_state_next <= S_i25_fun;
        else
          pro_state_next <= S_i26_assign;
        end if;
      when S_i26_assign => -- ASSIGN17812[par.cp:100]
        pro_state_next <= S_i27_fun;
      when S_i27_fun => -- FUN38990[par.cp:100]
        if not((MUTEX_LOCK_FUN_parity4_GD) = ('0')) then
          pro_state_next <= S_i27_fun;
        else
          pro_state_next <= S_i28_fun;
        end if;
      when S_i28_fun => -- FUN519[par.cp:101]
        if not((MUTEX_LOCK_FUN_parity5_GD) = ('0')) then
          pro_state_next <= S_i28_fun;
        else
          pro_state_next <= S_i29_assign;
        end if;
      when S_i29_assign => -- ASSIGN69953[par.cp:101]
        pro_state_next <= S_i30_fun;
      when S_i30_fun => -- FUN74204[par.cp:101]
        if PRO_FUN_parity5_GD = '1' then
          pro_state_next <= S_i30_fun;
        else
          pro_state_next <= S_i31_assign;
        end if;
      when S_i31_assign => -- ASSIGN13353[par.cp:101]
        pro_state_next <= S_i32_fun;
      when S_i32_fun => -- FUN38336[par.cp:101]
        if not((MUTEX_LOCK_FUN_parity5_GD) = ('0')) then
          pro_state_next <= S_i32_fun;
        else
          pro_state_next <= S_i33_fun;
        end if;
      when S_i33_fun => -- FUN46088[par.cp:102]
        if not((MUTEX_LOCK_FUN_parity6_GD) = ('0')) then
          pro_state_next <= S_i33_fun;
        else
          pro_state_next <= S_i34_assign;
        end if;
      when S_i34_assign => -- ASSIGN85514[par.cp:102]
        pro_state_next <= S_i35_fun;
      when S_i35_fun => -- FUN83234[par.cp:102]
        if PRO_FUN_parity6_GD = '1' then
          pro_state_next <= S_i35_fun;
        else
          pro_state_next <= S_i36_assign;
        end if;
      when S_i36_assign => -- ASSIGN76980[par.cp:102]
        pro_state_next <= S_i37_fun;
      when S_i37_fun => -- FUN92681[par.cp:102]
        if not((MUTEX_LOCK_FUN_parity6_GD) = ('0')) then
          pro_state_next <= S_i37_fun;
        else
          pro_state_next <= S_main_end;
        end if;
      when S_main_end => -- PROCESS0[:0]
        pro_state_next <= S_main_end;
        PRO_main_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          REG_d_RD,
          REG_RET_FUN_parity1_p_RD,
          REG_RET_FUN_parity2_p_RD,
          REG_RET_FUN_parity3_p_RD,
          REG_RET_FUN_parity4_p_RD,
          REG_RET_FUN_parity5_p_RD,
          REG_RET_FUN_parity6_p_RD,
          pro_state
          )
  begin
    -- Default values
    MUTEX_LOCK_FUN_parity1_INIT <= '0';
    MUTEX_LOCK_FUN_parity2_INIT <= '0';
    MUTEX_LOCK_FUN_parity3_INIT <= '0';
    MUTEX_LOCK_FUN_parity4_INIT <= '0';
    MUTEX_LOCK_FUN_parity5_INIT <= '0';
    MUTEX_LOCK_FUN_parity6_INIT <= '0';
    REG_d_WR <= "0000000000000000000000000000000000000000000000000000000000000000";
    REG_d_WE <= '0';
    MUTEX_LOCK_FUN_parity1_LOCK <= '0';
    REG_ARG_FUN_parity1_x_WR <= "0000000000000000000000000000000000000000000000000000000000000000";
    REG_ARG_FUN_parity1_x_WE <= '0';
    PRO_FUN_parity1_CALL <= '0';
    REG_p_WR <= '0';
    REG_p_WE <= '0';
    MUTEX_LOCK_FUN_parity1_UNLOCK <= '0';
    MUTEX_LOCK_FUN_parity2_LOCK <= '0';
    REG_ARG_FUN_parity2_x_WR <= "0000000000000000000000000000000000000000000000000000000000000000";
    REG_ARG_FUN_parity2_x_WE <= '0';
    PRO_FUN_parity2_CALL <= '0';
    MUTEX_LOCK_FUN_parity2_UNLOCK <= '0';
    MUTEX_LOCK_FUN_parity3_LOCK <= '0';
    REG_ARG_FUN_parity3_x_WR <= "0000000000000000000000000000000000000000000000000000000000000000";
    REG_ARG_FUN_parity3_x_WE <= '0';
    PRO_FUN_parity3_CALL <= '0';
    MUTEX_LOCK_FUN_parity3_UNLOCK <= '0';
    MUTEX_LOCK_FUN_parity4_LOCK <= '0';
    REG_ARG_FUN_parity4_x_WR <= "0000000000000000000000000000000000000000000000000000000000000000";
    REG_ARG_FUN_parity4_x_WE <= '0';
    PRO_FUN_parity4_CALL <= '0';
    MUTEX_LOCK_FUN_parity4_UNLOCK <= '0';
    MUTEX_LOCK_FUN_parity5_LOCK <= '0';
    REG_ARG_FUN_parity5_x_WR <= "0000000000000000000000000000000000000000000000000000000000000000";
    REG_ARG_FUN_parity5_x_WE <= '0';
    PRO_FUN_parity5_CALL <= '0';
    MUTEX_LOCK_FUN_parity5_UNLOCK <= '0';
    MUTEX_LOCK_FUN_parity6_LOCK <= '0';
    REG_ARG_FUN_parity6_x_WR <= "0000000000000000000000000000000000000000000000000000000000000000";
    REG_ARG_FUN_parity6_x_WE <= '0';
    PRO_FUN_parity6_CALL <= '0';
    MUTEX_LOCK_FUN_parity6_UNLOCK <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        null;
      when S_i1_fun => -- FUN70032[:0]
        MUTEX_LOCK_FUN_parity1_INIT <= MUTEX_LOCK_FUN_parity1_GD;
      when S_i2_fun => -- FUN33255[:0]
        MUTEX_LOCK_FUN_parity2_INIT <= MUTEX_LOCK_FUN_parity2_GD;
      when S_i3_fun => -- FUN70514[:0]
        MUTEX_LOCK_FUN_parity3_INIT <= MUTEX_LOCK_FUN_parity3_GD;
      when S_i4_fun => -- FUN11013[:0]
        MUTEX_LOCK_FUN_parity4_INIT <= MUTEX_LOCK_FUN_parity4_GD;
      when S_i5_fun => -- FUN98554[:0]
        MUTEX_LOCK_FUN_parity5_INIT <= MUTEX_LOCK_FUN_parity5_GD;
      when S_i6_fun => -- FUN12980[:0]
        MUTEX_LOCK_FUN_parity6_INIT <= MUTEX_LOCK_FUN_parity6_GD;
      when S_i7_assign => -- ASSIGN58548[par.cp:96]
        REG_d_WR <= "0000000000000000000000000000000000010010001101000101011001110000";
        REG_d_WE <= '1';
      when S_i8_fun => -- FUN87043[par.cp:97]
        MUTEX_LOCK_FUN_parity1_LOCK <= MUTEX_LOCK_FUN_parity1_GD;
      when S_i9_assign => -- ASSIGN49948[par.cp:97]
        REG_ARG_FUN_parity1_x_WR <= REG_d_RD;
        REG_ARG_FUN_parity1_x_WE <= '1';
      when S_i10_fun => -- FUN67070[par.cp:97]
        PRO_FUN_parity1_CALL <= '1';
      when S_i11_assign => -- ASSIGN93859[par.cp:97]
        REG_p_WR <= REG_RET_FUN_parity1_p_RD;
        REG_p_WE <= '1';
      when S_i12_fun => -- FUN57960[par.cp:97]
        MUTEX_LOCK_FUN_parity1_UNLOCK <= MUTEX_LOCK_FUN_parity1_GD;
      when S_i13_fun => -- FUN22985[par.cp:98]
        MUTEX_LOCK_FUN_parity2_LOCK <= MUTEX_LOCK_FUN_parity2_GD;
      when S_i14_assign => -- ASSIGN98146[par.cp:98]
        REG_ARG_FUN_parity2_x_WR <= REG_d_RD;
        REG_ARG_FUN_parity2_x_WE <= '1';
      when S_i15_fun => -- FUN17116[par.cp:98]
        PRO_FUN_parity2_CALL <= '1';
      when S_i16_assign => -- ASSIGN32752[par.cp:98]
        REG_p_WR <= REG_RET_FUN_parity2_p_RD;
        REG_p_WE <= '1';
      when S_i17_fun => -- FUN80820[par.cp:98]
        MUTEX_LOCK_FUN_parity2_UNLOCK <= MUTEX_LOCK_FUN_parity2_GD;
      when S_i18_fun => -- FUN18440[par.cp:99]
        MUTEX_LOCK_FUN_parity3_LOCK <= MUTEX_LOCK_FUN_parity3_GD;
      when S_i19_assign => -- ASSIGN20818[par.cp:99]
        REG_ARG_FUN_parity3_x_WR <= REG_d_RD;
        REG_ARG_FUN_parity3_x_WE <= '1';
      when S_i20_fun => -- FUN17407[par.cp:99]
        PRO_FUN_parity3_CALL <= '1';
      when S_i21_assign => -- ASSIGN62772[par.cp:99]
        REG_p_WR <= REG_RET_FUN_parity3_p_RD;
        REG_p_WE <= '1';
      when S_i22_fun => -- FUN72635[par.cp:99]
        MUTEX_LOCK_FUN_parity3_UNLOCK <= MUTEX_LOCK_FUN_parity3_GD;
      when S_i23_fun => -- FUN34114[par.cp:100]
        MUTEX_LOCK_FUN_parity4_LOCK <= MUTEX_LOCK_FUN_parity4_GD;
      when S_i24_assign => -- ASSIGN95395[par.cp:100]
        REG_ARG_FUN_parity4_x_WR <= REG_d_RD;
        REG_ARG_FUN_parity4_x_WE <= '1';
      when S_i25_fun => -- FUN3002[par.cp:100]
        PRO_FUN_parity4_CALL <= '1';
      when S_i26_assign => -- ASSIGN17812[par.cp:100]
        REG_p_WR <= REG_RET_FUN_parity4_p_RD;
        REG_p_WE <= '1';
      when S_i27_fun => -- FUN38990[par.cp:100]
        MUTEX_LOCK_FUN_parity4_UNLOCK <= MUTEX_LOCK_FUN_parity4_GD;
      when S_i28_fun => -- FUN519[par.cp:101]
        MUTEX_LOCK_FUN_parity5_LOCK <= MUTEX_LOCK_FUN_parity5_GD;
      when S_i29_assign => -- ASSIGN69953[par.cp:101]
        REG_ARG_FUN_parity5_x_WR <= REG_d_RD;
        REG_ARG_FUN_parity5_x_WE <= '1';
      when S_i30_fun => -- FUN74204[par.cp:101]
        PRO_FUN_parity5_CALL <= '1';
      when S_i31_assign => -- ASSIGN13353[par.cp:101]
        REG_p_WR <= REG_RET_FUN_parity5_p_RD;
        REG_p_WE <= '1';
      when S_i32_fun => -- FUN38336[par.cp:101]
        MUTEX_LOCK_FUN_parity5_UNLOCK <= MUTEX_LOCK_FUN_parity5_GD;
      when S_i33_fun => -- FUN46088[par.cp:102]
        MUTEX_LOCK_FUN_parity6_LOCK <= MUTEX_LOCK_FUN_parity6_GD;
      when S_i34_assign => -- ASSIGN85514[par.cp:102]
        REG_ARG_FUN_parity6_x_WR <= REG_d_RD;
        REG_ARG_FUN_parity6_x_WE <= '1';
      when S_i35_fun => -- FUN83234[par.cp:102]
        PRO_FUN_parity6_CALL <= '1';
      when S_i36_assign => -- ASSIGN76980[par.cp:102]
        REG_p_WR <= REG_RET_FUN_parity6_p_RD;
        REG_p_WE <= '1';
      when S_i37_fun => -- FUN92681[par.cp:102]
        MUTEX_LOCK_FUN_parity6_UNLOCK <= MUTEX_LOCK_FUN_parity6_GD;
      when S_main_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
