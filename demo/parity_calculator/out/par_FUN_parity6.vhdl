--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D100 Genetic size: 2261933
--         Compile date: Thu Aug 13 10:06:37 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <FUN_parity6> from module <Par>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity par_FUN_parity6 is
port(
  -- Connections to external objects, components and the outside world
  signal REG_ARG_FUN_parity6_x_RD: in std_logic_vector(63 downto 0);
  signal REG_RET_FUN_parity6_p_WR: out std_logic;
  signal REG_RET_FUN_parity6_p_WE: out std_logic;
  signal PRO_FUN_parity6_ENABLE: in std_logic;
  signal PRO_FUN_parity6_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end par_FUN_parity6;
architecture main of par_FUN_parity6 is
  -- Local and temporary data objects
  signal pl: std_logic;
  signal xl: std_logic_vector(63 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_FUN_parity6_start, -- PROCESS0[:0]
    S_BLOCKBOUND1_1, -- ASSIGN_BLOCK117165[par.cp:85]
    S_BLOCKBOUND1_2, -- ASSIGN_BLOCK284665[par.cp:89]
    S_FUN_parity6_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_FUN_parity6_start;
  signal pro_state_next: pro_states := S_FUN_parity6_start;
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
          PRO_FUN_parity6_ENABLE,
          pro_state_next,
          conpro_system_clk,
          conpro_system_reset
  )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset='1' or PRO_FUN_parity6_ENABLE='0' then
        pro_state <= S_FUN_parity6_start;
      else
        pro_state <= pro_state_next;
      end if;
    end if;
  end process state_transition;
  -- Process implementation
  -- Instruction Controlpath Unit - The Leitwerk
  control_path: process(
          pro_state
          )
  begin
    PRO_FUN_parity6_END <= '0';
    case pro_state is
      when S_FUN_parity6_start => -- PROCESS0[:0]
        pro_state_next <= S_BLOCKBOUND1_1;
      when S_BLOCKBOUND1_1 => -- ASSIGN_BLOCK117165[par.cp:85]
        pro_state_next <= S_BLOCKBOUND1_2;
      when S_BLOCKBOUND1_2 => -- ASSIGN_BLOCK284665[par.cp:89]
        pro_state_next <= S_FUN_parity6_end;
      when S_FUN_parity6_end => -- PROCESS0[:0]
        pro_state_next <= S_FUN_parity6_end;
        PRO_FUN_parity6_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          xl,
          pro_state
          )
  begin
    -- Default values
    REG_RET_FUN_parity6_p_WR <= '0';
    REG_RET_FUN_parity6_p_WE <= '0';
    case pro_state is
      when S_FUN_parity6_start => -- PROCESS0[:0]
        null;
      when S_BLOCKBOUND1_1 => -- ASSIGN_BLOCK117165[par.cp:85]
        null;
      when S_BLOCKBOUND1_2 => -- ASSIGN_BLOCK284665[par.cp:89]
        REG_RET_FUN_parity6_p_WR <= ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((('0' xor xl(0)) xor xl(1)) xor xl(2)) xor xl(3)) xor xl(4)) xor xl(5)) xor xl(6)) xor xl(7)) xor xl(8)) xor xl(9)) xor xl(10)) xor xl(11)) xor xl(12)) xor xl(13)) xor xl(14)) xor xl(15)) xor xl(16)) xor xl(17)) xor xl(18)) xor xl(19)) xor xl(20)) xor xl(21)) xor xl(22)) xor xl(23)) xor xl(24)) xor xl(25)) xor xl(26)) xor xl(27)) xor xl(28)) xor xl(29)) xor xl(30)) xor xl(31)) xor xl(32)) xor xl(33)) xor xl(34)) xor xl(35)) xor xl(36)) xor xl(37)) xor xl(38)) xor xl(39)) xor xl(40)) xor xl(41)) xor xl(42)) xor xl(43)) xor xl(44)) xor xl(45)) xor xl(46)) xor xl(47)) xor xl(48)) xor xl(49)) xor xl(50)) xor xl(51)) xor xl(52)) xor xl(53)) xor xl(54)) xor xl(55)) xor xl(56)) xor xl(57)) xor xl(58)) xor xl(59)) xor xl(60)) xor xl(61)) xor xl(62)) xor xl(63);
        REG_RET_FUN_parity6_p_WE <= '1';
      when S_FUN_parity6_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          REG_ARG_FUN_parity6_x_RD,
          xl,
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        xl <= "0000000000000000000000000000000000000000000000000000000000000000";
        pl <= '0';
      else
        case pro_state is
          when S_FUN_parity6_start => -- PROCESS0[:0]
            null;
          when S_BLOCKBOUND1_1 => -- ASSIGN_BLOCK117165[par.cp:85]
            xl <= REG_ARG_FUN_parity6_x_RD;
          when S_BLOCKBOUND1_2 => -- ASSIGN_BLOCK284665[par.cp:89]
            pl <= ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((('0' xor xl(0)) xor xl(1)) xor xl(2)) xor xl(3)) xor xl(4)) xor xl(5)) xor xl(6)) xor xl(7)) xor xl(8)) xor xl(9)) xor xl(10)) xor xl(11)) xor xl(12)) xor xl(13)) xor xl(14)) xor xl(15)) xor xl(16)) xor xl(17)) xor xl(18)) xor xl(19)) xor xl(20)) xor xl(21)) xor xl(22)) xor xl(23)) xor xl(24)) xor xl(25)) xor xl(26)) xor xl(27)) xor xl(28)) xor xl(29)) xor xl(30)) xor xl(31)) xor xl(32)) xor xl(33)) xor xl(34)) xor xl(35)) xor xl(36)) xor xl(37)) xor xl(38)) xor xl(39)) xor xl(40)) xor xl(41)) xor xl(42)) xor xl(43)) xor xl(44)) xor xl(45)) xor xl(46)) xor xl(47)) xor xl(48)) xor xl(49)) xor xl(50)) xor xl(51)) xor xl(52)) xor xl(53)) xor xl(54)) xor xl(55)) xor xl(56)) xor xl(57)) xor xl(58)) xor xl(59)) xor xl(60)) xor xl(61)) xor xl(62)) xor xl(63);
          when S_FUN_parity6_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
