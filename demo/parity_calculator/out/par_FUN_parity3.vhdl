--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D100 Genetic size: 2261933
--         Compile date: Thu Aug 13 10:06:37 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <FUN_parity3> from module <Par>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity par_FUN_parity3 is
port(
  -- Connections to external objects, components and the outside world
  signal REG_RET_FUN_parity3_p_WR: out std_logic;
  signal REG_RET_FUN_parity3_p_WE: out std_logic;
  signal REG_ARG_FUN_parity3_x_RD: in std_logic_vector(63 downto 0);
  signal PRO_FUN_parity3_ENABLE: in std_logic;
  signal PRO_FUN_parity3_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end par_FUN_parity3;
architecture main of par_FUN_parity3 is
  -- Local and temporary data objects
  signal pl: std_logic;
  signal xl: std_logic_vector(63 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_FUN_parity3_start, -- PROCESS0[:0]
    S_i1_assign, -- ASSIGN83560[par.cp:43]
    S_i2_assign, -- ASSIGN48484[par.cp:44]
    S_i3_assign, -- ASSIGN2398[par.cp:47]
    S_i4_assign, -- ASSIGN45421[par.cp:47]
    S_i5_assign, -- ASSIGN75600[par.cp:47]
    S_i6_assign, -- ASSIGN11089[par.cp:47]
    S_i7_assign, -- ASSIGN86320[par.cp:47]
    S_i8_assign, -- ASSIGN33708[par.cp:47]
    S_i9_assign, -- ASSIGN28093[par.cp:47]
    S_i10_assign, -- ASSIGN81698[par.cp:47]
    S_i11_assign, -- ASSIGN21229[par.cp:47]
    S_i12_assign, -- ASSIGN49386[par.cp:47]
    S_i13_assign, -- ASSIGN45725[par.cp:47]
    S_i14_assign, -- ASSIGN66809[par.cp:47]
    S_i15_assign, -- ASSIGN13048[par.cp:47]
    S_i16_assign, -- ASSIGN7507[par.cp:47]
    S_i17_assign, -- ASSIGN43691[par.cp:47]
    S_i18_assign, -- ASSIGN70779[par.cp:47]
    S_i19_assign, -- ASSIGN77351[par.cp:47]
    S_i20_assign, -- ASSIGN33362[par.cp:47]
    S_i21_assign, -- ASSIGN61683[par.cp:47]
    S_i22_assign, -- ASSIGN55762[par.cp:47]
    S_i23_assign, -- ASSIGN23606[par.cp:47]
    S_i24_assign, -- ASSIGN69501[par.cp:47]
    S_i25_assign, -- ASSIGN58009[par.cp:47]
    S_i26_assign, -- ASSIGN33018[par.cp:47]
    S_i27_assign, -- ASSIGN92729[par.cp:47]
    S_i28_assign, -- ASSIGN67938[par.cp:47]
    S_i29_assign, -- ASSIGN31439[par.cp:47]
    S_i30_assign, -- ASSIGN26142[par.cp:47]
    S_i31_assign, -- ASSIGN19363[par.cp:47]
    S_i32_assign, -- ASSIGN69947[par.cp:47]
    S_i33_assign, -- ASSIGN23575[par.cp:47]
    S_i34_assign, -- ASSIGN41945[par.cp:47]
    S_i35_assign, -- ASSIGN23398[par.cp:47]
    S_i36_assign, -- ASSIGN83561[par.cp:47]
    S_i37_assign, -- ASSIGN56720[par.cp:47]
    S_i38_assign, -- ASSIGN9273[par.cp:47]
    S_i39_assign, -- ASSIGN47275[par.cp:47]
    S_i40_assign, -- ASSIGN42577[par.cp:47]
    S_i41_assign, -- ASSIGN25650[par.cp:47]
    S_i42_assign, -- ASSIGN46547[par.cp:47]
    S_i43_assign, -- ASSIGN14832[par.cp:47]
    S_i44_assign, -- ASSIGN21022[par.cp:47]
    S_i45_assign, -- ASSIGN39979[par.cp:47]
    S_i46_assign, -- ASSIGN54265[par.cp:47]
    S_i47_assign, -- ASSIGN49014[par.cp:47]
    S_i48_assign, -- ASSIGN38777[par.cp:47]
    S_i49_assign, -- ASSIGN89994[par.cp:47]
    S_i50_assign, -- ASSIGN1928[par.cp:47]
    S_i51_assign, -- ASSIGN96414[par.cp:47]
    S_i52_assign, -- ASSIGN45574[par.cp:47]
    S_i53_assign, -- ASSIGN75204[par.cp:47]
    S_i54_assign, -- ASSIGN60409[par.cp:47]
    S_i55_assign, -- ASSIGN91716[par.cp:47]
    S_i56_assign, -- ASSIGN68501[par.cp:47]
    S_i57_assign, -- ASSIGN98283[par.cp:47]
    S_i58_assign, -- ASSIGN95127[par.cp:47]
    S_i59_assign, -- ASSIGN13359[par.cp:47]
    S_i60_assign, -- ASSIGN65215[par.cp:47]
    S_i61_assign, -- ASSIGN95407[par.cp:47]
    S_i62_assign, -- ASSIGN5683[par.cp:47]
    S_i63_assign, -- ASSIGN61831[par.cp:47]
    S_i64_assign, -- ASSIGN51668[par.cp:47]
    S_i65_assign, -- ASSIGN81819[par.cp:47]
    S_i66_assign, -- ASSIGN44627[par.cp:47]
    S_i67_assign, -- ASSIGN91123[par.cp:49]
    S_FUN_parity3_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_FUN_parity3_start;
  signal pro_state_next: pro_states := S_FUN_parity3_start;
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
          PRO_FUN_parity3_ENABLE,
          pro_state_next,
          conpro_system_clk,
          conpro_system_reset
  )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset='1' or PRO_FUN_parity3_ENABLE='0' then
        pro_state <= S_FUN_parity3_start;
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
    PRO_FUN_parity3_END <= '0';
    case pro_state is
      when S_FUN_parity3_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_assign;
      when S_i1_assign => -- ASSIGN83560[par.cp:43]
        pro_state_next <= S_i2_assign;
      when S_i2_assign => -- ASSIGN48484[par.cp:44]
        pro_state_next <= S_i3_assign;
      when S_i3_assign => -- ASSIGN2398[par.cp:47]
        pro_state_next <= S_i4_assign;
      when S_i4_assign => -- ASSIGN45421[par.cp:47]
        pro_state_next <= S_i5_assign;
      when S_i5_assign => -- ASSIGN75600[par.cp:47]
        pro_state_next <= S_i6_assign;
      when S_i6_assign => -- ASSIGN11089[par.cp:47]
        pro_state_next <= S_i7_assign;
      when S_i7_assign => -- ASSIGN86320[par.cp:47]
        pro_state_next <= S_i8_assign;
      when S_i8_assign => -- ASSIGN33708[par.cp:47]
        pro_state_next <= S_i9_assign;
      when S_i9_assign => -- ASSIGN28093[par.cp:47]
        pro_state_next <= S_i10_assign;
      when S_i10_assign => -- ASSIGN81698[par.cp:47]
        pro_state_next <= S_i11_assign;
      when S_i11_assign => -- ASSIGN21229[par.cp:47]
        pro_state_next <= S_i12_assign;
      when S_i12_assign => -- ASSIGN49386[par.cp:47]
        pro_state_next <= S_i13_assign;
      when S_i13_assign => -- ASSIGN45725[par.cp:47]
        pro_state_next <= S_i14_assign;
      when S_i14_assign => -- ASSIGN66809[par.cp:47]
        pro_state_next <= S_i15_assign;
      when S_i15_assign => -- ASSIGN13048[par.cp:47]
        pro_state_next <= S_i16_assign;
      when S_i16_assign => -- ASSIGN7507[par.cp:47]
        pro_state_next <= S_i17_assign;
      when S_i17_assign => -- ASSIGN43691[par.cp:47]
        pro_state_next <= S_i18_assign;
      when S_i18_assign => -- ASSIGN70779[par.cp:47]
        pro_state_next <= S_i19_assign;
      when S_i19_assign => -- ASSIGN77351[par.cp:47]
        pro_state_next <= S_i20_assign;
      when S_i20_assign => -- ASSIGN33362[par.cp:47]
        pro_state_next <= S_i21_assign;
      when S_i21_assign => -- ASSIGN61683[par.cp:47]
        pro_state_next <= S_i22_assign;
      when S_i22_assign => -- ASSIGN55762[par.cp:47]
        pro_state_next <= S_i23_assign;
      when S_i23_assign => -- ASSIGN23606[par.cp:47]
        pro_state_next <= S_i24_assign;
      when S_i24_assign => -- ASSIGN69501[par.cp:47]
        pro_state_next <= S_i25_assign;
      when S_i25_assign => -- ASSIGN58009[par.cp:47]
        pro_state_next <= S_i26_assign;
      when S_i26_assign => -- ASSIGN33018[par.cp:47]
        pro_state_next <= S_i27_assign;
      when S_i27_assign => -- ASSIGN92729[par.cp:47]
        pro_state_next <= S_i28_assign;
      when S_i28_assign => -- ASSIGN67938[par.cp:47]
        pro_state_next <= S_i29_assign;
      when S_i29_assign => -- ASSIGN31439[par.cp:47]
        pro_state_next <= S_i30_assign;
      when S_i30_assign => -- ASSIGN26142[par.cp:47]
        pro_state_next <= S_i31_assign;
      when S_i31_assign => -- ASSIGN19363[par.cp:47]
        pro_state_next <= S_i32_assign;
      when S_i32_assign => -- ASSIGN69947[par.cp:47]
        pro_state_next <= S_i33_assign;
      when S_i33_assign => -- ASSIGN23575[par.cp:47]
        pro_state_next <= S_i34_assign;
      when S_i34_assign => -- ASSIGN41945[par.cp:47]
        pro_state_next <= S_i35_assign;
      when S_i35_assign => -- ASSIGN23398[par.cp:47]
        pro_state_next <= S_i36_assign;
      when S_i36_assign => -- ASSIGN83561[par.cp:47]
        pro_state_next <= S_i37_assign;
      when S_i37_assign => -- ASSIGN56720[par.cp:47]
        pro_state_next <= S_i38_assign;
      when S_i38_assign => -- ASSIGN9273[par.cp:47]
        pro_state_next <= S_i39_assign;
      when S_i39_assign => -- ASSIGN47275[par.cp:47]
        pro_state_next <= S_i40_assign;
      when S_i40_assign => -- ASSIGN42577[par.cp:47]
        pro_state_next <= S_i41_assign;
      when S_i41_assign => -- ASSIGN25650[par.cp:47]
        pro_state_next <= S_i42_assign;
      when S_i42_assign => -- ASSIGN46547[par.cp:47]
        pro_state_next <= S_i43_assign;
      when S_i43_assign => -- ASSIGN14832[par.cp:47]
        pro_state_next <= S_i44_assign;
      when S_i44_assign => -- ASSIGN21022[par.cp:47]
        pro_state_next <= S_i45_assign;
      when S_i45_assign => -- ASSIGN39979[par.cp:47]
        pro_state_next <= S_i46_assign;
      when S_i46_assign => -- ASSIGN54265[par.cp:47]
        pro_state_next <= S_i47_assign;
      when S_i47_assign => -- ASSIGN49014[par.cp:47]
        pro_state_next <= S_i48_assign;
      when S_i48_assign => -- ASSIGN38777[par.cp:47]
        pro_state_next <= S_i49_assign;
      when S_i49_assign => -- ASSIGN89994[par.cp:47]
        pro_state_next <= S_i50_assign;
      when S_i50_assign => -- ASSIGN1928[par.cp:47]
        pro_state_next <= S_i51_assign;
      when S_i51_assign => -- ASSIGN96414[par.cp:47]
        pro_state_next <= S_i52_assign;
      when S_i52_assign => -- ASSIGN45574[par.cp:47]
        pro_state_next <= S_i53_assign;
      when S_i53_assign => -- ASSIGN75204[par.cp:47]
        pro_state_next <= S_i54_assign;
      when S_i54_assign => -- ASSIGN60409[par.cp:47]
        pro_state_next <= S_i55_assign;
      when S_i55_assign => -- ASSIGN91716[par.cp:47]
        pro_state_next <= S_i56_assign;
      when S_i56_assign => -- ASSIGN68501[par.cp:47]
        pro_state_next <= S_i57_assign;
      when S_i57_assign => -- ASSIGN98283[par.cp:47]
        pro_state_next <= S_i58_assign;
      when S_i58_assign => -- ASSIGN95127[par.cp:47]
        pro_state_next <= S_i59_assign;
      when S_i59_assign => -- ASSIGN13359[par.cp:47]
        pro_state_next <= S_i60_assign;
      when S_i60_assign => -- ASSIGN65215[par.cp:47]
        pro_state_next <= S_i61_assign;
      when S_i61_assign => -- ASSIGN95407[par.cp:47]
        pro_state_next <= S_i62_assign;
      when S_i62_assign => -- ASSIGN5683[par.cp:47]
        pro_state_next <= S_i63_assign;
      when S_i63_assign => -- ASSIGN61831[par.cp:47]
        pro_state_next <= S_i64_assign;
      when S_i64_assign => -- ASSIGN51668[par.cp:47]
        pro_state_next <= S_i65_assign;
      when S_i65_assign => -- ASSIGN81819[par.cp:47]
        pro_state_next <= S_i66_assign;
      when S_i66_assign => -- ASSIGN44627[par.cp:47]
        pro_state_next <= S_i67_assign;
      when S_i67_assign => -- ASSIGN91123[par.cp:49]
        pro_state_next <= S_FUN_parity3_end;
      when S_FUN_parity3_end => -- PROCESS0[:0]
        pro_state_next <= S_FUN_parity3_end;
        PRO_FUN_parity3_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          pl,
          pro_state
          )
  begin
    -- Default values
    REG_RET_FUN_parity3_p_WR <= '0';
    REG_RET_FUN_parity3_p_WE <= '0';
    case pro_state is
      when S_FUN_parity3_start => -- PROCESS0[:0]
        null;
      when S_i1_assign => -- ASSIGN83560[par.cp:43]
        null;
      when S_i2_assign => -- ASSIGN48484[par.cp:44]
        null;
      when S_i3_assign => -- ASSIGN2398[par.cp:47]
        null;
      when S_i4_assign => -- ASSIGN45421[par.cp:47]
        null;
      when S_i5_assign => -- ASSIGN75600[par.cp:47]
        null;
      when S_i6_assign => -- ASSIGN11089[par.cp:47]
        null;
      when S_i7_assign => -- ASSIGN86320[par.cp:47]
        null;
      when S_i8_assign => -- ASSIGN33708[par.cp:47]
        null;
      when S_i9_assign => -- ASSIGN28093[par.cp:47]
        null;
      when S_i10_assign => -- ASSIGN81698[par.cp:47]
        null;
      when S_i11_assign => -- ASSIGN21229[par.cp:47]
        null;
      when S_i12_assign => -- ASSIGN49386[par.cp:47]
        null;
      when S_i13_assign => -- ASSIGN45725[par.cp:47]
        null;
      when S_i14_assign => -- ASSIGN66809[par.cp:47]
        null;
      when S_i15_assign => -- ASSIGN13048[par.cp:47]
        null;
      when S_i16_assign => -- ASSIGN7507[par.cp:47]
        null;
      when S_i17_assign => -- ASSIGN43691[par.cp:47]
        null;
      when S_i18_assign => -- ASSIGN70779[par.cp:47]
        null;
      when S_i19_assign => -- ASSIGN77351[par.cp:47]
        null;
      when S_i20_assign => -- ASSIGN33362[par.cp:47]
        null;
      when S_i21_assign => -- ASSIGN61683[par.cp:47]
        null;
      when S_i22_assign => -- ASSIGN55762[par.cp:47]
        null;
      when S_i23_assign => -- ASSIGN23606[par.cp:47]
        null;
      when S_i24_assign => -- ASSIGN69501[par.cp:47]
        null;
      when S_i25_assign => -- ASSIGN58009[par.cp:47]
        null;
      when S_i26_assign => -- ASSIGN33018[par.cp:47]
        null;
      when S_i27_assign => -- ASSIGN92729[par.cp:47]
        null;
      when S_i28_assign => -- ASSIGN67938[par.cp:47]
        null;
      when S_i29_assign => -- ASSIGN31439[par.cp:47]
        null;
      when S_i30_assign => -- ASSIGN26142[par.cp:47]
        null;
      when S_i31_assign => -- ASSIGN19363[par.cp:47]
        null;
      when S_i32_assign => -- ASSIGN69947[par.cp:47]
        null;
      when S_i33_assign => -- ASSIGN23575[par.cp:47]
        null;
      when S_i34_assign => -- ASSIGN41945[par.cp:47]
        null;
      when S_i35_assign => -- ASSIGN23398[par.cp:47]
        null;
      when S_i36_assign => -- ASSIGN83561[par.cp:47]
        null;
      when S_i37_assign => -- ASSIGN56720[par.cp:47]
        null;
      when S_i38_assign => -- ASSIGN9273[par.cp:47]
        null;
      when S_i39_assign => -- ASSIGN47275[par.cp:47]
        null;
      when S_i40_assign => -- ASSIGN42577[par.cp:47]
        null;
      when S_i41_assign => -- ASSIGN25650[par.cp:47]
        null;
      when S_i42_assign => -- ASSIGN46547[par.cp:47]
        null;
      when S_i43_assign => -- ASSIGN14832[par.cp:47]
        null;
      when S_i44_assign => -- ASSIGN21022[par.cp:47]
        null;
      when S_i45_assign => -- ASSIGN39979[par.cp:47]
        null;
      when S_i46_assign => -- ASSIGN54265[par.cp:47]
        null;
      when S_i47_assign => -- ASSIGN49014[par.cp:47]
        null;
      when S_i48_assign => -- ASSIGN38777[par.cp:47]
        null;
      when S_i49_assign => -- ASSIGN89994[par.cp:47]
        null;
      when S_i50_assign => -- ASSIGN1928[par.cp:47]
        null;
      when S_i51_assign => -- ASSIGN96414[par.cp:47]
        null;
      when S_i52_assign => -- ASSIGN45574[par.cp:47]
        null;
      when S_i53_assign => -- ASSIGN75204[par.cp:47]
        null;
      when S_i54_assign => -- ASSIGN60409[par.cp:47]
        null;
      when S_i55_assign => -- ASSIGN91716[par.cp:47]
        null;
      when S_i56_assign => -- ASSIGN68501[par.cp:47]
        null;
      when S_i57_assign => -- ASSIGN98283[par.cp:47]
        null;
      when S_i58_assign => -- ASSIGN95127[par.cp:47]
        null;
      when S_i59_assign => -- ASSIGN13359[par.cp:47]
        null;
      when S_i60_assign => -- ASSIGN65215[par.cp:47]
        null;
      when S_i61_assign => -- ASSIGN95407[par.cp:47]
        null;
      when S_i62_assign => -- ASSIGN5683[par.cp:47]
        null;
      when S_i63_assign => -- ASSIGN61831[par.cp:47]
        null;
      when S_i64_assign => -- ASSIGN51668[par.cp:47]
        null;
      when S_i65_assign => -- ASSIGN81819[par.cp:47]
        null;
      when S_i66_assign => -- ASSIGN44627[par.cp:47]
        null;
      when S_i67_assign => -- ASSIGN91123[par.cp:49]
        REG_RET_FUN_parity3_p_WR <= pl;
        REG_RET_FUN_parity3_p_WE <= '1';
      when S_FUN_parity3_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          REG_ARG_FUN_parity3_x_RD,
          pl,
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
          when S_FUN_parity3_start => -- PROCESS0[:0]
            null;
          when S_i1_assign => -- ASSIGN83560[par.cp:43]
            xl <= REG_ARG_FUN_parity3_x_RD;
          when S_i2_assign => -- ASSIGN48484[par.cp:44]
            pl <= '0';
          when S_i3_assign => -- ASSIGN2398[par.cp:47]
            pl <= pl xor xl(0);
          when S_i4_assign => -- ASSIGN45421[par.cp:47]
            pl <= pl xor xl(1);
          when S_i5_assign => -- ASSIGN75600[par.cp:47]
            pl <= pl xor xl(2);
          when S_i6_assign => -- ASSIGN11089[par.cp:47]
            pl <= pl xor xl(3);
          when S_i7_assign => -- ASSIGN86320[par.cp:47]
            pl <= pl xor xl(4);
          when S_i8_assign => -- ASSIGN33708[par.cp:47]
            pl <= pl xor xl(5);
          when S_i9_assign => -- ASSIGN28093[par.cp:47]
            pl <= pl xor xl(6);
          when S_i10_assign => -- ASSIGN81698[par.cp:47]
            pl <= pl xor xl(7);
          when S_i11_assign => -- ASSIGN21229[par.cp:47]
            pl <= pl xor xl(8);
          when S_i12_assign => -- ASSIGN49386[par.cp:47]
            pl <= pl xor xl(9);
          when S_i13_assign => -- ASSIGN45725[par.cp:47]
            pl <= pl xor xl(10);
          when S_i14_assign => -- ASSIGN66809[par.cp:47]
            pl <= pl xor xl(11);
          when S_i15_assign => -- ASSIGN13048[par.cp:47]
            pl <= pl xor xl(12);
          when S_i16_assign => -- ASSIGN7507[par.cp:47]
            pl <= pl xor xl(13);
          when S_i17_assign => -- ASSIGN43691[par.cp:47]
            pl <= pl xor xl(14);
          when S_i18_assign => -- ASSIGN70779[par.cp:47]
            pl <= pl xor xl(15);
          when S_i19_assign => -- ASSIGN77351[par.cp:47]
            pl <= pl xor xl(16);
          when S_i20_assign => -- ASSIGN33362[par.cp:47]
            pl <= pl xor xl(17);
          when S_i21_assign => -- ASSIGN61683[par.cp:47]
            pl <= pl xor xl(18);
          when S_i22_assign => -- ASSIGN55762[par.cp:47]
            pl <= pl xor xl(19);
          when S_i23_assign => -- ASSIGN23606[par.cp:47]
            pl <= pl xor xl(20);
          when S_i24_assign => -- ASSIGN69501[par.cp:47]
            pl <= pl xor xl(21);
          when S_i25_assign => -- ASSIGN58009[par.cp:47]
            pl <= pl xor xl(22);
          when S_i26_assign => -- ASSIGN33018[par.cp:47]
            pl <= pl xor xl(23);
          when S_i27_assign => -- ASSIGN92729[par.cp:47]
            pl <= pl xor xl(24);
          when S_i28_assign => -- ASSIGN67938[par.cp:47]
            pl <= pl xor xl(25);
          when S_i29_assign => -- ASSIGN31439[par.cp:47]
            pl <= pl xor xl(26);
          when S_i30_assign => -- ASSIGN26142[par.cp:47]
            pl <= pl xor xl(27);
          when S_i31_assign => -- ASSIGN19363[par.cp:47]
            pl <= pl xor xl(28);
          when S_i32_assign => -- ASSIGN69947[par.cp:47]
            pl <= pl xor xl(29);
          when S_i33_assign => -- ASSIGN23575[par.cp:47]
            pl <= pl xor xl(30);
          when S_i34_assign => -- ASSIGN41945[par.cp:47]
            pl <= pl xor xl(31);
          when S_i35_assign => -- ASSIGN23398[par.cp:47]
            pl <= pl xor xl(32);
          when S_i36_assign => -- ASSIGN83561[par.cp:47]
            pl <= pl xor xl(33);
          when S_i37_assign => -- ASSIGN56720[par.cp:47]
            pl <= pl xor xl(34);
          when S_i38_assign => -- ASSIGN9273[par.cp:47]
            pl <= pl xor xl(35);
          when S_i39_assign => -- ASSIGN47275[par.cp:47]
            pl <= pl xor xl(36);
          when S_i40_assign => -- ASSIGN42577[par.cp:47]
            pl <= pl xor xl(37);
          when S_i41_assign => -- ASSIGN25650[par.cp:47]
            pl <= pl xor xl(38);
          when S_i42_assign => -- ASSIGN46547[par.cp:47]
            pl <= pl xor xl(39);
          when S_i43_assign => -- ASSIGN14832[par.cp:47]
            pl <= pl xor xl(40);
          when S_i44_assign => -- ASSIGN21022[par.cp:47]
            pl <= pl xor xl(41);
          when S_i45_assign => -- ASSIGN39979[par.cp:47]
            pl <= pl xor xl(42);
          when S_i46_assign => -- ASSIGN54265[par.cp:47]
            pl <= pl xor xl(43);
          when S_i47_assign => -- ASSIGN49014[par.cp:47]
            pl <= pl xor xl(44);
          when S_i48_assign => -- ASSIGN38777[par.cp:47]
            pl <= pl xor xl(45);
          when S_i49_assign => -- ASSIGN89994[par.cp:47]
            pl <= pl xor xl(46);
          when S_i50_assign => -- ASSIGN1928[par.cp:47]
            pl <= pl xor xl(47);
          when S_i51_assign => -- ASSIGN96414[par.cp:47]
            pl <= pl xor xl(48);
          when S_i52_assign => -- ASSIGN45574[par.cp:47]
            pl <= pl xor xl(49);
          when S_i53_assign => -- ASSIGN75204[par.cp:47]
            pl <= pl xor xl(50);
          when S_i54_assign => -- ASSIGN60409[par.cp:47]
            pl <= pl xor xl(51);
          when S_i55_assign => -- ASSIGN91716[par.cp:47]
            pl <= pl xor xl(52);
          when S_i56_assign => -- ASSIGN68501[par.cp:47]
            pl <= pl xor xl(53);
          when S_i57_assign => -- ASSIGN98283[par.cp:47]
            pl <= pl xor xl(54);
          when S_i58_assign => -- ASSIGN95127[par.cp:47]
            pl <= pl xor xl(55);
          when S_i59_assign => -- ASSIGN13359[par.cp:47]
            pl <= pl xor xl(56);
          when S_i60_assign => -- ASSIGN65215[par.cp:47]
            pl <= pl xor xl(57);
          when S_i61_assign => -- ASSIGN95407[par.cp:47]
            pl <= pl xor xl(58);
          when S_i62_assign => -- ASSIGN5683[par.cp:47]
            pl <= pl xor xl(59);
          when S_i63_assign => -- ASSIGN61831[par.cp:47]
            pl <= pl xor xl(60);
          when S_i64_assign => -- ASSIGN51668[par.cp:47]
            pl <= pl xor xl(61);
          when S_i65_assign => -- ASSIGN81819[par.cp:47]
            pl <= pl xor xl(62);
          when S_i66_assign => -- ASSIGN44627[par.cp:47]
            pl <= pl xor xl(63);
          when S_i67_assign => -- ASSIGN91123[par.cp:49]
            null;
          when S_FUN_parity3_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
