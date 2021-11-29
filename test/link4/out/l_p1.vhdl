--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D174 Genetic size: 2713135
--         Compile date: Fri Jan 21 09:49:21 CET 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <p1> from module <L>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity l_p1 is
port(
  -- Connections to external objects, components and the outside world
  signal REG_PRO_p1_EXCEPTION_RD: in signed(7 downto 0);
  signal REG_PRO_p1_EXCEPTION_WR: out signed(7 downto 0);
  signal REG_PRO_p1_EXCEPTION_WE: out std_logic;
  signal REG_x_WR: out signed(9 downto 0);
  signal REG_x_WE: out std_logic;
  signal LINK_ln_STOP: out std_logic;
  signal LINK_ln_RE: out std_logic;
  signal LINK_ln_RD: in std_logic_vector(9 downto 0);
  signal LINK_ln_RD_ERR: in std_logic;
  signal LINK_ln_GD: in std_logic;
  signal REG_xa_WR: out signed(7 downto 0);
  signal REG_xa_WE: out std_logic;
  signal PRO_p1_ENABLE: in std_logic;
  signal PRO_p1_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end l_p1;
architecture main of l_p1 is
  -- Local and temporary data objects
  signal d: std_logic_vector(9 downto 0);
  signal err: std_logic;
  signal LOOP_i_0: signed(4 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_p1_start, -- PROCESS0[:0]
    S_i5_for_loop, -- COUNT_LOOP39314[l.cp:35]
    S_i5_for_loop_cond, -- COUNT_LOOP39314[l.cp:35]
    S_i6_assign, -- ASSIGN2877[l.cp:37]
    S_i7_fun, -- FUN77977[l.cp:38]
    S_i8_assign, -- ASSIGN23890[l.cp:39]
    S_i9_branch, -- BRANCH7669[l.cp:40]
    S_i10_raise, -- BRANCH_TRUE58851[:0]
    S_i12_assign, -- ASSIGN83560[l.cp:41]
    S_i5_for_loop_incr, -- COUNT_LOOP39314[l.cp:35]
    S_i2_select, -- FUN7961[l.cp:46]
    S_i3_fun, -- FUN7961[l.cp:46]
    S_i4_assign, -- CASE_BODY37171[l.cp:46]
    S_p1_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_p1_start;
  signal pro_state_next: pro_states := S_p1_start;
  -- Auxilliary toplevel definitions
  constant CONST_I5_1: signed(4 downto 0) := "00001";
  constant CONST_I5_10: signed(4 downto 0) := "01010";
  constant CONST_I8_114: signed(7 downto 0) := "01110010";
  constant CONST_I8_46: signed(7 downto 0) := "00101110";
  constant CONST_I8_1: signed(7 downto 0) := "00000001";
  constant CONST_I8_0: signed(7 downto 0) := "00000000";
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
          LOOP_i_0,
          LINK_ln_GD,
          err,
          REG_PRO_p1_EXCEPTION_RD,
          pro_state
          )
  begin
    PRO_p1_END <= '0';
    case pro_state is
      when S_p1_start => -- PROCESS0[:0]
        pro_state_next <= S_i5_for_loop;
      when S_i5_for_loop => -- COUNT_LOOP39314[l.cp:35]
        pro_state_next <= S_i5_for_loop_cond;
      when S_i5_for_loop_cond => -- COUNT_LOOP39314[l.cp:35]
        if CONST_I5_10 >= LOOP_i_0 then
          pro_state_next <= S_i6_assign;
        else
          pro_state_next <= S_p1_end;
        end if;
      when S_i6_assign => -- ASSIGN2877[l.cp:37]
        pro_state_next <= S_i7_fun;
      when S_i7_fun => -- FUN77977[l.cp:38]
        if not((LINK_ln_GD) = ('0')) then
          pro_state_next <= S_i7_fun;
        else
          pro_state_next <= S_i8_assign;
        end if;
      when S_i8_assign => -- ASSIGN23890[l.cp:39]
        pro_state_next <= S_i9_branch;
      when S_i9_branch => -- BRANCH7669[l.cp:40]
        if err = '1' then
          pro_state_next <= S_i10_raise;
        else
          pro_state_next <= S_i12_assign;
        end if;
      when S_i10_raise => -- BRANCH_TRUE58851[:0]
        pro_state_next <= S_i2_select;
      when S_i12_assign => -- ASSIGN83560[l.cp:41]
        pro_state_next <= S_i5_for_loop_incr;
      when S_i5_for_loop_incr => -- COUNT_LOOP39314[l.cp:35]
        pro_state_next <= S_i5_for_loop_cond;
      when S_i2_select => -- FUN7961[l.cp:46]
        case REG_PRO_p1_EXCEPTION_RD  is
          when  CONST_I8_1 => pro_state_next <= S_i3_fun;
          when  others => pro_state_next <= S_p1_end;
        end case;
      when S_i3_fun => -- FUN7961[l.cp:46]
        if not((LINK_ln_GD) = ('0')) then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_i4_assign;
        end if;
      when S_i4_assign => -- CASE_BODY37171[l.cp:46]
        pro_state_next <= S_p1_end;
      when S_p1_end => -- PROCESS0[:0]
        pro_state_next <= S_p1_end;
        PRO_p1_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          d,
          pro_state
          )
  begin
    -- Default values
    REG_xa_WR <= to_signed(0,8);
    REG_xa_WE <= '0';
    LINK_ln_RE <= '0';
    REG_PRO_p1_EXCEPTION_WR <= to_signed(0,8);
    REG_PRO_p1_EXCEPTION_WE <= '0';
    REG_x_WR <= to_signed(0,10);
    REG_x_WE <= '0';
    LINK_ln_STOP <= '0';
    case pro_state is
      when S_p1_start => -- PROCESS0[:0]
        null;
      when S_i5_for_loop => -- COUNT_LOOP39314[l.cp:35]
        null;
      when S_i5_for_loop_cond => -- COUNT_LOOP39314[l.cp:35]
        null;
      when S_i6_assign => -- ASSIGN2877[l.cp:37]
        REG_xa_WR <= CONST_I8_114;
        REG_xa_WE <= '1';
      when S_i7_fun => -- FUN77977[l.cp:38]
        LINK_ln_RE <= LINK_ln_GD;
      when S_i8_assign => -- ASSIGN23890[l.cp:39]
        REG_xa_WR <= CONST_I8_46;
        REG_xa_WE <= '1';
      when S_i9_branch => -- BRANCH7669[l.cp:40]
        null;
      when S_i10_raise => -- BRANCH_TRUE58851[:0]
        REG_PRO_p1_EXCEPTION_WR <= CONST_I8_1;
        REG_PRO_p1_EXCEPTION_WE <= '1';
      when S_i12_assign => -- ASSIGN83560[l.cp:41]
        REG_x_WR <= L_to_I((d));
        REG_x_WE <= '1';
      when S_i5_for_loop_incr => -- COUNT_LOOP39314[l.cp:35]
        null;
      when S_i2_select => -- FUN7961[l.cp:46]
        null;
      when S_i3_fun => -- FUN7961[l.cp:46]
        LINK_ln_STOP <= LINK_ln_GD;
      when S_i4_assign => -- CASE_BODY37171[l.cp:46]
        REG_PRO_p1_EXCEPTION_WR <= CONST_I8_0;
        REG_PRO_p1_EXCEPTION_WE <= '1';
      when S_p1_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          LINK_ln_RD,
          LINK_ln_RD_ERR,
          LOOP_i_0,
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        LOOP_i_0 <= to_signed(0,5);
        d <= "0000000000";
        err <= '0';
      else
        case pro_state is
          when S_p1_start => -- PROCESS0[:0]
            null;
          when S_i5_for_loop => -- COUNT_LOOP39314[l.cp:35]
            LOOP_i_0 <= CONST_I5_1;
          when S_i5_for_loop_cond => -- COUNT_LOOP39314[l.cp:35]
            null;
          when S_i6_assign => -- ASSIGN2877[l.cp:37]
            null;
          when S_i7_fun => -- FUN77977[l.cp:38]
            d <= LINK_ln_RD;
            err <= LINK_ln_RD_ERR;
          when S_i8_assign => -- ASSIGN23890[l.cp:39]
            null;
          when S_i9_branch => -- BRANCH7669[l.cp:40]
            null;
          when S_i10_raise => -- BRANCH_TRUE58851[:0]
            null;
          when S_i12_assign => -- ASSIGN83560[l.cp:41]
            null;
          when S_i5_for_loop_incr => -- COUNT_LOOP39314[l.cp:35]
            LOOP_i_0 <= LOOP_i_0 + CONST_I5_1;
          when S_i2_select => -- FUN7961[l.cp:46]
            null;
          when S_i3_fun => -- FUN7961[l.cp:46]
            null;
          when S_i4_assign => -- CASE_BODY37171[l.cp:46]
            null;
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
