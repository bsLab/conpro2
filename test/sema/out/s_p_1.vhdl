--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 A15 **  2049142 total genes
--         Compiled on Thu Nov 27 11:07:26 CET 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

-- Process implementation of process <p_1> from module <S>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity s_p_1 is
port(
  -- Connections to external objects, components and the outside world
  signal REG_d_WR: out signed(7 downto 0);
  signal REG_d_WE: out std_logic;
  signal REG_d_GD: in std_logic;
  signal SEMA_s_DOWN: out std_logic;
  signal SEMA_s_UP: out std_logic;
  signal SEMA_s_GD: in std_logic;
  signal REG_x_RD: in signed(7 downto 0);
  signal REG_x_WR: out signed(7 downto 0);
  signal REG_x_WE: out std_logic;
  signal REG_x_GD: in std_logic;
  signal REG_y_RD: in signed(7 downto 0);
  signal REG_y_WR: out signed(7 downto 0);
  signal REG_y_WE: out std_logic;
  signal REG_y_GD: in std_logic;
  signal REG_z_RD: in signed(7 downto 0);
  signal REG_z_WR: out signed(7 downto 0);
  signal REG_z_WE: out std_logic;
  signal REG_z_GD: in std_logic;
  signal PRO_p_1_ENABLE: in std_logic;
  signal PRO_p_1_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end s_p_1;
architecture main of s_p_1 is
  -- Local and temporary data objects
  signal LOOP_i_1: signed(4 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_p_1_start, -- PROCESS0[:0]
    S_i1_for_loop, -- COUNT_LOOP39314[s.cp:17]
    S_i1_for_loop_cond, -- COUNT_LOOP39314[s.cp:17]
    S_i2_fun, -- FUN11997[s.cp:19]
    S_i3_assign, -- ASSIGN85449[s.cp:20]
    S_i4_assign, -- ASSIGN23890[s.cp:21]
    S_i5_assign, -- ASSIGN7669[s.cp:22]
    S_i6_assign, -- ASSIGN58851[s.cp:23]
    S_i7_fun, -- FUN25871[s.cp:24]
    S_i1_for_loop_incr, -- COUNT_LOOP39314[s.cp:17]
    S_p_1_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_p_1_start;
  signal pro_state_next: pro_states := S_p_1_start;
  -- Auxilliary toplevel definitions
  constant CONST_I5_1: signed(4 downto 0) := "00001";
  constant CONST_I5_10: signed(4 downto 0) := "01010";
  constant CONST_I8_2: signed(7 downto 0) := "00000010";
begin
  state_transition: process(
          PRO_p_1_ENABLE,
          pro_state_next,
          conpro_system_clk,
          conpro_system_reset
  )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset='1' or PRO_p_1_ENABLE='0' then
        pro_state <= S_p_1_start;
      else
        pro_state <= pro_state_next;
      end if;
    end if;
  end process state_transition;
  -- Process implementation
  -- Instruction Controlpath Unit - The Leitwerk
  control_path: process(
          LOOP_i_1,
          SEMA_s_GD,
          REG_d_GD,
          REG_x_GD,
          REG_y_GD,
          REG_z_GD,
          pro_state
          )
  begin
    PRO_p_1_END <= '0';
    case pro_state is
      when S_p_1_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_for_loop;
      when S_i1_for_loop => -- COUNT_LOOP39314[s.cp:17]
        pro_state_next <= S_i1_for_loop_cond;
      when S_i1_for_loop_cond => -- COUNT_LOOP39314[s.cp:17]
        if CONST_I5_10 >= LOOP_i_1 then
          pro_state_next <= S_i2_fun;
        else
          pro_state_next <= S_p_1_end;
        end if;
      when S_i2_fun => -- FUN11997[s.cp:19]
        if not((SEMA_s_GD) = ('0')) then
          pro_state_next <= S_i2_fun;
        else
          pro_state_next <= S_i3_assign;
        end if;
      when S_i3_assign => -- ASSIGN85449[s.cp:20]
        if REG_d_GD = '1' then
          pro_state_next <= S_i3_assign;
        else
          pro_state_next <= S_i4_assign;
        end if;
      when S_i4_assign => -- ASSIGN23890[s.cp:21]
        if REG_x_GD = '1' then
          pro_state_next <= S_i4_assign;
        else
          pro_state_next <= S_i5_assign;
        end if;
      when S_i5_assign => -- ASSIGN7669[s.cp:22]
        if REG_y_GD = '1' then
          pro_state_next <= S_i5_assign;
        else
          pro_state_next <= S_i6_assign;
        end if;
      when S_i6_assign => -- ASSIGN58851[s.cp:23]
        if REG_z_GD = '1' then
          pro_state_next <= S_i6_assign;
        else
          pro_state_next <= S_i7_fun;
        end if;
      when S_i7_fun => -- FUN25871[s.cp:24]
        if not((SEMA_s_GD) = ('0')) then
          pro_state_next <= S_i7_fun;
        else
          pro_state_next <= S_i1_for_loop_incr;
        end if;
      when S_i1_for_loop_incr => -- COUNT_LOOP39314[s.cp:17]
        pro_state_next <= S_i1_for_loop_cond;
      when S_p_1_end => -- PROCESS0[:0]
        pro_state_next <= S_p_1_end;
        PRO_p_1_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          REG_x_RD,
          REG_y_RD,
          REG_z_RD,
          LOOP_i_1,
          pro_state
          )
  begin
    -- Default values
    SEMA_s_DOWN <= '0';
    REG_d_WR <= to_signed(0,8);
    REG_d_WE <= '0';
    REG_x_WR <= to_signed(0,8);
    REG_x_WE <= '0';
    REG_y_WR <= to_signed(0,8);
    REG_y_WE <= '0';
    REG_z_WR <= to_signed(0,8);
    REG_z_WE <= '0';
    SEMA_s_UP <= '0';
    case pro_state is
      when S_p_1_start => -- PROCESS0[:0]
        null;
      when S_i1_for_loop => -- COUNT_LOOP39314[s.cp:17]
        null;
      when S_i1_for_loop_cond => -- COUNT_LOOP39314[s.cp:17]
        null;
      when S_i2_fun => -- FUN11997[s.cp:19]
        SEMA_s_DOWN <= '1';
      when S_i3_assign => -- ASSIGN85449[s.cp:20]
        REG_d_WR <= CONST_I8_2;
        REG_d_WE <= '1';
      when S_i4_assign => -- ASSIGN23890[s.cp:21]
        REG_x_WR <= CONST_I8_2 + REG_x_RD;
        REG_x_WE <= '1';
      when S_i5_assign => -- ASSIGN7669[s.cp:22]
        REG_y_WR <= CONST_I8_2 + REG_y_RD;
        REG_y_WE <= '1';
      when S_i6_assign => -- ASSIGN58851[s.cp:23]
        REG_z_WR <= CONST_I8_2 + REG_z_RD;
        REG_z_WE <= '1';
      when S_i7_fun => -- FUN25871[s.cp:24]
        SEMA_s_UP <= '1';
      when S_i1_for_loop_incr => -- COUNT_LOOP39314[s.cp:17]
        null;
      when S_p_1_end => -- PROCESS0[:0]
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
        LOOP_i_1 <= to_signed(0,5);
      else
        case pro_state is
          when S_p_1_start => -- PROCESS0[:0]
            null;
          when S_i1_for_loop => -- COUNT_LOOP39314[s.cp:17]
            LOOP_i_1 <= CONST_I5_1;
          when S_i1_for_loop_cond => -- COUNT_LOOP39314[s.cp:17]
            null;
          when S_i2_fun => -- FUN11997[s.cp:19]
            null;
          when S_i3_assign => -- ASSIGN85449[s.cp:20]
            null;
          when S_i4_assign => -- ASSIGN23890[s.cp:21]
            null;
          when S_i5_assign => -- ASSIGN7669[s.cp:22]
            null;
          when S_i6_assign => -- ASSIGN58851[s.cp:23]
            null;
          when S_i7_fun => -- FUN25871[s.cp:24]
            null;
          when S_i1_for_loop_incr => -- COUNT_LOOP39314[s.cp:17]
            LOOP_i_1 <= LOOP_i_1 + CONST_I5_1;
          when S_p_1_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
