--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D175 Genetic size: 2714497
--         Compile date: Fri Apr  1 18:08:25 CEST 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <p2> from module <Q>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity q_p2 is
port(
  -- Connections to external objects, components and the outside world
  signal QUEUEX_q_RD: in std_logic_vector(7 downto 0);
  signal QUEUEX_q_RE: out std_logic;
  signal QUEUEX_q_GD: in std_logic;
  signal REG_d2_WR: out std_logic_vector(7 downto 0);
  signal REG_d2_WE: out std_logic;
  signal PRO_p2_ENABLE: in std_logic;
  signal PRO_p2_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end q_p2;
architecture main of q_p2 is
  -- Local and temporary data objects
  signal d: std_logic_vector(7 downto 0);
  signal LOOP_i_1: signed(4 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_p2_start, -- PROCESS0[:0]
    S_i1_assign, -- ASSIGN7669[q.cp:32]
    S_i2_for_loop, -- COUNT_LOOP71138[q.cp:33]
    S_i2_for_loop_cond, -- COUNT_LOOP71138[q.cp:33]
    S_i3_fun, -- FUN16077[q.cp:35]
    S_i4_assign, -- ASSIGN48484[q.cp:36]
    S_i2_for_loop_incr, -- COUNT_LOOP71138[q.cp:33]
    S_p2_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_p2_start;
  signal pro_state_next: pro_states := S_p2_start;
  -- Auxilliary toplevel definitions
  constant CONST_I5_0: signed(4 downto 0) := "00000";
  constant CONST_I5_9: signed(4 downto 0) := "01001";
  constant CONST_I5_1: signed(4 downto 0) := "00001";
begin
  state_transition: process(
          PRO_p2_ENABLE,
          pro_state_next,
          conpro_system_clk,
          conpro_system_reset
  )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset='1' or PRO_p2_ENABLE='0' then
        pro_state <= S_p2_start;
      else
        pro_state <= pro_state_next;
      end if;
    end if;
  end process state_transition;
  -- Process implementation
  -- Instruction Controlpath Unit - The Leitwerk
  control_path: process(
          LOOP_i_1,
          QUEUEX_q_GD,
          pro_state
          )
  begin
    PRO_p2_END <= '0';
    case pro_state is
      when S_p2_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_assign;
      when S_i1_assign => -- ASSIGN7669[q.cp:32]
        pro_state_next <= S_i2_for_loop;
      when S_i2_for_loop => -- COUNT_LOOP71138[q.cp:33]
        pro_state_next <= S_i2_for_loop_cond;
      when S_i2_for_loop_cond => -- COUNT_LOOP71138[q.cp:33]
        if CONST_I5_9 >= LOOP_i_1 then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_p2_end;
        end if;
      when S_i3_fun => -- FUN16077[q.cp:35]
        if not((QUEUEX_q_GD) = ('0')) then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_i4_assign;
        end if;
      when S_i4_assign => -- ASSIGN48484[q.cp:36]
        pro_state_next <= S_i2_for_loop_incr;
      when S_i2_for_loop_incr => -- COUNT_LOOP71138[q.cp:33]
        pro_state_next <= S_i2_for_loop_cond;
      when S_p2_end => -- PROCESS0[:0]
        pro_state_next <= S_p2_end;
        PRO_p2_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          d,
          pro_state
          )
  begin
    -- Default values
    QUEUEX_q_RE <= '0';
    REG_d2_WR <= "00000000";
    REG_d2_WE <= '0';
    case pro_state is
      when S_p2_start => -- PROCESS0[:0]
        null;
      when S_i1_assign => -- ASSIGN7669[q.cp:32]
        null;
      when S_i2_for_loop => -- COUNT_LOOP71138[q.cp:33]
        null;
      when S_i2_for_loop_cond => -- COUNT_LOOP71138[q.cp:33]
        null;
      when S_i3_fun => -- FUN16077[q.cp:35]
        QUEUEX_q_RE <= QUEUEX_q_GD;
      when S_i4_assign => -- ASSIGN48484[q.cp:36]
        REG_d2_WR <= d;
        REG_d2_WE <= '1';
      when S_i2_for_loop_incr => -- COUNT_LOOP71138[q.cp:33]
        null;
      when S_p2_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          QUEUEX_q_RD,
          LOOP_i_1,
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        d <= "00000000";
        LOOP_i_1 <= to_signed(0,5);
      else
        case pro_state is
          when S_p2_start => -- PROCESS0[:0]
            null;
          when S_i1_assign => -- ASSIGN7669[q.cp:32]
            d <= "00000000";
          when S_i2_for_loop => -- COUNT_LOOP71138[q.cp:33]
            LOOP_i_1 <= CONST_I5_0;
          when S_i2_for_loop_cond => -- COUNT_LOOP71138[q.cp:33]
            null;
          when S_i3_fun => -- FUN16077[q.cp:35]
            d <= QUEUEX_q_RD;
          when S_i4_assign => -- ASSIGN48484[q.cp:36]
            null;
          when S_i2_for_loop_incr => -- COUNT_LOOP71138[q.cp:33]
            LOOP_i_1 <= LOOP_i_1 + CONST_I5_1;
          when S_p2_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
