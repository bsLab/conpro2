--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D161 Genetic size: 2703860
--         Compile date: Wed Jun 30 14:12:50 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <p1> from module <C>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity c_p1 is
port(
  -- Connections to external objects, components and the outside world
  signal REG_x_RD: in signed(7 downto 0);
  signal REG_x_WR: out signed(7 downto 0);
  signal REG_x_WE: out std_logic;
  signal REG_x_GD: in std_logic;
  signal PRO_p1_ENABLE: in std_logic;
  signal PRO_p1_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end c_p1;
architecture main of c_p1 is
  -- Local and temporary data objects
  signal LOOP_i_0: signed(4 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_p1_start, -- PROCESS0[:0]
    S_i1_for_loop, -- COUNT_LOOP44358[c.cp:11]
    S_i1_for_loop_cond, -- COUNT_LOOP44358[c.cp:11]
    S_i2_assign, -- ASSIGN65705[c.cp:11]
    S_i1_for_loop_incr, -- COUNT_LOOP44358[c.cp:11]
    S_p1_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_p1_start;
  signal pro_state_next: pro_states := S_p1_start;
  -- Auxilliary toplevel definitions
  constant CONST_I5_1: signed(4 downto 0) := "00001";
  constant CONST_I5_10: signed(4 downto 0) := "01010";
  constant CONST_I8_1: signed(7 downto 0) := "00000001";
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
          REG_x_GD,
          pro_state
          )
  begin
    PRO_p1_END <= '0';
    case pro_state is
      when S_p1_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_for_loop;
      when S_i1_for_loop => -- COUNT_LOOP44358[c.cp:11]
        pro_state_next <= S_i1_for_loop_cond;
      when S_i1_for_loop_cond => -- COUNT_LOOP44358[c.cp:11]
        if CONST_I5_10 >= LOOP_i_0 then
          pro_state_next <= S_i2_assign;
        else
          pro_state_next <= S_p1_end;
        end if;
      when S_i2_assign => -- ASSIGN65705[c.cp:11]
        if REG_x_GD = '1' then
          pro_state_next <= S_i2_assign;
        else
          pro_state_next <= S_i1_for_loop_incr;
        end if;
      when S_i1_for_loop_incr => -- COUNT_LOOP44358[c.cp:11]
        pro_state_next <= S_i1_for_loop_cond;
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
      when S_i1_for_loop => -- COUNT_LOOP44358[c.cp:11]
        null;
      when S_i1_for_loop_cond => -- COUNT_LOOP44358[c.cp:11]
        null;
      when S_i2_assign => -- ASSIGN65705[c.cp:11]
        REG_x_WR <= REG_x_RD + CONST_I8_1;
        REG_x_WE <= '1';
      when S_i1_for_loop_incr => -- COUNT_LOOP44358[c.cp:11]
        null;
      when S_p1_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          LOOP_i_0,
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        LOOP_i_0 <= to_signed(0,5);
      else
        case pro_state is
          when S_p1_start => -- PROCESS0[:0]
            null;
          when S_i1_for_loop => -- COUNT_LOOP44358[c.cp:11]
            LOOP_i_0 <= CONST_I5_1;
          when S_i1_for_loop_cond => -- COUNT_LOOP44358[c.cp:11]
            null;
          when S_i2_assign => -- ASSIGN65705[c.cp:11]
            null;
          when S_i1_for_loop_incr => -- COUNT_LOOP44358[c.cp:11]
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
