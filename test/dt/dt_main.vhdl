--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D150 Genetic size: 2683003
--         Compile date: Mon May  3 11:24:21 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <main> from module <Dt>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity dt_main is
port(
  -- Connections to external objects, components and the outside world
  signal PRO_main_ENABLE: in std_logic;
  signal PRO_main_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end dt_main;
architecture main of dt_main is
  -- Local and temporary data objects
  signal x: signed(7 downto 0);
  signal y: signed(8 downto 0);
  signal z: signed(17 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_main_start, -- PROCESS0[:0]
    S_i1_bind_to_3, -- ASSIGN17429[dt.cp:13]
    S_i4_assign, -- ASSIGN39314[dt.cp:14]
    S_main_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_main_start;
  signal pro_state_next: pro_states := S_main_start;
  -- Auxilliary toplevel definitions
  constant CONST_I18_0: signed(17 downto 0) := "000000000000000000";
  constant CONST_I9_1: signed(8 downto 0) := "000000001";
  constant CONST_I8_0: signed(7 downto 0) := "00000000";
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
          pro_state
          )
  begin
    PRO_main_END <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_bind_to_3;
      when S_i1_bind_to_3 => -- ASSIGN17429[dt.cp:13]
        pro_state_next <= S_i4_assign;
      when S_i4_assign => -- ASSIGN39314[dt.cp:14]
        pro_state_next <= S_main_end;
      when S_main_end => -- PROCESS0[:0]
        pro_state_next <= S_main_end;
        PRO_main_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          pro_state
          )
  begin
    -- Default values
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        null;
      when S_i1_bind_to_3 => -- ASSIGN17429[dt.cp:13]
        null;
      when S_i4_assign => -- ASSIGN39314[dt.cp:14]
        null;
      when S_main_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          y,
          z,
          x,
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        z <= to_signed(0,18);
        y <= to_signed(0,9);
        x <= to_signed(0,8);
      else
        case pro_state is
          when S_main_start => -- PROCESS0[:0]
            null;
          when S_i1_bind_to_3 => -- ASSIGN17429[dt.cp:13]
            z <= CONST_I18_0;
            y <= CONST_I9_1;
            x <= CONST_I8_0;
          when S_i4_assign => -- ASSIGN39314[dt.cp:14]
            z <= (z + In_to_Im((x),8,18)) + In_to_Im((y),9,18);
          when S_main_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
