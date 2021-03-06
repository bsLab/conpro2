--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681183
--         Compile date: Wed Apr 28 16:29:24 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <main> from module <T>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity t_main is
port(
  -- Connections to external objects, components and the outside world
  signal rl_RD: in std_logic_vector(9 downto 0);
  signal PRO_p1_START: out std_logic;
  signal PRO_p1_GD: in std_logic;
  signal F_f1_INIT: out std_logic;
  signal F_f1_GD: in std_logic;
  signal F_f1_WE: out std_logic;
  signal F_f1_TIME_SET: out std_logic_vector(2 downto 0);
  signal PRO_main_ENABLE: in std_logic;
  signal PRO_main_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end t_main;
architecture main of t_main is
  -- Local and temporary data objects
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_main_start, -- PROCESS0[:0]
    S_i1_fun, -- FUN70081[t.cp:44]
    S_i3_fun, -- FUN57915[t.cp:46]
    S_i4_fun, -- FUN34847[t.cp:48]
    S_main_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_main_start;
  signal pro_state_next: pro_states := S_main_start;
  -- Auxilliary toplevel definitions
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
          PRO_p1_GD,
          pro_state
          )
  begin
    PRO_main_END <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_fun;
      when S_i1_fun => -- FUN70081[t.cp:44]
        pro_state_next <= S_main_end;
      when S_i3_fun => -- FUN57915[t.cp:46]
        pro_state_next <= S_i4_fun;
      when S_i4_fun => -- FUN34847[t.cp:48]
        if PRO_p1_GD = '1' then
          pro_state_next <= S_i4_fun;
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
          pro_state
          )
  begin
    -- Default values
    F_f1_WE <= '0';
    F_f1_TIME_SET <= "000";
    F_f1_INIT <= '0';
    PRO_p1_START <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        null;
      when S_i1_fun => -- FUN70081[t.cp:44]
        F_f1_WE <= '1';
        F_f1_TIME_SET <= "100";
      when S_i3_fun => -- FUN57915[t.cp:46]
        F_f1_INIT <= '1';
      when S_i4_fun => -- FUN34847[t.cp:48]
        PRO_p1_START <= '1';
      when S_main_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
