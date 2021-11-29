--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D174 Genetic size: 2713135
--         Compile date: Fri Jan 21 09:49:21 CET 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <main> from module <L>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity l_main is
port(
  -- Connections to external objects, components and the outside world
  signal LINK_ln_INIT: out std_logic;
  signal LINK_ln_START: out std_logic;
  signal LINK_ln_GD: in std_logic;
  signal PRO_p1_START: out std_logic;
  signal PRO_p1_GD: in std_logic;
  signal PRO_p2_START: out std_logic;
  signal PRO_p2_GD: in std_logic;
  signal PRO_main_ENABLE: in std_logic;
  signal PRO_main_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end l_main;
architecture main of l_main is
  -- Local and temporary data objects
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_main_start, -- PROCESS0[:0]
    S_i1_fun, -- FUN7270[l.cp:76]
    S_i2_fun, -- FUN67142[l.cp:77]
    S_i3_fun, -- FUN85776[l.cp:78]
    S_i4_fun, -- FUN14484[l.cp:79]
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
          LINK_ln_GD,
          PRO_p1_GD,
          PRO_p2_GD,
          pro_state
          )
  begin
    PRO_main_END <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_fun;
      when S_i1_fun => -- FUN7270[l.cp:76]
        if not((LINK_ln_GD) = ('0')) then
          pro_state_next <= S_i1_fun;
        else
          pro_state_next <= S_i2_fun;
        end if;
      when S_i2_fun => -- FUN67142[l.cp:77]
        if not((LINK_ln_GD) = ('0')) then
          pro_state_next <= S_i2_fun;
        else
          pro_state_next <= S_i3_fun;
        end if;
      when S_i3_fun => -- FUN85776[l.cp:78]
        if PRO_p1_GD = '1' then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_i4_fun;
        end if;
      when S_i4_fun => -- FUN14484[l.cp:79]
        if PRO_p2_GD = '1' then
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
    LINK_ln_INIT <= '0';
    LINK_ln_START <= '0';
    PRO_p1_START <= '0';
    PRO_p2_START <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        null;
      when S_i1_fun => -- FUN7270[l.cp:76]
        LINK_ln_INIT <= LINK_ln_GD;
      when S_i2_fun => -- FUN67142[l.cp:77]
        LINK_ln_START <= LINK_ln_GD;
      when S_i3_fun => -- FUN85776[l.cp:78]
        PRO_p1_START <= '1';
      when S_i4_fun => -- FUN14484[l.cp:79]
        PRO_p2_START <= '1';
      when S_main_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
