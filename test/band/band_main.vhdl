--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D151 Genetic size: 2685546
--         Compile date: Mon May  3 17:06:14 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <main> from module <Band>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity band_main is
port(
  -- Connections to external objects, components and the outside world
  signal PRO_main_ENABLE: in std_logic;
  signal PRO_main_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end band_main;
architecture main of band_main is
  -- Local and temporary data objects
  signal x: signed(7 downto 0);
  signal y: signed(7 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_main_start, -- PROCESS0[:0]
    S_i1_assign, -- ASSIGN16603[band.cp:10]
    S_i2_assign, -- ASSIGN44358[band.cp:10]
    S_i3_assign, -- ASSIGN65705[band.cp:11]
    S_i4_assign, -- ASSIGN17429[band.cp:11]
    S_i5_branch, -- BRANCH39314[band.cp:12]
    S_i6_assign, -- ASSIGN85449[band.cp:12]
    S_main_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_main_start;
  signal pro_state_next: pro_states := S_main_start;
  -- Auxilliary toplevel definitions
  constant CONST_I8_0: signed(7 downto 0) := "00000000";
  constant CONST_I8_1: signed(7 downto 0) := "00000001";
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
          y,
          x,
          pro_state
          )
  begin
    PRO_main_END <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_assign;
      when S_i1_assign => -- ASSIGN16603[band.cp:10]
        pro_state_next <= S_i2_assign;
      when S_i2_assign => -- ASSIGN44358[band.cp:10]
        pro_state_next <= S_i3_assign;
      when S_i3_assign => -- ASSIGN65705[band.cp:11]
        pro_state_next <= S_i4_assign;
      when S_i4_assign => -- ASSIGN17429[band.cp:11]
        pro_state_next <= S_i5_branch;
      when S_i5_branch => -- BRANCH39314[band.cp:12]
        if (x = CONST_I8_1) and (y = CONST_I8_0) then
          pro_state_next <= S_i6_assign;
        else
          pro_state_next <= S_main_end;
        end if;
      when S_i6_assign => -- ASSIGN85449[band.cp:12]
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
      when S_i1_assign => -- ASSIGN16603[band.cp:10]
        null;
      when S_i2_assign => -- ASSIGN44358[band.cp:10]
        null;
      when S_i3_assign => -- ASSIGN65705[band.cp:11]
        null;
      when S_i4_assign => -- ASSIGN17429[band.cp:11]
        null;
      when S_i5_branch => -- BRANCH39314[band.cp:12]
        null;
      when S_i6_assign => -- ASSIGN85449[band.cp:12]
        null;
      when S_main_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          x,
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        x <= to_signed(0,8);
        y <= to_signed(0,8);
      else
        case pro_state is
          when S_main_start => -- PROCESS0[:0]
            null;
          when S_i1_assign => -- ASSIGN16603[band.cp:10]
            x <= CONST_I8_0;
          when S_i2_assign => -- ASSIGN44358[band.cp:10]
            x <= x + CONST_I8_1;
          when S_i3_assign => -- ASSIGN65705[band.cp:11]
            y <= CONST_I8_1;
          when S_i4_assign => -- ASSIGN17429[band.cp:11]
            y <= x + CONST_I8_1;
          when S_i5_branch => -- BRANCH39314[band.cp:12]
            null;
          when S_i6_assign => -- ASSIGN85449[band.cp:12]
            x <= x - CONST_I8_1;
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
