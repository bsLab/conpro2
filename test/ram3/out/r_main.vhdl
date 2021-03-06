--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D133 Genetic size: 2558806
--         Compile date: Fri Jan  8 10:17:58 CET 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <main> from module <R>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity r_main is
port(
  -- Connections to external objects, components and the outside world
  signal REG_d_WR: out signed(7 downto 0);
  signal REG_d_WE: out std_logic;
  signal PRO_p1_CALL: out std_logic;
  signal PRO_p1_GD: in std_logic;
  signal PRO_p2_CALL: out std_logic;
  signal PRO_p2_GD: in std_logic;
  signal PRO_main_ENABLE: in std_logic;
  signal PRO_main_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end r_main;
architecture main of r_main is
  -- Local and temporary data objects
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_main_start, -- PROCESS0[:0]
    S_i1_assign, -- ASSIGN2877[r.cp:30]
    S_i2_fun, -- FUN46811[r.cp:31]
    S_i3_assign, -- ASSIGN23890[r.cp:32]
    S_i4_fun, -- FUN96320[r.cp:33]
    S_i5_assign, -- ASSIGN58851[r.cp:34]
    S_main_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_main_start;
  signal pro_state_next: pro_states := S_main_start;
  -- Auxilliary toplevel definitions
  constant CONST_I8_1: signed(7 downto 0) := "00000001";
  constant CONST_I8_2: signed(7 downto 0) := "00000010";
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
          PRO_p1_GD,
          PRO_p2_GD,
          pro_state
          )
  begin
    PRO_main_END <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_assign;
      when S_i1_assign => -- ASSIGN2877[r.cp:30]
        pro_state_next <= S_i2_fun;
      when S_i2_fun => -- FUN46811[r.cp:31]
        if PRO_p1_GD = '1' then
          pro_state_next <= S_i2_fun;
        else
          pro_state_next <= S_i3_assign;
        end if;
      when S_i3_assign => -- ASSIGN23890[r.cp:32]
        pro_state_next <= S_i4_fun;
      when S_i4_fun => -- FUN96320[r.cp:33]
        if PRO_p2_GD = '1' then
          pro_state_next <= S_i4_fun;
        else
          pro_state_next <= S_i5_assign;
        end if;
      when S_i5_assign => -- ASSIGN58851[r.cp:34]
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
    REG_d_WR <= to_signed(0,8);
    REG_d_WE <= '0';
    PRO_p1_CALL <= '0';
    PRO_p2_CALL <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        null;
      when S_i1_assign => -- ASSIGN2877[r.cp:30]
        REG_d_WR <= CONST_I8_1;
        REG_d_WE <= '1';
      when S_i2_fun => -- FUN46811[r.cp:31]
        PRO_p1_CALL <= '1';
      when S_i3_assign => -- ASSIGN23890[r.cp:32]
        REG_d_WR <= CONST_I8_2;
        REG_d_WE <= '1';
      when S_i4_fun => -- FUN96320[r.cp:33]
        PRO_p2_CALL <= '1';
      when S_i5_assign => -- ASSIGN58851[r.cp:34]
        REG_d_WR <= CONST_I8_0;
        REG_d_WE <= '1';
      when S_main_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
