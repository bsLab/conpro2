--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D148 Genetic size: 2661823
--         Compile date: Fri Apr 23 17:13:19 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <init> from module <Dining>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity dining_init is
port(
  -- Connections to external objects, components and the outside world
  signal EVENT_ev_INIT: out std_logic;
  signal EVENT_ev_GD: in std_logic;
  signal SEMA_fork_INIT: out std_logic;
  signal SEMA_fork_WR: out std_logic_vector(7 downto 0);
  signal SEMA_fork_GD: in std_logic;
  signal SEMA_fork_SEL: out integer;
  signal PRO_init_ENABLE: in std_logic;
  signal PRO_init_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end dining_init;
architecture main of dining_init is
  -- Local and temporary data objects
  signal LOOP_i_0: signed(3 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_init_start, -- PROCESS0[:0]
    S_BLOCKBOUND4_1, -- COUNT_LOOP_BLOCK183561[dining.cp:34]
    S_i1_for_loop_cond, -- COUNT_LOOP83561[dining.cp:34]
    S_i2_fun, -- FUN64819[dining.cp:36]
    S_i1_for_loop_incr, -- COUNT_LOOP83561[dining.cp:34]
    S_i3_fun, -- FUN13512[dining.cp:38]
    S_init_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_init_start;
  signal pro_state_next: pro_states := S_init_start;
  -- Auxilliary toplevel definitions
  constant CONST_I4_0: signed(3 downto 0) := "0000";
  constant CONST_I4_4: signed(3 downto 0) := "0100";
  constant CONST_I4_1: signed(3 downto 0) := "0001";
begin
  state_transition: process(
          PRO_init_ENABLE,
          pro_state_next,
          conpro_system_clk,
          conpro_system_reset
  )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset='1' or PRO_init_ENABLE='0' then
        pro_state <= S_init_start;
      else
        pro_state <= pro_state_next;
      end if;
    end if;
  end process state_transition;
  -- Process implementation
  -- Instruction Controlpath Unit - The Leitwerk
  control_path: process(
          LOOP_i_0,
          SEMA_fork_GD,
          EVENT_ev_GD,
          pro_state
          )
  begin
    PRO_init_END <= '0';
    case pro_state is
      when S_init_start => -- PROCESS0[:0]
        pro_state_next <= S_BLOCKBOUND4_1;
      when S_BLOCKBOUND4_1 => -- COUNT_LOOP_BLOCK183561[dining.cp:34]
        pro_state_next <= S_i1_for_loop_cond;
      when S_i1_for_loop_cond => -- COUNT_LOOP83561[dining.cp:34]
        if CONST_I4_4 >= LOOP_i_0 then
          pro_state_next <= S_i2_fun;
        else
          pro_state_next <= S_i3_fun;
        end if;
      when S_i2_fun => -- FUN64819[dining.cp:36]
        if not((SEMA_fork_GD) = ('0')) then
          pro_state_next <= S_i2_fun;
        else
          pro_state_next <= S_i1_for_loop_incr;
        end if;
      when S_i1_for_loop_incr => -- COUNT_LOOP83561[dining.cp:34]
        pro_state_next <= S_i1_for_loop_cond;
      when S_i3_fun => -- FUN13512[dining.cp:38]
        if not((EVENT_ev_GD) = ('0')) then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_init_end;
        end if;
      when S_init_end => -- PROCESS0[:0]
        pro_state_next <= S_init_end;
        PRO_init_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          LOOP_i_0,
          pro_state
          )
  begin
    -- Default values
    SEMA_fork_INIT <= '0';
    SEMA_fork_WR <= "00000000";
    SEMA_fork_SEL <= 0;
    EVENT_ev_INIT <= '0';
    case pro_state is
      when S_init_start => -- PROCESS0[:0]
        null;
      when S_BLOCKBOUND4_1 => -- COUNT_LOOP_BLOCK183561[dining.cp:34]
        null;
      when S_i1_for_loop_cond => -- COUNT_LOOP83561[dining.cp:34]
        null;
      when S_i2_fun => -- FUN64819[dining.cp:36]
        SEMA_fork_INIT <= SEMA_fork_GD;
        SEMA_fork_WR <= "00000001";
        SEMA_fork_SEL <= I_to_N((LOOP_i_0));
      when S_i1_for_loop_incr => -- COUNT_LOOP83561[dining.cp:34]
        null;
      when S_i3_fun => -- FUN13512[dining.cp:38]
        EVENT_ev_INIT <= EVENT_ev_GD;
      when S_init_end => -- PROCESS0[:0]
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
        LOOP_i_0 <= to_signed(0,4);
      else
        case pro_state is
          when S_init_start => -- PROCESS0[:0]
            null;
          when S_BLOCKBOUND4_1 => -- COUNT_LOOP_BLOCK183561[dining.cp:34]
            LOOP_i_0 <= CONST_I4_0;
          when S_i1_for_loop_cond => -- COUNT_LOOP83561[dining.cp:34]
            null;
          when S_i2_fun => -- FUN64819[dining.cp:36]
            null;
          when S_i1_for_loop_incr => -- COUNT_LOOP83561[dining.cp:34]
            LOOP_i_0 <= LOOP_i_0 + CONST_I4_1;
          when S_i3_fun => -- FUN13512[dining.cp:38]
            null;
          when S_init_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
