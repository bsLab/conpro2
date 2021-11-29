--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681845
--         Compile date: Wed Apr 28 18:22:40 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <FUN_request> from module <Com>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_unsigned.all;
use work.ConPRO.all;
entity com_FUN_request is
port(
  -- Connections to external objects, components and the outside world
  signal REG_RET_FUN_request_d2_WR: out std_logic_vector(7 downto 0);
  signal REG_RET_FUN_request_d2_WE: out std_logic;
  signal REG_ARG_FUN_request_d1_RD: in std_logic_vector(7 downto 0);
  signal QUEUE_tx_q_WR: out std_logic_vector(7 downto 0);
  signal QUEUE_tx_q_WE: out std_logic;
  signal QUEUE_tx_q_GD: in std_logic;
  signal EVENT_rep_AWAIT: out std_logic;
  signal EVENT_rep_GD: in std_logic;
  signal REG_rep_d_RD: in std_logic_vector(7 downto 0);
  signal PRO_FUN_request_ENABLE: in std_logic;
  signal PRO_FUN_request_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end com_FUN_request;
architecture main of com_FUN_request is
  -- Local and temporary data objects
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_FUN_request_start, -- PROCESS0[:0]
    S_i1_assign, -- ASSIGN44358[com.cp:242]
    S_i2_assign, -- ASSIGN65705[com.cp:243]
    S_i3_fun, -- FUN79980[com.cp:244]
    S_i4_assign, -- ASSIGN39314[com.cp:245]
    S_FUN_request_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_FUN_request_start;
  signal pro_state_next: pro_states := S_FUN_request_start;
  -- Auxilliary toplevel definitions
begin
  state_transition: process(
          PRO_FUN_request_ENABLE,
          pro_state_next,
          conpro_system_clk,
          conpro_system_reset
  )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset='0' or PRO_FUN_request_ENABLE='0' then
        pro_state <= S_FUN_request_start;
      else
        pro_state <= pro_state_next;
      end if;
    end if;
  end process state_transition;
  -- Process implementation
  -- Instruction Controlpath Unit - The Leitwerk
  control_path: process(
          QUEUE_tx_q_GD,
          EVENT_rep_GD,
          pro_state
          )
  begin
    PRO_FUN_request_END <= '0';
    case pro_state is
      when S_FUN_request_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_assign;
      when S_i1_assign => -- ASSIGN44358[com.cp:242]
        if QUEUE_tx_q_GD = '1' then
          pro_state_next <= S_i1_assign;
        else
          pro_state_next <= S_i2_assign;
        end if;
      when S_i2_assign => -- ASSIGN65705[com.cp:243]
        if QUEUE_tx_q_GD = '1' then
          pro_state_next <= S_i2_assign;
        else
          pro_state_next <= S_i3_fun;
        end if;
      when S_i3_fun => -- FUN79980[com.cp:244]
        if not((EVENT_rep_GD) = ('0')) then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_i4_assign;
        end if;
      when S_i4_assign => -- ASSIGN39314[com.cp:245]
        pro_state_next <= S_FUN_request_end;
      when S_FUN_request_end => -- PROCESS0[:0]
        pro_state_next <= S_FUN_request_end;
        PRO_FUN_request_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          QUEUE_tx_q_GD,
          REG_ARG_FUN_request_d1_RD,
          REG_rep_d_RD,
          pro_state
          )
  begin
    -- Default values
    QUEUE_tx_q_WR <= "00000000";
    QUEUE_tx_q_WE <= '0';
    EVENT_rep_AWAIT <= '0';
    REG_RET_FUN_request_d2_WR <= "00000000";
    REG_RET_FUN_request_d2_WE <= '0';
    case pro_state is
      when S_FUN_request_start => -- PROCESS0[:0]
        null;
      when S_i1_assign => -- ASSIGN44358[com.cp:242]
        QUEUE_tx_q_WR <= "01010010";
        QUEUE_tx_q_WE <=  QUEUE_tx_q_GD;
      when S_i2_assign => -- ASSIGN65705[com.cp:243]
        QUEUE_tx_q_WR <= REG_ARG_FUN_request_d1_RD;
        QUEUE_tx_q_WE <=  QUEUE_tx_q_GD;
      when S_i3_fun => -- FUN79980[com.cp:244]
        EVENT_rep_AWAIT <= EVENT_rep_GD;
      when S_i4_assign => -- ASSIGN39314[com.cp:245]
        REG_RET_FUN_request_d2_WR <= REG_rep_d_RD;
        REG_RET_FUN_request_d2_WE <= '1';
      when S_FUN_request_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;