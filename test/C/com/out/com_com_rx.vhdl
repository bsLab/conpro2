--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681845
--         Compile date: Wed Apr 28 18:22:40 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <com_rx> from module <Com>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_unsigned.all;
use work.ConPRO.all;
entity com_com_rx is
port(
  -- Connections to external objects, components and the outside world
  signal REG_sys_status_WR: out std_logic_vector(2 downto 0);
  signal REG_sys_status_WE: out std_logic;
  signal REG_sys_status_GD: in std_logic;
  signal UART_com_RE: out std_logic;
  signal UART_com_RD: in std_logic_vector(7 downto 0);
  signal UART_com_RD_ERR: in std_logic;
  signal UART_com_GD: in std_logic;
  signal QUEUE_rx_q_WR: out std_logic_vector(7 downto 0);
  signal QUEUE_rx_q_WE: out std_logic;
  signal QUEUE_rx_q_GD: in std_logic;
  signal PRO_com_rx_ENABLE: in std_logic;
  signal PRO_com_rx_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end com_com_rx;
architecture main of com_com_rx is
  -- Local and temporary data objects
  signal d: std_logic_vector(7 downto 0);
  signal err: std_logic;
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_com_rx_start, -- PROCESS0[:0]
    S_i1_assign, -- ASSIGN23606[com.cp:167]
    S_i2_while_loop, -- COND_LOOP58009[com.cp:169]
    S_i3_fun, -- FUN46246[com.cp:171]
    S_i4_assign, -- ASSIGN92729[com.cp:172]
    S_i5_branch, -- BRANCH67938[com.cp:173]
    S_i6_assign, -- ASSIGN26142[com.cp:173]
    S_com_rx_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_com_rx_start;
  signal pro_state_next: pro_states := S_com_rx_start;
  -- Auxilliary toplevel definitions
begin
  state_transition: process(
          PRO_com_rx_ENABLE,
          pro_state_next,
          conpro_system_clk,
          conpro_system_reset
  )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset='0' or PRO_com_rx_ENABLE='0' then
        pro_state <= S_com_rx_start;
      else
        pro_state <= pro_state_next;
      end if;
    end if;
  end process state_transition;
  -- Process implementation
  -- Instruction Controlpath Unit - The Leitwerk
  control_path: process(
          err,
          UART_com_GD,
          REG_sys_status_GD,
          QUEUE_rx_q_GD,
          pro_state
          )
  begin
    PRO_com_rx_END <= '0';
    case pro_state is
      when S_com_rx_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_assign;
      when S_i1_assign => -- ASSIGN23606[com.cp:167]
        pro_state_next <= S_i2_while_loop;
      when S_i2_while_loop => -- COND_LOOP58009[com.cp:169]
        if err = '0' then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_com_rx_end;
        end if;
      when S_i3_fun => -- FUN46246[com.cp:171]
        if not((UART_com_GD) = ('0')) then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_i4_assign;
        end if;
      when S_i4_assign => -- ASSIGN92729[com.cp:172]
        if REG_sys_status_GD = '1' then
          pro_state_next <= S_i4_assign;
        else
          pro_state_next <= S_i5_branch;
        end if;
      when S_i5_branch => -- BRANCH67938[com.cp:173]
        if err = '0' then
          pro_state_next <= S_i6_assign;
        else
          pro_state_next <= S_i2_while_loop;
        end if;
      when S_i6_assign => -- ASSIGN26142[com.cp:173]
        if QUEUE_rx_q_GD = '1' then
          pro_state_next <= S_i6_assign;
        else
          pro_state_next <= S_i2_while_loop;
        end if;
      when S_com_rx_end => -- PROCESS0[:0]
        pro_state_next <= S_com_rx_end;
        PRO_com_rx_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          QUEUE_rx_q_GD,
          d,
          pro_state
          )
  begin
    -- Default values
    UART_com_RE <= '0';
    REG_sys_status_WR <= "000";
    REG_sys_status_WE <= '0';
    QUEUE_rx_q_WR <= "00000000";
    QUEUE_rx_q_WE <= '0';
    case pro_state is
      when S_com_rx_start => -- PROCESS0[:0]
        null;
      when S_i1_assign => -- ASSIGN23606[com.cp:167]
        null;
      when S_i2_while_loop => -- COND_LOOP58009[com.cp:169]
        null;
      when S_i3_fun => -- FUN46246[com.cp:171]
        UART_com_RE <= UART_com_GD;
      when S_i4_assign => -- ASSIGN92729[com.cp:172]
        REG_sys_status_WR <= "100";
        REG_sys_status_WE <= '1';
      when S_i5_branch => -- BRANCH67938[com.cp:173]
        null;
      when S_i6_assign => -- ASSIGN26142[com.cp:173]
        QUEUE_rx_q_WR <= d;
        QUEUE_rx_q_WE <=  QUEUE_rx_q_GD;
      when S_com_rx_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          UART_com_RD,
          UART_com_RD_ERR,
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '0' then
        err <= '0';
        d <= "00000000";
      else
        case pro_state is
          when S_com_rx_start => -- PROCESS0[:0]
            null;
          when S_i1_assign => -- ASSIGN23606[com.cp:167]
            err <= '0';
          when S_i2_while_loop => -- COND_LOOP58009[com.cp:169]
            null;
          when S_i3_fun => -- FUN46246[com.cp:171]
            d <= UART_com_RD;
            err <= UART_com_RD_ERR;
          when S_i4_assign => -- ASSIGN92729[com.cp:172]
            null;
          when S_i5_branch => -- BRANCH67938[com.cp:173]
            null;
          when S_i6_assign => -- ASSIGN26142[com.cp:173]
            null;
          when S_com_rx_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;