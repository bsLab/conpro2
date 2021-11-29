--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D148 Genetic size: 2661823
--         Compile date: Fri Apr 23 17:13:19 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Process implementation of process <main> from module <Dining>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity dining_main is
port(
  -- Connections to external objects, components and the outside world
  signal PRO_init_CALL: out std_logic;
  signal PRO_init_GD: in std_logic;
  signal PRO_philosopher_START: out std_logic;
  signal PRO_philosopher_GD: in std_logic;
  signal PRO_philosopher_SEL: out integer;
  signal REG_stat_WR: out std_logic_vector(7 downto 0);
  signal REG_stat_WE: out std_logic;
  signal EVENT_ev_WAKEUP: out std_logic;
  signal EVENT_ev_GD: in std_logic;
  signal PRO_main_ENABLE: in std_logic;
  signal PRO_main_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end dining_main;
architecture main of dining_main is
  -- Local and temporary data objects
  signal LOOP_i_1: signed(3 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_main_start, -- PROCESS0[:0]
    S_BLOCKBOUND8_1, -- ASSIGN_BLOCK146547[dining.cp:85]
    S_i2_fun, -- FUN74680[dining.cp:86]
    S_BLOCKBOUND6_1, -- COUNT_LOOP_BLOCK254265[dining.cp:88]
    S_i4_for_loop_cond, -- COUNT_LOOP54265[dining.cp:88]
    S_i5_fun, -- FUN16346[dining.cp:90]
    S_i4_for_loop_incr, -- COUNT_LOOP54265[dining.cp:88]
    S_BLOCKBOUND3_1, -- ASSIGN_BLOCK338777[dining.cp:92]
    S_i7_fun, -- FUN91205[dining.cp:93]
    S_main_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_main_start;
  signal pro_state_next: pro_states := S_main_start;
  -- Auxilliary toplevel definitions
  constant CONST_I4_0: signed(3 downto 0) := "0000";
  constant CONST_I4_4: signed(3 downto 0) := "0100";
  signal LOOP_i_1_CONV_NAT4: integer;
  constant CONST_I4_1: signed(3 downto 0) := "0001";
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
          PRO_init_GD,
          LOOP_i_1,
          PRO_philosopher_GD,
          EVENT_ev_GD,
          pro_state
          )
  begin
    PRO_main_END <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        pro_state_next <= S_BLOCKBOUND8_1;
      when S_BLOCKBOUND8_1 => -- ASSIGN_BLOCK146547[dining.cp:85]
        pro_state_next <= S_i2_fun;
      when S_i2_fun => -- FUN74680[dining.cp:86]
        if PRO_init_GD = '1' then
          pro_state_next <= S_i2_fun;
        else
          pro_state_next <= S_BLOCKBOUND6_1;
        end if;
      when S_BLOCKBOUND6_1 => -- COUNT_LOOP_BLOCK254265[dining.cp:88]
        pro_state_next <= S_i4_for_loop_cond;
      when S_i4_for_loop_cond => -- COUNT_LOOP54265[dining.cp:88]
        if CONST_I4_4 >= LOOP_i_1 then
          pro_state_next <= S_i5_fun;
        else
          pro_state_next <= S_BLOCKBOUND3_1;
        end if;
      when S_i5_fun => -- FUN16346[dining.cp:90]
        if PRO_philosopher_GD = '1' then
          pro_state_next <= S_i5_fun;
        else
          pro_state_next <= S_i4_for_loop_incr;
        end if;
      when S_i4_for_loop_incr => -- COUNT_LOOP54265[dining.cp:88]
        pro_state_next <= S_i4_for_loop_cond;
      when S_BLOCKBOUND3_1 => -- ASSIGN_BLOCK338777[dining.cp:92]
        pro_state_next <= S_i7_fun;
      when S_i7_fun => -- FUN91205[dining.cp:93]
        if not((EVENT_ev_GD) = ('0')) then
          pro_state_next <= S_i7_fun;
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
          LOOP_i_1_CONV_NAT4,
          pro_state
          )
  begin
    -- Default values
    REG_stat_WR <= "00000000";
    REG_stat_WE <= '0';
    PRO_init_CALL <= '0';
    PRO_philosopher_START <= '0';
    PRO_philosopher_SEL <= 0;
    EVENT_ev_WAKEUP <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        null;
      when S_BLOCKBOUND8_1 => -- ASSIGN_BLOCK146547[dining.cp:85]
        REG_stat_WR <= "01001001";
        REG_stat_WE <= '1';
      when S_i2_fun => -- FUN74680[dining.cp:86]
        PRO_init_CALL <= '1';
      when S_BLOCKBOUND6_1 => -- COUNT_LOOP_BLOCK254265[dining.cp:88]
        REG_stat_WR <= "01010011";
        REG_stat_WE <= '1';
      when S_i4_for_loop_cond => -- COUNT_LOOP54265[dining.cp:88]
        null;
      when S_i5_fun => -- FUN16346[dining.cp:90]
        PRO_philosopher_START <= '1';
        PRO_philosopher_SEL <= LOOP_i_1_CONV_NAT4;
      when S_i4_for_loop_incr => -- COUNT_LOOP54265[dining.cp:88]
        null;
      when S_BLOCKBOUND3_1 => -- ASSIGN_BLOCK338777[dining.cp:92]
        REG_stat_WR <= "01010111";
        REG_stat_WE <= '1';
      when S_i7_fun => -- FUN91205[dining.cp:93]
        EVENT_ev_WAKEUP <= EVENT_ev_GD;
      when S_main_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          LOOP_i_1,
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        LOOP_i_1 <= to_signed(0,4);
      else
        case pro_state is
          when S_main_start => -- PROCESS0[:0]
            null;
          when S_BLOCKBOUND8_1 => -- ASSIGN_BLOCK146547[dining.cp:85]
            null;
          when S_i2_fun => -- FUN74680[dining.cp:86]
            null;
          when S_BLOCKBOUND6_1 => -- COUNT_LOOP_BLOCK254265[dining.cp:88]
            LOOP_i_1 <= CONST_I4_0;
          when S_i4_for_loop_cond => -- COUNT_LOOP54265[dining.cp:88]
            null;
          when S_i5_fun => -- FUN16346[dining.cp:90]
            null;
          when S_i4_for_loop_incr => -- COUNT_LOOP54265[dining.cp:88]
            LOOP_i_1 <= LOOP_i_1 + CONST_I4_1;
          when S_BLOCKBOUND3_1 => -- ASSIGN_BLOCK338777[dining.cp:92]
            null;
          when S_i7_fun => -- FUN91205[dining.cp:93]
            null;
          when S_main_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
  LOOP_i_1_CONV_NAT4 <= I_to_N((LOOP_i_1));
end main;
