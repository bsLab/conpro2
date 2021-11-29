--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 A16 **  2049142 total genes
--         Compiled on Thu Nov 27 11:07:26 CET 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

-- Process implementation of process <main> from module <E>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity e_main is
port(
  -- Connections to external objects, components and the outside world
  signal EVENT_e_INIT: out std_logic;
  signal EVENT_e_WAKEUP: out std_logic;
  signal EVENT_e_GD: in std_logic;
  signal PRO_p_START: out std_logic;
  signal PRO_p_GD: in std_logic;
  signal PRO_p_SEL: out integer;
  signal PRO_main_ENABLE: in std_logic;
  signal PRO_main_END: out std_logic;
  signal conpro_system_clk: in std_logic;
  signal conpro_system_reset: in std_logic
);
end e_main;
architecture main of e_main is
  -- Local and temporary data objects
  signal TEMP_0: std_logic_vector(5 downto 0);
  signal LOOP_i_4: signed(3 downto 0);
  signal LOOP_i_5: signed(3 downto 0);
  -- Auxilliary ALU signals
  -- State Processing
  type pro_states is (
    S_main_start, -- PROCESS0[:0]
    S_i1_fun, -- FUN58065[e.cp:27]
    S_i2_for_loop, -- COUNT_LOOP98648[e.cp:29]
    S_i2_for_loop_cond, -- COUNT_LOOP98648[e.cp:29]
    S_i3_fun, -- FUN70081[e.cp:30]
    S_i2_for_loop_incr, -- COUNT_LOOP98648[e.cp:29]
    S_i4_for_loop, -- COUNT_LOOP5427[e.cp:31]
    S_i4_for_loop_cond, -- COUNT_LOOP5427[e.cp:31]
    S_i5_delay, -- COND_LOOP63574[e.cp:33]
    S_i5_delay_loop, -- COND_LOOP63574[e.cp:33]
    S_i6_fun, -- FUN45461[e.cp:34]
    S_i4_for_loop_incr, -- COUNT_LOOP5427[e.cp:31]
    S_main_end -- PROCESS0[:0]
    );
  signal pro_state: pro_states := S_main_start;
  signal pro_state_next: pro_states := S_main_start;
  -- Auxilliary toplevel definitions
  constant CONST_I4_0: signed(3 downto 0) := "0000";
  constant CONST_I4_3: signed(3 downto 0) := "0011";
  signal LOOP_i_4_CONV_NAT4: integer;
  constant CONST_I4_1: signed(3 downto 0) := "0001";
  constant CONST_I4_5: signed(3 downto 0) := "0101";
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
          EVENT_e_GD,
          LOOP_i_4,
          PRO_p_GD,
          LOOP_i_5,
          TEMP_0,
          pro_state
          )
  begin
    PRO_main_END <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_fun;
      when S_i1_fun => -- FUN58065[e.cp:27]
        if not((EVENT_e_GD) = ('0')) then
          pro_state_next <= S_i1_fun;
        else
          pro_state_next <= S_i2_for_loop;
        end if;
      when S_i2_for_loop => -- COUNT_LOOP98648[e.cp:29]
        pro_state_next <= S_i2_for_loop_cond;
      when S_i2_for_loop_cond => -- COUNT_LOOP98648[e.cp:29]
        if CONST_I4_3 >= LOOP_i_4 then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_i4_for_loop;
        end if;
      when S_i3_fun => -- FUN70081[e.cp:30]
        if PRO_p_GD = '1' then
          pro_state_next <= S_i3_fun;
        else
          pro_state_next <= S_i2_for_loop_incr;
        end if;
      when S_i2_for_loop_incr => -- COUNT_LOOP98648[e.cp:29]
        pro_state_next <= S_i2_for_loop_cond;
      when S_i4_for_loop => -- COUNT_LOOP5427[e.cp:31]
        pro_state_next <= S_i4_for_loop_cond;
      when S_i4_for_loop_cond => -- COUNT_LOOP5427[e.cp:31]
        if CONST_I4_5 >= LOOP_i_5 then
          pro_state_next <= S_i5_delay;
        else
          pro_state_next <= S_main_end;
        end if;
      when S_i5_delay => -- COND_LOOP63574[e.cp:33]
        pro_state_next <= S_i5_delay_loop;
      when S_i5_delay_loop => -- COND_LOOP63574[e.cp:33]
        if TEMP_0 = "000000" then
          pro_state_next <= S_i6_fun;
        else
          pro_state_next <= S_i5_delay_loop;
        end if;
      when S_i6_fun => -- FUN45461[e.cp:34]
        if not((EVENT_e_GD) = ('0')) then
          pro_state_next <= S_i6_fun;
        else
          pro_state_next <= S_i4_for_loop_incr;
        end if;
      when S_i4_for_loop_incr => -- COUNT_LOOP5427[e.cp:31]
        pro_state_next <= S_i4_for_loop_cond;
      when S_main_end => -- PROCESS0[:0]
        pro_state_next <= S_main_end;
        PRO_main_END <= '1';
    end case;
  end process control_path;
  
  -- Instruction Datapath Combinational Unit
  data_path: process(
          LOOP_i_4_CONV_NAT4,
          LOOP_i_4,
          TEMP_0,
          LOOP_i_5,
          pro_state
          )
  begin
    -- Default values
    EVENT_e_INIT <= '0';
    PRO_p_START <= '0';
    PRO_p_SEL <= 0;
    EVENT_e_WAKEUP <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        null;
      when S_i1_fun => -- FUN58065[e.cp:27]
        EVENT_e_INIT <= '1';
      when S_i2_for_loop => -- COUNT_LOOP98648[e.cp:29]
        null;
      when S_i2_for_loop_cond => -- COUNT_LOOP98648[e.cp:29]
        null;
      when S_i3_fun => -- FUN70081[e.cp:30]
        PRO_p_START <= '1';
        PRO_p_SEL <= LOOP_i_4_CONV_NAT4;
      when S_i2_for_loop_incr => -- COUNT_LOOP98648[e.cp:29]
        null;
      when S_i4_for_loop => -- COUNT_LOOP5427[e.cp:31]
        null;
      when S_i4_for_loop_cond => -- COUNT_LOOP5427[e.cp:31]
        null;
      when S_i5_delay => -- COND_LOOP63574[e.cp:33]
        null;
      when S_i5_delay_loop => -- COND_LOOP63574[e.cp:33]
        null;
      when S_i6_fun => -- FUN45461[e.cp:34]
        EVENT_e_WAKEUP <= '1';
      when S_i4_for_loop_incr => -- COUNT_LOOP5427[e.cp:31]
        null;
      when S_main_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Instruction Datapath Transitional Unit
  data_trans: process(
          conpro_system_clk,
          conpro_system_reset,
          pro_state
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
      if conpro_system_reset = '1' then
        LOOP_i_4 <= to_signed(0,4);
        LOOP_i_5 <= to_signed(0,4);
        TEMP_0 <= "000000";
      else
        case pro_state is
          when S_main_start => -- PROCESS0[:0]
            null;
          when S_i1_fun => -- FUN58065[e.cp:27]
            null;
          when S_i2_for_loop => -- COUNT_LOOP98648[e.cp:29]
            LOOP_i_4 <= CONST_I4_0;
          when S_i2_for_loop_cond => -- COUNT_LOOP98648[e.cp:29]
            null;
          when S_i3_fun => -- FUN70081[e.cp:30]
            null;
          when S_i2_for_loop_incr => -- COUNT_LOOP98648[e.cp:29]
            LOOP_i_4 <= LOOP_i_4 + CONST_I4_1;
          when S_i4_for_loop => -- COUNT_LOOP5427[e.cp:31]
            LOOP_i_5 <= CONST_I4_1;
          when S_i4_for_loop_cond => -- COUNT_LOOP5427[e.cp:31]
            null;
          when S_i5_delay => -- COND_LOOP63574[e.cp:33]
            TEMP_0 <= "010010";
          when S_i5_delay_loop => -- COND_LOOP63574[e.cp:33]
            TEMP_0 <= TEMP_0 - "000001";
          when S_i6_fun => -- FUN45461[e.cp:34]
            null;
          when S_i4_for_loop_incr => -- COUNT_LOOP5427[e.cp:31]
            LOOP_i_5 <= LOOP_i_5 + CONST_I4_1;
          when S_main_end => -- PROCESS0[:0]
            null;
        end case;
      end if;
    end if;
  end process data_trans;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
  LOOP_i_4_CONV_NAT4 <= to_integer(unsigned(LOOP_i_4));
end main;