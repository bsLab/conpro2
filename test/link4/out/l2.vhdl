--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2011 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D174 Genetic size: 2713135
--         Compile date: Fri Jan 21 09:49:21 CET 2011
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Implementation of Module <L2>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity MOD_l2 is
port(
  -- Connections to the outside world
  signal x_RD: out std_logic_vector(11 downto 0);
  signal xa_RD: out std_logic_vector(7 downto 0);
  signal DEV_ln_din: in std_logic_vector(7 downto 0);
  signal DEV_ln_din_ack: out std_logic;
  signal DEV_ln_dout: out std_logic_vector(7 downto 0);
  signal DEV_ln_dout_ack: in std_logic;
  signal CLK: in std_logic;
  signal RESET: in std_logic
);
end MOD_l2;
architecture main of MOD_l2 is
  -- Process instances
  component l2_p1
  port(
    -- Connections to external objects, components and the outside world
    signal REG_PRO_p1_EXCEPTION_RD: in signed(7 downto 0);
    signal REG_PRO_p1_EXCEPTION_WR: out signed(7 downto 0);
    signal REG_PRO_p1_EXCEPTION_WE: out std_logic;
    signal REG_x_WR: out signed(11 downto 0);
    signal REG_x_WE: out std_logic;
    signal LINK_ln_STOP: out std_logic;
    signal LINK_ln_RE: out std_logic;
    signal LINK_ln_RD: in std_logic_vector(11 downto 0);
    signal LINK_ln_RD_ERR: in std_logic;
    signal LINK_ln_WE: out std_logic;
    signal LINK_ln_WR: out std_logic_vector(11 downto 0);
    signal LINK_ln_WR_ERR: in std_logic;
    signal LINK_ln_GD: in std_logic;
    signal REG_xa_WR: out signed(7 downto 0);
    signal REG_xa_WE: out std_logic;
    signal PRO_p1_ENABLE: in std_logic;
    signal PRO_p1_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component l2_main
  port(
    -- Connections to external objects, components and the outside world
    signal LINK_ln_INIT: out std_logic;
    signal LINK_ln_START: out std_logic;
    signal LINK_ln_GD: in std_logic;
    signal PRO_p1_START: out std_logic;
    signal PRO_p1_GD: in std_logic;
    signal PRO_main_ENABLE: in std_logic;
    signal PRO_main_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  -- Local and temporary data objects
  signal REG_PRO_p1_EXCEPTION: signed(7 downto 0);
  signal REG_PRO_p1_EXCEPTION_RD: signed(7 downto 0);
  signal REG_PRO_p1_EXCEPTION_p1_WR: signed(7 downto 0);
  signal REG_PRO_p1_EXCEPTION_p1_WE: std_logic;
  signal REG_x: signed(11 downto 0);
  signal REG_x_p1_WR: signed(11 downto 0);
  signal REG_x_p1_WE: std_logic;
  signal DEV_ln_din_ack_WR: std_logic;
  signal PRO_main_ENABLE: std_logic;
  signal PRO_main_END: std_logic;
  signal DEV_ln_dout_WR: std_logic_vector(7 downto 0);
  type LINK_ln_STATES is (LINK_ln_S_EMPTY,
    LINK_ln_S_REQ,
    LINK_ln_S_SET);
  signal LINK_ln_main_INIT: std_logic;
  signal LINK_ln_main_START: std_logic;
  signal LINK_ln_p1_STOP: std_logic;
  signal LINK_ln_p1_RE: std_logic;
  signal LINK_ln_p1_RD: std_logic_vector(11 downto 0);
  signal LINK_ln_p1_RD_ERR: std_logic;
  signal LINK_ln_p1_WE: std_logic;
  signal LINK_ln_p1_WR: std_logic_vector(11 downto 0);
  signal LINK_ln_p1_WR_ERR: std_logic;
  signal LINK_ln_main_GD: std_logic;
  signal LINK_ln_main_LOCKed: std_logic;
  signal LINK_ln_p1_GD: std_logic;
  signal LINK_ln_p1_LOCKed: std_logic;
  signal LINK_ln_EN: std_logic;
  signal LINK_ln_RE: std_logic;
  signal LINK_ln_WE: std_logic;
  signal LINK_ln_AVAIL: std_logic;
  signal LINK_ln_BUSY: std_logic;
  signal LINK_ln_DIN_ACK: std_logic;
  signal LINK_ln_DIN_COMPL: std_logic;
  signal LINK_ln_DIN_EMPTY: std_logic;
  signal LINK_ln_DOUT_EMPTY: std_logic;
  signal LINK_ln_DOUT_ACK: std_logic;
  signal LINK_ln_DOUT_ACK_SAMPLED: std_logic;
  signal LINK_ln_DIN_REG: std_logic_vector(11 downto 0);
  signal LINK_ln_DIN: std_logic_vector(3 downto 0);
  signal LINK_ln_DIN_CNT: std_logic_vector(3 downto 0);
  signal LINK_ln_DIN2: std_logic_vector(7 downto 0);
  signal LINK_ln_DIN_SAMPLED: std_logic_vector(7 downto 0);
  signal LINK_ln_DOUT_REG: std_logic_vector(11 downto 0);
  signal LINK_ln_DOUT_SHIFT: std_logic_vector(11 downto 0);
  signal LINK_ln_DOUT_CNT: std_logic_vector(3 downto 0);
  signal LINK_ln_DOUT2: std_logic_vector(7 downto 0);
  signal LINK_ln_LOCKED: std_logic;
  signal LINK_ln_state_in: LINK_ln_STATES;
  signal LINK_ln_state_in_next: LINK_ln_STATES;
  signal LINK_ln_state_out: LINK_ln_STATES;
  signal LINK_ln_state_out_next: LINK_ln_STATES;
  signal PRO_p1_ENABLE: std_logic;
  signal PRO_p1_END: std_logic;
  signal PRO_p1_main_START: std_logic;
  signal PRO_p1_main_GD: std_logic;
  signal REG_xa: signed(7 downto 0);
  signal REG_xa_p1_WR: signed(7 downto 0);
  signal REG_xa_p1_WE: std_logic;
  signal DEV_ln_dout_ack_RD: std_logic;
  signal DEV_ln_din_RD: std_logic_vector(7 downto 0);
  -- State Processing
  -- Aux. signals
  signal conpro_system_clk: std_logic;
  signal conpro_system_reset: std_logic;
begin
  -- Module implementation
  -- Register
  IMPL_REG_PRO_p1_EXCEPTION: process(
          REG_PRO_p1_EXCEPTION_p1_WR,
          REG_PRO_p1_EXCEPTION_p1_WE,
          REG_PRO_p1_EXCEPTION,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_PRO_p1_EXCEPTION_RD <= REG_PRO_p1_EXCEPTION;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_PRO_p1_EXCEPTION <= to_signed(0,8);
     elsif REG_PRO_p1_EXCEPTION_p1_WE='1' then
      REG_PRO_p1_EXCEPTION <= REG_PRO_p1_EXCEPTION_p1_WR;
     end if;
    end if;
  end process IMPL_REG_PRO_p1_EXCEPTION;
  
  -- Register
  IMPL_REG_x: process(
          REG_x_p1_WR,
          REG_x_p1_WE,
          REG_x,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_x <= to_signed(0,12);
     elsif REG_x_p1_WE='1' then
      REG_x <= REG_x_p1_WR;
     end if;
    end if;
  end process IMPL_REG_x;
  
  -- Process control
  PRO_CONTROL_main: process(
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_main_ENABLE <= '1'; -- main process activated on reset
      end if;
    end if;
  end process PRO_CONTROL_main;
  
  --
  --  ConPro V2.1.D174 EMI Link4.link4 Vdevelop.4-3
  --
  
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_IN_PROC_S: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_EN,
    LINK_ln_state_in_next)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if ((conpro_system_reset) = ('1')) or ((LINK_ln_EN) = ('0')) then
        LINK_ln_state_in <= LINK_ln_S_EMPTY;
      else
        LINK_ln_state_in <= LINK_ln_state_in_next;
      end if;
    end if;
  end process LINK_ln_IN_PROC_S;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_IN_PROC_T: process(LINK_ln_state_in,
    LINK_ln_DIN_EMPTY,
    LINK_ln_DIN_CNT,
    LINK_ln_DIN_COMPL,
    LINK_ln_RE)
  begin
    case LINK_ln_state_in is
      when LINK_ln_S_EMPTY =>
        if (LINK_ln_DIN_EMPTY) = ('1') then
          if (LINK_ln_DIN_CNT) = ("0011") then
            LINK_ln_state_in_next <= LINK_ln_S_REQ;
          else
            LINK_ln_state_in_next <= LINK_ln_S_SET;
          end if;
        else
          LINK_ln_state_in_next <= LINK_ln_S_EMPTY;
        end if;
      when LINK_ln_S_SET =>
        if (LINK_ln_DIN_COMPL) = ('1') then
          LINK_ln_state_in_next <= LINK_ln_S_EMPTY;
        else
          LINK_ln_state_in_next <= LINK_ln_S_SET;
        end if;
      when LINK_ln_S_REQ =>
        if (LINK_ln_RE) = ('1') then
          LINK_ln_state_in_next <= LINK_ln_S_SET;
        else
          LINK_ln_state_in_next <= LINK_ln_S_REQ;
        end if;
    end case;
  end process LINK_ln_IN_PROC_T;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_IN_PROC_U: process(LINK_ln_state_in,
    LINK_ln_DIN_EMPTY,
    LINK_ln_DIN_COMPL)
  begin
    LINK_ln_DIN_ACK <= '0';
    LINK_ln_AVAIL <= '0';
    case LINK_ln_state_in is
      when LINK_ln_S_EMPTY =>
        LINK_ln_DIN_ACK <= '1';
        if (LINK_ln_DIN_EMPTY) = ('1') then
          LINK_ln_DIN_ACK <= '0';
        end if;
      when LINK_ln_S_SET =>
        if (LINK_ln_DIN_COMPL) = ('1') then
          LINK_ln_DIN_ACK <= '1';
        end if;
      when LINK_ln_S_REQ =>
        LINK_ln_AVAIL <= '1';
    end case;
  end process LINK_ln_IN_PROC_U;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_IN_PROC_V: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_EN,
    LINK_ln_state_in,
    LINK_ln_DIN_COMPL,
    LINK_ln_DIN,
    LINK_ln_DIN_REG,
    LINK_ln_DIN_CNT)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if ((conpro_system_reset) = ('1')) or ((LINK_ln_EN) = ('0')) then
        LINK_ln_DIN_REG <= "000000000000";
        LINK_ln_DIN_CNT <= "0000";
      else
        case LINK_ln_state_in is
          when LINK_ln_S_SET =>
            if (LINK_ln_DIN_COMPL) = ('1') then
              LINK_ln_DIN_REG(3 downto 0) <= LINK_ln_DIN;
              LINK_ln_DIN_REG(11 downto 4) <= LINK_ln_DIN_REG(7 downto 0);
              LINK_ln_DIN_CNT <= (LINK_ln_DIN_CNT) + (1);
            end if;
          when LINK_ln_S_REQ =>
            LINK_ln_DIN_CNT <= "0000";
          when others =>
            null;
        end case;
      end if;
    end if;
  end process LINK_ln_IN_PROC_V;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_OUT_PROC_S: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_EN,
    LINK_ln_state_out_next)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if ((conpro_system_reset) = ('1')) or ((LINK_ln_EN) = ('0')) then
        LINK_ln_state_out <= LINK_ln_S_EMPTY;
      else
        LINK_ln_state_out <= LINK_ln_state_out_next;
      end if;
    end if;
  end process LINK_ln_OUT_PROC_S;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_OUT_PROC_T: process(LINK_ln_state_out,
    LINK_ln_DOUT_ACK_SAMPLED,
    LINK_ln_DOUT_CNT,
    LINK_ln_WE)
  begin
    case LINK_ln_state_out is
      when LINK_ln_S_EMPTY =>
        if (LINK_ln_DOUT_ACK_SAMPLED) = ('0') then
          if (LINK_ln_DOUT_CNT) = ("0000") then
            LINK_ln_state_out_next <= LINK_ln_S_REQ;
          else
            LINK_ln_state_out_next <= LINK_ln_S_SET;
          end if;
        else
          LINK_ln_state_out_next <= LINK_ln_S_EMPTY;
        end if;
      when LINK_ln_S_REQ =>
        if (LINK_ln_WE) = ('1') then
          LINK_ln_state_out_next <= LINK_ln_S_SET;
        else
          LINK_ln_state_out_next <= LINK_ln_S_REQ;
        end if;
      when LINK_ln_S_SET =>
        if (LINK_ln_DOUT_ACK_SAMPLED) = ('1') then
          LINK_ln_state_out_next <= LINK_ln_S_EMPTY;
        else
          LINK_ln_state_out_next <= LINK_ln_S_SET;
        end if;
      when others =>
        LINK_ln_state_out_next <= LINK_ln_S_EMPTY;
    end case;
  end process LINK_ln_OUT_PROC_T;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_OUT_PROC_U: process(LINK_ln_state_out)
  begin
    LINK_ln_DOUT_EMPTY <= '0';
    case LINK_ln_state_out is
      when LINK_ln_S_EMPTY =>
        LINK_ln_DOUT_EMPTY <= '1';
      when LINK_ln_S_REQ =>
        LINK_ln_DOUT_EMPTY <= '1';
      when others =>
        null;
    end case;
  end process LINK_ln_OUT_PROC_U;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_OUT_PROC_V: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_EN,
    LINK_ln_state_out,
    LINK_ln_WE,
    LINK_ln_DOUT_REG,
    LINK_ln_DOUT_ACK_SAMPLED,
    LINK_ln_DOUT_CNT,
    LINK_ln_DOUT_SHIFT)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if ((conpro_system_reset) = ('1')) or ((LINK_ln_EN) = ('0')) then
        LINK_ln_DOUT_CNT <= "0000";
        LINK_ln_DOUT_SHIFT <= "000000000000";
        LINK_ln_BUSY <= '0';
      else
        case LINK_ln_state_out is
          when LINK_ln_S_REQ =>
            LINK_ln_BUSY <= '0';
            if (LINK_ln_WE) = ('1') then
              LINK_ln_BUSY <= '1';
              LINK_ln_DOUT_SHIFT <= LINK_ln_DOUT_REG;
            end if;
          when LINK_ln_S_SET =>
            if (LINK_ln_DOUT_ACK_SAMPLED) = ('1') then
              LINK_ln_DOUT_CNT <= (LINK_ln_DOUT_CNT) + (1);
              LINK_ln_DOUT_SHIFT(3 downto 0) <= "0000";
              LINK_ln_DOUT_SHIFT(11 downto 4) <= LINK_ln_DOUT_SHIFT(7 downto 0);
              if (LINK_ln_DOUT_CNT) = ("0010") then
                LINK_ln_DOUT_CNT <= "0000";
              end if;
            end if;
          when others =>
            null;
        end case;
      end if;
    end if;
  end process LINK_ln_OUT_PROC_V;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_SCHED: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_LOCKED,
    LINK_ln_main_INIT,
    LINK_ln_main_LOCKed,
    LINK_ln_p1_LOCKed,
    LINK_ln_main_START,
    LINK_ln_p1_STOP,
    LINK_ln_p1_RE,
    LINK_ln_AVAIL,
    LINK_ln_DIN_REG,
    LINK_ln_p1_WE,
    LINK_ln_BUSY,
    LINK_ln_p1_WR)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        LINK_ln_main_GD <= '1';
        LINK_ln_p1_GD <= '1';
        LINK_ln_main_LOCKed <= '0';
        LINK_ln_p1_LOCKed <= '0';
        LINK_ln_EN <= '0';
        LINK_ln_RE <= '0';
        LINK_ln_WE <= '0';
        LINK_ln_LOCKED <= '0';
        LINK_ln_DOUT_REG <= "000000000000";
      elsif (LINK_ln_LOCKED) = ('0') then
        if (LINK_ln_main_INIT) = ('1') then
          LINK_ln_main_GD <= '0';
          LINK_ln_LOCKED <= '1';
          if (LINK_ln_main_LOCKed) = ('1') then
            LINK_ln_main_LOCKed <= '0';
            LINK_ln_main_GD <= '0';
          end if;
          if (LINK_ln_p1_LOCKed) = ('1') then
            LINK_ln_p1_LOCKed <= '0';
            LINK_ln_p1_GD <= '0';
          end if;
        elsif (LINK_ln_main_START) = ('1') then
          LINK_ln_LOCKED <= '1';
          LINK_ln_main_GD <= '0';
          LINK_ln_EN <= '1';
        elsif (LINK_ln_p1_STOP) = ('1') then
          LINK_ln_LOCKED <= '1';
          LINK_ln_p1_GD <= '0';
          LINK_ln_EN <= '0';
        elsif (((LINK_ln_p1_RE) = ('1')) and ((LINK_ln_p1_LOCKed) = ('1'))) and ((LINK_ln_AVAIL) = ('1')) then
          LINK_ln_p1_RD <= LINK_ln_DIN_REG;
          LINK_ln_p1_RD_ERR <= '0';
          LINK_ln_p1_GD <= '0';
          LINK_ln_p1_LOCKed <= '0';
          LINK_ln_RE <= '1';
          LINK_ln_LOCKED <= '1';
        elsif (((LINK_ln_p1_WE) = ('1')) and ((LINK_ln_p1_LOCKed) = ('1'))) and ((LINK_ln_BUSY) = ('0')) then
          LINK_ln_p1_WR_ERR <= '0';
          LINK_ln_p1_GD <= '0';
          LINK_ln_p1_LOCKed <= '0';
          LINK_ln_LOCKED <= '1';
        elsif ((LINK_ln_p1_RE) = ('1')) and ((LINK_ln_p1_LOCKed) = ('0')) then
          if (LINK_ln_AVAIL) = ('1') then
            LINK_ln_p1_RD <= LINK_ln_DIN_REG;
            LINK_ln_p1_RD_ERR <= '0';
            LINK_ln_p1_GD <= '0';
            LINK_ln_RE <= '1';
            LINK_ln_LOCKED <= '1';
          else
            LINK_ln_p1_LOCKed <= '1';
          end if;
        elsif (((LINK_ln_p1_WE) = ('1')) and ((LINK_ln_p1_LOCKed) = ('0'))) and ((LINK_ln_BUSY) = ('0')) then
          LINK_ln_DOUT_REG <= LINK_ln_p1_WR;
          LINK_ln_WE <= '1';
          LINK_ln_LOCKED <= '1';
          LINK_ln_p1_LOCKed <= '1';
        end if;
      else
        LINK_ln_main_GD <= '1';
        LINK_ln_p1_GD <= '1';
        LINK_ln_RE <= '0';
        LINK_ln_WE <= '0';
        LINK_ln_LOCKED <= '0';
      end if;
    end if;
  end process LINK_ln_SCHED;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_SAMPLER1: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_DIN2)
    variable samples1: std_logic_vector(7 downto 0);
    variable samples2: std_logic_vector(7 downto 0);
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        samples1 := "00000000";
        samples2 := "00000000";
        LINK_ln_DIN_SAMPLED <= "00000000";
      else
        samples2 := samples1;
        samples1 := LINK_ln_DIN2;
      end if;
      if (samples1) = (samples2) then
        LINK_ln_DIN_SAMPLED <= samples1;
      end if;
    end if;
  end process LINK_ln_SAMPLER1;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_SAMPLER2: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_DOUT_ACK)
    variable samples: std_logic_vector(1 downto 0);
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        samples := "00";
        LINK_ln_DOUT_ACK_SAMPLED <= '0';
      else
        samples(1) := samples(0);
        samples(0) := LINK_ln_DOUT_ACK;
      end if;
      if (samples) = ("00") then
        LINK_ln_DOUT_ACK_SAMPLED <= '0';
      end if;
      if (samples) = ("11") then
        LINK_ln_DOUT_ACK_SAMPLED <= '1';
      end if;
    end if;
  end process LINK_ln_SAMPLER2;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_DECODER: process(LINK_ln_DIN_SAMPLED)
  begin
    LINK_ln_DIN_COMPL <= ((((LINK_ln_DIN_SAMPLED(0)) xor (LINK_ln_DIN_SAMPLED(1))) and ((LINK_ln_DIN_SAMPLED(2)) xor (LINK_ln_DIN_SAMPLED(3)))) and ((LINK_ln_DIN_SAMPLED(4)) xor (LINK_ln_DIN_SAMPLED(5)))) and ((LINK_ln_DIN_SAMPLED(6)) xor (LINK_ln_DIN_SAMPLED(7)));
    LINK_ln_DIN_EMPTY <= (((((((not (LINK_ln_DIN_SAMPLED(0))) and (not (LINK_ln_DIN_SAMPLED(1)))) and (not (LINK_ln_DIN_SAMPLED(2)))) and (not (LINK_ln_DIN_SAMPLED(3)))) and (not (LINK_ln_DIN_SAMPLED(4)))) and (not (LINK_ln_DIN_SAMPLED(5)))) and (not (LINK_ln_DIN_SAMPLED(6)))) and (not (LINK_ln_DIN_SAMPLED(7)));
    LINK_ln_DIN(0) <= LINK_ln_DIN_SAMPLED(0);
    LINK_ln_DIN(1) <= LINK_ln_DIN_SAMPLED(2);
    LINK_ln_DIN(2) <= LINK_ln_DIN_SAMPLED(4);
    LINK_ln_DIN(3) <= LINK_ln_DIN_SAMPLED(6);
  end process LINK_ln_DECODER;
  --
  -- EMI <Object Link4.link4.ln> Process
  --
  LINK_ln_ENCODER: process(LINK_ln_DOUT_EMPTY,
    LINK_ln_DOUT_SHIFT)
  begin
    if (LINK_ln_DOUT_EMPTY) = ('1') then
      LINK_ln_DOUT2 <= "00000000";
    else
      LINK_ln_DOUT2(0) <= LINK_ln_DOUT_SHIFT(8);
      LINK_ln_DOUT2(1) <= not (LINK_ln_DOUT_SHIFT(8));
      LINK_ln_DOUT2(2) <= LINK_ln_DOUT_SHIFT(9);
      LINK_ln_DOUT2(3) <= not (LINK_ln_DOUT_SHIFT(9));
      LINK_ln_DOUT2(4) <= LINK_ln_DOUT_SHIFT(10);
      LINK_ln_DOUT2(5) <= not (LINK_ln_DOUT_SHIFT(10));
      LINK_ln_DOUT2(6) <= LINK_ln_DOUT_SHIFT(11);
      LINK_ln_DOUT2(7) <= not (LINK_ln_DOUT_SHIFT(11));
    end if;
  end process LINK_ln_ENCODER;
  --
  -- EMI <Object Link4.link4.ln>
  --
  LINK_ln_DIN2 <= DEV_ln_din_RD;
  DEV_ln_dout_WR <= LINK_ln_DOUT2;
  DEV_ln_din_ack_WR <= LINK_ln_DIN_ACK;
  LINK_ln_DOUT_ACK <= DEV_ln_dout_ack_RD;
  --
  -- EMI <Object Link4.link4.ln>
  --
  select_WR <= LINK_ln_AVAIL;
  --
  -- End of <Object Link4.link4.ln>
  --
  
  -- Process control
  PRO_CONTROL_p1: process(
          PRO_p1_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_p1_ENABLE <= '0';
        PRO_p1_main_GD <= '1';
      elsif PRO_p1_main_START = '1' then
        PRO_p1_ENABLE <= '1';
        PRO_p1_main_GD <= '0';
      else
        PRO_p1_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_p1;
  
  -- Register
  IMPL_REG_xa: process(
          REG_xa_p1_WR,
          REG_xa_p1_WE,
          REG_xa,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_xa <= to_signed(0,8);
     elsif REG_xa_p1_WE='1' then
      REG_xa <= REG_xa_p1_WR;
     end if;
    end if;
  end process IMPL_REG_xa;
  
  
  -- Process instantiations
  PRO_MAP_p1: l2_p1 port map(
    REG_PRO_p1_EXCEPTION_RD => REG_PRO_p1_EXCEPTION_RD,
    REG_PRO_p1_EXCEPTION_WR => REG_PRO_p1_EXCEPTION_p1_WR,
    REG_PRO_p1_EXCEPTION_WE => REG_PRO_p1_EXCEPTION_p1_WE,
    REG_x_WR => REG_x_p1_WR,
    REG_x_WE => REG_x_p1_WE,
    LINK_ln_STOP => LINK_ln_p1_STOP,
    LINK_ln_RE => LINK_ln_p1_RE,
    LINK_ln_RD => LINK_ln_p1_RD,
    LINK_ln_RD_ERR => LINK_ln_p1_RD_ERR,
    LINK_ln_WE => LINK_ln_p1_WE,
    LINK_ln_WR => LINK_ln_p1_WR,
    LINK_ln_WR_ERR => LINK_ln_p1_WR_ERR,
    LINK_ln_GD => LINK_ln_p1_GD,
    REG_xa_WR => REG_xa_p1_WR,
    REG_xa_WE => REG_xa_p1_WE,
    PRO_p1_ENABLE => PRO_p1_ENABLE,
    PRO_p1_END => PRO_p1_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_main: l2_main port map(
    LINK_ln_INIT => LINK_ln_main_INIT,
    LINK_ln_START => LINK_ln_main_START,
    LINK_ln_GD => LINK_ln_main_GD,
    PRO_p1_START => PRO_p1_main_START,
    PRO_p1_GD => PRO_p1_main_GD,
    PRO_main_ENABLE => PRO_main_ENABLE,
    PRO_main_END => PRO_main_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  
  -- Toplevel assignments
  -- Monitors
  x_RD <= std_logic_vector(REG_x);
  xa_RD <= std_logic_vector(REG_xa);
  DEV_ln_din_RD <= DEV_ln_din;
  DEV_ln_din_ack <= DEV_ln_din_ack_WR;
  DEV_ln_dout <= DEV_ln_dout_WR;
  DEV_ln_dout_ack_RD <= DEV_ln_dout_ack;
  conpro_system_clk <= CLK;
  conpro_system_reset <= RESET;
end main;
