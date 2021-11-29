--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D114-M15 Genetic size: 2388587
--         Compile date: Wed Oct 21 14:09:57 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

-- Implementation of Module <Com>.
--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.ConPRO.all;
entity MOD_com is
port(
  -- Connections to the outside world
  signal DEV_data_in: in std_logic_vector(31 downto 0);
  signal DEV_data_in_ack: out std_logic;
  signal DEV_data_out: out std_logic_vector(31 downto 0);
  signal DEV_data_out_ack: in std_logic;
  signal DEV_data: out std_logic_vector(15 downto 0);
  signal DEV_data_en: out std_logic;
  signal CLK: in std_logic;
  signal RESET: in std_logic
);
end MOD_com;
architecture main of MOD_com is
  -- Process instances
  component com_send
  port(
    -- Connections to external objects, components and the outside world
    signal REG_PRO_send_EXCEPTION_RD: in signed(7 downto 0);
    signal REG_PRO_send_EXCEPTION_WR: out signed(7 downto 0);
    signal REG_PRO_send_EXCEPTION_WE: out std_logic;
    signal RND_rnd_RE: out std_logic;
    signal RND_rnd_RD: in std_logic_vector(7 downto 0);
    signal RND_rnd_GD: in std_logic;
    signal LINK_ln_STOP: out std_logic;
    signal LINK_ln_WE: out std_logic;
    signal LINK_ln_WR: out std_logic_vector(15 downto 0);
    signal LINK_ln_WR_ERR: in std_logic;
    signal LINK_ln_GD: in std_logic;
    signal PRO_send_ENABLE: in std_logic;
    signal PRO_send_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component com_recv
  port(
    -- Connections to external objects, components and the outside world
    signal REG_PRO_recv_EXCEPTION_RD: in signed(7 downto 0);
    signal REG_PRO_recv_EXCEPTION_WR: out signed(7 downto 0);
    signal REG_PRO_recv_EXCEPTION_WE: out std_logic;
    signal DEV_data_WR: out std_logic_vector(15 downto 0);
    signal LINK_ln_STOP: out std_logic;
    signal LINK_ln_RE: out std_logic;
    signal LINK_ln_RD: in std_logic_vector(15 downto 0);
    signal LINK_ln_RD_ERR: in std_logic;
    signal LINK_ln_GD: in std_logic;
    signal DEV_data_en_WR: out std_logic;
    signal PRO_recv_ENABLE: in std_logic;
    signal PRO_recv_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  component com_main
  port(
    -- Connections to external objects, components and the outside world
    signal PRO_send_START: out std_logic;
    signal PRO_send_GD: in std_logic;
    signal RND_rnd_INIT: out std_logic;
    signal RND_rnd_GD: in std_logic;
    signal LINK_ln_INIT: out std_logic;
    signal LINK_ln_START: out std_logic;
    signal LINK_ln_GD: in std_logic;
    signal PRO_recv_START: out std_logic;
    signal PRO_recv_GD: in std_logic;
    signal PRO_main_ENABLE: in std_logic;
    signal PRO_main_END: out std_logic;
    signal conpro_system_clk: in std_logic;
    signal conpro_system_reset: in std_logic
  );
  end component;
  -- Local and temporary data objects
  signal REG_PRO_send_EXCEPTION: signed(7 downto 0);
  signal REG_PRO_send_EXCEPTION_RD: signed(7 downto 0);
  signal REG_PRO_send_EXCEPTION_send_WR: signed(7 downto 0);
  signal REG_PRO_send_EXCEPTION_send_WE: std_logic;
  signal REG_PRO_recv_EXCEPTION: signed(7 downto 0);
  signal REG_PRO_recv_EXCEPTION_RD: signed(7 downto 0);
  signal REG_PRO_recv_EXCEPTION_recv_WR: signed(7 downto 0);
  signal REG_PRO_recv_EXCEPTION_recv_WE: std_logic;
  signal DEV_data_out_ack_RD: std_logic;
  signal PRO_send_ENABLE: std_logic;
  signal PRO_send_END: std_logic;
  signal PRO_send_main_START: std_logic;
  signal PRO_send_main_GD: std_logic;
  signal DEV_data_out_WR: std_logic_vector(31 downto 0);
  signal RND_rnd_send_RD: std_logic_vector(7 downto 0);
  signal RND_rnd_d_in: std_logic;
  signal RND_rnd_data_shift: std_logic_vector(7 downto 0);
  signal RND_rnd_data: std_logic_vector(7 downto 0);
  signal RND_rnd_shift: std_logic;
  signal RND_rnd_init: std_logic;
  signal RND_rnd_avail: std_logic;
  signal RND_rnd_send_RE: std_logic;
  signal RND_rnd_main_INIT: std_logic;
  signal RND_rnd_main_GD: std_logic;
  signal RND_rnd_send_GD: std_logic;
  signal RND_rnd_count: std_logic_vector(2 downto 0);
  signal PRO_main_ENABLE: std_logic;
  signal PRO_main_END: std_logic;
  signal DEV_data_WR: std_logic_vector(15 downto 0);
  type LINK_ln_STATES is (LINK_ln_S_RESET,
    LINK_ln_S_REQ,
    LINK_ln_S_SET);
  signal LINK_ln_main_INIT: std_logic;
  signal LINK_ln_main_START: std_logic;
  signal LINK_ln_recv_STOP: std_logic;
  signal LINK_ln_send_STOP: std_logic;
  signal LINK_ln_recv_RE: std_logic;
  signal LINK_ln_recv_RD: std_logic_vector(15 downto 0);
  signal LINK_ln_recv_RD_ERR: std_logic;
  signal LINK_ln_send_WE: std_logic;
  signal LINK_ln_send_WR: std_logic_vector(15 downto 0);
  signal LINK_ln_send_WR_ERR: std_logic;
  signal LINK_ln_main_GD: std_logic;
  signal LINK_ln_main_LOCKed: std_logic;
  signal LINK_ln_recv_GD: std_logic;
  signal LINK_ln_recv_LOCKed: std_logic;
  signal LINK_ln_send_GD: std_logic;
  signal LINK_ln_send_LOCKed: std_logic;
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
  signal LINK_ln_DIN_REG: std_logic_vector(15 downto 0);
  signal LINK_ln_DIN: std_logic_vector(15 downto 0);
  signal LINK_ln_DIN2: std_logic_vector(31 downto 0);
  signal LINK_ln_DOUT_REG: std_logic_vector(15 downto 0);
  signal LINK_ln_DOUT2: std_logic_vector(31 downto 0);
  signal LINK_ln_LOCKED: std_logic;
  signal LINK_ln_state_in: LINK_ln_STATES;
  signal LINK_ln_state_out: LINK_ln_STATES;
  signal DEV_data_in_ack_WR: std_logic;
  signal DEV_data_in_RD: std_logic_vector(31 downto 0);
  signal PRO_recv_ENABLE: std_logic;
  signal PRO_recv_END: std_logic;
  signal PRO_recv_main_START: std_logic;
  signal PRO_recv_main_GD: std_logic;
  signal DEV_data_en_WR: std_logic;
  -- State Processing
  -- Aux. signals
  signal conpro_system_clk: std_logic;
  signal conpro_system_reset: std_logic;
begin
  -- Module implementation
  -- Register
  IMPL_REG_PRO_send_EXCEPTION: process(
          REG_PRO_send_EXCEPTION_send_WR,
          REG_PRO_send_EXCEPTION_send_WE,
          REG_PRO_send_EXCEPTION,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_PRO_send_EXCEPTION_RD <= REG_PRO_send_EXCEPTION;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_PRO_send_EXCEPTION <= to_signed(0,8);
     elsif REG_PRO_send_EXCEPTION_send_WE='1' then
      REG_PRO_send_EXCEPTION <= REG_PRO_send_EXCEPTION_send_WR;
     end if;
    end if;
  end process IMPL_REG_PRO_send_EXCEPTION;
  
  -- Register
  IMPL_REG_PRO_recv_EXCEPTION: process(
          REG_PRO_recv_EXCEPTION_recv_WR,
          REG_PRO_recv_EXCEPTION_recv_WE,
          REG_PRO_recv_EXCEPTION,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    REG_PRO_recv_EXCEPTION_RD <= REG_PRO_recv_EXCEPTION;
    if conpro_system_clk'event and conpro_system_clk='1' then
     if conpro_system_reset='1' then
      REG_PRO_recv_EXCEPTION <= to_signed(0,8);
     elsif REG_PRO_recv_EXCEPTION_recv_WE='1' then
      REG_PRO_recv_EXCEPTION <= REG_PRO_recv_EXCEPTION_recv_WR;
     end if;
    end if;
  end process IMPL_REG_PRO_recv_EXCEPTION;
  
  -- Process control
  PRO_CONTROL_send: process(
          PRO_send_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_send_ENABLE <= '0';
        PRO_send_main_GD <= '1';
      elsif PRO_send_main_START = '1' then
        PRO_send_ENABLE <= '1';
        PRO_send_main_GD <= '0';
      else
        PRO_send_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_send;
  
  --
  --  ConPro V2.1.D114-M15 EMI Random.random V2.07
  --
  
  --
  -- EMI <Object Random.random.rnd> Process
  --
  RANDOM_rnd_SCHED: process(conpro_system_clk,
    conpro_system_reset,
    RND_rnd_init,
    RND_rnd_main_INIT,
    RND_rnd_send_RE,
    RND_rnd_avail,
    RND_rnd_data)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        RND_rnd_shift <= '0';
        RND_rnd_init <= '0';
        RND_rnd_main_GD <= '1';
        RND_rnd_send_GD <= '1';
      else
        RND_rnd_main_GD <= '1';
        RND_rnd_send_GD <= '1';
        if (RND_rnd_init) = ('1') then
          RND_rnd_init <= '0';
        elsif (RND_rnd_main_INIT) = ('1') then
          RND_rnd_init <= '1';
          RND_rnd_main_GD <= '0';
        elsif (RND_rnd_send_RE) = ('1') then
          RND_rnd_shift <= '1';
          if (RND_rnd_avail) = ('1') then
            RND_rnd_send_RD <= RND_rnd_data;
            RND_rnd_send_GD <= '0';
            RND_rnd_shift <= '0';
          end if;
        end if;
      end if;
    end if;
  end process RANDOM_rnd_SCHED;
  --
  -- EMI <Object Random.random.rnd> Process
  --
  RANDOM_rnd: process(conpro_system_clk,
    conpro_system_reset,
    RND_rnd_init,
    RND_rnd_shift,
    RND_rnd_d_in,
    RND_rnd_data_shift,
    RND_rnd_count)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if ((conpro_system_reset) = ('1')) or ((RND_rnd_init) = ('1')) then
        RND_rnd_data_shift <= "11111111";
        RND_rnd_data <= "00000000";
        RND_rnd_count <= "000";
        RND_rnd_avail <= '0';
      elsif (RND_rnd_shift) = ('1') then
        RND_rnd_data_shift <= (RND_rnd_d_in) & (RND_rnd_data_shift(7 downto 1));
        RND_rnd_count <= (RND_rnd_count) + (1);
        if (RND_rnd_count) = ("111") then
          RND_rnd_avail <= '1';
          RND_rnd_data <= RND_rnd_data_shift;
          RND_rnd_count <= "000";
        else
          RND_rnd_avail <= '0';
        end if;
      end if;
    end if;
  end process RANDOM_rnd;
  --
  -- EMI <Object Random.random.rnd>
  --
  RND_rnd_d_in <= (RND_rnd_data_shift(0)) xor ((RND_rnd_data_shift(4)) xor ((RND_rnd_data_shift(5)) xor (RND_rnd_data_shift(7))));
  --
  -- End of <Object Random.random.rnd>
  --
  
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
  --  ConPro V2.1.D114-M15 EMI Link.link V2.2
  --
  
  --
  -- EMI <Object Link.link.ln> Process
  --
  LINK_ln_IN_PROC: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_EN,
    LINK_ln_state_in,
    LINK_ln_DIN_EMPTY,
    LINK_ln_DIN_COMPL,
    LINK_ln_DIN,
    LINK_ln_RE)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if ((conpro_system_reset) = ('1')) or ((LINK_ln_EN) = ('0')) then
        LINK_ln_state_in <= LINK_ln_S_RESET;
        LINK_ln_DIN_REG <= "0000000000000000";
        LINK_ln_DIN_ACK <= '0';
        LINK_ln_AVAIL <= '0';
      else
        case LINK_ln_state_in is
          when LINK_ln_S_RESET =>
            if (LINK_ln_DIN_EMPTY) = ('1') then
              LINK_ln_state_in <= LINK_ln_S_SET;
              LINK_ln_DIN_ACK <= '0';
            end if;
          when LINK_ln_S_SET =>
            if (LINK_ln_DIN_COMPL) = ('1') then
              LINK_ln_state_in <= LINK_ln_S_REQ;
              LINK_ln_DIN_REG <= LINK_ln_DIN;
              LINK_ln_AVAIL <= '1';
              LINK_ln_DIN_ACK <= '1';
            end if;
          when LINK_ln_S_REQ =>
            if (LINK_ln_RE) = ('1') then
              LINK_ln_AVAIL <= '0';
              LINK_ln_state_in <= LINK_ln_S_RESET;
            end if;
        end case;
      end if;
    end if;
  end process LINK_ln_IN_PROC;
  --
  -- EMI <Object Link.link.ln> Process
  --
  LINK_ln_OUT_PROC: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_EN,
    LINK_ln_state_out,
    LINK_ln_DOUT_ACK,
    LINK_ln_WE)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if ((conpro_system_reset) = ('1')) or ((LINK_ln_EN) = ('0')) then
        LINK_ln_state_out <= LINK_ln_S_RESET;
        LINK_ln_BUSY <= '0';
        LINK_ln_DOUT_EMPTY <= '0';
      else
        LINK_ln_BUSY <= '1';
        case LINK_ln_state_out is
          when LINK_ln_S_RESET =>
            LINK_ln_DOUT_EMPTY <= '1';
            if (LINK_ln_DOUT_ACK) = ('0') then
              LINK_ln_state_out <= LINK_ln_S_REQ;
            end if;
          when LINK_ln_S_REQ =>
            LINK_ln_BUSY <= '0';
            if (LINK_ln_WE) = ('1') then
              LINK_ln_DOUT_EMPTY <= '0';
              LINK_ln_BUSY <= '1';
              LINK_ln_state_out <= LINK_ln_S_SET;
            end if;
          when LINK_ln_S_SET =>
            if (LINK_ln_DOUT_ACK) = ('1') then
              LINK_ln_state_out <= LINK_ln_S_RESET;
              LINK_ln_BUSY <= '0';
            end if;
        end case;
      end if;
    end if;
  end process LINK_ln_OUT_PROC;
  --
  -- EMI <Object Link.link.ln> Process
  --
  LINK_ln_SCHED: process(conpro_system_clk,
    conpro_system_reset,
    LINK_ln_LOCKED,
    LINK_ln_main_INIT,
    LINK_ln_main_LOCKed,
    LINK_ln_recv_LOCKed,
    LINK_ln_send_LOCKed,
    LINK_ln_main_START,
    LINK_ln_recv_STOP,
    LINK_ln_send_STOP,
    LINK_ln_recv_RE,
    LINK_ln_AVAIL,
    LINK_ln_DIN_REG,
    LINK_ln_send_WE,
    LINK_ln_BUSY,
    LINK_ln_send_WR)
  begin
    if (conpro_system_clk'event) and ((conpro_system_clk) = ('1')) then
      if (conpro_system_reset) = ('1') then
        LINK_ln_main_GD <= '1';
        LINK_ln_recv_GD <= '1';
        LINK_ln_send_GD <= '1';
        LINK_ln_main_LOCKed <= '0';
        LINK_ln_recv_LOCKed <= '0';
        LINK_ln_send_LOCKed <= '0';
        LINK_ln_EN <= '0';
        LINK_ln_RE <= '0';
        LINK_ln_WE <= '0';
        LINK_ln_LOCKED <= '0';
      elsif (LINK_ln_LOCKED) = ('0') then
        if (LINK_ln_main_INIT) = ('1') then
          LINK_ln_main_GD <= '0';
          LINK_ln_LOCKED <= '1';
          if (LINK_ln_main_LOCKed) = ('1') then
            LINK_ln_main_LOCKed <= '0';
            LINK_ln_main_GD <= '0';
          end if;
          if (LINK_ln_recv_LOCKed) = ('1') then
            LINK_ln_recv_LOCKed <= '0';
            LINK_ln_recv_GD <= '0';
          end if;
          if (LINK_ln_send_LOCKed) = ('1') then
            LINK_ln_send_LOCKed <= '0';
            LINK_ln_send_GD <= '0';
          end if;
        elsif (LINK_ln_main_START) = ('1') then
          LINK_ln_LOCKED <= '1';
          LINK_ln_main_GD <= '0';
          LINK_ln_EN <= '1';
        elsif (LINK_ln_recv_STOP) = ('1') then
          LINK_ln_LOCKED <= '1';
          LINK_ln_recv_GD <= '0';
          LINK_ln_EN <= '0';
        elsif (LINK_ln_send_STOP) = ('1') then
          LINK_ln_LOCKED <= '1';
          LINK_ln_send_GD <= '0';
          LINK_ln_EN <= '0';
        elsif (((LINK_ln_recv_RE) = ('1')) and ((LINK_ln_recv_LOCKed) = ('1'))) and ((LINK_ln_AVAIL) = ('1')) then
          LINK_ln_recv_RD <= LINK_ln_DIN_REG;
          LINK_ln_recv_RD_ERR <= '0';
          LINK_ln_recv_GD <= '0';
          LINK_ln_recv_LOCKed <= '0';
          LINK_ln_RE <= '1';
          LINK_ln_LOCKED <= '1';
        elsif (((LINK_ln_send_WE) = ('1')) and ((LINK_ln_send_LOCKed) = ('1'))) and ((LINK_ln_BUSY) = ('0')) then
          LINK_ln_send_WR_ERR <= '0';
          LINK_ln_send_GD <= '0';
          LINK_ln_send_LOCKed <= '0';
          LINK_ln_LOCKED <= '1';
        elsif ((LINK_ln_recv_RE) = ('1')) and ((LINK_ln_recv_LOCKed) = ('0')) then
          if (LINK_ln_AVAIL) = ('1') then
            LINK_ln_recv_RD <= LINK_ln_DIN_REG;
            LINK_ln_recv_RD_ERR <= '0';
            LINK_ln_recv_GD <= '0';
            LINK_ln_RE <= '1';
            LINK_ln_LOCKED <= '1';
          else
            LINK_ln_recv_LOCKed <= '1';
          end if;
        elsif (((LINK_ln_send_WE) = ('1')) and ((LINK_ln_send_LOCKed) = ('0'))) and ((LINK_ln_BUSY) = ('0')) then
          LINK_ln_DOUT_REG <= LINK_ln_send_WR;
          LINK_ln_WE <= '1';
          LINK_ln_LOCKED <= '1';
          LINK_ln_send_LOCKed <= '1';
        end if;
      else
        LINK_ln_main_GD <= '1';
        LINK_ln_recv_GD <= '1';
        LINK_ln_send_GD <= '1';
        LINK_ln_RE <= '0';
        LINK_ln_WE <= '0';
        LINK_ln_LOCKED <= '0';
      end if;
    end if;
  end process LINK_ln_SCHED;
  --
  -- EMI <Object Link.link.ln> Process
  --
  LINK_ln_DECODER: process(LINK_ln_DIN2)
    variable valid: std_logic;
    variable empty: std_logic;
  begin
    valid := '1';
    empty := '1';
    for i in 0 to 15 loop
      valid := (valid) and ((LINK_ln_DIN2((2) * (i))) xor (LINK_ln_DIN2(((2) * (i)) + (1))));
      empty := (empty) and ((not (LINK_ln_DIN2((2) * (i)))) and (not (LINK_ln_DIN2(((2) * (i)) + (1)))));
      LINK_ln_DIN(i) <= LINK_ln_DIN2((2) * (i));
    end loop;
    LINK_ln_DIN_COMPL <= valid;
    LINK_ln_DIN_EMPTY <= empty;
  end process LINK_ln_DECODER;
  --
  -- EMI <Object Link.link.ln> Process
  --
  LINK_ln_ENCODER: process(LINK_ln_DOUT_EMPTY,
    LINK_ln_DOUT_REG)
  begin
    if (LINK_ln_DOUT_EMPTY) = ('1') then
      LINK_ln_DOUT2 <= "00000000000000000000000000000000";
    else
      for i in 0 to 15 loop
        LINK_ln_DOUT2((2) * (i)) <= LINK_ln_DOUT_REG(i);
        LINK_ln_DOUT2(((2) * (i)) + (1)) <= not (LINK_ln_DOUT_REG(i));
      end loop;
    end if;
  end process LINK_ln_ENCODER;
  --
  -- EMI <Object Link.link.ln>
  --
  LINK_ln_DIN2 <= DEV_data_in_RD;
  DEV_data_out_WR <= LINK_ln_DOUT2;
  DEV_data_in_ack_WR <= LINK_ln_DIN_ACK;
  LINK_ln_DOUT_ACK <= DEV_data_out_ack_RD;
  --
  -- End of <Object Link.link.ln>
  --
  
  -- Process control
  PRO_CONTROL_recv: process(
          PRO_recv_main_START,
          conpro_system_clk,
          conpro_system_reset
          )
  begin
    if conpro_system_clk'event and conpro_system_clk = '1' then
      if conpro_system_reset = '1' then
        PRO_recv_ENABLE <= '0';
        PRO_recv_main_GD <= '1';
      elsif PRO_recv_main_START = '1' then
        PRO_recv_ENABLE <= '1';
        PRO_recv_main_GD <= '0';
      else
        PRO_recv_main_GD <= '1';
      end if;
    end if;
  end process PRO_CONTROL_recv;
  
  
  -- Process instantiations
  PRO_MAP_send: com_send port map(
    REG_PRO_send_EXCEPTION_RD => REG_PRO_send_EXCEPTION_RD,
    REG_PRO_send_EXCEPTION_WR => REG_PRO_send_EXCEPTION_send_WR,
    REG_PRO_send_EXCEPTION_WE => REG_PRO_send_EXCEPTION_send_WE,
    RND_rnd_RE => RND_rnd_send_RE,
    RND_rnd_RD => RND_rnd_send_RD,
    RND_rnd_GD => RND_rnd_send_GD,
    LINK_ln_STOP => LINK_ln_send_STOP,
    LINK_ln_WE => LINK_ln_send_WE,
    LINK_ln_WR => LINK_ln_send_WR,
    LINK_ln_WR_ERR => LINK_ln_send_WR_ERR,
    LINK_ln_GD => LINK_ln_send_GD,
    PRO_send_ENABLE => PRO_send_ENABLE,
    PRO_send_END => PRO_send_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_recv: com_recv port map(
    REG_PRO_recv_EXCEPTION_RD => REG_PRO_recv_EXCEPTION_RD,
    REG_PRO_recv_EXCEPTION_WR => REG_PRO_recv_EXCEPTION_recv_WR,
    REG_PRO_recv_EXCEPTION_WE => REG_PRO_recv_EXCEPTION_recv_WE,
    DEV_data_WR => DEV_data_WR,
    LINK_ln_STOP => LINK_ln_recv_STOP,
    LINK_ln_RE => LINK_ln_recv_RE,
    LINK_ln_RD => LINK_ln_recv_RD,
    LINK_ln_RD_ERR => LINK_ln_recv_RD_ERR,
    LINK_ln_GD => LINK_ln_recv_GD,
    DEV_data_en_WR => DEV_data_en_WR,
    PRO_recv_ENABLE => PRO_recv_ENABLE,
    PRO_recv_END => PRO_recv_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  PRO_MAP_main: com_main port map(
    PRO_send_START => PRO_send_main_START,
    PRO_send_GD => PRO_send_main_GD,
    RND_rnd_INIT => RND_rnd_main_INIT,
    RND_rnd_GD => RND_rnd_main_GD,
    LINK_ln_INIT => LINK_ln_main_INIT,
    LINK_ln_START => LINK_ln_main_START,
    LINK_ln_GD => LINK_ln_main_GD,
    PRO_recv_START => PRO_recv_main_START,
    PRO_recv_GD => PRO_recv_main_GD,
    PRO_main_ENABLE => PRO_main_ENABLE,
    PRO_main_END => PRO_main_END,
    conpro_system_clk => conpro_system_clk,
    conpro_system_reset => conpro_system_reset
  );
  
  -- Toplevel assignments
  -- Monitors
  DEV_data_in_RD <= DEV_data_in;
  DEV_data_in_ack <= DEV_data_in_ack_WR;
  DEV_data_out <= DEV_data_out_WR;
  DEV_data_out_ack_RD <= DEV_data_out_ack;
  DEV_data <= DEV_data_WR;
  DEV_data_en <= DEV_data_en_WR;
  conpro_system_clk <= CLK;
  conpro_system_reset <= RESET;
end main;