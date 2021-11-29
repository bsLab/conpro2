--
-- Automatically generated by
-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 A8 **  2049142 total genes
--         Compiled on Thu Nov 27 11:07:26 CET 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

-- Process implementation of process <main> from module <R>.
--
libIEEE
useIEEE1
useIEEE2
useIEEE3
useIEEE4
useIEEE5
useIEEE6
useIEEE7
useIEEE8
entity r_main is
port(
  -- Connections to external objects, components and the outside world
  signal PRO_p1_START: out std_logic;
  signal PRO_p1_GD: in std_logic;
  signal F_rnd1_INIT: out std_logic;
  signal F_rnd1_GD: in std_logic;
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
    S_i1_fun, -- FUN42151[r.cp:51]
    S_i2_fun, -- FUN11607[r.cp:52]
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
          PRO_p1_GD,
          pro_state
          )
  begin
    PRO_main_END <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        pro_state_next <= S_i1_fun;
      when S_i1_fun => -- FUN42151[r.cp:51]
        pro_state_next <= S_i2_fun;
      when S_i2_fun => -- FUN11607[r.cp:52]
        if PRO_p1_GD = '1' then
          pro_state_next <= S_i2_fun;
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
    F_rnd1_INIT <= '0';
    PRO_p1_START <= '0';
    case pro_state is
      when S_main_start => -- PROCESS0[:0]
        null;
      when S_i1_fun => -- FUN42151[r.cp:51]
        F_rnd1_INIT <= '1';
      when S_i2_fun => -- FUN11607[r.cp:52]
        PRO_p1_START <= '1';
      when S_main_end => -- PROCESS0[:0]
        null;
    end case;
  end process data_path;
  
  -- Object implementation
  
  -- Toplevel assignments
  -- Monitors
end main;
