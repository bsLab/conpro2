library IEEE;
use IEEE.std_logic_1164.all;    
use IEEE.std_logic_arith.all;   
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity muller_test_shift_2 is
port(
  signal M_load,M_shift: in std_logic;
  signal M_din: in std_logic_vector(3 downto 0);
  signal M_qe_0: out std_logic_vector(1 downto 0);
  signal M_qd_0: out std_logic_vector(1 downto 0);
  signal M_qe_1: out std_logic_vector(1 downto 0);
  signal M_qd_1: out std_logic_vector(1 downto 0);
  signal M_qe_2: out std_logic_vector(1 downto 0);
  signal M_qd_2: out std_logic_vector(1 downto 0);
  signal M_qe_3: out std_logic_vector(1 downto 0);
  signal M_qd_3: out std_logic_vector(1 downto 0);
  signal M_qe_4: out std_logic_vector(1 downto 0);
  signal M_qd_4: out std_logic_vector(1 downto 0);
  signal M_qe_5: out std_logic_vector(1 downto 0);
  signal M_qd_5: out std_logic_vector(1 downto 0);
  signal M_qe_6: out std_logic_vector(1 downto 0);
  signal M_qd_6: out std_logic_vector(1 downto 0);
  signal M_qe_7: out std_logic_vector(1 downto 0);
  signal M_qd_7: out std_logic_vector(1 downto 0);
  signal M_RST: in std_logic
);
end muller_test_shift_2;
architecture main of muller_test_shift_2 is
  component muller_scell
    port(
      signal DT,DF,DACK: in std_logic; 
      signal QT,QF,QACK: out std_logic;
      signal QET,QEF: out std_logic;
      signal DIN,DLOAD: in std_logic;
      signal RST: in std_logic                
    );
  end component;
  component muller_dcell
    port(
      signal DT,DF,DACK: in std_logic; 
      signal QT,QF,QACK: out std_logic;
      signal QET,QEF: out std_logic;
      signal RST: in std_logic                
    );
  end component;
  component muller_ecell
    port(
      signal DT,DF: in std_logic; 
      signal QACK: out std_logic;
      signal RST: in std_logic                
    );
  end component;
  signal sd_t,sd_f,sq_t,sq_f,sqe_t,sqe_f,sd_ack,sq_ack: 
        std_logic_vector(3 downto 0);
  signal dd_t,dd_f,dq_t,dq_f,dqe_t,dqe_f,dd_ack,dq_ack: 
        std_logic_vector(3 downto 0);
  signal d_rst: std_logic;
begin
  scells: for i in 0 to 3 generate
  begin
    scelli: muller_scell port map (
       DT => sd_t(i),
       DF => sd_f(i),
       DACK => sd_ack(i),
       QT => sq_t(i),
       QF => sq_f(i),
       QACK => sq_ack(i),
       QET => sqe_t(i),
       QEF => sqe_f(i),
       DIN => M_din(i),
       DLOAD => M_load,
       RST => M_RST
    );
  end generate scells;

  sd_t(0) <= '0';
  sd_f(0) <= '0';

  scells_connect: for i in 1 to 3 generate
  begin
    sd_t(i) <= sq_t(i-1);
    sd_f(i) <= sq_f(i-1);
    sd_ack(i-1) <= sq_ack(i);
  end generate scells_connect;
  sd_ack(3) <= dq_ack(0);

  -- -----------------------------------------------

  dcells: for i in 0 to 3 generate
  begin
    dcelli: muller_dcell port map (
       DT => dd_t(i),
       DF => dd_f(i),
       DACK => dd_ack(i),
       QT => dq_t(i),
       QF => dq_f(i),
       QACK => dq_ack(i),
       QET => dqe_t(i),
       QEF => dqe_f(i),
       RST => d_rst
    );
  end generate dcells;

  dd_t(0) <= sq_t(3);
  dd_f(0) <= sq_f(3);

  dcells_connect: for i in 1 to 3 generate
  begin
    dd_t(i) <= dq_t(i-1);
    dd_f(i) <= dq_f(i-1);
    dd_ack(i-1) <= dq_ack(i);
  end generate dcells_connect;

  M_qe_0 <= sqe_t(0) & sqe_f(0);
  M_qd_0 <= sq_t(0)  & sq_f(0);
  M_qe_1 <= sqe_t(1) & sqe_f(1);
  M_qd_1 <= sq_t(1)  & sq_f(1);
  M_qe_2 <= sqe_t(2) & sqe_f(2);
  M_qd_2 <= sq_t(2)  & sq_f(2);
  M_qe_3 <= sqe_t(3) & sqe_f(3);
  M_qd_3 <= sq_t(3)  & sq_f(3);
  M_qe_4 <= dqe_t(0) & dqe_f(0);
  M_qd_4 <= dq_t(0)  & dq_f(0);
  M_qe_5 <= dqe_t(1) & dqe_f(1);
  M_qd_5 <= dq_t(1)  & dq_f(1);
  M_qe_6 <= dqe_t(2) & dqe_f(2);
  M_qd_6 <= dq_t(2)  & dq_f(2);
  M_qe_7 <= dqe_t(3) & dqe_f(3);
  M_qd_7 <= dq_t(3)  & dq_f(3);


  d_rst <= M_RST or M_load;
end main;
