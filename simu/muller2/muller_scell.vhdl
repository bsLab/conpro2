library IEEE;
use IEEE.std_logic_1164.all;    
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;

entity muller_scell is
port(
  signal DT,DF,DACK: in std_logic;
  signal QT,QF,QACK: out std_logic;
  signal QET,QEF: out std_logic;
  signal DIN,DLOAD: in std_logic;
  signal RST: in std_logic
);
end muller_scell;
architecture main of muller_scell is
    component muller_2d_cell
    port(
      signal DT,DF,DACK: in std_logic;
      signal QT,QF,QACK: out std_logic;
      signal D: in std_logic;
      signal RST,SET: in std_logic
    );
    end component;
    signal e_rst,zero,q1e_t,q1e_f,q2e_ack: std_logic;
begin
  MAP_MUL_1E: muller_2d_cell port map (
   DT => DT,
   DF => DF,
   DACK => q2e_ack,
   QT => q1e_t,
   QF => q1e_f,
   QACK => QACK,
   SET => zero,
   D => zero,
   RST => e_rst
  );

  MAP_MUL_1D: muller_2d_cell port map (
   DT => q1e_t,
   DF => q1e_f,
   DACK => DACK,
   QT => QT,
   QF => QF,
   QACK => q2e_ack,
   SET => DLOAD,
   D => DIN,
   RST => RST
  );

  e_rst <= RST or DLOAD;
  QET <= q1e_t;
  QEF <= q1e_f;
  zero <= '0';
end;

