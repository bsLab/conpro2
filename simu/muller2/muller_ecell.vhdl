library IEEE;
use IEEE.std_logic_1164.all;    
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;

entity muller_ecell is
port(
  signal DT,DF: in std_logic;
  signal QACK: out std_logic;
  signal RST: in std_logic
);
end muller_ecell;
architecture main of muller_ecell is
    component muller_2d_cell
    port(
      signal DT,DF,DACK: in std_logic;
      signal QT,QF,QACK: out std_logic;
      signal D: in std_logic;
      signal RST,SET: in std_logic
    );
    end component;
    signal zero,one: std_logic;
begin
  MAP_MUL_1E: muller_2d_cell port map (
   DT => DT,
   DF => DF,
   DACK => one,
   QACK => QACK,
   SET => zero,
   D => zero,
   RST => RST
  );

  zero <= '0';
  one <= '1';
end;

