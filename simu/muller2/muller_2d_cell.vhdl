library IEEE;
use IEEE.std_logic_1164.all;    
use IEEE.std_logic_arith.all;   
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity muller_2d_cell is
port(
  signal DT,DF,DACK: in std_logic;
  signal QT,QF,QACK: out std_logic;
  signal D: in std_logic;
  signal RST,SET: in std_logic
);
end muller_2d_cell;
architecture main of muller_2d_cell is
  component muller2_x4
      port(
          signal A,B,R,S: in std_logic;
          signal Q: out std_logic   
      );
  end component;
  signal n_ack,q1,q2,st,sf: std_logic;
begin
  MAP_MUL1: muller2_x4 port map ( 
   A => n_ack,
   B => DT,
   R => RST,
   S => st,
   Q => q1
  );
  MAP_MUL2: muller2_x4 port map ( 
   A => n_ack,
   B => DF,
   R => RST,
   S => sf,
   Q => q2
  );
  st <= '1' when SET = '1' and D = '1' else '0';
  sf <= '1' when SET = '1' and D = '0' else '0';
  n_ack <= not DACK;
  QACK <= q1 or q2; 
  QT <= q1;
  QF <= q2;
end main;
