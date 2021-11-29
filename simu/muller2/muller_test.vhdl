library IEEE;
use IEEE.std_logic_1164.all;    
use IEEE.std_logic_arith.all;   
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity muller_test is
port(
  signal M_in:  in std_logic;
  signal M_out: out std_logic_vector(2 downto 0);
  signal conpro_system_reset: in std_logic
);
end muller_test;
architecture main of muller_test is
    component muller_2d_cell
    port(
      signal DT,DF,DACK: in std_logic;
      signal QT,QF,QACK: out std_logic;
      signal conpro_system_reset: in std_logic
    );
    end component;
    signal dt1,dt2,dt3,dt4: std_logic;
    signal df1,df2,df3,df4: std_logic;
    signal dack1,dack2,dack3,dack4: std_logic;
begin
  MAP_MUL1: muller_2d_cell port map ( 
   DT => dt1,
   DF => df1,
   DACK => dack2,
   QT => dt2,
   QF => df2,
   QACK => dack1,
   conpro_system_reset => conpro_system_reset
  );
  MAP_MUL2: muller_2d_cell port map ( 
   DT => dt2,
   DF => df2,
   DACK => dack3,
   QT => dt3,
   QF => df3,
   QACK => dack2,
   conpro_system_reset => conpro_system_reset
  );
  MAP_MUL3: muller_2d_cell port map ( 
   DT => dt3,
   DF => df3,
   DACK => dack4,
   QT => dt4,
   QF => df4,
   QACK => dack3,
   conpro_system_reset => conpro_system_reset
  );
  M_out <= dt3 & dt2 & dt1;
  dack4 <= '0';
  dt1 <= '1' when M_in = '1' and dack1 = '0' else '0';
  df1 <= '0' when M_in = '1' and dack1 = '0' else '0';

end main;
