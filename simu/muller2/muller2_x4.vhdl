
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY muller2_x4 IS
PORT(
  A,B,R,S: in std_logic;
  Q: out std_logic
);
END muller2_x4;

ARCHITECTURE RTL OF muller2_x4 IS
  signal t1,t2,t3,rs_t,s_t: std_logic;
BEGIN
  t1 <= A and B;
  t2 <= A and rs_t;
  t3 <= B and rs_t;
  s_t <= t1 or t2 or t3;
  rs_t <= '0' when R = '1' else
          '1' when S = '1' else s_t;
  Q <= rs_t;
END RTL;
