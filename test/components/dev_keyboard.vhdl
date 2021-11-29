library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity dev_keyboard is
port(
    kb_data : out std_logic_vector(7 downto 0);
    kb_init : in std_logic;
    kb_clk : in std_logic
);
end dev_keyboard;
architecture main of dev_keyboard is
  signal d: std_logic_vector(7 downto 0);
begin
  process(kb_clk)
  begin
    if kb_clk'event and kb_clk='1' then
      if kb_init = '1' then
        d <= X"00";
      else
        d <= d + 1;
      end if;
    end if;
  end process;
  kb_data <= d;
end main;
