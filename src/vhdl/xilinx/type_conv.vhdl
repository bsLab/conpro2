library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
entity MOD_type_conv is
port(
  -- Connections to the outside world
  signal TEST1,TEST2,TEST3: out std_logic;
  signal CLK: in std_logic;
  signal RESET: in std_logic
);


--
-- ConPro2 library
--

--
-- Type conversion
--
function conv_Ln_Lm(L_n:std_logic_vector;n,m:natural) return std_logic_vector is
  variable fill: std_logic_vector(m-n-1 downto 0) := (others => '0');
  variable L_m: std_logic_vector(m-1 downto 0);
  begin
    L_m := fill & L_n;
    return L_m;
  end;
function conv_Lm_Ln(L_m:std_logic_vector;m,n:natural) return std_logic_vector is
  variable L_n: std_logic_vector(n-1 downto 0);
  begin
    L_n := L_m(n-1 downto 0);
    return L_n;
  end;
function conv_Ln_Lm_1(L_n:std_logic_vector;n,m:natural) return std_logic_vector is
  variable L_m: std_logic_vector(m-1 downto 0);
  begin
    L_m := '1' & L_n;
    return L_m;
  end;
function conv_In_Im(I_n:signed;n,m:natural) return signed is
  variable I_m: signed(m-1 downto 0);
  begin
    I_m := conv_signed(I_n,m);
    return I_m;
  end;
function conv_Im_In(I_m:signed;m,n:natural) return signed is
  variable I_n: signed(n-1 downto 0);
  begin
    I_n := conv_signed(I_m,n);
    return I_n;
  end;
function conv_I_L(I:signed) return std_logic_vector is
  variable L: std_logic_vector(I'length-1 downto 0);
  begin
    L := std_logic_vector(I);
    return L;
  end;
function conv_In_Lm(I_n:signed;n,m:natural) return std_logic_vector is
  variable L_m: std_logic_vector(m-1 downto 0);
  begin
    L_m := std_logic_vector(conv_signed(I_n,m));
    return L_m;
  end;
function conv_Im_Ln(I_m:signed;m,n:natural) return std_logic_vector is
  variable L_n: std_logic_vector(n-1 downto 0);
  begin
    L_n := std_logic_vector(conv_signed(I_m,n));
    return L_n;
  end;
function conv_L_I(L:std_logic_vector) return signed is
  variable I: signed(L'length-1 downto 0);
  begin
    I := signed(L);
    return I;
  end;
function conv_Ln_Im(L_n:std_logic_vector;n,m:natural) return signed is
  variable I_m: signed(m-1 downto 0);
  begin
    I_m := conv_signed(signed(L_n),m);
    return I_m;
  end;
function conv_Lm_In(L_m:std_logic_vector;m,n:natural) return signed is
  variable I_n: signed(n-1 downto 0);
  begin
    I_n := conv_signed(signed(L_m),n);
    return I_n;
  end;
function conv_N_Im(N:natural;m:natural) return signed is
  variable I_m: signed(m-1 downto 0);
  begin
    I_m := conv_signed(N,m);
    return I_m;
  end;

end MOD_type_conv;


architecture main of MOD_type_conv is
  signal L9: std_logic_vector(8 downto 0);
  signal L12: std_logic_vector(11 downto 0);
  signal I4: signed(3 downto 0);
  signal I8: signed(7 downto 0);
begin
  L9 <= conv_Lm_Ln(L12,12,9) + conv_Lm_Ln(L12-1,12,9) + 2;
  L12 <= conv_Ln_Lm(L9,9,12) + conv_In_Lm(I8,8,12);
  I8 <= conv_N_Im(9,8) + conv_In_Im(I4,4,8);
  TEST1 <= '1' when conv_Lm_Ln(L9,9,4) = "0000" else '0';
  TEST2 <= '1' when L12 = X"000" else '0';
  TEST3 <= '1' when conv_I_L(I8) = X"00" else '0';
  testp: process(clk,reset)
  begin
    if clk'event and clk='1' then
      if reset = '1' then
        I4 <= conv_N_Im(0,4);
      else
        I4 <= conv_Im_In(I8+2,8,4);
      end if;
    end if;
  end process testp;
end main;
