
-- 
-- Definition of  MOD_type_conv
-- 
--      Fri 19 Dec 2008 11:31:38 AM CET
--      
--      LeonardoSpectrum Level 3, 2005b.24
-- 

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity MOD_type_conv is
   port (
      TEST1 : OUT std_logic ;
      TEST2 : OUT std_logic ;
      TEST3 : OUT std_logic ;
      CLK : IN std_logic ;
      RESET : IN std_logic) ;
end MOD_type_conv ;

architecture main of MOD_type_conv is
   signal I4_3, I4_1, nx4, I4_2, nx16, nx32, I4_0, nx48, nx64, nx88, nx94, 
      nx132, nx152, nx172, nx273, nx194, nx200, nx202, nx274, nx222, nx228, 
      nx234, nx242, nx260, nx275, nx282, nx288, nx304, nx318, nx322, nx328, 
      nx346, nx360, nx362, nx364, nx386, nx295, nx305, nx307, nx317, nx319, 
      nx323, nx329, nx339, nx343, nx345, nx347, nx355, nx359, nx363, nx369, 
      nx371, nx373, nx379, nx381, nx385, nx389, nx391, nx397, nx399, nx411, 
      nx413, nx415, nx418, nx421, nx431, nx434, nx437, nx444: std_logic ;

begin
   ix261 : zero_x0 port map ( nq=>TEST1);
   ix63 : no4_x1 port map ( nq=>TEST3, i0=>I4_3, i1=>I4_2, i2=>I4_1, i3=>
      I4_0);
   reg_I4_3 : sff1_x4 port map ( q=>I4_3, i=>nx32, ck=>CLK);
   ix33 : no2_x1 port map ( nq=>nx32, i0=>RESET, i1=>nx295);
   ix296 : oa22_x2 port map ( q=>nx295, i0=>I4_1, i1=>I4_2, i2=>nx307);
   reg_I4_1 : sff1_x4 port map ( q=>I4_1, i=>nx4, ck=>CLK);
   ix5 : no2_x1 port map ( nq=>nx4, i0=>RESET, i1=>I4_1);
   reg_I4_2 : sff1_x4 port map ( q=>I4_2, i=>nx16, ck=>CLK);
   ix17 : no2_x1 port map ( nq=>nx16, i0=>RESET, i1=>nx305);
   ix306 : nxr2_x1 port map ( nq=>nx305, i0=>I4_1, i1=>I4_2);
   ix308 : inv_x1 port map ( nq=>nx307, i=>I4_3);
   reg_I4_0 : sff1_x4 port map ( q=>I4_0, i=>nx48, ck=>CLK);
   ix395 : no4_x1 port map ( nq=>TEST2, i0=>nx317, i1=>nx273, i2=>nx274, i3
      =>nx379);
   ix318 : na3_x1 port map ( nq=>nx317, i0=>nx319, i1=>nx329, i2=>nx323);
   ix320 : nxr2_x1 port map ( nq=>nx319, i0=>I4_2, i1=>nx152);
   ix153 : nxr2_x1 port map ( nq=>nx152, i0=>I4_3, i1=>nx323);
   ix324 : nxr2_x1 port map ( nq=>nx323, i0=>I4_1, i1=>nx88);
   ix89 : nxr2_x1 port map ( nq=>nx88, i0=>I4_2, i1=>I4_1);
   ix173 : nmx2_x1 port map ( nq=>nx172, cmd=>nx152, i0=>nx323, i1=>nx339);
   ix340 : inv_x1 port map ( nq=>nx339, i=>I4_2);
   ix203 : nxr2_x1 port map ( nq=>nx202, i0=>I4_3, i1=>nx343);
   ix344 : nxr2_x1 port map ( nq=>nx343, i0=>nx345, i1=>nx359);
   ix346 : xr2_x1 port map ( q=>nx345, i0=>nx319, i1=>nx347);
   ix95 : na2_x1 port map ( nq=>nx94, i0=>nx355, i1=>I4_0);
   ix356 : nxr2_x1 port map ( nq=>nx355, i0=>I4_0, i1=>nx64);
   ix65 : nxr2_x1 port map ( nq=>nx64, i0=>I4_1, i1=>I4_0);
   ix360 : na3_x1 port map ( nq=>nx359, i0=>I4_0, i1=>nx64, i2=>nx323);
   ix245 : nxr2_x1 port map ( nq=>nx274, i0=>nx363, i1=>nx242);
   ix364 : noa2a22_x1 port map ( nq=>nx363, i0=>nx172, i1=>nx202, i2=>nx200, 
      i3=>I4_3);
   ix243 : nxr2_x1 port map ( nq=>nx242, i0=>I4_3, i1=>nx369);
   ix370 : xr2_x1 port map ( q=>nx369, i0=>nx371, i1=>nx194);
   ix372 : nxr2_x1 port map ( nq=>nx371, i0=>nx373, i1=>nx132);
   ix374 : nxr2_x1 port map ( nq=>nx373, i0=>nx172, i1=>nx202);
   ix133 : na2_x1 port map ( nq=>nx132, i0=>nx347, i1=>nx319);
   ix195 : no2_x1 port map ( nq=>nx194, i0=>nx345, i1=>nx359);
   ix380 : na4_x1 port map ( nq=>nx379, i0=>nx381, i1=>nx411, i2=>nx431, i3
      =>nx386);
   ix263 : nmx2_x1 port map ( nq=>nx260, cmd=>nx242, i0=>nx369, i1=>nx363);
   ix386 : nxr2_x1 port map ( nq=>nx385, i0=>I4_3, i1=>nx288);
   ix289 : nxr2_x1 port map ( nq=>nx288, i0=>nx389, i1=>nx234);
   ix390 : nxr2_x1 port map ( nq=>nx389, i0=>nx391, i1=>nx222);
   ix229 : nao22_x1 port map ( nq=>nx228, i0=>nx397, i1=>nx399, i2=>nx222);
   ix412 : nxr2_x1 port map ( nq=>nx411, i0=>nx413, i1=>nx415);
   ix414 : nmx2_x1 port map ( nq=>nx413, cmd=>nx385, i0=>nx260, i1=>nx288);
   ix416 : nxr2_x1 port map ( nq=>nx415, i0=>I4_3, i1=>nx328);
   ix329 : nxr2_x1 port map ( nq=>nx328, i0=>nx418, i1=>nx282);
   ix419 : nxr2_x1 port map ( nq=>nx418, i0=>nx304, i1=>nx318);
   ix422 : noa22_x1 port map ( nq=>nx421, i0=>nx275, i1=>nx222, i2=>nx318);
   ix293 : nxr2_x1 port map ( nq=>nx275, i0=>nx260, i1=>nx385);
   ix425 : no2_x1 port map ( nq=>nx318, i0=>nx222, i1=>nx275);
   ix432 : nxr2_x1 port map ( nq=>nx431, i0=>nx346, i1=>nx364);
   ix347 : nmx2_x1 port map ( nq=>nx346, cmd=>nx415, i0=>nx413, i1=>nx434);
   ix365 : nxr2_x1 port map ( nq=>nx364, i0=>I4_3, i1=>nx437);
   ix438 : nxr2_x1 port map ( nq=>nx437, i0=>nx360, i1=>nx322);
   ix361 : nxr2_x1 port map ( nq=>nx360, i0=>nx411, i1=>nx318);
   ix387 : xr2_x1 port map ( q=>nx386, i0=>nx444, i1=>I4_3);
   ix445 : noa2a22_x1 port map ( nq=>nx444, i0=>nx346, i1=>nx364, i2=>nx362, 
      i3=>I4_3);
   ix363 : inv_x1 port map ( nq=>nx362, i=>nx437);
   ix435 : inv_x1 port map ( nq=>nx434, i=>nx328);
   ix382 : inv_x1 port map ( nq=>nx381, i=>nx275);
   ix398 : inv_x1 port map ( nq=>nx397, i=>nx274);
   ix201 : inv_x1 port map ( nq=>nx200, i=>nx343);
   ix205 : inv_x1 port map ( nq=>nx273, i=>nx373);
   ix330 : inv_x1 port map ( nq=>nx329, i=>nx94);
   ix49 : an12_x1 port map ( q=>nx48, i0=>RESET, i1=>I4_0);
   ix348 : an12_x1 port map ( q=>nx347, i0=>nx94, i1=>nx323);
   ix394 : on12_x1 port map ( q=>nx391, i0=>nx228, i1=>nx399);
   ix400 : an12_x1 port map ( q=>nx399, i0=>nx132, i1=>nx373);
   ix223 : on12_x1 port map ( q=>nx222, i0=>nx399, i1=>nx274);
   ix235 : an12_x1 port map ( q=>nx234, i0=>nx371, i1=>nx194);
   ix305 : an12_x1 port map ( q=>nx304, i0=>nx421, i1=>nx222);
   ix283 : an12_x1 port map ( q=>nx282, i0=>nx389, i1=>nx234);
   ix323 : an12_x1 port map ( q=>nx322, i0=>nx418, i1=>nx282);
end main ;

