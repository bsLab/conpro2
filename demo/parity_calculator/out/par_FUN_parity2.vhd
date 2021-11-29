
-- 
-- Definition of  par_FUN_parity2
-- 
--      Fri Dec 19 11:31:38 2008
--      
--      LeonardoSpectrum Level 3, 2005b.24
-- 

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity par_FUN_parity2 is
   port (
      REG_ARG_FUN_parity2_x_RD : IN std_logic_vector (63 DOWNTO 0) ;
      REG_RET_FUN_parity2_p_WR : OUT std_logic ;
      REG_RET_FUN_parity2_p_WE : OUT std_logic ;
      PRO_FUN_parity2_ENABLE : IN std_logic ;
      PRO_FUN_parity2_END : OUT std_logic ;
      conpro_system_clk : IN std_logic ;
      conpro_system_reset : IN std_logic) ;
end par_FUN_parity2 ;

architecture main of par_FUN_parity2 is
   signal PRO_FUN_parity2_END_EXMPLR, REG_RET_FUN_parity2_p_WE_EXMPLR, 
      LOOP_i_1_7, pro_state_1, pro_state_0, nx2, nx12, nx20, pro_state_3, 
      nx1706, nx30, nx34, LOOP_i_1_6, LOOP_i_1_5, LOOP_i_1_4, LOOP_i_1_3, 
      LOOP_i_1_2, LOOP_i_1_1, nx1707, LOOP_i_1_0, nx56, nx1708, nx74, nx1709, 
      nx92, nx1710, nx110, nx1711, nx128, nx1712, nx146, pro_state_2, nx176, 
      nx196, pl, nx206, xl_2, nx210, nx222, xl_18, nx228, nx238, xl_50, 
      nx246, nx254, xl_34, nx260, nx270, nx276, xl_33, nx282, nx294, xl_35, 
      nx302, nx310, xl_3, nx316, nx324, xl_17, nx334, nx342, xl_16, nx348, 
      nx358, xl_19, nx364, nx372, xl_48, nx384, nx394, xl_0, nx400, nx410, 
      xl_51, nx416, nx424, xl_1, nx434, nx442, xl_32, nx448, nx458, xl_49, 
      nx464, nx472, nx482, xl_6, nx490, xl_22, nx502, xl_54, nx516, xl_38, 
      nx528, nx540, xl_37, nx546, xl_39, nx560, xl_7, nx572, xl_21, nx588, 
      xl_20, nx600, xl_23, nx612, xl_52, nx630, xl_4, nx642, xl_55, nx654, 
      xl_5, nx670, xl_36, nx682, xl_53, nx694, nx710, xl_10, nx722, xl_26, 
      nx734, xl_58, nx748, xl_42, nx760, nx772, xl_41, nx778, xl_43, nx792, 
      xl_11, nx804, xl_25, nx820, xl_24, nx832, xl_27, nx844, xl_56, nx862, 
      xl_8, nx874, xl_59, nx886, xl_9, nx902, xl_40, nx914, xl_57, nx926, 
      nx942, xl_14, nx950, xl_30, nx962, xl_62, nx976, xl_46, nx988, nx1000, 
      xl_45, nx1006, xl_47, nx1020, xl_15, nx1032, xl_29, nx1048, xl_28, 
      nx1060, xl_31, nx1072, xl_60, nx1090, xl_12, nx1102, xl_63, nx1114, 
      xl_13, nx1130, xl_44, nx1142, xl_61, nx1154, nx1170, nx1178, nx1198, 
      nx1719, nx1789, nx2439, nx2452, nx2456, nx2461, nx2468, nx2475, nx2482, 
      nx2492, nx2496, nx2499, nx2501, nx2504, nx2512, nx2516, nx2518, nx2522, 
      nx2524, nx2526, nx2530, nx2533, nx2536, nx2547, nx2552, nx2554, nx2565, 
      nx2570, nx2580, nx2608, nx2616, nx2624, nx2632, nx2649, nx2652, nx2660, 
      nx2668, nx2676, nx2694, nx2702, nx2710, nx2718, nx2735, nx2744, nx2746, 
      nx2748, nx2750, nx2752, nx2754, nx2756, nx2758, nx2760, nx2762, nx2764, 
      nx2766, nx2768, nx2770, nx2772: std_logic ;

begin
   REG_RET_FUN_parity2_p_WE <= REG_RET_FUN_parity2_p_WE_EXMPLR ;
   PRO_FUN_parity2_END <= PRO_FUN_parity2_END_EXMPLR ;
   ix1193 : a2_x2 port map ( q=>REG_RET_FUN_parity2_p_WR, i0=>
      REG_RET_FUN_parity2_p_WE_EXMPLR, i1=>pl);
   reg_pro_state_4 : sff1_x4 port map ( q=>REG_RET_FUN_parity2_p_WE_EXMPLR, 
      i=>nx196, ck=>conpro_system_clk);
   ix197 : no4_x1 port map ( nq=>nx196, i0=>nx2452, i1=>LOOP_i_1_7, i2=>
      nx2512, i3=>nx2);
   ix2453 : inv_x1 port map ( nq=>nx2452, i=>LOOP_i_1_6);
   LOOP_i_1_6_EXMPLR : sff2_x4 port map ( q=>LOOP_i_1_6, i0=>LOOP_i_1_6, i1
      =>nx146, cmd=>nx34, ck=>conpro_system_clk);
   ix147 : no3_x1 port map ( nq=>nx146, i0=>nx2456, i1=>nx1712, i2=>nx30);
   LOOP_i_1_5_EXMPLR : sff2_x4 port map ( q=>LOOP_i_1_5, i0=>LOOP_i_1_5, i1
      =>nx128, cmd=>nx34, ck=>conpro_system_clk);
   ix129 : no3_x1 port map ( nq=>nx128, i0=>nx2461, i1=>nx1711, i2=>nx30);
   ix2462 : no2_x1 port map ( nq=>nx2461, i0=>nx1710, i1=>LOOP_i_1_5);
   LOOP_i_1_4_EXMPLR : sff2_x4 port map ( q=>LOOP_i_1_4, i0=>LOOP_i_1_4, i1
      =>nx110, cmd=>nx34, ck=>conpro_system_clk);
   ix111 : no3_x1 port map ( nq=>nx110, i0=>nx2468, i1=>nx1710, i2=>nx30);
   LOOP_i_1_3_EXMPLR : sff2_x4 port map ( q=>LOOP_i_1_3, i0=>LOOP_i_1_3, i1
      =>nx92, cmd=>nx34, ck=>conpro_system_clk);
   ix93 : no3_x1 port map ( nq=>nx92, i0=>nx2475, i1=>nx1709, i2=>nx30);
   ix2476 : no2_x1 port map ( nq=>nx2475, i0=>nx1708, i1=>LOOP_i_1_3);
   LOOP_i_1_2_EXMPLR : sff2_x4 port map ( q=>LOOP_i_1_2, i0=>LOOP_i_1_2, i1
      =>nx74, cmd=>nx34, ck=>conpro_system_clk);
   ix75 : no3_x1 port map ( nq=>nx74, i0=>nx2482, i1=>nx1708, i2=>nx30);
   LOOP_i_1_1_EXMPLR : sff2_x4 port map ( q=>LOOP_i_1_1, i0=>LOOP_i_1_1, i1
      =>nx56, cmd=>nx34, ck=>conpro_system_clk);
   ix57 : no3_x1 port map ( nq=>nx56, i0=>nx1707, i1=>nx30, i2=>nx2518);
   reg_pro_state_3 : sff1_x4 port map ( q=>pro_state_3, i=>nx1706, ck=>
      conpro_system_clk);
   ix189 : no3_x1 port map ( nq=>nx1706, i0=>nx2492, i1=>nx2512, i2=>nx2);
   LOOP_i_1_7_EXMPLR : sff1_x4 port map ( q=>LOOP_i_1_7, i=>nx1789, ck=>
      conpro_system_clk);
   ix1790 : nao2o22_x1 port map ( nq=>nx1789, i0=>nx2496, i1=>nx30, i2=>
      nx2501, i3=>nx34);
   ix2497 : nxr2_x1 port map ( nq=>nx2496, i0=>LOOP_i_1_7, i1=>nx1712);
   ix2500 : na2_x1 port map ( nq=>nx2499, i0=>LOOP_i_1_5, i1=>nx1710);
   ix2502 : inv_x1 port map ( nq=>nx2501, i=>LOOP_i_1_7);
   ix35 : na2_x4 port map ( nq=>nx34, i0=>nx30, i1=>nx2504);
   ix2505 : no2_x4 port map ( nq=>nx2504, i0=>pro_state_1, i1=>
      conpro_system_reset);
   reg_pro_state_1 : sff1_x4 port map ( q=>pro_state_1, i=>nx12, ck=>
      conpro_system_clk);
   ix13 : an12_x1 port map ( q=>nx12, i0=>nx2, i1=>pro_state_0);
   reg_pro_state_0 : sff1_x4 port map ( q=>pro_state_0, i=>nx2, ck=>
      conpro_system_clk);
   ix2513 : inv_x1 port map ( nq=>nx2512, i=>pro_state_2);
   reg_pro_state_2 : sff1_x4 port map ( q=>pro_state_2, i=>nx176, ck=>
      conpro_system_clk);
   ix177 : ao22_x2 port map ( q=>nx176, i0=>pro_state_1, i1=>pro_state_3, i2
      =>nx2516);
   ix2519 : no2_x1 port map ( nq=>nx2518, i0=>LOOP_i_1_0, i1=>LOOP_i_1_1);
   LOOP_i_1_0_EXMPLR : sff1_x4 port map ( q=>LOOP_i_1_0, i=>nx1719, ck=>
      conpro_system_clk);
   ix1720 : nmx2_x1 port map ( nq=>nx1719, cmd=>LOOP_i_1_0, i0=>nx30, i1=>
      nx34);
   ix2523 : inv_x1 port map ( nq=>nx2522, i=>LOOP_i_1_0);
   ix2525 : na2_x1 port map ( nq=>nx2524, i0=>LOOP_i_1_1, i1=>LOOP_i_1_0);
   ix2527 : na2_x1 port map ( nq=>nx2526, i0=>LOOP_i_1_3, i1=>nx1708);
   reg_pl : sff1_x4 port map ( q=>pl, i=>nx2439, ck=>conpro_system_clk);
   ix2440 : nao2o22_x1 port map ( nq=>nx2439, i0=>nx2530, i1=>nx30, i2=>
      nx2735, i3=>nx34);
   ix2531 : nxr2_x1 port map ( nq=>nx2530, i0=>pl, i1=>nx1178);
   ix1179 : nmx2_x1 port map ( nq=>nx1178, cmd=>LOOP_i_1_3, i0=>nx2533, i1=>
      nx2649);
   ix2534 : nmx2_x1 port map ( nq=>nx2533, cmd=>LOOP_i_1_2, i0=>nx482, i1=>
      nx710);
   ix483 : na4_x1 port map ( nq=>nx482, i0=>nx2536, i1=>nx2554, i2=>nx2565, 
      i3=>nx2580);
   ix2537 : noa2a2a23_x1 port map ( nq=>nx2536, i0=>xl_1, i1=>nx442, i2=>
      xl_32, i3=>nx458, i4=>xl_49, i5=>nx472);
   reg_xl_1 : sff2_x4 port map ( q=>xl_1, i0=>xl_1, i1=>nx434, cmd=>nx2744, 
      ck=>conpro_system_clk);
   ix435 : an12_x1 port map ( q=>nx434, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(1));
   ix443 : no4_x4 port map ( nq=>nx442, i0=>LOOP_i_1_5, i1=>LOOP_i_1_4, i2=>
      LOOP_i_1_1, i3=>nx2522);
   reg_xl_32 : sff2_x4 port map ( q=>xl_32, i0=>xl_32, i1=>nx448, cmd=>
      nx2744, ck=>conpro_system_clk);
   ix449 : an12_x1 port map ( q=>nx448, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(32));
   ix459 : no3_x1 port map ( nq=>nx458, i0=>LOOP_i_1_0, i1=>LOOP_i_1_1, i2=>
      nx2547);
   reg_xl_49 : sff2_x4 port map ( q=>xl_49, i0=>xl_49, i1=>nx464, cmd=>
      nx2744, ck=>conpro_system_clk);
   ix465 : an12_x1 port map ( q=>nx464, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(49));
   ix473 : no3_x1 port map ( nq=>nx472, i0=>nx2552, i1=>LOOP_i_1_1, i2=>
      nx2522);
   ix2553 : na2_x1 port map ( nq=>nx2552, i0=>LOOP_i_1_5, i1=>LOOP_i_1_4);
   ix2555 : noa2a2a23_x1 port map ( nq=>nx2554, i0=>xl_0, i1=>nx410, i2=>
      xl_48, i3=>nx394, i4=>xl_51, i5=>nx424);
   reg_xl_0 : sff2_x4 port map ( q=>xl_0, i0=>xl_0, i1=>nx400, cmd=>nx2744, 
      ck=>conpro_system_clk);
   ix401 : an12_x1 port map ( q=>nx400, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(0));
   ix411 : no4_x4 port map ( nq=>nx410, i0=>LOOP_i_1_0, i1=>LOOP_i_1_1, i2=>
      LOOP_i_1_5, i3=>LOOP_i_1_4);
   reg_xl_48 : sff2_x4 port map ( q=>xl_48, i0=>xl_48, i1=>nx384, cmd=>
      nx2746, ck=>conpro_system_clk);
   ix385 : an12_x1 port map ( q=>nx384, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(48));
   ix395 : no3_x1 port map ( nq=>nx394, i0=>LOOP_i_1_0, i1=>LOOP_i_1_1, i2=>
      nx2552);
   reg_xl_51 : sff2_x4 port map ( q=>xl_51, i0=>xl_51, i1=>nx416, cmd=>
      nx2746, ck=>conpro_system_clk);
   ix417 : an12_x1 port map ( q=>nx416, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(51));
   ix425 : no2_x1 port map ( nq=>nx424, i0=>nx2524, i1=>nx2552);
   ix2566 : noa2a2a23_x1 port map ( nq=>nx2565, i0=>xl_19, i1=>nx372, i2=>
      xl_16, i3=>nx358, i4=>xl_17, i5=>nx342);
   reg_xl_19 : sff2_x4 port map ( q=>xl_19, i0=>xl_19, i1=>nx364, cmd=>
      nx2746, ck=>conpro_system_clk);
   ix365 : an12_x1 port map ( q=>nx364, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(19));
   ix373 : no2_x1 port map ( nq=>nx372, i0=>nx2524, i1=>nx2570);
   reg_xl_16 : sff2_x4 port map ( q=>xl_16, i0=>xl_16, i1=>nx348, cmd=>
      nx2746, ck=>conpro_system_clk);
   ix349 : an12_x1 port map ( q=>nx348, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(16));
   ix359 : no3_x1 port map ( nq=>nx358, i0=>LOOP_i_1_0, i1=>LOOP_i_1_1, i2=>
      nx2570);
   reg_xl_17 : sff2_x4 port map ( q=>xl_17, i0=>xl_17, i1=>nx334, cmd=>
      nx2748, ck=>conpro_system_clk);
   ix335 : an12_x1 port map ( q=>nx334, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(17));
   ix343 : no3_x1 port map ( nq=>nx342, i0=>nx2570, i1=>LOOP_i_1_1, i2=>
      nx2522);
   ix2581 : noa2a2a2a24_x1 port map ( nq=>nx2580, i0=>xl_3, i1=>nx324, i2=>
      xl_35, i3=>nx310, i4=>xl_33, i5=>nx294, i6=>nx206, i7=>nx276);
   reg_xl_3 : sff2_x4 port map ( q=>xl_3, i0=>xl_3, i1=>nx316, cmd=>nx2748, 
      ck=>conpro_system_clk);
   ix317 : an12_x1 port map ( q=>nx316, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(3));
   reg_xl_35 : sff2_x4 port map ( q=>xl_35, i0=>xl_35, i1=>nx302, cmd=>
      nx2748, ck=>conpro_system_clk);
   ix303 : an12_x1 port map ( q=>nx302, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(35));
   ix311 : no2_x1 port map ( nq=>nx310, i0=>nx2524, i1=>nx2547);
   reg_xl_33 : sff2_x4 port map ( q=>xl_33, i0=>xl_33, i1=>nx282, cmd=>
      nx2748, ck=>conpro_system_clk);
   ix283 : an12_x1 port map ( q=>nx282, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(33));
   ix295 : no3_x1 port map ( nq=>nx294, i0=>nx2547, i1=>LOOP_i_1_1, i2=>
      nx2522);
   ix277 : oa2a2a2a24_x2 port map ( q=>nx276, i0=>xl_34, i1=>nx270, i2=>
      xl_50, i3=>nx254, i4=>xl_18, i5=>nx238, i6=>xl_2, i7=>nx222);
   reg_xl_34 : sff2_x4 port map ( q=>xl_34, i0=>xl_34, i1=>nx260, cmd=>
      nx2750, ck=>conpro_system_clk);
   ix261 : an12_x1 port map ( q=>nx260, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(34));
   reg_xl_50 : sff2_x4 port map ( q=>xl_50, i0=>xl_50, i1=>nx246, cmd=>
      nx2750, ck=>conpro_system_clk);
   ix247 : an12_x1 port map ( q=>nx246, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(50));
   reg_xl_18 : sff2_x4 port map ( q=>xl_18, i0=>xl_18, i1=>nx228, cmd=>
      nx2750, ck=>conpro_system_clk);
   ix229 : an12_x1 port map ( q=>nx228, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(18));
   reg_xl_2 : sff2_x4 port map ( q=>xl_2, i0=>xl_2, i1=>nx210, cmd=>nx2750, 
      ck=>conpro_system_clk);
   ix211 : an12_x1 port map ( q=>nx210, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(2));
   ix223 : no2_x1 port map ( nq=>nx222, i0=>LOOP_i_1_5, i1=>LOOP_i_1_4);
   ix711 : na4_x1 port map ( nq=>nx710, i0=>nx2608, i1=>nx2616, i2=>nx2624, 
      i3=>nx2632);
   ix2609 : noa2a2a23_x1 port map ( nq=>nx2608, i0=>xl_5, i1=>nx442, i2=>
      xl_36, i3=>nx458, i4=>xl_53, i5=>nx472);
   reg_xl_5 : sff2_x4 port map ( q=>xl_5, i0=>xl_5, i1=>nx670, cmd=>nx2752, 
      ck=>conpro_system_clk);
   ix671 : an12_x1 port map ( q=>nx670, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(5));
   reg_xl_36 : sff2_x4 port map ( q=>xl_36, i0=>xl_36, i1=>nx682, cmd=>
      nx2752, ck=>conpro_system_clk);
   ix683 : an12_x1 port map ( q=>nx682, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(36));
   reg_xl_53 : sff2_x4 port map ( q=>xl_53, i0=>xl_53, i1=>nx694, cmd=>
      nx2752, ck=>conpro_system_clk);
   ix695 : an12_x1 port map ( q=>nx694, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(53));
   ix2617 : noa2a2a23_x1 port map ( nq=>nx2616, i0=>xl_4, i1=>nx410, i2=>
      xl_52, i3=>nx394, i4=>xl_55, i5=>nx424);
   reg_xl_4 : sff2_x4 port map ( q=>xl_4, i0=>xl_4, i1=>nx642, cmd=>nx2752, 
      ck=>conpro_system_clk);
   ix643 : an12_x1 port map ( q=>nx642, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(4));
   reg_xl_52 : sff2_x4 port map ( q=>xl_52, i0=>xl_52, i1=>nx630, cmd=>
      nx2754, ck=>conpro_system_clk);
   ix631 : an12_x1 port map ( q=>nx630, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(52));
   reg_xl_55 : sff2_x4 port map ( q=>xl_55, i0=>xl_55, i1=>nx654, cmd=>
      nx2754, ck=>conpro_system_clk);
   ix655 : an12_x1 port map ( q=>nx654, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(55));
   ix2625 : noa2a2a23_x1 port map ( nq=>nx2624, i0=>xl_23, i1=>nx372, i2=>
      xl_20, i3=>nx358, i4=>xl_21, i5=>nx342);
   reg_xl_23 : sff2_x4 port map ( q=>xl_23, i0=>xl_23, i1=>nx612, cmd=>
      nx2754, ck=>conpro_system_clk);
   ix613 : an12_x1 port map ( q=>nx612, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(23));
   reg_xl_20 : sff2_x4 port map ( q=>xl_20, i0=>xl_20, i1=>nx600, cmd=>
      nx2754, ck=>conpro_system_clk);
   ix601 : an12_x1 port map ( q=>nx600, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(20));
   reg_xl_21 : sff2_x4 port map ( q=>xl_21, i0=>xl_21, i1=>nx588, cmd=>
      nx2756, ck=>conpro_system_clk);
   ix589 : an12_x1 port map ( q=>nx588, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(21));
   ix2633 : noa2a2a2a24_x1 port map ( nq=>nx2632, i0=>xl_7, i1=>nx324, i2=>
      xl_39, i3=>nx310, i4=>xl_37, i5=>nx294, i6=>nx206, i7=>nx540);
   reg_xl_7 : sff2_x4 port map ( q=>xl_7, i0=>xl_7, i1=>nx572, cmd=>nx2756, 
      ck=>conpro_system_clk);
   ix573 : an12_x1 port map ( q=>nx572, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(7));
   reg_xl_39 : sff2_x4 port map ( q=>xl_39, i0=>xl_39, i1=>nx560, cmd=>
      nx2756, ck=>conpro_system_clk);
   ix561 : an12_x1 port map ( q=>nx560, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(39));
   reg_xl_37 : sff2_x4 port map ( q=>xl_37, i0=>xl_37, i1=>nx546, cmd=>
      nx2756, ck=>conpro_system_clk);
   ix547 : an12_x1 port map ( q=>nx546, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(37));
   ix541 : oa2a2a2a24_x2 port map ( q=>nx540, i0=>xl_38, i1=>nx270, i2=>
      xl_54, i3=>nx254, i4=>xl_22, i5=>nx238, i6=>xl_6, i7=>nx222);
   reg_xl_38 : sff2_x4 port map ( q=>xl_38, i0=>xl_38, i1=>nx528, cmd=>
      nx2758, ck=>conpro_system_clk);
   ix529 : an12_x1 port map ( q=>nx528, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(38));
   reg_xl_54 : sff2_x4 port map ( q=>xl_54, i0=>xl_54, i1=>nx516, cmd=>
      nx2758, ck=>conpro_system_clk);
   ix517 : an12_x1 port map ( q=>nx516, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(54));
   reg_xl_22 : sff2_x4 port map ( q=>xl_22, i0=>xl_22, i1=>nx502, cmd=>
      nx2758, ck=>conpro_system_clk);
   ix503 : an12_x1 port map ( q=>nx502, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(22));
   reg_xl_6 : sff2_x4 port map ( q=>xl_6, i0=>xl_6, i1=>nx490, cmd=>nx2758, 
      ck=>conpro_system_clk);
   ix491 : an12_x1 port map ( q=>nx490, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(6));
   ix2650 : nmx2_x1 port map ( nq=>nx2649, cmd=>LOOP_i_1_2, i0=>nx942, i1=>
      nx1170);
   ix943 : na4_x1 port map ( nq=>nx942, i0=>nx2652, i1=>nx2660, i2=>nx2668, 
      i3=>nx2676);
   ix2653 : noa2a2a23_x1 port map ( nq=>nx2652, i0=>xl_9, i1=>nx442, i2=>
      xl_40, i3=>nx458, i4=>xl_57, i5=>nx472);
   reg_xl_9 : sff2_x4 port map ( q=>xl_9, i0=>xl_9, i1=>nx902, cmd=>nx2760, 
      ck=>conpro_system_clk);
   ix903 : an12_x1 port map ( q=>nx902, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(9));
   reg_xl_40 : sff2_x4 port map ( q=>xl_40, i0=>xl_40, i1=>nx914, cmd=>
      nx2760, ck=>conpro_system_clk);
   ix915 : an12_x1 port map ( q=>nx914, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(40));
   reg_xl_57 : sff2_x4 port map ( q=>xl_57, i0=>xl_57, i1=>nx926, cmd=>
      nx2760, ck=>conpro_system_clk);
   ix927 : an12_x1 port map ( q=>nx926, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(57));
   ix2661 : noa2a2a23_x1 port map ( nq=>nx2660, i0=>xl_8, i1=>nx410, i2=>
      xl_56, i3=>nx394, i4=>xl_59, i5=>nx424);
   reg_xl_8 : sff2_x4 port map ( q=>xl_8, i0=>xl_8, i1=>nx874, cmd=>nx2760, 
      ck=>conpro_system_clk);
   ix875 : an12_x1 port map ( q=>nx874, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(8));
   reg_xl_56 : sff2_x4 port map ( q=>xl_56, i0=>xl_56, i1=>nx862, cmd=>
      nx2762, ck=>conpro_system_clk);
   ix863 : an12_x1 port map ( q=>nx862, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(56));
   reg_xl_59 : sff2_x4 port map ( q=>xl_59, i0=>xl_59, i1=>nx886, cmd=>
      nx2762, ck=>conpro_system_clk);
   ix887 : an12_x1 port map ( q=>nx886, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(59));
   ix2669 : noa2a2a23_x1 port map ( nq=>nx2668, i0=>xl_27, i1=>nx372, i2=>
      xl_24, i3=>nx358, i4=>xl_25, i5=>nx342);
   reg_xl_27 : sff2_x4 port map ( q=>xl_27, i0=>xl_27, i1=>nx844, cmd=>
      nx2762, ck=>conpro_system_clk);
   ix845 : an12_x1 port map ( q=>nx844, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(27));
   reg_xl_24 : sff2_x4 port map ( q=>xl_24, i0=>xl_24, i1=>nx832, cmd=>
      nx2762, ck=>conpro_system_clk);
   ix833 : an12_x1 port map ( q=>nx832, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(24));
   reg_xl_25 : sff2_x4 port map ( q=>xl_25, i0=>xl_25, i1=>nx820, cmd=>
      nx2764, ck=>conpro_system_clk);
   ix821 : an12_x1 port map ( q=>nx820, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(25));
   ix2677 : noa2a2a2a24_x1 port map ( nq=>nx2676, i0=>xl_11, i1=>nx324, i2=>
      xl_43, i3=>nx310, i4=>xl_41, i5=>nx294, i6=>nx206, i7=>nx772);
   reg_xl_11 : sff2_x4 port map ( q=>xl_11, i0=>xl_11, i1=>nx804, cmd=>
      nx2764, ck=>conpro_system_clk);
   ix805 : an12_x1 port map ( q=>nx804, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(11));
   reg_xl_43 : sff2_x4 port map ( q=>xl_43, i0=>xl_43, i1=>nx792, cmd=>
      nx2764, ck=>conpro_system_clk);
   ix793 : an12_x1 port map ( q=>nx792, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(43));
   reg_xl_41 : sff2_x4 port map ( q=>xl_41, i0=>xl_41, i1=>nx778, cmd=>
      nx2764, ck=>conpro_system_clk);
   ix779 : an12_x1 port map ( q=>nx778, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(41));
   ix773 : oa2a2a2a24_x2 port map ( q=>nx772, i0=>xl_42, i1=>nx270, i2=>
      xl_58, i3=>nx254, i4=>xl_26, i5=>nx238, i6=>xl_10, i7=>nx222);
   reg_xl_42 : sff2_x4 port map ( q=>xl_42, i0=>xl_42, i1=>nx760, cmd=>
      nx2766, ck=>conpro_system_clk);
   ix761 : an12_x1 port map ( q=>nx760, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(42));
   reg_xl_58 : sff2_x4 port map ( q=>xl_58, i0=>xl_58, i1=>nx748, cmd=>
      nx2766, ck=>conpro_system_clk);
   ix749 : an12_x1 port map ( q=>nx748, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(58));
   reg_xl_26 : sff2_x4 port map ( q=>xl_26, i0=>xl_26, i1=>nx734, cmd=>
      nx2766, ck=>conpro_system_clk);
   ix735 : an12_x1 port map ( q=>nx734, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(26));
   reg_xl_10 : sff2_x4 port map ( q=>xl_10, i0=>xl_10, i1=>nx722, cmd=>
      nx2766, ck=>conpro_system_clk);
   ix723 : an12_x1 port map ( q=>nx722, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(10));
   ix1171 : na4_x1 port map ( nq=>nx1170, i0=>nx2694, i1=>nx2702, i2=>nx2710, 
      i3=>nx2718);
   ix2695 : noa2a2a23_x1 port map ( nq=>nx2694, i0=>xl_13, i1=>nx442, i2=>
      xl_44, i3=>nx458, i4=>xl_61, i5=>nx472);
   reg_xl_13 : sff2_x4 port map ( q=>xl_13, i0=>xl_13, i1=>nx1130, cmd=>
      nx2768, ck=>conpro_system_clk);
   ix1131 : an12_x1 port map ( q=>nx1130, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(13));
   reg_xl_44 : sff2_x4 port map ( q=>xl_44, i0=>xl_44, i1=>nx1142, cmd=>
      nx2768, ck=>conpro_system_clk);
   ix1143 : an12_x1 port map ( q=>nx1142, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(44));
   reg_xl_61 : sff2_x4 port map ( q=>xl_61, i0=>xl_61, i1=>nx1154, cmd=>
      nx2768, ck=>conpro_system_clk);
   ix1155 : an12_x1 port map ( q=>nx1154, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(61));
   ix2703 : noa2a2a23_x1 port map ( nq=>nx2702, i0=>xl_12, i1=>nx410, i2=>
      xl_60, i3=>nx394, i4=>xl_63, i5=>nx424);
   reg_xl_12 : sff2_x4 port map ( q=>xl_12, i0=>xl_12, i1=>nx1102, cmd=>
      nx2768, ck=>conpro_system_clk);
   ix1103 : an12_x1 port map ( q=>nx1102, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(12));
   reg_xl_60 : sff2_x4 port map ( q=>xl_60, i0=>xl_60, i1=>nx1090, cmd=>
      nx2770, ck=>conpro_system_clk);
   ix1091 : an12_x1 port map ( q=>nx1090, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(60));
   reg_xl_63 : sff2_x4 port map ( q=>xl_63, i0=>xl_63, i1=>nx1114, cmd=>
      nx2770, ck=>conpro_system_clk);
   ix1115 : an12_x1 port map ( q=>nx1114, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(63));
   ix2711 : noa2a2a23_x1 port map ( nq=>nx2710, i0=>xl_31, i1=>nx372, i2=>
      xl_28, i3=>nx358, i4=>xl_29, i5=>nx342);
   reg_xl_31 : sff2_x4 port map ( q=>xl_31, i0=>xl_31, i1=>nx1072, cmd=>
      nx2770, ck=>conpro_system_clk);
   ix1073 : an12_x1 port map ( q=>nx1072, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(31));
   reg_xl_28 : sff2_x4 port map ( q=>xl_28, i0=>xl_28, i1=>nx1060, cmd=>
      nx2770, ck=>conpro_system_clk);
   ix1061 : an12_x1 port map ( q=>nx1060, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(28));
   reg_xl_29 : sff2_x4 port map ( q=>xl_29, i0=>xl_29, i1=>nx1048, cmd=>
      nx2772, ck=>conpro_system_clk);
   ix1049 : an12_x1 port map ( q=>nx1048, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(29));
   ix2719 : noa2a2a2a24_x1 port map ( nq=>nx2718, i0=>xl_15, i1=>nx324, i2=>
      xl_47, i3=>nx310, i4=>xl_45, i5=>nx294, i6=>nx206, i7=>nx1000);
   reg_xl_15 : sff2_x4 port map ( q=>xl_15, i0=>xl_15, i1=>nx1032, cmd=>
      nx2772, ck=>conpro_system_clk);
   ix1033 : an12_x1 port map ( q=>nx1032, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(15));
   reg_xl_47 : sff2_x4 port map ( q=>xl_47, i0=>xl_47, i1=>nx1020, cmd=>
      nx2772, ck=>conpro_system_clk);
   ix1021 : an12_x1 port map ( q=>nx1020, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(47));
   reg_xl_45 : sff2_x4 port map ( q=>xl_45, i0=>xl_45, i1=>nx1006, cmd=>
      nx2772, ck=>conpro_system_clk);
   ix1007 : an12_x1 port map ( q=>nx1006, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(45));
   ix1001 : oa2a2a2a24_x2 port map ( q=>nx1000, i0=>xl_46, i1=>nx270, i2=>
      xl_62, i3=>nx254, i4=>xl_30, i5=>nx238, i6=>xl_14, i7=>nx222);
   reg_xl_46 : sff2_x4 port map ( q=>xl_46, i0=>xl_46, i1=>nx988, cmd=>nx20, 
      ck=>conpro_system_clk);
   ix989 : an12_x1 port map ( q=>nx988, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(46));
   reg_xl_62 : sff2_x4 port map ( q=>xl_62, i0=>xl_62, i1=>nx976, cmd=>nx20, 
      ck=>conpro_system_clk);
   ix977 : an12_x1 port map ( q=>nx976, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(62));
   reg_xl_30 : sff2_x4 port map ( q=>xl_30, i0=>xl_30, i1=>nx962, cmd=>nx20, 
      ck=>conpro_system_clk);
   ix963 : an12_x1 port map ( q=>nx962, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(30));
   reg_xl_14 : sff2_x4 port map ( q=>xl_14, i0=>xl_14, i1=>nx950, cmd=>nx20, 
      ck=>conpro_system_clk);
   ix951 : an12_x1 port map ( q=>nx950, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity2_x_RD(14));
   ix2736 : inv_x1 port map ( nq=>nx2735, i=>pl);
   reg_pro_state_5 : sff1_x4 port map ( q=>PRO_FUN_parity2_END_EXMPLR, i=>
      nx1198, ck=>conpro_system_clk);
   ix1199 : ao22_x2 port map ( q=>nx1198, i0=>
      REG_RET_FUN_parity2_p_WE_EXMPLR, i1=>PRO_FUN_parity2_END_EXMPLR, i2=>
      nx2516);
   ix271 : inv_x1 port map ( nq=>nx270, i=>nx2547);
   ix255 : inv_x1 port map ( nq=>nx254, i=>nx2552);
   ix239 : inv_x1 port map ( nq=>nx238, i=>nx2570);
   ix137 : inv_x1 port map ( nq=>nx1711, i=>nx2499);
   ix101 : inv_x1 port map ( nq=>nx1709, i=>nx2526);
   ix65 : inv_x1 port map ( nq=>nx1707, i=>nx2524);
   ix21 : inv_x1 port map ( nq=>nx20, i=>nx2504);
   ix2517 : inv_x1 port map ( nq=>nx2516, i=>nx2);
   ix2743 : inv_x1 port map ( nq=>nx2744, i=>nx2504);
   ix2745 : inv_x1 port map ( nq=>nx2746, i=>nx2504);
   ix2747 : inv_x1 port map ( nq=>nx2748, i=>nx2504);
   ix2749 : inv_x1 port map ( nq=>nx2750, i=>nx2504);
   ix2751 : inv_x1 port map ( nq=>nx2752, i=>nx2504);
   ix2753 : inv_x1 port map ( nq=>nx2754, i=>nx2504);
   ix2755 : inv_x1 port map ( nq=>nx2756, i=>nx2504);
   ix2757 : inv_x1 port map ( nq=>nx2758, i=>nx2504);
   ix2759 : inv_x1 port map ( nq=>nx2760, i=>nx2504);
   ix2761 : inv_x1 port map ( nq=>nx2762, i=>nx2504);
   ix2763 : inv_x1 port map ( nq=>nx2764, i=>nx2504);
   ix2765 : inv_x1 port map ( nq=>nx2766, i=>nx2504);
   ix2767 : inv_x1 port map ( nq=>nx2768, i=>nx2504);
   ix2769 : inv_x1 port map ( nq=>nx2770, i=>nx2504);
   ix2771 : inv_x1 port map ( nq=>nx2772, i=>nx2504);
   ix2457 : an12_x1 port map ( q=>nx2456, i0=>LOOP_i_1_6, i1=>nx2499);
   ix119 : an12_x1 port map ( q=>nx1710, i0=>nx2526, i1=>LOOP_i_1_4);
   ix2469 : an12_x1 port map ( q=>nx2468, i0=>LOOP_i_1_4, i1=>nx2526);
   ix83 : an12_x1 port map ( q=>nx1708, i0=>nx2524, i1=>LOOP_i_1_2);
   ix2483 : an12_x1 port map ( q=>nx2482, i0=>LOOP_i_1_2, i1=>nx2524);
   ix31 : on12_x4 port map ( q=>nx30, i0=>pro_state_3, i1=>
      conpro_system_reset);
   ix2493 : an12_x1 port map ( q=>nx2492, i0=>LOOP_i_1_7, i1=>LOOP_i_1_6);
   ix155 : an12_x1 port map ( q=>nx1712, i0=>nx2499, i1=>LOOP_i_1_6);
   ix3 : on12_x1 port map ( q=>nx2, i0=>PRO_FUN_parity2_ENABLE, i1=>
      conpro_system_reset);
   ix2548 : on12_x1 port map ( q=>nx2547, i0=>LOOP_i_1_5, i1=>LOOP_i_1_4);
   ix2571 : on12_x1 port map ( q=>nx2570, i0=>LOOP_i_1_4, i1=>LOOP_i_1_5);
   ix325 : an12_x1 port map ( q=>nx324, i0=>nx2524, i1=>nx222);
   ix207 : an12_x1 port map ( q=>nx206, i0=>LOOP_i_1_0, i1=>LOOP_i_1_1);

end main ;

