
-- 
-- Definition of  par_FUN_parity1
-- 
--      Fri Dec 19 11:31:38 2008
--      
--      LeonardoSpectrum Level 3, 2005b.24
-- 

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity par_FUN_parity1 is
   port (
      REG_ARG_FUN_parity1_x_RD : IN std_logic_vector (63 DOWNTO 0) ;
      REG_RET_FUN_parity1_p_WR : OUT std_logic ;
      REG_RET_FUN_parity1_p_WE : OUT std_logic ;
      PRO_FUN_parity1_ENABLE : IN std_logic ;
      PRO_FUN_parity1_END : OUT std_logic ;
      conpro_system_clk : IN std_logic ;
      conpro_system_reset : IN std_logic) ;
end par_FUN_parity1 ;

architecture main of par_FUN_parity1 is
   signal PRO_FUN_parity1_END_EXMPLR, REG_RET_FUN_parity1_p_WE_EXMPLR, 
      LOOP_i_0_7, pro_state_3, pro_state_2, pro_state_1, pro_state_0, nx2, 
      nx12, nx22, nx32, pro_state_6, pro_state_5, nx1765, nx48, nx58, 
      LOOP_i_0_6, LOOP_i_0_5, LOOP_i_0_4, LOOP_i_0_3, LOOP_i_0_2, LOOP_i_0_1, 
      nx1766, nx62, LOOP_i_0_0, nx68, nx84, nx1767, nx102, nx1768, nx120, 
      nx1769, nx138, nx1770, nx156, nx1771, nx174, nx188, pro_state_4, nx204, 
      nx224, pl, xl_2, nx236, nx240, nx258, xl_18, nx264, nx276, xl_50, 
      nx284, nx294, xl_34, nx300, nx312, xl_33, nx322, nx334, xl_35, nx340, 
      nx348, xl_3, nx356, nx364, xl_16, nx370, nx380, xl_19, nx392, nx400, 
      xl_17, nx406, nx414, xl_0, nx422, nx432, xl_51, nx438, nx446, xl_48, 
      nx456, nx466, xl_32, nx472, nx482, xl_49, nx490, nx498, xl_1, nx504, 
      nx512, nx522, xl_6, nx530, xl_22, nx542, xl_54, nx556, xl_38, nx568, 
      xl_37, nx584, xl_39, nx596, xl_7, nx610, xl_20, nx622, xl_23, nx640, 
      xl_21, nx652, xl_4, nx666, xl_55, nx678, xl_52, nx694, xl_36, nx706, 
      xl_53, nx720, xl_5, nx732, nx748, xl_10, nx760, xl_26, nx772, xl_58, 
      nx786, xl_42, nx798, xl_41, nx814, xl_43, nx826, xl_11, nx840, xl_24, 
      nx852, xl_27, nx870, xl_25, nx882, xl_8, nx896, xl_59, nx908, xl_56, 
      nx924, xl_40, nx936, xl_57, nx950, xl_9, nx962, nx978, xl_14, nx986, 
      xl_30, nx998, xl_62, nx1012, xl_46, nx1024, xl_45, nx1040, xl_47, 
      nx1052, xl_15, nx1066, xl_28, nx1078, xl_31, nx1096, xl_29, nx1108, 
      xl_12, nx1122, xl_63, nx1134, xl_60, nx1150, xl_44, nx1162, xl_61, 
      nx1176, xl_13, nx1188, nx1204, nx1212, nx1234, nx2496, nx2498, nx2511, 
      nx2515, nx2520, nx2527, nx2534, nx2541, nx2551, nx2555, nx2559, nx2562, 
      nx2579, nx2583, nx2585, nx2589, nx2591, nx2593, nx2597, nx2600, nx2603, 
      nx2606, nx2615, nx2620, nx2625, nx2636, nx2643, nx2657, nx2662, nx2674, 
      nx2684, nx2694, nx2704, nx2714, nx2717, nx2727, nx2737, nx2747, nx2758, 
      nx2768, nx2778, nx2788, nx2805, nx2807, nx2809, nx2811, nx2813, nx2815, 
      nx2817, nx2819, nx2821, nx2823, nx2825, nx2827, nx2829, nx2831, nx2833
   : std_logic ;

begin
   REG_RET_FUN_parity1_p_WE <= REG_RET_FUN_parity1_p_WE_EXMPLR ;
   PRO_FUN_parity1_END <= PRO_FUN_parity1_END_EXMPLR ;
   ix1229 : a2_x2 port map ( q=>REG_RET_FUN_parity1_p_WR, i0=>
      REG_RET_FUN_parity1_p_WE_EXMPLR, i1=>pl);
   reg_pro_state_7 : sff1_x4 port map ( q=>REG_RET_FUN_parity1_p_WE_EXMPLR, 
      i=>nx224, ck=>conpro_system_clk);
   ix225 : no4_x1 port map ( nq=>nx224, i0=>nx2511, i1=>LOOP_i_0_7, i2=>
      nx2579, i3=>nx2);
   ix2512 : inv_x1 port map ( nq=>nx2511, i=>LOOP_i_0_6);
   LOOP_i_0_6_EXMPLR : sff2_x4 port map ( q=>LOOP_i_0_6, i0=>LOOP_i_0_6, i1
      =>nx174, cmd=>nx58, ck=>conpro_system_clk);
   ix175 : no3_x1 port map ( nq=>nx174, i0=>nx2515, i1=>nx1771, i2=>nx62);
   LOOP_i_0_5_EXMPLR : sff2_x4 port map ( q=>LOOP_i_0_5, i0=>LOOP_i_0_5, i1
      =>nx156, cmd=>nx58, ck=>conpro_system_clk);
   ix157 : no3_x1 port map ( nq=>nx156, i0=>nx2520, i1=>nx1770, i2=>nx62);
   ix2521 : no2_x1 port map ( nq=>nx2520, i0=>nx1769, i1=>LOOP_i_0_5);
   LOOP_i_0_4_EXMPLR : sff2_x4 port map ( q=>LOOP_i_0_4, i0=>LOOP_i_0_4, i1
      =>nx138, cmd=>nx58, ck=>conpro_system_clk);
   ix139 : no3_x1 port map ( nq=>nx138, i0=>nx2527, i1=>nx1769, i2=>nx62);
   LOOP_i_0_3_EXMPLR : sff2_x4 port map ( q=>LOOP_i_0_3, i0=>LOOP_i_0_3, i1
      =>nx120, cmd=>nx58, ck=>conpro_system_clk);
   ix121 : no3_x1 port map ( nq=>nx120, i0=>nx2534, i1=>nx1768, i2=>nx62);
   ix2535 : no2_x1 port map ( nq=>nx2534, i0=>nx1767, i1=>LOOP_i_0_3);
   LOOP_i_0_2_EXMPLR : sff2_x4 port map ( q=>LOOP_i_0_2, i0=>LOOP_i_0_2, i1
      =>nx102, cmd=>nx58, ck=>conpro_system_clk);
   ix103 : no3_x1 port map ( nq=>nx102, i0=>nx2541, i1=>nx1767, i2=>nx62);
   LOOP_i_0_1_EXMPLR : sff2_x4 port map ( q=>LOOP_i_0_1, i0=>LOOP_i_0_1, i1
      =>nx84, cmd=>nx58, ck=>conpro_system_clk);
   ix85 : no3_x1 port map ( nq=>nx84, i0=>nx1766, i1=>nx62, i2=>nx2585);
   reg_pro_state_6 : sff1_x4 port map ( q=>pro_state_6, i=>nx48, ck=>
      conpro_system_clk);
   ix2552 : inv_x1 port map ( nq=>nx2551, i=>pro_state_5);
   reg_pro_state_5 : sff1_x4 port map ( q=>pro_state_5, i=>nx1765, ck=>
      conpro_system_clk);
   ix217 : no3_x1 port map ( nq=>nx1765, i0=>nx2555, i1=>nx2579, i2=>nx2);
   LOOP_i_0_7_EXMPLR : sff2_x4 port map ( q=>LOOP_i_0_7, i0=>LOOP_i_0_7, i1
      =>nx188, cmd=>nx58, ck=>conpro_system_clk);
   ix189 : no2_x1 port map ( nq=>nx188, i0=>nx2559, i1=>nx62);
   ix2560 : nxr2_x1 port map ( nq=>nx2559, i0=>LOOP_i_0_7, i1=>nx1771);
   ix2563 : na2_x1 port map ( nq=>nx2562, i0=>LOOP_i_0_5, i1=>nx1769);
   ix59 : o3_x2 port map ( q=>nx58, i0=>pro_state_6, i1=>conpro_system_reset, 
      i2=>pro_state_3);
   reg_pro_state_3 : sff1_x4 port map ( q=>pro_state_3, i=>nx32, ck=>
      conpro_system_clk);
   reg_pro_state_2 : sff1_x4 port map ( q=>pro_state_2, i=>nx22, ck=>
      conpro_system_clk);
   reg_pro_state_1 : sff1_x4 port map ( q=>pro_state_1, i=>nx12, ck=>
      conpro_system_clk);
   ix13 : an12_x1 port map ( q=>nx12, i0=>nx2, i1=>pro_state_0);
   reg_pro_state_0 : sff1_x4 port map ( q=>pro_state_0, i=>nx2, ck=>
      conpro_system_clk);
   ix2580 : inv_x1 port map ( nq=>nx2579, i=>pro_state_4);
   reg_pro_state_4 : sff1_x4 port map ( q=>pro_state_4, i=>nx204, ck=>
      conpro_system_clk);
   ix205 : ao22_x2 port map ( q=>nx204, i0=>pro_state_3, i1=>pro_state_6, i2
      =>nx2583);
   ix2586 : no2_x1 port map ( nq=>nx2585, i0=>LOOP_i_0_0, i1=>LOOP_i_0_1);
   LOOP_i_0_0_EXMPLR : sff2_x4 port map ( q=>LOOP_i_0_0, i0=>LOOP_i_0_0, i1
      =>nx68, cmd=>nx58, ck=>conpro_system_clk);
   ix69 : no2_x1 port map ( nq=>nx68, i0=>LOOP_i_0_0, i1=>nx62);
   ix2590 : inv_x1 port map ( nq=>nx2589, i=>LOOP_i_0_0);
   ix2592 : na2_x1 port map ( nq=>nx2591, i0=>LOOP_i_0_1, i1=>LOOP_i_0_0);
   ix2594 : na2_x1 port map ( nq=>nx2593, i0=>LOOP_i_0_3, i1=>nx1767);
   reg_pl : sff1_x4 port map ( q=>pl, i=>nx2498, ck=>conpro_system_clk);
   ix2499 : oa22_x2 port map ( q=>nx2498, i0=>pl, i1=>nx2597, i2=>nx2496);
   ix2598 : no3_x1 port map ( nq=>nx2597, i0=>pro_state_5, i1=>
      conpro_system_reset, i2=>pro_state_2);
   ix2497 : no3_x1 port map ( nq=>nx2496, i0=>nx2600, i1=>
      conpro_system_reset, i2=>nx2551);
   ix2601 : nxr2_x1 port map ( nq=>nx2600, i0=>pl, i1=>nx1212);
   ix1213 : nmx2_x1 port map ( nq=>nx1212, cmd=>LOOP_i_0_3, i0=>nx2603, i1=>
      nx2714);
   ix2604 : nmx2_x1 port map ( nq=>nx2603, cmd=>LOOP_i_0_2, i0=>nx522, i1=>
      nx748);
   ix523 : na4_x1 port map ( nq=>nx522, i0=>nx2606, i1=>nx2625, i2=>nx2643, 
      i3=>nx2657);
   ix2607 : noa2a2a2a24_x1 port map ( nq=>nx2606, i0=>xl_1, i1=>nx512, i2=>
      xl_49, i3=>nx498, i4=>xl_32, i5=>nx482, i6=>xl_48, i7=>nx466);
   reg_xl_1 : sff2_x4 port map ( q=>xl_1, i0=>xl_1, i1=>nx504, cmd=>nx2805, 
      ck=>conpro_system_clk);
   ix505 : an12_x1 port map ( q=>nx504, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(1));
   ix513 : no4_x4 port map ( nq=>nx512, i0=>LOOP_i_0_5, i1=>LOOP_i_0_4, i2=>
      LOOP_i_0_1, i3=>nx2589);
   reg_xl_49 : sff2_x4 port map ( q=>xl_49, i0=>xl_49, i1=>nx490, cmd=>
      nx2805, ck=>conpro_system_clk);
   ix491 : an12_x1 port map ( q=>nx490, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(49));
   ix499 : no3_x1 port map ( nq=>nx498, i0=>nx2615, i1=>LOOP_i_0_1, i2=>
      nx2589);
   ix2616 : na2_x1 port map ( nq=>nx2615, i0=>LOOP_i_0_5, i1=>LOOP_i_0_4);
   reg_xl_32 : sff2_x4 port map ( q=>xl_32, i0=>xl_32, i1=>nx472, cmd=>
      nx2805, ck=>conpro_system_clk);
   ix473 : an12_x1 port map ( q=>nx472, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(32));
   ix483 : no3_x1 port map ( nq=>nx482, i0=>LOOP_i_0_0, i1=>LOOP_i_0_1, i2=>
      nx2620);
   reg_xl_48 : sff2_x4 port map ( q=>xl_48, i0=>xl_48, i1=>nx456, cmd=>
      nx2805, ck=>conpro_system_clk);
   ix457 : an12_x1 port map ( q=>nx456, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(48));
   ix467 : no3_x1 port map ( nq=>nx466, i0=>LOOP_i_0_0, i1=>LOOP_i_0_1, i2=>
      nx2615);
   ix2626 : noa2a2a2a24_x1 port map ( nq=>nx2625, i0=>xl_51, i1=>nx446, i2=>
      xl_0, i3=>nx432, i4=>xl_17, i5=>nx414, i6=>xl_19, i7=>nx400);
   reg_xl_51 : sff2_x4 port map ( q=>xl_51, i0=>xl_51, i1=>nx438, cmd=>
      nx2807, ck=>conpro_system_clk);
   ix439 : an12_x1 port map ( q=>nx438, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(51));
   ix447 : no2_x1 port map ( nq=>nx446, i0=>nx2591, i1=>nx2615);
   reg_xl_0 : sff2_x4 port map ( q=>xl_0, i0=>xl_0, i1=>nx422, cmd=>nx2807, 
      ck=>conpro_system_clk);
   ix423 : an12_x1 port map ( q=>nx422, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(0));
   ix433 : no4_x4 port map ( nq=>nx432, i0=>LOOP_i_0_0, i1=>LOOP_i_0_1, i2=>
      LOOP_i_0_5, i3=>LOOP_i_0_4);
   reg_xl_17 : sff2_x4 port map ( q=>xl_17, i0=>xl_17, i1=>nx406, cmd=>
      nx2807, ck=>conpro_system_clk);
   ix407 : an12_x1 port map ( q=>nx406, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(17));
   ix415 : no3_x1 port map ( nq=>nx414, i0=>nx2636, i1=>LOOP_i_0_1, i2=>
      nx2589);
   reg_xl_19 : sff2_x4 port map ( q=>xl_19, i0=>xl_19, i1=>nx392, cmd=>
      nx2807, ck=>conpro_system_clk);
   ix393 : an12_x1 port map ( q=>nx392, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(19));
   ix401 : no2_x1 port map ( nq=>nx400, i0=>nx2591, i1=>nx2636);
   ix2644 : noa2a2a2a24_x1 port map ( nq=>nx2643, i0=>xl_16, i1=>nx380, i2=>
      xl_3, i3=>nx364, i4=>xl_35, i5=>nx348, i6=>xl_33, i7=>nx334);
   reg_xl_16 : sff2_x4 port map ( q=>xl_16, i0=>xl_16, i1=>nx370, cmd=>
      nx2809, ck=>conpro_system_clk);
   ix371 : an12_x1 port map ( q=>nx370, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(16));
   ix381 : no3_x1 port map ( nq=>nx380, i0=>LOOP_i_0_0, i1=>LOOP_i_0_1, i2=>
      nx2636);
   reg_xl_3 : sff2_x4 port map ( q=>xl_3, i0=>xl_3, i1=>nx356, cmd=>nx2809, 
      ck=>conpro_system_clk);
   ix357 : an12_x1 port map ( q=>nx356, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(3));
   ix365 : no3_x1 port map ( nq=>nx364, i0=>nx2591, i1=>LOOP_i_0_5, i2=>
      LOOP_i_0_4);
   reg_xl_35 : sff2_x4 port map ( q=>xl_35, i0=>xl_35, i1=>nx340, cmd=>
      nx2809, ck=>conpro_system_clk);
   ix341 : an12_x1 port map ( q=>nx340, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(35));
   ix349 : no2_x1 port map ( nq=>nx348, i0=>nx2591, i1=>nx2620);
   reg_xl_33 : sff2_x4 port map ( q=>xl_33, i0=>xl_33, i1=>nx322, cmd=>
      nx2809, ck=>conpro_system_clk);
   ix323 : an12_x1 port map ( q=>nx322, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(33));
   ix335 : no3_x1 port map ( nq=>nx334, i0=>nx2620, i1=>LOOP_i_0_1, i2=>
      nx2589);
   ix2658 : noa2a2a2a24_x1 port map ( nq=>nx2657, i0=>xl_34, i1=>nx312, i2=>
      xl_50, i3=>nx294, i4=>xl_18, i5=>nx276, i6=>xl_2, i7=>nx258);
   reg_xl_34 : sff2_x4 port map ( q=>xl_34, i0=>xl_34, i1=>nx300, cmd=>
      nx2811, ck=>conpro_system_clk);
   ix301 : an12_x1 port map ( q=>nx300, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(34));
   ix313 : no2_x1 port map ( nq=>nx312, i0=>nx2662, i1=>nx2620);
   reg_xl_50 : sff2_x4 port map ( q=>xl_50, i0=>xl_50, i1=>nx284, cmd=>
      nx2811, ck=>conpro_system_clk);
   ix285 : an12_x1 port map ( q=>nx284, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(50));
   ix295 : no2_x1 port map ( nq=>nx294, i0=>nx2662, i1=>nx2615);
   reg_xl_18 : sff2_x4 port map ( q=>xl_18, i0=>xl_18, i1=>nx264, cmd=>
      nx2811, ck=>conpro_system_clk);
   ix265 : an12_x1 port map ( q=>nx264, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(18));
   ix277 : no2_x1 port map ( nq=>nx276, i0=>nx2662, i1=>nx2636);
   reg_xl_2 : sff2_x4 port map ( q=>xl_2, i0=>xl_2, i1=>nx240, cmd=>nx2811, 
      ck=>conpro_system_clk);
   ix241 : an12_x1 port map ( q=>nx240, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(2));
   ix259 : no3_x1 port map ( nq=>nx258, i0=>nx2662, i1=>LOOP_i_0_5, i2=>
      LOOP_i_0_4);
   ix749 : na4_x1 port map ( nq=>nx748, i0=>nx2674, i1=>nx2684, i2=>nx2694, 
      i3=>nx2704);
   ix2675 : noa2a2a2a24_x1 port map ( nq=>nx2674, i0=>xl_5, i1=>nx512, i2=>
      xl_53, i3=>nx498, i4=>xl_36, i5=>nx482, i6=>xl_52, i7=>nx466);
   reg_xl_5 : sff2_x4 port map ( q=>xl_5, i0=>xl_5, i1=>nx732, cmd=>nx2813, 
      ck=>conpro_system_clk);
   ix733 : an12_x1 port map ( q=>nx732, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(5));
   reg_xl_53 : sff2_x4 port map ( q=>xl_53, i0=>xl_53, i1=>nx720, cmd=>
      nx2813, ck=>conpro_system_clk);
   ix721 : an12_x1 port map ( q=>nx720, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(53));
   reg_xl_36 : sff2_x4 port map ( q=>xl_36, i0=>xl_36, i1=>nx706, cmd=>
      nx2813, ck=>conpro_system_clk);
   ix707 : an12_x1 port map ( q=>nx706, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(36));
   reg_xl_52 : sff2_x4 port map ( q=>xl_52, i0=>xl_52, i1=>nx694, cmd=>
      nx2813, ck=>conpro_system_clk);
   ix695 : an12_x1 port map ( q=>nx694, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(52));
   ix2685 : noa2a2a2a24_x1 port map ( nq=>nx2684, i0=>xl_55, i1=>nx446, i2=>
      xl_4, i3=>nx432, i4=>xl_21, i5=>nx414, i6=>xl_23, i7=>nx400);
   reg_xl_55 : sff2_x4 port map ( q=>xl_55, i0=>xl_55, i1=>nx678, cmd=>
      nx2815, ck=>conpro_system_clk);
   ix679 : an12_x1 port map ( q=>nx678, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(55));
   reg_xl_4 : sff2_x4 port map ( q=>xl_4, i0=>xl_4, i1=>nx666, cmd=>nx2815, 
      ck=>conpro_system_clk);
   ix667 : an12_x1 port map ( q=>nx666, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(4));
   reg_xl_21 : sff2_x4 port map ( q=>xl_21, i0=>xl_21, i1=>nx652, cmd=>
      nx2815, ck=>conpro_system_clk);
   ix653 : an12_x1 port map ( q=>nx652, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(21));
   reg_xl_23 : sff2_x4 port map ( q=>xl_23, i0=>xl_23, i1=>nx640, cmd=>
      nx2815, ck=>conpro_system_clk);
   ix641 : an12_x1 port map ( q=>nx640, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(23));
   ix2695 : noa2a2a2a24_x1 port map ( nq=>nx2694, i0=>xl_20, i1=>nx380, i2=>
      xl_7, i3=>nx364, i4=>xl_39, i5=>nx348, i6=>xl_37, i7=>nx334);
   reg_xl_20 : sff2_x4 port map ( q=>xl_20, i0=>xl_20, i1=>nx622, cmd=>
      nx2817, ck=>conpro_system_clk);
   ix623 : an12_x1 port map ( q=>nx622, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(20));
   reg_xl_7 : sff2_x4 port map ( q=>xl_7, i0=>xl_7, i1=>nx610, cmd=>nx2817, 
      ck=>conpro_system_clk);
   ix611 : an12_x1 port map ( q=>nx610, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(7));
   reg_xl_39 : sff2_x4 port map ( q=>xl_39, i0=>xl_39, i1=>nx596, cmd=>
      nx2817, ck=>conpro_system_clk);
   ix597 : an12_x1 port map ( q=>nx596, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(39));
   reg_xl_37 : sff2_x4 port map ( q=>xl_37, i0=>xl_37, i1=>nx584, cmd=>
      nx2817, ck=>conpro_system_clk);
   ix585 : an12_x1 port map ( q=>nx584, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(37));
   ix2705 : noa2a2a2a24_x1 port map ( nq=>nx2704, i0=>xl_38, i1=>nx312, i2=>
      xl_54, i3=>nx294, i4=>xl_22, i5=>nx276, i6=>xl_6, i7=>nx258);
   reg_xl_38 : sff2_x4 port map ( q=>xl_38, i0=>xl_38, i1=>nx568, cmd=>
      nx2819, ck=>conpro_system_clk);
   ix569 : an12_x1 port map ( q=>nx568, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(38));
   reg_xl_54 : sff2_x4 port map ( q=>xl_54, i0=>xl_54, i1=>nx556, cmd=>
      nx2819, ck=>conpro_system_clk);
   ix557 : an12_x1 port map ( q=>nx556, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(54));
   reg_xl_22 : sff2_x4 port map ( q=>xl_22, i0=>xl_22, i1=>nx542, cmd=>
      nx2819, ck=>conpro_system_clk);
   ix543 : an12_x1 port map ( q=>nx542, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(22));
   reg_xl_6 : sff2_x4 port map ( q=>xl_6, i0=>xl_6, i1=>nx530, cmd=>nx2819, 
      ck=>conpro_system_clk);
   ix531 : an12_x1 port map ( q=>nx530, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(6));
   ix2715 : nmx2_x1 port map ( nq=>nx2714, cmd=>LOOP_i_0_2, i0=>nx978, i1=>
      nx1204);
   ix979 : na4_x1 port map ( nq=>nx978, i0=>nx2717, i1=>nx2727, i2=>nx2737, 
      i3=>nx2747);
   ix2718 : noa2a2a2a24_x1 port map ( nq=>nx2717, i0=>xl_9, i1=>nx512, i2=>
      xl_57, i3=>nx498, i4=>xl_40, i5=>nx482, i6=>xl_56, i7=>nx466);
   reg_xl_9 : sff2_x4 port map ( q=>xl_9, i0=>xl_9, i1=>nx962, cmd=>nx2821, 
      ck=>conpro_system_clk);
   ix963 : an12_x1 port map ( q=>nx962, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(9));
   reg_xl_57 : sff2_x4 port map ( q=>xl_57, i0=>xl_57, i1=>nx950, cmd=>
      nx2821, ck=>conpro_system_clk);
   ix951 : an12_x1 port map ( q=>nx950, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(57));
   reg_xl_40 : sff2_x4 port map ( q=>xl_40, i0=>xl_40, i1=>nx936, cmd=>
      nx2821, ck=>conpro_system_clk);
   ix937 : an12_x1 port map ( q=>nx936, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(40));
   reg_xl_56 : sff2_x4 port map ( q=>xl_56, i0=>xl_56, i1=>nx924, cmd=>
      nx2821, ck=>conpro_system_clk);
   ix925 : an12_x1 port map ( q=>nx924, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(56));
   ix2728 : noa2a2a2a24_x1 port map ( nq=>nx2727, i0=>xl_59, i1=>nx446, i2=>
      xl_8, i3=>nx432, i4=>xl_25, i5=>nx414, i6=>xl_27, i7=>nx400);
   reg_xl_59 : sff2_x4 port map ( q=>xl_59, i0=>xl_59, i1=>nx908, cmd=>
      nx2823, ck=>conpro_system_clk);
   ix909 : an12_x1 port map ( q=>nx908, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(59));
   reg_xl_8 : sff2_x4 port map ( q=>xl_8, i0=>xl_8, i1=>nx896, cmd=>nx2823, 
      ck=>conpro_system_clk);
   ix897 : an12_x1 port map ( q=>nx896, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(8));
   reg_xl_25 : sff2_x4 port map ( q=>xl_25, i0=>xl_25, i1=>nx882, cmd=>
      nx2823, ck=>conpro_system_clk);
   ix883 : an12_x1 port map ( q=>nx882, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(25));
   reg_xl_27 : sff2_x4 port map ( q=>xl_27, i0=>xl_27, i1=>nx870, cmd=>
      nx2823, ck=>conpro_system_clk);
   ix871 : an12_x1 port map ( q=>nx870, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(27));
   ix2738 : noa2a2a2a24_x1 port map ( nq=>nx2737, i0=>xl_24, i1=>nx380, i2=>
      xl_11, i3=>nx364, i4=>xl_43, i5=>nx348, i6=>xl_41, i7=>nx334);
   reg_xl_24 : sff2_x4 port map ( q=>xl_24, i0=>xl_24, i1=>nx852, cmd=>
      nx2825, ck=>conpro_system_clk);
   ix853 : an12_x1 port map ( q=>nx852, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(24));
   reg_xl_11 : sff2_x4 port map ( q=>xl_11, i0=>xl_11, i1=>nx840, cmd=>
      nx2825, ck=>conpro_system_clk);
   ix841 : an12_x1 port map ( q=>nx840, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(11));
   reg_xl_43 : sff2_x4 port map ( q=>xl_43, i0=>xl_43, i1=>nx826, cmd=>
      nx2825, ck=>conpro_system_clk);
   ix827 : an12_x1 port map ( q=>nx826, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(43));
   reg_xl_41 : sff2_x4 port map ( q=>xl_41, i0=>xl_41, i1=>nx814, cmd=>
      nx2825, ck=>conpro_system_clk);
   ix815 : an12_x1 port map ( q=>nx814, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(41));
   ix2748 : noa2a2a2a24_x1 port map ( nq=>nx2747, i0=>xl_42, i1=>nx312, i2=>
      xl_58, i3=>nx294, i4=>xl_26, i5=>nx276, i6=>xl_10, i7=>nx258);
   reg_xl_42 : sff2_x4 port map ( q=>xl_42, i0=>xl_42, i1=>nx798, cmd=>
      nx2827, ck=>conpro_system_clk);
   ix799 : an12_x1 port map ( q=>nx798, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(42));
   reg_xl_58 : sff2_x4 port map ( q=>xl_58, i0=>xl_58, i1=>nx786, cmd=>
      nx2827, ck=>conpro_system_clk);
   ix787 : an12_x1 port map ( q=>nx786, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(58));
   reg_xl_26 : sff2_x4 port map ( q=>xl_26, i0=>xl_26, i1=>nx772, cmd=>
      nx2827, ck=>conpro_system_clk);
   ix773 : an12_x1 port map ( q=>nx772, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(26));
   reg_xl_10 : sff2_x4 port map ( q=>xl_10, i0=>xl_10, i1=>nx760, cmd=>
      nx2827, ck=>conpro_system_clk);
   ix761 : an12_x1 port map ( q=>nx760, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(10));
   ix1205 : na4_x1 port map ( nq=>nx1204, i0=>nx2758, i1=>nx2768, i2=>nx2778, 
      i3=>nx2788);
   ix2759 : noa2a2a2a24_x1 port map ( nq=>nx2758, i0=>xl_13, i1=>nx512, i2=>
      xl_61, i3=>nx498, i4=>xl_44, i5=>nx482, i6=>xl_60, i7=>nx466);
   reg_xl_13 : sff2_x4 port map ( q=>xl_13, i0=>xl_13, i1=>nx1188, cmd=>
      nx2829, ck=>conpro_system_clk);
   ix1189 : an12_x1 port map ( q=>nx1188, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(13));
   reg_xl_61 : sff2_x4 port map ( q=>xl_61, i0=>xl_61, i1=>nx1176, cmd=>
      nx2829, ck=>conpro_system_clk);
   ix1177 : an12_x1 port map ( q=>nx1176, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(61));
   reg_xl_44 : sff2_x4 port map ( q=>xl_44, i0=>xl_44, i1=>nx1162, cmd=>
      nx2829, ck=>conpro_system_clk);
   ix1163 : an12_x1 port map ( q=>nx1162, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(44));
   reg_xl_60 : sff2_x4 port map ( q=>xl_60, i0=>xl_60, i1=>nx1150, cmd=>
      nx2829, ck=>conpro_system_clk);
   ix1151 : an12_x1 port map ( q=>nx1150, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(60));
   ix2769 : noa2a2a2a24_x1 port map ( nq=>nx2768, i0=>xl_63, i1=>nx446, i2=>
      xl_12, i3=>nx432, i4=>xl_29, i5=>nx414, i6=>xl_31, i7=>nx400);
   reg_xl_63 : sff2_x4 port map ( q=>xl_63, i0=>xl_63, i1=>nx1134, cmd=>
      nx2831, ck=>conpro_system_clk);
   ix1135 : an12_x1 port map ( q=>nx1134, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(63));
   reg_xl_12 : sff2_x4 port map ( q=>xl_12, i0=>xl_12, i1=>nx1122, cmd=>
      nx2831, ck=>conpro_system_clk);
   ix1123 : an12_x1 port map ( q=>nx1122, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(12));
   reg_xl_29 : sff2_x4 port map ( q=>xl_29, i0=>xl_29, i1=>nx1108, cmd=>
      nx2831, ck=>conpro_system_clk);
   ix1109 : an12_x1 port map ( q=>nx1108, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(29));
   reg_xl_31 : sff2_x4 port map ( q=>xl_31, i0=>xl_31, i1=>nx1096, cmd=>
      nx2831, ck=>conpro_system_clk);
   ix1097 : an12_x1 port map ( q=>nx1096, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(31));
   ix2779 : noa2a2a2a24_x1 port map ( nq=>nx2778, i0=>xl_28, i1=>nx380, i2=>
      xl_15, i3=>nx364, i4=>xl_47, i5=>nx348, i6=>xl_45, i7=>nx334);
   reg_xl_28 : sff2_x4 port map ( q=>xl_28, i0=>xl_28, i1=>nx1078, cmd=>
      nx2833, ck=>conpro_system_clk);
   ix1079 : an12_x1 port map ( q=>nx1078, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(28));
   reg_xl_15 : sff2_x4 port map ( q=>xl_15, i0=>xl_15, i1=>nx1066, cmd=>
      nx2833, ck=>conpro_system_clk);
   ix1067 : an12_x1 port map ( q=>nx1066, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(15));
   reg_xl_47 : sff2_x4 port map ( q=>xl_47, i0=>xl_47, i1=>nx1052, cmd=>
      nx2833, ck=>conpro_system_clk);
   ix1053 : an12_x1 port map ( q=>nx1052, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(47));
   reg_xl_45 : sff2_x4 port map ( q=>xl_45, i0=>xl_45, i1=>nx1040, cmd=>
      nx2833, ck=>conpro_system_clk);
   ix1041 : an12_x1 port map ( q=>nx1040, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(45));
   ix2789 : noa2a2a2a24_x1 port map ( nq=>nx2788, i0=>xl_46, i1=>nx312, i2=>
      xl_62, i3=>nx294, i4=>xl_30, i5=>nx276, i6=>xl_14, i7=>nx258);
   reg_xl_46 : sff2_x4 port map ( q=>xl_46, i0=>xl_46, i1=>nx1024, cmd=>
      nx236, ck=>conpro_system_clk);
   ix1025 : an12_x1 port map ( q=>nx1024, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(46));
   reg_xl_62 : sff2_x4 port map ( q=>xl_62, i0=>xl_62, i1=>nx1012, cmd=>
      nx236, ck=>conpro_system_clk);
   ix1013 : an12_x1 port map ( q=>nx1012, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(62));
   reg_xl_30 : sff2_x4 port map ( q=>xl_30, i0=>xl_30, i1=>nx998, cmd=>nx236, 
      ck=>conpro_system_clk);
   ix999 : an12_x1 port map ( q=>nx998, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(30));
   reg_xl_14 : sff2_x4 port map ( q=>xl_14, i0=>xl_14, i1=>nx986, cmd=>nx236, 
      ck=>conpro_system_clk);
   ix987 : an12_x1 port map ( q=>nx986, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity1_x_RD(14));
   reg_pro_state_8 : sff1_x4 port map ( q=>PRO_FUN_parity1_END_EXMPLR, i=>
      nx1234, ck=>conpro_system_clk);
   ix1235 : ao22_x2 port map ( q=>nx1234, i0=>
      REG_RET_FUN_parity1_p_WE_EXMPLR, i1=>PRO_FUN_parity1_END_EXMPLR, i2=>
      nx2583);
   ix165 : inv_x1 port map ( nq=>nx1770, i=>nx2562);
   ix129 : inv_x1 port map ( nq=>nx1768, i=>nx2593);
   ix93 : inv_x1 port map ( nq=>nx1766, i=>nx2591);
   ix2584 : inv_x1 port map ( nq=>nx2583, i=>nx2);
   ix2516 : an12_x1 port map ( q=>nx2515, i0=>LOOP_i_0_6, i1=>nx2562);
   ix147 : an12_x1 port map ( q=>nx1769, i0=>nx2593, i1=>LOOP_i_0_4);
   ix2528 : an12_x1 port map ( q=>nx2527, i0=>LOOP_i_0_4, i1=>nx2593);
   ix111 : an12_x1 port map ( q=>nx1767, i0=>nx2591, i1=>LOOP_i_0_2);
   ix2542 : an12_x1 port map ( q=>nx2541, i0=>LOOP_i_0_2, i1=>nx2591);
   ix63 : on12_x4 port map ( q=>nx62, i0=>pro_state_6, i1=>
      conpro_system_reset);
   ix49 : an12_x1 port map ( q=>nx48, i0=>nx2, i1=>pro_state_5);
   ix2556 : an12_x1 port map ( q=>nx2555, i0=>LOOP_i_0_7, i1=>LOOP_i_0_6);
   ix183 : an12_x1 port map ( q=>nx1771, i0=>nx2562, i1=>LOOP_i_0_6);
   ix33 : an12_x1 port map ( q=>nx32, i0=>nx2, i1=>pro_state_2);
   ix23 : an12_x1 port map ( q=>nx22, i0=>nx2, i1=>pro_state_1);
   ix3 : on12_x4 port map ( q=>nx2, i0=>PRO_FUN_parity1_ENABLE, i1=>
      conpro_system_reset);
   ix237 : o2_x2 port map ( q=>nx236, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2621 : on12_x1 port map ( q=>nx2620, i0=>LOOP_i_0_5, i1=>LOOP_i_0_4);
   ix2637 : on12_x1 port map ( q=>nx2636, i0=>LOOP_i_0_4, i1=>LOOP_i_0_5);
   ix2663 : on12_x1 port map ( q=>nx2662, i0=>LOOP_i_0_1, i1=>LOOP_i_0_0);
   ix2804 : o2_x2 port map ( q=>nx2805, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2806 : o2_x2 port map ( q=>nx2807, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2808 : o2_x2 port map ( q=>nx2809, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2810 : o2_x2 port map ( q=>nx2811, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2812 : o2_x2 port map ( q=>nx2813, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2814 : o2_x2 port map ( q=>nx2815, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2816 : o2_x2 port map ( q=>nx2817, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2818 : o2_x2 port map ( q=>nx2819, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2820 : o2_x2 port map ( q=>nx2821, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2822 : o2_x2 port map ( q=>nx2823, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2824 : o2_x2 port map ( q=>nx2825, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2826 : o2_x2 port map ( q=>nx2827, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2828 : o2_x2 port map ( q=>nx2829, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2830 : o2_x2 port map ( q=>nx2831, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2832 : o2_x2 port map ( q=>nx2833, i0=>pro_state_1, i1=>
      conpro_system_reset);
end main ;

