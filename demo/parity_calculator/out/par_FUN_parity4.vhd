
-- 
-- Definition of  par_FUN_parity4
-- 
--      Fri Dec 19 11:31:38 2008
--      
--      LeonardoSpectrum Level 3, 2005b.24
-- 

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity par_FUN_parity4 is
   port (
      REG_ARG_FUN_parity4_x_RD : IN std_logic_vector (63 DOWNTO 0) ;
      REG_RET_FUN_parity4_p_WR : OUT std_logic ;
      REG_RET_FUN_parity4_p_WE : OUT std_logic ;
      PRO_FUN_parity4_ENABLE : IN std_logic ;
      PRO_FUN_parity4_END : OUT std_logic ;
      conpro_system_clk : IN std_logic ;
      conpro_system_reset : IN std_logic) ;
end par_FUN_parity4 ;

architecture main of par_FUN_parity4 is
   signal PRO_FUN_parity4_END_EXMPLR, REG_RET_FUN_parity4_p_WE_EXMPLR, 
      pro_state_1, pro_state_0, nx2, nx12, nx22, xl_0, nx30, nx34, xl_1, 
      nx44, xl_2, nx56, xl_3, nx66, xl_4, nx80, xl_5, nx90, xl_6, nx102, 
      xl_7, nx112, xl_8, nx128, xl_9, nx138, xl_10, nx150, xl_11, nx160, 
      xl_12, nx174, xl_13, nx184, xl_14, nx196, xl_15, nx206, xl_16, nx224, 
      xl_17, nx234, xl_18, nx246, xl_19, nx256, xl_20, nx270, xl_21, nx280, 
      xl_22, nx292, xl_23, nx302, xl_24, nx318, xl_25, nx328, xl_26, nx340, 
      xl_27, nx350, xl_28, nx364, xl_29, nx374, xl_30, nx386, xl_31, nx396, 
      xl_32, nx416, xl_33, nx426, xl_34, nx438, xl_35, nx448, xl_36, nx462, 
      xl_37, nx472, xl_38, nx484, xl_39, nx494, xl_40, nx510, xl_41, nx520, 
      xl_42, nx532, xl_43, nx542, xl_44, nx556, xl_45, nx566, xl_46, nx578, 
      xl_47, nx588, xl_48, nx606, xl_49, nx616, xl_50, nx628, xl_51, nx638, 
      xl_52, nx652, xl_53, nx662, xl_54, nx674, xl_55, nx684, xl_56, nx700, 
      xl_57, nx710, xl_58, nx722, xl_59, nx732, xl_60, nx746, xl_61, nx756, 
      xl_62, nx768, xl_63, nx778, pro_state_3, nx802, nx814, nx2280, nx2282, 
      nx2284, nx2286, nx2288, nx2290, nx2305, nx2311, nx2313, nx2319, nx2325, 
      nx2327, nx2329, nx2335, nx2341, nx2343, nx2349, nx2355, nx2357, nx2359, 
      nx2361, nx2367, nx2373, nx2375, nx2381, nx2387, nx2389, nx2391, nx2397, 
      nx2403, nx2405, nx2411, nx2417, nx2419, nx2421, nx2423, nx2425, nx2431, 
      nx2437, nx2439, nx2445, nx2451, nx2453, nx2455, nx2461, nx2467, nx2469, 
      nx2475, nx2481, nx2483, nx2485, nx2487, nx2493, nx2499, nx2501, nx2507, 
      nx2513, nx2515, nx2517, nx2523, nx2529, nx2531, nx2537, nx2549, nx2556, 
      nx2558, nx2560, nx2562, nx2564, nx2566, nx2568, nx2570, nx2572, nx2574, 
      nx2576, nx2578, nx2580, nx2582, nx2584: std_logic ;

begin
   REG_RET_FUN_parity4_p_WE <= REG_RET_FUN_parity4_p_WE_EXMPLR ;
   PRO_FUN_parity4_END <= PRO_FUN_parity4_END_EXMPLR ;
   ix799 : an12_x1 port map ( q=>REG_RET_FUN_parity4_p_WR, i0=>nx2280, i1=>
      REG_RET_FUN_parity4_p_WE_EXMPLR);
   ix2281 : nxr2_x1 port map ( nq=>nx2280, i0=>nx2282, i1=>nx2417);
   ix2283 : nxr2_x1 port map ( nq=>nx2282, i0=>nx2284, i1=>nx2355);
   ix2285 : nxr2_x1 port map ( nq=>nx2284, i0=>nx2286, i1=>nx2325);
   ix2287 : nxr2_x1 port map ( nq=>nx2286, i0=>nx2288, i1=>nx2311);
   ix2289 : nxr2_x1 port map ( nq=>nx2288, i0=>nx2290, i1=>nx2305);
   ix2291 : nxr2_x1 port map ( nq=>nx2290, i0=>xl_0, i1=>xl_1);
   reg_xl_0 : sff2_x4 port map ( q=>xl_0, i0=>xl_0, i1=>nx34, cmd=>nx2556, 
      ck=>conpro_system_clk);
   ix35 : an12_x1 port map ( q=>nx34, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(0));
   reg_pro_state_1 : sff1_x4 port map ( q=>pro_state_1, i=>nx12, ck=>
      conpro_system_clk);
   ix13 : an12_x1 port map ( q=>nx12, i0=>nx2, i1=>pro_state_0);
   reg_pro_state_0 : sff1_x4 port map ( q=>pro_state_0, i=>nx2, ck=>
      conpro_system_clk);
   reg_xl_1 : sff2_x4 port map ( q=>xl_1, i0=>xl_1, i1=>nx44, cmd=>nx2556, 
      ck=>conpro_system_clk);
   ix45 : an12_x1 port map ( q=>nx44, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(1));
   ix2306 : nxr2_x1 port map ( nq=>nx2305, i0=>xl_2, i1=>xl_3);
   reg_xl_2 : sff2_x4 port map ( q=>xl_2, i0=>xl_2, i1=>nx56, cmd=>nx2556, 
      ck=>conpro_system_clk);
   ix57 : an12_x1 port map ( q=>nx56, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(2));
   reg_xl_3 : sff2_x4 port map ( q=>xl_3, i0=>xl_3, i1=>nx66, cmd=>nx2556, 
      ck=>conpro_system_clk);
   ix67 : an12_x1 port map ( q=>nx66, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(3));
   ix2312 : nxr2_x1 port map ( nq=>nx2311, i0=>nx2313, i1=>nx2319);
   ix2314 : nxr2_x1 port map ( nq=>nx2313, i0=>xl_4, i1=>xl_5);
   reg_xl_4 : sff2_x4 port map ( q=>xl_4, i0=>xl_4, i1=>nx80, cmd=>nx2558, 
      ck=>conpro_system_clk);
   ix81 : an12_x1 port map ( q=>nx80, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(4));
   reg_xl_5 : sff2_x4 port map ( q=>xl_5, i0=>xl_5, i1=>nx90, cmd=>nx2558, 
      ck=>conpro_system_clk);
   ix91 : an12_x1 port map ( q=>nx90, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(5));
   ix2320 : nxr2_x1 port map ( nq=>nx2319, i0=>xl_6, i1=>xl_7);
   reg_xl_6 : sff2_x4 port map ( q=>xl_6, i0=>xl_6, i1=>nx102, cmd=>nx2558, 
      ck=>conpro_system_clk);
   ix103 : an12_x1 port map ( q=>nx102, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(6));
   reg_xl_7 : sff2_x4 port map ( q=>xl_7, i0=>xl_7, i1=>nx112, cmd=>nx2558, 
      ck=>conpro_system_clk);
   ix113 : an12_x1 port map ( q=>nx112, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(7));
   ix2326 : nxr2_x1 port map ( nq=>nx2325, i0=>nx2327, i1=>nx2341);
   ix2328 : nxr2_x1 port map ( nq=>nx2327, i0=>nx2329, i1=>nx2335);
   ix2330 : nxr2_x1 port map ( nq=>nx2329, i0=>xl_8, i1=>xl_9);
   reg_xl_8 : sff2_x4 port map ( q=>xl_8, i0=>xl_8, i1=>nx128, cmd=>nx2560, 
      ck=>conpro_system_clk);
   ix129 : an12_x1 port map ( q=>nx128, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(8));
   reg_xl_9 : sff2_x4 port map ( q=>xl_9, i0=>xl_9, i1=>nx138, cmd=>nx2560, 
      ck=>conpro_system_clk);
   ix139 : an12_x1 port map ( q=>nx138, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(9));
   ix2336 : nxr2_x1 port map ( nq=>nx2335, i0=>xl_10, i1=>xl_11);
   reg_xl_10 : sff2_x4 port map ( q=>xl_10, i0=>xl_10, i1=>nx150, cmd=>
      nx2560, ck=>conpro_system_clk);
   ix151 : an12_x1 port map ( q=>nx150, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(10));
   reg_xl_11 : sff2_x4 port map ( q=>xl_11, i0=>xl_11, i1=>nx160, cmd=>
      nx2560, ck=>conpro_system_clk);
   ix161 : an12_x1 port map ( q=>nx160, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(11));
   ix2342 : nxr2_x1 port map ( nq=>nx2341, i0=>nx2343, i1=>nx2349);
   ix2344 : nxr2_x1 port map ( nq=>nx2343, i0=>xl_12, i1=>xl_13);
   reg_xl_12 : sff2_x4 port map ( q=>xl_12, i0=>xl_12, i1=>nx174, cmd=>
      nx2562, ck=>conpro_system_clk);
   ix175 : an12_x1 port map ( q=>nx174, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(12));
   reg_xl_13 : sff2_x4 port map ( q=>xl_13, i0=>xl_13, i1=>nx184, cmd=>
      nx2562, ck=>conpro_system_clk);
   ix185 : an12_x1 port map ( q=>nx184, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(13));
   ix2350 : nxr2_x1 port map ( nq=>nx2349, i0=>xl_14, i1=>xl_15);
   reg_xl_14 : sff2_x4 port map ( q=>xl_14, i0=>xl_14, i1=>nx196, cmd=>
      nx2562, ck=>conpro_system_clk);
   ix197 : an12_x1 port map ( q=>nx196, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(14));
   reg_xl_15 : sff2_x4 port map ( q=>xl_15, i0=>xl_15, i1=>nx206, cmd=>
      nx2562, ck=>conpro_system_clk);
   ix207 : an12_x1 port map ( q=>nx206, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(15));
   ix2356 : nxr2_x1 port map ( nq=>nx2355, i0=>nx2357, i1=>nx2387);
   ix2358 : nxr2_x1 port map ( nq=>nx2357, i0=>nx2359, i1=>nx2373);
   ix2360 : nxr2_x1 port map ( nq=>nx2359, i0=>nx2361, i1=>nx2367);
   ix2362 : nxr2_x1 port map ( nq=>nx2361, i0=>xl_16, i1=>xl_17);
   reg_xl_16 : sff2_x4 port map ( q=>xl_16, i0=>xl_16, i1=>nx224, cmd=>
      nx2564, ck=>conpro_system_clk);
   ix225 : an12_x1 port map ( q=>nx224, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(16));
   reg_xl_17 : sff2_x4 port map ( q=>xl_17, i0=>xl_17, i1=>nx234, cmd=>
      nx2564, ck=>conpro_system_clk);
   ix235 : an12_x1 port map ( q=>nx234, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(17));
   ix2368 : nxr2_x1 port map ( nq=>nx2367, i0=>xl_18, i1=>xl_19);
   reg_xl_18 : sff2_x4 port map ( q=>xl_18, i0=>xl_18, i1=>nx246, cmd=>
      nx2564, ck=>conpro_system_clk);
   ix247 : an12_x1 port map ( q=>nx246, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(18));
   reg_xl_19 : sff2_x4 port map ( q=>xl_19, i0=>xl_19, i1=>nx256, cmd=>
      nx2564, ck=>conpro_system_clk);
   ix257 : an12_x1 port map ( q=>nx256, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(19));
   ix2374 : nxr2_x1 port map ( nq=>nx2373, i0=>nx2375, i1=>nx2381);
   ix2376 : nxr2_x1 port map ( nq=>nx2375, i0=>xl_20, i1=>xl_21);
   reg_xl_20 : sff2_x4 port map ( q=>xl_20, i0=>xl_20, i1=>nx270, cmd=>
      nx2566, ck=>conpro_system_clk);
   ix271 : an12_x1 port map ( q=>nx270, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(20));
   reg_xl_21 : sff2_x4 port map ( q=>xl_21, i0=>xl_21, i1=>nx280, cmd=>
      nx2566, ck=>conpro_system_clk);
   ix281 : an12_x1 port map ( q=>nx280, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(21));
   ix2382 : nxr2_x1 port map ( nq=>nx2381, i0=>xl_22, i1=>xl_23);
   reg_xl_22 : sff2_x4 port map ( q=>xl_22, i0=>xl_22, i1=>nx292, cmd=>
      nx2566, ck=>conpro_system_clk);
   ix293 : an12_x1 port map ( q=>nx292, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(22));
   reg_xl_23 : sff2_x4 port map ( q=>xl_23, i0=>xl_23, i1=>nx302, cmd=>
      nx2566, ck=>conpro_system_clk);
   ix303 : an12_x1 port map ( q=>nx302, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(23));
   ix2388 : nxr2_x1 port map ( nq=>nx2387, i0=>nx2389, i1=>nx2403);
   ix2390 : nxr2_x1 port map ( nq=>nx2389, i0=>nx2391, i1=>nx2397);
   ix2392 : nxr2_x1 port map ( nq=>nx2391, i0=>xl_24, i1=>xl_25);
   reg_xl_24 : sff2_x4 port map ( q=>xl_24, i0=>xl_24, i1=>nx318, cmd=>
      nx2568, ck=>conpro_system_clk);
   ix319 : an12_x1 port map ( q=>nx318, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(24));
   reg_xl_25 : sff2_x4 port map ( q=>xl_25, i0=>xl_25, i1=>nx328, cmd=>
      nx2568, ck=>conpro_system_clk);
   ix329 : an12_x1 port map ( q=>nx328, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(25));
   ix2398 : nxr2_x1 port map ( nq=>nx2397, i0=>xl_26, i1=>xl_27);
   reg_xl_26 : sff2_x4 port map ( q=>xl_26, i0=>xl_26, i1=>nx340, cmd=>
      nx2568, ck=>conpro_system_clk);
   ix341 : an12_x1 port map ( q=>nx340, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(26));
   reg_xl_27 : sff2_x4 port map ( q=>xl_27, i0=>xl_27, i1=>nx350, cmd=>
      nx2568, ck=>conpro_system_clk);
   ix351 : an12_x1 port map ( q=>nx350, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(27));
   ix2404 : nxr2_x1 port map ( nq=>nx2403, i0=>nx2405, i1=>nx2411);
   ix2406 : nxr2_x1 port map ( nq=>nx2405, i0=>xl_28, i1=>xl_29);
   reg_xl_28 : sff2_x4 port map ( q=>xl_28, i0=>xl_28, i1=>nx364, cmd=>
      nx2570, ck=>conpro_system_clk);
   ix365 : an12_x1 port map ( q=>nx364, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(28));
   reg_xl_29 : sff2_x4 port map ( q=>xl_29, i0=>xl_29, i1=>nx374, cmd=>
      nx2570, ck=>conpro_system_clk);
   ix375 : an12_x1 port map ( q=>nx374, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(29));
   ix2412 : nxr2_x1 port map ( nq=>nx2411, i0=>xl_30, i1=>xl_31);
   reg_xl_30 : sff2_x4 port map ( q=>xl_30, i0=>xl_30, i1=>nx386, cmd=>
      nx2570, ck=>conpro_system_clk);
   ix387 : an12_x1 port map ( q=>nx386, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(30));
   reg_xl_31 : sff2_x4 port map ( q=>xl_31, i0=>xl_31, i1=>nx396, cmd=>
      nx2570, ck=>conpro_system_clk);
   ix397 : an12_x1 port map ( q=>nx396, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(31));
   ix2418 : nxr2_x1 port map ( nq=>nx2417, i0=>nx2419, i1=>nx2481);
   ix2420 : nxr2_x1 port map ( nq=>nx2419, i0=>nx2421, i1=>nx2451);
   ix2422 : nxr2_x1 port map ( nq=>nx2421, i0=>nx2423, i1=>nx2437);
   ix2424 : nxr2_x1 port map ( nq=>nx2423, i0=>nx2425, i1=>nx2431);
   ix2426 : nxr2_x1 port map ( nq=>nx2425, i0=>xl_32, i1=>xl_33);
   reg_xl_32 : sff2_x4 port map ( q=>xl_32, i0=>xl_32, i1=>nx416, cmd=>
      nx2572, ck=>conpro_system_clk);
   ix417 : an12_x1 port map ( q=>nx416, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(32));
   reg_xl_33 : sff2_x4 port map ( q=>xl_33, i0=>xl_33, i1=>nx426, cmd=>
      nx2572, ck=>conpro_system_clk);
   ix427 : an12_x1 port map ( q=>nx426, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(33));
   ix2432 : nxr2_x1 port map ( nq=>nx2431, i0=>xl_34, i1=>xl_35);
   reg_xl_34 : sff2_x4 port map ( q=>xl_34, i0=>xl_34, i1=>nx438, cmd=>
      nx2572, ck=>conpro_system_clk);
   ix439 : an12_x1 port map ( q=>nx438, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(34));
   reg_xl_35 : sff2_x4 port map ( q=>xl_35, i0=>xl_35, i1=>nx448, cmd=>
      nx2572, ck=>conpro_system_clk);
   ix449 : an12_x1 port map ( q=>nx448, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(35));
   ix2438 : nxr2_x1 port map ( nq=>nx2437, i0=>nx2439, i1=>nx2445);
   ix2440 : nxr2_x1 port map ( nq=>nx2439, i0=>xl_36, i1=>xl_37);
   reg_xl_36 : sff2_x4 port map ( q=>xl_36, i0=>xl_36, i1=>nx462, cmd=>
      nx2574, ck=>conpro_system_clk);
   ix463 : an12_x1 port map ( q=>nx462, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(36));
   reg_xl_37 : sff2_x4 port map ( q=>xl_37, i0=>xl_37, i1=>nx472, cmd=>
      nx2574, ck=>conpro_system_clk);
   ix473 : an12_x1 port map ( q=>nx472, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(37));
   ix2446 : nxr2_x1 port map ( nq=>nx2445, i0=>xl_38, i1=>xl_39);
   reg_xl_38 : sff2_x4 port map ( q=>xl_38, i0=>xl_38, i1=>nx484, cmd=>
      nx2574, ck=>conpro_system_clk);
   ix485 : an12_x1 port map ( q=>nx484, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(38));
   reg_xl_39 : sff2_x4 port map ( q=>xl_39, i0=>xl_39, i1=>nx494, cmd=>
      nx2574, ck=>conpro_system_clk);
   ix495 : an12_x1 port map ( q=>nx494, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(39));
   ix2452 : nxr2_x1 port map ( nq=>nx2451, i0=>nx2453, i1=>nx2467);
   ix2454 : nxr2_x1 port map ( nq=>nx2453, i0=>nx2455, i1=>nx2461);
   ix2456 : nxr2_x1 port map ( nq=>nx2455, i0=>xl_40, i1=>xl_41);
   reg_xl_40 : sff2_x4 port map ( q=>xl_40, i0=>xl_40, i1=>nx510, cmd=>
      nx2576, ck=>conpro_system_clk);
   ix511 : an12_x1 port map ( q=>nx510, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(40));
   reg_xl_41 : sff2_x4 port map ( q=>xl_41, i0=>xl_41, i1=>nx520, cmd=>
      nx2576, ck=>conpro_system_clk);
   ix521 : an12_x1 port map ( q=>nx520, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(41));
   ix2462 : nxr2_x1 port map ( nq=>nx2461, i0=>xl_42, i1=>xl_43);
   reg_xl_42 : sff2_x4 port map ( q=>xl_42, i0=>xl_42, i1=>nx532, cmd=>
      nx2576, ck=>conpro_system_clk);
   ix533 : an12_x1 port map ( q=>nx532, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(42));
   reg_xl_43 : sff2_x4 port map ( q=>xl_43, i0=>xl_43, i1=>nx542, cmd=>
      nx2576, ck=>conpro_system_clk);
   ix543 : an12_x1 port map ( q=>nx542, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(43));
   ix2468 : nxr2_x1 port map ( nq=>nx2467, i0=>nx2469, i1=>nx2475);
   ix2470 : nxr2_x1 port map ( nq=>nx2469, i0=>xl_44, i1=>xl_45);
   reg_xl_44 : sff2_x4 port map ( q=>xl_44, i0=>xl_44, i1=>nx556, cmd=>
      nx2578, ck=>conpro_system_clk);
   ix557 : an12_x1 port map ( q=>nx556, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(44));
   reg_xl_45 : sff2_x4 port map ( q=>xl_45, i0=>xl_45, i1=>nx566, cmd=>
      nx2578, ck=>conpro_system_clk);
   ix567 : an12_x1 port map ( q=>nx566, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(45));
   ix2476 : nxr2_x1 port map ( nq=>nx2475, i0=>xl_46, i1=>xl_47);
   reg_xl_46 : sff2_x4 port map ( q=>xl_46, i0=>xl_46, i1=>nx578, cmd=>
      nx2578, ck=>conpro_system_clk);
   ix579 : an12_x1 port map ( q=>nx578, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(46));
   reg_xl_47 : sff2_x4 port map ( q=>xl_47, i0=>xl_47, i1=>nx588, cmd=>
      nx2578, ck=>conpro_system_clk);
   ix589 : an12_x1 port map ( q=>nx588, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(47));
   ix2482 : nxr2_x1 port map ( nq=>nx2481, i0=>nx2483, i1=>nx2513);
   ix2484 : nxr2_x1 port map ( nq=>nx2483, i0=>nx2485, i1=>nx2499);
   ix2486 : nxr2_x1 port map ( nq=>nx2485, i0=>nx2487, i1=>nx2493);
   ix2488 : nxr2_x1 port map ( nq=>nx2487, i0=>xl_48, i1=>xl_49);
   reg_xl_48 : sff2_x4 port map ( q=>xl_48, i0=>xl_48, i1=>nx606, cmd=>
      nx2580, ck=>conpro_system_clk);
   ix607 : an12_x1 port map ( q=>nx606, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(48));
   reg_xl_49 : sff2_x4 port map ( q=>xl_49, i0=>xl_49, i1=>nx616, cmd=>
      nx2580, ck=>conpro_system_clk);
   ix617 : an12_x1 port map ( q=>nx616, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(49));
   ix2494 : nxr2_x1 port map ( nq=>nx2493, i0=>xl_50, i1=>xl_51);
   reg_xl_50 : sff2_x4 port map ( q=>xl_50, i0=>xl_50, i1=>nx628, cmd=>
      nx2580, ck=>conpro_system_clk);
   ix629 : an12_x1 port map ( q=>nx628, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(50));
   reg_xl_51 : sff2_x4 port map ( q=>xl_51, i0=>xl_51, i1=>nx638, cmd=>
      nx2580, ck=>conpro_system_clk);
   ix639 : an12_x1 port map ( q=>nx638, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(51));
   ix2500 : nxr2_x1 port map ( nq=>nx2499, i0=>nx2501, i1=>nx2507);
   ix2502 : nxr2_x1 port map ( nq=>nx2501, i0=>xl_52, i1=>xl_53);
   reg_xl_52 : sff2_x4 port map ( q=>xl_52, i0=>xl_52, i1=>nx652, cmd=>
      nx2582, ck=>conpro_system_clk);
   ix653 : an12_x1 port map ( q=>nx652, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(52));
   reg_xl_53 : sff2_x4 port map ( q=>xl_53, i0=>xl_53, i1=>nx662, cmd=>
      nx2582, ck=>conpro_system_clk);
   ix663 : an12_x1 port map ( q=>nx662, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(53));
   ix2508 : nxr2_x1 port map ( nq=>nx2507, i0=>xl_54, i1=>xl_55);
   reg_xl_54 : sff2_x4 port map ( q=>xl_54, i0=>xl_54, i1=>nx674, cmd=>
      nx2582, ck=>conpro_system_clk);
   ix675 : an12_x1 port map ( q=>nx674, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(54));
   reg_xl_55 : sff2_x4 port map ( q=>xl_55, i0=>xl_55, i1=>nx684, cmd=>
      nx2582, ck=>conpro_system_clk);
   ix685 : an12_x1 port map ( q=>nx684, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(55));
   ix2514 : nxr2_x1 port map ( nq=>nx2513, i0=>nx2515, i1=>nx2529);
   ix2516 : nxr2_x1 port map ( nq=>nx2515, i0=>nx2517, i1=>nx2523);
   ix2518 : nxr2_x1 port map ( nq=>nx2517, i0=>xl_56, i1=>xl_57);
   reg_xl_56 : sff2_x4 port map ( q=>xl_56, i0=>xl_56, i1=>nx700, cmd=>
      nx2584, ck=>conpro_system_clk);
   ix701 : an12_x1 port map ( q=>nx700, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(56));
   reg_xl_57 : sff2_x4 port map ( q=>xl_57, i0=>xl_57, i1=>nx710, cmd=>
      nx2584, ck=>conpro_system_clk);
   ix711 : an12_x1 port map ( q=>nx710, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(57));
   ix2524 : nxr2_x1 port map ( nq=>nx2523, i0=>xl_58, i1=>xl_59);
   reg_xl_58 : sff2_x4 port map ( q=>xl_58, i0=>xl_58, i1=>nx722, cmd=>
      nx2584, ck=>conpro_system_clk);
   ix723 : an12_x1 port map ( q=>nx722, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(58));
   reg_xl_59 : sff2_x4 port map ( q=>xl_59, i0=>xl_59, i1=>nx732, cmd=>
      nx2584, ck=>conpro_system_clk);
   ix733 : an12_x1 port map ( q=>nx732, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(59));
   ix2530 : nxr2_x1 port map ( nq=>nx2529, i0=>nx2531, i1=>nx2537);
   ix2532 : nxr2_x1 port map ( nq=>nx2531, i0=>xl_60, i1=>xl_61);
   reg_xl_60 : sff2_x4 port map ( q=>xl_60, i0=>xl_60, i1=>nx746, cmd=>nx30, 
      ck=>conpro_system_clk);
   ix747 : an12_x1 port map ( q=>nx746, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(60));
   reg_xl_61 : sff2_x4 port map ( q=>xl_61, i0=>xl_61, i1=>nx756, cmd=>nx30, 
      ck=>conpro_system_clk);
   ix757 : an12_x1 port map ( q=>nx756, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(61));
   ix2538 : nxr2_x1 port map ( nq=>nx2537, i0=>xl_62, i1=>xl_63);
   reg_xl_62 : sff2_x4 port map ( q=>xl_62, i0=>xl_62, i1=>nx768, cmd=>nx30, 
      ck=>conpro_system_clk);
   ix769 : an12_x1 port map ( q=>nx768, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(62));
   reg_xl_63 : sff2_x4 port map ( q=>xl_63, i0=>xl_63, i1=>nx778, cmd=>nx30, 
      ck=>conpro_system_clk);
   ix779 : an12_x1 port map ( q=>nx778, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity4_x_RD(63));
   reg_pro_state_2 : sff1_x4 port map ( q=>REG_RET_FUN_parity4_p_WE_EXMPLR, 
      i=>nx22, ck=>conpro_system_clk);
   reg_pro_state_4 : sff1_x4 port map ( q=>PRO_FUN_parity4_END_EXMPLR, i=>
      nx814, ck=>conpro_system_clk);
   ix815 : ao22_x2 port map ( q=>nx814, i0=>pro_state_3, i1=>
      PRO_FUN_parity4_END_EXMPLR, i2=>nx2549);
   reg_pro_state_3 : sff1_x4 port map ( q=>pro_state_3, i=>nx802, ck=>
      conpro_system_clk);
   ix803 : an12_x1 port map ( q=>nx802, i0=>nx2, i1=>
      REG_RET_FUN_parity4_p_WE_EXMPLR);
   ix2550 : inv_x1 port map ( nq=>nx2549, i=>nx2);
   ix31 : o2_x2 port map ( q=>nx30, i0=>pro_state_1, i1=>conpro_system_reset
   );
   ix3 : on12_x1 port map ( q=>nx2, i0=>PRO_FUN_parity4_ENABLE, i1=>
      conpro_system_reset);
   ix23 : an12_x1 port map ( q=>nx22, i0=>nx2, i1=>pro_state_1);
   ix2555 : o2_x2 port map ( q=>nx2556, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2557 : o2_x2 port map ( q=>nx2558, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2559 : o2_x2 port map ( q=>nx2560, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2561 : o2_x2 port map ( q=>nx2562, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2563 : o2_x2 port map ( q=>nx2564, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2565 : o2_x2 port map ( q=>nx2566, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2567 : o2_x2 port map ( q=>nx2568, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2569 : o2_x2 port map ( q=>nx2570, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2571 : o2_x2 port map ( q=>nx2572, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2573 : o2_x2 port map ( q=>nx2574, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2575 : o2_x2 port map ( q=>nx2576, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2577 : o2_x2 port map ( q=>nx2578, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2579 : o2_x2 port map ( q=>nx2580, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2581 : o2_x2 port map ( q=>nx2582, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix2583 : o2_x2 port map ( q=>nx2584, i0=>pro_state_1, i1=>
      conpro_system_reset);
end main ;

