
-- 
-- Definition of  par_FUN_parity6
-- 
--      Fri Dec 19 11:31:38 2008
--      
--      LeonardoSpectrum Level 3, 2005b.24
-- 

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity par_FUN_parity6 is
   port (
      REG_ARG_FUN_parity6_x_RD : IN std_logic_vector (63 DOWNTO 0) ;
      REG_RET_FUN_parity6_p_WR : OUT std_logic ;
      REG_RET_FUN_parity6_p_WE : OUT std_logic ;
      PRO_FUN_parity6_ENABLE : IN std_logic ;
      PRO_FUN_parity6_END : OUT std_logic ;
      conpro_system_clk : IN std_logic ;
      conpro_system_reset : IN std_logic) ;
end par_FUN_parity6 ;

architecture main of par_FUN_parity6 is
   signal REG_RET_FUN_parity6_p_WE_EXMPLR, pro_state_1, pro_state_0, nx4, 
      nx8, nx22, xl_0, nx34, xl_1, nx44, xl_2, nx56, xl_3, nx66, xl_4, nx80, 
      xl_5, nx90, xl_6, nx102, xl_7, nx112, xl_8, nx128, xl_9, nx138, xl_10, 
      nx150, xl_11, nx160, xl_12, nx174, xl_13, nx184, xl_14, nx196, xl_15, 
      nx206, xl_16, nx224, xl_17, nx234, xl_18, nx246, xl_19, nx256, xl_20, 
      nx270, xl_21, nx280, xl_22, nx292, xl_23, nx302, xl_24, nx318, xl_25, 
      nx328, xl_26, nx340, xl_27, nx350, xl_28, nx364, xl_29, nx374, xl_30, 
      nx386, xl_31, nx396, xl_32, nx416, xl_33, nx426, xl_34, nx438, xl_35, 
      nx448, xl_36, nx462, xl_37, nx472, xl_38, nx484, xl_39, nx494, xl_40, 
      nx510, xl_41, nx520, xl_42, nx532, xl_43, nx542, xl_44, nx556, xl_45, 
      nx566, xl_46, nx578, xl_47, nx588, xl_48, nx606, xl_49, nx616, xl_50, 
      nx628, xl_51, nx638, xl_52, nx652, xl_53, nx662, xl_54, nx674, xl_55, 
      nx684, xl_56, nx700, xl_57, nx710, xl_58, nx722, xl_59, nx732, xl_60, 
      nx746, xl_61, nx756, xl_62, nx768, xl_63, nx778, nx2251, nx2253, 
      nx2255, nx2257, nx2259, nx2261, nx2266, nx2270, nx2274, nx2276, nx2280, 
      nx2286, nx2288, nx2294, nx2300, nx2302, nx2304, nx2310, nx2316, nx2318, 
      nx2324, nx2330, nx2332, nx2334, nx2336, nx2342, nx2348, nx2350, nx2356, 
      nx2362, nx2364, nx2366, nx2372, nx2378, nx2380, nx2386, nx2392, nx2394, 
      nx2396, nx2398, nx2400, nx2406, nx2412, nx2414, nx2420, nx2426, nx2428, 
      nx2430, nx2436, nx2442, nx2444, nx2450, nx2456, nx2458, nx2460, nx2462, 
      nx2468, nx2474, nx2476, nx2482, nx2488, nx2490, nx2492, nx2498, nx2504, 
      nx2506, nx2512, nx2527, nx2529, nx2531, nx2533, nx2535, nx2537, nx2539, 
      nx2541, nx2543, nx2545, nx2547, nx2549, nx2551, nx2553, nx2555: 
   std_logic ;

begin
   REG_RET_FUN_parity6_p_WE <= REG_RET_FUN_parity6_p_WE_EXMPLR ;
   ix803 : an12_x1 port map ( q=>REG_RET_FUN_parity6_p_WR, i0=>nx2251, i1=>
      REG_RET_FUN_parity6_p_WE_EXMPLR);
   ix2252 : nxr2_x1 port map ( nq=>nx2251, i0=>nx2253, i1=>nx2392);
   ix2254 : nxr2_x1 port map ( nq=>nx2253, i0=>nx2255, i1=>nx2330);
   ix2256 : nxr2_x1 port map ( nq=>nx2255, i0=>nx2257, i1=>nx2300);
   ix2258 : nxr2_x1 port map ( nq=>nx2257, i0=>nx2259, i1=>nx2286);
   ix2260 : nxr2_x1 port map ( nq=>nx2259, i0=>nx2261, i1=>nx2280);
   ix2262 : nxr2_x1 port map ( nq=>nx2261, i0=>xl_0, i1=>xl_1);
   reg_xl_0 : sff2_x4 port map ( q=>xl_0, i0=>xl_0, i1=>nx34, cmd=>nx2527, 
      ck=>conpro_system_clk);
   ix35 : an12_x1 port map ( q=>nx34, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(0));
   ix2267 : inv_x1 port map ( nq=>nx2266, i=>pro_state_1);
   reg_pro_state_1 : sff1_x4 port map ( q=>pro_state_1, i=>nx22, ck=>
      conpro_system_clk);
   ix23 : no3_x1 port map ( nq=>nx22, i0=>nx2270, i1=>conpro_system_reset, 
      i2=>nx2276);
   ix2271 : no2_x1 port map ( nq=>nx2270, i0=>pro_state_0, i1=>pro_state_1);
   reg_pro_state_0 : sff1_x4 port map ( q=>pro_state_0, i=>nx8, ck=>
      conpro_system_clk);
   ix9 : a2_x2 port map ( q=>nx8, i0=>PRO_FUN_parity6_ENABLE, i1=>nx2274);
   ix2275 : noa22_x4 port map ( nq=>nx2274, i0=>nx2266, i1=>pro_state_0, i2
      =>conpro_system_reset);
   ix2277 : inv_x1 port map ( nq=>nx2276, i=>PRO_FUN_parity6_ENABLE);
   reg_xl_1 : sff2_x4 port map ( q=>xl_1, i0=>xl_1, i1=>nx44, cmd=>nx2527, 
      ck=>conpro_system_clk);
   ix45 : an12_x1 port map ( q=>nx44, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(1));
   ix2281 : nxr2_x1 port map ( nq=>nx2280, i0=>xl_2, i1=>xl_3);
   reg_xl_2 : sff2_x4 port map ( q=>xl_2, i0=>xl_2, i1=>nx56, cmd=>nx2527, 
      ck=>conpro_system_clk);
   ix57 : an12_x1 port map ( q=>nx56, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(2));
   reg_xl_3 : sff2_x4 port map ( q=>xl_3, i0=>xl_3, i1=>nx66, cmd=>nx2527, 
      ck=>conpro_system_clk);
   ix67 : an12_x1 port map ( q=>nx66, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(3));
   ix2287 : nxr2_x1 port map ( nq=>nx2286, i0=>nx2288, i1=>nx2294);
   ix2289 : nxr2_x1 port map ( nq=>nx2288, i0=>xl_4, i1=>xl_5);
   reg_xl_4 : sff2_x4 port map ( q=>xl_4, i0=>xl_4, i1=>nx80, cmd=>nx2529, 
      ck=>conpro_system_clk);
   ix81 : an12_x1 port map ( q=>nx80, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(4));
   reg_xl_5 : sff2_x4 port map ( q=>xl_5, i0=>xl_5, i1=>nx90, cmd=>nx2529, 
      ck=>conpro_system_clk);
   ix91 : an12_x1 port map ( q=>nx90, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(5));
   ix2295 : nxr2_x1 port map ( nq=>nx2294, i0=>xl_6, i1=>xl_7);
   reg_xl_6 : sff2_x4 port map ( q=>xl_6, i0=>xl_6, i1=>nx102, cmd=>nx2529, 
      ck=>conpro_system_clk);
   ix103 : an12_x1 port map ( q=>nx102, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(6));
   reg_xl_7 : sff2_x4 port map ( q=>xl_7, i0=>xl_7, i1=>nx112, cmd=>nx2529, 
      ck=>conpro_system_clk);
   ix113 : an12_x1 port map ( q=>nx112, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(7));
   ix2301 : nxr2_x1 port map ( nq=>nx2300, i0=>nx2302, i1=>nx2316);
   ix2303 : nxr2_x1 port map ( nq=>nx2302, i0=>nx2304, i1=>nx2310);
   ix2305 : nxr2_x1 port map ( nq=>nx2304, i0=>xl_8, i1=>xl_9);
   reg_xl_8 : sff2_x4 port map ( q=>xl_8, i0=>xl_8, i1=>nx128, cmd=>nx2531, 
      ck=>conpro_system_clk);
   ix129 : an12_x1 port map ( q=>nx128, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(8));
   reg_xl_9 : sff2_x4 port map ( q=>xl_9, i0=>xl_9, i1=>nx138, cmd=>nx2531, 
      ck=>conpro_system_clk);
   ix139 : an12_x1 port map ( q=>nx138, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(9));
   ix2311 : nxr2_x1 port map ( nq=>nx2310, i0=>xl_10, i1=>xl_11);
   reg_xl_10 : sff2_x4 port map ( q=>xl_10, i0=>xl_10, i1=>nx150, cmd=>
      nx2531, ck=>conpro_system_clk);
   ix151 : an12_x1 port map ( q=>nx150, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(10));
   reg_xl_11 : sff2_x4 port map ( q=>xl_11, i0=>xl_11, i1=>nx160, cmd=>
      nx2531, ck=>conpro_system_clk);
   ix161 : an12_x1 port map ( q=>nx160, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(11));
   ix2317 : nxr2_x1 port map ( nq=>nx2316, i0=>nx2318, i1=>nx2324);
   ix2319 : nxr2_x1 port map ( nq=>nx2318, i0=>xl_12, i1=>xl_13);
   reg_xl_12 : sff2_x4 port map ( q=>xl_12, i0=>xl_12, i1=>nx174, cmd=>
      nx2533, ck=>conpro_system_clk);
   ix175 : an12_x1 port map ( q=>nx174, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(12));
   reg_xl_13 : sff2_x4 port map ( q=>xl_13, i0=>xl_13, i1=>nx184, cmd=>
      nx2533, ck=>conpro_system_clk);
   ix185 : an12_x1 port map ( q=>nx184, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(13));
   ix2325 : nxr2_x1 port map ( nq=>nx2324, i0=>xl_14, i1=>xl_15);
   reg_xl_14 : sff2_x4 port map ( q=>xl_14, i0=>xl_14, i1=>nx196, cmd=>
      nx2533, ck=>conpro_system_clk);
   ix197 : an12_x1 port map ( q=>nx196, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(14));
   reg_xl_15 : sff2_x4 port map ( q=>xl_15, i0=>xl_15, i1=>nx206, cmd=>
      nx2533, ck=>conpro_system_clk);
   ix207 : an12_x1 port map ( q=>nx206, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(15));
   ix2331 : nxr2_x1 port map ( nq=>nx2330, i0=>nx2332, i1=>nx2362);
   ix2333 : nxr2_x1 port map ( nq=>nx2332, i0=>nx2334, i1=>nx2348);
   ix2335 : nxr2_x1 port map ( nq=>nx2334, i0=>nx2336, i1=>nx2342);
   ix2337 : nxr2_x1 port map ( nq=>nx2336, i0=>xl_16, i1=>xl_17);
   reg_xl_16 : sff2_x4 port map ( q=>xl_16, i0=>xl_16, i1=>nx224, cmd=>
      nx2535, ck=>conpro_system_clk);
   ix225 : an12_x1 port map ( q=>nx224, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(16));
   reg_xl_17 : sff2_x4 port map ( q=>xl_17, i0=>xl_17, i1=>nx234, cmd=>
      nx2535, ck=>conpro_system_clk);
   ix235 : an12_x1 port map ( q=>nx234, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(17));
   ix2343 : nxr2_x1 port map ( nq=>nx2342, i0=>xl_18, i1=>xl_19);
   reg_xl_18 : sff2_x4 port map ( q=>xl_18, i0=>xl_18, i1=>nx246, cmd=>
      nx2535, ck=>conpro_system_clk);
   ix247 : an12_x1 port map ( q=>nx246, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(18));
   reg_xl_19 : sff2_x4 port map ( q=>xl_19, i0=>xl_19, i1=>nx256, cmd=>
      nx2535, ck=>conpro_system_clk);
   ix257 : an12_x1 port map ( q=>nx256, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(19));
   ix2349 : nxr2_x1 port map ( nq=>nx2348, i0=>nx2350, i1=>nx2356);
   ix2351 : nxr2_x1 port map ( nq=>nx2350, i0=>xl_20, i1=>xl_21);
   reg_xl_20 : sff2_x4 port map ( q=>xl_20, i0=>xl_20, i1=>nx270, cmd=>
      nx2537, ck=>conpro_system_clk);
   ix271 : an12_x1 port map ( q=>nx270, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(20));
   reg_xl_21 : sff2_x4 port map ( q=>xl_21, i0=>xl_21, i1=>nx280, cmd=>
      nx2537, ck=>conpro_system_clk);
   ix281 : an12_x1 port map ( q=>nx280, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(21));
   ix2357 : nxr2_x1 port map ( nq=>nx2356, i0=>xl_22, i1=>xl_23);
   reg_xl_22 : sff2_x4 port map ( q=>xl_22, i0=>xl_22, i1=>nx292, cmd=>
      nx2537, ck=>conpro_system_clk);
   ix293 : an12_x1 port map ( q=>nx292, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(22));
   reg_xl_23 : sff2_x4 port map ( q=>xl_23, i0=>xl_23, i1=>nx302, cmd=>
      nx2537, ck=>conpro_system_clk);
   ix303 : an12_x1 port map ( q=>nx302, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(23));
   ix2363 : nxr2_x1 port map ( nq=>nx2362, i0=>nx2364, i1=>nx2378);
   ix2365 : nxr2_x1 port map ( nq=>nx2364, i0=>nx2366, i1=>nx2372);
   ix2367 : nxr2_x1 port map ( nq=>nx2366, i0=>xl_24, i1=>xl_25);
   reg_xl_24 : sff2_x4 port map ( q=>xl_24, i0=>xl_24, i1=>nx318, cmd=>
      nx2539, ck=>conpro_system_clk);
   ix319 : an12_x1 port map ( q=>nx318, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(24));
   reg_xl_25 : sff2_x4 port map ( q=>xl_25, i0=>xl_25, i1=>nx328, cmd=>
      nx2539, ck=>conpro_system_clk);
   ix329 : an12_x1 port map ( q=>nx328, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(25));
   ix2373 : nxr2_x1 port map ( nq=>nx2372, i0=>xl_26, i1=>xl_27);
   reg_xl_26 : sff2_x4 port map ( q=>xl_26, i0=>xl_26, i1=>nx340, cmd=>
      nx2539, ck=>conpro_system_clk);
   ix341 : an12_x1 port map ( q=>nx340, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(26));
   reg_xl_27 : sff2_x4 port map ( q=>xl_27, i0=>xl_27, i1=>nx350, cmd=>
      nx2539, ck=>conpro_system_clk);
   ix351 : an12_x1 port map ( q=>nx350, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(27));
   ix2379 : nxr2_x1 port map ( nq=>nx2378, i0=>nx2380, i1=>nx2386);
   ix2381 : nxr2_x1 port map ( nq=>nx2380, i0=>xl_28, i1=>xl_29);
   reg_xl_28 : sff2_x4 port map ( q=>xl_28, i0=>xl_28, i1=>nx364, cmd=>
      nx2541, ck=>conpro_system_clk);
   ix365 : an12_x1 port map ( q=>nx364, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(28));
   reg_xl_29 : sff2_x4 port map ( q=>xl_29, i0=>xl_29, i1=>nx374, cmd=>
      nx2541, ck=>conpro_system_clk);
   ix375 : an12_x1 port map ( q=>nx374, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(29));
   ix2387 : nxr2_x1 port map ( nq=>nx2386, i0=>xl_30, i1=>xl_31);
   reg_xl_30 : sff2_x4 port map ( q=>xl_30, i0=>xl_30, i1=>nx386, cmd=>
      nx2541, ck=>conpro_system_clk);
   ix387 : an12_x1 port map ( q=>nx386, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(30));
   reg_xl_31 : sff2_x4 port map ( q=>xl_31, i0=>xl_31, i1=>nx396, cmd=>
      nx2541, ck=>conpro_system_clk);
   ix397 : an12_x1 port map ( q=>nx396, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(31));
   ix2393 : nxr2_x1 port map ( nq=>nx2392, i0=>nx2394, i1=>nx2456);
   ix2395 : nxr2_x1 port map ( nq=>nx2394, i0=>nx2396, i1=>nx2426);
   ix2397 : nxr2_x1 port map ( nq=>nx2396, i0=>nx2398, i1=>nx2412);
   ix2399 : nxr2_x1 port map ( nq=>nx2398, i0=>nx2400, i1=>nx2406);
   ix2401 : nxr2_x1 port map ( nq=>nx2400, i0=>xl_32, i1=>xl_33);
   reg_xl_32 : sff2_x4 port map ( q=>xl_32, i0=>xl_32, i1=>nx416, cmd=>
      nx2543, ck=>conpro_system_clk);
   ix417 : an12_x1 port map ( q=>nx416, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(32));
   reg_xl_33 : sff2_x4 port map ( q=>xl_33, i0=>xl_33, i1=>nx426, cmd=>
      nx2543, ck=>conpro_system_clk);
   ix427 : an12_x1 port map ( q=>nx426, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(33));
   ix2407 : nxr2_x1 port map ( nq=>nx2406, i0=>xl_34, i1=>xl_35);
   reg_xl_34 : sff2_x4 port map ( q=>xl_34, i0=>xl_34, i1=>nx438, cmd=>
      nx2543, ck=>conpro_system_clk);
   ix439 : an12_x1 port map ( q=>nx438, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(34));
   reg_xl_35 : sff2_x4 port map ( q=>xl_35, i0=>xl_35, i1=>nx448, cmd=>
      nx2543, ck=>conpro_system_clk);
   ix449 : an12_x1 port map ( q=>nx448, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(35));
   ix2413 : nxr2_x1 port map ( nq=>nx2412, i0=>nx2414, i1=>nx2420);
   ix2415 : nxr2_x1 port map ( nq=>nx2414, i0=>xl_36, i1=>xl_37);
   reg_xl_36 : sff2_x4 port map ( q=>xl_36, i0=>xl_36, i1=>nx462, cmd=>
      nx2545, ck=>conpro_system_clk);
   ix463 : an12_x1 port map ( q=>nx462, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(36));
   reg_xl_37 : sff2_x4 port map ( q=>xl_37, i0=>xl_37, i1=>nx472, cmd=>
      nx2545, ck=>conpro_system_clk);
   ix473 : an12_x1 port map ( q=>nx472, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(37));
   ix2421 : nxr2_x1 port map ( nq=>nx2420, i0=>xl_38, i1=>xl_39);
   reg_xl_38 : sff2_x4 port map ( q=>xl_38, i0=>xl_38, i1=>nx484, cmd=>
      nx2545, ck=>conpro_system_clk);
   ix485 : an12_x1 port map ( q=>nx484, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(38));
   reg_xl_39 : sff2_x4 port map ( q=>xl_39, i0=>xl_39, i1=>nx494, cmd=>
      nx2545, ck=>conpro_system_clk);
   ix495 : an12_x1 port map ( q=>nx494, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(39));
   ix2427 : nxr2_x1 port map ( nq=>nx2426, i0=>nx2428, i1=>nx2442);
   ix2429 : nxr2_x1 port map ( nq=>nx2428, i0=>nx2430, i1=>nx2436);
   ix2431 : nxr2_x1 port map ( nq=>nx2430, i0=>xl_40, i1=>xl_41);
   reg_xl_40 : sff2_x4 port map ( q=>xl_40, i0=>xl_40, i1=>nx510, cmd=>
      nx2547, ck=>conpro_system_clk);
   ix511 : an12_x1 port map ( q=>nx510, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(40));
   reg_xl_41 : sff2_x4 port map ( q=>xl_41, i0=>xl_41, i1=>nx520, cmd=>
      nx2547, ck=>conpro_system_clk);
   ix521 : an12_x1 port map ( q=>nx520, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(41));
   ix2437 : nxr2_x1 port map ( nq=>nx2436, i0=>xl_42, i1=>xl_43);
   reg_xl_42 : sff2_x4 port map ( q=>xl_42, i0=>xl_42, i1=>nx532, cmd=>
      nx2547, ck=>conpro_system_clk);
   ix533 : an12_x1 port map ( q=>nx532, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(42));
   reg_xl_43 : sff2_x4 port map ( q=>xl_43, i0=>xl_43, i1=>nx542, cmd=>
      nx2547, ck=>conpro_system_clk);
   ix543 : an12_x1 port map ( q=>nx542, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(43));
   ix2443 : nxr2_x1 port map ( nq=>nx2442, i0=>nx2444, i1=>nx2450);
   ix2445 : nxr2_x1 port map ( nq=>nx2444, i0=>xl_44, i1=>xl_45);
   reg_xl_44 : sff2_x4 port map ( q=>xl_44, i0=>xl_44, i1=>nx556, cmd=>
      nx2549, ck=>conpro_system_clk);
   ix557 : an12_x1 port map ( q=>nx556, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(44));
   reg_xl_45 : sff2_x4 port map ( q=>xl_45, i0=>xl_45, i1=>nx566, cmd=>
      nx2549, ck=>conpro_system_clk);
   ix567 : an12_x1 port map ( q=>nx566, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(45));
   ix2451 : nxr2_x1 port map ( nq=>nx2450, i0=>xl_46, i1=>xl_47);
   reg_xl_46 : sff2_x4 port map ( q=>xl_46, i0=>xl_46, i1=>nx578, cmd=>
      nx2549, ck=>conpro_system_clk);
   ix579 : an12_x1 port map ( q=>nx578, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(46));
   reg_xl_47 : sff2_x4 port map ( q=>xl_47, i0=>xl_47, i1=>nx588, cmd=>
      nx2549, ck=>conpro_system_clk);
   ix589 : an12_x1 port map ( q=>nx588, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(47));
   ix2457 : nxr2_x1 port map ( nq=>nx2456, i0=>nx2458, i1=>nx2488);
   ix2459 : nxr2_x1 port map ( nq=>nx2458, i0=>nx2460, i1=>nx2474);
   ix2461 : nxr2_x1 port map ( nq=>nx2460, i0=>nx2462, i1=>nx2468);
   ix2463 : nxr2_x1 port map ( nq=>nx2462, i0=>xl_48, i1=>xl_49);
   reg_xl_48 : sff2_x4 port map ( q=>xl_48, i0=>xl_48, i1=>nx606, cmd=>
      nx2551, ck=>conpro_system_clk);
   ix607 : an12_x1 port map ( q=>nx606, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(48));
   reg_xl_49 : sff2_x4 port map ( q=>xl_49, i0=>xl_49, i1=>nx616, cmd=>
      nx2551, ck=>conpro_system_clk);
   ix617 : an12_x1 port map ( q=>nx616, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(49));
   ix2469 : nxr2_x1 port map ( nq=>nx2468, i0=>xl_50, i1=>xl_51);
   reg_xl_50 : sff2_x4 port map ( q=>xl_50, i0=>xl_50, i1=>nx628, cmd=>
      nx2551, ck=>conpro_system_clk);
   ix629 : an12_x1 port map ( q=>nx628, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(50));
   reg_xl_51 : sff2_x4 port map ( q=>xl_51, i0=>xl_51, i1=>nx638, cmd=>
      nx2551, ck=>conpro_system_clk);
   ix639 : an12_x1 port map ( q=>nx638, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(51));
   ix2475 : nxr2_x1 port map ( nq=>nx2474, i0=>nx2476, i1=>nx2482);
   ix2477 : nxr2_x1 port map ( nq=>nx2476, i0=>xl_52, i1=>xl_53);
   reg_xl_52 : sff2_x4 port map ( q=>xl_52, i0=>xl_52, i1=>nx652, cmd=>
      nx2553, ck=>conpro_system_clk);
   ix653 : an12_x1 port map ( q=>nx652, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(52));
   reg_xl_53 : sff2_x4 port map ( q=>xl_53, i0=>xl_53, i1=>nx662, cmd=>
      nx2553, ck=>conpro_system_clk);
   ix663 : an12_x1 port map ( q=>nx662, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(53));
   ix2483 : nxr2_x1 port map ( nq=>nx2482, i0=>xl_54, i1=>xl_55);
   reg_xl_54 : sff2_x4 port map ( q=>xl_54, i0=>xl_54, i1=>nx674, cmd=>
      nx2553, ck=>conpro_system_clk);
   ix675 : an12_x1 port map ( q=>nx674, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(54));
   reg_xl_55 : sff2_x4 port map ( q=>xl_55, i0=>xl_55, i1=>nx684, cmd=>
      nx2553, ck=>conpro_system_clk);
   ix685 : an12_x1 port map ( q=>nx684, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(55));
   ix2489 : nxr2_x1 port map ( nq=>nx2488, i0=>nx2490, i1=>nx2504);
   ix2491 : nxr2_x1 port map ( nq=>nx2490, i0=>nx2492, i1=>nx2498);
   ix2493 : nxr2_x1 port map ( nq=>nx2492, i0=>xl_56, i1=>xl_57);
   reg_xl_56 : sff2_x4 port map ( q=>xl_56, i0=>xl_56, i1=>nx700, cmd=>
      nx2555, ck=>conpro_system_clk);
   ix701 : an12_x1 port map ( q=>nx700, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(56));
   reg_xl_57 : sff2_x4 port map ( q=>xl_57, i0=>xl_57, i1=>nx710, cmd=>
      nx2555, ck=>conpro_system_clk);
   ix711 : an12_x1 port map ( q=>nx710, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(57));
   ix2499 : nxr2_x1 port map ( nq=>nx2498, i0=>xl_58, i1=>xl_59);
   reg_xl_58 : sff2_x4 port map ( q=>xl_58, i0=>xl_58, i1=>nx722, cmd=>
      nx2555, ck=>conpro_system_clk);
   ix723 : an12_x1 port map ( q=>nx722, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(58));
   reg_xl_59 : sff2_x4 port map ( q=>xl_59, i0=>xl_59, i1=>nx732, cmd=>
      nx2555, ck=>conpro_system_clk);
   ix733 : an12_x1 port map ( q=>nx732, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(59));
   ix2505 : nxr2_x1 port map ( nq=>nx2504, i0=>nx2506, i1=>nx2512);
   ix2507 : nxr2_x1 port map ( nq=>nx2506, i0=>xl_60, i1=>xl_61);
   reg_xl_60 : sff2_x4 port map ( q=>xl_60, i0=>xl_60, i1=>nx746, cmd=>nx4, 
      ck=>conpro_system_clk);
   ix747 : an12_x1 port map ( q=>nx746, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(60));
   reg_xl_61 : sff2_x4 port map ( q=>xl_61, i0=>xl_61, i1=>nx756, cmd=>nx4, 
      ck=>conpro_system_clk);
   ix757 : an12_x1 port map ( q=>nx756, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(61));
   ix2513 : nxr2_x1 port map ( nq=>nx2512, i0=>xl_62, i1=>xl_63);
   reg_xl_62 : sff2_x4 port map ( q=>xl_62, i0=>xl_62, i1=>nx768, cmd=>nx4, 
      ck=>conpro_system_clk);
   ix769 : an12_x1 port map ( q=>nx768, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(62));
   reg_xl_63 : sff2_x4 port map ( q=>xl_63, i0=>xl_63, i1=>nx778, cmd=>nx4, 
      ck=>conpro_system_clk);
   ix779 : an12_x1 port map ( q=>nx778, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity6_x_RD(63));
   ix5 : inv_x1 port map ( nq=>nx4, i=>nx2274);
   ix2526 : inv_x1 port map ( nq=>nx2527, i=>nx2274);
   ix2528 : inv_x1 port map ( nq=>nx2529, i=>nx2274);
   ix2530 : inv_x1 port map ( nq=>nx2531, i=>nx2274);
   ix2532 : inv_x1 port map ( nq=>nx2533, i=>nx2274);
   ix2534 : inv_x1 port map ( nq=>nx2535, i=>nx2274);
   ix2536 : inv_x1 port map ( nq=>nx2537, i=>nx2274);
   ix2538 : inv_x1 port map ( nq=>nx2539, i=>nx2274);
   ix2540 : inv_x1 port map ( nq=>nx2541, i=>nx2274);
   ix2542 : inv_x1 port map ( nq=>nx2543, i=>nx2274);
   ix2544 : inv_x1 port map ( nq=>nx2545, i=>nx2274);
   ix2546 : inv_x1 port map ( nq=>nx2547, i=>nx2274);
   ix2548 : inv_x1 port map ( nq=>nx2549, i=>nx2274);
   ix2550 : inv_x1 port map ( nq=>nx2551, i=>nx2274);
   ix2552 : inv_x1 port map ( nq=>nx2553, i=>nx2274);
   ix2554 : inv_x1 port map ( nq=>nx2555, i=>nx2274);
   ix801 : an12_x1 port map ( q=>REG_RET_FUN_parity6_p_WE_EXMPLR, i0=>
      pro_state_0, i1=>pro_state_1);
   ix31 : a2_x2 port map ( q=>PRO_FUN_parity6_END, i0=>pro_state_1, i1=>
      pro_state_0);
end main ;

