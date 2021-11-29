
-- 
-- Definition of  par_FUN_parity5
-- 
--      Fri Dec 19 11:31:38 2008
--      
--      LeonardoSpectrum Level 3, 2005b.24
-- 

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity par_FUN_parity5 is
   port (
      REG_ARG_FUN_parity5_x_RD : IN std_logic_vector (63 DOWNTO 0) ;
      REG_RET_FUN_parity5_p_WR : OUT std_logic ;
      REG_RET_FUN_parity5_p_WE : OUT std_logic ;
      PRO_FUN_parity5_ENABLE : IN std_logic ;
      PRO_FUN_parity5_END : OUT std_logic ;
      conpro_system_clk : IN std_logic ;
      conpro_system_reset : IN std_logic) ;
end par_FUN_parity5 ;

architecture main of par_FUN_parity5 is
   signal PRO_FUN_parity5_END_EXMPLR, REG_RET_FUN_parity5_p_WE_EXMPLR, pl, 
      pro_state_0, nx2, pro_state_66, pro_state_65, pro_state_64, 
      pro_state_63, pro_state_62, pro_state_61, pro_state_60, pro_state_59, 
      pro_state_58, pro_state_57, pro_state_56, pro_state_55, pro_state_54, 
      pro_state_53, pro_state_52, pro_state_51, pro_state_50, pro_state_49, 
      pro_state_48, pro_state_47, pro_state_46, pro_state_45, pro_state_44, 
      pro_state_43, pro_state_42, pro_state_41, pro_state_40, pro_state_39, 
      pro_state_38, pro_state_37, pro_state_36, pro_state_35, pro_state_34, 
      pro_state_33, pro_state_32, pro_state_31, pro_state_30, pro_state_29, 
      pro_state_28, pro_state_27, pro_state_26, pro_state_25, pro_state_24, 
      pro_state_23, pro_state_22, pro_state_21, pro_state_20, pro_state_19, 
      pro_state_18, pro_state_17, pro_state_16, pro_state_15, pro_state_14, 
      pro_state_13, pro_state_12, pro_state_11, pro_state_10, pro_state_9, 
      pro_state_8, pro_state_7, pro_state_6, pro_state_5, pro_state_4, 
      pro_state_3, pro_state_2, pro_state_1, nx12, nx22, nx32, nx42, nx52, 
      nx62, nx72, nx82, nx92, nx102, nx112, nx122, nx132, nx142, nx152, 
      nx162, nx172, nx182, nx192, nx202, nx212, nx222, nx232, nx242, nx252, 
      nx262, nx272, nx282, nx292, nx302, nx312, nx322, nx332, nx342, nx352, 
      nx362, nx372, nx382, nx392, nx402, nx412, nx422, nx432, nx442, nx452, 
      nx462, nx472, nx482, nx492, nx502, nx512, nx522, nx532, nx542, nx552, 
      nx562, nx572, nx582, nx592, nx602, nx612, nx622, nx632, nx642, nx652, 
      nx662, nx674, nx692, xl_0, nx694, nx698, nx706, xl_1, nx712, nx720, 
      xl_2, nx728, nx736, xl_3, nx742, nx750, xl_4, nx760, nx768, xl_5, 
      nx774, nx782, xl_6, nx790, nx798, xl_7, nx804, nx812, xl_8, nx824, 
      nx832, xl_9, nx838, nx846, xl_10, nx854, nx862, xl_11, nx868, nx876, 
      xl_12, nx886, nx894, xl_13, nx900, nx908, xl_14, nx916, nx924, xl_15, 
      nx930, nx938, nx948, xl_16, nx952, nx960, xl_17, nx966, nx974, xl_18, 
      nx982, nx990, xl_19, nx996, nx1004, xl_20, nx1014, nx1022, xl_21, 
      nx1028, nx1036, xl_22, nx1044, nx1052, xl_23, nx1058, nx1066, xl_24, 
      nx1078, nx1086, xl_25, nx1092, nx1100, xl_26, nx1108, nx1116, xl_27, 
      nx1122, nx1130, xl_28, nx1140, nx1148, xl_29, nx1154, nx1162, xl_30, 
      nx1170, nx1178, xl_31, nx1184, nx1192, nx1202, xl_32, nx1208, nx1216, 
      xl_33, nx1222, nx1230, xl_34, nx1238, nx1246, xl_35, nx1252, nx1260, 
      xl_36, nx1270, nx1278, xl_37, nx1284, nx1292, xl_38, nx1300, nx1308, 
      xl_39, nx1314, nx1322, xl_40, nx1334, nx1342, xl_41, nx1348, nx1356, 
      xl_42, nx1364, nx1372, xl_43, nx1378, nx1386, xl_44, nx1396, nx1404, 
      xl_45, nx1410, nx1418, xl_46, nx1426, nx1434, xl_47, nx1440, nx1448, 
      nx1458, xl_48, nx1462, nx1470, xl_49, nx1476, nx1484, xl_50, nx1492, 
      nx1500, xl_51, nx1506, nx1514, xl_52, nx1524, nx1532, xl_53, nx1538, 
      nx1546, xl_54, nx1554, nx1562, xl_55, nx1568, nx1576, xl_56, nx1588, 
      nx1596, xl_57, nx1602, nx1610, xl_58, nx1618, nx1626, xl_59, nx1632, 
      nx1640, xl_60, nx1650, nx1658, xl_61, nx1664, nx1672, xl_62, nx1680, 
      nx1688, xl_63, nx1694, nx1702, nx1712, nx1720, nx1756, nx1786, nx5171, 
      nx5174, nx5453, nx5467, nx5481, nx5496, nx5510, nx5524, nx5538, nx5553, 
      nx5567, nx5581, nx5595, nx5610, nx5624, nx5638, nx5652, nx5667, nx5675, 
      nx5679, nx5681, nx5683, nx5685, nx5688, nx5690, nx5692, nx5694, nx5701, 
      nx5703, nx5705, nx5707, nx5709, nx5711, nx5713, nx5715, nx5717, nx5719, 
      nx5721, nx5723, nx5725, nx5727, nx5729, nx5731, nx5733, nx5735, nx5737, 
      nx5739, nx5741, nx5743, nx5745, nx5747, nx5749, nx5751, nx5753, nx5755, 
      nx5757, nx5759: std_logic ;

begin
   REG_RET_FUN_parity5_p_WE <= REG_RET_FUN_parity5_p_WE_EXMPLR ;
   PRO_FUN_parity5_END <= PRO_FUN_parity5_END_EXMPLR ;
   ix1791 : a2_x2 port map ( q=>REG_RET_FUN_parity5_p_WR, i0=>nx5703, i1=>
      REG_RET_FUN_parity5_p_WE_EXMPLR);
   reg_pl : sff2_x4 port map ( q=>pl, i0=>nx5701, i1=>nx1720, cmd=>nx692, ck
      =>conpro_system_clk);
   ix1721 : no2_x1 port map ( nq=>nx1720, i0=>conpro_system_reset, i1=>
      nx5171);
   ix5172 : no4_x1 port map ( nq=>nx5171, i0=>nx1712, i1=>nx1458, i2=>nx1202, 
      i3=>nx948);
   ix1713 : na4_x1 port map ( nq=>nx1712, i0=>nx5174, i1=>nx5453, i2=>nx5467, 
      i3=>nx5481);
   ix5175 : noa2a2a2a24_x1 port map ( nq=>nx5174, i0=>pro_state_65, i1=>
      nx1702, i2=>pro_state_64, i3=>nx1688, i4=>pro_state_63, i5=>nx1672, i6
      =>pro_state_62, i7=>nx1658);
   reg_pro_state_65 : sff1_x4 port map ( q=>pro_state_65, i=>nx652, ck=>
      conpro_system_clk);
   reg_pro_state_64 : sff1_x4 port map ( q=>pro_state_64, i=>nx642, ck=>
      conpro_system_clk);
   reg_pro_state_63 : sff1_x4 port map ( q=>pro_state_63, i=>nx632, ck=>
      conpro_system_clk);
   reg_pro_state_62 : sff1_x4 port map ( q=>pro_state_62, i=>nx622, ck=>
      conpro_system_clk);
   reg_pro_state_61 : sff1_x4 port map ( q=>pro_state_61, i=>nx612, ck=>
      conpro_system_clk);
   reg_pro_state_60 : sff1_x4 port map ( q=>pro_state_60, i=>nx602, ck=>
      conpro_system_clk);
   reg_pro_state_59 : sff1_x4 port map ( q=>pro_state_59, i=>nx592, ck=>
      conpro_system_clk);
   reg_pro_state_58 : sff1_x4 port map ( q=>pro_state_58, i=>nx582, ck=>
      conpro_system_clk);
   reg_pro_state_57 : sff1_x4 port map ( q=>pro_state_57, i=>nx572, ck=>
      conpro_system_clk);
   reg_pro_state_56 : sff1_x4 port map ( q=>pro_state_56, i=>nx562, ck=>
      conpro_system_clk);
   reg_pro_state_55 : sff1_x4 port map ( q=>pro_state_55, i=>nx552, ck=>
      conpro_system_clk);
   reg_pro_state_54 : sff1_x4 port map ( q=>pro_state_54, i=>nx542, ck=>
      conpro_system_clk);
   reg_pro_state_53 : sff1_x4 port map ( q=>pro_state_53, i=>nx532, ck=>
      conpro_system_clk);
   reg_pro_state_52 : sff1_x4 port map ( q=>pro_state_52, i=>nx522, ck=>
      conpro_system_clk);
   reg_pro_state_51 : sff1_x4 port map ( q=>pro_state_51, i=>nx512, ck=>
      conpro_system_clk);
   reg_pro_state_50 : sff1_x4 port map ( q=>pro_state_50, i=>nx502, ck=>
      conpro_system_clk);
   reg_pro_state_49 : sff1_x4 port map ( q=>pro_state_49, i=>nx492, ck=>
      conpro_system_clk);
   reg_pro_state_48 : sff1_x4 port map ( q=>pro_state_48, i=>nx482, ck=>
      conpro_system_clk);
   reg_pro_state_47 : sff1_x4 port map ( q=>pro_state_47, i=>nx472, ck=>
      conpro_system_clk);
   reg_pro_state_46 : sff1_x4 port map ( q=>pro_state_46, i=>nx462, ck=>
      conpro_system_clk);
   reg_pro_state_45 : sff1_x4 port map ( q=>pro_state_45, i=>nx452, ck=>
      conpro_system_clk);
   reg_pro_state_44 : sff1_x4 port map ( q=>pro_state_44, i=>nx442, ck=>
      conpro_system_clk);
   reg_pro_state_43 : sff1_x4 port map ( q=>pro_state_43, i=>nx432, ck=>
      conpro_system_clk);
   reg_pro_state_42 : sff1_x4 port map ( q=>pro_state_42, i=>nx422, ck=>
      conpro_system_clk);
   reg_pro_state_41 : sff1_x4 port map ( q=>pro_state_41, i=>nx412, ck=>
      conpro_system_clk);
   reg_pro_state_40 : sff1_x4 port map ( q=>pro_state_40, i=>nx402, ck=>
      conpro_system_clk);
   reg_pro_state_39 : sff1_x4 port map ( q=>pro_state_39, i=>nx392, ck=>
      conpro_system_clk);
   reg_pro_state_38 : sff1_x4 port map ( q=>pro_state_38, i=>nx382, ck=>
      conpro_system_clk);
   reg_pro_state_37 : sff1_x4 port map ( q=>pro_state_37, i=>nx372, ck=>
      conpro_system_clk);
   reg_pro_state_36 : sff1_x4 port map ( q=>pro_state_36, i=>nx362, ck=>
      conpro_system_clk);
   reg_pro_state_35 : sff1_x4 port map ( q=>pro_state_35, i=>nx352, ck=>
      conpro_system_clk);
   reg_pro_state_34 : sff1_x4 port map ( q=>pro_state_34, i=>nx342, ck=>
      conpro_system_clk);
   reg_pro_state_33 : sff1_x4 port map ( q=>pro_state_33, i=>nx332, ck=>
      conpro_system_clk);
   reg_pro_state_32 : sff1_x4 port map ( q=>pro_state_32, i=>nx322, ck=>
      conpro_system_clk);
   reg_pro_state_31 : sff1_x4 port map ( q=>pro_state_31, i=>nx312, ck=>
      conpro_system_clk);
   reg_pro_state_30 : sff1_x4 port map ( q=>pro_state_30, i=>nx302, ck=>
      conpro_system_clk);
   reg_pro_state_29 : sff1_x4 port map ( q=>pro_state_29, i=>nx292, ck=>
      conpro_system_clk);
   reg_pro_state_28 : sff1_x4 port map ( q=>pro_state_28, i=>nx282, ck=>
      conpro_system_clk);
   reg_pro_state_27 : sff1_x4 port map ( q=>pro_state_27, i=>nx272, ck=>
      conpro_system_clk);
   reg_pro_state_26 : sff1_x4 port map ( q=>pro_state_26, i=>nx262, ck=>
      conpro_system_clk);
   reg_pro_state_25 : sff1_x4 port map ( q=>pro_state_25, i=>nx252, ck=>
      conpro_system_clk);
   reg_pro_state_24 : sff1_x4 port map ( q=>pro_state_24, i=>nx242, ck=>
      conpro_system_clk);
   reg_pro_state_23 : sff1_x4 port map ( q=>pro_state_23, i=>nx232, ck=>
      conpro_system_clk);
   reg_pro_state_22 : sff1_x4 port map ( q=>pro_state_22, i=>nx222, ck=>
      conpro_system_clk);
   reg_pro_state_21 : sff1_x4 port map ( q=>pro_state_21, i=>nx212, ck=>
      conpro_system_clk);
   reg_pro_state_20 : sff1_x4 port map ( q=>pro_state_20, i=>nx202, ck=>
      conpro_system_clk);
   reg_pro_state_19 : sff1_x4 port map ( q=>pro_state_19, i=>nx192, ck=>
      conpro_system_clk);
   reg_pro_state_18 : sff1_x4 port map ( q=>pro_state_18, i=>nx182, ck=>
      conpro_system_clk);
   reg_pro_state_17 : sff1_x4 port map ( q=>pro_state_17, i=>nx172, ck=>
      conpro_system_clk);
   reg_pro_state_16 : sff1_x4 port map ( q=>pro_state_16, i=>nx162, ck=>
      conpro_system_clk);
   reg_pro_state_15 : sff1_x4 port map ( q=>pro_state_15, i=>nx152, ck=>
      conpro_system_clk);
   reg_pro_state_14 : sff1_x4 port map ( q=>pro_state_14, i=>nx142, ck=>
      conpro_system_clk);
   reg_pro_state_13 : sff1_x4 port map ( q=>pro_state_13, i=>nx132, ck=>
      conpro_system_clk);
   reg_pro_state_12 : sff1_x4 port map ( q=>pro_state_12, i=>nx122, ck=>
      conpro_system_clk);
   reg_pro_state_11 : sff1_x4 port map ( q=>pro_state_11, i=>nx112, ck=>
      conpro_system_clk);
   reg_pro_state_10 : sff1_x4 port map ( q=>pro_state_10, i=>nx102, ck=>
      conpro_system_clk);
   reg_pro_state_9 : sff1_x4 port map ( q=>pro_state_9, i=>nx92, ck=>
      conpro_system_clk);
   reg_pro_state_8 : sff1_x4 port map ( q=>pro_state_8, i=>nx82, ck=>
      conpro_system_clk);
   reg_pro_state_7 : sff1_x4 port map ( q=>pro_state_7, i=>nx72, ck=>
      conpro_system_clk);
   reg_pro_state_6 : sff1_x4 port map ( q=>pro_state_6, i=>nx62, ck=>
      conpro_system_clk);
   reg_pro_state_5 : sff1_x4 port map ( q=>pro_state_5, i=>nx52, ck=>
      conpro_system_clk);
   reg_pro_state_4 : sff1_x4 port map ( q=>pro_state_4, i=>nx42, ck=>
      conpro_system_clk);
   reg_pro_state_3 : sff1_x4 port map ( q=>pro_state_3, i=>nx32, ck=>
      conpro_system_clk);
   reg_pro_state_2 : sff1_x4 port map ( q=>pro_state_2, i=>nx22, ck=>
      conpro_system_clk);
   reg_pro_state_1 : sff1_x4 port map ( q=>pro_state_1, i=>nx12, ck=>
      conpro_system_clk);
   reg_pro_state_0 : sff1_x4 port map ( q=>pro_state_0, i=>nx5709, ck=>
      conpro_system_clk);
   ix1703 : xr2_x1 port map ( q=>nx1702, i0=>nx5701, i1=>xl_63);
   reg_xl_63 : sff2_x4 port map ( q=>xl_63, i0=>xl_63, i1=>nx1694, cmd=>
      nx5731, ck=>conpro_system_clk);
   ix1695 : an12_x1 port map ( q=>nx1694, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(63));
   ix1689 : xr2_x1 port map ( q=>nx1688, i0=>nx5701, i1=>xl_62);
   reg_xl_62 : sff2_x4 port map ( q=>xl_62, i0=>xl_62, i1=>nx1680, cmd=>
      nx5731, ck=>conpro_system_clk);
   ix1681 : an12_x1 port map ( q=>nx1680, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(62));
   ix1673 : xr2_x1 port map ( q=>nx1672, i0=>nx5701, i1=>xl_61);
   reg_xl_61 : sff2_x4 port map ( q=>xl_61, i0=>xl_61, i1=>nx1664, cmd=>
      nx5731, ck=>conpro_system_clk);
   ix1665 : an12_x1 port map ( q=>nx1664, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(61));
   ix1659 : xr2_x1 port map ( q=>nx1658, i0=>nx5701, i1=>xl_60);
   reg_xl_60 : sff2_x4 port map ( q=>xl_60, i0=>xl_60, i1=>nx1650, cmd=>
      nx5731, ck=>conpro_system_clk);
   ix1651 : an12_x1 port map ( q=>nx1650, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(60));
   ix5454 : noa2a2a2a24_x1 port map ( nq=>nx5453, i0=>pro_state_61, i1=>
      nx1640, i2=>pro_state_60, i3=>nx1626, i4=>pro_state_59, i5=>nx1610, i6
      =>pro_state_58, i7=>nx1596);
   ix1641 : xr2_x1 port map ( q=>nx1640, i0=>nx5701, i1=>xl_59);
   reg_xl_59 : sff2_x4 port map ( q=>xl_59, i0=>xl_59, i1=>nx1632, cmd=>
      nx5733, ck=>conpro_system_clk);
   ix1633 : an12_x1 port map ( q=>nx1632, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(59));
   ix1627 : xr2_x1 port map ( q=>nx1626, i0=>nx5701, i1=>xl_58);
   reg_xl_58 : sff2_x4 port map ( q=>xl_58, i0=>xl_58, i1=>nx1618, cmd=>
      nx5733, ck=>conpro_system_clk);
   ix1619 : an12_x1 port map ( q=>nx1618, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(58));
   ix1611 : xr2_x1 port map ( q=>nx1610, i0=>nx5701, i1=>xl_57);
   reg_xl_57 : sff2_x4 port map ( q=>xl_57, i0=>xl_57, i1=>nx1602, cmd=>
      nx5733, ck=>conpro_system_clk);
   ix1603 : an12_x1 port map ( q=>nx1602, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(57));
   ix1597 : xr2_x1 port map ( q=>nx1596, i0=>nx5701, i1=>xl_56);
   reg_xl_56 : sff2_x4 port map ( q=>xl_56, i0=>xl_56, i1=>nx1588, cmd=>
      nx5733, ck=>conpro_system_clk);
   ix1589 : an12_x1 port map ( q=>nx1588, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(56));
   ix5468 : noa2a2a2a24_x1 port map ( nq=>nx5467, i0=>pro_state_57, i1=>
      nx1576, i2=>pro_state_56, i3=>nx1562, i4=>pro_state_55, i5=>nx1546, i6
      =>pro_state_54, i7=>nx1532);
   ix1577 : xr2_x1 port map ( q=>nx1576, i0=>nx5701, i1=>xl_55);
   reg_xl_55 : sff2_x4 port map ( q=>xl_55, i0=>xl_55, i1=>nx1568, cmd=>
      nx5735, ck=>conpro_system_clk);
   ix1569 : an12_x1 port map ( q=>nx1568, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(55));
   ix1563 : xr2_x1 port map ( q=>nx1562, i0=>nx5701, i1=>xl_54);
   reg_xl_54 : sff2_x4 port map ( q=>xl_54, i0=>xl_54, i1=>nx1554, cmd=>
      nx5735, ck=>conpro_system_clk);
   ix1555 : an12_x1 port map ( q=>nx1554, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(54));
   ix1547 : xr2_x1 port map ( q=>nx1546, i0=>nx5701, i1=>xl_53);
   reg_xl_53 : sff2_x4 port map ( q=>xl_53, i0=>xl_53, i1=>nx1538, cmd=>
      nx5735, ck=>conpro_system_clk);
   ix1539 : an12_x1 port map ( q=>nx1538, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(53));
   ix1533 : xr2_x1 port map ( q=>nx1532, i0=>nx5701, i1=>xl_52);
   reg_xl_52 : sff2_x4 port map ( q=>xl_52, i0=>xl_52, i1=>nx1524, cmd=>
      nx5735, ck=>conpro_system_clk);
   ix1525 : an12_x1 port map ( q=>nx1524, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(52));
   ix5482 : noa2a2a2a24_x1 port map ( nq=>nx5481, i0=>pro_state_53, i1=>
      nx1514, i2=>pro_state_52, i3=>nx1500, i4=>pro_state_51, i5=>nx1484, i6
      =>pro_state_50, i7=>nx1470);
   ix1515 : xr2_x1 port map ( q=>nx1514, i0=>nx5701, i1=>xl_51);
   reg_xl_51 : sff2_x4 port map ( q=>xl_51, i0=>xl_51, i1=>nx1506, cmd=>
      nx5737, ck=>conpro_system_clk);
   ix1507 : an12_x1 port map ( q=>nx1506, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(51));
   ix1501 : xr2_x1 port map ( q=>nx1500, i0=>nx5701, i1=>xl_50);
   reg_xl_50 : sff2_x4 port map ( q=>xl_50, i0=>xl_50, i1=>nx1492, cmd=>
      nx5737, ck=>conpro_system_clk);
   ix1493 : an12_x1 port map ( q=>nx1492, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(50));
   ix1485 : xr2_x1 port map ( q=>nx1484, i0=>nx5703, i1=>xl_49);
   reg_xl_49 : sff2_x4 port map ( q=>xl_49, i0=>xl_49, i1=>nx1476, cmd=>
      nx5737, ck=>conpro_system_clk);
   ix1477 : an12_x1 port map ( q=>nx1476, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(49));
   ix1471 : xr2_x1 port map ( q=>nx1470, i0=>nx5703, i1=>xl_48);
   reg_xl_48 : sff2_x4 port map ( q=>xl_48, i0=>xl_48, i1=>nx1462, cmd=>
      nx5737, ck=>conpro_system_clk);
   ix1463 : an12_x1 port map ( q=>nx1462, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(48));
   ix1459 : na4_x1 port map ( nq=>nx1458, i0=>nx5496, i1=>nx5510, i2=>nx5524, 
      i3=>nx5538);
   ix5497 : noa2a2a2a24_x1 port map ( nq=>nx5496, i0=>pro_state_49, i1=>
      nx1448, i2=>pro_state_48, i3=>nx1434, i4=>pro_state_47, i5=>nx1418, i6
      =>pro_state_46, i7=>nx1404);
   ix1449 : xr2_x1 port map ( q=>nx1448, i0=>nx5703, i1=>xl_47);
   reg_xl_47 : sff2_x4 port map ( q=>xl_47, i0=>xl_47, i1=>nx1440, cmd=>
      nx5739, ck=>conpro_system_clk);
   ix1441 : an12_x1 port map ( q=>nx1440, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(47));
   ix1435 : xr2_x1 port map ( q=>nx1434, i0=>nx5703, i1=>xl_46);
   reg_xl_46 : sff2_x4 port map ( q=>xl_46, i0=>xl_46, i1=>nx1426, cmd=>
      nx5739, ck=>conpro_system_clk);
   ix1427 : an12_x1 port map ( q=>nx1426, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(46));
   ix1419 : xr2_x1 port map ( q=>nx1418, i0=>nx5703, i1=>xl_45);
   reg_xl_45 : sff2_x4 port map ( q=>xl_45, i0=>xl_45, i1=>nx1410, cmd=>
      nx5739, ck=>conpro_system_clk);
   ix1411 : an12_x1 port map ( q=>nx1410, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(45));
   ix1405 : xr2_x1 port map ( q=>nx1404, i0=>nx5703, i1=>xl_44);
   reg_xl_44 : sff2_x4 port map ( q=>xl_44, i0=>xl_44, i1=>nx1396, cmd=>
      nx5739, ck=>conpro_system_clk);
   ix1397 : an12_x1 port map ( q=>nx1396, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(44));
   ix5511 : noa2a2a2a24_x1 port map ( nq=>nx5510, i0=>pro_state_45, i1=>
      nx1386, i2=>pro_state_44, i3=>nx1372, i4=>pro_state_43, i5=>nx1356, i6
      =>pro_state_42, i7=>nx1342);
   ix1387 : xr2_x1 port map ( q=>nx1386, i0=>nx5703, i1=>xl_43);
   reg_xl_43 : sff2_x4 port map ( q=>xl_43, i0=>xl_43, i1=>nx1378, cmd=>
      nx5741, ck=>conpro_system_clk);
   ix1379 : an12_x1 port map ( q=>nx1378, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(43));
   ix1373 : xr2_x1 port map ( q=>nx1372, i0=>nx5703, i1=>xl_42);
   reg_xl_42 : sff2_x4 port map ( q=>xl_42, i0=>xl_42, i1=>nx1364, cmd=>
      nx5741, ck=>conpro_system_clk);
   ix1365 : an12_x1 port map ( q=>nx1364, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(42));
   ix1357 : xr2_x1 port map ( q=>nx1356, i0=>nx5703, i1=>xl_41);
   reg_xl_41 : sff2_x4 port map ( q=>xl_41, i0=>xl_41, i1=>nx1348, cmd=>
      nx5741, ck=>conpro_system_clk);
   ix1349 : an12_x1 port map ( q=>nx1348, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(41));
   ix1343 : xr2_x1 port map ( q=>nx1342, i0=>nx5703, i1=>xl_40);
   reg_xl_40 : sff2_x4 port map ( q=>xl_40, i0=>xl_40, i1=>nx1334, cmd=>
      nx5741, ck=>conpro_system_clk);
   ix1335 : an12_x1 port map ( q=>nx1334, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(40));
   ix5525 : noa2a2a2a24_x1 port map ( nq=>nx5524, i0=>pro_state_41, i1=>
      nx1322, i2=>pro_state_40, i3=>nx1308, i4=>pro_state_39, i5=>nx1292, i6
      =>pro_state_38, i7=>nx1278);
   ix1323 : xr2_x1 port map ( q=>nx1322, i0=>nx5703, i1=>xl_39);
   reg_xl_39 : sff2_x4 port map ( q=>xl_39, i0=>xl_39, i1=>nx1314, cmd=>
      nx5743, ck=>conpro_system_clk);
   ix1315 : an12_x1 port map ( q=>nx1314, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(39));
   ix1309 : xr2_x1 port map ( q=>nx1308, i0=>nx5703, i1=>xl_38);
   reg_xl_38 : sff2_x4 port map ( q=>xl_38, i0=>xl_38, i1=>nx1300, cmd=>
      nx5743, ck=>conpro_system_clk);
   ix1301 : an12_x1 port map ( q=>nx1300, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(38));
   ix1293 : xr2_x1 port map ( q=>nx1292, i0=>nx5703, i1=>xl_37);
   reg_xl_37 : sff2_x4 port map ( q=>xl_37, i0=>xl_37, i1=>nx1284, cmd=>
      nx5743, ck=>conpro_system_clk);
   ix1285 : an12_x1 port map ( q=>nx1284, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(37));
   ix1279 : xr2_x1 port map ( q=>nx1278, i0=>nx5703, i1=>xl_36);
   reg_xl_36 : sff2_x4 port map ( q=>xl_36, i0=>xl_36, i1=>nx1270, cmd=>
      nx5743, ck=>conpro_system_clk);
   ix1271 : an12_x1 port map ( q=>nx1270, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(36));
   ix5539 : noa2a2a2a24_x1 port map ( nq=>nx5538, i0=>pro_state_37, i1=>
      nx1260, i2=>pro_state_36, i3=>nx1246, i4=>pro_state_35, i5=>nx1230, i6
      =>pro_state_34, i7=>nx1216);
   ix1261 : xr2_x1 port map ( q=>nx1260, i0=>nx5705, i1=>xl_35);
   reg_xl_35 : sff2_x4 port map ( q=>xl_35, i0=>xl_35, i1=>nx1252, cmd=>
      nx5745, ck=>conpro_system_clk);
   ix1253 : an12_x1 port map ( q=>nx1252, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(35));
   ix1247 : xr2_x1 port map ( q=>nx1246, i0=>nx5705, i1=>xl_34);
   reg_xl_34 : sff2_x4 port map ( q=>xl_34, i0=>xl_34, i1=>nx1238, cmd=>
      nx5745, ck=>conpro_system_clk);
   ix1239 : an12_x1 port map ( q=>nx1238, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(34));
   ix1231 : xr2_x1 port map ( q=>nx1230, i0=>nx5705, i1=>xl_33);
   reg_xl_33 : sff2_x4 port map ( q=>xl_33, i0=>xl_33, i1=>nx1222, cmd=>
      nx5745, ck=>conpro_system_clk);
   ix1223 : an12_x1 port map ( q=>nx1222, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(33));
   ix1217 : xr2_x1 port map ( q=>nx1216, i0=>nx5705, i1=>xl_32);
   reg_xl_32 : sff2_x4 port map ( q=>xl_32, i0=>xl_32, i1=>nx1208, cmd=>
      nx5745, ck=>conpro_system_clk);
   ix1209 : an12_x1 port map ( q=>nx1208, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(32));
   ix1203 : na4_x1 port map ( nq=>nx1202, i0=>nx5553, i1=>nx5567, i2=>nx5581, 
      i3=>nx5595);
   ix5554 : noa2a2a2a24_x1 port map ( nq=>nx5553, i0=>pro_state_33, i1=>
      nx1192, i2=>pro_state_32, i3=>nx1178, i4=>pro_state_31, i5=>nx1162, i6
      =>pro_state_30, i7=>nx1148);
   ix1193 : xr2_x1 port map ( q=>nx1192, i0=>nx5705, i1=>xl_31);
   reg_xl_31 : sff2_x4 port map ( q=>xl_31, i0=>xl_31, i1=>nx1184, cmd=>
      nx5747, ck=>conpro_system_clk);
   ix1185 : an12_x1 port map ( q=>nx1184, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(31));
   ix1179 : xr2_x1 port map ( q=>nx1178, i0=>nx5705, i1=>xl_30);
   reg_xl_30 : sff2_x4 port map ( q=>xl_30, i0=>xl_30, i1=>nx1170, cmd=>
      nx5747, ck=>conpro_system_clk);
   ix1171 : an12_x1 port map ( q=>nx1170, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(30));
   ix1163 : xr2_x1 port map ( q=>nx1162, i0=>nx5705, i1=>xl_29);
   reg_xl_29 : sff2_x4 port map ( q=>xl_29, i0=>xl_29, i1=>nx1154, cmd=>
      nx5747, ck=>conpro_system_clk);
   ix1155 : an12_x1 port map ( q=>nx1154, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(29));
   ix1149 : xr2_x1 port map ( q=>nx1148, i0=>nx5705, i1=>xl_28);
   reg_xl_28 : sff2_x4 port map ( q=>xl_28, i0=>xl_28, i1=>nx1140, cmd=>
      nx5747, ck=>conpro_system_clk);
   ix1141 : an12_x1 port map ( q=>nx1140, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(28));
   ix5568 : noa2a2a2a24_x1 port map ( nq=>nx5567, i0=>pro_state_29, i1=>
      nx1130, i2=>pro_state_28, i3=>nx1116, i4=>pro_state_27, i5=>nx1100, i6
      =>pro_state_26, i7=>nx1086);
   ix1131 : xr2_x1 port map ( q=>nx1130, i0=>nx5705, i1=>xl_27);
   reg_xl_27 : sff2_x4 port map ( q=>xl_27, i0=>xl_27, i1=>nx1122, cmd=>
      nx5749, ck=>conpro_system_clk);
   ix1123 : an12_x1 port map ( q=>nx1122, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(27));
   ix1117 : xr2_x1 port map ( q=>nx1116, i0=>nx5705, i1=>xl_26);
   reg_xl_26 : sff2_x4 port map ( q=>xl_26, i0=>xl_26, i1=>nx1108, cmd=>
      nx5749, ck=>conpro_system_clk);
   ix1109 : an12_x1 port map ( q=>nx1108, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(26));
   ix1101 : xr2_x1 port map ( q=>nx1100, i0=>nx5705, i1=>xl_25);
   reg_xl_25 : sff2_x4 port map ( q=>xl_25, i0=>xl_25, i1=>nx1092, cmd=>
      nx5749, ck=>conpro_system_clk);
   ix1093 : an12_x1 port map ( q=>nx1092, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(25));
   ix1087 : xr2_x1 port map ( q=>nx1086, i0=>nx5705, i1=>xl_24);
   reg_xl_24 : sff2_x4 port map ( q=>xl_24, i0=>xl_24, i1=>nx1078, cmd=>
      nx5749, ck=>conpro_system_clk);
   ix1079 : an12_x1 port map ( q=>nx1078, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(24));
   ix5582 : noa2a2a2a24_x1 port map ( nq=>nx5581, i0=>pro_state_25, i1=>
      nx1066, i2=>pro_state_24, i3=>nx1052, i4=>pro_state_23, i5=>nx1036, i6
      =>pro_state_22, i7=>nx1022);
   ix1067 : xr2_x1 port map ( q=>nx1066, i0=>nx5705, i1=>xl_23);
   reg_xl_23 : sff2_x4 port map ( q=>xl_23, i0=>xl_23, i1=>nx1058, cmd=>
      nx5751, ck=>conpro_system_clk);
   ix1059 : an12_x1 port map ( q=>nx1058, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(23));
   ix1053 : xr2_x1 port map ( q=>nx1052, i0=>nx5705, i1=>xl_22);
   reg_xl_22 : sff2_x4 port map ( q=>xl_22, i0=>xl_22, i1=>nx1044, cmd=>
      nx5751, ck=>conpro_system_clk);
   ix1045 : an12_x1 port map ( q=>nx1044, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(22));
   ix1037 : xr2_x1 port map ( q=>nx1036, i0=>nx5707, i1=>xl_21);
   reg_xl_21 : sff2_x4 port map ( q=>xl_21, i0=>xl_21, i1=>nx1028, cmd=>
      nx5751, ck=>conpro_system_clk);
   ix1029 : an12_x1 port map ( q=>nx1028, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(21));
   ix1023 : xr2_x1 port map ( q=>nx1022, i0=>nx5707, i1=>xl_20);
   reg_xl_20 : sff2_x4 port map ( q=>xl_20, i0=>xl_20, i1=>nx1014, cmd=>
      nx5751, ck=>conpro_system_clk);
   ix1015 : an12_x1 port map ( q=>nx1014, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(20));
   ix5596 : noa2a2a2a24_x1 port map ( nq=>nx5595, i0=>pro_state_21, i1=>
      nx1004, i2=>pro_state_20, i3=>nx990, i4=>pro_state_19, i5=>nx974, i6=>
      pro_state_18, i7=>nx960);
   ix1005 : xr2_x1 port map ( q=>nx1004, i0=>nx5707, i1=>xl_19);
   reg_xl_19 : sff2_x4 port map ( q=>xl_19, i0=>xl_19, i1=>nx996, cmd=>
      nx5753, ck=>conpro_system_clk);
   ix997 : an12_x1 port map ( q=>nx996, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(19));
   ix991 : xr2_x1 port map ( q=>nx990, i0=>nx5707, i1=>xl_18);
   reg_xl_18 : sff2_x4 port map ( q=>xl_18, i0=>xl_18, i1=>nx982, cmd=>
      nx5753, ck=>conpro_system_clk);
   ix983 : an12_x1 port map ( q=>nx982, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(18));
   ix975 : xr2_x1 port map ( q=>nx974, i0=>nx5707, i1=>xl_17);
   reg_xl_17 : sff2_x4 port map ( q=>xl_17, i0=>xl_17, i1=>nx966, cmd=>
      nx5753, ck=>conpro_system_clk);
   ix967 : an12_x1 port map ( q=>nx966, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(17));
   ix961 : xr2_x1 port map ( q=>nx960, i0=>nx5707, i1=>xl_16);
   reg_xl_16 : sff2_x4 port map ( q=>xl_16, i0=>xl_16, i1=>nx952, cmd=>
      nx5753, ck=>conpro_system_clk);
   ix953 : an12_x1 port map ( q=>nx952, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(16));
   ix949 : na4_x1 port map ( nq=>nx948, i0=>nx5610, i1=>nx5624, i2=>nx5638, 
      i3=>nx5652);
   ix5611 : noa2a2a2a24_x1 port map ( nq=>nx5610, i0=>pro_state_17, i1=>
      nx938, i2=>pro_state_16, i3=>nx924, i4=>pro_state_15, i5=>nx908, i6=>
      pro_state_14, i7=>nx894);
   ix939 : xr2_x1 port map ( q=>nx938, i0=>nx5707, i1=>xl_15);
   reg_xl_15 : sff2_x4 port map ( q=>xl_15, i0=>xl_15, i1=>nx930, cmd=>
      nx5755, ck=>conpro_system_clk);
   ix931 : an12_x1 port map ( q=>nx930, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(15));
   ix925 : xr2_x1 port map ( q=>nx924, i0=>nx5707, i1=>xl_14);
   reg_xl_14 : sff2_x4 port map ( q=>xl_14, i0=>xl_14, i1=>nx916, cmd=>
      nx5755, ck=>conpro_system_clk);
   ix917 : an12_x1 port map ( q=>nx916, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(14));
   ix909 : xr2_x1 port map ( q=>nx908, i0=>nx5707, i1=>xl_13);
   reg_xl_13 : sff2_x4 port map ( q=>xl_13, i0=>xl_13, i1=>nx900, cmd=>
      nx5755, ck=>conpro_system_clk);
   ix901 : an12_x1 port map ( q=>nx900, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(13));
   ix895 : xr2_x1 port map ( q=>nx894, i0=>nx5707, i1=>xl_12);
   reg_xl_12 : sff2_x4 port map ( q=>xl_12, i0=>xl_12, i1=>nx886, cmd=>
      nx5755, ck=>conpro_system_clk);
   ix887 : an12_x1 port map ( q=>nx886, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(12));
   ix5625 : noa2a2a2a24_x1 port map ( nq=>nx5624, i0=>pro_state_13, i1=>
      nx876, i2=>pro_state_12, i3=>nx862, i4=>pro_state_11, i5=>nx846, i6=>
      pro_state_10, i7=>nx832);
   ix877 : xr2_x1 port map ( q=>nx876, i0=>nx5707, i1=>xl_11);
   reg_xl_11 : sff2_x4 port map ( q=>xl_11, i0=>xl_11, i1=>nx868, cmd=>
      nx5757, ck=>conpro_system_clk);
   ix869 : an12_x1 port map ( q=>nx868, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(11));
   ix863 : xr2_x1 port map ( q=>nx862, i0=>nx5707, i1=>xl_10);
   reg_xl_10 : sff2_x4 port map ( q=>xl_10, i0=>xl_10, i1=>nx854, cmd=>
      nx5757, ck=>conpro_system_clk);
   ix855 : an12_x1 port map ( q=>nx854, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(10));
   ix847 : xr2_x1 port map ( q=>nx846, i0=>nx5707, i1=>xl_9);
   reg_xl_9 : sff2_x4 port map ( q=>xl_9, i0=>xl_9, i1=>nx838, cmd=>nx5757, 
      ck=>conpro_system_clk);
   ix839 : an12_x1 port map ( q=>nx838, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(9));
   ix833 : xr2_x1 port map ( q=>nx832, i0=>nx5707, i1=>xl_8);
   reg_xl_8 : sff2_x4 port map ( q=>xl_8, i0=>xl_8, i1=>nx824, cmd=>nx5757, 
      ck=>conpro_system_clk);
   ix825 : an12_x1 port map ( q=>nx824, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(8));
   ix5639 : noa2a2a2a24_x1 port map ( nq=>nx5638, i0=>pro_state_9, i1=>nx812, 
      i2=>pro_state_8, i3=>nx798, i4=>pro_state_7, i5=>nx782, i6=>
      pro_state_6, i7=>nx768);
   ix813 : xr2_x1 port map ( q=>nx812, i0=>pl, i1=>xl_7);
   reg_xl_7 : sff2_x4 port map ( q=>xl_7, i0=>xl_7, i1=>nx804, cmd=>nx5759, 
      ck=>conpro_system_clk);
   ix805 : an12_x1 port map ( q=>nx804, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(7));
   ix799 : xr2_x1 port map ( q=>nx798, i0=>pl, i1=>xl_6);
   reg_xl_6 : sff2_x4 port map ( q=>xl_6, i0=>xl_6, i1=>nx790, cmd=>nx5759, 
      ck=>conpro_system_clk);
   ix791 : an12_x1 port map ( q=>nx790, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(6));
   ix783 : xr2_x1 port map ( q=>nx782, i0=>pl, i1=>xl_5);
   reg_xl_5 : sff2_x4 port map ( q=>xl_5, i0=>xl_5, i1=>nx774, cmd=>nx5759, 
      ck=>conpro_system_clk);
   ix775 : an12_x1 port map ( q=>nx774, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(5));
   ix769 : xr2_x1 port map ( q=>nx768, i0=>pl, i1=>xl_4);
   reg_xl_4 : sff2_x4 port map ( q=>xl_4, i0=>xl_4, i1=>nx760, cmd=>nx5759, 
      ck=>conpro_system_clk);
   ix761 : an12_x1 port map ( q=>nx760, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(4));
   ix5653 : noa2a2a2a24_x1 port map ( nq=>nx5652, i0=>pro_state_5, i1=>nx750, 
      i2=>pro_state_4, i3=>nx736, i4=>pro_state_3, i5=>nx720, i6=>
      pro_state_2, i7=>nx706);
   ix751 : xr2_x1 port map ( q=>nx750, i0=>pl, i1=>xl_3);
   reg_xl_3 : sff2_x4 port map ( q=>xl_3, i0=>xl_3, i1=>nx742, cmd=>nx694, 
      ck=>conpro_system_clk);
   ix743 : an12_x1 port map ( q=>nx742, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(3));
   ix737 : xr2_x1 port map ( q=>nx736, i0=>pl, i1=>xl_2);
   reg_xl_2 : sff2_x4 port map ( q=>xl_2, i0=>xl_2, i1=>nx728, cmd=>nx694, 
      ck=>conpro_system_clk);
   ix729 : an12_x1 port map ( q=>nx728, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(2));
   ix721 : xr2_x1 port map ( q=>nx720, i0=>pl, i1=>xl_1);
   reg_xl_1 : sff2_x4 port map ( q=>xl_1, i0=>xl_1, i1=>nx712, cmd=>nx694, 
      ck=>conpro_system_clk);
   ix713 : an12_x1 port map ( q=>nx712, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(1));
   ix707 : xr2_x1 port map ( q=>nx706, i0=>pl, i1=>xl_0);
   reg_xl_0 : sff2_x4 port map ( q=>xl_0, i0=>xl_0, i1=>nx698, cmd=>nx694, 
      ck=>conpro_system_clk);
   ix699 : an12_x1 port map ( q=>nx698, i0=>conpro_system_reset, i1=>
      REG_ARG_FUN_parity5_x_RD(0));
   ix5668 : o3_x2 port map ( q=>nx5667, i0=>pro_state_0, i1=>
      PRO_FUN_parity5_END_EXMPLR, i2=>pro_state_66);
   reg_pro_state_67 : sff1_x4 port map ( q=>PRO_FUN_parity5_END_EXMPLR, i=>
      nx674, ck=>conpro_system_clk);
   ix675 : ao22_x2 port map ( q=>nx674, i0=>pro_state_66, i1=>
      PRO_FUN_parity5_END_EXMPLR, i2=>nx5675);
   reg_pro_state_66 : sff1_x4 port map ( q=>pro_state_66, i=>nx662, ck=>
      conpro_system_clk);
   ix1789 : o2_x2 port map ( q=>REG_RET_FUN_parity5_p_WE_EXMPLR, i0=>nx1786, 
      i1=>nx1756);
   ix1787 : na4_x1 port map ( nq=>nx1786, i0=>nx5679, i1=>nx5681, i2=>nx5683, 
      i3=>nx5685);
   ix5680 : no4_x1 port map ( nq=>nx5679, i0=>pro_state_4, i1=>pro_state_6, 
      i2=>pro_state_8, i3=>pro_state_10);
   ix5682 : no4_x1 port map ( nq=>nx5681, i0=>pro_state_12, i1=>pro_state_14, 
      i2=>pro_state_16, i3=>pro_state_18);
   ix5684 : no4_x1 port map ( nq=>nx5683, i0=>pro_state_20, i1=>pro_state_22, 
      i2=>pro_state_24, i3=>pro_state_26);
   ix5686 : no4_x1 port map ( nq=>nx5685, i0=>pro_state_28, i1=>pro_state_30, 
      i2=>pro_state_32, i3=>pro_state_34);
   ix1757 : na4_x1 port map ( nq=>nx1756, i0=>nx5688, i1=>nx5690, i2=>nx5692, 
      i3=>nx5694);
   ix5689 : no4_x1 port map ( nq=>nx5688, i0=>pro_state_36, i1=>pro_state_38, 
      i2=>pro_state_40, i3=>pro_state_42);
   ix5691 : no4_x1 port map ( nq=>nx5690, i0=>pro_state_44, i1=>pro_state_46, 
      i2=>pro_state_48, i3=>pro_state_50);
   ix5693 : no4_x1 port map ( nq=>nx5692, i0=>pro_state_52, i1=>pro_state_54, 
      i2=>pro_state_56, i3=>pro_state_58);
   ix5695 : no4_x1 port map ( nq=>nx5694, i0=>pro_state_60, i1=>pro_state_62, 
      i2=>pro_state_64, i3=>pro_state_66);
   ix5676 : inv_x1 port map ( nq=>nx5675, i=>nx2);
   reg_pl_rep_1 : sff2_x4 port map ( q=>nx5701, i0=>pl, i1=>nx1720, cmd=>
      nx692, ck=>conpro_system_clk);
   reg_pl_rep_2 : sff2_x4 port map ( q=>nx5703, i0=>nx5701, i1=>nx1720, cmd
      =>nx692, ck=>conpro_system_clk);
   reg_pl_rep_3 : sff2_x4 port map ( q=>nx5705, i0=>nx5701, i1=>nx1720, cmd
      =>nx692, ck=>conpro_system_clk);
   reg_pl_rep_4 : sff2_x4 port map ( q=>nx5707, i0=>nx5701, i1=>nx1720, cmd
      =>nx692, ck=>conpro_system_clk);
   ix653 : an12_x1 port map ( q=>nx652, i0=>nx5729, i1=>pro_state_64);
   ix643 : an12_x1 port map ( q=>nx642, i0=>nx5729, i1=>pro_state_63);
   ix633 : an12_x1 port map ( q=>nx632, i0=>nx5729, i1=>pro_state_62);
   ix623 : an12_x1 port map ( q=>nx622, i0=>nx5729, i1=>pro_state_61);
   ix613 : an12_x1 port map ( q=>nx612, i0=>nx5729, i1=>pro_state_60);
   ix603 : an12_x1 port map ( q=>nx602, i0=>nx5729, i1=>pro_state_59);
   ix593 : an12_x1 port map ( q=>nx592, i0=>nx5727, i1=>pro_state_58);
   ix583 : an12_x1 port map ( q=>nx582, i0=>nx5727, i1=>pro_state_57);
   ix573 : an12_x1 port map ( q=>nx572, i0=>nx5727, i1=>pro_state_56);
   ix563 : an12_x1 port map ( q=>nx562, i0=>nx5727, i1=>pro_state_55);
   ix553 : an12_x1 port map ( q=>nx552, i0=>nx5727, i1=>pro_state_54);
   ix543 : an12_x1 port map ( q=>nx542, i0=>nx5727, i1=>pro_state_53);
   ix533 : an12_x1 port map ( q=>nx532, i0=>nx5725, i1=>pro_state_52);
   ix523 : an12_x1 port map ( q=>nx522, i0=>nx5725, i1=>pro_state_51);
   ix513 : an12_x1 port map ( q=>nx512, i0=>nx5725, i1=>pro_state_50);
   ix503 : an12_x1 port map ( q=>nx502, i0=>nx5725, i1=>pro_state_49);
   ix493 : an12_x1 port map ( q=>nx492, i0=>nx5725, i1=>pro_state_48);
   ix483 : an12_x1 port map ( q=>nx482, i0=>nx5725, i1=>pro_state_47);
   ix473 : an12_x1 port map ( q=>nx472, i0=>nx5723, i1=>pro_state_46);
   ix463 : an12_x1 port map ( q=>nx462, i0=>nx5723, i1=>pro_state_45);
   ix453 : an12_x1 port map ( q=>nx452, i0=>nx5723, i1=>pro_state_44);
   ix443 : an12_x1 port map ( q=>nx442, i0=>nx5723, i1=>pro_state_43);
   ix433 : an12_x1 port map ( q=>nx432, i0=>nx5723, i1=>pro_state_42);
   ix423 : an12_x1 port map ( q=>nx422, i0=>nx5723, i1=>pro_state_41);
   ix413 : an12_x1 port map ( q=>nx412, i0=>nx5721, i1=>pro_state_40);
   ix403 : an12_x1 port map ( q=>nx402, i0=>nx5721, i1=>pro_state_39);
   ix393 : an12_x1 port map ( q=>nx392, i0=>nx5721, i1=>pro_state_38);
   ix383 : an12_x1 port map ( q=>nx382, i0=>nx5721, i1=>pro_state_37);
   ix373 : an12_x1 port map ( q=>nx372, i0=>nx5721, i1=>pro_state_36);
   ix363 : an12_x1 port map ( q=>nx362, i0=>nx5721, i1=>pro_state_35);
   ix353 : an12_x1 port map ( q=>nx352, i0=>nx5719, i1=>pro_state_34);
   ix343 : an12_x1 port map ( q=>nx342, i0=>nx5719, i1=>pro_state_33);
   ix333 : an12_x1 port map ( q=>nx332, i0=>nx5719, i1=>pro_state_32);
   ix323 : an12_x1 port map ( q=>nx322, i0=>nx5719, i1=>pro_state_31);
   ix313 : an12_x1 port map ( q=>nx312, i0=>nx5719, i1=>pro_state_30);
   ix303 : an12_x1 port map ( q=>nx302, i0=>nx5719, i1=>pro_state_29);
   ix293 : an12_x1 port map ( q=>nx292, i0=>nx5717, i1=>pro_state_28);
   ix283 : an12_x1 port map ( q=>nx282, i0=>nx5717, i1=>pro_state_27);
   ix273 : an12_x1 port map ( q=>nx272, i0=>nx5717, i1=>pro_state_26);
   ix263 : an12_x1 port map ( q=>nx262, i0=>nx5717, i1=>pro_state_25);
   ix253 : an12_x1 port map ( q=>nx252, i0=>nx5717, i1=>pro_state_24);
   ix243 : an12_x1 port map ( q=>nx242, i0=>nx5717, i1=>pro_state_23);
   ix233 : an12_x1 port map ( q=>nx232, i0=>nx5715, i1=>pro_state_22);
   ix223 : an12_x1 port map ( q=>nx222, i0=>nx5715, i1=>pro_state_21);
   ix213 : an12_x1 port map ( q=>nx212, i0=>nx5715, i1=>pro_state_20);
   ix203 : an12_x1 port map ( q=>nx202, i0=>nx5715, i1=>pro_state_19);
   ix193 : an12_x1 port map ( q=>nx192, i0=>nx5715, i1=>pro_state_18);
   ix183 : an12_x1 port map ( q=>nx182, i0=>nx5715, i1=>pro_state_17);
   ix173 : an12_x1 port map ( q=>nx172, i0=>nx5713, i1=>pro_state_16);
   ix163 : an12_x1 port map ( q=>nx162, i0=>nx5713, i1=>pro_state_15);
   ix153 : an12_x1 port map ( q=>nx152, i0=>nx5713, i1=>pro_state_14);
   ix143 : an12_x1 port map ( q=>nx142, i0=>nx5713, i1=>pro_state_13);
   ix133 : an12_x1 port map ( q=>nx132, i0=>nx5713, i1=>pro_state_12);
   ix123 : an12_x1 port map ( q=>nx122, i0=>nx5713, i1=>pro_state_11);
   ix113 : an12_x1 port map ( q=>nx112, i0=>nx5711, i1=>pro_state_10);
   ix103 : an12_x1 port map ( q=>nx102, i0=>nx5711, i1=>pro_state_9);
   ix93 : an12_x1 port map ( q=>nx92, i0=>nx5711, i1=>pro_state_8);
   ix83 : an12_x1 port map ( q=>nx82, i0=>nx5711, i1=>pro_state_7);
   ix73 : an12_x1 port map ( q=>nx72, i0=>nx5711, i1=>pro_state_6);
   ix63 : an12_x1 port map ( q=>nx62, i0=>nx5711, i1=>pro_state_5);
   ix53 : an12_x1 port map ( q=>nx52, i0=>nx5709, i1=>pro_state_4);
   ix43 : an12_x1 port map ( q=>nx42, i0=>nx5709, i1=>pro_state_3);
   ix33 : an12_x1 port map ( q=>nx32, i0=>nx5709, i1=>pro_state_2);
   ix23 : an12_x1 port map ( q=>nx22, i0=>nx5709, i1=>pro_state_1);
   ix13 : an12_x1 port map ( q=>nx12, i0=>nx5709, i1=>pro_state_0);
   ix3 : on12_x1 port map ( q=>nx2, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix695 : o2_x2 port map ( q=>nx694, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix693 : on12_x4 port map ( q=>nx692, i0=>nx5667, i1=>conpro_system_reset
   );
   ix663 : an12_x1 port map ( q=>nx662, i0=>nx2, i1=>pro_state_65);
   ix5708 : on12_x1 port map ( q=>nx5709, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5710 : on12_x1 port map ( q=>nx5711, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5712 : on12_x1 port map ( q=>nx5713, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5714 : on12_x1 port map ( q=>nx5715, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5716 : on12_x1 port map ( q=>nx5717, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5718 : on12_x1 port map ( q=>nx5719, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5720 : on12_x1 port map ( q=>nx5721, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5722 : on12_x1 port map ( q=>nx5723, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5724 : on12_x1 port map ( q=>nx5725, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5726 : on12_x1 port map ( q=>nx5727, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5728 : on12_x1 port map ( q=>nx5729, i0=>PRO_FUN_parity5_ENABLE, i1=>
      conpro_system_reset);
   ix5730 : o2_x2 port map ( q=>nx5731, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5732 : o2_x2 port map ( q=>nx5733, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5734 : o2_x2 port map ( q=>nx5735, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5736 : o2_x2 port map ( q=>nx5737, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5738 : o2_x2 port map ( q=>nx5739, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5740 : o2_x2 port map ( q=>nx5741, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5742 : o2_x2 port map ( q=>nx5743, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5744 : o2_x2 port map ( q=>nx5745, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5746 : o2_x2 port map ( q=>nx5747, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5748 : o2_x2 port map ( q=>nx5749, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5750 : o2_x2 port map ( q=>nx5751, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5752 : o2_x2 port map ( q=>nx5753, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5754 : o2_x2 port map ( q=>nx5755, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5756 : o2_x2 port map ( q=>nx5757, i0=>pro_state_1, i1=>
      conpro_system_reset);
   ix5758 : o2_x2 port map ( q=>nx5759, i0=>pro_state_1, i1=>
      conpro_system_reset);
end main ;

