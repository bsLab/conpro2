-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D100 Genetic size: 2261933
--         Compile date: Thu Aug 13 10:06:37 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  reg RET_FUN_parity3_p: L1
  reg ARG_FUN_parity3_x: L64
end

data:
begin
  reg pl: L1
  reg xl: L64
end

code:
begin
          i1_assign: 
                     move (xl,ARG_FUN_parity3_x)
      i1_assign_end: 
                     nop
          i2_assign: 
                     move (pl,0)
      i2_assign_end: 
                     nop
          i3_assign: 
                     expr (pl,pl,lxor,xl[0]:L1)
                     nop
      i3_assign_end: 
                     nop
          i4_assign: 
                     expr (pl,pl,lxor,xl[1]:L1)
                     nop
      i4_assign_end: 
                     nop
          i5_assign: 
                     expr (pl,pl,lxor,xl[2]:L1)
                     nop
      i5_assign_end: 
                     nop
          i6_assign: 
                     expr (pl,pl,lxor,xl[3]:L1)
                     nop
      i6_assign_end: 
                     nop
          i7_assign: 
                     expr (pl,pl,lxor,xl[4]:L1)
                     nop
      i7_assign_end: 
                     nop
          i8_assign: 
                     expr (pl,pl,lxor,xl[5]:L1)
                     nop
      i8_assign_end: 
                     nop
          i9_assign: 
                     expr (pl,pl,lxor,xl[6]:L1)
                     nop
      i9_assign_end: 
                     nop
         i10_assign: 
                     expr (pl,pl,lxor,xl[7]:L1)
                     nop
     i10_assign_end: 
                     nop
         i11_assign: 
                     expr (pl,pl,lxor,xl[8]:L1)
                     nop
     i11_assign_end: 
                     nop
         i12_assign: 
                     expr (pl,pl,lxor,xl[9]:L1)
                     nop
     i12_assign_end: 
                     nop
         i13_assign: 
                     expr (pl,pl,lxor,xl[10]:L1)
                     nop
     i13_assign_end: 
                     nop
         i14_assign: 
                     expr (pl,pl,lxor,xl[11]:L1)
                     nop
     i14_assign_end: 
                     nop
         i15_assign: 
                     expr (pl,pl,lxor,xl[12]:L1)
                     nop
     i15_assign_end: 
                     nop
         i16_assign: 
                     expr (pl,pl,lxor,xl[13]:L1)
                     nop
     i16_assign_end: 
                     nop
         i17_assign: 
                     expr (pl,pl,lxor,xl[14]:L1)
                     nop
     i17_assign_end: 
                     nop
         i18_assign: 
                     expr (pl,pl,lxor,xl[15]:L1)
                     nop
     i18_assign_end: 
                     nop
         i19_assign: 
                     expr (pl,pl,lxor,xl[16]:L1)
                     nop
     i19_assign_end: 
                     nop
         i20_assign: 
                     expr (pl,pl,lxor,xl[17]:L1)
                     nop
     i20_assign_end: 
                     nop
         i21_assign: 
                     expr (pl,pl,lxor,xl[18]:L1)
                     nop
     i21_assign_end: 
                     nop
         i22_assign: 
                     expr (pl,pl,lxor,xl[19]:L1)
                     nop
     i22_assign_end: 
                     nop
         i23_assign: 
                     expr (pl,pl,lxor,xl[20]:L1)
                     nop
     i23_assign_end: 
                     nop
         i24_assign: 
                     expr (pl,pl,lxor,xl[21]:L1)
                     nop
     i24_assign_end: 
                     nop
         i25_assign: 
                     expr (pl,pl,lxor,xl[22]:L1)
                     nop
     i25_assign_end: 
                     nop
         i26_assign: 
                     expr (pl,pl,lxor,xl[23]:L1)
                     nop
     i26_assign_end: 
                     nop
         i27_assign: 
                     expr (pl,pl,lxor,xl[24]:L1)
                     nop
     i27_assign_end: 
                     nop
         i28_assign: 
                     expr (pl,pl,lxor,xl[25]:L1)
                     nop
     i28_assign_end: 
                     nop
         i29_assign: 
                     expr (pl,pl,lxor,xl[26]:L1)
                     nop
     i29_assign_end: 
                     nop
         i30_assign: 
                     expr (pl,pl,lxor,xl[27]:L1)
                     nop
     i30_assign_end: 
                     nop
         i31_assign: 
                     expr (pl,pl,lxor,xl[28]:L1)
                     nop
     i31_assign_end: 
                     nop
         i32_assign: 
                     expr (pl,pl,lxor,xl[29]:L1)
                     nop
     i32_assign_end: 
                     nop
         i33_assign: 
                     expr (pl,pl,lxor,xl[30]:L1)
                     nop
     i33_assign_end: 
                     nop
         i34_assign: 
                     expr (pl,pl,lxor,xl[31]:L1)
                     nop
     i34_assign_end: 
                     nop
         i35_assign: 
                     expr (pl,pl,lxor,xl[32]:L1)
                     nop
     i35_assign_end: 
                     nop
         i36_assign: 
                     expr (pl,pl,lxor,xl[33]:L1)
                     nop
     i36_assign_end: 
                     nop
         i37_assign: 
                     expr (pl,pl,lxor,xl[34]:L1)
                     nop
     i37_assign_end: 
                     nop
         i38_assign: 
                     expr (pl,pl,lxor,xl[35]:L1)
                     nop
     i38_assign_end: 
                     nop
         i39_assign: 
                     expr (pl,pl,lxor,xl[36]:L1)
                     nop
     i39_assign_end: 
                     nop
         i40_assign: 
                     expr (pl,pl,lxor,xl[37]:L1)
                     nop
     i40_assign_end: 
                     nop
         i41_assign: 
                     expr (pl,pl,lxor,xl[38]:L1)
                     nop
     i41_assign_end: 
                     nop
         i42_assign: 
                     expr (pl,pl,lxor,xl[39]:L1)
                     nop
     i42_assign_end: 
                     nop
         i43_assign: 
                     expr (pl,pl,lxor,xl[40]:L1)
                     nop
     i43_assign_end: 
                     nop
         i44_assign: 
                     expr (pl,pl,lxor,xl[41]:L1)
                     nop
     i44_assign_end: 
                     nop
         i45_assign: 
                     expr (pl,pl,lxor,xl[42]:L1)
                     nop
     i45_assign_end: 
                     nop
         i46_assign: 
                     expr (pl,pl,lxor,xl[43]:L1)
                     nop
     i46_assign_end: 
                     nop
         i47_assign: 
                     expr (pl,pl,lxor,xl[44]:L1)
                     nop
     i47_assign_end: 
                     nop
         i48_assign: 
                     expr (pl,pl,lxor,xl[45]:L1)
                     nop
     i48_assign_end: 
                     nop
         i49_assign: 
                     expr (pl,pl,lxor,xl[46]:L1)
                     nop
     i49_assign_end: 
                     nop
         i50_assign: 
                     expr (pl,pl,lxor,xl[47]:L1)
                     nop
     i50_assign_end: 
                     nop
         i51_assign: 
                     expr (pl,pl,lxor,xl[48]:L1)
                     nop
     i51_assign_end: 
                     nop
         i52_assign: 
                     expr (pl,pl,lxor,xl[49]:L1)
                     nop
     i52_assign_end: 
                     nop
         i53_assign: 
                     expr (pl,pl,lxor,xl[50]:L1)
                     nop
     i53_assign_end: 
                     nop
         i54_assign: 
                     expr (pl,pl,lxor,xl[51]:L1)
                     nop
     i54_assign_end: 
                     nop
         i55_assign: 
                     expr (pl,pl,lxor,xl[52]:L1)
                     nop
     i55_assign_end: 
                     nop
         i56_assign: 
                     expr (pl,pl,lxor,xl[53]:L1)
                     nop
     i56_assign_end: 
                     nop
         i57_assign: 
                     expr (pl,pl,lxor,xl[54]:L1)
                     nop
     i57_assign_end: 
                     nop
         i58_assign: 
                     expr (pl,pl,lxor,xl[55]:L1)
                     nop
     i58_assign_end: 
                     nop
         i59_assign: 
                     expr (pl,pl,lxor,xl[56]:L1)
                     nop
     i59_assign_end: 
                     nop
         i60_assign: 
                     expr (pl,pl,lxor,xl[57]:L1)
                     nop
     i60_assign_end: 
                     nop
         i61_assign: 
                     expr (pl,pl,lxor,xl[58]:L1)
                     nop
     i61_assign_end: 
                     nop
         i62_assign: 
                     expr (pl,pl,lxor,xl[59]:L1)
                     nop
     i62_assign_end: 
                     nop
         i63_assign: 
                     expr (pl,pl,lxor,xl[60]:L1)
                     nop
     i63_assign_end: 
                     nop
         i64_assign: 
                     expr (pl,pl,lxor,xl[61]:L1)
                     nop
     i64_assign_end: 
                     nop
         i65_assign: 
                     expr (pl,pl,lxor,xl[62]:L1)
                     nop
     i65_assign_end: 
                     nop
         i66_assign: 
                     expr (pl,pl,lxor,xl[63]:L1)
                     nop
     i66_assign_end: 
                     nop
         i67_assign: 
                     move (RET_FUN_parity3_p,pl)
     i67_assign_end: 
end
