-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D100 Genetic size: 2261933
--         Compile date: Thu Aug 13 10:06:37 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  reg ARG_FUN_parity5_x: L64
  reg d: L64
  reg RET_FUN_parity3_p: L1
  reg p: L1
  reg ARG_FUN_parity2_x: L64
  reg RET_FUN_parity5_p: L1
  reg ARG_FUN_parity4_x: L64
  reg RET_FUN_parity2_p: L1
  reg ARG_FUN_parity1_x: L64
  reg ARG_FUN_parity6_x: L64
  reg RET_FUN_parity4_p: L1
  function parity1(x)
  function parity2(x)
  function parity3(x)
  function parity4(x)
  function parity5(x)
  function parity6(x)
  reg ARG_FUN_parity3_x: L64
  reg RET_FUN_parity1_p: L1
  object LOCK_FUN_parity1: Mutex.mutex
  object LOCK_FUN_parity2: Mutex.mutex
  object LOCK_FUN_parity3: Mutex.mutex
  object LOCK_FUN_parity4: Mutex.mutex
  reg RET_FUN_parity6_p: L1
  object FUN_parity1: Process.Process
  object LOCK_FUN_parity5: Mutex.mutex
  object FUN_parity2: Process.Process
  object LOCK_FUN_parity6: Mutex.mutex
  object FUN_parity3: Process.Process
  object FUN_parity4: Process.Process
  object FUN_parity5: Process.Process
  object FUN_parity6: Process.Process
end

code:
begin
             i1_fun: 
                     fun (LOCK_FUN_parity1.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (LOCK_FUN_parity2.init,())
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (LOCK_FUN_parity3.init,())
         i3_fun_end: 
                     nop
             i4_fun: 
                     fun (LOCK_FUN_parity4.init,())
         i4_fun_end: 
                     nop
             i5_fun: 
                     fun (LOCK_FUN_parity5.init,())
         i5_fun_end: 
                     nop
             i6_fun: 
                     fun (LOCK_FUN_parity6.init,())
         i6_fun_end: 
                     nop
          i7_assign: 
                     move (d,305419888)
      i7_assign_end: 
                     nop
             i8_fun: 
                     fun (LOCK_FUN_parity1.lock,())
         i8_fun_end: 
                     nop
          i9_assign: 
                     move (ARG_FUN_parity1_x,d)
      i9_assign_end: 
                     nop
            i10_fun: 
                     fun (FUN_parity1.call,())
        i10_fun_end: 
                     nop
         i11_assign: 
                     move (p,RET_FUN_parity1_p)
     i11_assign_end: 
                     nop
            i12_fun: 
                     fun (LOCK_FUN_parity1.unlock,())
        i12_fun_end: 
                     nop
            i13_fun: 
                     fun (LOCK_FUN_parity2.lock,())
        i13_fun_end: 
                     nop
         i14_assign: 
                     move (ARG_FUN_parity2_x,d)
     i14_assign_end: 
                     nop
            i15_fun: 
                     fun (FUN_parity2.call,())
        i15_fun_end: 
                     nop
         i16_assign: 
                     move (p,RET_FUN_parity2_p)
     i16_assign_end: 
                     nop
            i17_fun: 
                     fun (LOCK_FUN_parity2.unlock,())
        i17_fun_end: 
                     nop
            i18_fun: 
                     fun (LOCK_FUN_parity3.lock,())
        i18_fun_end: 
                     nop
         i19_assign: 
                     move (ARG_FUN_parity3_x,d)
     i19_assign_end: 
                     nop
            i20_fun: 
                     fun (FUN_parity3.call,())
        i20_fun_end: 
                     nop
         i21_assign: 
                     move (p,RET_FUN_parity3_p)
     i21_assign_end: 
                     nop
            i22_fun: 
                     fun (LOCK_FUN_parity3.unlock,())
        i22_fun_end: 
                     nop
            i23_fun: 
                     fun (LOCK_FUN_parity4.lock,())
        i23_fun_end: 
                     nop
         i24_assign: 
                     move (ARG_FUN_parity4_x,d)
     i24_assign_end: 
                     nop
            i25_fun: 
                     fun (FUN_parity4.call,())
        i25_fun_end: 
                     nop
         i26_assign: 
                     move (p,RET_FUN_parity4_p)
     i26_assign_end: 
                     nop
            i27_fun: 
                     fun (LOCK_FUN_parity4.unlock,())
        i27_fun_end: 
                     nop
            i28_fun: 
                     fun (LOCK_FUN_parity5.lock,())
        i28_fun_end: 
                     nop
         i29_assign: 
                     move (ARG_FUN_parity5_x,d)
     i29_assign_end: 
                     nop
            i30_fun: 
                     fun (FUN_parity5.call,())
        i30_fun_end: 
                     nop
         i31_assign: 
                     move (p,RET_FUN_parity5_p)
     i31_assign_end: 
                     nop
            i32_fun: 
                     fun (LOCK_FUN_parity5.unlock,())
        i32_fun_end: 
                     nop
            i33_fun: 
                     fun (LOCK_FUN_parity6.lock,())
        i33_fun_end: 
                     nop
         i34_assign: 
                     move (ARG_FUN_parity6_x,d)
     i34_assign_end: 
                     nop
            i35_fun: 
                     fun (FUN_parity6.call,())
        i35_fun_end: 
                     nop
         i36_assign: 
                     move (p,RET_FUN_parity6_p)
     i36_assign_end: 
                     nop
            i37_fun: 
                     fun (LOCK_FUN_parity6.unlock,())
        i37_fun_end: 
end
