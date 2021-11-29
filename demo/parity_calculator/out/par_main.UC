modules:
begin
  module Par: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
end

import:
begin
  register ARG_FUN_parity5_x: L[64]
  register d: L[64]
  register RET_FUN_parity3_p: L[1]
  register p: L[1]
  register ARG_FUN_parity2_x: L[64]
  register RET_FUN_parity5_p: L[1]
  register ARG_FUN_parity4_x: L[64]
  register RET_FUN_parity2_p: L[1]
  register ARG_FUN_parity1_x: L[64]
  register ARG_FUN_parity6_x: L[64]
  register RET_FUN_parity4_p: L[1]
  function parity1(x:L[64])
  function parity2(x:L[64])
  function parity3(x:L[64])
  function parity4(x:L[64])
  function parity5(x:L[64])
  function parity6(x:L[64])
  register ARG_FUN_parity3_x: L[64]
  register RET_FUN_parity1_p: L[1]
  object LOCK_FUN_parity1: Mutex.mutex
  object LOCK_FUN_parity2: Mutex.mutex
  object LOCK_FUN_parity3: Mutex.mutex
  object LOCK_FUN_parity4: Mutex.mutex
  register RET_FUN_parity6_p: L[1]
  object FUN_parity1: Process.Process
  object LOCK_FUN_parity5: Mutex.mutex
  object FUN_parity2: Process.Process
  object LOCK_FUN_parity6: Mutex.mutex
  object FUN_parity3: Process.Process
  object FUN_parity4: Process.Process
  object FUN_parity5: Process.Process
  object FUN_parity6: Process.Process
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
end

code:
begin
               i1_fun: 
                       fun LOCK_FUN_parity1.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun LOCK_FUN_parity2.init()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun LOCK_FUN_parity3.init()
           i3_fun_end: 
                       nop
               i4_fun: 
                       fun LOCK_FUN_parity4.init()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun LOCK_FUN_parity5.init()
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun LOCK_FUN_parity6.init()
           i6_fun_end: 
                       nop
            i7_assign: 
                       move (d,305419888) with ET=L[64]
        i7_assign_end: 
                       nop
               i8_fun: 
                       fun LOCK_FUN_parity1.lock()
           i8_fun_end: 
                       nop
            i9_assign: 
                       move (ARG_FUN_parity1_x,d) with ET=L[64]
        i9_assign_end: 
                       nop
              i10_fun: 
                       fun FUN_parity1.call()
          i10_fun_end: 
                       nop
           i11_assign: 
                       move (p,RET_FUN_parity1_p) with ET=L[1]
       i11_assign_end: 
                       nop
              i12_fun: 
                       fun LOCK_FUN_parity1.unlock()
          i12_fun_end: 
                       nop
              i13_fun: 
                       fun LOCK_FUN_parity2.lock()
          i13_fun_end: 
                       nop
           i14_assign: 
                       move (ARG_FUN_parity2_x,d) with ET=L[64]
       i14_assign_end: 
                       nop
              i15_fun: 
                       fun FUN_parity2.call()
          i15_fun_end: 
                       nop
           i16_assign: 
                       move (p,RET_FUN_parity2_p) with ET=L[1]
       i16_assign_end: 
                       nop
              i17_fun: 
                       fun LOCK_FUN_parity2.unlock()
          i17_fun_end: 
                       nop
              i18_fun: 
                       fun LOCK_FUN_parity3.lock()
          i18_fun_end: 
                       nop
           i19_assign: 
                       move (ARG_FUN_parity3_x,d) with ET=L[64]
       i19_assign_end: 
                       nop
              i20_fun: 
                       fun FUN_parity3.call()
          i20_fun_end: 
                       nop
           i21_assign: 
                       move (p,RET_FUN_parity3_p) with ET=L[1]
       i21_assign_end: 
                       nop
              i22_fun: 
                       fun LOCK_FUN_parity3.unlock()
          i22_fun_end: 
                       nop
              i23_fun: 
                       fun LOCK_FUN_parity4.lock()
          i23_fun_end: 
                       nop
           i24_assign: 
                       move (ARG_FUN_parity4_x,d) with ET=L[64]
       i24_assign_end: 
                       nop
              i25_fun: 
                       fun FUN_parity4.call()
          i25_fun_end: 
                       nop
           i26_assign: 
                       move (p,RET_FUN_parity4_p) with ET=L[1]
       i26_assign_end: 
                       nop
              i27_fun: 
                       fun LOCK_FUN_parity4.unlock()
          i27_fun_end: 
                       nop
              i28_fun: 
                       fun LOCK_FUN_parity5.lock()
          i28_fun_end: 
                       nop
           i29_assign: 
                       move (ARG_FUN_parity5_x,d) with ET=L[64]
       i29_assign_end: 
                       nop
              i30_fun: 
                       fun FUN_parity5.call()
          i30_fun_end: 
                       nop
           i31_assign: 
                       move (p,RET_FUN_parity5_p) with ET=L[1]
       i31_assign_end: 
                       nop
              i32_fun: 
                       fun LOCK_FUN_parity5.unlock()
          i32_fun_end: 
                       nop
              i33_fun: 
                       fun LOCK_FUN_parity6.lock()
          i33_fun_end: 
                       nop
           i34_assign: 
                       move (ARG_FUN_parity6_x,d) with ET=L[64]
       i34_assign_end: 
                       nop
              i35_fun: 
                       fun FUN_parity6.call()
          i35_fun_end: 
                       nop
           i36_assign: 
                       move (p,RET_FUN_parity6_p) with ET=L[1]
       i36_assign_end: 
                       nop
              i37_fun: 
                       fun LOCK_FUN_parity6.unlock()
          i37_fun_end: 
end
