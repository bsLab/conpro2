modules:
begin
  module R: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
  module Ram: IMPORT
end

import:
begin
  object FUN_print_mem_err: Process.Process
  function print_kb(k:L[8],v:L[12])
  register ARG_FUN_print_mem_err_addr: L[19]
  object LOCK_FUN_print_kb: Mutex.mutex
  object print_lock: Mutex.mutex
  object FUN_print_kb: Process.Process
  function print_mem_err(addr:L[19])
  register ARG_FUN_print_kb_k: L[8]
  register ARG_FUN_print_kb_v: L[12]
  object ru: Ram.ram
  object LOCK_FUN_print_mem_err: Mutex.mutex
end

data:
begin
  register x: L[8]
  register y: L[8]
  register kb: L[12]
  register addr: L[19]
  register LOOP_i_5: I[21]
  register LOOP_i_6: I[21]
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
  type Ram.ram: {
      method write (L[8]:RHS,L[19]:RHS)
      method read (L[8]:LHS,L[19]:RHS)
      method interface (L[8]:LRHS,L[19]:RHS,L[1]:LHS,L[1]:LHS,L[1]:LHS)
    }
end

code:
begin
            i1_assign: 
                       move (x,3) with ET=L[8]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (kb,0) with ET=L[12]
        i2_assign_end: 
                       nop
          i3_for_loop: 
                       move (LOOP_i_5,0) with ET=I[21]
     i3_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],524287,>=,LOOP_i_5) with ET=I[21]
                       falsejump ($immed.[1],i14_assign)
            i4_assign: 
                       move (addr,LOOP_i_5:CT=L[0]) with ET=L[19]
        i4_assign_end: 
                       nop
               i5_fun: 
                       fun ru.write(x,addr)
           i5_fun_end: 
                       nop
            i6_assign: 
                       expr (x,x,+,7) with ET=L[8]
                       nop
        i6_assign_end: 
                       nop
            i7_branch: 
                       bind (2)
                       expr ($immed.[1],addr[0 to 9],=,0) with ET=L[10]
                       falsejump ($immed.[1],i3_for_loop_incr)
            i8_assign: 
                       expr (kb,kb,+,1) with ET=L[12]
                       nop
        i8_assign_end: 
                       nop
               i9_fun: 
                       fun LOCK_FUN_print_kb.lock()
           i9_fun_end: 
                       nop
           i10_assign: 
                       move (ARG_FUN_print_kb_k,'W') with ET=L[8]
       i10_assign_end: 
                       nop
           i11_assign: 
                       move (ARG_FUN_print_kb_v,kb)
       i11_assign_end: 
                       nop
              i12_fun: 
                       fun FUN_print_kb.call()
          i12_fun_end: 
                       nop
              i13_fun: 
                       fun LOCK_FUN_print_kb.unlock()
          i13_fun_end: 
                       nop
        i7_branch_end: 
                       nop
     i3_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_5,LOOP_i_5,+,1) with ET=I[21]
                       nop
                       jump (i3_for_loop_cond)
      i3_for_loop_end: 
                       nop
           i14_assign: 
                       move (x,3) with ET=L[8]
       i14_assign_end: 
                       nop
           i15_assign: 
                       move (kb,0) with ET=L[12]
       i15_assign_end: 
                       nop
         i16_for_loop: 
                       move (LOOP_i_6,0) with ET=I[21]
    i16_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],524287,>=,LOOP_i_6) with ET=I[21]
                       falsejump ($immed.[1],%END)
           i17_assign: 
                       move (addr,LOOP_i_6:CT=L[0]) with ET=L[19]
       i17_assign_end: 
                       nop
              i18_fun: 
                       fun ru.read(y,addr)
          i18_fun_end: 
                       nop
           i19_branch: 
                       bind (2)
                       expr ($immed.[1],x,<>,y)
                       falsejump ($immed.[1],i25_assign)
              i20_fun: 
                       fun print_lock.lock()
          i20_fun_end: 
                       nop
              i21_fun: 
                       fun LOCK_FUN_print_mem_err.lock()
          i21_fun_end: 
                       nop
           i22_assign: 
                       move (ARG_FUN_print_mem_err_addr,addr)
       i22_assign_end: 
                       nop
              i23_fun: 
                       fun FUN_print_mem_err.call()
          i23_fun_end: 
                       nop
              i24_fun: 
                       fun LOCK_FUN_print_mem_err.unlock()
          i24_fun_end: 
                       nop
       i19_branch_end: 
                       nop
           i25_assign: 
                       expr (x,x,+,7) with ET=L[8]
                       nop
       i25_assign_end: 
                       nop
           i26_branch: 
                       bind (2)
                       expr ($immed.[1],addr[0 to 9],=,0) with ET=L[10]
                       falsejump ($immed.[1],i16_for_loop_incr)
           i27_assign: 
                       expr (kb,kb,+,1) with ET=L[12]
                       nop
       i27_assign_end: 
                       nop
              i28_fun: 
                       fun LOCK_FUN_print_kb.lock()
          i28_fun_end: 
                       nop
           i29_assign: 
                       move (ARG_FUN_print_kb_k,'R') with ET=L[8]
       i29_assign_end: 
                       nop
           i30_assign: 
                       move (ARG_FUN_print_kb_v,kb)
       i30_assign_end: 
                       nop
              i31_fun: 
                       fun FUN_print_kb.call()
          i31_fun_end: 
                       nop
              i32_fun: 
                       fun LOCK_FUN_print_kb.unlock()
          i32_fun_end: 
                       nop
       i26_branch_end: 
                       nop
    i16_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_6,LOOP_i_6,+,1) with ET=I[21]
                       nop
                       jump (i16_for_loop_cond)
     i16_for_loop_end: 
end
