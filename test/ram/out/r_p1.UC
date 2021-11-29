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
end

import:
begin
  register d: L[8]
  object FUN_print_val: Process.Process
  queue q_u1_tx: L[8]
  register ARG_FUN_print_val_v: L[8]
  function print_val(v:L[8])
  object LOCK_FUN_print_val: Mutex.mutex
  object FUN_print_line: Process.Process
  queue q_u1_rx: L[8]
  function print_line(v:L[8])
  register stat_req: L[1]
  object LOCK_FUN_print_line: Mutex.mutex
  register ARG_FUN_print_line_v: L[8]
end

data:
begin
  register t: L[8]
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
              i1_loop: 
                       nop
            i2_assign: 
                       move (t,q_u1_rx)
        i2_assign_end: 
                       nop
               i3_fun: 
                       fun LOCK_FUN_print_val.lock()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (ARG_FUN_print_val_v,t)
        i4_assign_end: 
                       nop
               i5_fun: 
                       fun FUN_print_val.call()
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun LOCK_FUN_print_val.unlock()
           i6_fun_end: 
                       nop
               i7_fun: 
                       fun LOCK_FUN_print_line.lock()
           i7_fun_end: 
                       nop
            i8_assign: 
                       move (ARG_FUN_print_line_v,t)
        i8_assign_end: 
                       nop
               i9_fun: 
                       fun FUN_print_line.call()
           i9_fun_end: 
                       nop
              i10_fun: 
                       fun LOCK_FUN_print_line.unlock()
          i10_fun_end: 
                       nop
           i11_branch: 
                       bind (2)
                       expr ($immed.[1],t,=,) with ET=L[8]
                       falsejump ($immed.[1],i14_assign)
           i12_assign: 
                       expr (d,t,-,1) with ET=L[8]
                       nop
       i12_assign_end: 
                       nop
           i13_assign: 
                       move (stat_req,1) with ET=L[1]
       i13_assign_end: 
                       jump (i16_assign)
           i14_assign: 
                       expr (d,t,+,1) with ET=L[8]
                       nop
       i14_assign_end: 
                       nop
           i15_assign: 
                       move (stat_req,1) with ET=L[1]
       i15_assign_end: 
                       nop
       i11_branch_end: 
                       nop
           i16_assign: 
                       move (q_u1_tx,d)
       i16_assign_end: 
                       jump (i2_assign)
          i1_loop_end: 
end
