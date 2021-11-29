modules:
begin
  module U: PARENT
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
  register line_pos: L[5]
  object print_lock: Mutex.mutex
  queue q_lcd1_tx: L[9]
  register ARG_FUN_print_line_v: L[8]
end

types:
begin
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
end

code:
begin
               i1_fun: 
                       fun print_lock.lock()
           i1_fun_end: 
                       nop
            i2_assign: 
                       expr (q_lcd1_tx,416,+,line_pos) with ET=L[9]
                       nop
        i2_assign_end: 
                       nop
            i3_assign: 
                       expr (line_pos,line_pos,+,1) with ET=L[5]
                       nop
        i3_assign_end: 
                       nop
            i4_branch: 
                       bind (2)
                       expr ($immed.[1],line_pos,=,16) with ET=L[5]
                       falsejump ($immed.[1],i6_assign)
            i5_assign: 
                       move (line_pos,0) with ET=L[5]
        i5_assign_end: 
                       nop
        i4_branch_end: 
                       nop
            i6_assign: 
                       move (q_lcd1_tx,ARG_FUN_print_line_v) with ET=L[9]
        i6_assign_end: 
                       nop
               i7_fun: 
                       fun print_lock.unlock()
           i7_fun_end: 
end
