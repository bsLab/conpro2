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
  object lcd1: Lcd.lcd
  queue q_lcd1_tx: L[9]
end

data:
begin
  register t: L[9]
end

types:
begin
  type Lcd.lcd: {
      method init ()
      method cmd (L[8]:RHS)
      method write (L[8]:RHS)
      method interface (L[4]:LRHS,L[1]:LHS,L[1]:LHS,L[1]:LHS,L[1]:LHS)
    }
end

code:
begin
              i1_loop: 
                       nop
            i2_assign: 
                       move (t,q_lcd1_tx)
        i2_assign_end: 
                       nop
            i3_branch: 
                       bind (2)
                       expr ($immed.[1],t[8],=,1) with ET=L[1]
                       falsejump ($immed.[1],i5_fun)
               i4_fun: 
                       fun lcd1.cmd(t[0 to 7])
           i4_fun_end: 
                       jump (i2_assign)
               i5_fun: 
                       fun lcd1.write(t[0 to 7])
           i5_fun_end: 
                       nop
        i3_branch_end: 
                       jump (i2_assign)
          i1_loop_end: 
end
