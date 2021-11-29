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
  queue q_u1_tx: L[8]
  object u1: Uart.uart
end

data:
begin
  register t: L[8]
  register err: B
end

types:
begin
  type Uart.uart: {
      method init ()
      method start ()
      method stop ()
      method baud (N[0]:RHS)
      method write (L[8]:RHS,B:LHS)
      method read (L[8]:LHS,B:LHS)
      method interface (L[1]:RHS,L[1]:LHS)
    }
end

code:
begin
            i1_assign: 
                       move (err,false)
        i1_assign_end: 
                       jump (i3_branch)
              i2_loop: 
                       nop
            i3_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,false)
                       falsejump ($immed.[1],i5_fun)
            i4_assign: 
                       move (t,q_u1_tx)
        i4_assign_end: 
                       nop
        i3_branch_end: 
                       nop
               i5_fun: 
                       fun u1.write(t,err)
           i5_fun_end: 
                       jump (i3_branch)
          i2_loop_end: 
end
