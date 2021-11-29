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
  register stat_err: L[1]
  queue q_u1_rx: L[8]
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
              i1_loop: 
                       nop
               i2_fun: 
                       fun u1.read(t,err)
           i2_fun_end: 
                       nop
            i3_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,true)
                       falsejump ($immed.[1],i5_branch)
            i4_assign: 
                       move (stat_err,1) with ET=L[1]
        i4_assign_end: 
                       nop
        i3_branch_end: 
                       nop
            i5_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,false)
                       falsejump ($immed.[1],i2_fun)
            i6_assign: 
                       move (q_u1_rx,t)
        i6_assign_end: 
                       nop
        i5_branch_end: 
                       jump (i2_fun)
          i1_loop_end: 
end
