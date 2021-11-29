modules:
begin
  module Com: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Event: IMPORT
  module Timer: IMPORT
  module Mutex: IMPORT
end

import:
begin
  register sys_status: L[3]
  object com: Uart.uart
  queue rx_q: L[8]
  const STATUS_EV: 4
end

data:
begin
  register d: L[8]
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
      method level (N[0]:RHS,N[0]:RHS)
    }
end

code:
begin
            i1_assign: 
                       move (err,false) with ET=B
        i1_assign_end: 
                       nop
        i2_while_loop: 
                       bind (2)
                       expr ($immed.[1],err,=,false) with ET=B
                       falsejump ($immed.[1],%END)
               i3_fun: 
                       fun com.read(d,err)
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (sys_status,4) with ET=L[3]
        i4_assign_end: 
                       nop
            i5_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,false) with ET=B
                       falsejump ($immed.[1],i2_while_loop)
            i6_assign: 
                       move (rx_q,d) with ET=L[8]
        i6_assign_end: 
                       nop
        i5_branch_end: 
                       jump (i2_while_loop)
    i2_while_loop_end: 
end
