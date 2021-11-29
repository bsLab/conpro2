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
  object com: Uart.uart
  queue tx_q: L[8]
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
            i3_assign: 
                       move (d,tx_q) with ET=L[8]
        i3_assign_end: 
                       nop
               i4_fun: 
                       fun com.write(d,err)
           i4_fun_end: 
                       jump (i2_while_loop)
    i2_while_loop_end: 
end
