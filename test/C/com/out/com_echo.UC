modules:
begin
  module Com: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
end

import:
begin
  register act: L[1]
  object u: Uart.uart
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
               i1_fun: 
                       fun u.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun u.start()
           i2_fun_end: 
                       nop
            i3_assign: 
                       move (err,false) with ET=B
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (act,0) with ET=L[1]
        i4_assign_end: 
                       nop
        i5_while_loop: 
                       bind (2)
                       expr ($immed.[1],err,=,false) with ET=B
                       falsejump ($immed.[1],%END)
               i6_fun: 
                       fun u.read(d,err)
           i6_fun_end: 
                       nop
            i7_assign: 
                       move (act,not act) with ET=L[1]
        i7_assign_end: 
                       nop
               i8_fun: 
                       fun u.write(d,err)
           i8_fun_end: 
                       jump (i5_while_loop)
    i5_while_loop_end: 
end
