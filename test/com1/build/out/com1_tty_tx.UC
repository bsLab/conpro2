modules:
begin
  module Com1: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module Uart: IMPORT
  module Timer: IMPORT
  module System: IMPORT
end

import:
begin
  queue tty_stdout: C8
  object tty_link: Uart.uart
  register diag: L[3]
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
                       jump (i3_assign)
              i2_loop: 
                       nop
            i3_assign: 
                       move (d,tty_stdout:CT=L[0]) with ET=L[8]
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (diag[2],1) with ET=L[1]
        i4_assign_end: 
                       nop
               i5_fun: 
                       fun tty_link.write(d,err)
           i5_fun_end: 
                       nop
            i6_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,false) with ET=B
                       falsejump ($immed.[1],i8_assign)
            i7_assign: 
                       move (err,true) with ET=B
        i7_assign_end: 
                       nop
        i6_branch_end: 
                       nop
            i8_assign: 
                       move (diag[1],1) with ET=L[1]
        i8_assign_end: 
                       jump (i3_assign)
          i2_loop_end: 
end
