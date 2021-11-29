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
  object tty_link: Uart.uart
  queue tty_stdin: C8
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
              i1_loop: 
                       nop
               i2_fun: 
                       fun tty_link.read(d,err)
           i2_fun_end: 
                       nop
            i3_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,false) with ET=B
                       falsejump ($immed.[1],i2_fun)
            i4_assign: 
                       move (tty_stdin,d:CT=L[8]) with ET=C8
        i4_assign_end: 
                       nop
        i3_branch_end: 
                       jump (i2_fun)
          i1_loop_end: 
end
