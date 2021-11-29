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
  register diag: L[3]
  array line: register[32] of C8
end

data:
begin
  register i: I[5]
end

code:
begin
            i1_assign: 
                       move (i,0) with ET=I[5]
        i1_assign_end: 
                       nop
        i2_while_loop: 
                       bind (2)
                       expr ($immed.[1],line.[i:CT=L[5]],<>,'.') with ET=L[8]
                       falsejump ($immed.[1],i5_assign)
            i2_assign: 
                       move (diag[0],1) with ET=L[1]
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (tty_stdout,line.[i:CT=L[5]]) with ET=C8
        i3_assign_end: 
                       nop
            i4_assign: 
                       expr (i,i,+,1) with ET=I[5]
                       nop
        i4_assign_end: 
                       jump (i2_while_loop)
    i2_while_loop_end: 
                       nop
            i5_assign: 
                       move (tty_stdout,10) with ET=C8
        i5_assign_end: 
end
