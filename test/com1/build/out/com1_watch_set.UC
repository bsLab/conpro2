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
  object watch_timer: Timer.timer
  register diag: L[3]
  register stat_ev: L[4]
end

types:
begin
  type Timer.timer: {
      method init ()
      method time (N[0]:RHS)
      method await ()
      method wakeup ()
      method start ()
      method stop ()
      method mode (L[1]:RHS)
      method sig_action (L[1]:LHS,N[0]:RHS,N[0]:RHS)
    }
end

code:
begin
            i1_assign: 
                       move (diag,0) with ET=L[3]
        i1_assign_end: 
                       nop
               i2_fun: 
                       fun watch_timer.init()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun watch_timer.start()
           i3_fun_end: 
                       jump (i5_branch)
              i4_loop: 
                       nop
            i5_branch: 
                       bind (2)
                       expr ($immed.[1],diag[0],=,1) with ET=L[1]
                       falsejump ($immed.[1],i8_branch)
            i6_assign: 
                       move (stat_ev[0],1) with ET=L[1]
        i6_assign_end: 
                       nop
            i7_assign: 
                       move (diag[0],0) with ET=L[1]
        i7_assign_end: 
                       nop
        i5_branch_end: 
                       nop
            i8_branch: 
                       bind (2)
                       expr ($immed.[1],diag[1],=,1) with ET=L[1]
                       falsejump ($immed.[1],i11_branch)
            i9_assign: 
                       move (stat_ev[1],1) with ET=L[1]
        i9_assign_end: 
                       nop
           i10_assign: 
                       move (diag[1],0) with ET=L[1]
       i10_assign_end: 
                       nop
        i8_branch_end: 
                       nop
           i11_branch: 
                       bind (2)
                       expr ($immed.[1],diag[2],=,1) with ET=L[1]
                       falsejump ($immed.[1],i5_branch)
           i12_assign: 
                       move (stat_ev[2],1) with ET=L[1]
       i12_assign_end: 
                       nop
           i13_assign: 
                       move (diag[2],0) with ET=L[1]
       i13_assign_end: 
                       nop
       i11_branch_end: 
                       jump (i5_branch)
          i4_loop_end: 
end
