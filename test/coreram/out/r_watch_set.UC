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
  object watch_timer: Timer.timer
  register stat_rx: L[1]
  register stat_err: L[1]
  signal dev_rx: L[1]
  register stat_req: L[1]
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
                       move (stat_ev,0) with ET=L[4]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (stat_req,0) with ET=L[1]
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (stat_err,0) with ET=L[1]
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (stat_rx,dev_rx)
        i4_assign_end: 
                       nop
               i5_fun: 
                       fun watch_timer.start()
           i5_fun_end: 
                       jump (i7_branch)
              i6_loop: 
                       nop
            i7_branch: 
                       bind (2)
                       expr ($immed.[1],stat_req,=,1) with ET=L[1]
                       falsejump ($immed.[1],i10_branch)
            i8_assign: 
                       move (stat_ev[0],1) with ET=L[1]
        i8_assign_end: 
                       nop
            i9_assign: 
                       move (stat_req,0) with ET=L[1]
        i9_assign_end: 
                       nop
        i7_branch_end: 
                       nop
           i10_branch: 
                       bind (2)
                       expr ($immed.[1],stat_err,=,1) with ET=L[1]
                       falsejump ($immed.[1],i13_branch)
           i11_assign: 
                       move (stat_ev[1],1) with ET=L[1]
       i11_assign_end: 
                       nop
           i12_assign: 
                       move (stat_err,0) with ET=L[1]
       i12_assign_end: 
                       nop
       i10_branch_end: 
                       nop
           i13_branch: 
                       bind (2)
                       expr ($immed.[1],stat_rx,<>,dev_rx)
                       falsejump ($immed.[1],i7_branch)
           i14_assign: 
                       move (stat_rx,dev_rx)
       i14_assign_end: 
                       nop
           i15_assign: 
                       move (stat_ev[2],1) with ET=L[1]
       i15_assign_end: 
                       nop
       i13_branch_end: 
                       jump (i7_branch)
          i6_loop_end: 
end
