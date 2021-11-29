modules:
begin
  module L: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
  module Random: IMPORT
end

import:
begin
  register stat_a: L[1]
  register stat_b: L[1]
  register stat_c: L[1]
  object watch_timer: Timer.timer
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
    }
end

code:
begin
            i1_assign: 
                       move (stat_ev,0) with ET=L[4]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (stat_a,0) with ET=L[1]
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (stat_b,0) with ET=L[1]
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (stat_c,0) with ET=L[1]
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
                       expr ($immed.[1],stat_a,=,1) with ET=L[1]
                       falsejump ($immed.[1],i10_branch)
            i8_assign: 
                       move (stat_ev[0],1) with ET=L[1]
        i8_assign_end: 
                       nop
            i9_assign: 
                       move (stat_a,0) with ET=L[1]
        i9_assign_end: 
                       nop
        i7_branch_end: 
                       nop
           i10_branch: 
                       bind (2)
                       expr ($immed.[1],stat_b,=,1) with ET=L[1]
                       falsejump ($immed.[1],i13_branch)
           i11_assign: 
                       move (stat_ev[1],1) with ET=L[1]
       i11_assign_end: 
                       nop
           i12_assign: 
                       move (stat_b,0) with ET=L[1]
       i12_assign_end: 
                       nop
       i10_branch_end: 
                       nop
           i13_branch: 
                       bind (2)
                       expr ($immed.[1],stat_c,=,1) with ET=L[1]
                       falsejump ($immed.[1],i7_branch)
           i14_assign: 
                       move (stat_ev[2],1) with ET=L[1]
       i14_assign_end: 
                       nop
           i15_assign: 
                       move (stat_c,0) with ET=L[1]
       i15_assign_end: 
                       nop
       i13_branch_end: 
                       jump (i7_branch)
          i6_loop_end: 
end
