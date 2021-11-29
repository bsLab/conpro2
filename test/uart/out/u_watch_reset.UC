modules:
begin
  module U: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
end

import:
begin
  object watch_timer: Timer.timer
  register stat_leds: L[4]
  register stat_ev: L[4]
end

data:
begin
  register LOOP_i_0: I[3]
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
                       move (stat_leds,)
        i1_assign_end: 
                       jump (i3_for_loop)
              i2_loop: 
                       nop
          i3_for_loop: 
                       move (LOOP_i_0,1) with ET=I[3]
     i3_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],2,>=,LOOP_i_0) with ET=I[3]
                       falsejump ($immed.[1],i3_for_loop)
            i4_select: 
                       bind (8)
            i4_case_1: 
                       expr ($immed.[1],LOOP_i_0,=,1) with ET=I[3]
                       falsejump ($immed.[1],i4_case_2)
                       jump (i5_assign)
            i4_case_2: 
                       expr ($immed.[1],LOOP_i_0,=,2) with ET=I[3]
                       falsejump ($immed.[1],i9_fun)
                       jump (i8_assign)
            i5_assign: 
                       move (stat_leds[0 to 2],stat_ev) with ET=L[3]
        i5_assign_end: 
                       nop
            i6_assign: 
                       move (stat_leds[3],1) with ET=L[1]
        i6_assign_end: 
                       nop
            i7_assign: 
                       move (stat_ev,0) with ET=L[4]
        i7_assign_end: 
                       jump (i9_fun)
            i8_assign: 
                       move (stat_leds,)
        i8_assign_end: 
                       nop
        i4_select_end: 
                       nop
               i9_fun: 
                       fun watch_timer.await()
           i9_fun_end: 
                       nop
     i3_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[3]
                       nop
                       jump (i3_for_loop_cond)
      i3_for_loop_end: 
                       jump (i3_for_loop)
          i2_loop_end: 
end
