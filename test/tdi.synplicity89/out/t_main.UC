modules:
begin
  module T: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Timer: IMPORT
  module System: IMPORT
end

import:
begin
  array p: object[4] of Process.process
  object t: Timer.timer
end

data:
begin
  register LOOP_i_4: I[4]
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
               i1_fun: 
                       fun t.init()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun t.time()
           i2_fun_end: 
                       nop
          i3_for_loop: 
                       move (LOOP_i_4,0) with ET=I[4]
     i3_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],3,>=,LOOP_i_4) with ET=I[4]
                       falsejump ($immed.[1],i5_fun)
               i4_fun: 
                       fun sel[|p_0;...|].start()
           i4_fun_end: 
                       nop
     i3_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_4,LOOP_i_4,+,1) with ET=I[4]
                       nop
                       jump (i3_for_loop_cond)
      i3_for_loop_end: 
                       nop
               i5_fun: 
                       fun t.mode(0)
           i5_fun_end: 
                       nop
               i6_fun: 
                       fun t.start()
           i6_fun_end: 
end
