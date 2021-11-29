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
  register d: L[4]
  object t: Timer.timer
end

data:
begin
  register LOOP_i_3: I[4]
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
          i1_for_loop: 
                       move (LOOP_i_3,1) with ET=I[4]
     i1_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],5,>=,LOOP_i_3) with ET=I[4]
                       falsejump ($immed.[1],%END)
               i2_fun: 
                       fun t.await()
           i2_fun_end: 
                       nop
            i3_assign: 
                       move (d[3],1) with ET=L[1]
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (d[3],0) with ET=L[1]
        i4_assign_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_3,LOOP_i_3,+,1) with ET=I[4]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
end
