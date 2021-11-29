modules:
begin
  module R: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module System: IMPORT
  module Ram: IMPORT
end

import:
begin
  array ar: variable[10] of I[8] in ram1
  block ram1: cells[10] of L[8]
  register dbg: I[8]
  object p1: Process.Process
end

data:
begin
  register TEMPS_0: I[5]
  register LOOP_i_1: I[5]
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
end

code:
begin
          i1_for_loop: 
                       move (LOOP_i_1,1) with ET=I[5]
     i1_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_1) with ET=I[5]
                       falsejump ($immed.[1],i5_fun)
        BLOCKBOUND1_1: 
                       bind (8)
            i2_assign: 
                       move (dbg,LOOP_i_1) with ET=I[8]
        i2_assign_end: 
                       nop
            i3_assign: 
                       expr (TEMPS_0,LOOP_i_1,-,1) with ET=I[5]
                       nop
        i3_assign_end: 
        BLOCKBOUND1_2: 
                       bind (4)
                       nop
            i4_assign: 
                       move (ar.[TEMPS_0],LOOP_i_1:CT=L[8]) with ET=I[8]
        i4_assign_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[5]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
                       nop
               i5_fun: 
                       fun p1.start()
           i5_fun_end: 
end
