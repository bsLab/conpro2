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

temp:
begin
  register TEMP_0: I[8]
end

data:
begin
  register TEMPS_0: I[5]
  register TEMP_0: I[8]
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
                       falsejump ($immed.[1],i6_fun)
        BLOCKBOUND1_1: 
                       bind (4)
            i2_assign: 
                       expr (TEMPS_0,LOOP_i_1,-,1) with ET=I[5]
                       nop
        i2_assign_end: 
        BLOCKBOUND1_2: 
                       bind (4)
                       nop
            i3_assign: 
                       move (ar.[TEMPS_0],LOOP_i_1:CT=L[8]) with ET=I[8]
        i3_assign_end: 
        BLOCKBOUND1_3: 
                       bind (5)
                       nop
            i4_assign: 
                       expr (TEMPS_0,LOOP_i_1,-,1) with ET=I[5]
                       nop
        i4_assign_end: 
        BLOCKBOUND1_4: 
                       bind (3)
                       nop
            i5_assign: 
                       move ($tmp.[ar],ar.[TEMPS_0]) with ET=I[8]
        BLOCKBOUND1_5: 
                       bind (2)
                       move (dbg,$tmp.[ar]) with ET=I[8]
        i5_assign_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[5]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
                       nop
               i6_fun: 
                       fun p1.start()
           i6_fun_end: 
end
