modules:
begin
  module R: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Ram: IMPORT
end

import:
begin
  monitor:
  object p2: Process.process
  array ar: variable[10] of I[8] in ram1
  block ram1: cells[10] of L[8]
end

export:
begin
  monitor:
  object p2: Process.process
end

temp:
begin
  register TEMP_0: I[8]
end

data:
begin
  register TEMPS_0: I[5]
  register TEMPS_1: I[5]
  register TEMP_0: I[8]
  register LOOP_i_1: I[5]
end

types:
begin
  type Process.process: {
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
                       falsejump ($immed.[1],%END)
            i2_assign: 
                       expr (TEMPS_0,LOOP_i_1,-,1) with ET=I[5]
                       nop
        i2_assign_end: 
                       nop
            i3_assign: 
                       expr (TEMPS_1,LOOP_i_1,-,1) with ET=I[5]
                       nop
        i3_assign_end: 
                       nop
            i4_assign: 
                       move ($tmp.[ar],ar.[TEMPS_1]) with ET=I[8]
                       expr (ar.[TEMPS_0],$tmp.[ar],-,1) with ET=I[8]
                       nop
        i4_assign_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[5]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
end
