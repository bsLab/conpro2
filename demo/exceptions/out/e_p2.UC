modules:
begin
  module E: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

import:
begin
  register jg: I[8]
  register PRO_p2_EXCEPTION: I[8]
end

data:
begin
  register j: I[8]
  register LOOP_i_1: I[5]
end

code:
begin
            i1_assign: 
                       move (j,0) with ET=I[8]
        i1_assign_end: 
                       nop
          i2_for_loop: 
                       move (LOOP_i_1,1) with ET=I[5]
     i2_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_1) with ET=I[5]
                       falsejump ($immed.[1],i7_assign)
            i3_assign: 
                       expr (j,j,+,2) with ET=I[8]
                       nop
        i3_assign_end: 
                       nop
            i4_branch: 
                       bind (2)
                       expr ($immed.[1],j,=,8) with ET=I[8]
                       falsejump ($immed.[1],i2_for_loop_incr)
             i5_raise: 
                       bind (2)
                       move (PRO_p2_EXCEPTION,1) with ET=I[8]
                       jump (%END)
         i5_raise_end: 
                       nop
        i4_branch_end: 
                       nop
     i2_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[5]
                       nop
                       jump (i2_for_loop_cond)
      i2_for_loop_end: 
                       nop
            i7_assign: 
                       move (jg,33) with ET=I[8]
        i7_assign_end: 
end
