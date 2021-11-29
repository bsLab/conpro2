modules:
begin
  module Exc: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

import:
begin
  register PRO_p1_EXCEPTION: I[8]
  register jg: I[8]
end

data:
begin
  register j: I[8]
  register LOOP_i_0: I[5]
end

code:
begin
               i1_try: 
                       nop
            i6_assign: 
                       move (j,0) with ET=I[8]
        i6_assign_end: 
                       nop
          i7_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i7_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],i12_assign)
            i8_assign: 
                       expr (j,j,+,2) with ET=I[8]
                       nop
        i8_assign_end: 
                       nop
            i9_branch: 
                       bind (2)
                       expr ($immed.[1],j,=,4) with ET=I[8]
                       falsejump ($immed.[1],i7_for_loop_incr)
            i10_raise: 
                       bind (2)
                       move (PRO_p1_EXCEPTION,1) with ET=I[8]
                       jump (i2_select)
        i10_raise_end: 
                       nop
        i9_branch_end: 
                       nop
     i7_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i7_for_loop_cond)
      i7_for_loop_end: 
                       nop
           i12_assign: 
                       move (jg,18) with ET=I[8]
       i12_assign_end: 
                       jump (%END)
            i2_select: 
                       bind (6)
            i2_case_1: 
                       expr ($immed.[1],PRO_p1_EXCEPTION,=,1) with ET=I[8]
                       falsejump ($immed.[1],i2_case_2)
                       jump (i3_assign)
            i2_case_2: 
                       jump (i5_raise)
            i3_assign: 
                       move (jg,19) with ET=I[8]
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (PRO_p1_EXCEPTION,0) with ET=I[8]
        i4_assign_end: 
                       jump (%END)
             i5_raise: 
                       bind (2)
                       jump (%END)
         i5_raise_end: 
                       nop
        i2_select_end: 
                       jump (%END)
           i1_try_end: 
end
