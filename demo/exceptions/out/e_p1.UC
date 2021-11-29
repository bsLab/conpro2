modules:
begin
  module E: PARENT
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
            i5_assign: 
                       move (j,0) with ET=I[8]
        i5_assign_end: 
                       nop
          i6_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i6_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],i11_assign)
            i7_assign: 
                       expr (j,j,+,2) with ET=I[8]
                       nop
        i7_assign_end: 
                       nop
            i8_branch: 
                       bind (2)
                       expr ($immed.[1],j,=,4) with ET=I[8]
                       falsejump ($immed.[1],i6_for_loop_incr)
             i9_raise: 
                       bind (2)
                       move (PRO_p1_EXCEPTION,1) with ET=I[8]
                       jump (i2_select)
         i9_raise_end: 
                       nop
        i8_branch_end: 
                       nop
     i6_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i6_for_loop_cond)
      i6_for_loop_end: 
                       nop
           i11_assign: 
                       move (jg,18) with ET=I[8]
       i11_assign_end: 
                       jump (%END)
            i2_select: 
                       bind (4)
            i2_case_1: 
                       expr ($immed.[1],PRO_p1_EXCEPTION,=,1) with ET=I[8]
                       falsejump ($immed.[1],%END)
                       nop
            i3_assign: 
                       move (jg,19) with ET=I[8]
        i3_assign_end: 
                       nop
            i4_assign: 
                       move (PRO_p1_EXCEPTION,0) with ET=I[8]
        i4_assign_end: 
                       nop
        i2_select_end: 
                       jump (%END)
           i1_try_end: 
end
