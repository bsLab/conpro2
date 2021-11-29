modules:
begin
  module L2: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Link: IMPORT
  module System: IMPORT
end

import:
begin
  register PRO_p1_EXCEPTION: I[8]
  register x: I[12]
  object ln: Link.link
  register xa: I[8]
end

data:
begin
  register d: L[12]
  register err: B
  register LOOP_i_0: I[5]
end

types:
begin
  type Link.link: {
      method init ()
      method start ()
      method stop ()
      method write (L[12]:RHS,B:LHS)
      method read (L[12]:LHS,B:LHS)
      method interface (L[24]:RHS,L[1]:LHS,L[24]:LHS,L[1]:RHS)
      method select (L[1]:LHS)
    }
end

code:
begin
            i1_assign: 
                       move (d,841) with ET=L[12]
        i1_assign_end: 
                       nop
               i2_try: 
                       nop
          i6_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i6_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],i20_assign)
         i7_bind_to_8: 
                       bind (2)
                       move (xa,'w') with ET=I[8]
                       move (x,d:CT=I[0]) with ET=I[12]
     i7_bind_to_8_end: 
                       nop
               i9_fun: 
                       fun ln.write(d,err)
           i9_fun_end: 
                       nop
           i10_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,true) with ET=B
                       falsejump ($immed.[1],i13_assign)
            i11_raise: 
                       bind (2)
                       move (PRO_p1_EXCEPTION,1) with ET=I[8]
                       jump (i3_select)
        i11_raise_end: 
                       nop
       i10_branch_end: 
                       nop
           i13_assign: 
                       move (xa,'r') with ET=I[8]
       i13_assign_end: 
                       nop
              i14_fun: 
                       fun ln.read(d,err)
          i14_fun_end: 
                       nop
           i15_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,true) with ET=B
                       falsejump ($immed.[1],i18_bind_to_19)
            i16_raise: 
                       bind (2)
                       move (PRO_p1_EXCEPTION,1) with ET=I[8]
                       jump (i3_select)
        i16_raise_end: 
                       nop
       i15_branch_end: 
                       nop
       i18_bind_to_19: 
                       bind (3)
                       move (x,d:CT=I[0]) with ET=I[12]
                       expr (d,d,+,1) with ET=L[12]
                       nop
   i18_bind_to_19_end: 
                       nop
     i6_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i6_for_loop_cond)
      i6_for_loop_end: 
                       jump (i20_assign)
            i3_select: 
                       bind (4)
            i3_case_1: 
                       expr ($immed.[1],PRO_p1_EXCEPTION,=,1) with ET=I[8]
                       falsejump ($immed.[1],i20_assign)
                       nop
               i4_fun: 
                       fun ln.stop()
           i4_fun_end: 
                       nop
            i5_assign: 
                       move (PRO_p1_EXCEPTION,0) with ET=I[8]
        i5_assign_end: 
                       nop
        i3_select_end: 
                       jump (i20_assign)
           i2_try_end: 
                       nop
           i20_assign: 
                       move (xa,'.') with ET=I[8]
       i20_assign_end: 
end
