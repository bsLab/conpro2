modules:
begin
  module L: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Link2: IMPORT
  module System: IMPORT
end

import:
begin
  register PRO_p1_EXCEPTION: I[8]
  register x: I[10]
  object ln: Link2.link2
  register xa: I[8]
end

data:
begin
  register d: L[10]
  register err: B
  register LOOP_i_0: I[5]
end

types:
begin
  type Link2.link2: {
      method init ()
      method start ()
      method stop ()
      method write (L[10]:RHS,B:LHS)
      method read (L[10]:LHS,B:LHS)
      method interface (L[2]:RHS,L[1]:LHS,L[2]:LHS,L[1]:RHS)
      method monitor (L[1]:LHS)
    }
end

code:
begin
               i1_try: 
                       nop
          i5_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i5_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],%END)
            i6_assign: 
                       move (xa,'r') with ET=I[8]
        i6_assign_end: 
                       nop
               i7_fun: 
                       fun ln.read(d,err)
           i7_fun_end: 
                       nop
            i8_assign: 
                       move (xa,'.') with ET=I[8]
        i8_assign_end: 
                       nop
            i9_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,true) with ET=B
                       falsejump ($immed.[1],i12_assign)
            i10_raise: 
                       bind (2)
                       move (PRO_p1_EXCEPTION,1) with ET=I[8]
                       jump (i2_select)
        i10_raise_end: 
                       nop
        i9_branch_end: 
                       nop
           i12_assign: 
                       move (x,d:CT=I[0]) with ET=I[10]
       i12_assign_end: 
                       nop
     i5_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i5_for_loop_cond)
      i5_for_loop_end: 
                       jump (%END)
            i2_select: 
                       bind (4)
            i2_case_1: 
                       expr ($immed.[1],PRO_p1_EXCEPTION,=,1) with ET=I[8]
                       falsejump ($immed.[1],%END)
                       nop
               i3_fun: 
                       fun ln.stop()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (PRO_p1_EXCEPTION,0) with ET=I[8]
        i4_assign_end: 
                       nop
        i2_select_end: 
                       jump (%END)
           i1_try_end: 
end
