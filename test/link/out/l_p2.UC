modules:
begin
  module L: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Link: IMPORT
  module System: IMPORT
end

import:
begin
  register y: I[10]
  object ln: Link.link
  register PRO_p2_EXCEPTION: I[8]
  register ya: I[8]
end

data:
begin
  register d: L[10]
  register err: B
  register LOOP_i_1: I[5]
end

types:
begin
  type Link.link: {
      method init ()
      method start ()
      method stop ()
      method write (L[10]:RHS,B:LHS)
      method read (L[10]:LHS,B:LHS)
      method interface (L[20]:RHS,L[1]:LHS,L[20]:LHS,L[1]:RHS)
    }
end

code:
begin
               i1_try: 
                       nop
          i5_for_loop: 
                       move (LOOP_i_1,1) with ET=I[5]
     i5_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_1) with ET=I[5]
                       falsejump ($immed.[1],%END)
            i6_assign: 
                       move (d,LOOP_i_1:CT=L[0]) with ET=L[10]
        i6_assign_end: 
                       nop
            i7_assign: 
                       move (y,LOOP_i_1) with ET=I[10]
        i7_assign_end: 
                       nop
            i8_assign: 
                       move (ya,'w') with ET=I[8]
        i8_assign_end: 
                       nop
               i9_fun: 
                       fun ln.write(d,err)
           i9_fun_end: 
                       nop
           i10_assign: 
                       move (ya,'.') with ET=I[8]
       i10_assign_end: 
                       nop
           i11_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,true) with ET=B
                       falsejump ($immed.[1],i5_for_loop_incr)
            i12_raise: 
                       bind (2)
                       move (PRO_p2_EXCEPTION,1) with ET=I[8]
                       jump (i2_select)
        i12_raise_end: 
                       nop
       i11_branch_end: 
                       nop
     i5_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[5]
                       nop
                       jump (i5_for_loop_cond)
      i5_for_loop_end: 
                       jump (%END)
            i2_select: 
                       bind (4)
            i2_case_1: 
                       expr ($immed.[1],PRO_p2_EXCEPTION,=,1) with ET=I[8]
                       falsejump ($immed.[1],%END)
                       nop
               i3_fun: 
                       fun ln.stop()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (PRO_p2_EXCEPTION,0) with ET=I[8]
        i4_assign_end: 
                       nop
        i2_select_end: 
                       jump (%END)
           i1_try_end: 
end
