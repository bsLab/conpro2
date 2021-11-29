modules:
begin
  module Com: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Link: IMPORT
  module Random: IMPORT
end

import:
begin
  register PRO_send_EXCEPTION: I[8]
  object rnd: Random.random
  object ln: Link.link
end

data:
begin
  register d: L[16]
  register r8: L[8]
  register err: B
  register LOOP_i_0: I[8]
end

types:
begin
  type Random.random: {
      method init ()
      method read (L[8]:LHS)
      method seed (N[0]:RHS)
    }
  type Link.link: {
      method init ()
      method start ()
      method stop ()
      method write (L[16]:RHS,B:LHS)
      method read (L[16]:LHS,B:LHS)
      method interface (L[32]:RHS,L[1]:LHS,L[32]:LHS,L[1]:RHS)
    }
end

code:
begin
               i1_try: 
                       nop
          i5_for_loop: 
                       move (LOOP_i_0,1) with ET=I[8]
     i5_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],64,>=,LOOP_i_0) with ET=I[8]
                       falsejump ($immed.[1],%END)
               i6_fun: 
                       fun rnd.read(r8)
           i6_fun_end: 
                       nop
            i7_assign: 
                       move (d[0 to 7],r8) with ET=L[8]
        i7_assign_end: 
                       nop
            i8_assign: 
                       move (d[8 to 15],LOOP_i_0:CT=L[0]) with ET=L[8]
        i8_assign_end: 
                       nop
               i9_fun: 
                       fun ln.write(d,err)
           i9_fun_end: 
                       nop
           i10_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,true) with ET=B
                       falsejump ($immed.[1],i5_for_loop_incr)
            i11_raise: 
                       bind (2)
                       move (PRO_send_EXCEPTION,1) with ET=I[8]
                       jump (i2_select)
        i11_raise_end: 
                       nop
       i10_branch_end: 
                       nop
     i5_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[8]
                       nop
                       jump (i5_for_loop_cond)
      i5_for_loop_end: 
                       jump (%END)
            i2_select: 
                       bind (4)
            i2_case_1: 
                       expr ($immed.[1],PRO_send_EXCEPTION,=,1) with ET=I[8]
                       falsejump ($immed.[1],%END)
                       nop
               i3_fun: 
                       fun ln.stop()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (PRO_send_EXCEPTION,0) with ET=I[8]
        i4_assign_end: 
                       nop
        i2_select_end: 
                       jump (%END)
           i1_try_end: 
end
