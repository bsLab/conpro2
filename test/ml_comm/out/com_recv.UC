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
  register PRO_recv_EXCEPTION: I[8]
  signal DEV_data: L[16]
  object ln: Link.link
  signal DEV_data_en: L[1]
end

data:
begin
  register d: L[16]
  register err: B
  register LOOP_i_1: I[8]
end

types:
begin
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
                       move (LOOP_i_1,1) with ET=I[8]
     i5_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],64,>=,LOOP_i_1) with ET=I[8]
                       falsejump ($immed.[1],%END)
               i6_fun: 
                       fun ln.read(d,err)
           i6_fun_end: 
                       nop
            i7_branch: 
                       bind (2)
                       expr ($immed.[1],err,=,true) with ET=B
                       falsejump ($immed.[1],i10_bind_to_11)
             i8_raise: 
                       bind (2)
                       move (PRO_recv_EXCEPTION,2) with ET=I[8]
                       jump (i2_select)
         i8_raise_end: 
                       nop
        i7_branch_end: 
                       nop
       i10_bind_to_11: 
                       bind (2)
                       move (DEV_data,d) with ET=L[16]
                       move (DEV_data_en,1) with ET=L[1]
   i10_bind_to_11_end: 
                       nop
     i5_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[8]
                       nop
                       jump (i5_for_loop_cond)
      i5_for_loop_end: 
                       jump (%END)
            i2_select: 
                       bind (4)
            i2_case_1: 
                       expr ($immed.[1],PRO_recv_EXCEPTION,=,2) with ET=I[8]
                       falsejump ($immed.[1],%END)
                       nop
               i3_fun: 
                       fun ln.stop()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (PRO_recv_EXCEPTION,0) with ET=I[8]
        i4_assign_end: 
                       nop
        i2_select_end: 
                       jump (%END)
           i1_try_end: 
end
