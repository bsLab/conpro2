modules:
begin
  module T: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module F: IMPORT
  module System: IMPORT
  module Ram: IMPORT
end

import:
begin
  register d: I[10]
  variable v: L[8] in b.[0]
  signal x: I[10]
  signal y: I[10]
  object f1: F.f
  block b: cells[1] of L[8]
end

temp:
begin
  register TEMP_0: L[8]
end

data:
begin
  register TEMP_0: L[8]
  register LOOP_i_0: I[5]
end

types:
begin
  type F.f: {
      method init ()
      method read (L[10]:LHS)
      method time (N[0]:RHS)
      method set (L[10]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun f1.time(2000)
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun f1.time(3000)
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun f1.time(4000)
           i3_fun_end: 
                       nop
            i4_assign: 
                       move ($tmp.[v],v) with ET=L[8]
                       expr (v,$tmp.[v],+,1) with ET=L[8]
                       nop
        i4_assign_end: 
                       nop
          i5_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i5_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],%END)
               i6_fun: 
                       fun f1.read(d:CT=L[0])
           i6_fun_end: 
                       nop
         i7_bind_to_8: 
                       bind (2)
                       move (x,LOOP_i_0) with ET=I[10]
                       move (y,d) with ET=I[10]
     i7_bind_to_8_end: 
                       nop
     i5_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i5_for_loop_cond)
      i5_for_loop_end: 
end
