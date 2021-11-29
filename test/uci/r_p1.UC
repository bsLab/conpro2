modules:
begin
  module R: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Random: IMPORT
  module System: IMPORT
end

import:
begin
  register d: I[10]
  register e: L[12]
  register f: I[4]
  register g: L[4]
  variable v: I[8] in b.[0]
  signal x: I[10]
  signal y: I[10]
  object rnd1: Random.random
  block b: cells[1] of L[8]
end

temp:
begin
  register TEMP_0: I[8]
end

data:
begin
  register TEMP_0: I[8]
  register LOOP_i_0: I[5]
end

types:
begin
  type Random.random: {
      method init ()
      method read (L[10]:LHS)
      method seed (N[0]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun rnd1.read(e)
           i1_fun_end: 
                       nop
            i2_assign: 
                       expr (e,e,+,1) with ET=L[12]
                       nop
        i2_assign_end: 
                       nop
            i3_assign: 
                       move ($tmp.[v],v)
                       expr (v:CT=L[8],$tmp.[v],+,1) with ET=I[8]
                       nop
        i3_assign_end: 
                       nop
               i4_fun: 
                       fun rnd1.read(f:CT=L[0])
           i4_fun_end: 
                       nop
            i5_assign: 
                       expr (f,f,+,1) with ET=I[4]
                       nop
        i5_assign_end: 
                       nop
               i6_fun: 
                       fun rnd1.read(g)
           i6_fun_end: 
                       nop
            i7_assign: 
                       expr (g,g,+,1) with ET=L[4]
                       nop
        i7_assign_end: 
                       nop
          i8_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i8_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],%END)
               i9_fun: 
                       fun rnd1.read(d:CT=L[0])
           i9_fun_end: 
                       nop
       i10_bind_to_11: 
                       bind (2)
                       move (x,LOOP_i_0) with ET=I[10]
                       move (y,d)
   i10_bind_to_11_end: 
                       nop
     i8_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i8_for_loop_cond)
      i8_for_loop_end: 
end
