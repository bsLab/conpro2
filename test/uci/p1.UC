import:
begin
  register d: I[10]
  register e: L[12]
  register f: I[4]
  register g: L[4]
  signal x: I[10]
  signal y: I[10]
  object rnd1: Random.random
end

data:
begin
  register LOOP_i_0: I[5]
end

code:
begin
               i1_fun: 
                       fun (rnd1.read,e)
           i1_fun_end: 
                       nop
            i2_assign: 
                       expr (e,e,+,1) with ET=L[12]
                       nop
        i2_assign_end: 
                       nop
               i3_fun: 
                       fun (rnd1.read,f:CT=L[0])
           i3_fun_end: 
                       nop
            i4_assign: 
                       expr (f,f,+,1) with ET=I[4]
                       nop
        i4_assign_end: 
                       nop
               i5_fun: 
                       fun (rnd1.read,g)
           i5_fun_end: 
                       nop
            i6_assign: 
                       expr (g,g,+,1) with ET=L[4]
                       nop
        i6_assign_end: 
                       nop
          i7_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i7_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],10,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],%END)
               i8_fun: 
                       fun (rnd1.read,d:CT=L[0])
           i8_fun_end: 
                       nop
        i9_bind_to_10: 
                       bind (2)
                       move (x,LOOP_i_0) with ET=I[10]
                       move (y,d)
    i9_bind_to_10_end: 
                       nop
     i7_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i7_for_loop_cond)
      i7_for_loop_end: 
end
