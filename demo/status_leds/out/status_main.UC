modules:
begin
  module Status: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Timer: IMPORT
  module System: IMPORT
  module Random: IMPORT
end

import:
begin
  object watch_reset: Process.Process
  object rand: Random.random
  register diag: L[3]
  object watch_set: Process.Process
end

temp:
begin
  register TEMP_0: L[27]
end

data:
begin
  register d: L[8]
  register TEMP_0: L[27]
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
  type Random.random: {
      method init ()
      method read (L[8]:LHS)
      method seed (N[0]:RHS)
    }
end

code:
begin
               i1_fun: 
                       fun watch_set.start()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun watch_reset.start()
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun rand.init()
           i3_fun_end: 
                       jump (i5_fun)
              i4_loop: 
                       nop
               i5_fun: 
                       fun rand.read(d)
           i5_fun_end: 
                       nop
            i6_select: 
                       bind (14)
            i6_case_1: 
                       expr ($immed.[1],d,>,0) with ET=L[8]
                       falsejump ($immed.[1],i6_case_2)
                       expr ($immed.[1],d,<,16) with ET=L[8]
                       falsejump ($immed.[1],i6_case_2)
                       jump (i7_assign)
            i6_case_2: 
                       expr ($immed.[1],d,>,17) with ET=L[8]
                       falsejump ($immed.[1],i6_case_3)
                       expr ($immed.[1],d,<,45) with ET=L[8]
                       falsejump ($immed.[1],i6_case_3)
                       jump (i8_assign)
            i6_case_3: 
                       jump (i9_assign)
            i7_assign: 
                       move (diag[0],1) with ET=L[1]
        i7_assign_end: 
                       jump (i10_waitfor)
            i8_assign: 
                       move (diag[1],1) with ET=L[1]
        i8_assign_end: 
                       jump (i10_waitfor)
            i9_assign: 
                       move (diag[2],1) with ET=L[1]
        i9_assign_end: 
                       nop
        i6_select_end: 
                       nop
          i10_waitfor: 
                       move ($tmp.[waitfor_count],55555553) with ET=L[27]
     i10_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1) with ET=L[27]
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0) with ET=L[27]
                       falsejump ($immed.[1],i10_waitfor_loop)
  i10_waitfor_loop_end: 
                       jump (i5_fun)
      i10_waitfor_end: 
                       jump (i5_fun)
          i4_loop_end: 
end
