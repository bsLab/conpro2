modules:
begin
  module P: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Ioport: IMPORT
  module System: IMPORT
end

import:
begin
  object pt: Ioport.ioport
end

temp:
begin
  register TEMP_0: L[26]
end

data:
begin
  register d: L[8]
  register TEMP_0: L[26]
  register LOOP_i_0: I[5]
end

types:
begin
  type Ioport.ioport: {
      method init ()
      method read (L[8]:LHS)
      method write (L[8]:RHS)
      method dir (L[8]:RHS)
      method interface (L[8]:LRHS)
    }
end

code:
begin
               i1_fun: 
                       fun pt.dir(11111111)
           i1_fun_end: 
                       jump (i3_for_loop)
              i2_loop: 
                       nop
          i3_for_loop: 
                       move (LOOP_i_0,0) with ET=I[5]
     i3_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],7,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],i3_for_loop)
            i4_assign: 
                       move (d,0) with ET=L[8]
        i4_assign_end: 
                       nop
            i5_assign: 
                       move (d,1) with ET=L[1]
        i5_assign_end: 
                       nop
               i6_fun: 
                       fun pt.write(d)
           i6_fun_end: 
                       nop
           i7_waitfor: 
                       move ($tmp.[waitfor_count],18518516) with ET=L[26]
      i7_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1) with ET=L[26]
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0) with ET=L[26]
                       falsejump ($immed.[1],i7_waitfor_loop)
  i7_waitfor_loop_end: 
                       jump (i3_for_loop_incr)
       i7_waitfor_end: 
                       nop
     i3_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i3_for_loop_cond)
      i3_for_loop_end: 
                       jump (i3_for_loop)
          i2_loop_end: 
end
