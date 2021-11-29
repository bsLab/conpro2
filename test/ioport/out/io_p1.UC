modules:
begin
  module Io: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Ioport: IMPORT
  module Random: IMPORT
end

import:
begin
  object iop: Ioport.ioport
  object rnd: Random.random
end

data:
begin
  register d: L[8]
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
  type Random.random: {
      method init ()
      method read (L[8]:LHS)
      method seed (N[0]:RHS)
    }
end

code:
begin
          i1_for_loop: 
                       move (LOOP_i_0,1) with ET=I[5]
     i1_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],8,>=,LOOP_i_0) with ET=I[5]
                       falsejump ($immed.[1],%END)
               i2_fun: 
                       fun rnd.read(d)
           i2_fun_end: 
                       nop
               i3_fun: 
                       fun iop.write(d)
           i3_fun_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[5]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
end
