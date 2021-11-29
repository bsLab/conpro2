modules:
begin
  module B: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Barrier: IMPORT
  module System: IMPORT
end

import:
begin
  array p: object[4] of Process.process
  object b: Barrier.barrier
end

data:
begin
  register LOOP_i_4: I[4]
end

types:
begin
  type Barrier.barrier: {
      method init ()
      method await ()
    }
end

code:
begin
               i1_fun: 
                       fun b.init()
           i1_fun_end: 
                       nop
          i2_for_loop: 
                       move (LOOP_i_4,0) with ET=I[4]
     i2_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],3,>=,LOOP_i_4) with ET=I[4]
                       falsejump ($immed.[1],%END)
               i3_fun: 
                       fun sel[|p_0;...|].start()
           i3_fun_end: 
                       nop
     i2_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_4,LOOP_i_4,+,1) with ET=I[4]
                       nop
                       jump (i2_for_loop_cond)
      i2_for_loop_end: 
end
