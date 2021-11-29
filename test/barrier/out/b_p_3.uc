-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 A16 **  2049142 total genes
--         Compiled on Thu Nov 27 11:07:26 CET 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

import:
begin
  array d: reg[4] of I8 :: {
           d_0,d_1,d_2,d_3}
  object b: Barrier.barrier
end

data:
begin
  reg LOOP_i_3: I4
end

code:
begin
        i1_for_loop: 
                     move (LOOP_i_3,1)
   i1_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],5,>=,LOOP_i_3)
                     falsejump ($immed.[1],%END)
             i2_fun: 
                     fun (b.await,())
         i2_fun_end: 
                     nop
          i3_assign: 
                     move (d_3,4)
      i3_assign_end: 
                     nop
          i4_assign: 
                     move (d_3,0)
      i4_assign_end: 
                     nop
   i1_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_3,LOOP_i_3,+,1)
                     nop
                     jump (i1_for_loop_cond)
    i1_for_loop_end: 
end
