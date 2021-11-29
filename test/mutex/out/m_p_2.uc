-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (C) 2006-2008 ** BSSLAB, Dr. Stefan Bosse
--         Version 2.1 A16 **  2049142 total genes
--         Compiled on Thu Nov 27 11:07:26 CET 2008
--         Compiled by sbosse
--         Compiled on SunOS sun203 5.10 Generic_118833-33 sun4u sparc SUNW,A70

import:
begin
  reg d: I8
  object m: Mutex.mutex
  reg x: I8
  reg y: I8
  reg z: I8
end

data:
begin
  reg LOOP_i_2: I5
end

code:
begin
        i1_for_loop: 
                     move (LOOP_i_2,1)
   i1_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_2)
                     falsejump ($immed.[1],%END)
             i2_fun: 
                     fun (m.lock,())
         i2_fun_end: 
                     nop
          i3_assign: 
                     move (d,3)
      i3_assign_end: 
                     nop
          i4_assign: 
                     expr (x,3,+,x)
                     nop
      i4_assign_end: 
                     nop
          i5_assign: 
                     expr (y,3,+,y)
                     nop
      i5_assign_end: 
                     nop
          i6_assign: 
                     expr (z,3,+,z)
                     nop
      i6_assign_end: 
                     nop
             i7_fun: 
                     fun (m.unlock,())
         i7_fun_end: 
                     nop
   i1_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_2,LOOP_i_2,+,1)
                     nop
                     jump (i1_for_loop_cond)
    i1_for_loop_end: 
end
