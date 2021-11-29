-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D148 Genetic size: 2661823
--         Compile date: Fri Apr 23 17:13:19 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object init: Process.process
  array philosopher: object[5] of Process.process
  register stat: C
  object ev: Event.event
end

data:
begin
  register LOOP_i_1: I4
end

code:
begin
      BLOCKBOUND8_1: 
                     bind (2)
          i1_assign: 
                     move (stat,I)
      i1_assign_end: 
                     nop
             i2_fun: 
                     fun (init.call,())
      BLOCKBOUND6_1: 
                     bind (8)
         i2_fun_end: 
                     nop
          i3_assign: 
                     move (stat,S)
      i3_assign_end: 
                     nop
        i4_for_loop: 
                     move (LOOP_i_1,0)
   i4_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],4,>=,LOOP_i_1)
                     falsejump ($immed.[1],BLOCKBOUND3_1)
             i5_fun: 
                     fun (sel[|philosopher_0;...|].start,())
         i5_fun_end: 
                     nop
   i4_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_1,LOOP_i_1,+,1)
                     nop
                     jump (i4_for_loop_cond)
    i4_for_loop_end: 
                     nop
      BLOCKBOUND3_1: 
                     bind (2)
          i6_assign: 
                     move (stat,W)
      i6_assign_end: 
                     nop
             i7_fun: 
                     fun (ev.wakeup,())
end
