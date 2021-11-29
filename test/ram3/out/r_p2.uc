-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D133 Genetic size: 2558806
--         Compile date: Fri Jan  8 10:17:58 CET 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  monitor:
  object p2: Process.process
  array ar: variable[10] of I8 in ram1
  block ram1: cells[10] of L8
end

export:
begin
  monitor:
  object p2: Process.process
end

temp:
begin
  register TEMP_0: I8
end

data:
begin
  register TEMPS_0: I5
  register TEMPS_1: I5
  register TEMP_0: I8
  register LOOP_i_1: I5
end

code:
begin
        i1_for_loop: 
                     move (LOOP_i_1,1)
   i1_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],10,>=,LOOP_i_1)
                     falsejump ($immed.[1],%END)
          i2_assign: 
                     expr (TEMPS_0,LOOP_i_1,-,1)
                     nop
      i2_assign_end: 
                     nop
          i3_assign: 
                     expr (TEMPS_1,LOOP_i_1,-,1)
                     nop
      i3_assign_end: 
                     nop
          i4_assign: 
                     move ($tmp.[ar],ar.[TEMPS_1])
                     expr (ar.[TEMPS_0],$tmp.[ar],-,1)
                     nop
      i4_assign_end: 
                     nop
   i1_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_1,LOOP_i_1,+,1)
                     nop
                     jump (i1_for_loop_cond)
    i1_for_loop_end: 
end
