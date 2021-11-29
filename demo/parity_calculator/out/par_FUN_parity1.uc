-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D100 Genetic size: 2261933
--         Compile date: Thu Aug 13 10:06:37 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  reg ARG_FUN_parity1_x: L64
  reg RET_FUN_parity1_p: L1
end

data:
begin
  reg pl: L1
  reg LOOP_i_0: I8
  reg xl: L64
end

code:
begin
          i1_assign: 
                     move (xl,ARG_FUN_parity1_x)
      i1_assign_end: 
                     nop
          i2_assign: 
                     move (pl,0)
      i2_assign_end: 
                     nop
        i3_for_loop: 
                     move (LOOP_i_0,0)
   i3_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],63,>=,LOOP_i_0)
                     falsejump ($immed.[1],i5_assign)
          i4_assign: 
                     expr (pl,pl,lxor,xl:L1)
                     nop
      i4_assign_end: 
                     nop
   i3_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_0,LOOP_i_0,+,1)
                     nop
                     jump (i3_for_loop_cond)
    i3_for_loop_end: 
                     nop
          i5_assign: 
                     move (RET_FUN_parity1_p,pl)
      i5_assign_end: 
end
