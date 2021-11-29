-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D100 Genetic size: 2261933
--         Compile date: Thu Aug 13 10:06:37 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  reg ARG_FUN_parity2_x: L64
  reg RET_FUN_parity2_p: L1
end

data:
begin
  reg pl: L1
  reg LOOP_i_1: I8
  reg xl: L64
end

code:
begin
      BLOCKBOUND5_1: 
                     bind (10)
          i1_assign: 
                     move (xl,ARG_FUN_parity2_x)
      i1_assign_end: 
                     nop
          i2_assign: 
                     move (pl,0)
      i2_assign_end: 
                     nop
        i3_for_loop: 
                     move (LOOP_i_1,0)
   i3_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],63,>=,LOOP_i_1)
                     falsejump ($immed.[1],BLOCKBOUND1_1)
      BLOCKBOUND3_1: 
                     bind (10)
          i4_assign: 
                     expr (pl,pl,lxor,xl:L1)
                     nop
      i4_assign_end: 
   i3_for_loop_incr: 
                     expr (LOOP_i_1,LOOP_i_1,+,1)
                     nop
                     jump (i3_for_loop_cond)
    i3_for_loop_end: 
                     nop
      BLOCKBOUND1_1: 
                     bind (3)
          i5_assign: 
                     move (RET_FUN_parity2_p,pl)
      i5_assign_end: 
end
