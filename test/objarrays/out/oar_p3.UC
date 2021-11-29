modules:
begin
  module Oar: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module Ioport: IMPORT
  module Ram: IMPORT
end

import:
begin
  block var_array_pkt_pool: cells[16] of L[8]
  array pkt_pool_addr: variable[8] of L[8] in var_array_pkt_pool
  array iop: object[2] of Ioport.ioport
  array ma: object[2] of Mutex.mutex
  array xa: register[2] of I[8]
  array pkt_pool_data: variable[8] of L[8] in var_array_pkt_pool
end

data:
begin
  register d: L[8]
  register p_data: L[8]
  structure p: pkt
  register TEMPS_0: I[8]
  register p_addr: L[8]
  register LOOP_i_0: I[3]
  register LOOP_i_1: I[3]
  register LOOP_i_2: I[4]
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
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
end

code:
begin
          i1_for_loop: 
                       move (LOOP_i_0,0) with ET=I[3]
     i1_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],1,>=,LOOP_i_0) with ET=I[3]
                       falsejump ($immed.[1],i6_for_loop)
               i2_fun: 
                       fun ma.[LOOP_i_0].lock()
           i2_fun_end: 
                       nop
            i3_assign: 
                       move (TEMPS_0,xa.[LOOP_i_0:CT=L[1]]) with ET=I[8]
        i3_assign_end: 
                       nop
            i4_assign: 
                       expr (xa.[LOOP_i_0:CT=L[1]],TEMPS_0,+,1) with ET=I[8]
                       nop
        i4_assign_end: 
                       nop
               i5_fun: 
                       fun ma.[LOOP_i_0].unlock()
           i5_fun_end: 
                       nop
     i1_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_0,LOOP_i_0,+,1) with ET=I[3]
                       nop
                       jump (i1_for_loop_cond)
      i1_for_loop_end: 
                       nop
          i6_for_loop: 
                       move (LOOP_i_1,0) with ET=I[3]
     i6_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],1,>=,LOOP_i_1) with ET=I[3]
                       falsejump ($immed.[1],i10_for_loop)
               i7_fun: 
                       fun iop.[LOOP_i_1].read(d)
           i7_fun_end: 
                       nop
            i8_assign: 
                       expr (d,d,-,1) with ET=L[8]
                       nop
        i8_assign_end: 
                       nop
               i9_fun: 
                       fun iop.[LOOP_i_1].write(d)
           i9_fun_end: 
                       nop
     i6_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_1,LOOP_i_1,+,1) with ET=I[3]
                       nop
                       jump (i6_for_loop_cond)
      i6_for_loop_end: 
                       nop
         i10_for_loop: 
                       move (LOOP_i_2,0) with ET=I[4]
    i10_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],5,>=,LOOP_i_2) with ET=I[4]
                       falsejump ($immed.[1],%END)
           i11_assign: 
                       move (p_data,pkt_pool_data.[LOOP_i_2]) with ET=L[8]
       i11_assign_end: 
                       nop
           i12_assign: 
                       move (p_addr,pkt_pool_addr.[LOOP_i_2]) with ET=L[8]
       i12_assign_end: 
                       nop
           i13_assign: 
                       expr (TEMPS_0,LOOP_i_2,+,1) with ET=I[8]
                       nop
       i13_assign_end: 
                       nop
           i14_assign: 
                       move (pkt_pool_data.[TEMPS_0],p_data) with ET=L[8]
       i14_assign_end: 
                       nop
           i15_assign: 
                       expr (TEMPS_0,LOOP_i_2,+,1) with ET=I[8]
                       nop
       i15_assign_end: 
                       nop
           i16_assign: 
                       move (pkt_pool_addr.[TEMPS_0],p_addr) with ET=L[8]
       i16_assign_end: 
                       nop
    i10_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_2,LOOP_i_2,+,1) with ET=I[4]
                       nop
                       jump (i10_for_loop_cond)
     i10_for_loop_end: 
end
