-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D115 Genetic size: 2534927
--         Compile date: Fri Oct 23 13:13:50 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register PRO_recv_EXCEPTION: I8
  signal DEV_data: L8
  object ln: Uart.uart
  signal DEV_data_en: L1
end

data:
begin
  register d: L8
  register err: B
  register LOOP_i_1: I8
end

code:
begin
             i1_try: 
                     nop
        i5_for_loop: 
                     move (LOOP_i_1,1)
   i5_for_loop_cond: 
                     bind (2)
                     expr ($immed.[1],64,>=,LOOP_i_1)
                     falsejump ($immed.[1],%END)
             i6_fun: 
                     fun (ln.read,d,err)
         i6_fun_end: 
                     nop
          i7_branch: 
                     bind (2)
                     expr ($immed.[1],err,=,$true)
                     falsejump ($immed.[1],i10_bind_to_11)
           i8_raise: 
                     bind (2)
                     move (PRO_recv_EXCEPTION,2)
                     jump (i2_select)
       i8_raise_end: 
                     nop
      i7_branch_end: 
                     nop
     i10_bind_to_11: 
                     bind (2)
                     move (DEV_data,d)
                     move (DEV_data_en,1)
 i10_bind_to_11_end: 
                     nop
   i5_for_loop_incr: 
                     bind (3)
                     expr (LOOP_i_1,LOOP_i_1,+,1)
                     nop
                     jump (i5_for_loop_cond)
    i5_for_loop_end: 
                     jump (%END)
          i2_select: 
                     bind (4)
          i2_case_1: 
                     expr ($immed.[1],PRO_recv_EXCEPTION,=,2)
                     falsejump ($immed.[1],%END)
                     nop
             i3_fun: 
                     fun (ln.stop,())
         i3_fun_end: 
                     nop
          i4_assign: 
                     move (PRO_recv_EXCEPTION,0)
      i4_assign_end: 
                     nop
      i2_select_end: 
                     jump (%END)
         i1_try_end: 
end
