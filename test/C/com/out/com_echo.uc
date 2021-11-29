-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D148 Genetic size: 2671415
--         Compile date: Tue Apr 27 15:45:04 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register act: L1
  object u: Uart.uart
end

data:
begin
  register d: L8
  register err: B
end

code:
begin
             i1_fun: 
                     fun (u.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (u.start,())
         i2_fun_end: 
                     nop
          i3_assign: 
                     move (err,$false)
      i3_assign_end: 
                     nop
          i4_assign: 
                     move (act,0)
      i4_assign_end: 
                     nop
      i5_while_loop: 
                     bind (2)
                     expr ($immed.[1],err,=,$false)
                     falsejump ($immed.[1],%END)
             i6_fun: 
                     fun (u.read,d,err)
         i6_fun_end: 
                     nop
          i7_assign: 
                     move (act,not act)
      i7_assign_end: 
                     nop
             i8_fun: 
                     fun (u.write,d,err)
         i8_fun_end: 
                     jump (i5_while_loop)
  i5_while_loop_end: 
end
