-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681845
--         Compile date: Wed Apr 28 18:22:40 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  object com: Uart.uart
  queue tx_q: L8
end

data:
begin
  register d: L8
  register err: B
end

code:
begin
          i1_assign: 
                     move (err,$false)
      i1_assign_end: 
                     nop
      i2_while_loop: 
                     bind (2)
                     expr ($immed.[1],err,=,$false)
                     falsejump ($immed.[1],%END)
          i3_assign: 
                     move (d,tx_q)
      i3_assign_end: 
                     nop
             i4_fun: 
                     fun (com.write,d,err)
         i4_fun_end: 
                     jump (i2_while_loop)
  i2_while_loop_end: 
end
