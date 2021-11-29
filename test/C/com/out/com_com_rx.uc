-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681845
--         Compile date: Wed Apr 28 18:22:40 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register sys_status: L3
  object com: Uart.uart
  queue rx_q: L8
  const STATUS_EV: 4
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
             i3_fun: 
                     fun (com.read,d,err)
         i3_fun_end: 
                     nop
          i4_assign: 
                     move (sys_status,4)
      i4_assign_end: 
                     nop
          i5_branch: 
                     bind (2)
                     expr ($immed.[1],err,=,$false)
                     falsejump ($immed.[1],i2_while_loop)
          i6_assign: 
                     move (rx_q,d)
      i6_assign_end: 
                     nop
      i5_branch_end: 
                     jump (i2_while_loop)
  i2_while_loop_end: 
end
