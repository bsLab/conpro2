-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D149 Genetic size: 2681845
--         Compile date: Wed Apr 28 18:22:40 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  register sys_status: L3
  register sys_status_next: L3
  const STATUS_ERR: 2
  object com_rx: Process.process
  register com_timeout: B
  object com: Uart.uart
  object com_tmo: Process.process
  queue rx_q: L8
  const STATUS_OK: 1
  queue tx_q: L8
  object com_tx: Process.process
  object rep: Event.event
  register rep_d: L8
end

data:
begin
  register d: L8
end

code:
begin
             i1_fun: 
                     fun (com.init,())
         i1_fun_end: 
                     nop
             i2_fun: 
                     fun (com.start,())
         i2_fun_end: 
                     nop
             i3_fun: 
                     fun (com_rx.start,())
         i3_fun_end: 
                     nop
             i4_fun: 
                     fun (com_tx.start,())
         i4_fun_end: 
                     nop
             i5_fun: 
                     fun (rep.init,())
         i5_fun_end: 
                     nop
          i6_assign: 
                     move (sys_status,1)
      i6_assign_end: 
                     jump (i8_assign)
            i7_loop: 
                     nop
          i8_assign: 
                     move (d,rx_q)
      i8_assign_end: 
                     nop
          i9_assign: 
                     move (sys_status_next,1)
      i9_assign_end: 
                     nop
         i10_select: 
                     bind (10)
         i10_case_1: 
                     expr ($immed.[1],d,=,R)
                     falsejump ($immed.[1],i10_case_2)
                     jump (i11_fun)
         i10_case_2: 
                     expr ($immed.[1],d,=,P)
                     falsejump ($immed.[1],i10_case_3)
                     jump (i18_fun)
         i10_case_3: 
                     jump (i25_assign)
            i11_fun: 
                     fun (com_tmo.start,())
        i11_fun_end: 
                     nop
         i12_assign: 
                     move (d,rx_q)
     i12_assign_end: 
                     nop
            i13_fun: 
                     fun (com_tmo.stop,())
        i13_fun_end: 
                     nop
         i14_branch: 
                     bind (2)
                     expr ($immed.[1],com_timeout,=,$false)
                     falsejump ($immed.[1],i17_assign)
         i15_assign: 
                     move (tx_q,P)
     i15_assign_end: 
                     nop
         i16_assign: 
                     move (tx_q,d)
     i16_assign_end: 
                     jump (i8_assign)
         i17_assign: 
                     move (sys_status,2)
     i17_assign_end: 
                     nop
     i14_branch_end: 
                     jump (i8_assign)
            i18_fun: 
                     fun (com_tmo.start,())
        i18_fun_end: 
                     nop
         i19_assign: 
                     move (d,rx_q)
     i19_assign_end: 
                     nop
            i20_fun: 
                     fun (com_tmo.stop,())
        i20_fun_end: 
                     nop
         i21_branch: 
                     bind (2)
                     expr ($immed.[1],com_timeout,=,$false)
                     falsejump ($immed.[1],i24_assign)
         i22_assign: 
                     move (rep_d,d)
     i22_assign_end: 
                     nop
            i23_fun: 
                     fun (rep.wakeup,())
        i23_fun_end: 
                     jump (i8_assign)
         i24_assign: 
                     move (sys_status,2)
     i24_assign_end: 
                     nop
     i21_branch_end: 
                     jump (i8_assign)
         i25_assign: 
                     move (sys_status,2)
     i25_assign_end: 
                     nop
     i10_select_end: 
                     jump (i8_assign)
        i7_loop_end: 
end
