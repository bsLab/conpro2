-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D90 Genetic size: 2230944
--         Compile date: Fri Jul 17 13:28:54 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  reg jg: I8
  reg PRO_p2_EXCEPTION: I8
  object p1: Process.Process
  object p2: Process.Process
  reg PRO_main_EXCEPTION: I8
end

code:
begin
          i1_assign: 
                     move (jg,0)
      i1_assign_end: 
                     nop
             i2_fun: 
                     fun (p1.start,())
         i2_fun_end: 
                     nop
             i3_try: 
                     nop
             i9_fun: 
                     fun (p2.call,())
                     move (PRO_main_EXCEPTION,PRO_p2_EXCEPTION)
                     bind (2)
                     expr ($immed.[1],PRO_main_EXCEPTION,=,0)
                     falsejump ($immed.[1],i4_select)
         i9_fun_end: 
                     jump (%END)
          i4_select: 
                     bind (6)
          i4_case_1: 
                     expr ($immed.[1],PRO_main_EXCEPTION,=,1)
                     falsejump ($immed.[1],i4_case_2)
                     jump (i5_assign)
          i4_case_2: 
                     jump (i7_assign)
          i5_assign: 
                     move (jg,1)
      i5_assign_end: 
                     nop
          i6_assign: 
                     move (PRO_main_EXCEPTION,0)
      i6_assign_end: 
                     jump (%END)
          i7_assign: 
                     move (jg,2)
      i7_assign_end: 
                     nop
          i8_assign: 
                     move (PRO_main_EXCEPTION,0)
      i8_assign_end: 
                     nop
      i4_select_end: 
                     jump (%END)
         i3_try_end: 
end
