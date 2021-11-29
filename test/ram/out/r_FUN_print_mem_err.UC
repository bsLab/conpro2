modules:
begin
  module R: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
  module Ram: IMPORT
end

import:
begin
  object LOCK_FUN_print: Mutex.mutex
  register ARG_FUN_print_mem_err_addr: L[19]
  object print_lock: Mutex.mutex
  object FUN_print: Process.Process
  array line: variable[32] of L[8]var_array_line
  block var_array_line: cells[32] of L[8]
  register ARG_FUN_print_linenum: L[2]
  function print(linenum:L[2])
end

temp:
begin
  register TEMP_0: L[5]
end

data:
begin
  register TEMP_0: L[5]
end

types:
begin
  type Process.Process: {
      method start ()
      method stop ()
      method call ()
    }
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
end

code:
begin
               i1_fun: 
                       fun print_lock.lock()
           i1_fun_end: 
                       nop
               i2_fun: 
                       fun line.copy("MEM ERR AAAAA.")
           i2_fun_end: 
                       nop
            i3_assign: 
                       expr (line.[8],'0',+,ARG_FUN_print_mem_err_addr[16 to 18]) with ET=L[8]
                       nop
        i3_assign_end: 
                       nop
            i4_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_print_mem_err_addr[12 to 15],<,10) with ET=L[4]
                       falsejump ($immed.[1],i6_assign)
            i5_assign: 
                       expr (line.[9],'0',+,ARG_FUN_print_mem_err_addr[12 to 15]) with ET=L[8]
                       nop
        i5_assign_end: 
                       jump (i7_branch)
            i6_assign: 
                       expr (line.[9],55,+,ARG_FUN_print_mem_err_addr[12 to 15]) with ET=L[8]
                       nop
        i6_assign_end: 
                       nop
        i4_branch_end: 
                       nop
            i7_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_print_mem_err_addr[8 to 11],<,10) with ET=L[4]
                       falsejump ($immed.[1],i9_assign)
            i8_assign: 
                       expr (line.[10],'0',+,ARG_FUN_print_mem_err_addr[8 to 11]) with ET=L[8]
                       nop
        i8_assign_end: 
                       jump (i10_branch)
            i9_assign: 
                       expr (line.[10],55,+,ARG_FUN_print_mem_err_addr[8 to 11]) with ET=L[8]
                       nop
        i9_assign_end: 
                       nop
        i7_branch_end: 
                       nop
           i10_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_print_mem_err_addr[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i12_assign)
           i11_assign: 
                       expr (line.[11],'0',+,ARG_FUN_print_mem_err_addr[4 to 7]) with ET=L[8]
                       nop
       i11_assign_end: 
                       jump (i13_branch)
           i12_assign: 
                       expr (line.[11],55,+,ARG_FUN_print_mem_err_addr[4 to 7]) with ET=L[8]
                       nop
       i12_assign_end: 
                       nop
       i10_branch_end: 
                       nop
           i13_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_print_mem_err_addr[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i15_assign)
           i14_assign: 
                       expr (line.[11],'0',+,ARG_FUN_print_mem_err_addr[0 to 3]) with ET=L[8]
                       nop
       i14_assign_end: 
                       jump (i16_fun)
           i15_assign: 
                       expr (line.[11],55,+,ARG_FUN_print_mem_err_addr[0 to 3]) with ET=L[8]
                       nop
       i15_assign_end: 
                       nop
       i13_branch_end: 
                       nop
              i16_fun: 
                       fun LOCK_FUN_print.lock()
          i16_fun_end: 
                       nop
           i17_assign: 
                       move (ARG_FUN_print_linenum,3) with ET=L[2]
       i17_assign_end: 
                       nop
              i18_fun: 
                       fun FUN_print.call()
          i18_fun_end: 
                       nop
              i19_fun: 
                       fun LOCK_FUN_print.unlock()
          i19_fun_end: 
end
