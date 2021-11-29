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
  register ARG_FUN_print_val_v: L[8]
  object LOCK_FUN_print: Mutex.mutex
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
                       fun line.copy("VAL X YY.")
           i2_fun_end: 
                       nop
            i3_assign: 
                       move (line.[4],ARG_FUN_print_val_v)
        i3_assign_end: 
                       nop
            i4_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_print_val_v[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i6_assign)
            i5_assign: 
                       expr (line.[6],'0',+,ARG_FUN_print_val_v[4 to 7]) with ET=L[8]
                       nop
        i5_assign_end: 
                       jump (i7_branch)
            i6_assign: 
                       expr (line.[6],55,+,ARG_FUN_print_val_v[4 to 7]) with ET=L[8]
                       nop
        i6_assign_end: 
                       nop
        i4_branch_end: 
                       nop
            i7_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_print_val_v[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i9_assign)
            i8_assign: 
                       expr (line.[7],'0',+,ARG_FUN_print_val_v[0 to 3]) with ET=L[8]
                       nop
        i8_assign_end: 
                       jump (i10_fun)
            i9_assign: 
                       expr (line.[7],55,+,ARG_FUN_print_val_v[0 to 3]) with ET=L[8]
                       nop
        i9_assign_end: 
                       nop
        i7_branch_end: 
                       nop
              i10_fun: 
                       fun LOCK_FUN_print.lock()
          i10_fun_end: 
                       nop
           i11_assign: 
                       move (ARG_FUN_print_linenum,2) with ET=L[2]
       i11_assign_end: 
                       nop
              i12_fun: 
                       fun FUN_print.call()
          i12_fun_end: 
                       nop
              i13_fun: 
                       fun LOCK_FUN_print.unlock()
          i13_fun_end: 
end
