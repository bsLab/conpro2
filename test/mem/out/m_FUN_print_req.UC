modules:
begin
  module M: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
  module Mem: IMPORT
  module Event: IMPORT
end

import:
begin
  object LOCK_FUN_print: Mutex.mutex
  object print_lock: Mutex.mutex
  object FUN_print: Process.Process
  register ARG_FUN_print_req_k: L[8]
  array line: variable[32] of L[8]var_array_line
  register count: L[16]
  block var_array_line: cells[32] of L[8]
  register ARG_FUN_print_req_v: L[8]
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
            i2_assign: 
                       expr (count,count,+,1) with ET=L[16]
                       nop
        i2_assign_end: 
                       nop
               i3_fun: 
                       fun line.copy()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (line.[4],ARG_FUN_print_req_k)
        i4_assign_end: 
                       nop
            i5_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_print_req_v[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i7_assign)
            i6_assign: 
                       expr (line.[6],,+,ARG_FUN_print_req_v[4 to 7]) with ET=L[8]
                       nop
        i6_assign_end: 
                       jump (i8_branch)
            i7_assign: 
                       expr (line.[6],55,+,ARG_FUN_print_req_v[4 to 7]) with ET=L[8]
                       nop
        i7_assign_end: 
                       nop
        i5_branch_end: 
                       nop
            i8_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_print_req_v[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i10_assign)
            i9_assign: 
                       expr (line.[7],,+,ARG_FUN_print_req_v[0 to 3]) with ET=L[8]
                       nop
        i9_assign_end: 
                       jump (i11_branch)
           i10_assign: 
                       expr (line.[7],55,+,ARG_FUN_print_req_v[0 to 3]) with ET=L[8]
                       nop
       i10_assign_end: 
                       nop
        i8_branch_end: 
                       nop
           i11_branch: 
                       bind (2)
                       expr ($immed.[1],count[12 to 15],<,10) with ET=L[4]
                       falsejump ($immed.[1],i13_assign)
           i12_assign: 
                       expr (line.[10],,+,count[12 to 15]) with ET=L[8]
                       nop
       i12_assign_end: 
                       jump (i14_branch)
           i13_assign: 
                       expr (line.[10],55,+,count[12 to 15]) with ET=L[8]
                       nop
       i13_assign_end: 
                       nop
       i11_branch_end: 
                       nop
           i14_branch: 
                       bind (2)
                       expr ($immed.[1],count[8 to 11],<,10) with ET=L[4]
                       falsejump ($immed.[1],i16_assign)
           i15_assign: 
                       expr (line.[11],,+,count[8 to 11]) with ET=L[8]
                       nop
       i15_assign_end: 
                       jump (i17_branch)
           i16_assign: 
                       expr (line.[11],55,+,count[8 to 11]) with ET=L[8]
                       nop
       i16_assign_end: 
                       nop
       i14_branch_end: 
                       nop
           i17_branch: 
                       bind (2)
                       expr ($immed.[1],count[4 to 7],<,10) with ET=L[4]
                       falsejump ($immed.[1],i19_assign)
           i18_assign: 
                       expr (line.[12],,+,count[4 to 7]) with ET=L[8]
                       nop
       i18_assign_end: 
                       jump (i20_branch)
           i19_assign: 
                       expr (line.[12],55,+,count[4 to 7]) with ET=L[8]
                       nop
       i19_assign_end: 
                       nop
       i17_branch_end: 
                       nop
           i20_branch: 
                       bind (2)
                       expr ($immed.[1],count[0 to 3],<,10) with ET=L[4]
                       falsejump ($immed.[1],i22_assign)
           i21_assign: 
                       expr (line.[13],,+,count[0 to 3]) with ET=L[8]
                       nop
       i21_assign_end: 
                       jump (i23_fun)
           i22_assign: 
                       expr (line.[13],55,+,count[0 to 3]) with ET=L[8]
                       nop
       i22_assign_end: 
                       nop
       i20_branch_end: 
                       nop
              i23_fun: 
                       fun LOCK_FUN_print.lock()
          i23_fun_end: 
                       nop
           i24_assign: 
                       move (ARG_FUN_print_linenum,3) with ET=L[2]
       i24_assign_end: 
                       nop
              i25_fun: 
                       fun FUN_print.call()
          i25_fun_end: 
                       nop
              i26_fun: 
                       fun LOCK_FUN_print.unlock()
          i26_fun_end: 
end
