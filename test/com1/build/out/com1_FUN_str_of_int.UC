modules:
begin
  module Com1: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
  module Uart: IMPORT
  module Timer: IMPORT
  module System: IMPORT
end

import:
begin
  function div(a:L[16],b:L[16])
  object LOCK_FUN_div: Mutex.mutex
  register ARG_FUN_str_of_int_x: I[16]
  register ARG_FUN_div_a: L[16]
  register ARG_FUN_div_b: L[16]
  array line: register[32] of C8
  object FUN_div: Process.Process
  register RET_FUN_div_z: L[16]
end

temp:
begin
  register TEMP_0: C8
end

data:
begin
  register i: I[8]
  register q: L[16]
  register r: L[16]
  register z: L[16]
  register TEMP_0: C8
  register suppress: B
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
            i1_assign: 
                       move (i,0) with ET=I[8]
        i1_assign_end: 
                       nop
            i2_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_str_of_int_x,<,0) with ET=I[16]
                       falsejump ($immed.[1],i6_assign)
            i3_assign: 
                       move (line.[i:CT=L[5]],'-') with ET=C8
        i3_assign_end: 
                       nop
            i4_assign: 
                       expr (i,i,+,1) with ET=I[8]
                       nop
        i4_assign_end: 
                       nop
            i5_assign: 
                       move (ARG_FUN_str_of_int_x,-ARG_FUN_str_of_int_x) with ET=I[16]
        i5_assign_end: 
                       nop
        i2_branch_end: 
                       nop
            i6_assign: 
                       move (z,ARG_FUN_str_of_int_x:CT=L[0]) with ET=L[16]
        i6_assign_end: 
                       nop
            i7_assign: 
                       move (q,10000) with ET=L[16]
        i7_assign_end: 
                       nop
            i8_assign: 
                       move (suppress,true) with ET=B
        i8_assign_end: 
                       nop
        i9_while_loop: 
                       bind (2)
                       expr ($immed.[1],q,<>,0) with ET=L[16]
                       falsejump ($immed.[1],i26_assign)
               i9_fun: 
                       fun LOCK_FUN_div.lock()
           i9_fun_end: 
                       nop
           i10_assign: 
                       move (ARG_FUN_div_a,z) with ET=L[16]
       i10_assign_end: 
                       nop
           i11_assign: 
                       move (ARG_FUN_div_b,q) with ET=L[16]
       i11_assign_end: 
                       nop
              i12_fun: 
                       fun FUN_div.call()
          i12_fun_end: 
                       nop
           i13_assign: 
                       move (r,RET_FUN_div_z) with ET=L[16]
       i13_assign_end: 
                       nop
              i14_fun: 
                       fun LOCK_FUN_div.unlock()
          i14_fun_end: 
                       nop
           i15_branch: 
                       bind (6)
                       expr ($immed.[1],suppress,=,true) with ET=B
                       expr ($immed.[2],r,<>,0) with ET=L[16]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       expr ($immed.[4],suppress,=,false) with ET=B
                       expr ($immed.[5],$immed.[3],or,$immed.[4])
                       falsejump ($immed.[5],i19_assign)
           i16_assign: 
                       expr ($tmp.[%S],r:CT=C8,+,'0') with ET=C8 and ET=L[8]
                       move (line.[i:CT=L[5]],$tmp.[%S]) with ET=C8
                       nop
       i16_assign_end: 
                       nop
           i17_assign: 
                       move (suppress,false) with ET=B
       i17_assign_end: 
                       nop
           i18_assign: 
                       expr (i,i,+,1) with ET=I[8]
                       nop
       i18_assign_end: 
                       nop
       i15_branch_end: 
                       nop
           i19_assign: 
                       bind (2)
                       expr ($immed.[2],q,*,r) with ET=L[16]
                       expr (z,z,-,$immed.[2]) with ET=L[16]
                       nop
       i19_assign_end: 
                       nop
              i20_fun: 
                       fun LOCK_FUN_div.lock()
          i20_fun_end: 
                       nop
           i21_assign: 
                       move (ARG_FUN_div_a,q) with ET=L[16]
       i21_assign_end: 
                       nop
           i22_assign: 
                       move (ARG_FUN_div_b,10) with ET=L[16]
       i22_assign_end: 
                       nop
              i23_fun: 
                       fun FUN_div.call()
          i23_fun_end: 
                       nop
           i24_assign: 
                       move (q,RET_FUN_div_z) with ET=L[16]
       i24_assign_end: 
                       nop
              i25_fun: 
                       fun LOCK_FUN_div.unlock()
          i25_fun_end: 
                       jump (i9_while_loop)
    i9_while_loop_end: 
                       nop
           i26_assign: 
                       move (line.[i:CT=L[5]],'.') with ET=C8
       i26_assign_end: 
end
