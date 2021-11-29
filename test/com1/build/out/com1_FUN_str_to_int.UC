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
  array line: register[32] of C8
  register RET_FUN_str_to_int_y: I[16]
end

data:
begin
  register i: I[8]
  register sign: B
  register c: I[8]
end

code:
begin
            i1_assign: 
                       move (i,0) with ET=I[8]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (RET_FUN_str_to_int_y,0) with ET=I[16]
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (sign,false) with ET=B
        i3_assign_end: 
                       nop
            i4_branch: 
                       bind (2)
                       expr ($immed.[1],line.[i:CT=L[5]],=,'-') with ET=L[8]
                       falsejump ($immed.[1],i7_while_loop)
            i5_assign: 
                       move (sign,true) with ET=B
        i5_assign_end: 
                       nop
            i6_assign: 
                       expr (i,i,+,1) with ET=I[8]
                       nop
        i6_assign_end: 
                       nop
        i4_branch_end: 
                       nop
        i7_while_loop: 
                       bind (2)
                       expr ($immed.[1],line.[i:CT=L[5]],<>,'.') with ET=L[8]
                       falsejump ($immed.[1],i11_branch)
            i7_assign: 
                       expr (RET_FUN_str_to_int_y,RET_FUN_str_to_int_y,*,10) with ET=I[16]
                       nop
        i7_assign_end: 
                       nop
            i8_assign: 
                       expr (c,line.[i:CT=L[5]],-,'0') with ET=I[8]
                       nop
        i8_assign_end: 
                       nop
            i9_assign: 
                       expr (RET_FUN_str_to_int_y,RET_FUN_str_to_int_y,+,c) with ET=I[16]
                       nop
        i9_assign_end: 
                       nop
           i10_assign: 
                       expr (i,i,+,1) with ET=I[8]
                       nop
       i10_assign_end: 
                       jump (i7_while_loop)
    i7_while_loop_end: 
                       nop
           i11_branch: 
                       bind (2)
                       expr ($immed.[1],sign,=,true) with ET=B
                       falsejump ($immed.[1],%END)
           i12_assign: 
                       move (RET_FUN_str_to_int_y,-RET_FUN_str_to_int_y) with ET=I[16]
       i12_assign_end: 
                       nop
       i11_branch_end: 
end
