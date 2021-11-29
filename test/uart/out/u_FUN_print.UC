modules:
begin
  module U: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
end

import:
begin
  object print_lock: Mutex.mutex
  array line: variable[32] of L[8]var_array_line
  block var_array_line: cells[32] of L[8]
  register ARG_FUN_print_linenum: L[2]
  queue q_lcd1_tx: L[9]
end

temp:
begin
  register TEMP_0: L[8]
end

data:
begin
  register i: L[5]
  register TEMP_0: L[8]
  register eol: B
end

types:
begin
  type Mutex.mutex: {
      method init ()
      method lock ()
      method unlock ()
    }
end

code:
begin
            i1_assign: 
                       move (i,0) with ET=L[5]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (eol,)
        i2_assign_end: 
                       nop
            i3_select: 
                       bind (12)
            i3_case_1: 
                       expr ($immed.[1],ARG_FUN_print_linenum,=,1) with ET=L[2]
                       falsejump ($immed.[1],i3_case_2)
                       jump (i4_assign)
            i3_case_2: 
                       expr ($immed.[1],ARG_FUN_print_linenum,=,2) with ET=L[2]
                       falsejump ($immed.[1],i3_case_3)
                       jump (i5_assign)
            i3_case_3: 
                       expr ($immed.[1],ARG_FUN_print_linenum,=,3) with ET=L[2]
                       falsejump ($immed.[1],i7_while_loop)
                       jump (i6_assign)
            i4_assign: 
                       move (q_lcd1_tx,384) with ET=L[9]
        i4_assign_end: 
                       jump (i7_while_loop)
            i5_assign: 
                       move (q_lcd1_tx,400) with ET=L[9]
        i5_assign_end: 
                       jump (i7_while_loop)
            i6_assign: 
                       move (q_lcd1_tx,416) with ET=L[9]
        i6_assign_end: 
                       nop
        i3_select_end: 
                       nop
        i7_while_loop: 
                       bind (2)
                       expr ($immed.[1],eol,=,)
                       falsejump ($immed.[1],i10_fun)
            i7_assign: 
                       move ($tmp.[line],line.[i])
                       move (q_lcd1_tx,$tmp.[line]) with ET=L[9]
        i7_assign_end: 
                       nop
            i8_assign: 
                       move ($tmp.[line],line.[i])
                       expr (eol:CT=L[1],$tmp.[line],=,) with ET=L[8]
                       nop
        i8_assign_end: 
                       nop
            i9_assign: 
                       expr (i,i,+,1) with ET=L[5]
                       nop
        i9_assign_end: 
                       jump (i7_while_loop)
    i7_while_loop_end: 
                       nop
              i10_fun: 
                       fun print_lock.unlock()
          i10_fun_end: 
end
