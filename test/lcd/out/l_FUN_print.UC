modules:
begin
  module L: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module System: IMPORT
  module Timer: IMPORT
  module Lcd: IMPORT
  module Mutex: IMPORT
  module Random: IMPORT
end

import:
begin
  register stat_b: L[1]
  object print_lock: Mutex.mutex
  object lcd1: Lcd.lcd
  array line: register[32] of L[8]
  register ARG_FUN_print_linenum: L[2]
end

data:
begin
  register i: L[5]
  register TEMPS_0: L[9]
  register TEMPS_1: L[9]
  register TEMPS_2: L[8]
  register eol: B
end

types:
begin
  type Lcd.lcd: {
      method init ()
      method cmd (L[8]:RHS)
      method write (L[8]:RHS)
      method interface (L[4]:LRHS,L[1]:LHS,L[1]:LHS,L[1]:LHS,L[1]:LHS)
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
                       move (stat_b,1) with ET=L[1]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (i,0) with ET=L[5]
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (eol,)
        i3_assign_end: 
                       nop
            i4_select: 
                       bind (12)
            i4_case_1: 
                       expr ($immed.[1],ARG_FUN_print_linenum,=,1) with ET=L[2]
                       falsejump ($immed.[1],i4_case_2)
                       jump (i5_fun)
            i4_case_2: 
                       expr ($immed.[1],ARG_FUN_print_linenum,=,2) with ET=L[2]
                       falsejump ($immed.[1],i4_case_3)
                       jump (i6_assign)
            i4_case_3: 
                       expr ($immed.[1],ARG_FUN_print_linenum,=,3) with ET=L[2]
                       falsejump ($immed.[1],i10_while_loop)
                       jump (i8_assign)
               i5_fun: 
                       fun lcd1.cmd(128)
           i5_fun_end: 
                       jump (i10_while_loop)
            i6_assign: 
                       move (TEMPS_0,144) with ET=L[9]
        i6_assign_end: 
                       nop
               i7_fun: 
                       fun lcd1.cmd(TEMPS_0)
           i7_fun_end: 
                       jump (i10_while_loop)
            i8_assign: 
                       move (TEMPS_1,160) with ET=L[9]
        i8_assign_end: 
                       nop
               i9_fun: 
                       fun lcd1.cmd(TEMPS_1)
           i9_fun_end: 
                       nop
        i4_select_end: 
                       nop
       i10_while_loop: 
                       bind (2)
                       expr ($immed.[1],eol,=,)
                       falsejump ($immed.[1],i14_fun)
           i10_assign: 
                       move (TEMPS_2,line.[i:CT=L[5]])
       i10_assign_end: 
                       nop
              i11_fun: 
                       fun lcd1.write(TEMPS_2)
          i11_fun_end: 
                       nop
           i12_assign: 
                       expr (eol:CT=L[1],line.[i:CT=L[5]],=,) with ET=L[8]
                       nop
       i12_assign_end: 
                       nop
           i13_assign: 
                       expr (i,i,+,1) with ET=L[5]
                       nop
       i13_assign_end: 
                       jump (i10_while_loop)
   i10_while_loop_end: 
                       nop
              i14_fun: 
                       fun print_lock.unlock()
          i14_fun_end: 
end
