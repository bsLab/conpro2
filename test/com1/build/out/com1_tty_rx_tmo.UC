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
  register tty_stdin_tmo: L[1]
  object tty_stdin: Core.Core
end

temp:
begin
  register TEMP_0: L[19]
end

data:
begin
  register TEMP_0: L[19]
end

types:
begin
  type Core.Core: {
      method init (O#'a:RHS)
      method set (O#'a:RHS,O#'a:RHS,O#'a:RHS)
      method copy (O#'a:RHS)
      method copyn (O#'a:RHS,O#'a:RHS,O#'a:RHS,O#'a:RHS)
      method cmp_eq (O#'a:LHS,O#'a:RHS)
      method cmp_neq (O#'a:LHS,O#'a:RHS)
      method cmpn_eq (O#'a:LHS,O#'a:RHS,O#'a:RHS,O#'a:RHS,O#'a:RHS)
      method cmpn_neq (O#'a:LHS,O#'a:RHS,O#'a:RHS,O#'a:RHS,O#'a:RHS)
      method unlock ()
      method guard (O#'a:RHS)
      method empty (O#'a:LHS)
      method full (O#'a:LHS)
    }
end

code:
begin
            i1_assign: 
                       move (tty_stdin_tmo,0) with ET=L[1]
        i1_assign_end: 
                       nop
           i2_waitfor: 
                       move ($tmp.[waitfor_count],185183) with ET=L[19]
      i2_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1) with ET=L[19]
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0) with ET=L[19]
                       falsejump ($immed.[1],i2_waitfor_loop)
  i2_waitfor_loop_end: 
                       jump (i3_assign)
       i2_waitfor_end: 
                       nop
            i3_assign: 
                       move (tty_stdin_tmo,1) with ET=L[1]
        i3_assign_end: 
                       nop
               i4_fun: 
                       fun tty_stdin.unlock()
           i4_fun_end: 
end
