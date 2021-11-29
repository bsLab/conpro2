modules:
begin
  module Com: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Uart: IMPORT
  module System: IMPORT
  module Event: IMPORT
  module Timer: IMPORT
  module Mutex: IMPORT
end

import:
begin
  register com_timeout: B
  object rx_q: Core.Core
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
                       move (com_timeout,false) with ET=B
        i1_assign_end: 
                       nop
             i2_delay: 
                       move ($tmp.[delay_count],185183) with ET=L[19]
        i2_delay_loop: 
                       bind (4)
                       expr ($tmp.[delay_count],$tmp.[delay_count],-,1) with ET=L[19]
                       nop
                       expr ($immed.[1],$tmp.[delay_count],=,0) with ET=L[19]
                       falsejump ($immed.[1],i2_delay_loop)
    i2_delay_loop_end: 
                       jump (i3_fun)
         i2_delay_end: 
                       nop
               i3_fun: 
                       fun rx_q.unlock()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (com_timeout,true) with ET=B
        i4_assign_end: 
end
