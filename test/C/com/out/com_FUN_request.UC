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
  register RET_FUN_request_d2: L[8]
  register ARG_FUN_request_d1: L[8]
  queue tx_q: L[8]
  object rep: Event.event
  register rep_d: L[8]
end

types:
begin
  type Event.event: {
      method init ()
      method await ()
      method wakeup ()
    }
end

code:
begin
            i1_assign: 
                       move (tx_q,'R') with ET=L[8]
        i1_assign_end: 
                       nop
            i2_assign: 
                       move (tx_q,ARG_FUN_request_d1) with ET=L[8]
        i2_assign_end: 
                       nop
               i3_fun: 
                       fun rep.await()
           i3_fun_end: 
                       nop
            i4_assign: 
                       move (RET_FUN_request_d2,rep_d) with ET=L[8]
        i4_assign_end: 
end
