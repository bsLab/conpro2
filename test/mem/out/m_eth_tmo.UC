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
  object eth_ev: Event.event
  register eth_timeout: B
end

temp:
begin
  register TEMP_0: L[26]
end

data:
begin
  register TEMP_0: L[26]
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
                       move (eth_timeout,)
        i1_assign_end: 
                       nop
           i2_waitfor: 
                       move ($tmp.[waitfor_count],18518516)
      i2_waitfor_loop: 
                       bind (4)
                       expr ($tmp.[waitfor_count],$tmp.[waitfor_count],-,1)
                       nop
                       expr ($immed.[1],$tmp.[waitfor_count],=,0)
                       falsejump ($immed.[1],i2_waitfor_loop)
  i2_waitfor_loop_end: 
                       jump (i3_assign)
       i2_waitfor_end: 
                       nop
            i3_assign: 
                       move (eth_timeout,)
        i3_assign_end: 
                       nop
               i4_fun: 
                       fun eth_ev.wakeup()
           i4_fun_end: 
end
