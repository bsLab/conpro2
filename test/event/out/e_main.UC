modules:
begin
  module E: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Event: IMPORT
  module System: IMPORT
end

import:
begin
  object e: Event.event
  array p: object[4] of Process.process
end

temp:
begin
  register TEMP_0: L[6]
end

data:
begin
  register TEMP_0: L[6]
  register LOOP_i_4: I[4]
  register LOOP_i_5: I[4]
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
               i1_fun: 
                       fun e.init()
           i1_fun_end: 
                       nop
          i2_for_loop: 
                       move (LOOP_i_4,0) with ET=I[4]
     i2_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],3,>=,LOOP_i_4) with ET=I[4]
                       falsejump ($immed.[1],i4_for_loop)
               i3_fun: 
                       fun sel[|p_0;...|].start()
           i3_fun_end: 
                       nop
     i2_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_4,LOOP_i_4,+,1) with ET=I[4]
                       nop
                       jump (i2_for_loop_cond)
      i2_for_loop_end: 
                       nop
          i4_for_loop: 
                       move (LOOP_i_5,1) with ET=I[4]
     i4_for_loop_cond: 
                       bind (2)
                       expr ($immed.[1],5,>=,LOOP_i_5) with ET=I[4]
                       falsejump ($immed.[1],%END)
             i5_delay: 
                       move ($tmp.[delay_count],18)
        i5_delay_loop: 
                       bind (4)
                       expr ($tmp.[delay_count],$tmp.[delay_count],-,1)
                       nop
                       expr ($immed.[1],$tmp.[delay_count],=,0)
                       falsejump ($immed.[1],i5_delay_loop)
         i5_delay_end: 
                       nop
               i6_fun: 
                       fun e.wakeup()
           i6_fun_end: 
                       nop
     i4_for_loop_incr: 
                       bind (3)
                       expr (LOOP_i_5,LOOP_i_5,+,1) with ET=I[4]
                       nop
                       jump (i4_for_loop_cond)
      i4_for_loop_end: 
end
