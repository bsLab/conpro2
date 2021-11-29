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
  const STATUS_ACT: 3
  register sys_status: L[3]
  register stat_led: L[1]
  register sys_status_next: L[3]
  object watch_timer: Timer.timer
  const STATUS_ERR: 2
  const STATUS_OK: 1
  const STATUS_EV: 4
  const STATUS_DOWN: 5
end

data:
begin
  register last_status: L[3]
  register counter: I[6]
  register onl: B
end

types:
begin
  type Timer.timer: {
      method init ()
      method time (N[0]:RHS)
      method await ()
      method wakeup ()
      method start ()
      method stop ()
      method mode (L[1]:RHS)
      method sig_action (L[1]:LHS,N[0]:RHS,N[0]:RHS)
    }
end

code:
begin
            i1_assign: 
                       move (stat_led,0) with ET=L[1]
        i1_assign_end: 
                       nop
         i2_bind_to_3: 
                       bind (2)
                       move (counter,0) with ET=I[6]
                       move (onl,false) with ET=B
     i2_bind_to_3_end: 
                       nop
               i4_fun: 
                       fun watch_timer.init()
           i4_fun_end: 
                       nop
               i5_fun: 
                       fun watch_timer.start()
           i5_fun_end: 
                       nop
            i6_assign: 
                       move (last_status,5) with ET=L[3]
        i6_assign_end: 
                       nop
            i7_assign: 
                       move (sys_status_next,last_status) with ET=L[3]
        i7_assign_end: 
                       jump (i9_assign)
              i8_loop: 
                       nop
            i9_assign: 
                       move (last_status,sys_status_next) with ET=L[3]
        i9_assign_end: 
                       nop
           i10_select: 
                       bind (20)
           i10_case_1: 
                       expr ($immed.[1],sys_status,=,1) with ET=L[3]
                       falsejump ($immed.[1],i10_case_2)
                       jump (i11_bind_to_13)
           i10_case_2: 
                       expr ($immed.[1],sys_status,=,5) with ET=L[3]
                       falsejump ($immed.[1],i10_case_3)
                       jump (i14_bind_to_16)
           i10_case_3: 
                       expr ($immed.[1],sys_status,=,2) with ET=L[3]
                       falsejump ($immed.[1],i10_case_4)
                       jump (i17_branch)
           i10_case_4: 
                       expr ($immed.[1],sys_status,=,3) with ET=L[3]
                       falsejump ($immed.[1],i10_case_5)
                       jump (i25_branch)
           i10_case_5: 
                       expr ($immed.[1],sys_status,=,4) with ET=L[3]
                       falsejump ($immed.[1],i41_assign)
                       jump (i33_branch)
       i11_bind_to_13: 
                       bind (3)
                       move (onl,true) with ET=B
                       move (counter,0) with ET=I[6]
                       move (last_status,1) with ET=L[3]
   i11_bind_to_13_end: 
                       jump (i41_assign)
       i14_bind_to_16: 
                       bind (3)
                       move (onl,false) with ET=B
                       move (counter,0) with ET=I[6]
                       move (last_status,5) with ET=L[3]
   i14_bind_to_16_end: 
                       jump (i41_assign)
           i17_branch: 
                       bind (4)
                       expr ($immed.[1],onl,=,false) with ET=B
                       expr ($immed.[2],counter,=,0) with ET=I[6]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],i21_branch)
       i18_bind_to_20: 
                       bind (3)
                       move (onl,true) with ET=B
                       move (counter,2) with ET=I[6]
                       move (last_status,2) with ET=L[3]
   i18_bind_to_20_end: 
                       jump (i41_assign)
           i21_branch: 
                       bind (4)
                       expr ($immed.[1],onl,=,true) with ET=B
                       expr ($immed.[2],counter,=,0) with ET=I[6]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],i24_assign)
       i22_bind_to_23: 
                       bind (2)
                       move (onl,false) with ET=B
                       move (counter,2) with ET=I[6]
   i22_bind_to_23_end: 
                       jump (i41_assign)
           i24_assign: 
                       expr (counter,counter,-,1) with ET=I[6]
                       nop
       i24_assign_end: 
                       nop
       i21_branch_end: 
                       nop
       i17_branch_end: 
                       jump (i41_assign)
           i25_branch: 
                       bind (4)
                       expr ($immed.[1],onl,=,false) with ET=B
                       expr ($immed.[2],counter,=,0) with ET=I[6]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],i29_branch)
       i26_bind_to_27: 
                       bind (2)
                       move (onl,true) with ET=B
                       move (counter,6) with ET=I[6]
   i26_bind_to_27_end: 
                       nop
           i28_assign: 
                       move (last_status,3) with ET=L[3]
       i28_assign_end: 
                       jump (i41_assign)
           i29_branch: 
                       bind (4)
                       expr ($immed.[1],onl,=,true) with ET=B
                       expr ($immed.[2],counter,=,0) with ET=I[6]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],i32_assign)
       i30_bind_to_31: 
                       bind (2)
                       move (onl,false) with ET=B
                       move (counter,6) with ET=I[6]
   i30_bind_to_31_end: 
                       jump (i41_assign)
           i32_assign: 
                       expr (counter,counter,-,1) with ET=I[6]
                       nop
       i32_assign_end: 
                       nop
       i29_branch_end: 
                       nop
       i25_branch_end: 
                       jump (i41_assign)
           i33_branch: 
                       bind (2)
                       expr ($immed.[1],counter,=,0) with ET=I[6]
                       falsejump ($immed.[1],i36_branch)
       i34_bind_to_35: 
                       bind (2)
                       move (onl,true) with ET=B
                       move (counter,3) with ET=I[6]
   i34_bind_to_35_end: 
                       jump (i41_assign)
           i36_branch: 
                       bind (2)
                       expr ($immed.[1],counter,=,1) with ET=I[6]
                       falsejump ($immed.[1],i39_assign)
           i37_assign: 
                       move (counter,0) with ET=I[6]
       i37_assign_end: 
                       nop
           i38_assign: 
                       move (sys_status,last_status) with ET=L[3]
       i38_assign_end: 
                       jump (i41_assign)
           i39_assign: 
                       move (onl,not onl) with ET=B
       i39_assign_end: 
                       nop
           i40_assign: 
                       expr (counter,counter,-,1) with ET=I[6]
                       nop
       i40_assign_end: 
                       nop
       i36_branch_end: 
                       nop
       i33_branch_end: 
                       nop
       i10_select_end: 
                       nop
           i41_assign: 
                       move (sys_status_next,last_status) with ET=L[3]
       i41_assign_end: 
                       nop
           i42_branch: 
                       bind (2)
                       expr ($immed.[1],onl,=,true) with ET=B
                       falsejump ($immed.[1],i44_assign)
           i43_assign: 
                       move (stat_led,1) with ET=L[1]
       i43_assign_end: 
                       jump (i45_fun)
           i44_assign: 
                       move (stat_led,0) with ET=L[1]
       i44_assign_end: 
                       nop
       i42_branch_end: 
                       nop
              i45_fun: 
                       fun watch_timer.await()
          i45_fun_end: 
                       jump (i9_assign)
          i8_loop_end: 
end
