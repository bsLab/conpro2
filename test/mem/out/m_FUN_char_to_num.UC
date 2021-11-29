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
  register RET_FUN_char_to_num_y: L[8]
  register ARG_FUN_char_to_num_x: L[8]
end

code:
begin
            i1_branch: 
                       bind (4)
                       expr ($immed.[1],ARG_FUN_char_to_num_x,>=,) with ET=L[8]
                       expr ($immed.[2],ARG_FUN_char_to_num_x,<=,) with ET=L[8]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],i3_branch)
            i2_assign: 
                       expr (RET_FUN_char_to_num_y,ARG_FUN_char_to_num_x,-,) with ET=L[8]
                       nop
        i2_assign_end: 
                       jump (%END)
            i3_branch: 
                       bind (4)
                       expr ($immed.[1],ARG_FUN_char_to_num_x,>=,) with ET=L[8]
                       expr ($immed.[2],ARG_FUN_char_to_num_x,<=,) with ET=L[8]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],i5_assign)
            i4_assign: 
                       expr (RET_FUN_char_to_num_y,ARG_FUN_char_to_num_x,-,55) with ET=L[8]
                       nop
        i4_assign_end: 
                       jump (%END)
            i5_assign: 
                       move (RET_FUN_char_to_num_y,0) with ET=L[8]
        i5_assign_end: 
                       nop
        i3_branch_end: 
                       nop
        i1_branch_end: 
end
