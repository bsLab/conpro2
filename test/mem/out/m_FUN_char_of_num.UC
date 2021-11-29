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
  register RET_FUN_char_of_num_y: L[8]
  register ARG_FUN_char_of_num_x: L[4]
end

code:
begin
            i1_branch: 
                       bind (2)
                       expr ($immed.[1],ARG_FUN_char_of_num_x,<=,9) with ET=L[4]
                       falsejump ($immed.[1],i3_assign)
            i2_assign: 
                       expr (RET_FUN_char_of_num_y,ARG_FUN_char_of_num_x,+,) with ET=L[8]
                       nop
        i2_assign_end: 
                       jump (%END)
            i3_assign: 
                       expr (RET_FUN_char_of_num_y,55,+,ARG_FUN_char_of_num_x) with ET=L[8]
                       nop
        i3_assign_end: 
                       nop
        i1_branch_end: 
end
