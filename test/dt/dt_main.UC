modules:
begin
  module Dt: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

data:
begin
  register x: I[8]
  register y: I[9]
  register z: I[18]
end

code:
begin
         i1_bind_to_3: 
                       bind (3)
                       move (x,0) with ET=I[8]
                       move (y,1) with ET=I[9]
                       move (z,0) with ET=I[18]
     i1_bind_to_3_end: 
                       nop
            i4_assign: 
                       bind (2)
                       expr ($immed.[2],z,+,x) with ET=I[18]
                       expr (z,$immed.[2],+,y) with ET=I[18]
                       nop
        i4_assign_end: 
end
