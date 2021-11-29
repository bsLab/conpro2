modules:
begin
  module Band: PARENT
  module Core: IMPORT
  module Process: IMPORT
end

data:
begin
  register x: I[8]
  register y: I[8]
end

code:
begin
            i1_assign: 
                       move (x,0) with ET=I[8]
        i1_assign_end: 
                       nop
            i2_assign: 
                       expr (x,x,+,1) with ET=I[8]
                       nop
        i2_assign_end: 
                       nop
            i3_assign: 
                       move (y,1) with ET=I[8]
        i3_assign_end: 
                       nop
            i4_assign: 
                       expr (y,x,+,1) with ET=I[8]
                       nop
        i4_assign_end: 
                       nop
            i5_branch: 
                       bind (4)
                       expr ($immed.[1],x,=,1) with ET=I[8]
                       expr ($immed.[2],y,=,0) with ET=I[8]
                       expr ($immed.[3],$immed.[1],and,$immed.[2])
                       falsejump ($immed.[3],%END)
            i6_assign: 
                       expr (x,x,-,1) with ET=I[8]
                       nop
        i6_assign_end: 
                       nop
        i5_branch_end: 
end
