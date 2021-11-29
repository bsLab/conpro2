modules:
begin
  module A: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module System: IMPORT
end

import:
begin
  register d: I[16]
end

data:
begin
  register n: I[16]
  register prod: I[16]
end

alu 1: 
begin
  type: I[16]
  op: *
end;

code:
begin
         i1_bind_to_2: 
                       bind (2)
                       move (prod,d) with ET=I[16]
                       move (n,2) with ET=I[16]
     i1_bind_to_2_end: 
                       nop
            i3_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],1) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
        i3_assign_end: 
                       nop
            i4_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
        i4_assign_end: 
                       nop
            i5_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],2) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
        i5_assign_end: 
                       nop
            i6_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
        i6_assign_end: 
                       nop
            i7_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],3) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
        i7_assign_end: 
                       nop
            i8_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
        i8_assign_end: 
                       nop
            i9_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],4) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
        i9_assign_end: 
                       nop
           i10_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i10_assign_end: 
                       nop
           i11_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],5) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i11_assign_end: 
                       nop
           i12_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i12_assign_end: 
                       nop
           i13_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],6) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i13_assign_end: 
                       nop
           i14_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i14_assign_end: 
                       nop
           i15_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],7) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i15_assign_end: 
                       nop
           i16_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i16_assign_end: 
                       nop
           i17_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],8) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i17_assign_end: 
                       nop
           i18_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i18_assign_end: 
                       nop
           i19_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],9) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i19_assign_end: 
                       nop
           i20_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i20_assign_end: 
                       nop
           i21_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],10) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i21_assign_end: 
                       nop
           i22_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i22_assign_end: 
                       nop
           i23_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],11) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i23_assign_end: 
                       nop
           i24_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i24_assign_end: 
                       nop
           i25_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],12) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i25_assign_end: 
                       nop
           i26_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i26_assign_end: 
                       nop
           i27_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],13) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i27_assign_end: 
                       nop
           i28_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i28_assign_end: 
                       nop
           i29_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],14) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i29_assign_end: 
                       nop
           i30_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i30_assign_end: 
                       nop
           i31_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],15) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i31_assign_end: 
                       nop
           i32_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i32_assign_end: 
                       nop
           i33_assign: 
                       expr ($alu.[DT_int 16],prod,[*,@ALU],16) with ET=I[16]
                       expr (prod,$alu.[DT_int 16],[*,@ALU],n) with ET=I[16]
                       nop
       i33_assign_end: 
                       nop
           i34_assign: 
                       expr (n,n,[*,@ALU],2) with ET=I[16]
                       nop
       i34_assign_end: 
                       nop
           i35_assign: 
                       move (d,prod) with ET=I[16]
       i35_assign_end: 
end
