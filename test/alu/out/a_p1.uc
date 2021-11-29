-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2009 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D101 Genetic size: 2267134
--         Compile date: Mon Aug 17 19:21:41 CEST 2009
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

import:
begin
  reg d: I16
end

data:
begin
  reg n: I16
  reg sum: I16
end

alu 1: 
begin
  type: I16
  op: +
end;

code:
begin
       i1_bind_to_2: 
                     bind (2)
                     move (sum,d)
                     move (n,2)
   i1_bind_to_2_end: 
                     nop
          i3_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,0)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
      i3_assign_end: 
                     nop
          i4_assign: 
                     expr (n,n,+@ALU,2)
                     nop
      i4_assign_end: 
                     nop
          i5_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,1)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
      i5_assign_end: 
                     nop
          i6_assign: 
                     expr (n,n,+@ALU,2)
                     nop
      i6_assign_end: 
                     nop
          i7_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,2)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
      i7_assign_end: 
                     nop
          i8_assign: 
                     expr (n,n,+@ALU,2)
                     nop
      i8_assign_end: 
                     nop
          i9_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,3)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
      i9_assign_end: 
                     nop
         i10_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i10_assign_end: 
                     nop
         i11_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,4)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i11_assign_end: 
                     nop
         i12_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i12_assign_end: 
                     nop
         i13_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,5)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i13_assign_end: 
                     nop
         i14_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i14_assign_end: 
                     nop
         i15_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,6)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i15_assign_end: 
                     nop
         i16_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i16_assign_end: 
                     nop
         i17_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,7)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i17_assign_end: 
                     nop
         i18_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i18_assign_end: 
                     nop
         i19_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,8)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i19_assign_end: 
                     nop
         i20_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i20_assign_end: 
                     nop
         i21_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,9)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i21_assign_end: 
                     nop
         i22_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i22_assign_end: 
                     nop
         i23_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,10)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i23_assign_end: 
                     nop
         i24_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i24_assign_end: 
                     nop
         i25_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,11)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i25_assign_end: 
                     nop
         i26_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i26_assign_end: 
                     nop
         i27_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,12)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i27_assign_end: 
                     nop
         i28_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i28_assign_end: 
                     nop
         i29_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,13)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i29_assign_end: 
                     nop
         i30_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i30_assign_end: 
                     nop
         i31_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,14)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i31_assign_end: 
                     nop
         i32_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i32_assign_end: 
                     nop
         i33_assign: 
                     expr ($alu.[DT_int 16],sum,+@ALU,15)
                     expr (sum,$alu.[DT_int 16],+@ALU,n)
                     nop
     i33_assign_end: 
                     nop
         i34_assign: 
                     expr (n,n,+@ALU,2)
                     nop
     i34_assign_end: 
                     nop
         i35_assign: 
                     move (d,sum)
     i35_assign_end: 
end
