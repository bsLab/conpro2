-- CONPRO: Hardware Synthesis with an Imperative High Level Multiprocess Approach
--         (c) 2006-2010 by BSSLAB, Dr. Stefan Bosse
--         Version: 2.1 Revision: D150 Genetic size: 2683003
--         Compile date: Mon May  3 11:24:21 CEST 2010
--         Compiled by:  sbosse
--         Compiled on:  SunOS sunsil 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Blade-2500

data:
begin
  register x: I8
  register y: I9
  register z: I18
end

code:
begin
       i1_bind_to_3: 
                     bind (3)
                     move (x,0)
                     move (y,1)
                     move (z,0)
   i1_bind_to_3_end: 
                     nop
          i4_assign: 
                     bind (2)
                     expr ($immed.[2],z,+,x:I18)
                     expr (z,$immed.[2],+,y:I18)
                     nop
      i4_assign_end: 
end
