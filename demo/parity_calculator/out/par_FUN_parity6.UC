modules:
begin
  module Par: PARENT
  module Core: IMPORT
  module Process: IMPORT
  module Mutex: IMPORT
end

import:
begin
  register ARG_FUN_parity6_x: L[64]
  register RET_FUN_parity6_p: L[1]
end

data:
begin
  register pl: L[1]
  register xl: L[64]
end

code:
begin
        BLOCKBOUND1_1: 
                       bind (3)
            i1_assign: 
                       move (xl,ARG_FUN_parity6_x) with ET=L[64]
        i1_assign_end: 
        BLOCKBOUND1_2: 
                       bind (136)
                       nop
            i2_assign: 
                       expr ($immed.[64],0,lxor,xl[0]) with ET=L[1]
                       expr ($immed.[63],$immed.[64],lxor,xl[1]) with ET=L[1]
                       expr ($immed.[62],$immed.[63],lxor,xl[2]) with ET=L[1]
                       expr ($immed.[61],$immed.[62],lxor,xl[3]) with ET=L[1]
                       expr ($immed.[60],$immed.[61],lxor,xl[4]) with ET=L[1]
                       expr ($immed.[59],$immed.[60],lxor,xl[5]) with ET=L[1]
                       expr ($immed.[58],$immed.[59],lxor,xl[6]) with ET=L[1]
                       expr ($immed.[57],$immed.[58],lxor,xl[7]) with ET=L[1]
                       expr ($immed.[56],$immed.[57],lxor,xl[8]) with ET=L[1]
                       expr ($immed.[55],$immed.[56],lxor,xl[9]) with ET=L[1]
                       expr ($immed.[54],$immed.[55],lxor,xl[10]) with ET=L[1]
                       expr ($immed.[53],$immed.[54],lxor,xl[11]) with ET=L[1]
                       expr ($immed.[52],$immed.[53],lxor,xl[12]) with ET=L[1]
                       expr ($immed.[51],$immed.[52],lxor,xl[13]) with ET=L[1]
                       expr ($immed.[50],$immed.[51],lxor,xl[14]) with ET=L[1]
                       expr ($immed.[49],$immed.[50],lxor,xl[15]) with ET=L[1]
                       expr ($immed.[48],$immed.[49],lxor,xl[16]) with ET=L[1]
                       expr ($immed.[47],$immed.[48],lxor,xl[17]) with ET=L[1]
                       expr ($immed.[46],$immed.[47],lxor,xl[18]) with ET=L[1]
                       expr ($immed.[45],$immed.[46],lxor,xl[19]) with ET=L[1]
                       expr ($immed.[44],$immed.[45],lxor,xl[20]) with ET=L[1]
                       expr ($immed.[43],$immed.[44],lxor,xl[21]) with ET=L[1]
                       expr ($immed.[42],$immed.[43],lxor,xl[22]) with ET=L[1]
                       expr ($immed.[41],$immed.[42],lxor,xl[23]) with ET=L[1]
                       expr ($immed.[40],$immed.[41],lxor,xl[24]) with ET=L[1]
                       expr ($immed.[39],$immed.[40],lxor,xl[25]) with ET=L[1]
                       expr ($immed.[38],$immed.[39],lxor,xl[26]) with ET=L[1]
                       expr ($immed.[37],$immed.[38],lxor,xl[27]) with ET=L[1]
                       expr ($immed.[36],$immed.[37],lxor,xl[28]) with ET=L[1]
                       expr ($immed.[35],$immed.[36],lxor,xl[29]) with ET=L[1]
                       expr ($immed.[34],$immed.[35],lxor,xl[30]) with ET=L[1]
                       expr ($immed.[33],$immed.[34],lxor,xl[31]) with ET=L[1]
                       expr ($immed.[32],$immed.[33],lxor,xl[32]) with ET=L[1]
                       expr ($immed.[31],$immed.[32],lxor,xl[33]) with ET=L[1]
                       expr ($immed.[30],$immed.[31],lxor,xl[34]) with ET=L[1]
                       expr ($immed.[29],$immed.[30],lxor,xl[35]) with ET=L[1]
                       expr ($immed.[28],$immed.[29],lxor,xl[36]) with ET=L[1]
                       expr ($immed.[27],$immed.[28],lxor,xl[37]) with ET=L[1]
                       expr ($immed.[26],$immed.[27],lxor,xl[38]) with ET=L[1]
                       expr ($immed.[25],$immed.[26],lxor,xl[39]) with ET=L[1]
                       expr ($immed.[24],$immed.[25],lxor,xl[40]) with ET=L[1]
                       expr ($immed.[23],$immed.[24],lxor,xl[41]) with ET=L[1]
                       expr ($immed.[22],$immed.[23],lxor,xl[42]) with ET=L[1]
                       expr ($immed.[21],$immed.[22],lxor,xl[43]) with ET=L[1]
                       expr ($immed.[20],$immed.[21],lxor,xl[44]) with ET=L[1]
                       expr ($immed.[19],$immed.[20],lxor,xl[45]) with ET=L[1]
                       expr ($immed.[18],$immed.[19],lxor,xl[46]) with ET=L[1]
                       expr ($immed.[17],$immed.[18],lxor,xl[47]) with ET=L[1]
                       expr ($immed.[16],$immed.[17],lxor,xl[48]) with ET=L[1]
                       expr ($immed.[15],$immed.[16],lxor,xl[49]) with ET=L[1]
                       expr ($immed.[14],$immed.[15],lxor,xl[50]) with ET=L[1]
                       expr ($immed.[13],$immed.[14],lxor,xl[51]) with ET=L[1]
                       expr ($immed.[12],$immed.[13],lxor,xl[52]) with ET=L[1]
                       expr ($immed.[11],$immed.[12],lxor,xl[53]) with ET=L[1]
                       expr ($immed.[10],$immed.[11],lxor,xl[54]) with ET=L[1]
                       expr ($immed.[9],$immed.[10],lxor,xl[55]) with ET=L[1]
                       expr ($immed.[8],$immed.[9],lxor,xl[56]) with ET=L[1]
                       expr ($immed.[7],$immed.[8],lxor,xl[57]) with ET=L[1]
                       expr ($immed.[6],$immed.[7],lxor,xl[58]) with ET=L[1]
                       expr ($immed.[5],$immed.[6],lxor,xl[59]) with ET=L[1]
                       expr ($immed.[4],$immed.[5],lxor,xl[60]) with ET=L[1]
                       expr ($immed.[3],$immed.[4],lxor,xl[61]) with ET=L[1]
                       expr ($immed.[2],$immed.[3],lxor,xl[62]) with ET=L[1]
                       expr (RET_FUN_parity6_p,$immed.[2],lxor,xl[63]) with ET=L[1]
                       nop
        i2_assign_end: 
                       nop
            i3_assign: 
                       expr ($immed.[64],0,lxor,xl[0]) with ET=L[1]
                       expr ($immed.[63],$immed.[64],lxor,xl[1]) with ET=L[1]
                       expr ($immed.[62],$immed.[63],lxor,xl[2]) with ET=L[1]
                       expr ($immed.[61],$immed.[62],lxor,xl[3]) with ET=L[1]
                       expr ($immed.[60],$immed.[61],lxor,xl[4]) with ET=L[1]
                       expr ($immed.[59],$immed.[60],lxor,xl[5]) with ET=L[1]
                       expr ($immed.[58],$immed.[59],lxor,xl[6]) with ET=L[1]
                       expr ($immed.[57],$immed.[58],lxor,xl[7]) with ET=L[1]
                       expr ($immed.[56],$immed.[57],lxor,xl[8]) with ET=L[1]
                       expr ($immed.[55],$immed.[56],lxor,xl[9]) with ET=L[1]
                       expr ($immed.[54],$immed.[55],lxor,xl[10]) with ET=L[1]
                       expr ($immed.[53],$immed.[54],lxor,xl[11]) with ET=L[1]
                       expr ($immed.[52],$immed.[53],lxor,xl[12]) with ET=L[1]
                       expr ($immed.[51],$immed.[52],lxor,xl[13]) with ET=L[1]
                       expr ($immed.[50],$immed.[51],lxor,xl[14]) with ET=L[1]
                       expr ($immed.[49],$immed.[50],lxor,xl[15]) with ET=L[1]
                       expr ($immed.[48],$immed.[49],lxor,xl[16]) with ET=L[1]
                       expr ($immed.[47],$immed.[48],lxor,xl[17]) with ET=L[1]
                       expr ($immed.[46],$immed.[47],lxor,xl[18]) with ET=L[1]
                       expr ($immed.[45],$immed.[46],lxor,xl[19]) with ET=L[1]
                       expr ($immed.[44],$immed.[45],lxor,xl[20]) with ET=L[1]
                       expr ($immed.[43],$immed.[44],lxor,xl[21]) with ET=L[1]
                       expr ($immed.[42],$immed.[43],lxor,xl[22]) with ET=L[1]
                       expr ($immed.[41],$immed.[42],lxor,xl[23]) with ET=L[1]
                       expr ($immed.[40],$immed.[41],lxor,xl[24]) with ET=L[1]
                       expr ($immed.[39],$immed.[40],lxor,xl[25]) with ET=L[1]
                       expr ($immed.[38],$immed.[39],lxor,xl[26]) with ET=L[1]
                       expr ($immed.[37],$immed.[38],lxor,xl[27]) with ET=L[1]
                       expr ($immed.[36],$immed.[37],lxor,xl[28]) with ET=L[1]
                       expr ($immed.[35],$immed.[36],lxor,xl[29]) with ET=L[1]
                       expr ($immed.[34],$immed.[35],lxor,xl[30]) with ET=L[1]
                       expr ($immed.[33],$immed.[34],lxor,xl[31]) with ET=L[1]
                       expr ($immed.[32],$immed.[33],lxor,xl[32]) with ET=L[1]
                       expr ($immed.[31],$immed.[32],lxor,xl[33]) with ET=L[1]
                       expr ($immed.[30],$immed.[31],lxor,xl[34]) with ET=L[1]
                       expr ($immed.[29],$immed.[30],lxor,xl[35]) with ET=L[1]
                       expr ($immed.[28],$immed.[29],lxor,xl[36]) with ET=L[1]
                       expr ($immed.[27],$immed.[28],lxor,xl[37]) with ET=L[1]
                       expr ($immed.[26],$immed.[27],lxor,xl[38]) with ET=L[1]
                       expr ($immed.[25],$immed.[26],lxor,xl[39]) with ET=L[1]
                       expr ($immed.[24],$immed.[25],lxor,xl[40]) with ET=L[1]
                       expr ($immed.[23],$immed.[24],lxor,xl[41]) with ET=L[1]
                       expr ($immed.[22],$immed.[23],lxor,xl[42]) with ET=L[1]
                       expr ($immed.[21],$immed.[22],lxor,xl[43]) with ET=L[1]
                       expr ($immed.[20],$immed.[21],lxor,xl[44]) with ET=L[1]
                       expr ($immed.[19],$immed.[20],lxor,xl[45]) with ET=L[1]
                       expr ($immed.[18],$immed.[19],lxor,xl[46]) with ET=L[1]
                       expr ($immed.[17],$immed.[18],lxor,xl[47]) with ET=L[1]
                       expr ($immed.[16],$immed.[17],lxor,xl[48]) with ET=L[1]
                       expr ($immed.[15],$immed.[16],lxor,xl[49]) with ET=L[1]
                       expr ($immed.[14],$immed.[15],lxor,xl[50]) with ET=L[1]
                       expr ($immed.[13],$immed.[14],lxor,xl[51]) with ET=L[1]
                       expr ($immed.[12],$immed.[13],lxor,xl[52]) with ET=L[1]
                       expr ($immed.[11],$immed.[12],lxor,xl[53]) with ET=L[1]
                       expr ($immed.[10],$immed.[11],lxor,xl[54]) with ET=L[1]
                       expr ($immed.[9],$immed.[10],lxor,xl[55]) with ET=L[1]
                       expr ($immed.[8],$immed.[9],lxor,xl[56]) with ET=L[1]
                       expr ($immed.[7],$immed.[8],lxor,xl[57]) with ET=L[1]
                       expr ($immed.[6],$immed.[7],lxor,xl[58]) with ET=L[1]
                       expr ($immed.[5],$immed.[6],lxor,xl[59]) with ET=L[1]
                       expr ($immed.[4],$immed.[5],lxor,xl[60]) with ET=L[1]
                       expr ($immed.[3],$immed.[4],lxor,xl[61]) with ET=L[1]
                       expr ($immed.[2],$immed.[3],lxor,xl[62]) with ET=L[1]
                       expr (pl,$immed.[2],lxor,xl[63]) with ET=L[1]
                       nop
        i3_assign_end: 
end
