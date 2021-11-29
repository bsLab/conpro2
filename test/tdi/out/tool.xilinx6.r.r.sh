SRCDIR="."
SHREG_EXTRACT="Yes"
FSM_ENCODING="Sequential"
res=0
SPEED=""
REG_BAL="No"
MUX_EXTRACT="YES"
REG_DUPL="Yes"
OPT_PRIM="No"
PACK_REG="b"
SHIFT_EXTRACT="Yes"
TRI_2_LOGIC="No"
MUX_STYLE="Auto"
PLACE_COST="1"
TOP="MOD_r"
MULT_STYLE="lut"
ROM_STYLE="Auto"
RAM_EXTRACT="Yes"
LOG="MOD_r.log"
proj="r"
DECODER_EXTRACT="Yes"
PACK_CLB="100"
BUFG="4"
prom=$target2
RECLEN="1024"
SLICE_RATIO="100"
DEVICE=$target1
IOB="Auto"
XILINX="$TEMP298648"
BITGEN_CONF_COM="-g DebugBitstream:No -g Binary:no -g CclkPin:PullUp -g M0Pin:PullUp -g M1Pin:PullUp -g M2Pin:PullUp -g ProgPin:PullUp -g DonePin:PullUp -g TckPin:PullUp -g TdiPin:PullUp -g TdoPin:PullUp -g TmsPin:PullUp -g UnusedPin:PullDown -g UserID:0xFFFFFFFF -g StartUpClk:CClk -g DONE_cycle:4"
ROM_EXTRACT="Yes"
OPT="speed"
DESIGN="r"
MAP_IN_FUN="4"
PROM=$target2
IUC="NO"
TRANS_BUSES="off"
RES_SHARE="Yes"
device=$target1
GLOBOPT="AllClockNets"
REG_EQ_REM="Yes"
DUP="tee"
speed=""
XOR_COLLAPSE="Yes"
IOBUF="YES"
BITGEN_CONF_XC2="-g Gclkdel0:11111 -g Gclkdel1:11111 -g Gclkdel2:11111 -g Gclkdel3:11111 -g ConfigRate:4"
RAM_STYLE="Block"
SCRAM="$TEMP161374"
PAR_OPT="std"
vhdl1="r"
vhdl2="r_p1"
vhdl3="r_main"
MAX_FANOUT="100"
OPTLEV="1"
FSM_STYLE="lut"
FSM_EXTRACT="YES"
OBJDIR="obj"
SLICE_PACK="Yes"
PRIO_EXTRACT="Yes"
COVER_MODE="area"
SLICE_MAXMAR="5"
TOP="MOD_r"
DEVICE=$target1
SPEED=""
PROM=$target2
OBJDIR="obj"
SRCDIR="."
DESIGN="r"
LOG="MOD_r.log"
DUP="tee"
if [ "X"$XILINX"" = "X" ]
then
  TEMP298648="/opt/Xilinx9"
else
  TEMP298648="$XILINX"
fi
if [ "X"$XILINX"" = "X" ]
then
  TEMP872721="/opt/Xilinx9"
else
  TEMP872721="$XILINX"
fi
XILINX=$TEMP872721
if [ "X"$SCRAM"" = "X" ]
then
  TEMP161374="/opt/alliance/bin/scram"
else
  TEMP161374="$SCRAM"
fi
if [ "X"$SCRAM"" = "X" ]
then
  TEMP205427="/opt/alliance/bin/scram"
else
  TEMP205427="$SCRAM"
fi
SCRAM=$TEMP205427
res=0
VHDL[1]="r"
VHDL[2]="r_p1"
VHDL[3]="r_main"
FSM_EXTRACT="YES"
FSM_ENCODING="Sequential"
FSM_STYLE="lut"
MUX_EXTRACT="YES"
MUX_STYLE="Auto"
RAM_EXTRACT="Yes"
RAM_STYLE="Block"
ROM_EXTRACT="Yes"
ROM_STYLE="Auto"
MULT_STYLE="lut"
PRIO_EXTRACT="Yes"
SHREG_EXTRACT="Yes"
DECODER_EXTRACT="Yes"
SHIFT_EXTRACT="Yes"
OPT="speed"
OPTLEV="1"
IOBUF="YES"
IOB="Auto"
MAX_FANOUT="100"
COVER_MODE="area"
PACK_REG="b"
MAP_IN_FUN="4"
PACK_CLB="100"
TRANS_BUSES="off"
PAR_OPT="std"
PLACE_COST="1"
IUC="NO"
GLOBOPT="AllClockNets"
XOR_COLLAPSE="Yes"
RES_SHARE="Yes"
BUFG="4"
OPT_PRIM="No"
TRI_2_LOGIC="No"
SLICE_RATIO="100"
SLICE_PACK="Yes"
SLICE_MAXMAR="5"
REG_DUPL="Yes"
REG_BAL="No"
REG_EQ_REM="Yes"
RECLEN="1024"
BITGEN_CONF_XC2="-g Gclkdel0:11111 -g Gclkdel1:11111 -g Gclkdel2:11111 -g Gclkdel3:11111 -g ConfigRate:4"
BITGEN_CONF_COM="-g DebugBitstream:No -g Binary:no -g CclkPin:PullUp -g M0Pin:PullUp -g M1Pin:PullUp -g M2Pin:PullUp -g ProgPin:PullUp -g DonePin:PullUp -g TckPin:PullUp -g TdiPin:PullUp -g TdoPin:PullUp -g TmsPin:PullUp -g UnusedPin:PullDown -g UserID:0xFFFFFFFF -g StartUpClk:CClk -g DONE_cycle:4"
if [ "$target1" = "xc2" ]
then
  BITGEN_CONF="-g DebugBitstream:No -g Binary:no -g CclkPin:PullUp -g M0Pin:PullUp -g M1Pin:PullUp -g M2Pin:PullUp -g ProgPin:PullUp -g DonePin:PullUp -g TckPin:PullUp -g TdiPin:PullUp -g TdoPin:PullUp -g TmsPin:PullUp -g UnusedPin:PullDown -g UserID:0xFFFFFFFF -g StartUpClk:CClk -g DONE_cycle:4-g Gclkdel0:11111 -g Gclkdel1:11111 -g Gclkdel2:11111 -g Gclkdel3:11111 -g ConfigRate:4"
else
  BITGEN_CONF="-g DebugBitstream:No -g Binary:no -g CclkPin:PullUp -g M0Pin:PullUp -g M1Pin:PullUp -g M2Pin:PullUp -g ProgPin:PullUp -g DonePin:PullUp -g TckPin:PullUp -g TdiPin:PullUp -g TdoPin:PullUp -g TmsPin:PullUp -g UnusedPin:PullDown -g UserID:0xFFFFFFFF -g StartUpClk:CClk -g DONE_cycle:4"
fi
check()
{
  if [ ! 0 = 0 ]
  then
  echo ""[Abort due to errors building target.]""
  exit 1
  fi
}
init()
{
  echo "[Opening file <MOD_r.log>...]"
}
make_xst_runscript()
{
  echo > "r.xst_run"
  CMD="run -ifn r.prj -ifmt mixed -ofn r -ofmt NGC -p "$target1" -opt_mode speed -opt_level 1 -iobuf YES -iob Auto -fsm_extract YES -fsm_encoding Sequential -fsm_style lut -mux_extract YES -mux_style Auto -ram_extract Yes -ram_style Block -rom_extract Yes -rom_style Block -decoder_extract Yes -priority_extract Yes -shreg_extract Yes -shift_extract Yes -xor_collapse Yes -mult_style lut -resource_sharing Yes -slice_utilization_ratio 100 -slice_utilization_ratio_maxmargin 5 -slice_packing Yes -max_fanout 100 -bufg 4 -register_duplication Yes -register_balancing No -equivalent_register_removal Yes -optimize_primitives No -tristate2logic No -top MOD_r -iuc NO -glob_opt AllClockNets"
  echo $CMD >> "r.xst_run"
}
make_proj()
{
  PROJFILE="r.prj"
  echo > $PROJFILE
  SRC="r.vhdl"
  echo "vhdl work"$SRC >> $PROJFILE
  SRC="r_p1.vhdl"
  echo "vhdl work"$SRC >> $PROJFILE
  SRC="r_main.vhdl"
  echo "vhdl work"$SRC >> $PROJFILE
}
do_synth()
{
  echo ""[building run and project scripts...]""
  make_xst_runscript
  check
  make_proj
  check
  echo ""[starting xst...]""
  "xst -ifn r.xst_run -ofn $DESIGN_xilinx.log"
  $TEMP63574=$?
  res=$TEMP63574
  check
  cat "r_xilinx.log" >> "r.log"
}
do_finish()
{
  cd "-"
}
do_scram()
{
  rm -f "work._savant_lib"
  SRC="r.vhdl"
  TEMP847348=`gdate +%s`
  DATE=$TEMP847348
  echo $DATE >> "MOD_r.log"
  echo "$DATE"
  echo "[$TEMP161374 "$SRC"]" >> "MOD_r.log"
  echo ""[$TEMP161374 "$SRC"]""
  TEMP516077=`date`
  "$TEMP161374 "$SRC 2>&1 >> "MOD_r.log"
  echo $TEMP516077 >> "MOD_r.log"
  $TEMP911607=$?
  res=$TEMP911607
  rm -f "work._savant_lib"
  rm -f "work._savant_lib"
  SRC="r_p1.vhdl"
  TEMP142151=`gdate +%s`
  DATE=$TEMP142151
  echo $DATE >> "MOD_r.log"
  echo "$DATE"
  echo "[$TEMP161374 "$SRC"]" >> "MOD_r.log"
  echo ""[$TEMP161374 "$SRC"]""
  TEMP143252=`date`
  "$TEMP161374 "$SRC 2>&1 >> "MOD_r.log"
  echo $TEMP143252 >> "MOD_r.log"
  $TEMP741425=$?
  res=$TEMP741425
  rm -f "work._savant_lib"
  rm -f "work._savant_lib"
  SRC="r_main.vhdl"
  TEMP758270=`gdate +%s`
  DATE=$TEMP758270
  echo $DATE >> "MOD_r.log"
  echo "$DATE"
  echo "[$TEMP161374 "$SRC"]" >> "MOD_r.log"
  echo ""[$TEMP161374 "$SRC"]""
  TEMP743856=`date`
  "$TEMP161374 "$SRC 2>&1 >> "MOD_r.log"
  echo $TEMP743856 >> "MOD_r.log"
  $TEMP199481=$?
  res=$TEMP199481
  rm -f "work._savant_lib"
}
do_trans()
{
  echo ""[starting ngdbuild...]""
  "ngdbuild -p "$target1" -uc ../r.ucf -dd . r.ngc r.ngd" 2>&1 >> "MOD_r.log"
  $TEMP834847=$?
  res=$TEMP834847
  check
}
do_map()
{
  echo ""[starting map...]""
  "map -p "$target1" -cm area -pr b-k 4-c 100-tx off-o r_map.ncdr.ngdr.pcf" 2>&1 >> "MOD_r.log"
  $TEMP257915=$?
  res=$TEMP257915
  check
  cat "r.rpt" >> "MOD_r.log"
}
do_place()
{
  echo ""[starting bitgen...]""
  BITCONF="r.bitgen_conf"
  echo > $BITCONF
  echo echo $BITGEN_CONF >> $BITCONF
  "bitgen -w -f "$BITCONF" r.ncd" 2>&1 >> "MOD_r.log"
  $TEMP945461=$?
  res=$TEMP945461
  cp "r.bit" "../."
}
if [ "$@X" = "X" ]
then
  echo "Usage: tool.xilinx6.r.r.sh [init vhdl synth]"
fi
for TARGET in $@ 
do
  echo "Building target [$TARGET] ..."
  case $TARGET in
    init)
      init
      check
      ;;
    vhdl)
      init
      cd "obj"
      do_scram
      check
      ;;
    synth)
      init
      cd "obj"
      do_synth
      check
      ;;
  esac
done
