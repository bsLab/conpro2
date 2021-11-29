#
# Tool Interface Tool xilinx6.t
# Version 2.04
#
TOP="MOD_t"
DEVICE="xc2s100-tq144-5"
SPEED="5"
PROM="xc18v01"
OBJDIR="obj"
SRCDIR="."
DESIGN="t"
LOG="t.log"
DUP="tee"
if [ "X$XILINX" = "X" ]
then
  _XILINX="/opt/Xilinx9"
else
  _XILINX=$XILINX
fi
XILINX=$_XILINX
if [ "X$SCRAM" = "X" ]
then
  _SCRAM="/opt/alliance/bin/scram"
else
  _SCRAM=$SCRAM
fi
SCRAM=$_SCRAM
res="0"
VHDL[1]="t"
VHDL[2]="t_p_0"
VHDL[3]="t_p_1"
VHDL[4]="t_p_2"
VHDL[5]="t_p_3"
VHDL[6]="t_main"
SYNSET="-fsm_extract YES -fsm_encoding Sequential -fsm_style lut -mux_extract YES -mux_style Auto -ram_extract Yes -ram_style Block -rom_extract Yes -rom_style Auto -mult_style lut -priority_extract Yes -shreg_extract Yes -decoder_extract Yes -shift_extract Yes -opt_mode speed -opt_level 1 -iobuf YES -iob Auto -max_fanout 100 -xor_collapse Yes -resource_sharing Yes -slice_packing Yes -slice_utilization_ratio 100 -slice_utilization_ratio_maxmargin 5 -bufg 4 -register_duplication Yes -register_balancing No -equivalent_register_removal Yes -optimize_primitives No -tristate2logic No -glob_opt AllClockNets -iuc NO "
MAPSET="-pr b -k 4 -c 100 -tx off "
PARSET="-t 1 "
BITGEN_CONF="-g Gclkdel0:11111 -g Gclkdel1:11111 -g Gclkdel2:11111 -g Gclkdel3:11111 -g ConfigRate:4 -g DebugBitstream:No -g Binary:no -g CclkPin:PullUp -g M0Pin:PullUp -g M1Pin:PullUp -g M2Pin:PullUp -g ProgPin:PullUp -g DonePin:PullUp -g TckPin:PullUp -g TdiPin:PullUp -g TdoPin:PullUp -g TmsPin:PullUp -g UnusedPin:PullDown -g UserID:0xFFFFFFFF -g StartUpClk:CClk -g DONE_cycle:4"
RECLEN="1024"
do_fpga()
{
  echo "[creating xsvf fpga file...]"
  rm -f  "t_xc2s100-tq144-5.xsvf" "t_xc2s100-tq144-5.svf" "t_xc2s100-tq144-5.bit"
  CMD="impact.cmd"
  rm -f $CMD; touch $CMD
  echo "setMode -bsfile" >> $CMD
  echo "setCable -port svf -file t_xc2s100-tq144-5.svf" >> $CMD
  echo "addDevice -p 1 -file t.bit" >> $CMD
  echo "identify" >> $CMD
  echo "program -e -p 1" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  rm -f $CMD; touch $CMD
  echo "svf2xsvf -fpga -r 0 -rlen 1024 -i t_xc2s100-tq144-5.svf -o t_xc2s100-tq144-5.xsvf" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cp "t_xc2s100-tq144-5.xsvf" "../."
}
do_prom()
{
  echo "[creating xsvf prom file...]"
  rm -f  "t_xc18v01.xsvf" "t_xc18v01.svf"
  CMD="impact.cmd"
  rm -f $CMD; touch $CMD
  echo "setMode -pff" >> $CMD
  echo "setsubMode -pffserial" >> $CMD
  echo "addpromdevice -p 1 -name xc18v01" >> $CMD
  echo "addCollection -name t" >> $CMD
  echo "addDesign -version 0 -name 0000" >> $CMD
  echo "addDeviceChain -index 0" >> $CMD
  echo "addDevice -p 1 -file t.bit" >> $CMD
  echo "generate" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  rm -f $CMD; touch $CMD
  echo "setMode -bsfile" >> $CMD
  echo "setCable -port svf -file t_xc18v01.svf" >> $CMD
  echo "addDevice -p 1 -part xc18v01" >> $CMD
  echo "setAttribute -position 1 -attr configFileName -value t.mcs" >> $CMD
  echo "identify" >> $CMD
  echo "program -e -p 1 -v" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  rm -f $CMD; touch $CMD
  echo "svf2xsvf  -i t_xc18v01.svf -o t_xc18v01.xsvf" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cp "t_xc18v01.xsvf" "../."
}
do_ucf()
{
  echo "[creating ucf file...]"
  UCF="../t.ucf"
  [ -f $UCF ]
  if [ $? = 0 ]
  then
    _RES=1
  else
    _RES=0
  fi
  if [ $_RES = "1" ]
  then
    echo "Keeping existing constraint file "$UCF
  else
    rm -f $UCF; touch $UCF
    IND="1"
    echo "#NET \"d_RD<0>\" LOC = \"\";
    #NET \"d_RD<1>\" LOC = \"\";
    #NET \"d_RD<2>\" LOC = \"\";
    #NET \"d_RD<3>\" LOC = \"\";" >> $UCF
    echo "#NET \"CLK\" LOC = \"\";" >> $UCF
    echo "#NET \"RESET\" LOC = \"\";" >> $UCF
  fi
}
do_bitgen()
{
  echo "[starting bitgen...]"
  BITCONF="t.bitgen_conf"
  rm -f $BITCONF; touch $BITCONF
  echo "-g Gclkdel0:11111 -g Gclkdel1:11111 -g Gclkdel2:11111 -g Gclkdel3:11111 -g ConfigRate:4 -g DebugBitstream:No -g Binary:no -g CclkPin:PullUp -g M0Pin:PullUp -g M1Pin:PullUp -g M2Pin:PullUp -g ProgPin:PullUp -g DonePin:PullUp -g TckPin:PullUp -g TdiPin:PullUp -g TdoPin:PullUp -g TmsPin:PullUp -g UnusedPin:PullDown -g UserID:0xFFFFFFFF -g StartUpClk:CClk -g DONE_cycle:4" >> $BITCONF
  EXEC="bitgen -w -f "$BITCONF" t.ncd"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cp "t.bit" "../."
}
do_place()
{
  echo "[starting place and route...]"
  EXEC="par -w "$PARSET" t_map.ncd t.ncd t.pcf"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cat "t.par" >> "t.log"
}
do_map()
{
  echo "[starting map...]"
  EXEC="map -p xc2s100-tq144-5 "$MAPSET" -o t_map.ncd t.ngd t.pcf"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  cat "t_map.mrp" >> "t.log"
}
do_trans()
{
  echo "[starting ngdbuild...]"
  EXEC="ngdbuild -p xc2s100-tq144-5 -uc ../t.ucf -dd . t.ngc t.ngd"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
}
do_scram()
{
  for file in ${VHDL[@]}
  do
    rm -f  "work._savant_lib"
    $_BASENAME=$file
    SRC=${_BASENAME##*/}".vhdl"
    _DATE=`date`
    DATE=$_DATE
    echo $DATE >> "t.log"
    echo $DATE
    echo "["$SCRAM" "$SRC"]" >> "t.log"
    echo "["$SCRAM" "$SRC"]"
    _TIME=`gdate +%s`
    EXEC=$SCRAM" "$SRC
    { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
    echo $_TIME >> "t.log"
    _EXEC_RES=`cat /tmp/_EXEC_RES`
    rm -f /tmp/_EXEC_RES
    res=$_EXEC_RES
    rm -f  "work._savant_lib"
  done
}
do_synth()
{
  echo "[building run and project scripts...]"
  make_xst_runscript
  check
  make_proj
  check
  echo "[starting xst...]"
  EXEC="xst -ifn t.xst_run -ofn t_xilinx.log"
  $EXEC
  _EXEC_RES=$?
  res=$_EXEC_RES
  check
  cat "t_xilinx.log" >> "t.log"
}
make_proj()
{
  PROJFILE="t.prj"
  rm -f $PROJFILE; touch $PROJFILE
  DIR="."
  FILE="t"
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  DIR="."
  FILE="t_p_0"
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  DIR="."
  FILE="t_p_1"
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  DIR="."
  FILE="t_p_2"
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  DIR="."
  FILE="t_p_3"
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  DIR="."
  FILE="t_main"
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
}
make_xst_runscript()
{
  rm -f "t.xst_run"; touch "t.xst_run"
  CMD="run -ifn t.prj -ifmt mixed -ofn t -ofmt NGC -p xc2s100-tq144-5 "$SYNSET" -top MOD_t"
  echo $CMD >> "t.xst_run"
}
finish()
{
  cat "t.log" >> "../t.log"
  cd "-"
}
init()
{
  cd "obj"
  rm -f "t.log"; touch "t.log"
}
check()
{
  if [ ! $res = "0" ]
  then
    echo "[Abort due to errors building target.]"
    exit "1"
  fi
}
if [ $# = 0 ]
then
  echo "Usage: t.xilinx6.tool.sh [init vhdl synth tech svf ucf]"
fi
for TARGET in $@ 
do
  echo "Building target [$TARGET] ..."
  case $TARGET in
    init)
      init
      check
      finish
      ;;
    vhdl)
      init
      do_scram
      check
      finish
      ;;
    synth)
      init
      do_synth
      check
      finish
      ;;
    tech)
      init
      do_ucf
      check
      do_trans
      check
      do_map
      check
      do_place
      check
      do_bitgen
      check
      do_prom
      check
      do_fpga
      check
      finish
      ;;
    svf)
      init
      do_prom
      check
      do_fpga
      check
      finish
      ;;
    ucf)
      init
      do_ucf
      check
      ;;
  esac
done
