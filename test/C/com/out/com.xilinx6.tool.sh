#
# Tool Interface Tool xilinx6.com
# Version 2.05
#
TOP="MOD_com"
DEVICE="xc3s1000-ft256-5"
SPEED="5"
PROM="xc18v04"
OBJDIR="obj"
SRCDIR="."
DESIGN="com"
LOG="com.log"
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
VHDL[1]="com"
VHDL[2]="com_sys_status_proc"
VHDL[3]="com_com_tmo"
VHDL[4]="com_com_rx"
VHDL[5]="com_com_tx"
VHDL[6]="com_interpreter"
VHDL[7]="com_FUN_request"
VHDL[8]="com_main"
VHDL[9]="conpro"
SYNSET="-fsm_extract YES -fsm_encoding Sequential -fsm_style lut -mux_extract YES -mux_style Auto -ram_extract Yes -ram_style Block -rom_extract Yes -rom_style Auto -mult_style lut -priority_extract Yes -shreg_extract Yes -decoder_extract Yes -shift_extract Yes -opt_mode speed -opt_level 1 -iobuf YES -iob Auto -max_fanout 100 -xor_collapse Yes -resource_sharing Yes -slice_packing Yes -slice_utilization_ratio 100 -slice_utilization_ratio_maxmargin 5 -bufg 4 -register_duplication Yes -register_balancing No -equivalent_register_removal Yes -optimize_primitives No -tristate2logic No -glob_opt AllClockNets -iuc NO "
MAPSET="-pr b -k 4 -c 100 -tx off "
PARSET="-t 1 "
BITGEN_CONF="-g DebugBitstream:No -g Binary:no -g CclkPin:PullUp -g M0Pin:PullUp -g M1Pin:PullUp -g M2Pin:PullUp -g ProgPin:PullUp -g DonePin:PullUp -g TckPin:PullUp -g TdiPin:PullUp -g TdoPin:PullUp -g TmsPin:PullUp -g UnusedPin:PullDown -g UserID:0xFFFFFFFF -g StartUpClk:CClk -g DONE_cycle:4"
RECLEN="1024"
do_fpga()
{
  echo [creating xsvf fpga file...]
  rm -f  "com_xc3s1000-ft256-5.xsvf" "com_xc3s1000-ft256-5.svf" "com_xc3s1000-ft256-5.bit"
  CMD="impact.cmd"
  rm -f $CMD; touch $CMD
  echo "setMode -bsfile" >> $CMD
  echo "setCable -port svf -file com_xc3s1000-ft256-5.svf" >> $CMD
  echo "addDevice -p 1 -file com.bit" >> $CMD
  echo "identify" >> $CMD
  echo "program -e -p 1" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  rm -f $CMD; touch $CMD
  echo "svf2xsvf -fpga -r 0 -rlen 1024 -i com_xc3s1000-ft256-5.svf -o com_xc3s1000-ft256-5.xsvf" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cp "com_xc3s1000-ft256-5.xsvf" "../."
}
do_prom()
{
  echo [creating xsvf prom file...]
  rm -f  "com_xc18v04.xsvf" "com_xc18v04.svf"
  CMD="impact.cmd"
  rm -f $CMD; touch $CMD
  echo "setMode -pff" >> $CMD
  echo "setsubMode -pffserial" >> $CMD
  echo "addpromdevice -p 1 -name xc18v04" >> $CMD
  echo "addCollection -name com" >> $CMD
  echo "addDesign -version 0 -name 0000" >> $CMD
  echo "addDeviceChain -index 0" >> $CMD
  echo "addDevice -p 1 -file com.bit" >> $CMD
  echo "generate" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  rm -f $CMD; touch $CMD
  echo "setMode -bsfile" >> $CMD
  echo "setCable -port svf -file com_xc18v04.svf" >> $CMD
  echo "addDevice -p 1 -part xc18v04" >> $CMD
  echo "setAttribute -position 1 -attr configFileName -value com.mcs" >> $CMD
  echo "identify" >> $CMD
  echo "program -e -p 1 -v" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  rm -f $CMD; touch $CMD
  echo "svf2xsvf  -i com_xc18v04.svf -o com_xc18v04.xsvf" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cp "com_xc18v04.xsvf" "../."
}
do_ucf()
{
  echo [creating ucf file...]
  UCF="../com.ucf"
  [ -r $UCF ]
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
    echo "#NET \"stat_led_RD\" LOC = \"\";" >> $UCF
    echo "#NET \"dev_io_RX\" LOC = \"\";" >> $UCF
    echo "#NET \"dev_io_TX\" LOC = \"\";" >> $UCF
    echo "#NET \"CLK\" LOC = \"\";" >> $UCF
    echo "#NET \"RESET\" LOC = \"\";" >> $UCF
  fi
}
do_bitgen()
{
  echo [starting bitgen...]
  BITCONF="com.bitgen_conf"
  rm -f $BITCONF; touch $BITCONF
  echo "-g DebugBitstream:No -g Binary:no -g CclkPin:PullUp -g M0Pin:PullUp -g M1Pin:PullUp -g M2Pin:PullUp -g ProgPin:PullUp -g DonePin:PullUp -g TckPin:PullUp -g TdiPin:PullUp -g TdoPin:PullUp -g TmsPin:PullUp -g UnusedPin:PullDown -g UserID:0xFFFFFFFF -g StartUpClk:CClk -g DONE_cycle:4" >> $BITCONF
  EXEC="bitgen -w -f "$BITCONF" com.ncd"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cp "com.bit" "../."
}
do_place()
{
  echo [starting place and route...]
  EXEC="par -w "$PARSET" com_map.ncd com.ncd com.pcf"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cat "com.par" >> "com.log"
}
do_map()
{
  echo [starting map...]
  EXEC="map -p xc3s1000-ft256-5 "$MAPSET" -o com_map.ncd com.ngd com.pcf"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  cat "com_map.mrp" >> "com.log"
}
do_trans()
{
  echo [starting ngdbuild...]
  EXEC="ngdbuild -p xc3s1000-ft256-5 -uc ../com.ucf -dd . com.ngc com.ngd"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
}
do_scram()
{
  for file in "${VHDL[@]}"
  do
    rm -f  "work._savant_lib"
    _BASENAME=$file
    SRC=${_BASENAME##*/}".vhdl"
    _DATE=`date`
    DATE=$_DATE
    echo $DATE >> "com.log"
    echo $DATE
    echo [$SCRAM" "$SRC"]" >> "com.log"
    echo [$SCRAM" "$SRC"]"
    _TIME=`gdate +%s`
    EXEC=$SCRAM" "$SRC
    { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "com.log"
    echo $_TIME >> "com.log"
    _EXEC_RES=`cat /tmp/_EXEC_RES`
    rm -f /tmp/_EXEC_RES
    res=$_EXEC_RES
    rm -f  "work._savant_lib"
  done
}
do_synth()
{
  echo [building run and project scripts...]
  make_xst_runscript
  check
  make_proj
  check
  echo [starting xst...]
  EXEC="xst -ifn com.xst_run -ofn com_xilinx.log"
  $EXEC
  _EXEC_RES=$?
  res=$_EXEC_RES
  check
  cat "com_xilinx.log" >> "com.log"
}
make_proj()
{
  PROJFILE="com.prj"
  rm -f $PROJFILE; touch $PROJFILE
  _DIRNAME1="com"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="com"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="com_sys_status_proc"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="com_sys_status_proc"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="com_com_tmo"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="com_com_tmo"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="com_com_rx"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="com_com_rx"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="com_com_tx"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="com_com_tx"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="com_interpreter"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="com_interpreter"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="com_FUN_request"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="com_FUN_request"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="com_main"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="com_main"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="conpro"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="conpro"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
}
make_xst_runscript()
{
  rm -f "com.xst_run"; touch "com.xst_run"
  CMD="run -ifn com.prj -ifmt mixed -ofn com -ofmt NGC -p xc3s1000-ft256-5 "$SYNSET" -top MOD_com"
  echo $CMD >> "com.xst_run"
}
finish()
{
  cat "com.log" >> "../com.log"
  cd "-"
}
init()
{
  [ -r "obj" ]
  if [ $? = 0 ]
  then
    _RES=1
  else
    _RES=0
  fi
  if [ $_RES = "0" ]
  then
    mkdir -p "obj"
  fi
  cd "obj"
  rm -f "com.log"; touch "com.log"
}
check()
{
  if [ ! $res = "0" ]
  then
    echo [Abort due to errors building target.]
    exit "1"
  fi
}
if [ $# = 0 ]
then
  echo "Usage: com.xilinx6.tool.sh [init vhdl synth tech svf ucf]"
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
