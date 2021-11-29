#
# Tool Interface Tool xilinx6.p
# Version 2.05
#
TOP="MOD_p"
DEVICE="xc2s100-tq144"
SPEED="5"
PROM="xc18v01"
OBJDIR="obj"
SRCDIR="."
DESIGN="p"
LOG="p.log"
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
VHDL[1]="p"
VHDL[2]="p_p1"
VHDL[3]="p_main"
SYNSET="-fsm_extract YES -fsm_encoding Sequential -fsm_style lut -mux_extract YES -mux_style Auto -ram_extract Yes -ram_style Block -rom_extract Yes -rom_style Auto -mult_style lut -priority_extract Yes -shreg_extract Yes -decoder_extract Yes -shift_extract Yes -opt_mode speed -opt_level 1 -iobuf YES -iob Auto -max_fanout 100 -xor_collapse Yes -resource_sharing Yes -slice_packing Yes -slice_utilization_ratio 100 -slice_utilization_ratio_maxmargin 5 -bufg 4 -register_duplication Yes -register_balancing No -equivalent_register_removal Yes -optimize_primitives No -tristate2logic No -glob_opt AllClockNets -iuc NO "
MAPSET="-pr b -k 4 -c 100 -tx off "
PARSET="-t 1 "
BITGEN_CONF=""
RECLEN="1024"
do_fpga()
{
  echo [creating xsvf fpga file...]
  rm -f  "p_xc2s100-tq144.xsvf" "p_xc2s100-tq144.svf" "p_xc2s100-tq144.bit"
  CMD="impact.cmd"
  rm -f $CMD; touch $CMD
  echo "setMode -bsfile" >> $CMD
  echo "setCable -port svf -file p_xc2s100-tq144.svf" >> $CMD
  echo "addDevice -p 1 -file p.bit" >> $CMD
  echo "identify" >> $CMD
  echo "program -e -p 1" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  rm -f $CMD; touch $CMD
  echo "svf2xsvf -fpga -r 0 -rlen 1024 -i p_xc2s100-tq144.svf -o p_xc2s100-tq144.xsvf" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cp "p_xc2s100-tq144.xsvf" "../."
}
do_prom()
{
  echo [creating xsvf prom file...]
  rm -f  "p_xc18v01.xsvf" "p_xc18v01.svf"
  CMD="impact.cmd"
  rm -f $CMD; touch $CMD
  echo "setMode -pff" >> $CMD
  echo "setsubMode -pffserial" >> $CMD
  echo "addpromdevice -p 1 -name xc18v01" >> $CMD
  echo "addCollection -name p" >> $CMD
  echo "addDesign -version 0 -name 0000" >> $CMD
  echo "addDeviceChain -index 0" >> $CMD
  echo "addDevice -p 1 -file p.bit" >> $CMD
  echo "generate" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  rm -f $CMD; touch $CMD
  echo "setMode -bsfile" >> $CMD
  echo "setCable -port svf -file p_xc18v01.svf" >> $CMD
  echo "addDevice -p 1 -part xc18v01" >> $CMD
  echo "setAttribute -position 1 -attr configFileName -value p.mcs" >> $CMD
  echo "identify" >> $CMD
  echo "program -e -p 1 -v" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  rm -f $CMD; touch $CMD
  echo "svf2xsvf  -i p_xc18v01.svf -o p_xc18v01.xsvf" >> $CMD
  echo "quit" >> $CMD
  EXEC="impact -batch "$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cp "p_xc18v01.xsvf" "../."
}
do_ucf()
{
  echo [creating ucf file...]
  UCF="../p.ucf"
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
    echo "#NET \"top_p_io1<0>\" LOC = \"\";
    #NET \"top_p_io1<1>\" LOC = \"\";
    #NET \"top_p_io1<2>\" LOC = \"\";
    #NET \"top_p_io1<3>\" LOC = \"\";
    #NET \"top_p_io1<4>\" LOC = \"\";
    #NET \"top_p_io1<5>\" LOC = \"\";
    #NET \"top_p_io1<6>\" LOC = \"\";
    #NET \"top_p_io1<7>\" LOC = \"\";" >> $UCF
    echo "#NET \"top_p_io2<0>\" LOC = \"\";
    #NET \"top_p_io2<1>\" LOC = \"\";
    #NET \"top_p_io2<2>\" LOC = \"\";
    #NET \"top_p_io2<3>\" LOC = \"\";
    #NET \"top_p_io2<4>\" LOC = \"\";
    #NET \"top_p_io2<5>\" LOC = \"\";
    #NET \"top_p_io2<6>\" LOC = \"\";
    #NET \"top_p_io2<7>\" LOC = \"\";" >> $UCF
    echo "#NET \"CLK\" LOC = \"\";" >> $UCF
    echo "#NET \"RESET\" LOC = \"\";" >> $UCF
  fi
}
do_bitgen()
{
  echo [starting bitgen...]
  BITCONF="p.bitgen_conf"
  rm -f $BITCONF; touch $BITCONF
  echo "" >> $BITCONF
  EXEC="bitgen -w -f "$BITCONF" p.ncd"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cp "p.bit" "../."
}
do_place()
{
  echo [starting place and route...]
  EXEC="par -w "$PARSET" p_map.ncd p.ncd p.pcf"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  cat "p.par" >> "p.log"
}
do_map()
{
  echo [starting map...]
  EXEC="map -p xc2s100-tq144 "$MAPSET" -o p_map.ncd p.ngd p.pcf"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  cat "p_map.mrp" >> "p.log"
}
do_trans()
{
  echo [starting ngdbuild...]
  EXEC="ngdbuild -p xc2s100-tq144 -uc ../p.ucf -dd . p.ngc p.ngd"
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
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
    echo $DATE >> "p.log"
    echo $DATE
    echo [$SCRAM" "$SRC"]" >> "p.log"
    echo [$SCRAM" "$SRC"]"
    _TIME=`gdate +%s`
    EXEC=$SCRAM" "$SRC
    { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "p.log"
    echo $_TIME >> "p.log"
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
  EXEC="xst -ifn p.xst_run -ofn p_xilinx.log"
  $EXEC
  _EXEC_RES=$?
  res=$_EXEC_RES
  check
  cat "p_xilinx.log" >> "p.log"
}
make_proj()
{
  PROJFILE="p.prj"
  rm -f $PROJFILE; touch $PROJFILE
  _DIRNAME1="p"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="p"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="p_p1"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="p_p1"
  FILE=${_BASENAME##*/}
  if [ $DIR = "." ]
  then
    echo "vhdl work ../"$FILE".vhdl" >> $PROJFILE
  else
    echo "vhdl work "$DIR"/"$FILE".vhdl" >> $PROJFILE
  fi
  _DIRNAME1="p_main"
  _DIRNAME2=${_DIRNAME1%/*}
  if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
  then
  _DIRNAME2="."
  fi
  DIR=$_DIRNAME2
  _BASENAME="p_main"
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
  rm -f "p.xst_run"; touch "p.xst_run"
  CMD="run -ifn p.prj -ifmt mixed -ofn p -ofmt NGC -p xc2s100-tq144 "$SYNSET" -top MOD_p"
  echo $CMD >> "p.xst_run"
}
finish()
{
  cat "p.log" >> "../p.log"
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
  rm -f "p.log"; touch "p.log"
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
  echo "Usage: p.xilinx6.tool.sh [init vhdl synth tech svf ucf]"
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
