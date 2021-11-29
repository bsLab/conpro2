#
# Tool Interface Tool leonardo.l2
# Version 2.06
#
TOP="l2"
OBJDIR="obj"
SRCDIR="."
DESIGN="l2"
LOG="l2.log"
DUP="tee"
if [ "X$PWD" = "X" ]
then
  _PWD="/"
else
  _PWD=$PWD
fi
PWD=$_PWD
if [ "X$ALLIANCE_TOP" = "X" ]
then
  _ALLIANCE_TOP="/opt/alliance-5.0"
else
  _ALLIANCE_TOP=$ALLIANCE_TOP
fi
ALLIANCE_TOP=$_ALLIANCE_TOP
BIN=$ALLIANCE_TOP"/bin"
CELLS=$ALLIANCE_TOP"/cells"
MBK_CATA_LIB=$CELLS"/sxlib"
SCRAM=$BIN"/scram"
VHD2=$BIN"/vhd2vst"
SXLIB_COMP=$CELLS"/sxlib/sxlib_components.vhd"
if [ "X$EXEMPLAR" = "X" ]
then
  _EXEMPLAR="/export/home/leonardo"
else
  _EXEMPLAR=$EXEMPLAR
fi
EXEMPLAR=$_EXEMPLAR
LEONARDO_TOP=$EXEMPLAR
SPECTRUM=$LEONARDO_TOP"/bin/spectrum"
LIBRARY="sxlib"
ENCODING="binary"
res="0"
if [ "X$PATH" = "X" ]
then
  _PATH="/bin"
else
  _PATH=$PATH
fi
PATH=$_PATH
PATH=$PATH":/usr/ccs/bin"
SIM_CYCLES="100"
SIM_PERIOD="100"
SIM_RES="5"
CLOCK_EDGE="1"
if [ "1" = "1" ]
then
  CLOCK_EDGE_NEG="0"
else
  CLOCK_EDGE_NEG="1"
fi
RESET_STATE="1"
if [ "1" = "1" ]
then
  RESET_STATE_NEG="0"
else
  RESET_STATE_NEG="1"
fi
VHDL[1]="l2"
VHDL[2]="l2_p1"
VHDL[3]="l2_main"
VHDL[4]="conpro"
do_xpat()
{
  echo [Starting xpat...]
  CMD="xpat -l obj/l2_simu.pat"
  EXEC=$CMD
  $EXEC
  _EXEC_RES=$?
  res=$_EXEC_RES
}
do_asimut()
{
  export ALLIANCE_TOP
  echo [Starting simulator...]
  MBK_IN_LO="vst"
  MBK_OUT_LO="vst"
  MBK_CATAL_NAME="CATAL"
  export MBK_IN_LO
  export MBK_OUT_LO
  export MBK_CATAL_NAME
  export MBK_CATA_LIB
  rm -f "CATAL_ASIMUT_VASY"; touch "CATAL_ASIMUT_VASY"
  echo "l2 C" >> "CATAL_ASIMUT_VASY"
  VSTFILES["1"]=""
  VSTFILES=( `ls "*.vst"` )
  for file in "${VSTFILES[@]}"
  do
    echo $file" C" >> "CATAL_ASIMUT_VASY"
  done
  CMD=$BIN"/asimut -bdd l2 l2 l2_simu"
  echo [$CMD"]" >> "l2.log"
  _TIME=`gdate +%s`
  EXEC=$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "l2.log"
  echo $_TIME >> "l2.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
}
do_pat()
{
  export ALLIANCE_TOP
  export PATH
  echo [Creating pattern file...]
  CMD=$BIN"/genpat l2_pat"
  echo [$CMD"]" >> "l2.log"
  EXEC=$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "l2.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
}
do_vst()
{
  echo [Converting VHD output to VSD...]
  CMD=$VHD2" -nomod "$SXLIB_COMP" l2.vhd"
  echo [$CMD"]" >> "l2.log"
  EXEC=$CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "l2.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
}
do_synth()
{
  echo [Creating Leonardo Spectrum command file...]
  TCL="l2.tcl"
  FILES=""
  for file in "${VHDL[@]}"
  do
    _DIRNAME1=$file
    _DIRNAME2=${_DIRNAME1%/*}
    if [ X"$_DIRNAME2" = X"$_DIRNAME1" ]
    then
    _DIRNAME2="."
    fi
    DIR=$_DIRNAME2
    _BASENAME=$file
    FILE=${_BASENAME##*/}
    SRC=$file".vhdl"
    if [ $DIR = "." ]
    then
      FILES="../"$SRC" "$FILES
    else
      FILES=$SRC" "$FILES
    fi
  done
  rm -f $TCL; touch $TCL
  if [ "X$PWD" = "X" ]
  then
    _PWD="/"
  else
    _PWD=$PWD
  fi
  PWD=$_PWD
  echo "set_working_dir "$PWD >> $TCL
  echo "set hierarchy_flatten TRUE" >> $TCL
  echo "set output_file l2.vhd" >> $TCL
  echo "set novendor_constraint_file FALSE" >> $TCL
  echo "set bubble_tristates FALSE" >> $TCL
  echo "set encoding binary" >> $TCL
  echo "load_library sxlib" >> $TCL
  echo "read -dont_elaborate {"$FILES"}" >> $TCL
  echo "pd" >> $TCL
  echo "read -technology sxlib {"$FILES"}" >> $TCL
  echo "pre_optimize -common_logic -unused_logic -boundary -xor_comparator_optimize" >> $TCL
  echo "pre_optimize -extract" >> $TCL
  echo "pd" >> $TCL
  echo "optimize .work.MOD_l2.main -target sxlib -macro -area -effort quick -hierarchy flatten" >> $TCL
  echo "optimize_timing .work.MOD_l2.main" >> $TCL
  echo "report_area -cell_usage -hierarchy -all_leafs" >> $TCL
  echo "report_delay  -num_paths 1 -critical_paths -clock_frequency" >> $TCL
  echo "auto_write -format VHD l2.vhd" >> $TCL
  echo [Forking Leonardo Spectrum...]
  CMD=$SPECTRUM" -f "$TCL
  echo [$CMD"]" >> "l2.log"
  EXEC=$CMD
  $EXEC
  _EXEC_RES=$?
  res=$_EXEC_RES
  check
  cat "leospec.log" >> "l2.log"
}
do_scram()
{
  for file in "${VHDL[@]}"
  do
    rm -rf "work._savant_lib"
    _BASENAME=$file
    SRC=${_BASENAME##*/}".vhdl"
    _DATE=`date`
    DATE=$_DATE
    echo $DATE >> "l2.log"
    echo $DATE
    echo [$SCRAM" "$SRC"]" >> "l2.log"
    echo [$SCRAM" "$SRC"]"
    _TIME=`gdate +%s`
    EXEC=$SCRAM" "$SRC
    { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "l2.log"
    echo $_TIME >> "l2.log"
    _EXEC_RES=`cat /tmp/_EXEC_RES`
    rm -f /tmp/_EXEC_RES
    res=$_EXEC_RES
    rm -rf "work._savant_lib"
  done
}
do_patc()
{
  PATC="l2_pat.c"
  rm -f $PATC; touch $PATC
  echo [Creating pattern generator file $PATC"...]"
  echo "#include <stdio.h>" >> $PATC
  echo "#include <genpat.h>" >> $PATC
  echo "#define PERIOD       100" >> $PATC
  echo "#define RES          5" >> $PATC
  echo "#define CLKDIV       (PERIOD/(2*RES))" >> $PATC
  echo "#define NS(clk,n)    ((clk*2)*PERIOD*500+(n*PERIOD*500)/CLKDIV)" >> $PATC
  echo "#define NS2(clk,n)   ((clk*2+1)*PERIOD*500+(n*PERIOD*500)/CLKDIV)" >> $PATC
  echo "#define N            100" >> $PATC
  echo "char *i2s(int v)" >> $PATC
  echo "{" >> $PATC
  echo "  char *str;" >> $PATC
  echo "  str=(char *) mbkalloc(32);" >> $PATC
  echo "  sprintf(str,\"%d\",v);" >> $PATC
  echo "  return str;" >> $PATC
  echo "};" >> $PATC
  echo "int main(int argc,char **argv)" >> $PATC
  echo "{" >> $PATC
  echo "  int clk;" >> $PATC
  echo "  int i,j;" >> $PATC
  echo "  DEF_GENPAT(\"l2\");" >> $PATC
  IND="1"
  IND=`expr $IND + "1"`
  echo "  DECLAR(\"x_rd\",\":2\",\"X\",OUT,\"11 downto 0\",\"\");" >> $PATC
  if [ "OUT" = "IN" ]
  then
    PORTINP[$IND]="x_rd"
  fi
  IND=`expr $IND + "1"`
  echo "  DECLAR(\"xa_rd\",\":2\",\"X\",OUT,\"7 downto 0\",\"\");" >> $PATC
  if [ "OUT" = "IN" ]
  then
    PORTINP[$IND]="xa_rd"
  fi
  IND=`expr $IND + "1"`
  echo "  DECLAR(\"dev_ln_din\",\":2\",\"X\",IN,\"7 downto 0\",\"\");" >> $PATC
  if [ "IN" = "IN" ]
  then
    PORTINP[$IND]="dev_ln_din"
  fi
  IND=`expr $IND + "1"`
  echo "  DECLAR(\"dev_ln_din_ack\",\":2\",\"B\",OUT,\"\",\"\");" >> $PATC
  if [ "OUT" = "IN" ]
  then
    PORTINP[$IND]="dev_ln_din_ack"
  fi
  IND=`expr $IND + "1"`
  echo "  DECLAR(\"dev_ln_dout\",\":2\",\"X\",OUT,\"7 downto 0\",\"\");" >> $PATC
  if [ "OUT" = "IN" ]
  then
    PORTINP[$IND]="dev_ln_dout"
  fi
  IND=`expr $IND + "1"`
  echo "  DECLAR(\"dev_ln_dout_ack\",\":2\",\"B\",IN,\"\",\"\");" >> $PATC
  if [ "IN" = "IN" ]
  then
    PORTINP[$IND]="dev_ln_dout_ack"
  fi
  IND=`expr $IND + "1"`
  echo "  DECLAR(\"clk\",\":2\",\"B\",IN,\"\",\"\");" >> $PATC
  if [ "IN" = "IN" ]
  then
    PORTINP[$IND]="clk"
  fi
  IND=`expr $IND + "1"`
  echo "  DECLAR(\"reset\",\":2\",\"B\",IN,\"\",\"\");" >> $PATC
  if [ "IN" = "IN" ]
  then
    PORTINP[$IND]="reset"
  fi
  echo "  for(clk=0;clk<N*2;clk++){" >> $PATC
  echo "    for(j=0;j<CLKDIV;j++){" >> $PATC
  echo "      AFFECT(i2s(NS(clk,j)),\"clk\",i2s(1));" >> $PATC
  echo "      switch (clk) {" >> $PATC
  echo "        case 0:" >> $PATC
  echo "          AFFECT(i2s(NS(clk,j)),\"reset\",i2s(1));" >> $PATC
  for signal in "${PORTINP[@]}"
  do
    if [ ! $signal = "reset" ] && [ ! $signal = "clk" ]
    then
      echo "          AFFECT(i2s(NS(clk,j)),\""$signal"\",i2s(0));" >> $PATC
    fi
  done
  echo "          break;" >> $PATC
  echo "        };" >> $PATC
  echo "      };" >> $PATC
  echo "    for(j=0;j<CLKDIV;j++){" >> $PATC
  echo "      AFFECT(i2s(NS2(clk,j)),\"clk\",i2s("$CLOCK_EDGE_NEG"));" >> $PATC
  echo "      switch (clk) {" >> $PATC
  echo "        case 2:" >> $PATC
  echo "          AFFECT(i2s(NS(clk,j)),\"reset\",i2s("$RESET_STATE_NEG"));" >> $PATC
  echo "          break;" >> $PATC
  echo "      };" >> $PATC
  echo "    };" >> $PATC
  echo "  };" >> $PATC
  echo "  SAV_GENPAT();" >> $PATC
  echo "  return 0;" >> $PATC
  echo "};" >> $PATC
}
finish()
{
  cat "l2.log" >> "../l2.log"
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
  rm -f "l2.log"; touch "l2.log"
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
  echo "Usage: l2.leonardo.tool.sh [init vhdl synth simu xpat patc pat vst asimut]"
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
    simu)
      init
      do_vst
      check
      do_patc
      check
      do_pat
      check
      do_asimut
      check
      finish
      ;;
    xpat)
      init
      do_xpat
      check
      finish
      ;;
    patc)
      init
      do_patc
      check
      finish
      ;;
    pat)
      init
      do_pat
      check
      finish
      ;;
    vst)
      init
      do_vst
      check
      finish
      ;;
    asimut)
      init
      do_asimut
      check
      finish
      ;;
  esac
done
