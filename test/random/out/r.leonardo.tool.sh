TOP="r"
OBJDIR="obj"
SRCDIR="."
DESIGN="r"
LOG="r.log"
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
res=0
if [ "X$PATH" = "X" ]
then
  _PATH="/bin"
else
  _PATH=$PATH
fi
PATH=$_PATH
PATH=$PATH":""/usr/ccs/bin"
SIM_CYCLES=100
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
VHDL[1]="r"
VHDL[2]="r_p1"
VHDL[3]="r_main"
do_xpat()
{
  echo "[Starting xpat...]"
  do_xpat_CMD="xpat -l obj/r_simu.pat"
  EXEC=$do_xpat_CMD
  $EXEC
  _EXEC_RES=$?
  res=$_EXEC_RES
}
do_asimut()
{
  export ALLIANCE_TOP=$ALLIANCE_TOP
  echo "[Starting simulator...]"
  do_asimut_MBK_IN_LO="vst"
  do_asimut_MBK_OUT_LO="vst"
  do_asimut_MBK_CATAL_NAME="CATAL"
  export MBK_IN_LO=$do_asimut_MBK_IN_LO
  export MBK_OUT_LO=$do_asimut_MBK_OUT_LO
  export MBK_CATAL_NAME=$do_asimut_MBK_CATAL_NAME
  export MBK_CATA_LIB=$MBK_CATA_LIB
  rm -f "CATAL_ASIMUT_VASY"; touch "CATAL_ASIMUT_VASY"
  echo "r C" >> "CATAL_ASIMUT_VASY"
  do_asimut_VSTFILES=( `ls *.vst` )
  for do_asimut_file in ${do_asimut_VSTFILES[@]}
  do
    echo $do_asimut_file" C" >> "CATAL_ASIMUT_VASY"
  done
  do_asimut_CMD=$BIN"/asimut -bdd ""r"" ""r"" ""r""_simu"
  echo "["$do_asimut_CMD"]" >> "r.log"
  _TIME=`gdate +%s`
  EXEC=$do_asimut_CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "r.log"
  echo $_TIME >> "r.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
}
do_pat()
{
  export ALLIANCE_TOP=$ALLIANCE_TOP
  export PATH=$PATH
  echo "[Creating pattern file...]"
  do_pat_CMD=$BIN"/genpat ""r""_pat"
  echo "["$do_pat_CMD"]" >> "r.log"
  EXEC=$do_pat_CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "r.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
}
do_vst()
{
  echo "[Converting VHD output to VSD...]"
  do_vst_CMD=$VHD2" -nomod "$SXLIB_COMP" ""r"".vhd"
  echo "["$do_vst_CMD"]" >> "r.log"
  EXEC=$do_vst_CMD
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "r.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
}
do_synth()
{
  echo "[Creating Leonardo Spectrum command file...]"
  do_synth_TCL="r.tcl"
  do_synth_FILES=""
  for do_synth_file in ${VHDL[@]}
  do
    do_synth_SRC=$do_synth_file".vhdl"
    do_synth_FILES="../"$do_synth_SRC" "$do_synth_FILES
  done
  rm -f $do_synth_TCL; touch $do_synth_TCL
  if [ "X$PWD" = "X" ]
  then
    _PWD="/"
  else
    _PWD=$PWD
  fi
  PWD=$_PWD
  echo "set_working_dir "$PWD >> $do_synth_TCL
  echo "set hierarchy_flatten TRUE" >> $do_synth_TCL
  echo "set output_file r.vhd" >> $do_synth_TCL
  echo "set novendor_constraint_file FALSE" >> $do_synth_TCL
  echo "set bubble_tristates FALSE" >> $do_synth_TCL
  echo "set encoding binary" >> $do_synth_TCL
  echo "load_library sxlib" >> $do_synth_TCL
  echo "read -dont_elaborate {"$do_synth_FILES"}" >> $do_synth_TCL
  echo "pd" >> $do_synth_TCL
  echo "read -technology sxlib {"$do_synth_FILES"}" >> $do_synth_TCL
  echo "pre_optimize -common_logic -unused_logic -boundary -xor_comparator_optimize" >> $do_synth_TCL
  echo "pre_optimize -extract" >> $do_synth_TCL
  echo "pd" >> $do_synth_TCL
  echo "optimize .work.MOD_r.main -target sxlib -macro -area -effort quick -hierarchy flatten" >> $do_synth_TCL
  echo "optimize_timing .work.MOD_r.main" >> $do_synth_TCL
  echo "report_area -cell_usage -hierarchy -all_leafs" >> $do_synth_TCL
  echo "report_delay  -num_paths 1 -critical_paths -clock_frequency" >> $do_synth_TCL
  echo "auto_write -format VHD r.vhd" >> $do_synth_TCL
  echo "[Forking Leonardo Spectrum...]"
  do_synth_CMD=$SPECTRUM" -f "$do_synth_TCL
  echo "["$do_synth_CMD"]" >> "r.log"
  EXEC=$do_synth_CMD
  $EXEC
  _EXEC_RES=$?
  res=$_EXEC_RES
  check
  cat "leospec.log" >> "r.log"
}
do_scram()
{
  for do_scram_file in ${VHDL[@]}
  do
    rm -rf "work._savant_lib"
    _BASENAME=$do_scram_file
    do_scram_SRC=${_BASENAME##*/}".vhdl"
    _DATE=`date`
    do_scram_DATE=$_DATE
    echo $do_scram_DATE >> "r.log"
    echo $do_scram_DATE
    echo "["$SCRAM" "$do_scram_SRC"]" >> "r.log"
    echo "["$SCRAM" "$do_scram_SRC"]"
    _TIME=`gdate +%s`
    EXEC=$SCRAM" "$do_scram_SRC
    { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "r.log"
    echo $_TIME >> "r.log"
    _EXEC_RES=`cat /tmp/_EXEC_RES`
    rm -f /tmp/_EXEC_RES
    res=$_EXEC_RES
    rm -rf "work._savant_lib"
  done
}
do_patc()
{
  do_patc_PATC="r_pat.c"
  rm -f $do_patc_PATC; touch $do_patc_PATC
  echo "[Creating pattern generator file "$do_patc_PATC"...]"
  echo "#include <stdio.h>" >> $do_patc_PATC
  echo "#include <genpat.h>" >> $do_patc_PATC
  echo "#define PERIOD       100" >> $do_patc_PATC
  echo "#define RES          5" >> $do_patc_PATC
  echo "#define CLKDIV       (PERIOD/(2*RES))" >> $do_patc_PATC
  echo "#define NS(clk,n)    ((clk*2)*PERIOD*500+(n*PERIOD*500)/CLKDIV)" >> $do_patc_PATC
  echo "#define NS2(clk,n)   ((clk*2+1)*PERIOD*500+(n*PERIOD*500)/CLKDIV)" >> $do_patc_PATC
  echo "#define N            100" >> $do_patc_PATC
  echo "char *i2s(int v)" >> $do_patc_PATC
  echo "{" >> $do_patc_PATC
  echo "  char *str;" >> $do_patc_PATC
  echo "  str=(char *) mbkalloc(32);" >> $do_patc_PATC
  echo "  sprintf(str,\"%d\",v);" >> $do_patc_PATC
  echo "  return str;" >> $do_patc_PATC
  echo "};" >> $do_patc_PATC
  echo "int main(int argc,char **argv)" >> $do_patc_PATC
  echo "{" >> $do_patc_PATC
  echo "  int clk;" >> $do_patc_PATC
  echo "  int i,j;" >> $do_patc_PATC
  echo "  DEF_GENPAT(\"r\");" >> $do_patc_PATC
  do_patc_IND=1
  do_patc_IND=`expr $do_patc_IND + 1`
  echo "  DECLAR(\"x\",\":2\",\"X\",OUT,\"9 downto 0\",\"\");" >> $do_patc_PATC
  if [ "OUT" = "IN" ]
  then
    do_patc_PORTINP[$do_patc_IND]="x"
  fi
  do_patc_IND=`expr $do_patc_IND + 1`
  echo "  DECLAR(\"y\",\":2\",\"X\",OUT,\"9 downto 0\",\"\");" >> $do_patc_PATC
  if [ "OUT" = "IN" ]
  then
    do_patc_PORTINP[$do_patc_IND]="y"
  fi
  do_patc_IND=`expr $do_patc_IND + 1`
  echo "  DECLAR(\"clk\",\":2\",\"B\",IN,\"\",\"\");" >> $do_patc_PATC
  if [ "IN" = "IN" ]
  then
    do_patc_PORTINP[$do_patc_IND]="clk"
  fi
  do_patc_IND=`expr $do_patc_IND + 1`
  echo "  DECLAR(\"reset\",\":2\",\"B\",IN,\"\",\"\");" >> $do_patc_PATC
  if [ "IN" = "IN" ]
  then
    do_patc_PORTINP[$do_patc_IND]="reset"
  fi
  echo "  for(clk=0;clk<N*2;clk++){" >> $do_patc_PATC
  echo "    for(j=0;j<CLKDIV;j++){" >> $do_patc_PATC
  echo "      AFFECT(i2s(NS(clk,j)),\"clk\",i2s(1));" >> $do_patc_PATC
  echo "      switch (clk) {" >> $do_patc_PATC
  echo "        case 0:" >> $do_patc_PATC
  echo "          AFFECT(i2s(NS(clk,j)),\"reset\",i2s(1));" >> $do_patc_PATC
  for do_patc_signal in ${do_patc_PORTINP[@]}
  do
    if [ ! "$do_patc_signal" = "reset" ] && [ ! "$do_patc_signal" = "clk" ]
    then
      echo "          AFFECT(i2s(NS(clk,j)),\""$do_patc_signal"\",i2s(0));" >> $do_patc_PATC
    fi
  done
  echo "          break;" >> $do_patc_PATC
  echo "        };" >> $do_patc_PATC
  echo "      };" >> $do_patc_PATC
  echo "    for(j=0;j<CLKDIV;j++){" >> $do_patc_PATC
  echo "      AFFECT(i2s(NS2(clk,j)),\"clk\",i2s("$CLOCK_EDGE_NEG"));" >> $do_patc_PATC
  echo "      switch (clk) {" >> $do_patc_PATC
  echo "        case 2:" >> $do_patc_PATC
  echo "          AFFECT(i2s(NS(clk,j)),\"reset\",i2s("$RESET_STATE_NEG"));" >> $do_patc_PATC
  echo "          break;" >> $do_patc_PATC
  echo "      };" >> $do_patc_PATC
  echo "    };" >> $do_patc_PATC
  echo "  };" >> $do_patc_PATC
  echo "  SAV_GENPAT();" >> $do_patc_PATC
  echo "  return 0;" >> $do_patc_PATC
  echo "};" >> $do_patc_PATC
}
finish()
{
  cat "r.log" >> "../r.log"
  cd "-"
}
init()
{
  cd "obj"
  rm -f "r.log"; touch "r.log"
}
check()
{
  if [ ! $res = 0 ]
  then
    echo "[Abort due to errors building target.]"
    exit 1
  fi
}
if [ $# = 0 ]
then
  echo "Usage: r.leonardo.tool.sh [init vhdl synth simu xpat patc]"
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
  esac
done
