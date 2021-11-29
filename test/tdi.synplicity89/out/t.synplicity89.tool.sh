#
# Tool Interface Tool synplicity89.t
# Version 2.02
#
TOP="MOD_t"
DEVICE="a3p125"
DEVICE_CLASS="proasic3"
DEVICE_PACKAGE="ft256"
DEVICE_SPEED="2"
DEVICE_VOLT="1.5"
OBJDIR="obj"
SRCDIR="."
DESIGN="t"
LOG="t.log"
DUP="tee"
if [ "X$PWD" = "X" ]
then
  _PWD="/tmp"
else
  _PWD=$PWD
fi
PWD=$_PWD
if [ "X$SYNPLICITY" = "X" ]
then
  _SYNPLICITY="/export/home/fpga_89"
else
  _SYNPLICITY=$SYNPLICITY
fi
SYNPLICITY=$_SYNPLICITY
if [ "X$ACTEL" = "X" ]
then
  _ACTEL="/export/home/Actel/Designer73"
else
  _ACTEL=$ACTEL
fi
ACTEL=$_ACTEL
if [ "X$SYNPLIFY" = "X" ]
then
  _SYNPLIFY=$SYNPLICITY"/bin/synplify_pro"
else
  _SYNPLIFY=$SYNPLIFY
fi
SYNPLIFY=$_SYNPLIFY
if [ "X$DESIGNER" = "X" ]
then
  _DESIGNER=$ACTEL"/bin/designer"
else
  _DESIGNER=$DESIGNER
fi
DESIGNER=$_DESIGNER
res="0"
VHDL[1]="t_main"
VHDL[2]="t_p_3"
VHDL[3]="t_p_2"
VHDL[4]="t_p_1"
VHDL[5]="t_p_0"
VHDL[6]="t"
do_tech()
{
  echo "[building run and project scripts for Actel Designer...]"
  cd ".."
  PROJFILE="t.tcl"
  make_designer_runscript
  check
  echo "[starting Designer...]"
  echo $DESIGNER" SCRIPT:"$PROJFILE
  EXEC=$DESIGNER" SCRIPT:"$PROJFILE
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  check
  cd "obj"
}
make_designer_runscript()
{
  PROJFILE="t.tcl"
  PROJ="t"
  rm -f $PROJFILE; touch $PROJFILE
  echo "new_design -name \""$PROJ"\" \" >> $PROJFILE
  echo "  -family \"proasic3\" \" >> $PROJFILE
  echo "  -path {"$PWD"/obj} \" >> $PROJFILE
  echo "  -block \"off\"" >> $PROJFILE
  echo "set_design -name \""$PROJ"\" \" >> $PROJFILE
  echo "  -family \"proasic3\" \" >> $PROJFILE
  echo "  -path {"$PWD"/obj} \" >> $PROJFILE
  echo "  -block \"off\"" >> $PROJFILE
  echo "set_device -die \"a3p125\" \" >> $PROJFILE
  echo "  -package \"ft256\" \" >> $PROJFILE
  echo "  -speed \"-2\" \" >> $PROJFILE
  echo "  -voltage \"1.5\" \" >> $PROJFILE
  echo "  -iostd \"LVTTL\" \" >> $PROJFILE
  echo "  -jtag \"yes\" \" >> $PROJFILE
  echo "  -probe \"yes\" \" >> $PROJFILE
  echo "  -trst \"yes\" \" >> $PROJFILE
  echo "  -temprange \"COM\" \" >> $PROJFILE
  echo "  -voltrange \"COM\"" >> $PROJFILE
  echo "import_source -format \"edif\" \" >> $PROJFILE
  echo "  -edif_flavor \"GENERIC\" \" >> $PROJFILE
  echo "  {"$PWD"/obj/"$PROJ".edn} \" >> $PROJFILE
  echo "  -merge_physical \"no\" \" >> $PROJFILE
  echo "  -merge_timing \"yes\"" >> $PROJFILE
  echo "compile -pdc_abort_on_error \"on\" -reserve_ff_pin \"off\" \" >> $PROJFILE
  echo "  -pdc_eco_display_unmatched_objects \"off\" \" >> $PROJFILE
  echo "  -pdc_eco_max_warnings 10000 -demote_globals \"off\" \" >> $PROJFILE
  echo "  -demote_globals_max_fanout 12 -promote_globals \"off\" \" >> $PROJFILE
  echo "  -promote_globals_min_fanout 200 \" >> $PROJFILE
  echo "  -promote_globals_max_limit 0 \" >> $PROJFILE
  echo "  -localclock_max_shared_instances 12 \" >> $PROJFILE
  echo "  -localclock_buffer_tree_max_fanout 12 \" >> $PROJFILE
  echo "  -combine_register \"off\" \" >> $PROJFILE
  echo "  -delete_buffer_tree \"off\" \" >> $PROJFILE
  echo "  -delete_buffer_tree_max_fanout 12 \" >> $PROJFILE
  echo "  -report_high_fanout_nets_limit 10" >> $PROJFILE
  echo "layout -timing_driven \" >> $PROJFILE
  echo "  -run_placer \"on\" \" >> $PROJFILE
  echo "  -place_incremental \"off\" \" >> $PROJFILE
  echo "  -run_router \"on\" \" >> $PROJFILE
  echo "  -route_incremental \"OFF\" \" >> $PROJFILE
  echo "  -placer_high_effort \"off\"" >> $PROJFILE
  echo "export -format \"bts_stp\" \" >> $PROJFILE
  echo "  -feature \"prog_fpga\" \" >> $PROJFILE
  echo "  -io_state \"Z\" \" >> $PROJFILE
  echo "  {"$PWD"/obj/"$PROJ".stp}" >> $PROJFILE
}
do_synth()
{
  echo "[building run and project scripts for "$SYNPLIFY"...]"
  cd ".."
  PROJFILE="t.prj"
  make_synp_runscript
  check
  echo "[starting "$SYNPLIFY"...]"
  echo $SYNPLIFY" -batch "$PROJFILE
  EXEC=$SYNPLIFY" -batch "$PROJFILE
  { $EXEC; echo $? > /tmp/_EXEC_RES; } 2>&1 | tee -a "t.log"
  _EXEC_RES=`cat /tmp/_EXEC_RES`
  rm -f /tmp/_EXEC_RES
  res=$_EXEC_RES
  if [ $res = "2" ]
  then
    cat "obj/t.srr"
  fi
  check
  cat "obj/t.srr" >> "t.log"
  cd "obj"
}
make_synp_runscript()
{
  PROJFILE="t.prj"
  rm -f $PROJFILE; touch $PROJFILE
  DIR="."
  SRC="t_main"
  if [ $DIR = $SRC ]
  then
    echo "add_file -vhdl -lib work \""$SRC".vhdl\"" >> $PROJFILE
  else
    echo "add_file -vhdl -lib work \""$DIR"/"$SRC".vhdl\"" >> $PROJFILE
  fi
  DIR="."
  SRC="t_p_3"
  if [ $DIR = $SRC ]
  then
    echo "add_file -vhdl -lib work \""$SRC".vhdl\"" >> $PROJFILE
  else
    echo "add_file -vhdl -lib work \""$DIR"/"$SRC".vhdl\"" >> $PROJFILE
  fi
  DIR="."
  SRC="t_p_2"
  if [ $DIR = $SRC ]
  then
    echo "add_file -vhdl -lib work \""$SRC".vhdl\"" >> $PROJFILE
  else
    echo "add_file -vhdl -lib work \""$DIR"/"$SRC".vhdl\"" >> $PROJFILE
  fi
  DIR="."
  SRC="t_p_1"
  if [ $DIR = $SRC ]
  then
    echo "add_file -vhdl -lib work \""$SRC".vhdl\"" >> $PROJFILE
  else
    echo "add_file -vhdl -lib work \""$DIR"/"$SRC".vhdl\"" >> $PROJFILE
  fi
  DIR="."
  SRC="t_p_0"
  if [ $DIR = $SRC ]
  then
    echo "add_file -vhdl -lib work \""$SRC".vhdl\"" >> $PROJFILE
  else
    echo "add_file -vhdl -lib work \""$DIR"/"$SRC".vhdl\"" >> $PROJFILE
  fi
  DIR="."
  SRC="t"
  if [ $DIR = $SRC ]
  then
    echo "add_file -vhdl -lib work \""$SRC".vhdl\"" >> $PROJFILE
  else
    echo "add_file -vhdl -lib work \""$DIR"/"$SRC".vhdl\"" >> $PROJFILE
  fi
  echo "impl -add obj -type fpga" >> $PROJFILE
  echo "set_option -technology proasic3" >> $PROJFILE
  echo "set_option -part a3p125" >> $PROJFILE
  echo "set_option -package \"\"" >> $PROJFILE
  echo "set_option -speed_grade -2" >> $PROJFILE
  echo "set_option -part_companion \"\"" >> $PROJFILE
  echo "set_option -default_enum_encoding default" >> $PROJFILE
  echo "set_option -resource_sharing 1" >> $PROJFILE
  echo "set_option -top_module \"MOD_t\"" >> $PROJFILE
  echo "set_option -frequency auto" >> $PROJFILE
  echo "set_option -run_prop_extract 1" >> $PROJFILE
  echo "set_option -fanout_limit 12" >> $PROJFILE
  echo "set_option -globalthreshold 50" >> $PROJFILE
  echo "set_option -maxfan_hard 0" >> $PROJFILE
  echo "set_option -disable_io_insertion 0" >> $PROJFILE
  echo "set_option -retiming 0" >> $PROJFILE
  echo "set_option -report_path 4000" >> $PROJFILE
  echo "set_option -opcond Default" >> $PROJFILE
  echo "set_option -update_models_cp 0" >> $PROJFILE
  echo "set_option -preserve_registers 0" >> $PROJFILE
  echo "set_option -symbolic_fsm_compiler 1" >> $PROJFILE
  echo "set_option -write_verilog 0" >> $PROJFILE
  echo "set_option -write_vhdl 0" >> $PROJFILE
  echo "set_option -write_apr_constraint 1" >> $PROJFILE
  echo "project -result_format \"edif\"" >> $PROJFILE
  echo "project -result_file \"obj/t.edn\"" >> $PROJFILE
  echo "set_option -vlog_std v2001" >> $PROJFILE
  echo "set_option -synthesis_onoff_pragma 0" >> $PROJFILE
  echo "set_option -project_relative_includes 1" >> $PROJFILE
  echo "impl -active \"obj\"" >> $PROJFILE
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
  echo "Usage: t.synplicity89.tool.sh [init synth tech]"
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
    synth)
      init
      do_synth
      check
      finish
      ;;
    tech)
      init
      do_tech
      check
      finish
      ;;
  esac
done
