#!/bin/bash
#
# Project definitions and synthesis parameters
#
TOP=t

VHDL[1]=t
VHDL[2]=t_p1
VHDL[3]=t_main
OBJDIR=obj
SRCDIR=.
ASIMUT=""
AUX[1]=t_pat.c

#
# Alliance definitions
#
if [ "X$ALLIANCE_TOP" = "X" ]
then
  ALLIANCE_TOP=/opt/alliance-5.0
fi
export ALLIANCE_TOP
BIN=$ALLIANCE_TOP/bin
CELLS=$ALLIANCE_TOP/cells
MBK_CATA_LIB=$CELLS/sxlib
SCRAM=$BIN/scram
VHD2=$BIN/vhd2vst
SXLIB_COMP=$CELLS/sxlib/sxlib_components.vhd
LOG="$TOP"".log"
DUP=tee
TIME="time -p"
#
# Leonardo definitions
#
if [ "X$EXEMPLAR" = "X" ]
then
  EXEMPLAR=/export/home/leonardo
fi
export LEONARDO_TOP=$EXEMPLAR
export EXEMPLAR

SPECTRUM=$LEONARDO_TOP/bin/spectrum


check ()
{
    if [ $? != 0 ]
    then
        echo "[Abort due to errors building target.]"
        exit 1
    fi
}    

if [ $# = 0 ]
then
  echo "Usage: build <targets>"
  echo "Target vhdl:
VHDL Syntax Parser and VHDL Analyzer for SYnthesis: VHDL -> ()"
  echo "Target synth:
Leonardo Spectrum Logic Gate Synthesis: VHDL -> VHD"
  echo "Target vst:
Postprocess VHD output for Asimut: VHD -> VST"
  echo "Target pat:
Create pattern file: C -> PAT"
  echo "Target simu:
A SIMUlation Tool: VST -> PAT"
  echo "Target clean:
Project cleanup"
  exit 1
fi

do_init ()
{
    if [ ! -d $OBJDIR ]
    then
        mkdir $OBJDIR
    fi
    cd $OBJDIR
    for i in ${VHDL[@]}
    do
        FILE="$i"".vhdl"
        if [ -f "../$SRCDIR/$FILE" ] 
        then
          cp "../$SRCDIR/$FILE" .
        else
          cp  $FILE .
        fi
    done
    for i in ${AUX[@]}
    do
        if [ -f "../$SRCDIR/$i" ] 
        then
          cp ../$SRCDIR/$i .
        else
          cp  $i .
        fi
    done
};


do_finish ()
{
    cd -
}


#
# VHDL Parser
#
# VHDL      
#
do_scram()
{
    for NAME in ${VHDL[@]}
    do
        rm -rf work._savant_lib
        SRC="$NAME"".vhdl"
        $DATE | $DUP -a $LOG
        echo "[$SCRAM $SRC]" | $DUP -a $LOG
        $TIME $SCRAM $SRC 2>&1 | $DUP -a $LOG 
    done
    rm -rf work._savant_lib
}


do_synth()
{
  echo "Creating Leonardo Spectrum command file..."
  TCL="$TOP.tcl"
  FILES=""
  for NAME in ${VHDL[@]}
  do
    SRC="$NAME"".vhdl"
    FILES="../$SRC $FILES"
  done
  echo "set_working_dir $PWD" > $TCL
  echo "set hierarchy_flatten TRUE" >> $TCL
  echo "set output_file $TOP.vhd" >> $TCL
  echo "set novendor_constraint_file FALSE" >> $TCL
  echo "set bubble_tristates FALSE" >> $TCL
  echo "set encoding binary" >> $TCL   
  echo "load_library sxlib" >> $TCL
  echo "read -dont_elaborate  {$FILES}" >> $TCL
  echo "pd" >> $TCL
  echo "read -technology "sxlib" {$FILES}" >> $TCL
  echo "pre_optimize -common_logic -unused_logic -boundary -xor_comparator_optimize" >> $TCL
  echo "pre_optimize -extract" >> $TCL
  echo "pd" >> $TCL
  echo "optimize .work.MOD_""$TOP.main -target sxlib -macro -area -effort quick -hierarchy flatten" >> $TCL
  echo "optimize_timing .work.MOD_""$TOP.main" >> $TCL
  echo "report_area -cell_usage -hierarchy -all_leafs" >> $TCL
  echo "report_delay  -num_paths 1 -critical_paths -clock_frequency" >> $TCL
  echo "auto_write -format VHD $TOP.vhd" >> $TCL
  echo "Forking Leonardo Spectrum..."
  $SPECTRUM -f $TCL
  cp leospec.log "../$TOP""_synth.log"
}


do_vst()
{
  echo "Converting..."
  $VHD2 -nomod $SXLIB_COMP $TOP.vhd 
}


do_pat()
{
    $BIN/genpat t_pat
};


#
# A SIMUlation Tool
#
# VST -> PAT
#
do_asimut()
{
    MBK_IN_LO=vst
    MBK_OUT_LO=vst
    MBK_CATAL_NAME=CATAL
    export MBK_IN_LO
    export MBK_OUT_LO
    export MBK_CATAL_NAME    
    export MBK_CATA_LIB

    n=1
    echo "$TOP  C" > CATAL_ASIMUT_VASY
    for NAME in *.vst
    do
        echo "$NAME  C" >> CATAL_ASIMUT_VASY
        n=`expr $n + 1`
    done
    $DATE | $DUP -a $LOG
    echo "[$BIN/asimut -bdd $ASIMUT $TOP $TOP ""$TOP""_simu]"  | $DUP -a $LOG
    $TIME $BIN/asimut -bdd $AIMUT $TOP $TOP "$TOP""_simu"  2>&1 | $DUP -a $LOG
}


do_init 
for TARGET in $@
do
    echo "Building target [$TARGET] ..."
    case $TARGET in

        vhdl)
            do_scram
            check
            ;;
        synth)
            do_synth
            check
            ;;
        vst)
            do_vst
            check
            ;;
        pat)
            do_pat
            check
            ;;
        simu)
            do_asimut
            check
            ;;
        clean)
            rm -rf *
            ;;
    esac
done
do_finish
