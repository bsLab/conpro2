
--
--  CONPRO Mentor Graphics Leonardo Spectrum + Alliance Asimut Tool Descriptor Interface
--

#version "2.06";

#parameter
begin
  $TOP <= $proj;
  $OBJDIR <= "obj";
  $SRCDIR <= ".";
  $DESIGN <= $proj;
  $LOG <= $TOP + ".log";
  $DUP <= "tee";
  $PWD <= get_env($PWD,"/");
  $ALLIANCE_TOP <= get_env($ALLIANCE_TOP,"/opt/alliance-5.0");
  $BIN <= $ALLIANCE_TOP + "/bin";
  $CELLS <= $ALLIANCE_TOP + "/cells";
  $MBK_CATA_LIB <= $CELLS + "/sxlib";
  $SCRAM <= $BIN + "/scram";
  $VHD2 <= $BIN + "/vhd2vst";
  $SXLIB_COMP <= $CELLS +"/sxlib/sxlib_components.vhd";
  $EXEMPLAR <= get_env($EXEMPLAR,"/export/home/leonardo");
  $LEONARDO_TOP <= $EXEMPLAR;
  $SPECTRUM <= $LEONARDO_TOP + "/bin/spectrum";
  $LIBRARY <= "sxlib";
  $ENCODING <= get_opt($encoding,"binary");
  $res <= 0;
  $PATH <= get_env($PATH,"/bin");
  $PATH <= $PATH + ":" + "/usr/ccs/bin";
  $SIM_CYCLES <= get_opt($simu_cycles,"100");
  $SIM_PERIOD <= get_opt($simu_period,"100");
  $SIM_RES <= get_opt($simu_res,"5");
  $CLOCK_EDGE <= get_opt($clock_edge,"1");
  if $CLOCK_EDGE = "1" then
    $CLOCK_EDGE_NEG <= "0"
  else
    $CLOCK_EDGE_NEG <= "1";
  $RESET_STATE <= get_opt($reset_level,"1");
  if $RESET_STATE = "1" then
    $RESET_STATE_NEG <= "0"
  else
    $RESET_STATE_NEG <= "1";
end;

#parameter
begin
  -- VHDL sources
  foreach $file in $vhdl do
  begin
    $VHDL.[$I] <= chop_extension($file);
  end;
end;


check: #fun
begin
  if  $res <> 0 then
  begin
    print ("[Abort due to errors building target.]");
    exit (1);
  end;
end;

init: #fun
begin
  if exist($OBJDIR) = 0 then
    make_dir($OBJDIR);
  change_dir($OBJDIR);
  create_file($LOG);
end;

finish:#fun
begin
  append_file ($LOG,"../"+$LOG);
  change_dir("-");
end;

--
-- Pattern generator file / emit C Code
--
do_patc:#fun
begin
  $PATC <= $TOP + "_pat.c";
  create_file($PATC);
  print_line("[Creating pattern generator file "+$PATC+"...]");
  write_line($PATC,
    "#include <stdio.h>",
    "#include <genpat.h>",
    "#define PERIOD       "+$SIM_PERIOD,
    "#define RES          "+$SIM_RES,
    "#define CLKDIV       (PERIOD/(2*RES))",
    "#define NS(clk,n)    ((clk*2)*PERIOD*500+(n*PERIOD*500)/CLKDIV)",
    "#define NS2(clk,n)   ((clk*2+1)*PERIOD*500+(n*PERIOD*500)/CLKDIV)",
    "#define N            "+$SIM_CYCLES,
    "char *i2s(int v)",
    "{",
    "  char *str;",
    "  str=(char *) mbkalloc(32);",
    "  sprintf(str,\"%d\",v);",
    "  return str;",
    "};",
    "int main(int argc,char **argv)",
    "{",
    "  int clk;",
    "  int i,j;",
    "  DEF_GENPAT(\""+$proj+"\");"
    );

  $IND <= 1;
  foreach $signal in $port do
  begin
    $IND <= $IND + 1;
    write_line ($PATC,
    "  DECLAR(\""+
                port($signal,"asimut::name")+
                "\",\":2\",\""+
                port($signal,"asimut::type")+
                "\","+
                port($signal,"asimut::dir")+
                ",\""+
                port($signal,"asimut::range")+
                "\",\"\");");
    if port($signal,"asimut::dir") = "IN" then
    begin
      $PORTINP.[$IND] <= port($signal,"asimut::name");
    end;
  end;
  write_line ($PATC,
    "  for(clk=0;clk<N*2;clk++){",
    "    for(j=0;j<CLKDIV;j++){");
  write_line ($PATC,
    "      AFFECT(i2s(NS(clk,j)),\"clk\",i2s("+$CLOCK_EDGE+"));");
  write_line ($PATC,
    "      switch (clk) {",
    "        case 0:");
  write_line ($PATC,
    "          AFFECT(i2s(NS(clk,j)),\"reset\",i2s("+$RESET_STATE+"));");
  foreach $signal in $PORTINP do
  begin
    if $signal <> "reset" and $signal <> "clk" then
      write_line ($PATC,
        "          AFFECT(i2s(NS(clk,j)),\""+$signal+"\",i2s(0));");
  end;
  write_line ($PATC,
    "          break;",
    "        };",
    "      };",
    "    for(j=0;j<CLKDIV;j++){",
    "      AFFECT(i2s(NS2(clk,j)),\"clk\",i2s("+$CLOCK_EDGE_NEG+"));",
    "      switch (clk) {",
    "        case 2:",
    "          AFFECT(i2s(NS(clk,j)),\"reset\",i2s("+$RESET_STATE_NEG+"));",
    "          break;",
    "      };",
    "    };",
    "  };",
    "  SAV_GENPAT();",
    "  return 0;",
    "};");     
end;

--
-- VHDL checker
--

do_scram:#fun
begin
  foreach $file in $VHDL do
  begin
    remove_dir("work._savant_lib");
    $SRC <= basename($file)+".vhdl";
    $DATE <= date();
    write_line($LOG,$DATE);
    print_line($DATE);
    write_line($LOG,"["+$SCRAM+" "+$SRC+"]");
    print_line("["+$SCRAM+" "+$SRC+"]");
    --
    -- exec: exec command and print stdout to stdout
    -- exec_write: exec command and print stdout to file
    --             third argument is optional (additional output)
    $res <= exec_write($LOG,$SCRAM+" "+$SRC,time());
    remove_dir("work._savant_lib");
  end;
end;

--
-- Synthesis
--

do_synth:#fun
begin
  print ("[Creating Leonardo Spectrum command file...]");
  $TCL <= $TOP  + ".tcl";
  $FILES <= "";
  foreach $file in $VHDL do
  begin
    $DIR <= dirname($file);
    $FILE <= basename($file);
    $SRC <= $file + ".vhdl";
    if $DIR = "." then
    begin
      $FILES <= "../" + $SRC + " " + $FILES;
    end
    else
    begin
      $FILES <= $SRC + " " + $FILES;    
    end;
  end;

  create_file ($TCL);
  $PWD <= get_env($PWD,"/");
  write_line ($TCL,"set_working_dir " + $PWD);
  write_line ($TCL,"set hierarchy_flatten TRUE");
  write_line ($TCL,"set output_file "+ $TOP + ".vhd");
  write_line ($TCL,"set novendor_constraint_file FALSE");
  write_line ($TCL,"set bubble_tristates FALSE");
  write_line ($TCL,"set encoding "+ $ENCODING);
  write_line ($TCL,"load_library "+ $LIBRARY);
  write_line ($TCL,"read -dont_elaborate {"+ $FILES + "}");
  write_line ($TCL,"pd");
  write_line ($TCL,"read -technology " + $LIBRARY + " {" + $FILES + "}");
  write_line ($TCL,"pre_optimize -common_logic -unused_logic -boundary -xor_comparator_optimize");
  write_line ($TCL,"pre_optimize -extract");
  write_line ($TCL,"pd");
  write_line ($TCL,"optimize .work.MOD_"+ $TOP + ".main -target sxlib -macro -area -effort quick -hierarchy flatten");
  write_line ($TCL,"optimize_timing .work.MOD_"+$TOP+".main");
  write_line ($TCL,"report_area -cell_usage -hierarchy -all_leafs");
  write_line ($TCL,"report_delay  -num_paths 1 -critical_paths -clock_frequency");
  write_line ($TCL,"auto_write -format VHD "+$TOP+".vhd");
  print_line ("[Forking Leonardo Spectrum...]");
  $CMD <= $SPECTRUM + " -f "+$TCL;
  write_line($LOG,"["+$CMD+"]");
  
  $res <= exec($CMD);
  check();
  append_file ("leospec.log",$LOG);
end;


--
-- Post synthesis conversion
--
do_vst:#fun
begin
  print_line("[Converting VHD output to VSD...]");
  $CMD <= $VHD2 + " -nomod "+$SXLIB_COMP+" "+$TOP+".vhd";
  write_line($LOG,"["+$CMD+"]");
  $res <= exec_write($LOG,$CMD);
  check();
end;

--
-- Pattern generation
--
do_pat:#fun
begin
  export($ALLIANCE_TOP);
  export($PATH);
  print_line("[Creating pattern file...]");
  $CMD <= $BIN+ "/genpat " + $TOP + "_pat";
  write_line($LOG,"["+$CMD+"]");
  $res <= exec_write($LOG,$CMD);
  check();
end;


--
-- Simulation
--

do_asimut:#fun
begin
  export($ALLIANCE_TOP);
  print_line("[Starting simulator...]");
  $MBK_IN_LO <= "vst";
  $MBK_OUT_LO <= "vst";
  $MBK_CATAL_NAME <= "CATAL";
  export($MBK_IN_LO);
  export($MBK_OUT_LO);
  export($MBK_CATAL_NAME);    
  export($MBK_CATA_LIB);
  create_file ("CATAL_ASIMUT_VASY");
  write_line ("CATAL_ASIMUT_VASY",$TOP + " C");
  $VSTFILES.[1] <= "";
  list($VSTFILES,"*.vst");
  foreach $file in $VSTFILES do
  begin
    write_line ("CATAL_ASIMUT_VASY",$file + " C");
  end;
  $CMD <= $BIN + "/asimut -bdd " + 
          $TOP + " " + $TOP + " " + $TOP + "_simu";
  write_line($LOG,"["+$CMD+"]");
  $res <= exec_write($LOG,$CMD,time()); 
end;

--
-- Pattern viewer
--

do_xpat:#fun
begin
  print_line("[Starting xpat...]");
  $CMD <= "xpat -l obj/" + $TOP + "_simu.pat";
  $res <= exec($CMD);
end;

#targets
begin
  init:#target
  begin
    init ();
    check ();
    finish ();
  end;
  vhdl:#target
  begin
    init ();
    do_scram ();
    check ();
    finish ();
  end;
  synth:#target
  begin
    init ();
    do_synth ();
    check ();
    finish ();
  end;
  simu:#target
  begin
    init ();
    do_vst ();
    check ();
    do_patc ();
    check ();
    do_pat ();
    check ();
    do_asimut ();
    check ();
    finish ();
  end;
  xpat:#target
  begin
    init ();
    do_xpat ();
    check ();
    finish ();
  end;
  patc:#target
  begin
    init ();
    do_patc ();
    check ();
    finish ();
  end;
  pat:#target
  begin
    init ();
    do_pat ();
    check ();
    finish ();
  end;
  vst:#target
  begin
    init ();
    do_vst ();
    check ();
    finish ();
  end;
  asimut:#target
  begin
    init ();
    do_asimut ();
    check ();
    finish ();
  end;
end;
