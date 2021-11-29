
--
-- Synplify(Pro) Synthesis && Actel Designer Backends
--
#version "2.03";


#parameter
begin

  $TOP <= "MOD_" + $proj;
  $DEVICE <= get_opt($target_device.[1],"A3P125");
  $DEVICE_CLASS <= get_opt($target_family.[1],"PROASIC3");
  $DEVICE_PACKAGE <= get_opt($target_package.[1],"144 FBGA");
  $DEVICE_SPEED <= get_opt($target_speed.[1],"2");
  $DEVICE_VOLT <= get_opt($target_volt.[1],"1.5");
  $OBJDIR <= "obj";
  $SRCDIR <= ".";
  $DESIGN <= $proj;
  $LOG <= $proj + ".log";
  $DUP <= "tee";
  $PWD <= get_env($PWD,"/tmp");
  $SYNPLICITY <= get_env($SYNPLICITY,"/export/home/fpga_89");
  $ACTEL <= get_env($ACTEL,"/export/home/Actel/Designer73");
  $SYNPLIFY <= get_env($SYNPLIFY,$SYNPLICITY + "/bin/synplify_pro");
  $DESIGNER <= get_env($DESIGNER,$ACTEL + "/bin/designer");
  $res <= 0;
end;

#parameter
begin
  -- VHDL sources
  foreach $file in rev($vhdl) do
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

make_synp_runscript: #fun
begin
  $PROJFILE <= $DESIGN+".prj";
  create_file($PROJFILE);
  --
  -- write: write arguments (append)
  -- write_line: write all arguments in one line (append)
  -- write_lines: write each argument into a new line
  --
  foreach $file in rev($vhdl) do
  begin
    $DIR <= dirname($file);
    $SRC <= basename($file);
    if $DIR = $SRC then
      write_line($PROJFILE,"add_file -vhdl -lib work \""+$SRC+".vhdl\"")
    else
      write_line($PROJFILE,"add_file -vhdl -lib work \""+$DIR+"/"+$SRC+".vhdl\"");
    
  end;
  write_line($PROJFILE,
            "impl -add obj -type fpga",
            "set_option -technology "+$DEVICE_CLASS,
            "set_option -part "+$DEVICE,
            "set_option -package \"\"",
            "set_option -speed_grade -"+$DEVICE_SPEED,
            "set_option -part_companion \"\"",
            "set_option -default_enum_encoding default",
            "set_option -resource_sharing 1",
            "set_option -top_module \""+$TOP+"\"",
            "set_option -frequency auto",
            "set_option -run_prop_extract 1",
            "set_option -fanout_limit 12",
            "set_option -globalthreshold 50",
            "set_option -maxfan_hard 0",
            "set_option -disable_io_insertion 0",
            "set_option -retiming 0",
            "set_option -report_path 4000",
            "set_option -opcond Default",
            "set_option -update_models_cp 0",
            "set_option -preserve_registers 0",
            "set_option -symbolic_fsm_compiler 1",
            "set_option -write_verilog 0",
            "set_option -write_vhdl 0",
            "set_option -write_apr_constraint 1",
            "project -result_format \"edif\"",
            "project -result_file \"obj/"+$proj+".edn\"",
            "set_option -vlog_std v2001",
            "set_option -synthesis_onoff_pragma 0",
            "set_option -project_relative_includes 1",
            "impl -active \"obj\"");
           
 
end;



do_synth:#fun
begin
  print_line ("[building run and project scripts for "+$SYNPLIFY+"...]");
  change_dir("..");
  $PROJFILE <= $DESIGN+".prj";
  make_synp_runscript ();
  check ();
  print_line ("[starting "+$SYNPLIFY+"...]");
  print_line ($SYNPLIFY+" -batch "+$PROJFILE);
  $res <= exec_write($LOG,$SYNPLIFY+" -batch "+$PROJFILE);
  if $res = 2 then
  begin
    print_file($OBJDIR+"/"+$DESIGN+".srr");
  end;
  check ();
  append_file($OBJDIR+"/"+$DESIGN+".srr",$DESIGN+".log");
  change_dir($OBJDIR);
end;

make_designer_runscript: #fun
begin
  $PROJFILE <= $DESIGN+".tcl";
  $PROJ <= $proj;
  create_file($PROJFILE);
  --
  -- write: write arguments (append)
  -- write_line: write all arguments in one line (append)
  -- write_lines: write each argument into a new line
  --
  write_line($PROJFILE,
          "new_design -name \""+$PROJ+"\" \\",
          "  -family \""+$DEVICE_CLASS+"\" \\",
          "  -path {"+$PWD+"/"+$OBJDIR+"} \\",
          "  -block \"off\"", 
          "set_design -name \""+$PROJ+"\" \\",
          "  -family \""+$DEVICE_CLASS+"\" \\",
          "  -path {"+$PWD+"/"+$OBJDIR+"} \\",
          "  -block \"off\"", 
          "set_device -die \""+$DEVICE+"\" \\",
          "  -package \""+$DEVICE_PACKAGE+"\" \\",
          "  -speed \"-"+$DEVICE_SPEED+"\" \\",
          "  -voltage \""+$DEVICE_VOLT+"\" \\",
          "  -iostd \"LVTTL\" \\",
          "  -jtag \"yes\" \\",
          "  -probe \"yes\" \\",
          "  -trst \"yes\" \\",
          "  -temprange \"COM\" \\",
          "  -voltrange \"COM\"" ,
          "import_source -format \"edif\" \\",
          "  -edif_flavor \"GENERIC\" \\",
          "  {"+$PWD+"/"+$OBJDIR+"/"+$PROJ+".edn} \\",
          "  -merge_physical \"no\" \\",
          "  -merge_timing \"yes\"", 
          "compile -pdc_abort_on_error \"on\" -reserve_ff_pin \"off\" \\",
          "  -pdc_eco_display_unmatched_objects \"off\" \\",
          "  -pdc_eco_max_warnings 10000 -demote_globals \"off\" \\",
          "  -demote_globals_max_fanout 12 -promote_globals \"off\" \\",
          "  -promote_globals_min_fanout 200 \\",
          "  -promote_globals_max_limit 0 \\",
          "  -localclock_max_shared_instances 12 \\",
          "  -localclock_buffer_tree_max_fanout 12 \\",
          "  -combine_register \"off\" \\",
          "  -delete_buffer_tree \"off\" \\",
          "  -delete_buffer_tree_max_fanout 12 \\",
          "  -report_high_fanout_nets_limit 10", 
          "layout -timing_driven \\",
          "  -run_placer \"on\" \\",
          "  -place_incremental \"off\" \\",
          "  -run_router \"on\" \\",
          "  -route_incremental \"OFF\" \\",
          "  -placer_high_effort \"off\"", 
          "export -format \"bts_stp\" \\",
          "  -feature \"prog_fpga\" \\",
          "  -io_state \"Z\" \\",
          "  {"+$PWD+"/"+$OBJDIR+"/"+$PROJ+".stp}"); 

end;

do_tech:#fun
begin
  print_line ("[building run and project scripts for Actel Designer...]");
  change_dir("..");
  $PROJFILE <= $DESIGN+".tcl";
  make_designer_runscript ();
  check ();
  print_line ("[starting Designer...]");
  print_line ($DESIGNER+" SCRIPT:"+$PROJFILE);
  $res <= exec_write($LOG,$DESIGNER+" SCRIPT:"+$PROJFILE);
  check ();
  change_dir($OBJDIR);
  -- append_file($DESIGN+"_xilinx.log",$DESIGN+".log");
end;


#targets
begin
  init:#target
  begin
    init ();
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
  tech:#target
  begin
    init ();
    do_tech ();
    check ();
    finish ();
  end;
end;
