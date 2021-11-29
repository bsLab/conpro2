
--
-- Synplify(Pro) Synthesis && Xilinx Backends
--

#version "2.05";

#parameter
begin

  $TOP <= "MOD_" + $proj;
  $DEVICE <= get_opt($target_device.[1],"XC2S100");
  $DEVICE_CLASS <= get_opt($target_family.[1],"SPARTAN2");
  $DEVICE_PACKAGE <= get_opt($target_package.[1],"TQ144");
  $DEVICE_SPEED <= get_opt($target_speed.[1],"5");
  $DEVICE_VOLT <= get_opt($target_volt,"1.5");
  $DEVICEX <= $DEVICE+"-"+$DEVICE_PACKAGE+"-"+$DEVICE_SPEED;
  $PROM <= get_opt($target.[2],"xc18v01");
  $CLOCK <= get_opt ($clock,"0");
  $PERIOD <= get_opt ($period,"0");
  $OBJDIR <= "obj";
  $SRCDIR <= ".";
  $DESIGN <= $proj;
  $LOG <= $proj + ".log";
  $DUP <= "tee";
  $PWD <= get_env($PWD,"/tmp");
  $SYNPLICITY <= get_env($SYNPLICITY,"/export/home/fpga_89");
  $XILINX <= get_env($XILINX,"/export/home/Xilinx6");
  $SYNPLIFY <= get_env($SYNPLIFY,$SYNPLICITY + "/bin/synplify_pro");
  $res <= 0;
end;

--
-- Synthesis settings
--
#parameter
begin
  $FREQUENCY <= "auto"; -- $CLOCK;
  $WRITE_VERILOG <= "0";
  $WRITE_VHDL <= "0";
  $WRITE_APC_CONSTRAINT <= "1";
  $RESULT_FORMAT <= "edif";
  $RESULT_FILE_SUFF <= "edn";
  $VLOG_STD <= "v2001";
  $SYNTHESIS_ONOFF_PRAGMA <= "0";
  foreach $opt in rev($syn_synth_set) do
  begin
    $SYNSET.[$I] <= $opt;
  end;
end;

--
-- Technology settings
--

#parameter
begin

  $MAPSET <= get_opts($syn_tech_set_map,"");
  $PARSET <= get_opts($syn_tech_set_par,"");
  $BITGEN_CONF <= get_opt($bitgen_conf,""); 
  $RECLEN <= get_opt($reclen,"2048"); 
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
  write_line($PROJFILE,
            "impl -add obj -type fpga");  
  foreach $file in rev($vhdl) do
  begin
    $DIR <= dirname($file);
    $SRC <= basename($file);
    if $DIR = "." then
      write_line($PROJFILE,
            "add_file -vhdl -lib work \""+$SRC+".vhdl\"")
    else
      write_line($PROJFILE,
            "add_file -vhdl -lib work \""+$DIR+"/"+$SRC+".vhdl\"");
    
  end;
  foreach $opt in $SYNSET do
  begin
    write_line($PROJFILE,
            "set_option "+$opt);
  end;
  write_line($PROJFILE,
            "set_option -technology "+$DEVICE_CLASS,
            "set_option -part "+$DEVICE,
            "set_option -package \"\"",
            "set_option -speed_grade -"+$DEVICE_SPEED,
            "set_option -top_module \""+$TOP+"\"",
            "set_option -write_verilog "+$WRITE_VERILOG,
            "set_option -write_vhdl "+$WRITE_VHDL,
            "set_option -write_apr_constraint "+$WRITE_APC_CONSTRAINT,
            "project -result_format \""+$RESULT_FORMAT+"\"",
            "project -result_file \"obj/"+$proj+"."+$RESULT_FILE_SUFF+"\"",
            "set_option -vlog_std "+$VLOG_STD,
            "set_option -synthesis_onoff_pragma "+$SYNTHESIS_ONOFF_PRAGMA,
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
  if $res = 0 then
  begin
    print_line ("[fixing constraint file...]");
    $NCF <= $DESIGN+".ncf";
    $NCFF <= $DESIGN+".ncff";
    $FIXSH <= "fix.sh";
    create_file($FIXSH);
    write_line($FIXSH,"/usr/bin/sed -e '/TIMESPEC/s/[0-9]*.[0-9]* ns/"+$PERIOD+".000 ns/;/TIMESPEC/s/100.00%/50.00%/' <"+$NCF+">"+$NCFF);
    $res <= exec_write($LOG,"sh "+$FIXSH);
    move_file($NCF,$NCF+".synthesis"); 
  end;
end;

do_trans:#fun
begin
  print_line("[starting edif2ngd...]");
  $res <= exec_write($LOG,
        "edif2ngd -p "+$DEVICEX+" "+$DESIGN+".edn");
  check();
  print_line("[starting ngdbuild...]");
  $res <= exec_write($LOG,
        "ngdbuild -p "+$DEVICEX+" -uc ../"+
            $DESIGN+"_edif_fixed.ucf -dd . "+$DESIGN+".ngo");
  check();
end;

do_map:#fun
begin
    print_line("[starting map...]");
    $res <= exec_write($LOG,"map -p "+ $DEVICEX+ " "+
                    $MAPSET +    
                    " -o "+$DESIGN+"_map.ncd "+  
                    $DESIGN+".ngd "+         
                    $DESIGN+".pcf");
    check();
    append_file($DESIGN+"_map.mrp",$LOG);
end;


do_place:#fun
begin
  print_line("[starting place and route...]");
  $res <= exec_write($LOG,"par -w"+
                  $PARSET + " "+
                  $DESIGN+"_map.ncd "+
                  $DESIGN+".ncd "+
                  $DESIGN+".pcf");
    append_file($DESIGN+".par",$LOG);
end;

do_bitgen:#fun
begin
  print_line("[starting bitgen...]");
  $BITCONF <= $DESIGN+".bitgen_conf";
  create_file($BITCONF);
  write($BITCONF,$BITGEN_CONF);
  $res <= exec_write($LOG,"bitgen -w"+
                  " -f "+$BITCONF+" "+
                  $DESIGN+".ncd");
  copy_file($DESIGN+".bit","../.");
end;

do_ucf:#fun
begin
  print_line("[creating ucf file...]");
  $UCF <= "../"+$proj + ".ucf";
  $UCFF <= "../"+$proj + "_edif_fixed.ucf";
  if exist($UCF) = 1 then
  begin
    print_line ("Keeping existing constraint file " +$UCF);
  end
  else
  begin
    create_file($UCF);
    $IND <= 1;
    foreach $signal in $port do
    begin
      write_line($UCF,port($signal,"xilinx::ucf"));
    end;  
  end;
  print_line("[creating edif vector fixed ucf file...]");
  $FIXSH <= "fix.sh";
  create_file($FIXSH);
  write_line($FIXSH,"/usr/bin/sed -e 's/\"\([a-zA-Z_]*\)<\([0-9]*\)>\"/\1(\2)/' <"+$UCF+">"+$UCFF); 
  $res <= exec_write($LOG,"sh "+$FIXSH);
end;

do_prom:#fun
begin
  print_line("[creating xsvf prom file...]");
  remove_file($DESIGN+"_"+$PROM+".xsvf",
              $DESIGN+"_"+$PROM+".svf",
              $DESIGN+"_"+$PROM+".bit");
  $CMD <= "impact.cmd";
  create_file($CMD);
  write_line ($CMD,
    "setMode -pff",
    "setsubMode -pffserial",
    "addpromdevice -p 1 -name "+$PROM,
    "addCollection -name "+$DESIGN,
    "addDesign -version 0 -name 0000",
    "addDeviceChain -index 0",
    "addDevice -p 1 -file "+$DESIGN+".bit",
    "generate",
    "quit");
  $res <= exec_write($LOG,"impact -batch "+$CMD);
  check ();
  create_file($CMD);
  write_line ($CMD,
    "setMode -bsfile",
    "setCable -port svf -file "+$DESIGN+"_"+$PROM+".svf",
    "addDevice -p 1 -part "+$PROM,
    "setAttribute -position 1 -attr configFileName -value "+$DESIGN+".mcs",
    "identify",
    "program -e -p 1 -v",
    "quit");
  $res <= exec_write($LOG,"impact -batch "+$CMD);
  check ();
  create_file($CMD);
  write_line ($CMD,
    "svf2xsvf  -i "+$DESIGN+"_"+$PROM+".svf -o "+$DESIGN+"_"+$PROM+".xsvf",
    "quit");
  $res <= exec_write($LOG,"impact -batch "+$CMD);
  copy_file($DESIGN+"_"+$PROM+".bit","../.");
  copy_file($DESIGN+"_"+$PROM+".xsvf","../.");
end;

do_fpga:#fun
begin
  print_line("[creating xsvf fpga file...]");
  remove_file($DESIGN+"_"+$DEVICEX+".xsvf",
              $DESIGN+"_"+$DEVICEX+".svf",
              $DESIGN+"_"+$DEVICEX+".bit");
  $CMD <= "impact.cmd";
  create_file($CMD);
  write_line ($CMD,
    "setMode -bsfile",
    "setCable -port svf -file "+$DESIGN+"_"+$DEVICEX+".svf",
    "addDevice -p 1 -file "+$DESIGN+".bit",
    "identify",
    "program -e -p 1",
    "quit");
  $res <= exec_write($LOG,"impact -batch "+$CMD);
  check ();
  create_file($CMD);
  write_line ($CMD,
    "svf2xsvf -fpga -r 0 -rlen "+$RECLEN+" -i "+$DESIGN+"_"+$DEVICEX+".svf -o "+$DESIGN+"_"+$DEVICEX+".xsvf",
    "quit");
  $res <= exec_write($LOG,"impact -batch "+$CMD);
  copy_file($DESIGN+"_"+$DEVICEX+".xsvf","../.");
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
    do_ucf ();
    check ();
    do_trans ();
    check ();
    do_map ();
    check ();
    do_place ();
    check ();
    do_bitgen ();
    check ();
    do_prom ();
    check ();
    do_fpga ();
    check ();
    finish ();
  end;
  ucf:#target
  begin
    init ();
    do_ucf();
    check (); 
  end;
end;
