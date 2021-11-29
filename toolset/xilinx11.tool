
--
-- Xilinx ISE Version 9 Tool Description
--

#version "2.05";

#parameter
begin

  $TOP <= "MOD_" + $proj;
  $DEVICE <= get_opt($target.[1],"xc2s100-tq144");
  $SPEED <= get_opt($target_speed.[1],"5");
  $PROM <= get_opt($target.[2],"xc18v01");
  $OBJDIR <= "obj";
  $SRCDIR <= ".";
  $DESIGN <= $proj;
  $LOG <= $proj + ".log";
  $DUP <= "tee";
  $XILINX <= get_env($XILINX,"/opt/Xilinx9");

  $SCRAM <= get_env($SCRAM,"/opt/alliance/bin/scram");
  $res <= 0;
end;

#parameter
begin
  -- VHDL sources
  foreach $file in $vhdl do
  begin
    $VHDL.[$I] <= chop_extension($file);
  end;
end;


--
-- Synthesis & Technology settings
--

#parameter
begin

  $SYNSET <= get_opts($syn_synth_set,"");
  $MAPSET <= get_opts($syn_tech_set_map,"");
  $PARSET <= get_opts($syn_tech_set_par,"");
  $BITGEN_CONF <= get_opt($bitgen_conf,""); 
  $RECLEN <= get_opt($reclen,"2048"); 
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

make_xst_runscript: #fun
begin
  create_file($DESIGN+".xst_run");
  --
  -- write: write arguments (append)
  -- write_line: write all arguments in one line (append)
  -- write_lines: write each argument into a new line
  --
  $CMD <=  "run "+
       "-ifn "+$DESIGN+".prj "+
       "-ifmt mixed "+
       "-ofn "+$DESIGN+" "+
       "-ofmt NGC "+
       "-p "+$DEVICE+" "+
       $SYNSET + " " +
       "-top "+$TOP;
  write_line($DESIGN+".xst_run",$CMD);       
end;

make_proj:#fun
begin
  $PROJFILE <= $DESIGN+".prj";
  create_file ($PROJFILE);

  foreach $file in $vhdl do
  begin
    $DIR <= dirname($file);
    $FILE <= basename($file);
    if $DIR = "." then
      write_line($PROJFILE,"vhdl work ../"+$FILE+".vhdl")
    else
      write_line($PROJFILE,"vhdl work "+$DIR+"/"+$FILE+".vhdl");
  end;
end;

do_synth:#fun
begin
  print ("[building run and project scripts...]");
  make_xst_runscript ();
  check ();
  make_proj ();
  check ();
  print ("[starting xst...]");
  $res <= exec("xst -ifn "+$DESIGN+".xst_run -ofn "+$DESIGN+"_xilinx.log");
  check ();
  append_file($DESIGN+"_xilinx.log",$DESIGN+".log");
end;


do_scram:#fun
begin
  foreach $file in $VHDL do
  begin
    remove_file("work._savant_lib");
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
    remove_file("work._savant_lib");
  end;
end;

do_trans:#fun
begin
  print_line("[starting ngdbuild...]");
  $res <= exec_write($LOG,
        "ngdbuild -p "+$DEVICE+" -uc ../"+
            $DESIGN+".ucf -dd . "+$DESIGN+".ngc "+$DESIGN+".ngd");
  check();
end;

do_map:#fun
begin
    print_line("[starting map...]");
    $res <= exec_write($LOG,"map -p "+ $DEVICE+ " "+
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
  $res <= exec_write($LOG,"par -w "+
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
end;

do_prom:#fun
begin
  print_line("[creating xsvf prom file...]");
  remove_file($DESIGN+"_"+$PROM+".xsvf",
              $DESIGN+"_"+$PROM+".svf");
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
  copy_file($DESIGN+"_"+$PROM+".xsvf","../.");
end;

do_fpga:#fun
begin
  print_line("[creating xsvf fpga file...]");
  remove_file($DESIGN+"_"+$DEVICE+".xsvf",
              $DESIGN+"_"+$DEVICE+".svf",
              $DESIGN+"_"+$DEVICE+".bit");
  $CMD <= "impact.cmd";
  create_file($CMD);
  write_line ($CMD,
    "setMode -bsfile",
    "setCable -port svf -file "+$DESIGN+"_"+$DEVICE+".svf",
    "addDevice -p 1 -file "+$DESIGN+".bit",
    "identify",
    "program -e -p 1",
    "quit");
  $res <= exec_write($LOG,"impact -batch "+$CMD);
  check ();
  create_file($CMD);
  write_line ($CMD,
    "svf2xsvf -fpga -r 0 -rlen "+$RECLEN+" -i "+$DESIGN+"_"+$DEVICE+".svf -o "+$DESIGN+"_"+$DEVICE+".xsvf",
    "quit");
  $res <= exec_write($LOG,"impact -batch "+$CMD);
  copy_file($DESIGN+"_"+$DEVICE+".xsvf","../.");
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
  svf:#target
  begin
    init ();
    do_prom ();
    check ();
    do_fpga ();
    check ();
    finish ();    
  end;
  ucf:#target
  begin
    init ();
    do_ucf ();
    check ();
  end;
end;
