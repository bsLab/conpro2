
--
--

#parameter
begin

  $TOP <= "MOD_" + $proj;
  $DEVICE <= $device;
  $SPEED <= $speed;
  $PROM <= $prom;
  $OBJDIR <= "obj";
  $SRCDIR <= ".";
  $DESIGN <= $proj;
  $LOG <= $TOP + ".log";
  $DUP <= "tee";
  $XILINX <= get_env("$XILINX","/opt/Xilinx9");

  $SCRAM <= get_env("$SCRAM","/opt/alliance/bin/scram");
  $res <= 0;
end;

#parameter
begin
  -- VHDL sources
  foreach $vhdl do
  begin
    $VHDL.[$i] <= chop_extension($vhdl);
  end;
end;

#parameter
begin
  $FSM_EXTRACT <= "YES";
  --FSM Encoding Algorithm and style
  $FSM_ENCODING <= "Sequential";
  $FSM_STYLE <= "lut";
  -- Mux Extraction and style
  $MUX_EXTRACT <= "YES";
  $MUX_STYLE <= "Auto";
  -- RAM/ROM settings
  $RAM_EXTRACT <= "Yes";
  $RAM_STYLE <= "Block";
  $ROM_EXTRACT <= "Yes";
  $ROM_STYLE <= "Auto";
  -- Multiplier style
  $MULT_STYLE <= "lut";
  -- Some more extractions
  $PRIO_EXTRACT <= "Yes";
  $SHREG_EXTRACT <= "Yes";
  $DECODER_EXTRACT <= "Yes";
  $SHIFT_EXTRACT <= "Yes";
  -- Optimization Goal
  $OPT <= "speed";
  -- Optimization Effort
  $OPTLEV <= "1";
  -- Add IO Buffers
  $IOBUF <= "YES";
  $IOB <= "Auto";
  $MAX_FANOUT <= "100";
  -- Mapping constrains
  $COVER_MODE <= "area";
  $PACK_REG <= "b";
  $MAP_IN_FUN <= "4";
  $PACK_CLB <= "100";
  $TRANS_BUSES <= "off";
  -- Place and route constraints
  $PAR_OPT <= "std";
  $PLACE_COST <= "1";
  -- MISC
  $IUC <= "NO";
  $GLOBOPT <= "AllClockNets";
  $XOR_COLLAPSE <= "Yes";
  $RES_SHARE <= "Yes";
  $BUFG <= "4";
  $OPT_PRIM <= "No";
  $TRI_2_LOGIC <= "No";
  -- Slice management
  $SLICE_RATIO <= "100";
  $SLICE_PACK <= "Yes";
  $SLICE_MAXMAR <= "5";
  -- Register logic
  $REG_DUPL <= "Yes";
  $REG_BAL <= "No";
  $REG_EQ_REM <= "Yes";
  -- XSVF maximal bit length
  $RECLEN <= "1024";
  -- Bitgen FPGA configuration settings
  $BITGEN_CONF_XC2 <= 
    "-g Gclkdel0:11111 "+
    "-g Gclkdel1:11111 "+
    "-g Gclkdel2:11111 "+
    "-g Gclkdel3:11111 "+
    "-g ConfigRate:4";
  $BITGEN_CONF_COM <= 
    "-g DebugBitstream:No "+
    "-g Binary:no "+
    "-g CclkPin:PullUp "+
    "-g M0Pin:PullUp "+
    "-g M1Pin:PullUp "+
    "-g M2Pin:PullUp "+
    "-g ProgPin:PullUp "+ 
    "-g DonePin:PullUp "+
    "-g TckPin:PullUp "+
    "-g TdiPin:PullUp "+
    "-g TdoPin:PullUp "+
    "-g TmsPin:PullUp "+
    "-g UnusedPin:PullDown "+
    "-g UserID:0xFFFFFFFF "+
    "-g StartUpClk:CClk "+
    "-g DONE_cycle:4";

  if $DEVICE = "xc2" then
    $BITGEN_CONF <= $BITGEN_CONF_COM + $BITGEN_CONF_XC2
  else
    $BITGEN_CONF <= $BITGEN_CONF_COM;  
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
  open_file($LOG);
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
       "-opt_mode "+$OPT+" "+
       "-opt_level "+$OPTLEV+" "+
       "-iobuf "+$IOBUF+" "+
       "-iob "+$IOB+" "+
       "-fsm_extract "+$FSM_EXTRACT+" "+
       "-fsm_encoding "+$FSM_ENCODING+" "+
       "-fsm_style "+$FSM_STYLE+" "+
       "-mux_extract "+$MUX_EXTRACT+" "+
       "-mux_style "+$MUX_STYLE+" "+
       "-ram_extract "+$RAM_EXTRACT+" "+
       "-ram_style "+$RAM_STYLE+" "+
       "-rom_extract "+$RAM_EXTRACT+" "+
       "-rom_style "+$RAM_STYLE+" "+
       "-decoder_extract "+$DECODER_EXTRACT+" "+
       "-priority_extract "+$PRIO_EXTRACT+" "+
       "-shreg_extract "+$SHREG_EXTRACT+" "+
       "-shift_extract "+$SHIFT_EXTRACT+" "+
       "-xor_collapse "+$XOR_COLLAPSE+" "+
       "-mult_style "+$MULT_STYLE+" "+
       "-resource_sharing "+$RES_SHARE+" "+
       "-slice_utilization_ratio "+$SLICE_RATIO+" "+
       "-slice_utilization_ratio_maxmargin "+$SLICE_MAXMAR+" "+
       "-slice_packing "+$SLICE_PACK+" "+
       "-max_fanout "+$MAX_FANOUT+" "+
       "-bufg "+$BUFG+" "+
       "-register_duplication "+$REG_DUPL+" " +
       "-register_balancing "+$REG_BAL+" "+
       "-equivalent_register_removal "+$REG_EQ_REM+" "+
       "-optimize_primitives "+$OPT_PRIM+" "+
       "-tristate2logic "+$TRI_2_LOGIC+" "+
       "-top "+$TOP+" "+
       "-iuc "+$IUC+" "+
       "-glob_opt "+$GLOBOPT;
  write_line($DESIGN+".xst_run",$CMD);       
end;

make_proj:#fun
begin
  $PROJFILE <= $DESIGN+".prj";
  create_file ($PROJFILE);

  foreach $vhdl do
  begin
    $SRC <= basename($vhdl) + ".vhdl";
    write_line($PROJFILE,"vhdl work"+$SRC); 
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
  $res <= exec("xst -ifn "+$DESIGN+".xst_run -ofn "+"$DESIGN"+"_xilinx.log");
  check ();
  append_file($DESIGN+"_xilinx.log",$DESIGN+".log");
end;

do_finish:#fun
begin
  change_dir("-");
end;

do_scram:#fun
begin
  foreach $vhdl do
  begin
    remove_file("work._savant_lib");
    $SRC <= basename($vhdl)+".vhdl";
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
    $res <= exec_write($LOG,"map -p "+ $DEVICE+
                    " -cm "+$COVER_MODE+     
                    " -pr "+$PACK_REG+       
                    "-k "+$MAP_IN_FUN+      
                    "-c "+$PACK_CLB+        
                    "-tx "+$TRANS_BUSES+    
                    "-o "+$DESIGN+"_map.ncd"+  
                    $DESIGN+".ngd"+         
                    $DESIGN+".pcf");
    check();
    append_file($DESIGN+".rpt",$LOG);
end;


do_place:#fun
begin
  print_line("[starting bitgen...]");
  $BITCONF <= $DESIGN+".bitgen_conf";
  create_file($BITCONF);
  write($BITCONF,$BITGEN_CONF);
  $res <= exec_write($LOG,"bitgen -w "+
                  "-f "+$BITCONF+" "+
                  $DESIGN+".ncd");
  copy_file($DESIGN+".bit","../.");
end;


#targets
begin
  init:#target
  begin
    init ();
    check ();
  end;
  vhdl:#target
  begin
    init ();
    change_dir($OBJDIR);
    do_scram ();
    check ();
  end;
  synth:#target
  begin
    init ();
    change_dir($OBJDIR);
    do_synth ();
    check ();
  end;
end;
