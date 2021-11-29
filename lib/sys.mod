--
--      ==================================
--      OOOO   OOOO OOOO  O      O   OOOO
--      O   O  O    O     O     O O  O   O
--      O   O  O    O     O     O O  O   O
--      OOOO   OOOO OOOO  O     OOO  OOOO
--      O   O     O    O  O    O   O O   O
--      O   O     O    O  O    O   O O   O
--      OOOO   OOOO OOOO  OOOO O   O OOOO
--      ==================================
--      BSSLAB, Dr. Stefan Bosse, http://www.bsslab.de
--
--      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
--                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
--                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
--                 OUTSIDE OF THE SOFTWARE SYSTEM.
--
--    $AUTHORS:     Stefan Bosse
--    $INITIAL:     (C) 2006-2009 BSSLAB
--    $CREATED:     10.12.2008
--    $VERSION:     #version
--
--    $INFO:
--
--  CONPRO EMI library
--
--
-- Implements dummy system object
--
--
--    $ENDOFINFO
--

#version "2.08";


--
-- parameters used, allowed and default values
--
#parameter
begin
  --
  -- Clock and reset settings
  --
  $clock <= 10 megahz;
  $clock_level <= 1;
  $reset_level <= 1;
  --
  -- No external reset ?
  --
  $reset_internal <= 0;
  
  --
  -- State encoding
  --
  $state_enc ["binary","onehot","gray","auto"] <= "binary";
  
  --
  -- Target devices
  ---
  $targets;
  --
  -- Simulation: number of clock cycles and simulation resolution.
  --

  $simu_cycles <= 50;
  $simu_res <= 5;
  
  --
  -- Expression model, default case
  --
  $expr_type ["flat","binary","alu","top"] <= "flat"; 
  
  --
  -- Expression model, default case
  --
  $scheduler_type ["basicblock","refstack","expression","default"] <= "default"; 

  --
  -- Synthesis tool
  --
  $synth_tool;
  
  --
  -- ConPro logic-type to VHDL-type mapping
  --
  $logic_type <= "std_logic";
  
  --
  -- ALU settings?
  --
  $alu_top_expr <= 1;
  $alu_thres <= 8;
end;


--
-- Supported object methods
--
#methods
begin
  clock (#rhs:natural);
  clock_level (#rhs:logic);
  reset_level (#rhs:logic);
  reset_internal (#rhs:bool);
  simu_cycles (#rhs:natural);
  simu_res (#rhs:natural);
  target(#rhs:string);
  expr_type(#rhs:string);
  schedule(#rhs:string);
end;

--#assert
--begin
--end;


clock: #access
begin
  #set
  begin
    $clock <= $ARG1;
  end;
end;

clock_level: #access
begin
  #set
  begin
    $clock_level <= $ARG1;
  end;
end;

reset_level: #access
begin
  #set
  begin
    $reset_level <= $ARG1;
  end;
end;

reset_internal: #access
begin
  #set
  begin
    $reset_internal <= $ARG1;
  end;
end;

target: #access
begin
  #set
  begin
    $targets <= $ARG1;
  end;
end;

simu_cycles: #access
begin
  #set
  begin
    $simu_cycles <= $ARG1;
  end;
end;

simu_res: #access
begin
  #set
  begin
    $simu_res <= $ARG1;
  end;
end;

expr_type: #access
begin
  #set
  begin
    $expr_type <= $ARG1;
  end;
end;

schedule: #access
begin
  #set
  begin
    $scheduler_type <= $ARG1;
  end;
end;
