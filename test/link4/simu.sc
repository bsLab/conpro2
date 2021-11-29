--
-- Project settings (project = top module = top file)
--
--  Usage: simu.sc project=<project> path=<obj-path> [clock_cycles=<clock_cycles> ...]
--
--  Input: <project>.vhd
--
--  Intermediate: <project>.pat
--
--  Output: <project>.vst
--          <project>_simu.pat
--
lookup sica /opt/Sica-1
lookup sicabin [concat [get sica] /bin/]
lookup sxlib [concat [get sica] /sxlib/]
lookup project l2_simu
lookup path out/obj/
lookup clock_cycles 1900
lookup reset_cycles 2
lookup time_unit ps
lookup lib sxlib

--
-- Two clock domains: 
--   assert (period1 >= period2)
--
lookup period1 100000
lookup period2 89000
lookup resolution 25000
--

-- Comment out for auxilliary internal signals
set monitors Lc1_LINK_ln_DOUT_CNT_0 Lc1_LINK_ln_DOUT_CNT_1 Lc1_LINK_ln_DOUT_CNT_2 Lc1_LINK_ln_DOUT_CNT_3
set monitors [get monitors] Lc1_LINK_ln_DIN_CNT_0 Lc1_LINK_ln_DIN_CNT_1 Lc1_LINK_ln_DIN_CNT_2 Lc1_LINK_ln_DIN_CNT_3
set monitors [get monitors] Lc2_LINK_ln_DOUT_CNT_0 Lc2_LINK_ln_DOUT_CNT_1 Lc2_LINK_ln_DOUT_CNT_2 Lc2_LINK_ln_DOUT_CNT_3
set monitors [get monitors] Lc2_LINK_ln_DIN_CNT_0 Lc2_LINK_ln_DIN_CNT_1 Lc2_LINK_ln_DIN_CNT_2 Lc2_LINK_ln_DIN_CNT_3
-- set monitors [get monitors] Lc1_DEV_ln_din_ack Lc2_DEV_ln_din_ack
set monitors [get monitors] Lc1_LINK_ln_state_out_0 Lc1_LINK_ln_state_out_1
set monitors [get monitors] Lc2_LINK_ln_state_out_0 Lc2_LINK_ln_state_out_1
set monitors [get monitors] Lc1_LINK_ln_state_in_0 Lc1_LINK_ln_state_in_1
set monitors [get monitors] Lc2_LINK_ln_state_in_0 Lc2_LINK_ln_state_in_1
set monitors [get monitors] Lc1_LINK_ln_BUSY Lc2_LINK_ln_BUSY
set monitors [get monitors] Lc1_LINK_ln_DIN_REG_0 Lc1_LINK_ln_DIN_REG_1 Lc1_LINK_ln_DIN_REG_2
set monitors [get monitors] Lc2_LINK_ln_DIN_REG_0 Lc2_LINK_ln_DIN_REG_1 Lc2_LINK_ln_DIN_REG_2

--
set PATH [get path]
set file [get project]
set top [get project]
set bufcomp buf_x2
set vhd_unit [to_mod [get file]]
set pat [get project]
set pat_unit [to_mod [get pat]]
set simu [concat [get project] _simu]
set simu_unit [to_mod [get simu]]
set halfperiod1 [div [get period1] 2]
set halfperiod2 [div [get period2] 2]

set start_time [time]

: make_structs
  read [concat [get sica] /lib/ [get lib] .db]
  mkdir /Pat
  mkdir /Set
;

-- 
-- 1. VHD -> VST synthesis 
--
: read_vst
  read [postfix .vhd [get file]]
  vhd resolve [concat / [get vhd_unit]]
  rename [get top] [concat / [get vhd_unit] /MOD_ [get top]]
;


: make_comps
  set comps [vhd components [get vhd_unit]]
  if [not [member [[get bufcomp] [get comps]]]] [set comps [get comps] [get bufcomp]] 
;

: post-synth
  vhd beh_to_struct [get vhd_unit] /sxlib/cells/buf_x2
  vhd add_component_map [get vhd_unit] "vdd,vdd" "vss,vss" [get comps]
  vhd add_port_decl [get vhd_unit]"vdd: in bit" "vss: in bit"
  vhd add_component_decls [get vhd_unit] /sxlib [get comps] 
  vhd type_conv std_logic bit [concat / [get vhd_unit] /entity/port_decl/.*]
  vhd type_conv std_logic bit [concat / [get vhd_unit] /architecture/declarations/.*]
  write [postfix .vst [get file]] [get vhd_unit]
;

--
-- 2. PAT generator
--

--
-- Create pattern vhd_unit, derive pattern signal from VHD vhd_unit
-- 
: make_pat
  cd /Pat
  pat make [to_mod [get pat]] [get time_unit] [get resolution] [concat / [get vhd_unit]]
  cd [get pat_unit]
  lowercase [dir -l]
  print "Pattern vhd_unit created."
  --
  -- Create clock and reset signals
  --
  print "Creating clock and reset signals..."
  set time_end [add [mul [get clock_cycles] [get period1]] [get halfperiod1]]
  pat event_seq clk1 0 [mul [get period1] [get clock_cycles]] [get period1] 0 1 [get halfperiod1] 0 
  pat event_seq clk2 0 [mul [get period2] [get clock_cycles]] [get period2] 0 1 [get halfperiod2] 0 
  pat event reset 0 1 [mul [get reset_cycles] [get period1]] 0 [get time_end] 0 
  pat event clk2 [get time_end] 0 
  cd ..
  pat write [postfix .pat [get pat]] [get pat_unit]
;

--
-- 3. Gate-level simulator (AsiMUT)  VST,PAT -> PAT'
--

: do_simu
  exec bash [concat [get sicabin] power.sh] [get PATH] [get project]
;

--
-- For debugging: read simualted patterns back, do extract from set file
-- additional internal signals and create extended pattern file.
--
: read_pat
  cd /Set
  read [postfix .set [get simu]]
  set round [get simu_unit] 1000
  cd /Pat
  read [postfix .pat [get simu]]
  pat round [get simu_unit] 1000
  cd /
  copy_attr [concat /Pat/ [get vhd_unit]] [concat /Set/ [get vhd_unit]]
;

: make_monitor
  print_line "Adding monitor signal " [get monitor] " to pattern unit " [concat [get simu] _ext]] "..." 
  pat pattern_from_signal [get monitor] [concat / [get vhd_unit]]
  pat_set events [get monitor] [concat /Set/ [get simu_unit]]
  pat extend  [concat /Pat/ [get simu_unit]] [get monitor]
;

: write_pat
  cd /Pat
  cd [get simu_unit]
  foreach monitor monitors [make_monitor]
  cd ..
  pat write [postfix .pat [concat [get simu] _ext]] [get simu_unit]
;

make_structs
read_vst
make_comps
post-synth
make_pat
do_simu
read_pat
write_pat 

set stop_time [time]
print line "Total time consumed: " [sub [get stop_time] [get start_time]] "second(s)."

