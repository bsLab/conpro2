new_design -name "t" \
  -family "proasic3" \
  -path {/home/sbosse/proj/conpro2/test/tdi.synplicity89/out/obj} \
  -block "off"
set_design -name "t" \
  -family "proasic3" \
  -path {/home/sbosse/proj/conpro2/test/tdi.synplicity89/out/obj} \
  -block "off"
set_device -die "a3p125" \
  -package "144 FBGA" \
  -speed "-2" \
  -voltage "1.5" \
  -iostd "LVTTL" \
  -jtag "yes" \
  -probe "yes" \
  -trst "yes" \
  -temprange "COM" \
  -voltrange "COM"
import_source -format "edif" \
  -edif_flavor "GENERIC" \
  {/home/sbosse/proj/conpro2/test/tdi.synplicity89/out/obj/t.edn} \
  -merge_physical "no" \
  -merge_timing "yes"
compile -pdc_abort_on_error "on" -reserve_ff_pin "off" \
  -pdc_eco_display_unmatched_objects "off" \
  -pdc_eco_max_warnings 10000 -demote_globals "off" \
  -demote_globals_max_fanout 12 -promote_globals "off" \
  -promote_globals_min_fanout 200 \
  -promote_globals_max_limit 0 \
  -localclock_max_shared_instances 12 \
  -localclock_buffer_tree_max_fanout 12 \
  -combine_register "off" \
  -delete_buffer_tree "off" \
  -delete_buffer_tree_max_fanout 12 \
  -report_high_fanout_nets_limit 10
layout -timing_driven \
  -run_placer "on" \
  -place_incremental "off" \
  -run_router "on" \
  -route_incremental "OFF" \
  -placer_high_effort "off"
export -format "bts_stp" \
  -feature "prog_fpga" \
  -io_state "Z" \
  {/home/sbosse/proj/conpro2/test/tdi.synplicity89/out/obj/t.stp}
