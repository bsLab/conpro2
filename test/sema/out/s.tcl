set_working_dir /home/sbosse/proj/conpro2/test/sema/out
set hierarchy_flatten TRUE
set output_file s.vhd
set novendor_constraint_file FALSE
set bubble_tristates FALSE
set encoding binary
load_library sxlib
read -dont_elaborate {../s_main.vhdl ../s_p_3.vhdl ../s_p_2.vhdl ../s_p_1.vhdl ../s_p_0.vhdl ../s.vhdl }
pd
read -technology sxlib {../s_main.vhdl ../s_p_3.vhdl ../s_p_2.vhdl ../s_p_1.vhdl ../s_p_0.vhdl ../s.vhdl }
pre_optimize -common_logic -unused_logic -boundary -xor_comparator_optimize
pre_optimize -extract
pd
optimize .work.MOD_s.main -target sxlib -macro -area -effort quick -hierarchy flatten
optimize_timing .work.MOD_s.main
report_area -cell_usage -hierarchy -all_leafs
report_delay  -num_paths 1 -critical_paths -clock_frequency
auto_write -format VHD s.vhd
