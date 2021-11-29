set_working_dir /home/sbosse/proj/conpro2/test/mutex/out
set hierarchy_flatten TRUE
set output_file m.vhd
set novendor_constraint_file FALSE
set bubble_tristates FALSE
set encoding binary
load_library sxlib
read -dont_elaborate {../m_main.vhdl ../m_p_3.vhdl ../m_p_2.vhdl ../m_p_1.vhdl ../m_p_0.vhdl ../m.vhdl }
pd
read -technology sxlib {../m_main.vhdl ../m_p_3.vhdl ../m_p_2.vhdl ../m_p_1.vhdl ../m_p_0.vhdl ../m.vhdl }
pre_optimize -common_logic -unused_logic -boundary -xor_comparator_optimize
pre_optimize -extract
pd
optimize .work.MOD_m.main -target sxlib -macro -area -effort quick -hierarchy flatten
optimize_timing .work.MOD_m.main
report_area -cell_usage -hierarchy -all_leafs
report_delay  -num_paths 1 -critical_paths -clock_frequency
auto_write -format VHD m.vhd
