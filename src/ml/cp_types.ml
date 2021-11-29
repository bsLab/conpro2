(*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse, http://www.bsslab.de
**
**      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
**                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
**                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
**                 OUTSIDE OF THE SOFTWARE SYSTEM.
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2006-2014 BSSLAB
**    $CREATED:     1.3.2006
**    $VERSION:     2.16
**
**    $INFO:
**
** Structural program representation, common type definitions.
**
**
**    $ENDOFINFO
**
*)

open Cp_syntax

(*
** <DT>
*)
type data_id =
  | T_logic
  | T_int
  | T_str
  | T_char
  | T_bool
  | T_object
  | T_natural

type data_type = 
    (*
    ** VHDL: std_logic_vector (unsigned)
    *)
    | DT_logic of int                (* logic width      *)
    (*
    ** VHDL: signed
    *)
    | DT_int of int                  (* int width        *)

    | DT_string of int               (* char length      *)
    | DT_char
    (*
    ** VHDL: std_logic/bit
    *)
    | DT_bool
    (*
    ** special
    *)
    | DT_object of string
    
    (*
    ** Only for conversion purposes
    *)
    | DT_lneg
    | DT_aneg
    | DT_bneg

    (*
    ** Natural numbers, required for example in VHDL array selectors (VHDL integer!)
    *)
    | DT_natural of int

(*
** Port types
*)
and port_type = 
    PT_in
  | PT_out
  | PT_bus

(*
** <V>
*)
and value = 
    | V_int of int64        (* Up to 64 bits are supported!     *)
    | V_float of float
    | V_string of string
    | V_char of char        (* ASCII code                       *)
    | V_logic of string     (* logic set {0,1,H,L,Z,X}          *)
    | V_bool of bool        (* true/false                       *)
    | V_list of value list
    | V_z
    | V_time of (int64*Cp_syntax.timeunit)
    | V_freq of (int64*Cp_syntax.frequnit)
    | V_null

    
(*
** Model parameters, mainly used for block RAMs.
** One data storage block, e.g. the RAM with data word width w and
** size s, for example 8x512 bits. Width and size can be expanded
** dynamically during the compilation process.
*)
and model_params = 
  (*
  ** Read mode
  *)
  | Mp_readsync
  | Mp_readasync
  (*
  ** Byte order
  *)
  | Mp_bigendian
  | Mp_littleendian
  (*
  ** Exclusive (but guarded) read and write access (single port).
  *)
  | Mp_EREW
  (*
  ** Concurrent read (dual port) and exclusive write access.
  *)
  | Mp_CREW
  (*
  ** RAM is inlined or not in arbiter 
  *)
  | Mp_inline
  | Mp_outline
  (*
  ** RAM ports
  *)
  | Mp_singleport
  | Mp_dualport
  | Mp_multiport

  (*
  ** Process scheduling policy fifo/static
  *)
  | Mp_schedule of string
and data_block = {
    mutable db_name : string;
    mutable db_rules: rl_module option;
    (*
    ** Default RAM data word width. Data cells may be of any width
    ** lower or equal b_width!
    *)
    mutable db_width : int;  
    (*
    ** Maximal size in word units.
    *)
    mutable db_size : int;

    (*
    ** Member objects of this data block
    *)
    mutable db_objs : object_type list;

    (*
    ** Technology parameters
    *)
    mutable db_params : model_params list;

    (*
    ** Clock domain
    *)
    mutable db_domain: string;
  	(*
	** Some abstract object flags like additional guards (name,value).
	*)
    mutable db_flags : (string*string) list;
    
}

(*
** Object guard (aka. the mutex)
*)
and guard_type =
    | GD_rd
    | GD_wr
    | GD_ext    (* external user supplied guard with mutex *)
and guard = {
    (*
    ** Guard name: guard_<module>_<object>
    *)
    gd_name : string;
    (*
    ** Processes needing the guard
    *)
    gd_procs: process list;

    (*
    ** Used for
    *)
    gd_req: guard_type list;
}
and obj_flags = 
    | Obj_created
    | Obj_temp
    | Obj_inuse
    | Obj_local
    | Obj_port_in 
    | Obj_port_out 
    | Obj_port      (* exported object; signal direction actually unk. *)
    | Obj_mon
    | Obj_loop
    | Obj_array (*  Object of a block or dynamic selected array *)
    | Obj_no_guard
    | Obj_schedule of string
(*
** Array support
*)
and at_flags = 
  | AT_dyn      (* dynamic selector access      *)
  | AT_block    (* RAM block                    *)
  | AT_static   (* only static selector allowed *)
  | AT_temp     (* temporary only               *) 
and array_type = {
    mutable at_name: string;
    (*
    ** The object list contains:
    **  [ot] when OT_var
    **  [ot1;ot2;...;otN] when OT_reg/OT_sig...
    **  The latter case means: all array elements are treated 
    **  seperately.
    *)
    mutable at_objs: object_type array;
    (*
    ** Size of array (one- or multidimensional)
    *)
    mutable at_dim : int array;
    
    mutable at_flags: at_flags list;

    (*
    ** Core methods can be applied to native type arrays!
    ** Not applicable to arrays of abstract objects!!
    *)
    mutable at_ao: abstract_object;
}
and struct_type = {
  (*
  ** Object name, not type name!
  *)
  mutable st_name: string;      
  mutable st_type: type_struct;
  mutable st_objs: object_type list;
  (*
  ** Component elements can be direclt
  ** connected (mapped) to other
  ** signals
  *)
  mutable st_connect: object_type list;
  
  (*
  ** Array element?
  *)
  mutable st_array : array_type option;
}
(*
** Core objects
*)
and core_object = {
    (*
    ** Object name
    *)
    co_name:   string;     

    (*
    ** Object tree is spanned by objects -> processes -> modules
    ** Toplevel objects and exported process objects directly
    ** belongs to modules without a process.
    *)
    mutable co_process: process option;
    (*
    ** Each object belongs to a content module.
    *)
    mutable co_module: cp_module;
    (*
    ** Each object belongs to a rule module.
    *)
    mutable co_rules: rl_module option;

    (*
    ** data type of object      
    *)

    mutable co_type:   data_type;   (* data type of object      *)
    
    (*
    ** Associated symbolic type name (Type_const == DT_int...)
    *)
    mutable co_type_name: string;
    
    (*
    ** Block level needed for resource sharing
    **
    **  L0
    **  begin
    **      -- L=1
    **      begin
    **          -- L=2
    **      end
    **      begin
    **          -- L=2
    **      end
    **  end
    **
    *)
    mutable co_level:  int; 

    mutable co_init:    value;   (* initial value if any *)

    (*
    ** Default value if any
    *)
    mutable co_default : value;
    (*
    ** do the object need a mutex guard ?   
    **
    **  Constant: no
    **  Register: yes if # of writers > 1
    **  Variable: yes if # of processes > 1
    *)
    mutable co_guard:   guard option;

    (*
    ** Which processes are interested to read from this object?
    ** In general at least the parent process.
    *)
    mutable co_reader: process list;
    (*
    ** Which processes are interested to write to this object?
    ** In general at least the parent process.
    *)
    mutable co_writer: process list;

    (*
    ** Array element object? [array]
    ** Structure Array element object? [array of struct_elem;array of struct]  
    *)
    mutable co_array:  array_type list;   
    
    (*
    ** Structure element object? [struct]
    *)
    mutable co_struct: struct_type list;
     
    (*
    ** Data blocks, e.g. variables.
    ** Size: 1 for scalar, N for aggregates of size N.
    ** Subsize: 1 for full width, N for fragmented objects
    ** in cell units.
    **
    ** For exmaple: logic[32%4] -> size=1, subsize=4
    **              array [32] logic[8] -> size=32, subsize=1
    *)
    
    mutable co_index:  int;     (* data_block address       *)
    mutable co_size : int;      (* size in cell units       *)
    mutable co_subsize : int;   (* subsize in cell units    *) 
    mutable co_block:  data_block 
                       option;   

    (*
    ** Object created ? ...
    *)
    mutable co_flags: obj_flags list;

    (*
    ** Bitnamed structured logic vector?
    *)
    mutable co_bittype : type_bit option;

    (*
    ** Clock domain
    *)
    mutable co_domain: string;
}
(*
** Abstract objects. Handled by external modules, for example Mutex
*)
and abstract_object = {
    ao_name : string;
    mutable ao_type : type_abstract;
    mutable ao_module : cp_module;
    (*
    ** operation * process
    *)
    mutable ao_procs : (string * process) list;

    (*
    ** Array element object? [array]
    ** Structure array element object? [array_sel;array]
    *)
    mutable ao_array : array_type list;
    (*
    ** Structure element object?
    *)
    mutable ao_struct: struct_type list;
    
    (*
    ** Core object?
    *)
    mutable ao_obj: object_type option;
    (*
    ** Flag list (name,value)
    *)
    mutable ao_flags : (string*string) list;

    (*
    ** Auxilliary object list (name,value)
    *)
    mutable ao_objs : (string*uc_data) list;

    mutable ao_domain: string;
    
    (*
    ** Object environment parameters
    *)
    mutable ao_env: (string*string) list;
}
(*
** Data channel
*)
and channel_model =
  | Chan_buffered
  | Chan_unbuffered
  | Chan_uni
  | Chan_bi
  | Chan_multi
and channel_type = {
    ch_obj: core_object;
    ch_ot: object_type option;
    mutable ch_model: channel_model list;
    ch_ao : abstract_object;
}
(*
** Data queue
*)
and queue_type = {
    qu_obj: core_object;
    qu_ot: object_type option;
    qu_depth: int;
    qu_ao: abstract_object;
}
(*
** <OT>: CONPRO data objects
*)
and object_id = 
    | O_const
    | O_named_value
    | O_signal  
    | O_reg
    | O_var
    | O_channel
    | O_queue
    | O_component
    | O_struct
    | O_array 
    | O_object
    | O_value
    | O_reference

and object_type =
    (*
    ** constant signal value 
    *)
    | OT_const of core_object   
    (*
    ** Named value (type=value)
    *)
    | OT_named_value of (string*value) 
    (*
    ** connection wire only!
    *)
    | OT_signal of core_object  
    (*
    ** A register with independent read and write access
    *)
    | OT_reg of core_object    
    (*
    ** Classic imperative variables grouped in data block (RAM/ROM)
    ** with address selection. Variables appearing on the RHS of an
    ** expression must be loaded in temporarily registers!
    *) 
    | OT_var of core_object
    (*
    ** External VHDL component
    *)
    | OT_component of struct_type
    (*
    ** Multitype aggregate
    *)
    | OT_struct of struct_type
    (*
    ** Unitype aggregate
    *)
    | OT_array of array_type
    (*
    ** Arrary element access 
    *)
    | OT_array_sel of (array_type * int)

    (*
    ** Abstract objects handled by builtin modules (for example Mutex.t)
    *)
    | OT_object of abstract_object  
    (*
    ** Inlined constant value
    *)
    | OT_value of value             
    (*
    ** Handshaked communication channel
    *)
    | OT_channel of channel_type
    (*
    ** Queue
    *)
    | OT_queue of queue_type


    (*
    ** Some special needed for scheduling and relocation
    *)
    | OT_reference of (string*(int list))
    


(*
** Object Context Description
*)
and object_params =
  | OD_sub of (int*int)             (* subrange         *)
  | OD_index of instr               (* bit index selector *)
  | OD_conv of data_type            (* type conversion  *)
  | OD_sel of int array             (* array selector   *)
  | OD_sel_obj of instr             (* array selector   *)
  | OD_lneg
  | OD_aneg
  | OD_bneg
and source = {
  mutable s_file: string;
  mutable s_line: int;
  mutable s_cpos: int;
}
(*
** Schedule mode of process instruction block
*)
and scheduler =
  (*
  ** Reference stack scheduler:
  ** ALAP
  *)
  | Sched_refstack
  (*
  ** Expression scheduler:
  ** Timing constrains
  *)	  
  | Sched_expr
  (*
  ** Basicblock scheduler:
  ** Parallelism scheduler
  *)
  | Sched_basicblock
and schedule_mode =
  (*
  ** All scheduler above
  *) 
  | Sched_auto
  | Sched_custom of (scheduler list)
  (*
  ** No automatic scheduling at all
  *)
  | Sched_def
(*
** Instruction block customization
*)
and expr_type =
  | EXPR_flat
  | EXPR_alu 
  | EXPR_binary
  | EXPR_top
  
and block_params =
  (*
  ** "unroll"
  ** Complete unrolling of loops    
  *)
  | BP_unroll   
  (*
  ** "bind"
  ** Execute all bounded instructions concurrently
  *)
  | BP_bind
  (*
  ** "expr=X"
  ** Expression type model:
  **  {"alu","binary","flat"}.
  *)
  | BP_expr of expr_type
  (*
  ** "temp=X"
  ** Temporary register model
  ** { "shared","not_shared"}
  *)
  | BP_temp of string

  (*
  ** "alu_thres=N"
  ** ALU data width threshold
  *)
  | BP_alu_min_width of int
  (*
  ** "alu_op=OP"
  ** Preferred ALU operations
  *)
  | BP_alu_ops of operator list
  (*
  ** "alu_type=T"
  ** Preferred ALU data type
  *)
  | BP_alu_type of data_id list
  (*
  ** "inline"
  ** Inline conditional statements in control path and prevent
  ** ALU usage.
  *)
  | BP_inline
  (*
  ** Block frame descriptor
  *)
  | BP_schedule of schedule_mode

  (*
  ** Never move, schedule or remove this block
  *)
  | BP_locked
(*
** Synthesis block constrains
*)
and block_constraints = {
  mutable bc_name: string;
  mutable bc_src: source;
  mutable bc_params: block_params list;
}
(*
** Blockframe times in time units (TU) == clock cycles
*)
and frame_time =
  (*
  ** N TUs
  *)
  | FT of int
  (*
  ** Lower bound: At least N TUs
  *)
  | FT_min of int
  (*
  ** Upper bound: At most N TUs
  *)
  | FT_max of int
  (*
  ** Range of different times resulting from unpredictable
  ** branching (binary if-then-else or multicase)
  *)
  | FT_minmax of (frame_time*frame_time)
  (*
  ** Loop
  *)
  | FT_n of (frame_time*frame_time)
  (*
  ** List of TUs. Elements can't be merged further, for example due to
  ** loop boundary.
  *)
  | FT_list of frame_time list
  (*
  ** Unknown, not actually calculated
  *)
  | FT_0
and frame_type = 
  | BF_data
  | BF_branch
  | BF_compound
  | BF_empty
  | BF_loop
  | BF_conditional
and block_frame = {
  (*
  ** Unique identification number
  *)
  mutable bf_id: int;
  (*
  ** Name specifying kind of block
  *)
  mutable bf_name: string;
  (*
  ** Source location
  *)
  mutable bf_src_start: source;
  mutable bf_src_end: source;
  (*
  ** Parent node
  *) 
  mutable bf_parent: block_frame option;
  (*
  ** Child nodes
  *)
  mutable bf_childs: block_frame list;
  (*
  ** Estimated block runtime
  *)
  mutable bf_time: frame_time;
  (*
  ** Aux. for loop calculations
  *)
  mutable bf_loop: frame_time*frame_time;
  (*
  ** Frame class type
  *)
  mutable bf_type: frame_type;
  (*
  ** Block synthesis rules
  *)
  mutable bf_params: block_params list;
}
(*
** Process instruction tree with data objects needed for synthesis.
*)
and instr =
    (*
    ** The operands: full and subrange objects
    *)
    | PI_obj of (object_params list * object_type)
    | PI_tuple of instr list

    | PI_list of instr list
    | PI_block of (instr list * block_frame)

    (*
    ** Expression evaluation
    **
    **  Operators (ML style)
    **  arithm: {+,-,*,...}
    **  bool  : {=,<>,<,>,...}
    **
    **  Args: [<kind>],<operation>,<op1>,<op2>
    *)
    | PI_arithm    of (operator * instr * instr)
    | PI_bool      of (bool_kind * operator * instr * instr)

    | PI_concat    of (instr * instr)

    (*
    ** Data transfer and expression evaluation (RHS):
    ** LHS * RHS => LHS = RHS
    ** Args: <src>,<lhs>,<rhs>
    *)
    | PI_assign of (source * instr * instr)



    (*
    ** Core functions
    *)
    (*
    ** wait for <expr> with <expr> else <expr>;
    ** delay <cyc>
    ** apply <cyc> with <expr> else <expr>;
    *)
    (*
    ** Args: <src>,<expr>,<cyc>,<e_false>,<e_true>
    *)
    | PI_waitfor of (source * instr * int * timeunit * instr * instr)

    (*
    ** Args: <src>,<expr>,<e_true>,<e_false>
    *)
    | PI_branch of (source * instr * instr * instr)
    (*
    ** for i = a to b do ...
    ** Args: <src>,<expr>,<dir>,<lim1>,<lim2>,<block>
    *)
    | PI_forloop of (source * instr * char * instr * instr * instr)
    (*
    ** while E do ....
    ** Args: <src>,<kind>,<expr>,<block>
    *)
    | PI_loop of (source * loop_kind * instr * instr)
    
    (*
    ** match E with E1: ... 
    ** Args: <src>,<expr>,<case list>
    *)
    | PI_select of (source * instr * instr)
    (*
    **  try ... with E1:...
    ** Args: <block>,<case list>
    *)
    | PI_try of (instr * instr)
    (*
    ** Args: <src>,<expr list>,<block>
    *)
    | PI_case of (source * instr list * instr)

    (*
    ** A <=> B
    *)
    | PI_map of (source * instr * instr)

    (*
    ** Function call. A function is either generic (user supplied) or an object
    ** operation, for example a mu_lock from the Mutex module, and therefore
    ** abstract. 
    ** Generic functions are handled by the Core module.
    **
    ** Args: <src>,<object>,<function>,<arg list>
    *)
    | PI_fun of (source * (object_params list * object_type) * string * (instr list))

    (*
    ** Raise an exception
    *)
    | PI_raise of int
    
    | PI_monitor of (source * sym_type * bool)
    | PI_nop
(*
** Process ALU specification
*)
and pro_alu = {
    mutable alu_name : string;
    mutable alu_ops : operator list;
    mutable alu_type : data_type; 
}
(*
** Data and control path (string = VHDL). Some instruction infer
** auxilliary processes!
**
** The data list of a state consists of Data_in, Data_out and/or 
** Data_trans constructors. All other constructors used for auxilliary
** purposes. Because for example Data_default can occurs both in a Data_out
** and Data_trans statement, the order of the data list is important:
**
**      << Data_in >>
**      << Data_out >>
**      << Aux. for IN/OUT >>
**      ---------------------
**      << Data_trans >>
**      << Aux. for TRANS >> 
*)

and synth_data =
    (*
    ** RHS of an expression, mainly feeding the ALU
    *)
    | Data_in   of string 
    (*
    ** Sensivity list for control, data and transitional path.
    *)
    | Data_sens of string  
    | Data_trans_sens of string
    | Data_cond_sens of string

    (*
    ** LHS of an expression, mainly from ALU result, not transitionial
    ** driven.
    *)
    | Data_out  of string 

    (*
    ** Signal assignments (state dependent), static driven.
    *)
    | Data_signal  of string 

    (*
    ** Clock driven transitional data transfer
    *)
    | Data_trans of string

    (*
    ** (External) toplevel expression and architecture definition
    *)
    | Data_top  of string 
    | Data_top_def of string
    (*
    ** Conditional expression (IF/CASE)
    *)
    | Data_cond  of string 

    (*
    ** Auxilliary process
    *)
    | Data_process of string list
    
    (*
    ** Default data assignment in data and transitional path
    *)
    | Data_def of (string * string)
    | Data_trans_def of (string*string)
(*
** Control Path: States
*)

(*
** Transition to next state (can be conditional)
*)
and case_state = {
    mutable cs_data: synth_data list;
    mutable cs_next: next_state;
}
and next_state =
    (*
    ** Jump simply to next state...
    *)
    | Next of string
    (*
    ** actually unresolved
    *)
    | Next_instr
    (*
    ** Jump S
    ** IF <expr> THEN S ELSE Next
    *)
    | Branch of (synth_data list * next_state * next_state)

    (*
    ** Select
    ** CASE <expr> IS CASE_LIST
    *)
    | Select of (synth_data list * case_state list)

(*
** Each instruction requires at least one process state, in general
** the instruction splits into several sub states.
*)
and state = {
    (*
    ** State name, specified in modue cp_X.ml, for example the
    ** X=Core module: S_load, S_eval, S_next, S_store...
    **
    ** Generic format:
    **  S_i<instrid>_<instrname>_<subop>
    *)    
    s_name: string;
    (*
    ** Control Path
    *)
    mutable s_next: next_state;
    
    (*
    ** Data Path
    *)
    mutable s_data: synth_data list;

    (*
    ** Block environment
    *)
    mutable s_block: block_frame;
}
(*
** State sequences are grouped and can be nested, for example in
** branches.
*)
and state_block =
    | State of state 
    | State_block of state_block list
    | State_top of state    (* toplevel instruction only *)
(*
** One process - one instruction unit, executed sequentially, but concurrently
** to other processes. Each process is emitted to one separated VHDL entity.
** 
**  in module <m> process <p> ... -> <m>_<p>.vhdl with entity <m>_<p>
**
*)
and process = {
    pro_name:   string;
    (*
    ** Parent module
    *)
    pro_module: cp_module;
 
    (*
    ** Original syntax graph
    *)
    pro_syntax: Cp_syntax.syntax;

    (*
    ** All process private objects created inside the process.
    ** Objects not exported are created in this process entity.
    ** This is the process symbol table!
    *)
    pro_objs:   (string,sym_type) Hashtbl.t;

    (*
    ** Imported objects 
    *)
    pro_import: (string,sym_type) Hashtbl.t;


    (*
    ** All exported objects (mainly signals). Theses symbols are
    ** moved from pro_objs table to mod_objs module table and 
    ** duplicated in pro_export.
    *)
    pro_export: (string,sym_type) Hashtbl.t;


    (*
    ** Temprorary registers. 
    *)
    mutable pro_temps: core_object list;

    (*
    ** Programm level:
    ** All process instructions (excluding object creation).
    *)
    mutable pro_instr:  instr list;
    
    (*
    ** MicroCode level
    *)
    mutable pro_ucode: uc_instr list;
    (*
    ** Control- and datapath level:
    ** Synthesized process instructions: process states.
    *)
    mutable pro_states: state_block list;

    (*
    ** Different ALUs can exists with different data types
    ** and widths.
    *)
    mutable pro_alu : pro_alu list;

    (*
    ** Head of list specifies actual block constraints.
    ** At least the process main block is required.
    *)
    mutable pro_constr: block_constraints list;


    (*
    ** Head of list specifies actual block frame.
    *)
    mutable pro_frame: block_frame list;
    (*
    ** Processes are ADTOs, too.
    *)

    mutable pro_ao: abstract_object;

    (*
    ** Clock domain
    *)
    mutable pro_domain: string;
    
    (*
    ** Control  Graph
    *)
    mutable pro_control: pro_control;

} 
(*
** Control Graph:
** Interprocess Control (start,end,call) and
** exception links (raise, try)
*)
and pro_control = {
    mutable pro_start: process list;
    mutable pro_stop: process list;
    mutable pro_call: process list;
    (*
    ** ALL exceptions raised within process 
    ** (including within nested catch environments)
    *)
    mutable pro_raise: type_exception list;
    (*
    ** All (not nested toplevel) catch environments within process
    *)
    mutable pro_catch: pro_catch list;
} 
and pro_catch = {
    (*
    ** Block frame of actual catch environment PI_try
    *)
    mutable catch_env: block_frame;
    (*
    ** All exceptions caught in this catch environment
    *)
    mutable catch_exl: type_exception list;
    (*
    ** Control activities inside instruction block of catch environment
    *)
    mutable catch_ctl: pro_control;
}

(*
** Module description. A XX.cp file is treated like a module XX.
** A cp_module is object based, that means it contains at least data
** objects and processes.
** A compiled module can be saved in file XX.cpo.
*)
and module_flags =
    | Mod_main
and cp_module = {
    mutable mod_name:   string;
    (*
    ** Module private objects visible to all processes. 
    ** All module level objects are created in the module 
    ** VHDL entity and linked to all sub entities (modules and processes)!
    *)
    mutable mod_objs:   (string,sym_type) Hashtbl.t;


    (*
    ** Only exported processes and objects (visible for higher level modules).
    ** Sym_obj objects are duplicated from mod_objs. 
    ** Sym_pro processes are duplicated from mod_objs.
    *)
    mutable mod_export: (string,sym_type) Hashtbl.t;     

    (*
    ** All processes of this module.
    ** For each process an own entity and VHDL file is created. 
    ** Name:
    **  <module>_<pname>
    *)
    mutable mod_procs:  process list;

    (*
    ** List of all imported modules: pre compiled.
    *)
    mutable mod_external: cp_module list;
    (*
    ** Copy of all mod_external.mod_export symbols
    *)
    mutable mod_import: (string,sym_type) Hashtbl.t;

    (*
    ** All included rule based modules, like Core, Mutex,...
    *)
    mutable mod_rules: rl_module list;

    (*
    ** Toplevel instructions (e.g. monitor...)
    *)
    mutable mod_instr : instr list;
    (*
    ** Line to file position mapping. A module can include 
    ** files. Therefore teh filename must be recorded, too.
    *)
    mutable mod_fmap : (string * int*int*int) list;
    mutable mod_syntax: Cp_syntax.syntax list;

    (*
    ** Module flags
    *)
    mutable mod_flags: module_flags list;
}
(*
** Method parameters and arguments 
*)
and arg_type =
  | Arg_lhs 
  | Arg_rhs 
  | Arg_lrhs 
and arg_desc = {
  mutable arg_label: string;
  mutable arg_type: arg_type;
  (* 
  ** can be parameterized with each object 
  ** on object instantiation
  *)
  mutable arg_data_type: data_type;  
}
(*
** Rule based module/ Embedded and External Module Interface
*)
and rl_module = {
    rl_name: string;

    (*
    ** Return true if object is handled by this rule module.
    *)
    rl_my : sym_type -> bool;

    (*
    ** Return port declaration (in VHDL entity header) 
    **  [Module or Process level]
    **
    ** Returns: port list * aux (toplevel) list
    *)
    rl_obj_port : sym_type -> cp_module -> process option -> 
                  (string list) * (string list);

    (*
    ** Return port map declaration (in VHDL port map) 
    **  [Process level]
    *)
    rl_obj_map : sym_type -> cp_module -> process option -> string list;

    (*
    ** Return object declaration (in VHDL architecture header)
    ** -> creation of data objects and signals 
    **  [Module or Process level]
    **  
    **  decl list * toplevel list
    *)
    rl_obj_decl : sym_type -> cp_module -> process option -> string list *
                                                             string list;

    (*
    ** Return object implementation code (in architecture body)
    ** -> creation of data and control path of data objects 
    **  [Module or Process level]
    *)
    rl_obj_code : sym_type -> cp_module -> process option -> string list;

    (*
    ** Create MicroCode from Process instruction
    ** instr -> instr_id -> pro -> ucl
    *)
    rl_instr_ucode : instr -> int ref -> process -> uc_instr list;
    (*
    ** Abstract types supplied by this module.
    *)
    mutable rl_types : type_abstract list;
    (*
    ** Supported AO methods (name*args)
    *)
    mutable rl_methods : (string*(arg_desc list)) list;
    (*
    ** Precompile and check function (method) call. Bool: toplevel
    *)
    rl_fun_compile : cp_module -> process option -> instr -> bool -> unit;
    (*
    ** Functions. Returns directly state list. First string specifies 
    ** actual, second next label if any.
    *)
    rl_fun_scode : uc_instr -> string -> string -> 
                   cp_module -> process -> 
                   state list;
    (*
    ** Toplevel or special process instructions like monitors.
    ** Returns (top_expr list * top_def list)
    *)
    rl_top_vcode : instr -> cp_module -> process option -> (string list) * (string list);

    (*
    ** Function execution time estimation
    *)
    rl_time: cp_module -> process option -> instr -> frame_time;

    (*
    ** Create new object of this module type. Returns copy of module structure and in
    ** case of EMI module a copy of the module class. Object can be parameterized using
    ** environment list (name,value), for example $datawidth=12!
    *)
    rl_new_obj: cp_module -> string -> (string*env_expr) list -> rl_module;
    
    (*
    ** Talk to EMI object
    *)
    rl_interp: string -> string;  
    
    (*
    ** Linked list of modules.
    ** For example: Core objects are handled by Cp_core.rl_module (parent),
    ** but the RTL-Implementation is handled by EMI ram.mod (child)!
    *)
    mutable rl_child: rl_module option;
}
(*
** Type definitions
*)
and tp_params = 
  | TP_bin
  | TP_gray
  | TP_name 
  | TP_onehot
(*
** Constant type = enumeration
*)
and type_const = {
    mutable tc_name : string;
    mutable tc_type : data_type;
    mutable tc_elems: object_type list;
    mutable tc_params : tp_params list;
}
(*
** Structure type
*)
and type_elem = {
    mutable te_name : string;
    mutable te_type : data_type * int;
    (*
    ** Used with components.
    ** There are two different component views of
    ** port directions:
    ** 1. External VHDL entities =>
    **          VHDL: OUT => CONPRO: IN
    ** 1. CONPRO TOP VHDL entity =>
    **          VHDL: OUT => CONPRO: OUT!!!
    *)
    mutable te_port : port_type option;
}
and type_struct = {
    mutable ts_name : string;
    mutable ts_elems : type_elem list;
}
and type_method = {
    mutable tm_method: string;
    mutable tm_method_args: arg_desc list;
}
and type_module = {
    (*
    ** Module.object_type name tupel
    *)
    mutable tm_module : string;
    mutable tm_type: string;
    mutable tm_methods : type_method list;
}
and type_bitrange = {
    mutable tr_name : string;
    mutable tr_range : int * int;
}
and type_bit = {
    mutable tb_name : string;
    mutable tb_elems : type_bitrange list;
    mutable tb_dt : data_type;
}
and type_abstract = {
    mutable ta_name : string;
    mutable ta_rules : rl_module;
    mutable ta_flags : (string*string) list;
}
and type_exception = {
  mutable tx_name: string;
  mutable tx_id: int;
}
and typedef =
    | Type_const of type_const
    | Type_struct of type_struct
    | Type_module of type_module
    | Type_bit of type_bit
    | Type_abstract of type_abstract
    | Type_exc of type_exception
(*
** User defined procedures/functions (fun_ret <> None)
** Either code inlined (== macro substitution) or shared function
** block modelled with a process call.
*)
and fundef = {
  fun_name : string;
  mutable fun_inline: bool;
  (*
  ** Both inline and shared function block
  *)
  mutable fun_args: string list;
  mutable fun_ret: string list;   
  mutable fun_instr : syntax;
  (*
  ** Local function objects
  *)
  mutable fun_objs : (string,sym_type) Hashtbl.t;

  (*
  ** Shared function block only
  *)
  mutable fun_args_obj: core_object list;
  mutable fun_ret_obj: core_object list;
}
(*
** Symbol table types
*)
and sym_type =
    | Sym_mod of cp_module
    | Sym_rule of rl_module
    | Sym_pro of process
    | Sym_obj of object_type
    | Sym_block of data_block
    | Sym_mon of (bool*sym_type)
    | Sym_type of typedef
    | Sym_fun of fundef
(*
** MicroCode types
*)
(*
** Already resolved data object (range,type conversion,address).
*)
and uo_flags = 
    | UO_lhs
    | UO_rhs
    | UO_loc
    | UO_ot of object_type (* some special caces requires original OT *)
(*
** Object type environment within expressions.
**
**              DT  ET  PT  LT  ST  TC
** ---------------------------------------------
** DEF: RL16    L16 
** DEF: RI32    I32
** DEF: VL8     L8      LN      (N: RAM data width)
** DEF: VL16    L16     LN
** DEF: VI8     I8      LN
** DEF: RB      B       L1
** DEF: VB      B       LN
** DEF: VL32/4  L32     LN  L8
** DEF: VI32/4  I32     LN  I8
** TEMP(RL8)    L8      LM      (M: TEMP register data width)
** TEMP(RI16)   I16     IM
*)

and uo_type = {
    (*
    ** This type reflects original defined data type independent of actual
    ** expression occurence.
    *)
    mutable uo_data_type : data_type;
    (*
    ** This type reflects physical data type independent of actual
    ** expression occurence. For example variables in RAM blocks have always
    ** DT_logic physical type!
    *)
    mutable uo_phys_type : data_type option;


    (*
    ** This type reflects data type of object actually used in expression.
    *)
    mutable uo_expr_type : data_type;
    (*
    ** Subexpression type (bit range)
    *)
    mutable uo_sub_type : data_type option;
    (*
    ** Logical type. Used with sub range (bit range) and fragmented types.
    *)
    mutable uo_log_type : data_type option;

    
    (*
    ** Post Type conversion not covered by other type specifiers here, 
    ** required for example in assignments with LHS PT <> ET!
    **
    ** Distinguish different conversion context:
    **
    ** 1. uo_flags=[UO_rhs] -> apply type conversion to this object (in general TC(RHS)=DT(LHS))
    ** 2. uo_flags=[UO_lhs] -> only in assignments, apply type conversion of compound expression RHS!
    **                         (in general TC(LHS) = DT(LHS))
    *)
    mutable uo_conv : data_type option;
    (*
    ** DT_lneg/DT_aneg?
    *)
    mutable uo_sign : data_type option;
    
}
and uc_object = {
    mutable uo_name: string;
    mutable uo_obj : core_object option;
    mutable uo_type: uo_type;

    (*
    ** Static subrange/bit index specifier
    *)
    mutable uo_range : (int * int) option;
    (*
    ** Dynamic bit selector
    *)
    mutable uo_index: uc_arg option;
    
    (*
    ** Block type only. The address is either a 
    ** static integer index or a data object name (indirect indexing)
    ** in VHDL!!! representation. The integer argument
    ** specifies an offset. Non fragmented objects always use index 0.
    *)
    mutable uo_addr : int -> string;
    mutable uo_sel : uc_data list;
    mutable uo_block : data_block option;
    
    (*
    ** Flags
    *)
    mutable uo_flags : uo_flags list;
}

(*
** A temporary register.
*)
and uc_temp = {
    mutable ut_type : uo_type;
    mutable ut_range : (int * int) option;
    mutable ut_name : string;
    mutable ut_frame: uc_label option;
    mutable ut_obj  : core_object option; 
    mutable ut_flags : uo_flags list;
}
and uc_val = {
    mutable uv_type : uo_type;
    mutable uv_val  : value;
}
and uc_alu = {
    (*
    ** Actual expression type required, not alu_type!
    *)
    mutable ua_type : data_type;
    mutable ua_flags : uo_flags list;
}
(*
** Array selector object
*)
and uc_sel = {
  mutable us_name : string;
  (*
  ** Either selected or first array object. Name of object is product of
  ** us_name and us_sel in the latter case!
  *)
  mutable us_obj: uc_data;
  mutable us_array: array_type;
  (*
  ** Selector object (Value/Register/Signal...)
  *)
  mutable us_sel : uc_data list;
}
(*
** Compound object
*)
and uc_list = {
  mutable ul_type : uo_type;
  mutable ul_list : uc_data list;
  mutable ul_flags : uo_flags list;
}
and uc_array = {
    mutable uat_type : uo_type;
    mutable uat_range : (int * int) option; (* vector subrange only *)
    mutable uat_name : string;
    mutable uat_obj  :  array_type; 
    mutable uat_flags : uo_flags list;
}

(*
** uCode data objects (only synthesizable object types!), as they
** appear in their appropiate program context, that means including
** expression particular type and range conversion.
*)
and uc_data =
    | UC_reg of uc_object
    | UC_var of uc_object
    | UC_sig of uc_object
    | UC_chan of uc_object
    | UC_queue of uc_object
    | UC_val of uc_val
    | UC_list of uc_list
    | UC_sel of uc_sel
    
    (*
    ** Invokation of ALU (alu_reg,alu_bool_reg,alu_res,alu_bool)
    ** required.
    *)
    | UC_alu of uc_alu
    (*
    ** Immediately sources and destinations are
    ** concatenated (just a place holder). The integer value is an
    ** identifier.
    *)
    | UC_immed of int
    (*
    ** A temprorary register is required...
    *)
    | UC_temp of uc_temp
    (*
    ** Special treatment: boolean expression result (like UC_immed)
    *)
    | UC_bool of int

    (*
    ** Full (or subrange) array arguments
    *)
    | UC_array of uc_array

and uc_label = 
    | UC_next
    | UC_label of string

    
(*
** uCode instructions
*)
and uc_code =
    (*
    ** Symbolic label
    *)
    | Label of string
    (*
    ** Treat next n instructions concurrently.
    *)
    | Bind of int
    (*
    ** Transfer data from SRC to DATA
    **
    ** MOVE {DST,SRC}
    *)
    | Move of (uc_data * uc_data)
    (*
    ** Evaluate expression and transfer result to DST
    **
    ** EXPR { [OP],DST,DATA1,DATA2 }
    *)
    | Expr of (operator list * uc_data * uc_data * uc_data)
    (*
    ** FALSEJMP S  -- jump on alu_state = false condition
    *)
    | Falsejump of (uc_data * uc_label)
    (*
    ** JMP S -- jump always 
    *)
    | Jump of uc_label


    (*
    ** Special instruction not handled by MicroCode, for example
    ** process or monitor control.
    *)
    | Special of instr
    (*
    ** Functions
    *)
    | Fun of ((object_params list * object_type) * string * uc_arg list)
 

    (*
    ** Placeholder without action
    *)
    | Nop
and uc_arg =
    | UA_data of uc_data
    | UA_expr of uc_instr list
and uc_instr = {
    mutable ui_code : uc_code;
    mutable ui_frame: block_frame;
}
(*
** Synthesis and External Module Environment (eme)
*)

and env_range =
  | ENV_point of (char*string)
  | ENV_line of ((char*string)*(char*string))
and env_attr = 
  | ENV_scalar
  | ENV_sorted
  | ENV_filtered
and env_func =
  | F_INDEX
  | F_SIZE 
  | F_WIDTH 
  | F_INDEX_WIDTH 
  | F_EXPAND 
  | F_MAX 
  | F_MIN 
  | F_NTH 
  | F_MEMBER 
  | F_PRINT 
  | F_REV 
  | F_TO_LOGIC 
  | F_TO_INT 
  | F_TO_NAT 
  | F_TO_STRING 
  | F_TO_BOOL 
  | F_USER of string 
and env_expr =
  | ENV_logic of string
  | ENV_int of int64
  | ENV_str of string
  | ENV_sig of env_expr list
  | ENV_bool of bool
  | ENV_range of (char * env_expr * env_expr)
  | ENV_set of (env_expr list)
  | ENV_op of (env_expr * string * env_expr)
  | ENV_var of string
  | ENV_sel of (env_expr * env_expr)
  | ENV_fun of (env_func * env_expr list)
  
and env = {
  (*
  ** Name of environment parameter
  *)
  mutable env_name : string;
  (*
  ** Value of environment parameter 
  ** Actual value: head of list
  *)
  mutable env_val  : env_expr list;
  (*
  ** Optional specification of parameter value range
  *)
  mutable env_range: env_range list;
  (*
  ** Type specifier
  *)
  mutable env_type : char;
  (*
  ** Only used for import of ConPro objects
  *)
  mutable env_obj: uc_data list;
  
  (*
  ** Attributes
  *)
  mutable env_attr: env_attr list;
}

(*
** Synthesis failure (caught)
*)
exception Synthesis of string

(*
** Encoding formats
*)
type enc_format =
    | Enc_binary
    | Enc_onehot
    | Enc_gray
    | Enc_auto

(*
** Module generators (CONPRO synthesis of operators like multipliers). The
** string specifies type or model. Auto type means absence of a module
** generator. In this case a CONPRO operator ist directly passed (perhaps in
** transformed syntax) to backend output, for example:
**
**  CONPRO:
**      c <- a * b;
**  VHDL:
**      c <= a * b;
*)

type modgen =
    | Mod_mult of string    (* multiplier   *)
    | Mod_add of string     (* adder        *)
    | Mod_shift of string   (* shifter      *)
    | Mod_mux of string     (* multiplexer  *)
    | Mod_ram of string     (* ram          *)


(*
** Structure returned by vhdl_of_ud
*)  

type vhdl_data = {
  (*
  ** Signal name
  *)
  mutable dp_sig: string;
  (*
  ** Default signal <name>,<val> pairs
  *)
  mutable dp_def: (string*string) list;
  (*
  ** Signal name for sensivity process list 
  *)
  mutable dp_sen: string list;
  (*
  ** Some auxilliary signals (_WE,_GD,....)
  *)
  mutable dp_aux: string list;
  (*
  ** Auxilliary default signal <name>,<val> pairs
  *)
  mutable dp_aux_def: (string*string) list;
  (*
  ** Auxilliary signal sensivity list
  *)
  mutable dp_aux_sen: string list;
  (*
  ** Control path signal (only RHS of expression)
  *)
  mutable cp_sig: string;
  (*
  ** Control path signal sensivity name
  *)
  mutable cp_sen: string list;
  (*
  ** LHS signals can require additional RHS type conversion in assignments. Returns mask of kind "signed(%s)"
  ** for final post type conversion of RHS expression (not for single objects!).
  *)
  mutable dp_conv: string;
  (*
  ** Additional toplevel expressions and definitions required.
  *)
  mutable top_def: string list;
  mutable top_expr: string list;
}  

(*
** Analysis environment
*)
      
type analysis = {
  mutable a_mname : string;
  mutable a_curpos : source;
  mutable a_funpos : source;
  mutable a_errlabel: string;
  mutable a_toplevel : bool;
  mutable a_exceptions: int;
  mutable a_pro: process option;
  mutable a_pro_syms : (string, sym_type) Hashtbl.t option;
  mutable a_pro_import : (string, sym_type) Hashtbl.t option;
  mutable a_pro_export : (string, sym_type) Hashtbl.t option;
  mutable a_pro_temps : core_object list ref;
  mutable a_pro_name : string;
  mutable a_pro_subst: (string*string) list;
  mutable a_pro_num : int;
  (*
  ** <fun param name>,<arg name>,<arg syntax>
  *)
  mutable a_fun_subst : (string * string * Cp_syntax.syntax) list;
  mutable a_fun_syms : (string, sym_type) Hashtbl.t option;
  mutable a_fun_name : string;
  mutable a_procs_to_compile: (int * string * Cp_syntax.syntax * abstract_object option) list;
  mutable a_block_level: int;
  mutable a_modu: cp_module;
  (*
  ** Additional auxilliary instructions, for exmaple function lock
  ** initialisation
  *)
  mutable a_main_aux: Cp_syntax.syntax list;
  (*
  ** Actual for-loop register index
  *)
  mutable a_loop_index: int;
}

(*
** Compiler arguments
*)
type compiler = {
    (*
    ** Source and library include/search paths
    *)
    mutable t_incl : string list;
    mutable t_lib : string list;
    
    (*
    ** Output directory
    *)
    mutable t_output : string;
    
    (*
    ** Project definitions creating
    ** or overriding constant values
    *)
    mutable t_defs: (string*string) list;
    (*
    ** Short project name
    *)
    mutable t_proj: string;
    
    (*
    ** Synthesis tools.
    ** Additional operations
    *)
    mutable t_synth_tool: synthesis_tool;
 

    (*
    ** Parse and print syntax   
    *)
    mutable t_syntax: bool;
    (*
    ** First compiling stage: Module analysis
    *)
    mutable t_module: bool;
    
    (*
    ** Dump internal states
    *)
    mutable t_dump: string list;
    
    (*
    ** Synthesize stage
    *)
    mutable t_emit : bool;
    (*
    ** Do synthesis 
    *)
    mutable t_synth: bool;
    (*
    ** Only synthesize up to uCode level 
    *)
    mutable t_ucode: bool;
    (*
    ** Only synthesize ML model, affects
    ** different synthesis, too.
    *)
    mutable t_ml: bool;
    (*
    ** Only synthesize C model, affects
    ** different synthesis, too.
    *)
    mutable t_C: bool;
    mutable t_bf_time_calc: bool;

    (*
    ** Top level
    *)
    mutable t_top: string;
    mutable t_files : string list;

    mutable t_debug: bool;
    mutable t_opt: string list;

    mutable t_check: bool;
    mutable t_trace: bool;

    mutable t_silent: bool;
    mutable t_notty: bool;
    mutable t_report: string list;
    mutable t_parse: bool;
    
    mutable t_graph: string list;


    (*
    ** Module generator settings
    *)
    mutable t_modgens : modgen list;
    

    (*
    ** Default block constraints    
    *)
    mutable t_block_constr: block_constraints;

    (*
    ** Global modules to be included and opened
    *)
    mutable t_modules: string list;
}

(*
** Supported synthesis tools
*)

and synthesis_tool = {
  mutable syn_tool: string;
  mutable syn_top: string;
  mutable syn_ver: int;
  (*
  ** VHDL operator to specific synthesis tool mapping
  *)
  mutable syn_vhdl_map: (string * string) list;
  (*
  ** Additional VHDL library definitions
  ** inserted in architecture header of
  ** each created VHDL entity
  *) 
  mutable syn_vhdl_lib: string list;
  (*
  ** Synthesis tool run time options
  **  [<dev>,[<opt>,<val>;...];...]
  **  dev: regular expression matching main target device name
  **  opt: program option
  **    1. program option <opt>=[a-ZA-Z0-9_]  -> -OPT <val>
  **    2. script variable <opt>=$[a-ZA-Z0-9_] -> -opt $OPT
  **    3. program specific option: 
  **       <opt>=<prog>:<opt>
  **             <prog>:<opt>:<desc>
  *) 
  mutable syn_synth_set: (string * (string*string) list) list;
  mutable syn_tech_set: (string * (string*string) list) list;
}


(*
** Device info structure
**
**  Example:    dev_name=xc2s100e-tq144-6
**              dev_vendor=xilinx
**              dev_familiy=spartan2
**              dev_class=xc2s
**              dev_subclass=100    
**              dev_package=tq144
**              dev_speed=6
**              dev_library=xis2
**              dev_ext="e"
*)

type device = {
  dev_name: string;     (* vendor name *)
  mutable dev_vendor: string;   (* Device manufacturer *)
  mutable dev_family: string;   (* Device family *)
  mutable dev_class: string;
  mutable dev_subclass: string;
  mutable dev_ext: string;
  mutable dev_package: string;
  mutable dev_speed: string;
  mutable dev_library: string;  (* leonardo library name *)
}
(*
** Resource informations
*)
type resource_timing = {
  rt_ops : operator list;
  rt_td : data_id;
  rt_fun: int -> float;
}


(*
** VHDL conversion rules
*)

type vd_conv = {
  (*
  ** Converted signal 
  *)
  vdc_sig : string;
  (*
  ** Conversion mask; only for LHS objects, applied
  ** to RHS later...
  *)
  vdc_mask : string;
  (*
  ** type conversion description name
  *)
  vdc_desc: (string*data_type) option;
}


