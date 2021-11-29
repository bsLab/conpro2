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
**    $INITIAL:     (C) 2006-2011 BSSLAB
**    $CREATED:     12.10.2006
**    $VERSION:     2.07
**
**    $INFO:
**
** System confguration module. This module doesn't infer real
** code or objects. It's just a way to modify the compiler structure
** from inside a CONPRO design, for example the clock frequency
** parameter.
**
**    -- mainly replaced by EMI.Sys! --
**
**    $ENDOFINFO
**
*)

open Cp_common
open Cp_types
open Cp_symbol
open Cp_utils
open Cp_analysis
open Cp_ucode
open Cp_expr
open Cp_syntax
open Cp_resource
open Cp_fun
open Printf
open Cp_emi
open Cp_emi_interp
open Cp_emi_types



let (self_emi:(emi option) ref) = ref None
let (self_obj:(emi option) ref) = ref None
let (self:(rl_module option) ref) = ref None


let get_env_val name =
  let emi =
    match !self_obj with
    | Some emi -> emi
    | None ->
    begin
      match !self_emi with
      | Some emi -> emi
      | None -> error 0 (sprintf "Sys: there is no system module loaded!");
    end in

  let rec lookup_env envl name =
    match envl with
    | env :: tl ->
    begin
      if env.env_name = name then Some env
      else
        lookup_env tl name;  
    end;
    | _ -> None in
  match lookup_env emi#get_env name with
  | Some eme ->
  begin
    eme.env_val
  end;
  | None -> error 0 (sprintf "Sys: there is no environment parameter <%s>" name)


let set_env_val name v =    
  let emi =
    match !self_obj with
    | Some emi -> emi
    | None ->
    begin
      match !self_emi with
      | Some emi -> emi
      | None -> error 0 (sprintf "Sys: there is no system module loaded!");
    end in
  let t =
    match v with
    | ENV_str _ -> 's'
    | ENV_int _ -> 'i'
    | ENV_bool _ -> 'b'
    | ENV_var  _ -> '$'
    | ENV_sig _ -> 'S'
    | _ -> '?' in
  emi#set_env [{
      env_name=name;
      env_val=[v];
      env_range=[];
      env_type=t;
      env_obj=[];
      env_attr=[];  
    }]
    
(*
** Return true if object is handled by this rule module.
*)
let rec my sym =
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_object ao -> ao.ao_type.ta_name = "system" 
    | _ -> false;
  end;
  | _ -> false

    
(*
** Object port declaration in entity header.
*)
let rec obj_port sym modu pro =
  [],[]


(*
** Object port mapping.
*)
let rec obj_map sym modu pro =
  []


(*
** Object declaration in architecture header.
** pro = None -> mod_objs (used by processes of this module...)
** pro <> None -> pro_objs (process local objects)
*)
let rec obj_decl sym modu pro =
  [],[]
    

(*
** Object implementation in architecture body.
*)
let rec obj_code sym modu pro =
  []
  
(*
** Synthesize one instruction or instructions from a block list
** and create linear MicroCode list. 
*)

let rec instr_ucode instr id pro =
    []

(*
** Emit monitor object
*)
let emit_mon instr modu pro sym =
    []



(*
** Synthesize a function into state list.
*)
let fun_scode instr label next modu pro =
    []


(*
** Toplevel instructions of module modu or process pro.
*)
let emit_top_code instr modu pro =
    [],[]



let fun_compile modu pro instr top =
  match instr with
  | PI_fun (src,(opl,ot),sel,args) ->
  begin
    let ao = 
          match ot with
          | OT_object ao -> ao;
          | _ -> error 157454 ""; in 
    let emi =
      match !self_obj with
      | Some emi -> emi
      | None ->
      begin
        error 0 (sprintf "<Sys.%s>#fun_compile: there is no system object?" ao.ao_name);
      end in
    let name = ao.ao_name in
    let source = ref "" in
    let addl = ref [] in
    out (sprintf "Compiling system setting method <%s>." sel);
    match sel with
    | "modgen" -> 
    begin
          let s = fun_get_str "System" name args 1 in
          let strl = Str.split (Str.regexp "=") s in
          let n,v = 
              match strl with
              | (n::(v::[])) -> n,v;
              | _ -> error 0 (sprintf "System %s.%s: Invalid modgen parameter format [name=val]"
                                      name sel );
              in
          let modgen =
              match n with
              | "multiplier" -> Mod_mult v;
              | "adder" -> Mod_add v;
              | "shifter" -> Mod_shift v;
              | "multiplexer" -> Mod_mux v;
              | "ram" -> Mod_ram v 
              | _ -> error 0 (sprintf
                                "System %s.%s: Invalid modgen parameter name <%s> [multiplier,adder,shifter,multiplexer,ram]"
                                name sel n) in

          (*
          ** First remove old remains...
          *)
          compiler.t_modgens <- List.filter (fun modgen' ->
                                      (name_of_modgen modgen') <> n
                                     ) compiler.t_modgens;
          if v <> "auto" then  
              compiler.t_modgens <- compiler.t_modgens @ [modgen];

          out (sprintf "System %s.%s: setting module generator <%s>." 
                        name sel s);
    end;
    | "expr" ->
    begin
      let s = fun_get_str "System" name args 1 in
      match s with
      | "flat" 
      | "binary"
      | "alu" 
      | "top" -> 
          compiler.t_block_constr.bc_params <- 
              List.filter (fun bp -> 
                  match bp with
                  | BP_expr _ -> false;
                  | _ -> true) compiler.t_block_constr.bc_params;
          compiler.t_block_constr.bc_params <-
              compiler.t_block_constr.bc_params @ [BP_expr (expr_type_of_str s)];
      | _ -> error 0 (sprintf "System %s.%s: invalid expr=<%s> setting."
                              name sel s);
    end;    
    | "schedule" ->
    begin
      let s = fun_get_str "System" name args 1 in
      let old = ref None in
      let sched_type_of_str s =
        match s with
        | "basicblock" -> 
        begin
          match !old with
          | Some Sched_auto | Some Sched_def | None -> Sched_custom [Sched_basicblock];
          | Some (Sched_custom sm) -> Sched_custom (Sched_basicblock :: sm);
        end;
        | "refstack" -> 
        begin
          match !old with
          | Some Sched_auto | Some Sched_def| None -> Sched_custom [Sched_refstack];
          | Some (Sched_custom sm) -> Sched_custom (Sched_refstack :: sm);
        end;
        | "expression" -> 
        begin
          match !old with
          | Some Sched_auto | Some Sched_def| None -> Sched_custom [Sched_expr];
          | Some (Sched_custom sm) -> Sched_custom (Sched_expr :: sm);
        end;
        | "auto" -> Sched_auto
        | _ -> error 0 (sprintf "Unknown scheduler type <%s> found." s) in
      match s with
      | "basicblock" 
      | "refstack"
      | "expression" -> 
          compiler.t_block_constr.bc_params <- 
              List.filter (fun bp -> 
                  match bp with
                  | BP_schedule sm -> old := Some sm; false;
                  | _ -> true) compiler.t_block_constr.bc_params;
          compiler.t_block_constr.bc_params <-
              compiler.t_block_constr.bc_params @ [BP_schedule (sched_type_of_str s)];
      | _ -> error 0 (sprintf "System %s.%s: invalid schedule=<%s> setting."
                              name sel s);
    end;    
    | "temp" ->
    begin
      let s = fun_get_str "System" name args 1 in
      match s with
      | "shared" 
      | "not_shared" ->
          compiler.t_block_constr.bc_params <- 
              List.filter (fun bp -> 
                  match bp with
                  | BP_temp _ -> false;
                  | _ -> true) compiler.t_block_constr.bc_params;
          compiler.t_block_constr.bc_params <-
              compiler.t_block_constr.bc_params @ [BP_temp s];
      | _ -> error 0 (sprintf "System %s.%s: invalid expr=<%s> setting."
                              name sel s);
    end;       
    | "alu_thres" ->
    begin
      let n = fun_get_int "System" name args 1 in
       compiler.t_block_constr.bc_params <- 
              List.filter (fun bp -> 
                  match bp with
                  | BP_alu_min_width _ -> false;
                  | _ -> true) compiler.t_block_constr.bc_params;
       compiler.t_block_constr.bc_params <-
              compiler.t_block_constr.bc_params @ 
                  [BP_alu_min_width n];
    end;       
    | "alu_op" ->
    begin
      let s = fun_get_str "System" name args 1 in
      let op = 
          try op_type s
          with _ -> 
              error 0 (sprintf "System %s.%s: Invalid alu_op=<%s> setting." 
                               name sel s);
          in
      let bpl' = ref [] in
      List.iter (fun bp ->
          match bp with
          | BP_alu_ops ops ->
              if not (List.mem op ops) then
                  bpl' := !bpl' @ [BP_alu_ops (op::ops)]
              else
                  bpl' := !bpl' @ [bp];
          | _ -> bpl' := !bpl' @ [bp];
        ) compiler.t_block_constr.bc_params;
       compiler.t_block_constr.bc_params <- !bpl';
    end;       
    | "res_time" ->
    begin
      let op_str = fun_get_str "System" name args 1 in
      let dt_str = fun_get_str "System" name args 2 in
      let fun_str = fun_get_str "System" name args 3 in
      let td =
        match dt_str with
        | "logic" -> T_logic;
        | "int" -> T_int;
        | "char" -> T_char;
        | "string" -> T_str;
        | "bool" -> T_bool;
        | _ -> error 0 (sprintf "System %s.%s: unexpected data type <%s>."
                                name sel dt_str) in
      let opl =
        try
        begin
          let strl = Str.split (Str.regexp ",") op_str in
          List.map op_type strl
        end
        with
          | Exit -> error 0 (sprintf "System %s.%s: unexpected operator (list) <%s>."
                                      name sel op_str) in
      op_add_time_table opl td fun_str;
    end;
    | "res_costs" ->
    begin
      ();
    end;
    | "set_env" ->
    begin
      let vname = fun_get_str "System" name args 1 in
      let varg = fun_get_str2 "System" name args 2 in
      out (sprintf "  %s=%s" vname varg);
      let vargs = Str.split (Str.regexp ",") varg in
      List.iter (fun varg -> 
        set_env vname (ENV_str varg);
        ) vargs;
      top_env := emi#get_env;
    end;
    | _ ->
      emi#fun_compile modu pro instr top 

  end;
 | _ -> error 454191 ""  

let bf_time modu proo f =
  FT_min 1


let rec rules = {
    rl_name = "System";
    rl_my = my;
    rl_obj_port = obj_port;
    rl_obj_map = obj_map;
    rl_obj_decl = obj_decl;
    rl_obj_code = obj_code;
    rl_instr_ucode = instr_ucode;
    rl_types = [{
        ta_name = "system";
        ta_rules = rules; 
        ta_flags = [];
    }];
    rl_methods = [
      "modgen",[new_arg_desc Arg_rhs];

      (*
      ** Global Block parameter settings
      *)
      "expr",[new_arg_desc Arg_rhs];
      "alu_thres",[new_arg_desc Arg_rhs];
      "alu_op",[new_arg_desc Arg_rhs];
      "temp",[new_arg_desc Arg_rhs];
      (*
      ** Resource specifications
      *)
      (*
      ** OP,DT,FUN -> weight factor {0.0 .. 1.0}
      *)
      "res_time",[new_arg_desc Arg_rhs;
                  new_arg_desc Arg_rhs;
                  new_arg_desc Arg_rhs];
      (*
      ** OP,DT,FUN -> costs {0.0 .. 1.0}
      *)
      "res_costs",[new_arg_desc Arg_rhs;
                   new_arg_desc Arg_rhs; 
                   new_arg_desc Arg_rhs];
                   
      (*
      ** set_env
      *)
      "set_env",[new_arg_desc Arg_rhs;
                 new_arg_desc Arg_rhs];
    ];
    rl_fun_compile = fun_compile;
    rl_fun_scode = fun_scode;
    rl_top_vcode = emit_top_code;
    rl_time = bf_time;
    rl_new_obj = (fun modu oname env -> 
        match !self_emi with
        | Some emi ->
          let obj = emi#instance in
          out (sprintf "EMI <%s>: creating object <%s>..." emi#info oname);          
          self_obj := Some obj;
          obj#set_oname oname;
          obj#set_module modu;
          let meths = obj#read_methods in
          obj#set_methods meths;
          let acc = obj#read_access in
          obj#set_access acc;
          obj#create_rules;
          rules.rl_methods <- rules.rl_methods @ (obj#get_rules).rl_methods;
          rules
        | None -> rules);
    rl_interp = (fun _ -> "");
    rl_child=None;
}



let init () =
    out "Init: System.";
    let emi = open_module "Sys" in
    self_emi := Some emi;
    self := Some rules;
    lookup_env := Some get_env_val;
    store_env := Some set_env_val;
    top_env := emi#get_env;
    sym_add top_syms (Sym_rule rules)
