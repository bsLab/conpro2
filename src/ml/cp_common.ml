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
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     16.4.2006
**    $VERSION:     2.22
**
**    $INFO:
**
**  Basic utility functions shared by all modules without references
**  to other modules.
**
**    $ENDOFINFO
**
*)

open Cp_syntax
open Cp_types
open Printf
open Cp_print




let nilsrc () = {s_file="";s_line=0;s_cpos=0}

let conpro_path = try Unix.getenv "CONPRO" with Not_found -> "/opt/Conpro2"  


let compiler = 
 {
    t_incl = [sprintf "%s/incl" conpro_path];
    t_lib = [sprintf "%s/lib" conpro_path];
    t_output = "";
    t_defs = [];
    t_synth_tool = 
      {
          syn_tool="generic";
          syn_ver=0;
          syn_top="";
          syn_vhdl_map = [
            "to_integer","to_integer";
            "logic","std_logic";
            "resize","resize";
            "libIEEE","library IEEE;";
            "useIEEE1","use IEEE.std_logic_1164.all;";
            "useIEEE2","use IEEE.std_logic_arith.all;";
            "useIEEE3","use IEEE.std_logic_unsigned.all;";
            "useIEEE4","use IEEE.numeric_std.all;";
            "useIEEE5","";
            "useIEEE6","";
            "useIEEE7","";
            "useIEEE8","";
          ];
          syn_vhdl_lib = [];
          syn_synth_set = [];
          syn_tech_set = [];
      };
    t_syntax = true;
    t_module = true;
    t_dump = [];
    t_emit = true;
    t_synth = true;
    t_proj = "";
    t_ucode = false;
    t_ml = false;
    t_C = false;
    t_bf_time_calc = true;
    t_top = "";
    t_files = [];
    t_debug = false;
    t_opt = ["fold";"dead";"ucomp";"ubind"];
    t_check = false;
    t_trace = false;
    t_silent = true;
    t_notty = false;
    t_report = [];
    t_parse = false;
    t_graph = [];
    t_modgens = [];
    t_block_constr = {
        bc_name="default";
        bc_src =
            {s_file="default settings";s_line=0;s_cpos=0;};
        bc_params = [
            BP_expr EXPR_flat;
            BP_alu_min_width 8;
            BP_temp "shared";
        ];
      };
    t_modules = [];
}


let print_string str =
  __(Unix.write Unix.stdout str 0 (String.length str))

let print_newline () =
  __(Unix.write Unix.stdout "\n" 0 1)

let log_oc = ref Pervasives.stdout

let out_ind = ref 0
let ind_incr () = out_ind := !out_ind + 2
let ind_decr () = out_ind := !out_ind - 2
    

let out str =
  if not compiler.t_silent then
  begin
    let spaces n = String.make n ' ' in

    print_string ((spaces !out_ind)^str);
    print_newline (); flush Pervasives.stdout;
    if !log_oc <> Pervasives.stdout && not compiler.t_check then
        output_string !log_oc ((spaces !out_ind)^str^"\n")
  end
  else
  begin
    let spaces n =
        String.make n ' ' in

    if !log_oc <> Pervasives.stdout && not compiler.t_check then
        output_string !log_oc ((spaces !out_ind)^str^"\n")
  end
    

(*
** Print message line in silent mode and without logging to file.
*)
let out_ str =
  if compiler.t_silent then
  begin
    let spaces n = String.make n ' ' in

    print_string ((spaces !out_ind)^str);
    print_newline (); flush Pervasives.stdout;
    if !log_oc <> Pervasives.stdout && not compiler.t_check then
        output_string !log_oc ((spaces !out_ind)^str^"\n")
  end

let last_progress = ref ""                           

(*
** Progress output message
*)
let progress str n m =
  let spaces n = String.make n ' ' in
  let str' = sprintf "%s%s [%d/%d]" (spaces !out_ind) str n m in
  let len = String.length str' in
  let len' = String.length !last_progress in
  let bsp n = String.make n '\b' in  
  if n = 1 then
    print_string str'
  else
    print_string (sprintf "%s%s" (bsp len') str');
  if n < m then
    last_progress := str'
  else
  begin
    print_newline ();
    last_progress := "";
  end;
  Pervasives.flush Pervasives.stdout
 
let vhdl_oc = ref Pervasives.stdout
let vhdl_ind = ref 0
let vhdl_incr () = vhdl_ind := !vhdl_ind + 2
let vhdl_decr () = vhdl_ind := !vhdl_ind - 2

let uc_oc = ref Pervasives.stdout
let uc_ind = ref 0
let uc_incr () = uc_ind := !uc_ind + 2
let uc_decr () = uc_ind := !uc_ind - 2
    

let vhdl str =
    let spaces n =
        String.make n ' ' in
    if not compiler.t_check then
        output_string !vhdl_oc ((spaces !vhdl_ind)^str^"\n")

let uc_out str =
    let spaces n =
        String.make n ' ' in
    if not compiler.t_check then
        output_string !uc_oc ((spaces !uc_ind)^str^"\n")


  
let to_mod name =
        let name' = String.copy name in
        match name.[0] with
        | 'a' .. 'z' -> 
          name'.[0] <- char_of_int ((int_of_char name'.[0])-32);
          name'
        | _ -> name 

let of_mod name = 
        let name' = String.copy name in
        match name.[0] with
        | 'A'..'Z' ->
          name'.[0] <- char_of_int ((int_of_char name'.[0])+32);
          name' 
        | _ -> name

let (core_rules:rl_module option ref) = ref None
let (process_rules:rl_module option ref) = ref None

let an = 
{
  a_mname="";
  a_curpos=nilsrc();
  a_funpos=nilsrc();
  a_errlabel="";
  a_toplevel=false;
  a_exceptions=0;
  a_pro=None;
  a_pro_syms=None;
  a_pro_import=None;
  a_pro_export=None;
  a_pro_temps = ref [];
  a_pro_name="";
  a_pro_subst=[];
  a_pro_num=0;
  a_fun_subst=[];
  a_fun_syms=None;
  a_fun_name="";
  a_procs_to_compile=[];
  a_block_level=0;
  a_modu = 
  {
        mod_name = "";
        mod_objs = Hashtbl.create 1;
        mod_export = Hashtbl.create 1;
        mod_procs = [];
        mod_external = [];
        mod_import = Hashtbl.create 1;
        mod_rules = [];
        mod_instr = [];
        mod_fmap = [];
        mod_syntax = [];
        mod_flags = [];
  };
  a_main_aux = [];
  a_loop_index = 0;
}



(*
** Returns (line,pos in line) tuple from file position 'pos'.
*)
let line_of_pos pos filemap =
    let res = ref (0,0) in
    protect (List.iter (fun (f,s,e,l) ->
            if pos.f_name = f &&  pos.f_cpos >= s && pos.f_cpos <= e then
            begin
                res := (l,pos.f_cpos-s);
                raise Exit;
            end;
        ) filemap);
    !res
let source_of_pos pos =
    let l,p = line_of_pos pos an.a_modu.mod_fmap in
    {s_file=pos.f_name;s_line=l;s_cpos=pos.f_cpos;}
let pos_of_source s =
    {f_name=s.s_file;f_cpos=s.s_cpos}

let chars c n = 
  if n > 0 then String.make n c else ""
  
let print_err id msg =
  (*
  ** id = -1 -> print no location!
  *)
  let msg =
    if msg = "" then "Internal Error."
    else msg in
    
  let err_id = 
    if id > 0 then sprintf " #%d" id
    else "" in
  let f = an.a_curpos.s_file in
  let f' = an.a_funpos.s_file in
  let l = an.a_curpos.s_line in
  let l' = an.a_funpos.s_line in
 
  let loc =
    if l > 0 && id >= 0 then
      sprintf "# File \"%s\", line %d:" f l 
    else if id >= 0 then
      "# Unkown location:"
    else "" in
  let loc_len = String.length loc in
   
  let loc' =
    if (an.a_funpos).s_line > 0 && id >= 0 then
      sprintf "# File \"%s\", line %d:" f' l'
    else "" in
 
  let loc'_len = String.length loc' in
       
  let err_str = sprintf "Error%s" err_id in
  let err_len = (String.length err_str)+2 in
                           
  let msg,len = 
    let lines = Str.split (Str.regexp "\n") msg in
    let msg' = ref "" in
    let first = ref true in
    let len = ref ((max loc_len loc'_len)+2) in
     let lines' = 
      List.map (fun line ->
        let line' = 
          if not !first then 
            sprintf "# %s%s"
                   (chars ' ' err_len)
                   line 
          else
            sprintf "# %s: %s" err_str line in
        len := max !len (String.length line');
        first := false;
        line'
        ) lines in
    first := true;
    List.iter (fun line ->
      let len' = String.length line in
      let len'' = !len - len' in
      if not !first then
        msg' := sprintf "%s\n%s%s #" 
                        !msg' 
                        line 
                        (chars ' ' (len''- 2)) 
      else
        msg' := sprintf "%s%s #" 
                        line 
                        (chars ' ' (len''-2));
      first := false;
      ) lines';
    !msg' ^ "\n", !len+2 in
  let loc = sprintf "%s%s#\n" loc (chars ' ' (len-loc_len-1)) in
  let loc' = 
    if loc' <> "" then
      sprintf "%s%s#\n" loc' (chars ' ' (len-loc'_len-1))
    else "" in
        
  let str = 
    if l > 0 then
        sprintf "%s\n%s%s%s%s\n"                
                (chars '#' len)
                loc'
                loc
                msg
                (chars '#' len)
    else
        sprintf "%s\n%s%s%s\n"
                (chars '#' len) 
                loc
                msg
                (chars '#' len)
        in
  str

let print_err_tab id msg =
  let rows = ref [] in
  (*
  ** id = -1 -> print no location!
  *)
  let msg =
    if msg = "" then "Internal Error."
    else msg in
    
  let err_id = id in

  let f = an.a_curpos.s_file in
  let f' = an.a_funpos.s_file in
  let l = an.a_curpos.s_line in
  let l' = an.a_funpos.s_line in
 
  if l > 0 && id >= 0 then
    rows := !rows @ [
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="File:";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=sprintf "%s" f;
          };
        ];          
      };
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="Line:";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=sprintf "%d" l;
          };
        ];          
      };
      ]
  else if id >= 0 then
    rows := !rows @ [
      {
        row_cols = [
          {
            col_width=75;
            col_align='l';
            col_cont="-- Unknown location --";
          };
        ];          
      };
      ]; 
   
  if (an.a_funpos).s_line > 0 && id >= 0 then
    rows := !rows @ [
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="+ File:";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=sprintf "%s" f';
          };
        ];          
      };
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="+ Line:";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=sprintf "%d" l';
          };
        ];          
      };
      ];
 

  if err_id <> 0 then
    rows := !rows @ [
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="Error ID:";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=sprintf "%d" err_id;
          };
        ];          
      };
      ];
           
  let lines = Str.split (Str.regexp "\n") msg in 
      
  let i = ref 0 in
  rows := !rows @ (List.map (fun line ->
      incr i;
      if !i = 1 then
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="Description:";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=line;
          };
        ];          
      }
      else
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=line;
          };
        ];          
      };
    ) lines);
                         
  format_table {
      tab_outline = true;
      tab_width = 75;
      tab_title = "ERROR REPORT";
      tab_header = [];
      tab_rows = !rows;
    }

let print_syntax_error f l p =
  let msg = "Syntax error" in
  let rows = ref [] in
 
  if l > 0 then
    rows := !rows @ [
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="File:";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=sprintf "%s" f;
          };
        ];          
      };
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="Line:";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=sprintf "%d" l;
          };
        ];          
      };
      ]
  else 
    rows := !rows @ [
      {
        row_cols = [
          {
            col_width=75;
            col_align='l';
            col_cont="-- Unknown location --";
          };
        ];          
      };
      ]; 
   
           
  let lines = Str.split (Str.regexp "\n") msg in 
      
  let  i = ref 0 in
  rows := !rows @ (List.map (fun line ->
      incr i;
      if !i = 1 then
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="Description:";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=line;
          };
        ];          
      }
      else
      {
        row_cols = [
          {
            col_width=15;
            col_align='l';
            col_cont="";
          };
          {
            col_width=60;
            col_align='l';
            col_cont=line;
          };
        ];          
      };
    ) lines);
                         
  let lines = format_table {
      tab_outline = true;
      tab_width = 75;
      tab_title = "ERROR REPORT";
      tab_header = [];
      tab_rows = !rows;
    } in
  compiler.t_silent <- false;
  out_ind := 2; out ""; 
  List.iter (fun l -> out l) lines
    
    
let print_src src = 
  if src.s_file <> "" || src.s_line > 0 then
    sprintf "File \"%s\", line %d" src.s_file src.s_line
  else ""

let error id msg =
  let lines = print_err_tab id msg in
  compiler.t_silent <- false;
  out_ind := 2; out ""; 
  List.iter (fun l -> out l) lines;
  raise Exit

let print_error msg =
  let lines = print_err_tab (-1) msg in
  compiler.t_silent <- false;
  out_ind := 2; out ""; 
  List.iter (fun l -> out l) lines  

let warning msg =
  let f = an.a_curpos.s_file in
  let f' = an.a_funpos.s_file in
  let l = an.a_curpos.s_line in
  let l' = an.a_funpos.s_line in
  let str = sprintf "Warning %s: %s"
                    (if l > 0 then sprintf "[file <%s>, line %d]" f l else "") msg in
  let lines = Str.split (Str.regexp "\n") str in 
  List.iter (fun l -> out l;) lines

let warning_ msg =
  let f = an.a_curpos.s_file in
  let f' = an.a_funpos.s_file in
  let l = an.a_curpos.s_line in
  let l' = an.a_funpos.s_line in
  let str = sprintf "Warning %s: %s"
                    (if l > 0 then sprintf "[file <%s>, line %d]" f l else "") msg in
  let lines = Str.split (Str.regexp "\n") str in 
  List.iter (fun l -> out l;out_ l) lines

let info msg =
  let f = an.a_curpos.s_file in
  let f' = an.a_funpos.s_file in
  let l = an.a_curpos.s_line in
  let l' = an.a_funpos.s_line in
  let str = sprintf "Info %s: %s"
                    (if l > 0 then sprintf "[file <%s>, line %d]" f l else "") msg in
  let lines = Str.split (Str.regexp "\n") str in 
  List.iter (fun l -> out l) lines

let where () =
  let f = an.a_curpos.s_file in
  let f' = an.a_funpos.s_file in
  let l = an.a_curpos.s_line in
  let l' = an.a_funpos.s_line in
  let str1 = if l > 0 then sprintf "[file <%s>, line %d]" f l else "" in
  let str2 = if l' > 0 then sprintf " [in file <%s>, line %d]" f' l' else "" in
  str1^str2
    
(*
** Return [x],last from l, initial rem=[]
*)
let rec split_last l rem =
    match l with
    | [hd] -> [],hd;
    | hd :: [last] -> rem @ [hd], last;
    | hd :: tl -> split_last tl (rem@[hd]);
    | [] -> error 830353 "" 

(*
** Print debug messages. 
*)

let (debug_it_select_list:string list ref) = ref []
let debug_it target = List.mem target !debug_it_select_list
let debug_it_print target msg = 
  out (box 0 (sprintf "DEBUG[%s]: %%s" target) msg);
  out_ (box 0 (sprintf "DEBUG[%s]: %%s" target) msg)
  

(*
** Synthesis Environment Interface
*)
let (lookup_env: (string -> Cp_types.env_expr list) option ref) = ref None
let (store_env: (string -> Cp_types.env_expr -> unit) option ref) = ref None
let (top_env:(env list) ref) = ref []
  
let get_env_val name =
  match !lookup_env with
  | Some f -> f name
  | None -> error 0 (sprintf "No environment found.")
  
let get_env name =
  List.hd (get_env_val name)
  
let get_env_int64 name =
  match get_env name with
  | ENV_str s -> (try Int64.of_string s with _ -> error 0 (sprintf "Unexpected integer value <%s> found." s));
  | ENV_int i -> i
  | ENV_bool b -> if b then Int64.one else Int64.zero
  | _ -> error 0 (sprintf "Expected integer value not found.")

let get_env_str name =
  match get_env name with
  | ENV_str s -> s
  | ENV_int i -> Int64.to_string i
  | ENV_bool b -> if b then "treu" else "false"
  | _ -> error 0 (sprintf "Expected string value not found.")
  
let get_env_int name =
  Int64.to_int (get_env_int64 name)
    
let get_env_bool name =
  (get_env_int64 name) = Int64.one 
    
let get_enc_format name =
  match get_env name with
  | ENV_str "binary" -> Enc_binary 
  | ENV_str "onehot" -> Enc_onehot 
  | ENV_str "gray" -> Enc_gray 
  | _ -> Enc_auto

let set_env name v =
  match !store_env with
  | Some f -> f name v
  | None -> error 0 (sprintf "No environment found.")

let print_env ev =
  let func_name ft =
    match ft with
    | F_INDEX -> "index"
    | F_SIZE -> "size"
    | F_WIDTH -> "width"
    | F_INDEX_WIDTH -> "index_width"
    | F_EXPAND -> "expand"
    | F_MAX -> "max"
    | F_MIN -> "min"
    | F_NTH -> "nth"
    | F_MEMBER -> "member"
    | F_PRINT -> "print"
    | F_REV -> "rev"
    | F_TO_LOGIC -> "to_logic"
    | F_TO_STRING -> "to_string"
    | F_TO_INT -> "to_int"
    | F_TO_NAT -> "to_nat"
    | F_TO_BOOL -> "to_bool"
    | F_USER fn -> fn in
  let rec pr tdv =
    match tdv with
    | ENV_str str -> sprintf "\"%s\"" str
    | ENV_logic str -> sprintf "%s" str
    | ENV_int n -> Int64.to_string n
    | ENV_bool b -> if true then "true" else "false"
    | ENV_sig sl -> sprintf "§(%s)" (pl sl)
    | ENV_range (dir,r1,r2) -> 
    begin
      match dir with
      | '=' -> (pr r1)
      | '+' -> sprintf "%s to %s" (pr r1) (pr r2)
      | '-' -> sprintf "%s downto %s" (pr r1) (pr r2)
      | _ -> "?" 
    end;
    | ENV_op (op1,op,op2) -> sprintf "(%s) %s (%s)" (pr op1) op (pr op2)
    | ENV_var tn -> sprintf "$%s" tn
    | ENV_sel (tn,ENV_str sel) -> sprintf "%s'%s" (pr tn) sel
    | ENV_sel (tn,ts) -> sprintf "%s[%s]" (pr tn) (pr ts)
    | ENV_fun (ft,args) -> sprintf "%s()" (func_name ft)
    | ENV_set el -> sprintf "[%s]" (pl el) 
  and pl l  =
    match l with
    | [hd] -> pr hd;
    | hd::tl -> sprintf "%s;%s" (pr hd) (pl tl);
    | [] -> ""  in
  pr ev
