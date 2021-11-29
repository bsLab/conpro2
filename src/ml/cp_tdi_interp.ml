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
**      BSSLAB, Dr. Stefan Bosse http://www.bsslab.de
**
**      COPYRIGHT: THIS SOFTWARE, EXECUTABLE AND SOURCE CODE IS OWNED BY BSSLAB.
**                 THIS SOURCE CODE MAY NOT BE COPIED, EXTRACTED,
**                 MODIFIED, OR OTHERWISE USED IN A CONTEXT
**                 OUTSIDE OF THE SOFTWARE SYSTEM.
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     8.7.2008
**    $VERSION:     2.37
**
**    $INFO:
**
**  Tool Description Interface - Interpreter and Synthesis
**
**    $ENDOFINFO
**
*)
open Cp_common
open Cp_types
open Cp_syntax
open Cp_symbol
open Cp_utils
open Cp_data
open Cp_printtypes
open Cp_data_core
open Printf
open Cp_fun
open Cp_tdi_types
open Cp_print


  
class tdi tname tops lmap =
  object (self)    
  (*
  ** Instantiate an object from this TDI module description
  *)            
  method instance = Oo.copy self 
    
  (*
  ** Module, object and type name
  *)
  val tname = tname 
  val mutable oname = "root"
  val mutable target = L_BASH

  val empty_env : ((string,tdi_env) Hashtbl.t) = Hashtbl.create 1

  val mutable env: ((string,tdi_env) Hashtbl.t) = Hashtbl.create 100
  val mutable funs = []
  val mutable files = []
  val mutable params: (string list) = []
  val mutable targets: ((string * string list) list)  = []
  
  val mutable version = ""
  

  val oc = ref (Pervasives.stdout)
  val mutable out_indent = 0
  method private out str = 
    output_string !oc ((ibox out_indent str)^"\n")
  method private ind_incr = out_indent <- out_indent + 2
  method private ind_decr = out_indent <- out_indent - 2
  
  method set_oname str = oname <- str
  
  val mutable verbose_level = 0  
  method info = sprintf "Tool %s.%s" tname oname
  method private err str = error 0 (sprintf "TDI <%s>: %s" self#info str)
  method private print level str = 
    if level <= verbose_level then out (lbox !out_ind
                                          (sprintf "TDI [%s]%s%s" self#info 
                                                (if str <> "" && str.[0] <> '#' && str.[0] <> ':' then ": " else "") 
                                                str))
  method private cur_env =
    match funs with
    | f :: _ -> f.tdf_env
    | [] -> empty_env

  method private print_expr tm =
    let rec pr tm =
      match tm with
      | TDE_str str -> sprintf "<TDE_str \"%s\">" str
      | TDE_int n -> sprintf "<TDE_int %d>" n
      | TDE_op (op1,op,op2) -> sprintf "<TDE_op (%s) %s (%s)>" (pr op1) op (pr op2)
      | TDE_var tn -> sprintf "<TDE_var $%s>" tn 
      | TDE_sel (tn,ts) -> sprintf "<TDE_sel $%s[%s]>" tn (pr ts) 
      | TDE_fun (ft,args) -> sprintf "<TDE_fun %s()>" (self#func_name ft) in
    pr tm
  
  (*
  ***************************************************
  ** Files
  ***************************************************
  *)
  method private file_open fname =
    let n = List.length files in
    try 
    begin
        let (fname,fid) = List.find (fun (n,id) -> n = fname) files in
        fid
    end
    with Not_found -> 
    begin
      files <- files @ [fname,n];
      n
    end;
    
      
  method private file_id fname =
    try 
    begin
        let (fname,fid) = List.find (fun (n,id) -> n = fname) files in
        fid
    end
    with Not_found -> error 0 (sprintf "TDI<%s>: unknown filename <%s> found." self#info fname)
  
  (*
  ***************************************************
  ** Functions
  ***************************************************
  *) 
  method private  func_map fname = 
    let argn c = Array.to_list (Array.create 10 c) in
      
    match fname with
    | "print"  -> F_PRINT,'_',(argn 's') 
    | "print_line" -> F_PRINT_LINE ,'_',(argn 's')
    | "print_file" -> F_PRINT_FILE ,'_',(argn 's')
    | "write" ->  F_WRITE,'_',(argn 's')
    | "write_line" ->  F_WRITE_LINE,'_',(argn 's')
    | "create_file" -> F_CREATE_FILE,'_',['s'] 
    | "open_file" -> F_OPEN_FILE,'_',['s'] 
    | "append_file" -> F_APPEND_FILE,'_',['s'] 
    | "remove_file" -> F_REMOVE_FILE,'_',(argn 's') 
    | "rename_file" -> F_RENAME_FILE,'_',['s';'s'] 
    | "copy_file" -> F_COPY_FILE ,'_',['s';'s']
    | "move_file" -> F_MOVE_FILE ,'_',['s';'s']
    | "list" -> F_LIST , '_' , ['$';'s']
    | "remove_dir" -> F_REMOVE_DIR ,'_',(argn 's')
    | "make_dir" -> F_MAKE_DIR ,'_',['s']
    | "change_dir" -> F_CHANGE_DIR ,'_',['s';'s']
    | "exit" -> F_EXIT,'_',[]
    | "exec" ->  F_EXEC,'i',['s']
    | "exec_log" ->  F_EXEC_LOG,'i',['s';'s']
    | "exec_write" -> F_EXEC_WRITE,'i',['s';'s']@(argn 's')
    | "basename" -> F_BASENAME,'s',['s']
    | "chop_extension" -> F_CHOPEXT,'s',['s']
    | "dirname" -> F_DIRNAME,'s',['s']
    | "get_env" -> F_GETENV,'s',['$';'s']
    | "get_opt" -> F_GETOPT,'s',['$';'s']
    | "get_opts" -> F_GETOPTS,'s',['$';'s']
    | "date" -> F_DATE,'s',[]
    | "time" -> F_TIME,'i',[]
    | "export" ->  F_EXPORT,'_',['s']
    | "port" ->  F_PORT,'s',['s';'s']
    | "exist" ->  F_EXIST,'i',['s']
    | "rev" -> F_REV,'$',['$']
    | _ -> F_USER fname,'?',['?']
    
  method private func_name ft =
    match ft with
    | F_PRINT -> "print"
    | F_PRINT_LINE -> "print_line"
    | F_PRINT_FILE -> "print_file"
    | F_WRITE -> "write"
    | F_WRITE_LINE -> "write_line"
    | F_OPEN_FILE -> "open_file"
    | F_CREATE_FILE -> "create_file"
    | F_APPEND_FILE -> "append_file"
    | F_REMOVE_FILE -> "remove_file"
    | F_RENAME_FILE -> "rename_file"
    | F_COPY_FILE -> "copy_file"
    | F_MOVE_FILE -> "move_file"
    | F_LIST -> "list"
    | F_REMOVE_DIR -> "remove_dir"
    | F_MAKE_DIR -> "make_dir"
    | F_CHANGE_DIR -> "change_dir"
    | F_EXIT -> "exit"
    | F_EXEC -> "exec"
    | F_EXEC_LOG -> "exec_log"
    | F_EXEC_WRITE -> "exec_write"
    | F_BASENAME -> "basename"
    | F_CHOPEXT -> "chop_extension"
    | F_DIRNAME -> "dirname"
    | F_GETENV -> "get_env"
    | F_GETOPT -> "get_opt"
    | F_GETOPTS -> "get_opts"
    | F_DATE -> "date"
    | F_TIME -> "time"
    | F_EXPORT -> "export"
    | F_PORT -> "port"
    | F_EXIST -> "exist" 
    | F_REV -> "rev"
    | F_USER fn -> fn
    
  method private func_type ft =
    let _,rt,atl = self#func_map (self#func_name ft) in
    rt,atl
    
  method private func_const ft args =
    match ft with
    | F_GETOPT -> true;
    | _ -> false
    
    
  (*
  *********************************************************************
  ** Formatting
  *********************************************************************
  *)
    
  method private format_func env' ft (args:tdi_expr list) =
    let fname = self#func_name ft in

    debug "tdi" with (sprintf "format_fun <%s>" fname);
    let argn = List.length args in
    let e str = error 0 (sprintf "TDI <%s>#format_fun#%s: %s."  self#info fname str) in
    let to_lower str =
      let str' = String.copy str in
      let len = String.length str in
      for i = 0 to len-1
      do
          match str.[i] with
          | 'A'..'Z' -> str'.[i] <- char_of_int ((int_of_char str.[i]) +32);
          | _ -> (); 
      done;
      str' in
      
    match ft with
    | F_CHOPEXT -> 
      let expr = List.nth args 0 in
      if self#expr_const expr then
      begin
        match target with
        | L_BASH ->
          (try sprintf "\"%s\"" (Filename.chop_extension (self#str_of_expr env' expr)) with _ -> self#format_expr env' expr)
        | _ -> e "unsupported target";
      end
      else 
      begin
        match target with
        | L_BASH -> 
          let expr = self#format_expr env' expr in
          let temp = sprintf "_CHOPEXT"  in
          self#aux [sprintf "%s=%s" temp expr];
          sprintf "${%s\\.*}" temp;
        | _ -> e "unsupported target";
      end;
    | F_BASENAME -> 
      let expr = List.nth args 0 in
      if self#expr_const expr then
      begin
        match target with
        | L_BASH ->
          (try sprintf "\"%s\"" (Filename.basename (self#str_of_expr env' expr)) with _ -> self#format_expr env' expr)
        | _ -> e "unsupported target";
      end
      else 
      begin
        match target with
        | L_BASH -> 
          let expr = self#format_expr env' expr in
          let temp = sprintf "_BASENAME"  in
          self#aux [sprintf "%s=%s" temp expr];
          sprintf "${%s##*/}" temp;
        | _ -> e "unsupported target";
      end;
    | F_DIRNAME -> 
      let expr = List.nth args 0 in
      if self#expr_const expr then
      begin
        match target with
        | L_BASH ->
          (try sprintf "\"%s\"" (Filename.dirname (self#str_of_expr env' expr)) with _ -> self#format_expr env' expr)
        | _ -> e "unsupported target";
      end
      else 
      begin
        match target with
        | L_BASH -> 
          let expr = self#format_expr env' expr in
          let temp1 = sprintf "_DIRNAME1"  in
          let temp2 = sprintf "_DIRNAME2"  in
          self#aux [sprintf "%s=%s" temp1 expr;
                    sprintf "%s=${%s%%/*}" temp2 temp1;
                    sprintf "if [ X\"$%s\" = X\"$%s\" ]" temp2 temp1;
                    "then";
                    sprintf "%s=\".\"" temp2;
                    "fi"];
          sprintf "$%s" temp2
        | _ -> e "unsupported target";
      end;

    | F_GETENV -> 
    begin
      let vn,_ = self#get_var (List.nth args 0) in
      let def = self#format_expr env' (List.nth args 1) in
      match target with
      | L_BASH ->
        let temp = sprintf "_%s" vn in
        self#aux [sprintf "if [ \"X$%s\" = \"X\" ]" vn;
                  "then";
                  sprintf "  %s=%s" temp  def;
                  "else";
                  sprintf "  %s=$%s" temp vn;
                  "fi"];
        sprintf "$%s" temp
      | _ -> e "unsupported target";
    end;
    | F_GETOPT -> 
    begin
      let vn,vs = self#get_var (List.nth args 0) in
      let def = self#format_expr env' (List.nth args 1) in
      let n = 
        match vs with
        | TDE_int n -> n
        | _ ->  e (sprintf "Unexpected non-constant array selector found.") in
      match target with
      | L_BASH ->
      begin
        match self#try_lookup_env env vn with
        | Some tde ->
          if n = (-1) && tde.tde_array then e (sprintf "Scalar access of array <%s> found." tde.tde_name);
          if tde.tde_array then
            self#format_expr env' (self#env_valn tde n)
          else
            self#format_expr env' (self#env_val tde)
        | None -> 
          self#print 0 (sprintf "Parameter <%s> not found. Using default value." vn); 
          def
      end;
      | _ -> e "unsupported target";
    end;
    | F_GETOPTS -> 
    begin
      let vn,_ = self#get_var (List.nth args 0) in
      let def = self#format_expr env' (List.nth args 1) in
      match target with
      | L_BASH ->
      begin
        match self#try_lookup_env env vn with
        | Some tde ->
          let s = ref [] in
          let rec map vl =
            match vl with
            | [v] ->
              let v' =  
                match v with
                | TDE_str str -> TDE_str (sprintf "-%s " str);
                | _ -> e (sprintf "Unsupported environment parameter type found.") in
              v'         
            | v :: tl ->
              let v' =
                match v with
                | TDE_str str -> TDE_str (sprintf "-%s " str);
                | _ -> e (sprintf "Unsupported environment parameter type found.") in
              TDE_op (v',"+",map tl)
            | [] -> e "Empty environment found." in
              
          self#format_expr env' (map tde.tde_val);
        | None -> def
      end;
      | _ -> e "unsupported target";
    end;
    
    | F_EXPORT -> 
    begin
      let ve,_ = self#get_var (List.nth args 0) in
      match target with
      | L_BASH ->
      begin
        sprintf "export %s" ve 
      end;
      | _ -> e "unsupported target";
    end;
    | F_EXIST -> 
    begin
      let vs = self#format_expr env' (List.nth args 0) in
      match target with
      | L_BASH ->
      begin
        self#aux [sprintf "[ -r %s ]" vs;
                  "if [ $? = 0 ]";
                  "then";
                  "  _RES=1";
                  "else";
                  "  _RES=0";
                  "fi"];
        "$_RES"
      end;
      | _ -> e "unsupported target";
    end;
    | F_DATE -> 
    begin
      match target with
      | L_BASH ->
        let temp = sprintf "_DATE"  in
        self#aux [sprintf "%s=`date`" temp];
        sprintf "$%s" temp
      | _ -> e "unsupported target";
    end;
    | F_TIME -> 
    begin
      match target with
      | L_BASH ->
        let temp = sprintf "_TIME"  in
        self#aux [sprintf "%s=`gdate +%%s`" temp];
        sprintf "$%s" temp
      | _ -> e "unsupported target";
    end;
    | F_PRINT ->
    begin
      match target with
      | L_BASH ->
        sprintf "echo %s" (self#format_expr env' (self#env_map args));   
      | _ -> e "unsupported target";
    end;
    | F_PRINT_LINE ->
    begin
      match target with
      | L_BASH ->
        let s = ref "" in
        List.iter (fun arg ->
          s := sprintf "%secho %s\n" !s (self#format_expr env' arg);
           ) args;
        !s  
      | _ -> e "unsupported target";
    end;
    | F_PRINT_FILE ->
    begin
      match target with
      | L_BASH ->
        let s = ref "" in
        List.iter (fun arg ->
          let fname = self#format_expr env' arg in
          s := sprintf "%scat %s\n" !s fname;
           ) args;
        !s  
      | _ -> e "unsupported target";
    end;
    | F_EXIT ->
    begin
      let arg = List.nth args 0 in
      match target with
      | L_BASH ->
      begin
        sprintf "exit %s" (self#format_expr env' arg);
      end;
      | _ -> e "unsupported target";
    end;
    | F_EXEC ->
    begin
      let arg = List.nth args 0 in
      match target with
      | L_BASH ->
      begin
        let temp = sprintf "_EXEC_RES"  in
        let exec = self#format_expr env' arg in
        self#aux [
          sprintf "EXEC=%s" exec;
          sprintf "$EXEC"; 
          sprintf "%s=$?" temp;
          ];
        sprintf "$%s" temp;
      end;
      | _ -> e "unsupported target";
    end;
    | F_EXEC_LOG ->
    begin
      let arg = List.nth args 0 in
      let arg2 = List.nth args 1 in
      match target with
      | L_BASH ->
      begin
        let temp = sprintf "_EXEC_RES"  in
        let exec = self#format_expr env' arg in
        let log = self#format_expr env' arg2 in
        self#aux [
          sprintf "EXEC=%s" exec;
          sprintf "$EXEC 2>&1 | tee %s" log; 
          sprintf "%s=$?" temp;
          ];
        sprintf "$%s" temp;
      end;
      | _ -> e "unsupported target";
    end;
    | F_EXEC_WRITE ->
    begin      
      let fname,exec,printl = 
        match args with
        | [f;e] -> 
          self#format_expr env' f,
          self#format_expr env' e,
          []
        | f :: (e  :: tl) -> 
          self#format_expr env' f, 
          self#format_expr env' e ,
          List.map (self#format_expr env') tl;
        | _ -> e "expected at least 2 arguments"; in
      let fid = self#file_id fname in
      match target with
      | L_BASH ->
      begin
        let temp = sprintf "_EXEC_RES"  in
        self#aux (
          [
            sprintf "EXEC=%s" exec;
            sprintf "{ $EXEC; echo $? > /tmp/%s; } 2>&1 | tee -a %s" temp fname;
          ]@
            (
              if printl <> [] 
                then List.map (fun line -> sprintf "echo %s >> %s" line fname) printl
                else []
            )@
          [
            sprintf "%s=`cat /tmp/%s`" temp temp;
            sprintf "rm -f /tmp/%s" temp;
          ]);
        sprintf "$%s" temp;
      end;
      | _ -> e "unsupported target";
    end;
    | F_CREATE_FILE ->
    begin
      let fname = self#format_expr env' (List.nth args 0) in
      let fid = self#file_open fname in
      match target with
      | L_BASH ->
      begin
        sprintf "rm -f %s; touch %s" fname fname;
      end;
      | _ -> e "unsupported target";
    end;
    | F_MAKE_DIR ->
    begin
      let dname = self#format_expr env' (List.nth args 0) in
      match target with
      | L_BASH ->
      begin
        sprintf "mkdir -p %s" dname;
      end;
      | _ -> e "unsupported target";
    end;
    | F_OPEN_FILE ->
    begin
      let fname = self#format_expr env' (List.nth args 0) in
      let fid = self#file_open fname in
      match target with
      | L_BASH ->
      begin
        sprintf "echo \"[Opening file <\"%s\">...]\"" fname;
      end;
      | _ -> e "unsupported target";
    end;
    | F_APPEND_FILE ->
    begin
      let src = self#format_expr env' (List.nth args 0) in
      let dst = self#format_expr env' (List.nth args 1) in
      match target with
      | L_BASH ->
      begin
        sprintf "cat %s >> %s" src dst;
      end;
      | _ -> e "unsupported target";
    end;
    | F_COPY_FILE ->
    begin
      let src = self#format_expr env' (List.nth args 0) in
      let dst = self#format_expr env' (List.nth args 1) in
      match target with
      | L_BASH ->
      begin
        sprintf "cp %s %s" src dst;
      end;
      | _ -> e "unsupported target";
    end;
    | F_MOVE_FILE ->
    begin
      let src = self#format_expr env' (List.nth args 0) in
      let dst = self#format_expr env' (List.nth args 1) in
      match target with
      | L_BASH ->
      begin
        sprintf "mv %s %s" src dst;
      end;
      | _ -> e "unsupported target";
    end;
    | F_REMOVE_FILE ->
    begin
      let files  = List.map (self#format_expr env') args in
      match target with
      | L_BASH ->
      begin
        let s = ref "" in
        List.iter (fun s' -> s := sprintf "%s %s" !s s') files;
        sprintf "rm -f %s" !s;
      end;
      | _ -> e "unsupported target";
    end;
    | F_WRITE ->
    begin
      let fname = self#format_expr env' (List.nth args 0) in
      let fid = self#file_id fname in
      match target with
      | L_BASH ->
      begin
       let s = self#format_expr env' (self#env_map (List.tl args)) in
       sprintf "echo %s >> %s" s fname;
      end;
      | _ -> e "unsupported target";
    end;
    | F_WRITE_LINE ->
    begin
      let fname = self#format_expr env' (List.nth args 0) in
      let fid = self#file_id fname in
      match target with
      | L_BASH ->
      begin
        let s = ref "" in
        List.iter (fun arg ->
          let line = self#format_expr env' arg in
          s := sprintf "%secho %s >> %s" (if !s <> "" then sprintf "%s\n" !s else "") line fname;
          ) (List.tl args);
        !s
      end;
      | _ -> e "unsupported target";
    end;
    | F_CHANGE_DIR ->
    begin
      let dirname = self#format_expr env' (List.nth args 0) in
      match target with
      | L_BASH ->
      begin
        sprintf "cd %s" dirname;
      end;
      | _ -> e "unsupported target";
    end;
    | F_REMOVE_DIR ->
    begin
      let file  = self#format_expr env' (List.nth args 0) in
      match target with
      | L_BASH ->
      begin
        sprintf "rm -rf %s" file;
      end;
      | _ -> e "unsupported target";
    end;
    | F_LIST -> 
    begin
      let v = List.nth args 0 in
      let p = List.nth args 1 in
      let vname,_ = self#get_var v in
      self#set_env env' [{
          tde_name=vname;
          tde_type='s';
          tde_val=[];
          tde_var=true;
          tde_array=true;
          tde_range=[]
        }]; 
      match target with
      | L_BASH ->
      begin
        sprintf "%s=( `ls %s` )" 
          vname
          (self#format_expr env' p)
      end;
      | _ -> e "unsupported target";
    end;
    | F_PORT -> 
    begin
      let v,_ = self#get_var (List.nth args 0) in
      let tde = 
        match self#try_lookup_env env' v with
        | Some tde -> tde;
        | None -> self#lookup_env env v in
      (*
      ** vs format: signal <s>: <d> <t> 
      *)
      let ps,pd,pt,pr =
        match Str.split (Str.regexp ":") (self#str_of_expr env' (self#env_val tde)) with
        | [s;d;t;r] -> s,d,t,r;
        | [s;d;t] -> s,d,t,"";
        | _ -> e (sprintf "Unexpected port signal format <%s> found." (self#str_of_expr env' (self#env_val tde))) in
      let ds = self#str_of_expr env' (List.nth args 1) in
      self#wrap_str (
        match Str.split (Str.regexp "::") ds with
        | ["asimut";"name"] ->
        begin
          to_lower ps;
        end;
        | ["asimut";"dir"] ->
        begin
            match pd with
            | "in" -> "IN";
            | "out" -> "OUT";
            | "inout" -> "INOUT";
            | _ -> e (sprintf "unsupported port signal direction format <%s> found." pd);
        end;
        | ["asimut";"type"] ->
        begin
            match [pt;pr] with
            | ["std_logic";""] | ["bit";""] -> "B";
            | ["signed";r]  
            | ["std_logic_vector";r] 
            | ["bit_vector";r] -> "X";
            | _ -> e (sprintf "unsupported port signal type <%s:%s> format found." pt pr)
        end;
        | ["asimut";"range"] ->
        begin
            match [pt;pr] with
            | ["std_logic";""] | ["bit";""] -> "";
            | ["signed";r]  
            | ["std_logic_vector";r] 
            | ["bit_vector";r] -> r;
            | _ -> e "unsupported port signal type format"
        end;
        | ["xilinx";"ucf"] ->
        begin
            match [pt;pr] with
            | ["std_logic";""] | ["bit";""] -> sprintf "#NET \\\"%s\\\" LOC = \\\"\\\";" ps;
            | ["signed";r]  
            | ["std_logic_vector";r] 
            | ["bit_vector";r] -> 
            begin
              match Str.split (Str.regexp "to\|downto") r with
              | [a;b] -> 
                let s = ref "" in
                let ai = int_of_string (Str.global_replace (Str.regexp " ") "" a) in
                let bi = int_of_string (Str.global_replace (Str.regexp " ") "" b) in
                if ai <= bi then 
                  for i = ai to bi 
                  do
                    if !s = "" then s := sprintf "#NET \\\"%s<%d>\\\" LOC = \\\"\\\";" ps i
                    else s := sprintf "%s\n#NET \\\"%s<%d>\\\" LOC = \\\"\\\";" !s ps i;  
                  done
                else
                  for i = bi to ai 
                  do
                    if !s = "" then s := sprintf "#NET \\\"%s<%d>\\\" LOC = \\\"\\\";" ps i
                    else s := sprintf "%s\n#NET \\\"%s<%d>\\\" LOC = \\\"\\\";" !s ps i;  
                  done;
                !s
              | _ -> e (sprintf "unexpected range found: <%s>." ps);
            end;
            | _ -> e "unsupported port signal type format"
        end;
          
        | _ -> e "unsupported port signal format");
    end;
    | F_USER fname ->
    begin
      try 
      begin
        let f = List.find (fun f -> f.tdf_name = fname) funs in
        match target with
        | L_BASH ->
          sprintf "%s" fname;
        | _ -> e "unsupported target";
      end
      with Not_found -> e (sprintf "unknown user function <%s> found." fname);
      
    end;
    | _ -> e (sprintf "unsupported function <%s> found." fname)
    
 (*
 ** Format RHS expression, check all variables in environment (env': local, env: global)
 *)
 method private format_expr env' (te:tdi_expr) =
   debug "tdi" with (sprintf "format_expr");
   let e str = error 0 (sprintf "TDI <%s>#format_expr: %s."  self#info str) in
   match target with
   | L_BASH ->
   begin
     let flat sl =
      let last_str = ref false in
      let last_var = ref false in
      let rec iter sl =
        match sl with
        | s :: tl ->
          let is_var = (String.contains s '$')  in
          let is_eval1 = (String.contains s '[') in
          let is_eval2 = (String.contains s ']') in
          let s =
            let sl = String.length s in
            if not is_var && sl > 1 && s.[0] = '"' then String.sub s 1 (sl-2) 
            else s in
          if not is_eval1 && not !last_str && is_var then
          begin
            last_var := is_var;
            last_str := not is_var;
            s^(iter tl)
          end
          else if not is_eval1 && not is_var && not !last_str then
          begin
            last_str := true;
            (sprintf "\"%s" s)^(iter tl)
          end 
          else if not is_eval1 && is_var && !last_str then
          begin
            last_var := is_var;
            last_str := not is_var;
            (sprintf "\"%s" s)^(iter tl)            
          end
          else
          begin
            last_var := is_var;
            s^(iter tl)            
          end;
        | [] -> if !last_str then "\"" else "" in
      iter sl in
            
     let rec map te =
       match te with
       | TDE_str str -> [Str.global_replace (Str.regexp "\"") "\\\"" str]
       | TDE_int n -> [sprintf "%d" n]
       | TDE_var vn ->
        let tde = 
          match self#try_lookup_env env' vn with
          | Some tde -> tde;
          | None -> self#lookup_env env vn in
        
        if tde.tde_var then
          [sprintf "$%s" vn] 
        else
          map (self#env_val tde);
      | TDE_sel (vn,vs) ->
        let tde = 
          match self#try_lookup_env env' vn with
          | Some tde -> tde;
          | None -> self#lookup_env env vn in
        let n = 
          match vs with
          | TDE_int n -> n
          | _ ->  e (sprintf "Unexpected non-constant array selector found.") in
        if tde.tde_var then
          [sprintf "$%s[%d]" vn n] 
        else
          map (self#env_valn tde n);
      | TDE_op (op1,op,op2) ->
      begin
        let t1 = self#get_type env' op1 in
        let t2 = self#get_type env' op2 in
        let et = self#expr_type t1 t2 in
        let f tde = flat (map tde) in
        match op,et with
        | "+",'s' -> (map op1)@(map op2);
        | "+",'i'  -> [sprintf "`expr %s %s %s`" (f op1) op (f op2)]
        | "<>",'s'  -> [sprintf "[ ! %s = %s ]" (f op1) (f op2)]
        | "=",'i' -> [sprintf "[ %s = %s ]" (f op1) (f op2)]
        | "<>",'i'  -> [sprintf "[ ! %s = %s ]" (f op1) (f op2)]
        | "=",'s' -> [sprintf "[ %s = %s ]" (f op1) (f op2)]
        | "and",'b' -> [sprintf "%s && %s" (f op1) (f op2)]
        | "or",'b' -> [sprintf "%s || %s" (f op1) (f op2)]
        | _ -> e (sprintf "Unsupported operator.type <%s.%c>" op et);
          
      end;
      | TDE_fun (ft,args) ->
        [self#format_func env' ft args] in
      let str = flat (map te) in
      debug "tdi#format_expr" with (str);
      str
   end;
   | _ -> e "unsupported target"
    

  (*
  ** Return printable expression string
  *)
  method private str_of_expr env' (te:tdi_expr) =
   debug "tdi" with (sprintf "str_of_expr");
   let e str = error 0 (sprintf "TDI <%s>#str_of_expr: %s."  self#info str) in
   let flat sl =
    let rec iter sl =
      match sl with
      | s :: tl -> s^(iter tl)            
      | [] ->  "" in
    iter sl in

   let rec map te =
     match te with
     | TDE_str str -> [str]
     | TDE_int n -> [sprintf "%d" n]
     | TDE_var vn ->
      let tde = 
        match self#try_lookup_env env' vn with
        | Some tde -> tde;
        | None -> self#lookup_env env vn in

      if tde.tde_var then
        e (sprintf "Unresolvable variable <%s> found." vn) 
      else
        map (self#env_val tde);
    | TDE_sel (vn,vs) ->
      let tde = 
        match self#try_lookup_env env' vn with
        | Some tde -> tde;
        | None -> self#lookup_env env vn in

      let n = 
          match vs with
          | TDE_int n -> n
          | _ ->  e (sprintf "Unexpected non-constant array selector found." ) in
      if tde.tde_var then
        e (sprintf "Unresolvable variable <%s> found." vn) 
      else
        map (self#env_valn tde n);
    | TDE_op (op1,op,op2) ->
    begin
      let t1 = self#get_type env' op1 in
      let t2 = self#get_type env' op2 in
      let et = self#expr_type t1 t2 in
      
      let f tde = flat (map tde) in
      match op,et with
      | "+",'s' -> (map op1)@(map op2);
      | _ -> e (sprintf "unsupported operator.type <%s.%c>" op et);

    end;
    | TDE_fun (ft,args) ->
      [self#format_func env' ft args] in
  flat (map te)
      
        
  (*
  *********************************************************************
  ** Utils
  *********************************************************************
  *)
  method private cur_pos p =
    if p <> {f_name="";f_cpos=0} then 
    begin
      an.a_curpos <- source_of_pos p;
    end
  method private get_pos =
    pos_of_source an.a_curpos
              

  val mutable aux_lines = []
  method private aux lines = aux_lines <- aux_lines @ lines
  method private get_aux = 
    let sl = aux_lines in
    aux_lines <- [];
    sl
  method private clear_aux = aux_lines <- []
    
  val mutable top_aux = "" 


  method private merge_strings str1 str2 =
    let str1' = String.sub str1 1 ((String.length str1)-2) in 
    let str2' = String.sub str2 1 ((String.length str2)-2) in 
    sprintf "\"%s%s\"" str1' str2' 

  method private strip v =
    if v <> "" && v.[0] = '"' then String.sub v 1 ((String.length v)-2) else v 

  method private strip_var v =
    if v.[0] = '$' then String.sub v 1 ((String.length v)-1) else v 
      
  method private strip_str str =
    let strl = Str.split (Str.regexp "\"\|\"") str in
    match strl with
    | [str'] -> str'
    | _ -> str  (* multiple string literals found *)

  method private wrap_str str =
    if str <> "" && str.[0] = '$' then str else sprintf "\"%s\"" str

  method private wrap_strl strl =
    let e str = error 0 (sprintf "TDI <%s>#wrap_strl: %s."  self#info str) in
    match target with
    | L_BASH ->
    begin
      match strl with
      | str :: tl ->
        sprintf "%s%s"
            (if str <> "" && str.[0] = '$' then str else sprintf "\"%s\"" str)
            (self#wrap_strl tl)
      | [] -> ""
    end
    | _ -> e "unsupported target"

  method private split_exec str = 
    (*
    ** Split program call string str into program name (+path), head of,  and program argument, tail of list
    *)
    let e str = error 0 (sprintf "TDI <%s>#split_exec: %s." self#info str) in
    match target with
    | L_BASH ->
    begin    
      let strl = Str.split (Str.regexp "\"\|\"") str in
      let tokens = List.map (fun str ->
                    (*
                    ** Replace space marker from below again
                    *)
                    let n = String.length str in
                    if n > 0 && str.[0] = (char_of_int 0) then str.[0] <- ' ' ;
                    if n > 0 && str.[n-1] = (char_of_int 0) then str.[n-1] <- ' '; 
                    str
                    ) (List.concat (List.map (fun str ->
                          (*
                          ** Mark leading and trailing spaces - must be preserved
                          *)
                          let n = String.length str in
                          if n > 0 && str.[0] = ' ' then str.[0] <- char_of_int 0;
                          if n > 0 && str.[n-1] = ' ' then str.[n-1] <- char_of_int 0; 
                          Str.split (Str.regexp " ") str
                          ) strl))
                          in
      let rec merge strl strm =
        let tail str = 
          let n = String.length str in
          n > 0 && str.[n-1] = ' ' in
        let head str = 
          let n = String.length str in
          n > 0 && str.[0] = ' ' in
          
        match strl with
        | str1 :: (str2 :: tl) ->
        begin
          if (head str2) || (tail str1) then [strm @ [str1]] @ (merge (str2 :: tl) [])
          else merge (str ::tl) strm; 
        end;
        | [str1] -> [strm @ [str1]]
        | [] -> []  in
                            
      List.map (fun strl -> self#wrap_strl strl) (merge tokens [])
    end
    | _ -> e "unsupported target"
    
  method private str_is_value str =
    let n = String.length str in
    let m = n - 2 in
    if m > 0 then
    begin
      match str.[0],str.[1] with
      | '0','b' -> true
      | '0','x' -> true
      | '0','o' -> true
      | '0'..'9',_ -> true
      | 'X','"' -> true
      | '\'',_ -> true
      | '"',_ -> true
      | _ -> false
    end
    else if n > 0 then
    begin
      match str.[0] with
      | '0'..'9' -> true
      | _ -> false        
    end
    else false

  method private str_is_const str =
    (*
    ** Constant value or actuallay not evaluated expression (containing variables and functions)?
    *)
    let expr = ref false in
    let in_str = ref false in
    String.iter (fun c ->
      match c with
      | '+' | '-'  | '$' | '(' | ')' -> expr := !expr || (not !in_str);
      | '"' -> in_str := not !in_str;
      | _ -> expr := !expr;) str;
    not !expr
    
  method private str_type str =
    let dot = protects(__(String.index str '.')) in
    let n = String.length str in
    let m = n - 2 in
    if m > 0 then
    begin
      match str.[0],str.[1] with
      | '0','b' -> 'i'
      | '0','x' -> 'i'
      | '0','o' -> 'i'
      | '0'..'9',_ -> if dot then 'f' else 'i'
      | '\'',_ -> 'c'
      | '"',_ -> 's'
      | '$',_ -> '$';
      | _ -> '?'
    end
    else if n > 0 then
    begin
      match str.[0] with
      | '0'..'9' -> 'i'
      | '$' -> '$' 
      | _ -> '?'     
    end
    else '?'
    

  method private expr_const te =
    match te with
    | TDE_str _ | TDE_int _ -> true
    | TDE_var vn ->
    begin
      match self#try_lookup_env env vn with
      | Some tde ->
          not tde.tde_var
      | None -> false
    end;
    | TDE_sel (vn,sn) ->
    begin
      match self#try_lookup_env env vn with
      | Some tde ->
          not tde.tde_var
      | None -> false
    end;
    | TDE_fun (ft,args) ->
      self#func_const ft args;
    | TDE_op (op1,op,op2) ->
      (self#expr_const op1) && (self#expr_const op2) 
   
  method private get_type env' te =
    let e str = error 0 (sprintf "TDI <%s>#get_type: %s" self#info str) in
    match te with
    | TDE_str _ -> 's'
    | TDE_int _ -> 'i'
    | TDE_var vn | TDE_sel (vn,_)  ->
    begin
      let tde = 
        match self#try_lookup_env env' vn with
        | Some tde -> tde;
        | None -> self#lookup_env env vn in
      if tde.tde_type = '?' then self#print 0 (sprintf "$%s has unknown type." vn);
      tde.tde_type
    end;
    | TDE_op (op1,op,op2) ->
    begin
      match op with
      | "=" | "<" | ">" | "<>" | "<=" | ">=" -> 
        let t1 = self#get_type env' op1 in
        let t2 = self#get_type env' op2 in
        let et = self#expr_type t1 t2 in
        if et = '?' then 
          e (sprintf "Different types in expression found [op1='%c' op='%s' op2='%c']" t1 op t2)
        else
          'b';
      | _ -> 
        let t1 = self#get_type env' op1 in
        let t2 = self#get_type env' op2 in
        let et = self#expr_type t1 t2 in
        if et = '?' then 
          e (sprintf "Different types in expression found [op1='%c' op='%s' op2='%c']" t1 op t2)
        else
          et
    end;
    | TDE_fun (ft,_) ->
    begin
      let rt,atl = self#func_type ft in
      rt 
    end
       
  method private expr_type t1 t2 =
    if t1 = t2 then t2
    else 
    match (t1,t2) with
    | '$',t2 -> t2
    | t1,'$' -> t1
    | _ -> '?'
                                   

  method private get_var (te:tdi_expr) =
    let e str = error 0 (sprintf "TDI <%s>#get_var: %s" self#info str) in
    match te with
    | TDE_var vn ->
      vn,(TDE_int (-1))
    | TDE_sel (vn,vs) ->
      vn,vs
    | TDE_str s ->
    begin
      if s <> "" && s.[0] = '$' then
      begin
        let name,sel = 
          let is_sel = s.[(String.length s)-1] = ']' in
          if not is_sel then
            String.sub s 1 ((String.length s)-1),TDE_int (-1)
          else match Str.split (Str.regexp "[.]") s with
          | [name;sel] -> 
            String.sub name 1 ((String.length name)-1),
            (
              try
                TDE_int (int_of_string (String.sub sel 1 ((String.length sel)-2)))
              with
                _ -> e (sprintf "Unexpected selector expression <%s> found." sel)
            )
          | [name] -> 
            String.sub name 1 ((String.length name)-1),
            TDE_int (-1)
          | _ -> e (sprintf "Invalid variable name <%s> found." s) in
        name,sel
      end
      else
        e (sprintf "Unexpected variable expression <%s> found." s);
    end;
    | _ -> e "Unexpected variable expression found."
    
    
  (*
  *********************************************************************
  ** Variable and parameter environment, for example $datawidth
  ** Parameters are immutable!
  *********************************************************************
  *)
  method set_env env new_env =
    let err str =  error 0 (sprintf "TDI <%s>#set_env: %s" self#info str) in
    let rec pr tdv =
      match tdv with
      | TDE_str str -> sprintf "\"%s\"" str
      | TDE_int n -> sprintf "%d" n
      | TDE_op (op1,op,op2) -> sprintf "(%s) %s (%s)" (pr op1) op (pr op2)
      | TDE_var tn -> sprintf "$%s" tn 
      | TDE_sel (tn,ts) -> sprintf "$%s[%s]" tn (pr ts) 
      | TDE_fun (ft,args) -> sprintf "%s()" (self#func_name ft) in
    let rec pl l =
      match l with
      | hd :: [] -> pr hd;
      | hd :: tl -> sprintf "%s,%s" (pr hd) (pl tl)
      | [] -> "" in
      
    List.iter (fun tde ->
      let name,sel = 
        let is_sel = tde.tde_name.[(String.length tde.tde_name)-1] = ']' in
        if not is_sel then
          tde.tde_name,(-1)
        else match Str.split (Str.regexp "\[\|\]") tde.tde_name with
        | [name;sel] -> name,(try int_of_string sel with _ -> err (sprintf "Invalid array selector <%s> found." sel));
        | [name] -> name,(-1);
        | _ -> err (sprintf "Invalid parameter name <%s> found." tde.tde_name) in
      try
      begin
        debug "tdi"  with (sprintf "set_env(%s)" tde.tde_name);
        let tde' = Hashtbl.find env name in
        if tde'.tde_array && not tde.tde_array then err (sprintf "Array <%s> exptected, but got scalar value." tde'.tde_name);
        if tde.tde_array && not tde'.tde_array then err (sprintf "Scalar value <%s> expected, but got array." tde'.tde_name);
  
        let add_val () =
           if sel > 0 then
            begin
              (*
              ** Add single array element n[i], with i > 0
              *)
              let n = List.length tde'.tde_val in
              let n' = max n sel in
              let vl = Array.of_list tde'.tde_val in
              let vl' = 
                if n = n' then
                begin
                  vl.(sel-1) <- self#env_val tde;
                  vl
                end                  
                else
                  Array.init n' (fun i -> if (i+1) <> sel then vl.(i) else self#env_val tde) in
              tde'.tde_val <- Array.to_list vl'; 
                 
            end
            else
              tde'.tde_val <- tde.tde_val in
                  
        let is_var = tde'.tde_var || tde.tde_var || ((not tde.tde_array) && sel = (-1)) in
        match tde'.tde_type with
        | 'i' ->
        begin
          (*
          ** Check value wether in range or not
          *)
          let checked = ref false in
          let vl = List.map (fun te ->
            match te with
            | TDE_int n -> Int64.of_int n;
            | TDE_str s -> Int64.of_string s;
            | _-> err "Unexpected value found") tde.tde_val in
          List.iter (fun v -> 
            List.iter (fun r ->
              match r with
              | TDE_point p -> checked := !checked or (v=p);
              | TDE_line (a,b) -> checked := !checked or (v <= b && v >= a)) tde'.tde_range
              ) vl;  
          if !checked then 
          begin
            if not tde'.tde_array then 
              self#print 1 (sprintf "#set_env: setting %s <%s> to value(s) [%s]." 
                             (if is_var then "variable" else "environmenet parameter") 
                             tde.tde_name 
                             (pl tde.tde_val));
            add_val ();
            tde'.tde_var <- is_var;      
          end
          else
            err (sprintf "parameter <%s> value(s) [%s] out of range."  tde.tde_name (pl tde.tde_val));
        end;
        | _ -> 
          if not tde'.tde_array then
            self#print 1 (sprintf "#set_env: setting %s <%s> to value(s) [%s]." 
                             (if is_var then "variable" else "environmenet parameter") 
                             tde.tde_name 
                             (pl tde.tde_val));
          add_val ();
          tde'.tde_var <- is_var;
          if tde'.tde_type = '?' then 
            tde'.tde_type <- tde.tde_type;
      end
      with
        Not_found -> 
        begin
          Hashtbl.add env name {tde with tde_name=name}; 
        end;     
      ) new_env

 
  method private del_env env old_env =
    List.iter (fun tde ->
      Hashtbl.remove env tde.tde_name
      ) old_env
    
  method get_env = env

  method copy_env =
    self#clear_env;
    let envl = self#get_env_list in
    List.iter (fun tde ->
      Hashtbl.add env tde.tde_name {tde with tde_name=sprintf "%s" tde.tde_name}; 
      ) envl

  method private get_env_list =
      let l = ref [] in
      Hashtbl.iter (fun _ tde -> l := !l @ [tde]) (self#get_env);
      !l
    

  method clear_env = env <- Hashtbl.create 100 
    
  method private env_val tde =
    match tde.tde_val with
    | hd :: tl -> hd;
    | _ -> TDE_str ""

  method private env_valn tde n =
    if n = 0 then self#err (sprintf "Invalid array selector value [%d] found." 0);
    let n' =  List.length tde.tde_val in
    if n > n' then self#err (sprintf "Invalid array selector value [%d] found, expected [1..%d]." n n');
    List.nth  tde.tde_val (n-1)
 
  method private env_map tdel =
    let rec map vl =
      match vl with
      | [v] -> v
      | v :: tl ->
        TDE_op (v,"+",map tl)
      | [] -> TDE_str "" in
    map tdel
    
  method print_env env =
    let indent = ref 0 in
    let spaces () = String.make !indent ' ' in 
    let rec pr tdv =
      match tdv with
      | TDE_str str -> sprintf "\"%s\"" str
      | TDE_int n -> sprintf "%d" n
      | TDE_op (op1,op,op2) -> sprintf "(%s) %s (%s)" (pr op1) op (pr op2)
      | TDE_var tn -> sprintf "$%s" tn
      | TDE_sel (tn,ts) -> sprintf "$%s[%s]" tn (pr ts)
      | TDE_fun (ft,args) -> sprintf "%s()" (self#func_name ft)
       in
    let rec pl l =
      match l with
      | hd :: [] -> pr hd;
      | hd :: tl -> sprintf "%s,\n%s%s" (pr hd) (spaces ()) (pl tl)
      | [] -> "" in
    let rec el l =
      match l with
      | hd :: [] -> 
        indent := (String.length hd.tde_name)+2+(if hd.tde_array then 1 else 0);
        sprintf "%s=%s%s%s" hd.tde_name 
                            (if hd.tde_array then "[|" else "[")
                            (if hd.tde_var then "$" else pl hd.tde_val)
                            (if hd.tde_array then "|]" else "]");

      | hd :: tl -> sprintf "%s,\n%s" (el [hd]) (el tl)
      | [] -> "" in
    let env = self#get_env_list in
    el env

  (*
  ** Add a variable, do type and range checking.
  ** First occurence is immutable iff RHS=value, hence a parameter like value.
  *)
  method private set_var env (ten:tdi_expr) (tev:tdi_expr) =
    let err str =  error 0 (sprintf "TDI <%s>#set_var: %s" self#info str) in
    let vname,vsel = self#get_var ten in
    let is_array = vsel <> (TDE_int (-1)) in
    let const = self#expr_const tev in
      
    debug "tdi"  with (sprintf "set_var(%s)" vname);
    let vtype = self#get_type env tev in
    if vtype = '?' then self#print 0 (sprintf "Unknown type in assignment $%s <= ?." vname);
    match self#try_lookup_env env vname with
    | Some tde -> 
      let check = tde.tde_type = vtype ||
                  vtype = '?' ||
                  vtype = '$' in
      if not check then
        err (sprintf "variable <%s> value doesn't match parameter type."  vname);
      if tde.tde_array && not is_array then err (sprintf "Array <%s> exptected, but got scalar value." tde.tde_name);
      if is_array && not tde.tde_array then err (sprintf "Scalar value <%s> expected, but got array." tde.tde_name);
      tde.tde_var <- true;  
      tde.tde_val <- [];
    | None ->
      self#set_env env [{
        tde_name = vname;
        tde_val = if const then [tev] else [];
        tde_range = [];
        tde_type = vtype;
        tde_var = not const;
        tde_array = is_array; 
        }]
  method private var_name (v:tdi_expr) =
    match v with
    | TDE_var vn -> vn;
    | TDE_sel (vn,_) -> vn
    | _ -> error 0 (sprintf  "TDI <%s>#var_name: Invalid variable found." self#info) 

          
  method private lookup_env env name = 
    if Hashtbl.mem env name then
      Hashtbl.find env name
    else
    begin
      self#print 0 (self#print_env env); 
      error 0 (sprintf "TDI <%s>#lookup_env: parameter <%s> not found." self#info name) 
    end
    
  method private try_lookup_env env name = 
    if Hashtbl.mem env name then
      Some (Hashtbl.find env name)
    else
      None


  method private lookup_env_int name =
    if Hashtbl.mem env name then
    begin
      let tde = Hashtbl.find env name in
      try
      begin
        match tde.tde_val with
        | (TDE_int n) :: _  -> n;
        | (TDE_str s) :: _ -> int_of_string s
        | _ -> raise Exit;
      end
      with
        _ -> error 0 (sprintf "TDI <%s>#lookup_env_int: parameter <%s> with unexpected." self#info name)
    end
    else
    begin
      self#print 0 (self#print_env env); 
      error 0 (sprintf "TDI <%s>#lookup_env_int: parameter <%s> not found." self#info name) 
    end
    
  method private lookup_env_int64 name =
    if Hashtbl.mem env name then
    begin
      let tde = Hashtbl.find env name in
      try
      begin
        match tde.tde_val with
        | (TDE_int n) :: _  -> Int64.of_int n;
        | (TDE_str s) :: _ -> Int64.of_string s
        | _ -> raise Exit;
      end
      with
        _ -> error 0 (sprintf "TDI <%s>#lookup_env_int64: parameter <%s> with unexpected value."  self#info name )
    end
    else
    begin
      self#print 0 (self#print_env env); 
      error 0 (sprintf "TDI <%s>#lookup_env_int64: parameter <%s> not found." self#info name) 
    end
 
  method private env_global name =
    match target with
    | L_BASH ->
    begin
      match self#try_lookup_env env name with
      | Some _ -> name;
      | None -> 
      begin
        match funs with
        | f :: _ ->
          sprintf "$%s_%s" f.tdf_name name
        | [] -> 
          name (* actually unknown, but must be global context *)
      end;   
    end;
    | _ -> name   
      
    
  (*
  ************************************************************
  ** Compile
  ************************************************************
  *)
  method private tt_ident (tt:tdi_syntax) =
    match tt with
    | TT_ident (pos,str) ->
      self#cur_pos pos;
      str
    | _ -> error 0 (sprintf "TDI <%s>#tt_ident: unexpected argument found."  self#info)

  method private tt_int64_val tt =
    let to_int64 str  = 
      try 
        Int64.of_string str
      with _ -> error 0 (sprintf "TDI <%s>#tt_int64_val: Invalid integer value found." self#info) in
      
    match tt with
    | TT_ident (pos,str) ->
    begin
      self#cur_pos pos;
      to_int64 str;
    end;
    | TT_var (pos,vname) -> 
    begin
      self#cur_pos pos;
      match self#try_lookup_env env vname with
      | Some tde -> to_int64 (self#str_of_expr env (self#env_val tde));
      | None -> Int64.zero;      
    end;
    | TT_value (t,(pos,str)) ->
    begin
      self#cur_pos pos;
      to_int64 str; 
    end; 
    | _ -> error 0 (sprintf "TDI <%s>#tt_int64_val: unexpected value." self#info)

  method private tt_range (tt:tdi_syntax) =
    match tt with
    | TT_sub (_,tr) -> self#tt_range tr;
    | TT_range ('-',a,b) -> Some (self#tt_int64_val b,self#tt_int64_val a); 
    | TT_range ('+',a,b) -> Some (self#tt_int64_val a,self#tt_int64_val b); 
    | TT_range ('=',a,_) -> Some (self#tt_int64_val a,self#tt_int64_val a);  
    | TT_ident _ | TT_var _ | TT_value _ -> None;     
    | _ -> error 0 (sprintf "TDI <%s>#tt_range: unexpected statement found." self#info) 
    
  method private tt_var (tt:tdi_syntax) =
    match tt with
    | TT_var (pos,str) ->
      self#cur_pos pos;
      TDE_var str
    | TT_sel (TT_var (pos,str),ts)  
    | TT_sub (TT_var (pos,str),ts) -> 
      self#cur_pos pos;
      TDE_sel (str,self#compile_expr self#cur_env ts)
    | _ -> error 0 (sprintf "TDI <%s>#tt_var: unexpected argument found."  self#info)

  (*
  ** Return type of argument, can be an expression, so a list of data type is returned
  *)
  method private tt_expr_type (tt:tdi_syntax) =
    match tt with
    | TT_value (t,(fp,str)) -> 
      self#cur_pos fp;
      [t];
    | TT_var (fp,str) ->
    begin
      self#cur_pos fp;
      match self#try_lookup_env env str with
      | Some tde ->
        [tde.tde_type]
      | None -> ['$']
    end;
    | TT_expr (op,op1,op2) -> 
    begin
      match op with
      | "<" | ">" | "<>" | "<=" | "=>" | "=" -> ['b']
      | _ ->
        (self#tt_expr_type op1)@(self#tt_expr_type op2)
    end;
    | TT_func (f,args) ->
      let fname = self#tt_ident f in
      let _,rt,_=self#func_map fname in
      [rt]
    | _ -> error 0 (sprintf "TDI <%s>#tt_expr_type: unexpected argument found." self#info)
  (*
  ** Return type of argument,
  *)
  method private tt_type (tt:tdi_syntax) =
    match tt with
    | TT_value (t,(fp,str)) -> 
      self#cur_pos fp;
      t;
    | TT_var (fp,str) ->
    begin
      self#cur_pos fp;
      match self#try_lookup_env env str with
      | Some tde ->
        tde.tde_type
      | None -> '$'
    end;
    | TT_expr (op,op1,op2) ->
    begin
      let op1_type = self#tt_type op1 in
      let op2_type = self#tt_type op2 in
      if op1_type <> '$' && op1_type <> '?' &&
         op2_type <> '$' && op2_type <> '?' &&
         op1_type <> op2_type then
           error 0 (sprintf "TDI <%s>#tt_type: different types in expressions found: op1:%c <> op2:%c."
                            self#info op1_type op2_type);
      match op with
      | "=" | "<" | "<=" | ">" | ">=" | "<>" -> 'b';
      | _ -> if op1_type <> '$' && op1_type <> '?' then op1_type else op2_type  
    end; 
    | TT_func (f,args) ->
      let fname = self#tt_ident f in
      let _,rt,_=self#func_map fname in
      rt
    | _ -> error 0 (sprintf "TDI <%s>#tt_type: unexpected argument found." self#info)

  method private compile_expr env' tm =
    debug "tdi" with (sprintf "compile_expr ");
    let e str = error 0 (sprintf "TDI <%s>#compile_expr: %s." self#info str) in
    match tm with
    | TT_value (t,(fp,str)) -> 
    begin
       self#cur_pos fp; 
       match t with
       | 's' -> TDE_str str;
       | 'i' -> TDE_int (int_of_string str);
       | _ -> e (sprintf "unsupported value type <%c>" t);
    end;
    | TT_range ('=',ts,_) ->
      self#compile_expr env' ts
    | TT_sel (TT_var(pos,str),ts) 
    | TT_sub (TT_var(pos,str),ts) ->
    begin
      self#cur_pos pos;
      let vname = str in
      let sel = self#compile_expr env' ts in
      match self#try_lookup_env env' vname  with
      | Some tde ->
      begin
       (*
       ** local variable 
       *)
       TDE_sel (vname,sel)
      end;
      | None -> 
      begin
        match self#try_lookup_env env vname with
        | Some tde ->
        begin
          (*
          ** Global variable or environment parameter!
          *)
          TDE_sel (vname,sel)
        end;
        | None ->
          self#print 0 "Global environment:";
          self#print 0 (self#print_env env);
          self#print 0 "Current environment:";
          self#print 0 (self#print_env self#cur_env);
          e (sprintf "Found undefined variable <%s>." vname); 
      end;
      
    end;
    | TT_var (fp,str) ->
    begin
      self#cur_pos fp;
      let vname = str in
      match self#try_lookup_env env' vname  with
      | Some tde ->
      begin
       (*
       ** local variable 
       *)
       TDE_var vname
      end;
      | None -> 
      begin
        match self#try_lookup_env env vname with
        | Some tde ->
        begin
          (*
          ** Global variable or environment parameter!
          *)
          TDE_var vname
        end;
        | None ->
          self#print 0 "Global environment:";
          self#print 0 (self#print_env env);
          self#print 0 "Current environment:";
          self#print 0 (self#print_env self#cur_env);
          e (sprintf "Found undefined variable <%s>." vname); 
      end;
    end;
    | TT_func (fn,args) ->
       let fname = self#tt_ident fn in
       let ft,rt,atl = self#func_map fname in 
       let an = List.length args in
       let atl = 
        let n = List.length atl in
        if n > 10 && n > an then 
        (
          let i = ref 0 in
          List.filter (fun at -> incr i; !i <= an) atl
        ) 
        else atl in
        
       if (List.length args) <> (List.length atl) then
         e (sprintf "Function <%s>: Expected %d function arguments, but got %d." fname (List.length atl) (List.length args));
       TDE_fun (ft,List.map2 (fun at arg ->
                match at with
                | '$' ->
                begin
                  match arg with
                  | TT_var (pos,vn) ->
                    self#cur_pos pos;
                    TDE_str (sprintf "$%s" vn)
                  | TT_sel (TT_var(pos,vn),ts) 
                  | TT_sub (TT_var(pos,vn),ts) ->
                    self#cur_pos pos;
                    let sel = self#str_of_expr env' (self#compile_expr env' ts) in
                    TDE_str (sprintf "$%s.[%s]" vn sel)
                  | _ -> e (sprintf "Function <%s>: expected variable argument not found." fname);
                end;
                | _ -> self#compile_expr env' arg) atl args);
    | TT_expr (op,op1,op2) ->
    begin
      let op1'  = self#compile_expr env' op1 in
      let op2'  = self#compile_expr env' op2 in
      let t1 = self#get_type env' op1' in
      let t2 = self#get_type env' op2' in
      let et = self#expr_type t1 t2 in
      if et = '?' then
       e (sprintf "Different types in expression found [op1='%c' op='%s' op2='%c']" t1 op t2);
      match (op,et) with
      | "+",'s' -> TDE_op (op1',"+",op2')
      | "+",'i' | "-",'i' | "*",'i' | "/",'i' -> TDE_op (op1',op,op2')
      | "=",_ | "<",_ | ">",_ | "<>",_ | "<=",_ | ">=",_ -> TDE_op (op1',op,op2')
      | "and",'b' | "or",'b' | "xor",'b' -> TDE_op (op1',op,op2')
      | _ ->
        out (self#print_expr op1');
        out (self#print_expr op2');
        e (sprintf "Unexpected operator '%s' and expression type '%c' found." op et);
    end;
    | _ -> e "unsupported expression statement"
    
    
  method private compile f indent tm =
    let env' = f.tdf_env in
    debug "tdi" with (sprintf "compile <%s>" f.tdf_name);
    let e str = error 0 (sprintf "TDI <%s>#compile: in function <%s>:%s."  
                           self#info f.tdf_name str) in
    let add_var vname vtype expr is_array =
      match self#try_lookup_env env vname  with
      | Some tde -> 
          let check = tde.tde_type = vtype ||
                      vtype = '?' ||
                      vtype = '$' in
          if not check then
            e (sprintf "variable <%s> value doesn't match variable type"  vname);
          tde.tde_val <- [];
          tde.tde_var <- true; 
      | None ->
      begin
        match self#try_lookup_env f.tdf_env vname  with
        | Some tde -> 
          let check = tde.tde_type = vtype ||
                      vtype = '?' ||
                      vtype = '$' in
          if not check then
            e (sprintf "variable <%s> value doesn't match variable type"  vname);
          tde.tde_val <- [];
        | None ->
          self#set_env f.tdf_env [{
            tde_name = vname;
            tde_val = [];
            tde_range = [];
            tde_type = vtype;
            tde_array = is_array; 
            tde_var=true;
            }];
        end;
      in
    
    match tm with 
    | TT_assign (lhs,rhs) -> 
      let v = self#tt_var lhs in
      let vname = self#var_name v in        
      let vtype = self#tt_type rhs in
      let expr = self#compile_expr f.tdf_env rhs in
      let vexpr = self#format_expr f.tdf_env expr in
      
      let is_array,sel = 
        match v with
        | TDE_sel (_,vs) -> true,vs
        | _ -> false,(TDE_int (-1)) in
      add_var vname vtype expr is_array;
      let sl = [
        match target with
        | L_BASH ->
          if is_array then
            ibox indent (sprintf "%s[%s]=%s" vname (self#format_expr f.tdf_env sel) vexpr)
          else
            ibox indent (sprintf "%s=%s" vname vexpr)
        | _ -> e "unsupported target";
        ] in
      let aux = List.map (ibox indent) self#get_aux in
      aux @ sl; 
    | TT_if (te,tt,tf) ->
    begin
      let te_type = self#tt_type te in
      if te_type <> 'b' then 
        e (sprintf "conditional expression is not boolean type in function" );
      let expr = self#compile_expr f.tdf_env te in
      let vexpr = self#format_expr f.tdf_env expr in
      let pre_aux = self#get_aux in
      match target with
      | L_BASH ->
        let sl = [
            ibox indent (sprintf "if %s" vexpr);
            ibox indent "then";
          ] @  (List.flatten (List.map (fun tm' -> self#compile f (indent+2) tm') tt)) @
          (if tf <> [] then 
            (ibox indent "else") :: ((List.flatten (List.map (fun tm' ->  self#compile f (indent+2) tm') tf)) @ [ibox indent "fi"])
           else [ibox indent "fi"]) in
        let aux = self#get_aux in
        pre_aux @ aux @ sl; 
      | _ -> e "unsupported target";
    end;
    | TT_func (fn,args) ->
      let fname = self#tt_ident fn in
      let ft,rt,atl = self#func_map fname in
      let args' = List.map (self#compile_expr f.tdf_env) args in
      let ff = self#format_func f.tdf_env ft args' in
      let aux = self#get_aux in
      let sl = List.map (fun line -> ibox indent line) (aux @ [ff]) in
      sl; 
    | TT_foreach (vn,va,ttl) ->
    begin
      
      let v = self#tt_var vn in
      let vname = self#var_name v in        
      let vaname,rev = 
        match va with
        | TT_var (pos,str) -> str,false
        | TT_func (TT_ident (pos,fname),[TT_var (pos',str')]) ->
        begin
          self#cur_pos pos;
          match fname with
          | "rev" -> str',true;
          | _ -> e (sprintf "unexpected function <%s> in foreach array expression found." fname);
        end;
        | _ -> e "unexpected object in foreach array expression found." in
      let tde = 
        match self#try_lookup_env env vaname with
        | Some tde -> tde;
        | None -> self#lookup_env env' vaname in
      if tde.tde_type = '?' then self#print 0 (sprintf "Array $%s has unknwon type." tde.tde_name);
      if not tde.tde_var then
      begin
        let i = ref 0 in
        let tde_i = {
              tde_name="I";
              tde_val=[];
              tde_type='i';
              tde_array=false;
              tde_var=false;
              tde_range=[];
              } in
        let code' = ref [] in
        List.iter (fun v ->
          incr i;
          let tde' = {tde with tde_name=vname;tde_val=[v]} in
          let tde_i' ={tde_i with tde_val=[TDE_int !i]} in
          self#set_env env' [tde_i'];
          self#set_env env' [tde'];
          List.iter (fun tt -> code' := !code' @ (self#compile f indent tt))  ttl;
          self#del_env env' [tde'];
          self#del_env env' [tde_i'];
           ) (if not rev then tde.tde_val else List.rev tde.tde_val);
       !code';
      end
      else
      begin
        let tde' = {tde with tde_name=vname;tde_val=[]} in
        self#set_env env' [tde'];

        let code = 
          match target with
          | L_BASH ->
          begin
            [
              ibox indent (sprintf "for %s in \"${%s[@]}\"" (self#strip_var vname) (self#strip_var vaname));
              ibox indent "do"; 
            ] @ 
            (List.concat (List.map (fun tt -> self#compile f (indent+2) tt) ttl)) @
            [
              ibox indent "done";
            ]
          end;
          | _ -> e "unsupported target" in
        self#del_env env' [tde'];
        code
      end;
    end;
    | _ ->   e (sprintf "unexpected statement found")  


  (*
  *******************************************************************************
  **  Extract variable access with modification from global parameter environment in local functions
  *******************************************************************************
  *)

  method private mod_var tm =
    let e str = error 0 (sprintf "TDI <%s>#mod_var: %s."  
                           self#info str) in
    let add_var vname  =
      match self#try_lookup_env env vname  with
      | Some tde -> 
          tde.tde_val <- [];
          tde.tde_var <- true;        
      | None -> () in
    match tm with 
    | TT_assign (lhs,rhs) -> 
      let vname = 
        match lhs with
        | TT_var (pos,str) ->
          self#cur_pos pos;
          str
        | TT_sel (TT_var (pos,str),ts)  
        | TT_sub (TT_var (pos,str),ts) -> 
          self#cur_pos pos;
          str
        | _ -> e "Unexpected object on LHS found." in
      add_var vname ;
    | TT_if (te,tt,tf) ->
    begin
      List.iter (fun tm' -> self#mod_var tm') tt;
      if tf <> [] then 
        List.iter (fun tm' -> self#mod_var tm') tf;
    end;
    | TT_func (fn,args) ->
      let fname = self#tt_ident fn in
      ()
    | TT_foreach (vn,va,ttl) ->
    begin
      let v = self#tt_var vn in
      let vname = self#var_name v in
      let vaname,rev = 
        match va with
        | TT_var (pos,str) -> str,false
        | TT_func (TT_ident (pos,fname),[TT_var (pos',str')]) ->
        begin
          self#cur_pos pos;
          match fname with
          | "rev" -> str',true;
          | _ -> e (sprintf "unexpected function <%s> in foreach array expression found." fname);
        end;
        | _ -> e "unexpected object in foreach array expression found." in
      List.iter (fun tt -> self#mod_var tt) ttl;
    end;
    | _ ->   e (sprintf "unexpected statement found")  
  (*
  ************************************************************
  ** #version section (optional)
  ************************************************************
  *)
  method private read_version = 
    let err str = error 0 (sprintf "TDI <%s>#read_version: %s" self#info str) in
    let tml = List.filter (fun tm -> 
      match tm with TT_version _ -> true | _ -> false ) tops in
    if tml = [] then
    begin
      info (sprintf "TDI <%s>: No #version section found." self#info);
    end
    else if (List.length tml) > 1 then
      err (sprintf "More than one #version section found.")
    else  match List.hd tml with
    | TT_version (pos,str) ->
      self#print 0 (sprintf "found #version %s..." str);
      version <- str;
    | _ -> err "unexpected statement in #vesion section found."
    

    
  (*
  ************************************************************
  ** #parameter section 
  ************************************************************
  *)
  method private read_parameters = 
    let e str = error 0 (sprintf "TDI <%s>#read_parameters: %s."  self#info str) in
    let tml = List.filter (fun tm -> 
      match tm with TT_parameter _ -> true | _ -> false ) tops in
    if tml = [] then
    begin
      info (sprintf "TDI <%s>: No #parameter section found." self#info);
    end;
    List.iter (fun tt ->
    match tt with
      | TT_parameter il ->
        self#print 0 (sprintf "compiling #parameter section...");
        ind_incr ();
        let rec compile indent tm =
          match tm with 
          | TT_assign (lhs,rhs) ->
          begin
            debug "tdi" with "compile:TT_assign";
            let lhs',rhs' = self#tt_var lhs,
                            self#compile_expr self#cur_env rhs in
            let is_array,sel =
              match lhs' with
              | TDE_sel (_,sel) -> true,sel
              | _ -> false,(TDE_int (-1)) in 
            self#set_var env lhs' rhs'; 
            match target with
            | L_BASH ->
              let expr = self#format_expr empty_env rhs' in
              let aux = self#get_aux in
              aux @ [
                ibox indent (sprintf "%s%s=%s" 
                                     (self#var_name lhs') 
                                     (if is_array then sprintf "[%s]" (self#str_of_expr empty_env sel)
                                      else "")
                                     expr);
              ];
            | _ -> e "unsupported target";
          end;
          | TT_sub (ev,sub) ->
          begin
            let env_name = self#tt_ident ev in
            try 
            begin
              let tde = self#lookup_env env env_name in
              (*
              ** Additional parameter range specifier found.
              *)
              let env_range = self#tt_range sub in
              match env_range with
              | Some (a,b) -> 
                if a=b then tde.tde_range <- tde.tde_range @ [TDE_point a]
                else tde.tde_range <- tde.tde_range @ [TDE_line (a,b)];
                []
              | None -> e (sprintf "duplicated parameter <%s> defintion in #parameter section found." env_name);
            end
            with Not_found ->
            begin
              e (sprintf "parameter <%s> defintion without value in #parameter section found." env_name);
            end;
          end;
          | TT_foreach (vn,va,ttl) ->
          begin
            
            let v = self#tt_var vn in
            let vname = self#var_name v in
            let vaname,rev = 
              match va with
              | TT_var (pos,str) -> str,false
              | TT_func (TT_ident (pos,fname),[TT_var (pos',str')]) ->
              begin
                self#cur_pos pos;
                match fname with
                | "rev" -> str',true;
                | _ -> e (sprintf "unexpected function <%s> in foreach array expression found." fname);
              end;
              | _ -> e "unexpected operand in foreach array expression found." in
            
            let tde = self#lookup_env env vaname in
            
            let i = ref 0 in
            let tde_i = {
              tde_name="I";
              tde_val=[];
              tde_type='i';
              tde_array=false;
              tde_var=false;
              tde_range=[];
              } in
            let params' = ref [] in
            List.iter (fun v ->
              incr i;
              let tde' = {tde with tde_name=vname; tde_val=[v];} in
              let tde_i' = {tde_i with tde_val=[TDE_int !i]} in
              self#set_env env [tde_i'];
              self#set_env env [tde'];
              List.iter (fun tt -> params' := !params' @ (compile indent tt))  ttl;
              self#del_env env [tde'];
              self#del_env env [tde_i'];
               ) (if not rev then tde.tde_val else List.rev tde.tde_val);
            !params';
          end;
          | TT_if (te,tt,tf) ->
          begin
            let te_type = self#tt_type te in
            if te_type <> 'b' then 
              e "conditional expression is not of boolean type.";
            let expr = self#compile_expr self#cur_env te in 
            let vexpr = self#format_expr empty_env expr in
             
            match target with
            | L_BASH ->
              let sl = [
                  ibox indent (sprintf "if %s" vexpr);
                  ibox indent "then";
                ] @  (List.flatten (List.map (fun tm' -> compile (indent+2) tm') tt)) @
                (if tf <> [] then 
                  (ibox indent "else") :: ((List.flatten (List.map (fun tm' ->  compile (indent+2) tm') tf)) @ [ibox indent "fi"]) 
                 else [ibox indent "fi"]) in
              let aux = self#get_aux in
              aux @ sl; 
            | _ -> e "unsupported target";
          end;
          | _ -> e "unexpected statement in #parameter section found."  in
        params <- params @ (List.flatten (List.map (fun tt -> compile 0 tt) il)); 
        ind_decr ();
      | _ ->
        e "unknown toplevel statement found.";
      ) tml
    
  (*
  ************************************************************
  ** #fun section 
  ************************************************************
  *)
  method private read_funs = 
    let err str = error 0 (sprintf "TDI <%s>#read_funs: %s" self#info str) in 
    let tml = List.filter (fun tm -> 
      match tm with TT_fun _ -> true | _ -> false ) tops in
    if tml = [] then
    begin
      info (sprintf "TDI <%s>: No #fun section found." self#info);
    end;
    (*
    ** First extract global variable access with modification in ALL functions.
    ** => backward dependency possible!!
    *)
    List.iter (fun tt ->
      match tt with
      | TT_fun (label,ttl) ->
        List.iter (fun tt -> self#mod_var tt) ttl;
      | _ ->
        err "unknown toplevel statement found."            
      ) tml;
      
    List.iter (fun tt ->
      match tt with
      | TT_fun (label,ttl) ->
        
        let fun_name = self#tt_ident label in
        self#print 0 (sprintf "compiling function <%s>..." fun_name);
        let f = {
          tdf_name = fun_name;
          tdf_code = [];
          tdf_env = Hashtbl.create 100;
        } in
        funs <- f :: funs;
        ind_incr ();
        f.tdf_code <- f.tdf_code @ (List.flatten (
            List.map (fun i -> self#compile f 0 i)  ttl)); 
        ind_decr ();
      | _ ->
        err "unknown toplevel statement found." 
      ) tml


    
  (*
  ************************************************************
  ** #targets section 
  ************************************************************
  *)
  method private read_targets = 
    let tml = List.filter (fun tm -> 
      match tm with TT_targets _ -> true | _ -> false ) tops in
    if tml = [] then
    begin
      info (sprintf "TDI <%s>: No #targets section found." self#info);
    end;
    List.iter (fun tt ->
      match tt with
      | TT_targets (tml) ->
      begin
        List.iter (fun tm ->
          match tm with
          | TT_target (label,ttl) -> 
          begin
            let target = self#tt_ident label in
            self#print 0 (sprintf "compiling target <%s>..." target);
            let f = {
              tdf_name = target;
              tdf_code = [];
              tdf_env = Hashtbl.create 100;
              } in
            targets <- targets @ [target,
                (List.flatten (
                  List.map (fun i -> self#compile f 0 i) ttl))];
          end;
          | _ -> error 0 (sprintf "TDI <%s>: unexpected statement in #targets section found." self#info)
          ) tml;
      end;
      | _ ->
        error 0 (sprintf "TDI <%s>: unknown toplevel statement found." self#info);
      ) tml
     
  (*
  ****************************************************************************
  ** EMIT
  ****************************************************************************
  *)
  
  method private emit =
    let e str = error 0 (sprintf "TDI<%s>#emit: %s" self#info str) in
    
    let proj_tde = self#lookup_env env "proj" in
    let proj = self#str_of_expr env (self#env_val proj_tde) in
    let build = 
      match target with
      | L_BASH -> sprintf "%s.%s.tool.sh" oname tname ;
      | _ -> e "unsupported target"in 
    (
      try oc := open_out (sprintf "%s%s" compiler.t_output build) with
      _ -> e (sprintf "Can't open file <%s%s>." compiler.t_output build)
    );
    self#print 0 (sprintf "#emit: Creating build script <%s>..." build);

    let emit_info () =
      match target with
      | L_BASH -> 
      begin  
        self#out "#";
        self#out (sprintf "# Tool Interface %s" self#info); 
        self#out (sprintf "# Version %s" version); 
        self#out "#";
      end;
      | _ -> e "unsupported target" in
      
      
    let envl = self#get_env_list in
    let emit_env () =
      List.iter (fun tde ->
        match target with
        | L_BASH -> 
        begin  
          let n = ref 0 in
          if not tde.tde_var then 

          List.iter (fun ve ->
            let v = self#format_expr env ve in 
            incr n;
            let a = if tde.tde_array then sprintf "[%d]" !n else "" in 
            self#out (sprintf "%s%s=%s" tde.tde_name a v);
            ) tde.tde_val;
        end;
        | _ -> e "unsupported target";
        ) envl in
      
    let emit_params () =
      List.iter (fun param ->
        self#out param;
        ) params in
        
    emit_info ();    
    emit_params ();
      
      
    List.iter (fun f ->
      (match target with
       | L_BASH -> 
          self#out (sprintf "%s()" f.tdf_name);
          self#out "{";
       | _ -> e "unsupported target";
      );
      self#ind_incr;
      if f.tdf_code = [] then
      begin
        match target with
        | L_BASH -> self#out "echo";
        | _ -> e "unsupported target";
      end
      else List.iter self#out f.tdf_code;
      self#ind_decr;
      (match target with
       | L_BASH -> self#out "}"
       | _ -> e "unsupported target";
      );
      ) funs;
    if targets = [] then e "No targets found!"
    else
    begin
      let targets' = 
        let n = ref 0 in
        let s = ref "" in
        List.iter (fun (tn,_) -> 
          incr n;
          if !n = 1 then s := tn else s := sprintf "%s %s" !s tn) targets;
        !s in 
      match target with
      | L_BASH ->
        self#out "if [ $# = 0 ]";
        self#out "then";
        self#out (sprintf "  echo \"Usage: %s [%s]\"" build targets');
        self#out "fi"; 
        self#out "for TARGET in $@ ";
        self#out "do";
        self#ind_incr;
        self#out "echo \"Building target [$TARGET] ...\"";
        self#out "case $TARGET in";
        self#ind_incr;
        List.iter (fun (tn,tl) ->
          self#out (sprintf "%s)" tn);
          self#ind_incr;
          List.iter (fun tt ->
            self#out tt;
            ) tl;
          self#out ";;";
          self#ind_decr;
          ) targets;
        self#ind_decr;
        self#out "esac";
        self#ind_decr;
        self#out "done";
      | _ -> e "unsupported target";
    end;
    close_out !oc
    
  (*
  ** Required before synthesis and after analysis
  *)
  val mutable compiled = false
  val mutable initialized = false

  method private compile_all =
    if not initialized then
    begin
      self#print 0 (sprintf "#compile_all: Initializing tool ...");
      ind_incr ();
      self#read_version;
      self#read_parameters;
      self#read_funs;
      self#read_targets;
      ind_decr ();
      initialized <- true;
    end;
    if not compiled then
    begin
      self#print 0 (sprintf "#compiler_all: Compiling tool ...");
      ind_incr ();
      self#emit;
      compiled <- true;
      ind_incr ();
      self#print 0 (sprintf "Environment is:\n\n%s" (ibox 2 (self#print_env env)));
      ind_decr ();
      ind_decr ();
    end
     


  (*
  ********************************************************
  ** Analysis and Synthesis entry point functions - API
  ********************************************************
  *)
  
 
  method new_obj obj_name params =
    self#print 0 (sprintf "#new_obj: creating object <%s>..." obj_name);
    let obj = self#instance in 
    let get_type v =
      match v with
      | TDE_str _ -> 's';
      | TDE_int _ -> 'i';
      | TDE_var _ -> '$';
      | _ -> 's' in
    obj#set_oname obj_name;
    (*
    ** Clear all parameters modified in parameter list
    *)
    obj#copy_env;
    let env = obj#get_env in
    obj#set_env env (List.map (fun (n,v) -> 
      let t = get_type v in
      let is_array =
        match Str.split (Str.regexp "\[\|\]") n with
        | [name;sel] -> 
          true;
        | _ -> false in
      
      {tde_name=n;
       tde_val=[v];
       tde_type=t;
       tde_range=[];
       tde_array=is_array;
       tde_var=false}) params);
    debug "tdi" with (self#print_env env);
    obj
    
  (*
  ** External access to this class object
  *)
  method interp cmd =
    match cmd with
    | "compile" -> self#compile_all; "ok";
    | "init" -> "ok";
    | _ -> "err";
    
      

end

