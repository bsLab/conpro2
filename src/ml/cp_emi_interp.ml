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
**    $INITIAL:     (C) 2006-2010 BSSLAB
**    $CREATED:     8.7.2008
**    $VERSION:     2.65
**
**    $INFO:
**
**  External Module Interface - Interpreter and Synthesis
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
open Cp_emi_types
open Cp_vhdl
open Cp_print
open Cp_stat
open Cp_block_frame 

(*
** First this is module class is generated one time at opening of module.
** An instance is created using the #instance method each time an object
** is created.
*)

class emi mod_name tops lmap =
  object (self)    

  initializer
    if debug_it "emi" then self#verbose 2
    
  (*
  ** Instantiate an object from this emi module description
  *)            
  method instance = Oo.copy self
  (*
  ** Module, object and type name
  *)
  val mname = mod_name 
  val mutable mname' = mod_name
  val mutable oname = "root"
  method set_oname str =  oname <- str
    
  val mutable tname = of_mod mod_name
  val mutable is_local = false
  val mutable is_array = false
  
  val mutable verbose_level = 0
  val mutable version = "?"
  val mutable my_pro_main = None
  val mutable my_module = None
  method set_module modu = my_module <- Some modu
  val mutable my_process = (None:Cp_types.process option)
  method set_process pro = my_process <- Some pro
  
  method info = sprintf "Object %s.%s.%s" mname tname oname
  method private err str = error 0 (sprintf "EMI [%s]: %s" self#info str)
  method private print level str = 
    if level <= verbose_level then out (lbox !out_ind
                                          (sprintf "EMI [%s]%s%s" self#info 
                                                (if str <> "" && str.[0] <> '#' && str.[0] <> ':' then ": " else "") 
                                                str))
  method private out str = self#print 0 str
  method private warn str = self#print 0 (sprintf "Warning: %s" str)
  val mutable methods = []
  val mutable allprocs = []

  method private print_expr em =
    let rec pl el =
      match el with
      | [em] -> pr em
      | em :: tl -> sprintf "%s,%s" (pr em) (pl tl)
      | [] -> ""
    and pr em =
      match em with
      | ENV_str str -> sprintf "<ENV_str \"%s\">" str
      | ENV_int n -> sprintf "<ENV_int %s>" (Int64.to_string n)
      | ENV_logic s -> sprintf "<ENV_logic %s>" s
      | ENV_bool b -> sprintf "<ENV_bool %b>" b
      | ENV_sig el -> sprintf "<ENV_sig [%s]>" (pl el);
      | ENV_op (op1,op,op2) -> sprintf "<ENV_op (%s) %s (%s)>" (pr op1) op (pr op2)
      | ENV_var tn -> sprintf "<ENV_var $%s>" tn 
      | ENV_sel (tn,ts) -> sprintf "<ENV_sel $%s[%s]>" (pr tn) (pr ts) 
      | ENV_range (dir,ta,tb) -> sprintf "<ENV_range %s '%c' %s>" (pr ta) dir (pr tb);
      | ENV_fun (ft,args) -> sprintf "<ENV_fun %s()>" (self#func_name ft)
      | ENV_set el -> sprintf "<ENV_set [%s]>" (pl el) in
    pr em

  (*
  ***************************************************
  ** Functions
  ***************************************************
  *) 
  method private  func_map fname = 
    let argn c = Array.to_list (Array.create 10 c) in
      
    (*
    ** argument types:
    **  $: variable
    **  s: string
    **  i: integer/natural
    **  l: logic
    **  b: bool
    **  S: signal
    **  *: operator
    **  1,2,3,...: type of #nth argument
    **    
    ** Array functions
    **  index($array,VAL|$arg): used in #access section, returns array index for element equal to VAL/ value of $arg
    **  width($array): returns required data type width in bits
    **  index_width($array): returns required index selector  width in bits
    **  size($array): returns number of array elements
    *)
    
    match fname with
    | "index"  -> F_INDEX,'i',['$';'1']
    | "size" -> F_SIZE,'i',['$']
    | "width" -> F_WIDTH,'i',['i']
    | "index_width" -> F_INDEX_WIDTH,'i',['$']
    | "expand" -> F_EXPAND,'b',['$';'$';'*';'b']
    | "max" -> F_MAX,'1',(argn '*')
    | "min" -> F_MIN,'1',(argn '*')
    | "nth" -> F_NTH,'1',['$';'i']
    | "rev" -> F_REV,'1',['$']
    | "member" -> F_MEMBER,'b',['$';'1']
    | "print" -> F_PRINT,'_',(argn 's')
    | "to_logic" -> F_TO_LOGIC,'l',['i';'i']
    | "to_string" -> F_TO_STRING,'s',['i']
    | "to_int" -> F_TO_INT,'l',['l';'i']
    | "to_nat" -> F_TO_NAT,'i',['l']
    | "to_bool" -> F_TO_BOOL,'b',['i']
    | _ -> F_USER fname,'?',[]
    
  method private func_name ft =
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
    | F_USER fn -> fn
    
  method private func_type ft =
    let _,rt,atl = self#func_map (self#func_name ft) in
    rt,atl
    
  method private func_const ft args =
    match ft with
    | F_INDEX -> true
    | F_SIZE -> true
    | F_WIDTH -> true
    | F_INDEX_WIDTH -> true
    | F_EXPAND -> false
    | F_MAX -> true
    | F_MIN -> true
    | F_NTH -> true
    | F_MEMBER -> true
    | F_PRINT -> false
    | F_REV -> false
    | F_TO_LOGIC -> false
    | F_TO_INT -> false
    | F_TO_NAT -> false
    | F_TO_STRING -> true
    | _ -> false

  (*
  ** VHDL formating
  *)
  method private format_expr em =
    let err str = error 0 (sprintf "EMI <%s>#format_expr: %s" self#info str)  in
    match em with
    | ENV_logic str -> 
      let n = String.length str in
      let m = n - 2 in
      if m > 0 then
      begin
        match str.[0],str.[1] with
        | '0','b' -> if m > 1 then sprintf "\"%s\"" (String.sub str 2 m) else sprintf "'%c'" str.[2];
        | '0','x' -> sprintf "X\"%s\"" (String.sub str 2 m);
        | _ -> str
      end
      else
        str
    | ENV_int i64 -> Int64.to_string i64;
    | ENV_bool b -> if b then "true" else "false";
    | ENV_op (op1,op,op2) when (op="=>") -> 
      let v1 = self#format_expr op1 in
      let v2 = self#format_expr op2 in
      if v2 <> "" then 
      begin
        sprintf "(%s => %s)" v1 v2;
      end
      else
        v1
    | ENV_str str -> 
      sprintf "\"%s\"" str;
    | ENV_sig _ -> self#get_sig em;
    | ENV_range ('=',r1,_) -> 
      sprintf "%s" (self#format_expr (self#expr_fold r1))
    | ENV_range ('-',r1,r2) ->
      sprintf "%s downto %s" 
              (self#format_expr (self#expr_fold r1))
              (self#format_expr (self#expr_fold r2))
    | ENV_range ('+',r1,r2) ->
      sprintf "%s to %s" 
              (self#format_expr (self#expr_fold r1))
              (self#format_expr (self#expr_fold r2))
    | ENV_fun (F_TO_LOGIC,args) ->
    begin
      let arg = List.nth args 0  in
      match self#expr_fold arg with
      | ENV_sig el -> 
      begin
        let n = Int64.to_int (self#eval_expr (List.nth args 1)) in
        if self#check_signal arg then
        begin
          let s = self#find_signal arg in
          match s.emi_sig_type with
          | DT_logic n' -> 
            if n' < n then sprintf "Ln_to_Lm(%s,%d,%d)" s.emi_sig_name n' n
            else if n' > n then sprintf "Lm_to_Ln(%s,%d,%d)" s.emi_sig_name n' n
            else s.emi_sig_name
          | DT_int n' ->
            if n' < n then sprintf "In_to_Lm(%s,%d,%d)" s.emi_sig_name n' n
            else if n' > n then sprintf "Im_to_Ln(%s,%d,%d)" s.emi_sig_name n' n
            else sprintf "I_to_L(%s)" s.emi_sig_name 
          | DT_natural _  ->
            sprintf "N_to_Lm(%s,%d)" s.emi_sig_name n
          | _ -> "to_logic(S?)"  
        end
        else if self#check_constant arg then
        begin
          let s = self#find_constant arg in
          match s.emi_const_type with
          | DT_logic n' -> s.emi_const_val
          | DT_int n' -> s.emi_const_val
          | DT_natural _  -> 
            let v = V_int (Int64.of_string s.emi_const_val)  in
            val_str (DT_logic n) v;
            
          | _ -> sprintf "to_logic(C? %s)" (sprint_dt  s.emi_const_type)
        end
        else "to_logic(?)"
      end;
      | ENV_int i64 -> 
        let n = self#eval_expr (List.nth args 1) in
        if n = Int64.one && i64 > Int64.one then
          "'Z'" 
        else
        begin
          let v = V_int i64 in
          val_str (DT_logic (Int64.to_int n)) v;
        end;
       | _ -> err (sprintf "Unexpected object in type conversion (to_logic) found.");
    end;
    | ENV_fun (F_TO_INT,args) ->
    begin
      let arg = List.nth args 0  in
      match arg with
      | ENV_sig el -> 
      begin
        let n = Int64.to_int (self#eval_expr (List.nth args 1)) in
        if self#check_signal arg then
        begin
          let s = self#find_signal arg in
          match s.emi_sig_type with
          | DT_logic n' -> 
            if n' < n then sprintf "Ln_to_Im(%s,%d,%d)" s.emi_sig_name n' n
            else if n' > n then sprintf "Lm_to_In(%s,%d,%d)" s.emi_sig_name n' n
            else sprintf "L_to_I(%s)" s.emi_sig_name
          | DT_int n' ->
            if n' < n then sprintf "In_to_Im(%s,%d,%d)" s.emi_sig_name n' n
            else if n' > n then sprintf "Im_to_In(%s,%d,%d)" s.emi_sig_name n' n
            else  s.emi_sig_name 
          | DT_natural _  ->
            sprintf "N_to_Im(%s,%d)" s.emi_sig_name n
          | _ -> "to_int(S?)"  
        end
        else if self#check_constant arg then
        begin
          let s = self#find_constant arg in
          match s.emi_const_type with
          | DT_logic n' -> s.emi_const_val
          | DT_int n' -> s.emi_const_val
          | DT_natural _  -> 
            let v = V_int (Int64.of_string s.emi_const_val)  in
            sprintf "signed'(%s)" (val_str (DT_logic n) v);
            
          | _ -> sprintf "to_int(C? %s)" (sprint_dt  s.emi_const_type)
        end
        else "to_int(?)"
      end;
      | ENV_int i64 -> 
        let n = self#get_int (List.nth args 1) in
        if n = Int64.one && i64 > Int64.one then
          "'Z'" 
        else
        begin
          let v = V_int i64 in
          val_str (DT_int (Int64.to_int n)) v;
        end;
       | _ -> err (sprintf "Unexpected object in type conversion (to_logic) found.");
    end;
    | ENV_fun (F_TO_NAT,args) ->
    begin
      match List.nth args 0 with
      | ENV_sig el -> 
        let s = self#get_sig (ENV_sig el) in
        sprintf "(L_to_N(%s))" s;
      | ENV_int i64 -> 
        let n = self#get_int (List.nth args 1) in
        begin
          Int64.to_string i64
        end;
       | _ -> err (sprintf "Unexpected object in type conversion (to_logic) found.");
    end;
    | ENV_var vn ->
    begin 
      let check =
        match vn with
        | "RES" | "CLK"  -> true;
        | _ -> 
        begin
          match self#try_lookup_env vn with
          | Some _ -> true;
          | None -> false 
        end in
      if not check then err (sprintf "Unknown parameter <$%s> found." vn);
      let eme = self#lookup_env vn in
      if self#is_obj eme then
        self#format_expr (self#env_obj eme)
      else
        self#format_expr (self#env_val eme)
    end;  
    | ENV_op (op1,op,op2) -> 
      let op1' = self#format_expr op1 in
      let op2' = self#format_expr op2 in
      let op',unary =
        match op with
        | "+" | "-" | "*" | "/" | "%" -> op,false
        | "=" | "/=" | "<" | ">" | "<=" | "=>" -> op,false
        | "&" | "||" | "and" | "or" | "xor" -> op,false
        | "not" -> op,true
        | _ -> err (sprintf "Unexpected operator <%s> found." op) in
      if not unary then 
        sprintf "(%s) %s (%s)" op1' op' op2' 
      else
        sprintf "%s (%s)" op' op2'
              
    | ENV_sel (em',sel') ->
    begin
      match em' with
      | ENV_sig _ ->
      begin
        let s = self#get_sig em' in
        match sel' with
        | ENV_str s'  ->
          sprintf "%s'%s" s s'
        | ENV_sig _ -> 
          sprintf "%s'%s" s (self#format_expr sel')
        | ENV_range _ -> 
          sprintf "%s(%s)" s (self#format_expr sel')
        | _ -> err (sprintf "Unexpected selector found.");  
      end;
      | ENV_var vn ->
      begin
        let eme = self#lookup_env vn in
        let s = self#get_sig (self#env_val eme) in
        self#format_expr (ENV_sel (ENV_sig [ENV_str s],sel'))
      end;
      | _ -> out_ (print_env em); err (sprintf "Unexpected selected object found.");
    end; 
        
    | ENV_fun (F_INDEX, args) ->
    begin
      let va = self#get_var (List.nth args 0) in
      let vt = self#format_expr (List.nth args 1) in
      let eme = self#lookup_env va in
      
      let rec index vl n = 
        match vl with
        | v' :: tl ->
        begin
          let v'' = self#format_expr v' in
          if v'' = vt then n
          else index tl (n+1)
        end
        | [] -> err (sprintf "Array <$%s> element <%s> not found." va vt) in
      let n = List.length eme.env_val in
      let w = const_width (V_int (Int64.of_int n)) in
      let n' = index (List.rev eme.env_val) 0 in
      val_str (DT_logic w) (V_int (Int64.of_int n'))
    end;
    | ENV_fun (F_NTH, args) ->
    begin
      let va = self#get_var (List.nth args 0) in
      let eme = self#lookup_env va in
      let n = Int64.to_int (self#get_int (List.nth args 1)) in
      let n' = List.length eme.env_val in
      if n < 1 || n > n' then err (sprintf "Function nth: Array <%s> out of bound access (1..%d, but %d)." va n' n);
      if (self#env_type eme) <> 'i' then
        err (sprintf "Function nth: Unsupported array <%s> type <%c> in expression found." va eme.env_type);
      let m = self#env_valn eme n in
      self#format_expr m
    end;
    | ENV_fun (F_EXPAND,args) ->
    begin
      let vp = self#get_var (List.nth args 1) in
      let vep = {
              env_name=vp;
              env_val=[];
              env_type='*';
              env_attr=[ENV_scalar];
              env_obj=[];
              env_range=[];
              } in

      let op = 
        match List.nth args 2 with
        | ENV_str s -> s
        | _ -> err (sprintf "Function member: Unexpected operator found.") in
      let expr = List.nth args 3 in
      
      let rec el em =
        match em with
        | ENV_var vn ->
          let eme = self#lookup_env vn in
          eme.env_val
        | ENV_sel (ENV_var "P",ENV_str asel) ->
    
          let procs = ref [] in
          List.iter (fun meth ->
                          procs := !procs @ (List.map (fun pro -> pro.pro_name) meth.emm_procs))
                            (List.filter (fun meth ->meth.emm_name = asel) methods);
          List.map (fun p -> ENV_str p) !procs
        | ENV_op o -> 
          let procs = List.map (fun pro -> pro.pro_name) allprocs in
          let procs' = self#foreach_set em procs in
          List.map (fun p -> ENV_str p) procs'
        | _ -> err (sprintf "Function expand: Unexpected array expression found (arg 1)");
        in
      let el = el (List.nth args 0) in
      let rec iter vl =
        match vl with
        | [v'] -> 
          self#set_env [{vep with env_val=[v']}];
          self#format_expr expr;
          
        | v' :: tl ->
          self#set_env [{vep with env_val=[v']}];
          let expr1 = self#format_expr expr in 
          let expr2 = iter tl in
          sprintf "(%s) %s %s" expr1 op expr2
        | [] -> ""  in
      iter el
    end; 
    | ENV_fun (F_MEMBER,args) ->
    begin
      let va,vs = self#get_var_sel (List.nth args 0) in
      let vp = List.nth args 1 in
      let eme = self#lookup_env va in
      let v = 
        match vp with
        | ENV_var vn ->
          self#format_expr (self#env_val (self#lookup_env vn))
        | _ -> self#format_expr vp in
      let rec iter vl =
        match vl with
        | v' :: tl ->
          let v'' = self#format_expr v' in
          if v'' = v then "true" else iter tl
        | [] -> "false"  in
      match va,vs with
      | aname,"" ->
        let eme = self#lookup_env aname in
        iter eme.env_val
      | "P",asel ->
        let procs = ref [] in
        List.iter (fun meth ->
                        procs := !procs @ (List.map (fun pro -> pro.pro_name) meth.emm_procs))
                          (List.filter (fun meth ->meth.emm_name = asel) methods);
        iter (List.map (fun s -> ENV_str s) !procs);
      | _ -> err ("Invalid array selector found.");
    end;
    | ENV_fun (F_SIZE,args) ->
      let vn,vs = self#get_var_sel (List.nth args 0) in
      let vs,filter =
        if vs = "" then "",false
        else if vs.[0] = '!' then
        begin
          String.sub vs 1 ((String.length vs)-1),true
        end
        else vs,false in
      let va = if vs = "" then vn else sprintf "%s.%s" vn vs in 
      let eme = self#lookup_env va in
      let n = 
        if filter && vn = "P" then
        begin
          (*
          ** Filter out MOD_!
          *)
          let procs = List.filter (fun ev ->
                        match ev with
                        | ENV_str p -> not (self#modu_pro p);
                        | _ -> true) eme.env_val in
          List.length procs;
        end
        else 
          List.length eme.env_val in
      let w = const_width (V_int (Int64.of_int n)) in
      val_str (DT_logic w) (V_int (Int64.of_int n))
    | _ ->  err (sprintf "Unexpected expression statement found.")

  (*
  ** Returns int64 result value
  *)
  method private eval_expr em =
    let err str = error 0 (sprintf "EMI <%s>#eval_expr: %s" self#info str)  in
      
    match em with
    | ENV_int _
    | ENV_str _ 
    | ENV_logic _
    | ENV_bool _ -> self#get_int em;
    | ENV_range (dir,a,b) ->
    begin
      match dir with
      | '=' -> (self#eval_expr a)
      | '+' -> Int64.add (Int64.sub (self#eval_expr b)  (self#eval_expr a)) Int64.one
      | '-' -> Int64.add (Int64.sub (self#eval_expr a)  (self#eval_expr b)) Int64.one
      | _ -> err (sprintf "unexpected range '%c' found" dir);
    end;
    | ENV_var vn -> 
      let check =
        match vn with
        | "RES" | "CLK" | "O" | "P" -> false;
        | _ -> 
        begin
          match self#try_lookup_env vn with
          | Some _ -> true;
          | None -> false 
        end in
      if not check then err (sprintf "Unknown or invalid parameter <$%s> found." vn);
      let eme = self#lookup_env vn in
      self#eval_expr (self#env_val eme)
    | ENV_op (op1,op,op2) -> 
      let op1' = self#eval_expr op1 in
      let op2' = self#eval_expr op2 in
      let res =
        match op with
        | "+" -> Int64.add op1' op2'
        | "-" -> Int64.sub op1' op2'
        | "*" -> Int64.mul op1' op2'
        | "^" -> if (op1' = (Int64.of_int 2)) then Int64.shift_left Int64.one (Int64.to_int op2') 
                 else err (sprintf "Unexpected exponent base found (only 2 is supported)");
        | "/" -> Int64.div op1' (if op2' = Int64.zero then err (sprintf "Division by zero.") else op2')
        | "=" -> if op1' = op2' then Int64.one else Int64.zero 
        | "/=" -> if op1' <> op2' then Int64.one else Int64.zero 
        | "<" -> if op1' < op2' then Int64.one else Int64.zero 
        | "<=" -> if op1' <= op2' then Int64.one else Int64.zero 
        | ">" -> if op1' > op2' then Int64.one else Int64.zero 
        | ">=" -> if op1' >= op2' then Int64.one else Int64.zero 
        | _ -> err (sprintf "Unexpected operator <%s> found." op) in
      res
        
    | ENV_fun (F_INDEX, args) ->
    begin
      let vn,vs = self#get_var_sel (List.nth args 0) in
      let va = if vs = "" then vn else sprintf "%s.%s" vn vs in 
      let eme = self#lookup_env va in
      let at = self#get_type (List.nth args 1) in
      
      match at with
      | 'i' ->
        let vt = self#eval_expr (List.nth args 1) in
        let rec index vl n = 
          match vl with
          | v' :: tl ->
          begin
            let v'' = self#eval_expr v' in
            if v'' = vt then n
            else index tl (n+1)
          end
          | [] -> err (sprintf "Function index: Array <$%s> element <%s> not found." va (Int64.to_string vt)) in
        let n = List.length eme.env_val in
        let w = const_width (V_int (Int64.of_int n)) in
        let n' = index (List.rev eme.env_val) 0 in
        Int64.of_int n'
      | 's' | 'U' ->
        let vt = self#eval_expr_str (List.nth args 1) in
        let rec index vl n = 
          match vl with
          | v' :: tl ->
          begin
            let v'' = self#eval_expr_str v' in
            if v'' = vt then n
            else index tl (n+1)
          end
          | [] -> err (sprintf "Function index: Array <$%s> element <%s> not found." va vt) in
        let n = List.length eme.env_val in
        let w = const_width (V_int (Int64.of_int n)) in
        let n' = index (List.rev eme.env_val) 0 in
        Int64.of_int n'
      | _ -> err (sprintf "Function index: Unsupported array search argument type '%c' found." at)
    end;
    | ENV_fun (F_NTH, args) ->
    begin
      let vn,vs = self#get_var_sel (List.nth args 0) in
      let va = if vs = "" then vn else sprintf "%s.%s" vn vs in 
      let eme = self#lookup_env va in
      let n = Int64.to_int (self#get_int (self#expr_fold (List.nth args 1))) in
      let n' = List.length eme.env_val in
      if n < 1 || n > n' then err (sprintf "Function nth: Array <%s> out of bound access (1..%d, but %d)." va n' n);
      if (self#env_type eme) <> 'i' then
        err (sprintf "Function nth: Unsupported array <%s> type <%c> in expression found." va eme.env_type);
      let m = self#env_valn eme n in
      self#eval_expr m
    end;
    | ENV_fun (F_MAX,args) -> 
      let argn = List.length args in
      if argn = 1 then
      begin
        let vn,vs = self#get_var_sel (List.nth args 0) in
        let va = if vs = "" then vn else sprintf "%s.%s" vn vs in 
        let ve = self#lookup_env va in

        let m = ref (Int64.min_int) in
        List.iter (fun em' ->
          let i = self#eval_expr em' in
          if i > !m then m := i;
          ) ve.env_val;
        !m
      end
      else
      begin
        let m = ref (Int64.min_int) in
        List.iter (fun em' ->
          let i = self#eval_expr em' in
          if i > !m then m := i;
          ) args;
        !m
      end;
    | ENV_fun (F_MIN,args) ->
      let vn,vs = self#get_var_sel (List.nth args 0) in
      let va = if vs = "" then vn else sprintf "%s.%s" vn vs in 
      let ve = self#lookup_env va in

      let m = ref (Int64.max_int) in
      List.iter (fun em' ->
        let i = self#eval_expr em' in
        if i < !m then m := i;
        ) ve.env_val;
      !m
    | ENV_fun (F_MEMBER,args) ->
    begin
      let va,vs = self#get_var_sel (List.nth args 0) in
      let vp = List.nth args 1 in
      let eme = self#lookup_env va in
      let vt = 
        match vp with
        | ENV_var vn ->
          self#format_expr (self#env_val (self#lookup_env vn))
        | _ -> self#format_expr vp in
      let rec iter vl =
        match vl with
        | v' :: tl ->
          let v'' = self#format_expr v' in
          if v'' = vt then Int64.one else iter tl
        | [] -> Int64.zero  in
      match va,vs with
      | aname,"" ->
        let eme = self#lookup_env aname in
        iter eme.env_val
      | "P",asel ->
        let procs = ref [] in
        List.iter (fun meth ->
                        procs := !procs @ (List.map (fun pro -> pro.pro_name) meth.emm_procs))
                          (List.filter (fun meth ->meth.emm_name = asel) methods);
        iter (List.map (fun s -> ENV_str s) !procs);
      | _ -> err ("Invalid array selector found.");
    end;
    | ENV_fun (F_SIZE,args) ->
      let vn,vs = self#get_var_sel (List.nth args 0) in
      let vs,filter =
        if vs = "" then "",false
        else if vs.[0] = '!' then
        begin
          String.sub vs 1 ((String.length vs)-1),true
        end
        else vs,false in
      let va = if vs = "" then vn else sprintf "%s.%s" vn vs in 
      let eme = self#lookup_env va in
      let n = 
        if filter && vn = "P" then
        begin
          (*
          ** Filter out MOD_!
          *)
          let procs = List.filter (fun ev ->
                        match ev with
                        | ENV_str p -> not (self#modu_pro p);
                        | _ -> true) eme.env_val in
          List.length procs;
        end
        else 
          List.length eme.env_val in
      Int64.of_int n
    | ENV_fun (F_WIDTH,args) ->
    begin
      let arg = List.nth args 0 in
      let n = 
        if self#expr_const arg then
          self#eval_expr arg
        else
        begin
          let va = self#get_var arg in
          let eme = self#lookup_env va in
          let m = ref (Int64.min_int) in
          List.iter (fun em' ->
            let i = self#eval_expr em' in
            if i > !m then m := i;
            ) eme.env_val;
          !m
        end in
      let w = const_width (V_int n) in
      Int64.of_int w
    end;
    | ENV_fun (F_INDEX_WIDTH,args) ->
      let va = self#get_var (List.nth args 0) in
      let eme = self#lookup_env va in
      let n = List.length eme.env_val in
      let w = const_width (V_int (Int64.of_int n)) in
      Int64.of_int w
    | ENV_fun (ft,_) ->
      err (sprintf "Unexpected function <%s> in expression statement found." (self#func_name ft))
    | _ ->  err (sprintf "Unexpected expression statement found.")

  method private eval_expr_str em =
    let err str = error 0 (sprintf "EMI <%s>#eval_expr_str: %s" self#info str)  in
      
    match em with
    | ENV_int i64 -> Int64.to_string i64
    | ENV_str s -> s
    | ENV_logic s -> s
    | ENV_bool b -> if b then "true" else "false";
    | ENV_var vn -> 
      let check =
        match self#try_lookup_env vn with
        | Some _ -> true;
        | None -> false 
        in
      if not check then err (sprintf "Unknown or invalid parameter <$%s> found." vn);
      let eme = self#lookup_env vn in
      self#eval_expr_str (self#env_val eme)
    | ENV_op (op1,op,op2) -> 
    begin
      let op1' = self#eval_expr_str op1 in
      let op2' = self#eval_expr_str op2 in
      let res =
        match op with
        | "+" | "+.s" -> sprintf "%s%s" op1' op2'
        | _ -> err (sprintf "Unexpected operator <%s> found." op) in
      res
    end;
    | ENV_fun (F_TO_STRING,args) ->
    begin
      let arg = List.nth args 0 in
      let res = self#eval_expr arg in
      self#eval_expr_str (ENV_int res)
    end;
    | ENV_fun (F_PRINT,args) ->
    begin
      let rec pr l =
        match l with
        | hd::tl -> sprintf "%s%s" (self#eval_expr_str hd) (pr tl)
        | [] -> "" in
      pr args
    end;
    | ENV_fun (F_NTH, args) ->
    begin
      let vn,vs = self#get_var_sel (List.nth args 0) in
      let va = if vs = "" then vn else sprintf "%s.%s" vn vs in 
      let eme = self#lookup_env va in
      let n = Int64.to_int (self#get_int (List.nth args 1)) in
      let n' = List.length eme.env_val in
      if n < 1 || n > n' then err (sprintf "Function nth: Array <%s> out of bound access (1..%d, but %d)." va n' n);
      if (self#env_type eme) <> 's' then
        err (sprintf "Function nth: Unsupported array <%s> type <%c> in expression found." va eme.env_type);
      let m = self#env_valn eme n in
      self#eval_expr_str m
    end;
    | _ ->  err (sprintf "Unexpected expression statement found.")


  (*
  **
  *)


  method private get_objs em =
    let err str = error 0 (sprintf "EMI <%s>#get_objs: %s" self#info str)  in
    match em with
    | ENV_sig _ -> [self#get_sig em];
    | ENV_var vn ->
    begin
      match self#try_lookup_env vn with
      | Some eme -> self#get_objs (self#env_val eme)
      | None -> []
    end;
    | ENV_op (op1,_,op2) ->  
        (self#get_objs op1) @
        (self#get_objs op2)
    | ENV_sel (em',sel') -> (self#get_objs em')@(self#get_objs sel');
    | ENV_fun (_,args) -> List.concat (List.map self#get_objs args);
    | _ ->  [] 

  method private get_all_objs em =
    let err str = error 0 (sprintf "EMI <%s>#get_all_objs: %s" self#info str)  in
    match em with
    | ENV_sig _ -> [self#get_sig em];
    | ENV_var vn ->
    begin
      match self#try_lookup_env vn with
      | Some eme -> self#get_objs (self#env_val eme)
      | None -> []
    end;
    | ENV_op (op1,_,op2) ->  
        (self#get_all_objs op1) @
        (self#get_all_objs op2)
    | ENV_sel (em',sel') -> (self#get_objs em')@(self#get_objs sel');
    | ENV_fun (F_EXPAND,args) ->
    begin
   
      let vp = self#get_var (List.nth args 1) in

      
      let rec el em =
        match em with
        | ENV_var va ->
          let eme = self#lookup_env va in
          eme.env_val
        | ENV_sel (ENV_var "P",ENV_str asel) ->
    
          let procs = ref [] in
          List.iter (fun meth ->
                          procs := !procs @ (List.map (fun pro -> pro.pro_name) meth.emm_procs))
                            (List.filter (fun meth ->meth.emm_name = asel) methods);
          List.map (fun s -> ENV_str s) !procs;
        | ENV_op o -> 
          let procs = List.map (fun pro -> pro.pro_name) allprocs in
          let procs' = self#foreach_set em procs in
          List.map (fun s -> ENV_str s) procs'
        | _ -> err (sprintf "Function expand: Unexpected array expression found (argument 1)");
        in
      let el = el (List.nth args 0) in
      let expr = List.nth args 3 in
      List.concat (List.map (fun em ->
        self#set_env_name vp em;
        let ol = self#get_all_objs expr in
        self#del_env_name vp;
        ol
        ) el);
    end; 
    | ENV_fun (F_TO_LOGIC,args) -> List.concat (List.map self#get_all_objs args)
    | ENV_fun (_,args) -> []
    | _ ->  [] 

  method private get_name em =
    let err str = error 0 (sprintf "EMI <%s>#get_objs: %s" self#info str)  in
    match em with
    | ENV_sig _ -> self#get_sig em
    | ENV_var vn -> vn
    | ENV_op (op1,op,op2) ->
    begin
      match op with
      | "+.s" -> sprintf "%s%s"(self#get_name op1) (self#get_name op2)
      | _ -> err ("Unexpected expression in identifier statement found.");
    end;
    | ENV_sel (em',sel') -> (self#get_name em')
    | _ ->  err ("Unexpected identifier statement found.") 

  method private contains em em' =
    match em' with
    | ENV_op (op1,op,op2) ->
      (self#contains em op1) || (self#contains em op2)
    | _ -> em = em'   

  method private is_arg tm =
      match tm with
      | ENV_var vn ->
        (Str.string_match (Str.regexp "ARG[0-9]") vn 0)||
        (Str.string_match (Str.regexp "arg[0-9]") vn 0)
      | _ -> false 

  method private is_var tm =
      match tm with
      | ENV_var vn -> true
      | _ -> false 

  method private basename name =
      (*
      ** Remove index of name,
      ** for example SEMA_fork_0_GD => SEMA_fork_GD
      *)
      let oname' = Str.global_replace (Str.regexp "_[0-9]+") "" oname in
      let name' = Str.global_replace (Str.regexp oname) oname' name in
      name'

  method private basename_n n name =
      (*
      ** Replace index i of oname,
      ** for example SEMA_fork_0_GD => SEMA_fork_1_GD
      *)
      let oname' = Str.global_replace (Str.regexp "_[0-9]+") (sprintf "_%d" n) oname in
      let name' = Str.global_replace (Str.regexp oname) oname' name in
      name'
       

  method private get_arg_num arg =
    let err str = error 0 (sprintf "EMI <%s>#get_arg_num: %s" self#info str)  in
    match arg with
    | ENV_var argstr ->
    begin
      let len = String.length argstr in
      try
      begin
        int_of_string (List.hd (Str.split (Str.regexp "ARG") argstr))
      end
      with _ -> err (sprintf "Unexpected argument specifier <%s> in variable name found." argstr) 
    end;
    |_ -> err (sprintf "Unexpected argument specifier found.")
      
  method private is_value em =
    match em with
    | ENV_int _ | ENV_str _ | ENV_logic _ | ENV_bool _ -> true;
    | ENV_var vn ->
    begin
      let eme = self#lookup_env vn in
      self#is_value (self#env_val eme)  
    end;
    | _ -> false 
    
  method private expr_const em =
    match em with
    | ENV_sig _ -> false
    | ENV_var vn ->
      let eme = self#lookup_env vn in
      self#expr_const (self#env_val eme)
    | ENV_str _ | ENV_int _ | ENV_bool _ | ENV_logic _ -> true
    | ENV_fun (ft,args) ->
      self#func_const ft args
    | ENV_op (op1,op,op2) ->
      (self#expr_const op1) && (self#expr_const op2)
    | _ -> false
    
  method private expr_fold em =
    if (self#expr_const em) then
    begin
      match self#get_type em with
      | 'i' -> ENV_int (self#eval_expr em)
      | 's' -> ENV_str (self#eval_expr_str em)
      | 'b' -> ENV_bool (self#eval_cond em)
      | _ -> em
    end
    else
      em 
      
  method private eval_cond em =
    match em with
    | ENV_op (op1,op,op2) ->
    begin
      match op with
      | "and" -> (self#eval_cond op1) && (self#eval_cond op2)
      | "or" -> (self#eval_cond op1) || (self#eval_cond op2)
      | "=" | "/=" | "<" | "<=" | ">" | ">=" -> 
      begin
        let et1 = self#get_type op1 in
        let et2 = self#get_type op2 in
        let et = self#expr_type et1 et2 in
        if (self#expr_const op1) && (self#expr_const op2) then
        begin
          if et = 'i'  then
          begin
            let op1' = self#eval_expr op1 in
            let op2' = self#eval_expr op2 in
            match op with
            | "=" -> op1' = op2'
            | "<" -> op1' < op2'
            | ">" -> op1' > op2'
            | "<=" -> op1' <= op2'
            | ">=" -> op1' >= op2'
            | "/=" -> op1' <> op2'
            | _ -> raise Not_found
          end
          else if et = 's' then
          begin
            let op1' = self#format_expr op1 in
            let op2' = self#format_expr op2 in
            match op with
            | "=" -> op1' = op2'
            | "<" -> op1' < op2'
            | ">" -> op1' > op2'
            | "<=" -> op1' <= op2'
            | ">=" -> op1' >= op2'
            | "/=" -> op1' <> op2'
            | _ -> raise Not_found      
          end
          else if et = 'b' then
          begin
            let op1' = self#format_expr op1 in
            let op2' = self#format_expr op2 in
            match op with
            | "=" -> op1' = op2'
            | "/=" -> op1' <> op2'
            | _ -> raise Not_found      
          end
          else
            raise Not_found
        end
        else 
         raise Not_found
      end;
      | _ -> raise Not_found
    end; 
    | _ -> raise Not_found
    
  method private try_eval_cond em =
    try
    begin
      let b = self#eval_cond em in
      true,b
    end
    with Not_found -> false,false


  method private cmp_type t1 t2 =
    if t1 = t2 then true
    else 
    match (t1,t2) with
    | '$',t2 -> true
    | t1,'$' -> true
    | 'S','l' -> true
    | 'l','S' -> true
    | 'S','b' -> true
    | 'b','S' -> true
    | 'S','i' -> true
    | 'i','S' -> true
    | _ -> false
  method private expr_type t1 t2 =
    if t1 = t2 then t2
    else 
    match (t1,t2) with
    | '$',t2 -> t2
    | t1,'$' -> t1
    | 'S','l' -> t2
    | 'l','S' -> t1
    | 'S','b' -> t2
    | 'b','S' -> t1
    | 'S','i' -> t2
    | 'i','S' -> t1
    | _ -> '?'
    
  method private cur_pos p =
    if p <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos p        

  method private modu_pro p =
    Str.string_match (Str.regexp "MOD_") p 0
    

  method private set_arg_context aco =
    let err str = error 0 (sprintf "EMI<%s>#set_arg_context: %s" self#info str) in
    self#print 2 (sprintf "#set_arg_context: <%s>." aco.eac_ao.ao_name);
    (*
    ** Set actual method argument context 
    *)
    let n =  ref 0 in
    let get_val uarg =
      match uarg with
      | UA_data ud when (match ud with UC_val _ -> true | _ -> false) ->
      begin
        let v = ud_val ud in
        match v with
        | V_int i64 -> [ENV_int i64],[]
        | V_time (v,u) ->
        begin
          [ENV_int (match u with
          | Nsec -> v;
          | Usec -> Int64.mul v (Int64.of_string "1000");
          | Msec -> Int64.mul v (Int64.of_string "1000000");
          | Sec -> Int64.mul v (Int64.of_string "1000000000");
          | Cycles -> v)],[];
        end;
       | V_freq (v,u) ->
        begin
          [ENV_int (match u with
          | Hz -> v;
          | Khz -> Int64.mul v (Int64.of_string "1000");
          | Mhz -> Int64.mul v (Int64.of_string "1000000");
          | Ghz -> Int64.mul v (Int64.of_string "1000000000"))],[];
        end;
        | V_string s -> [ENV_str s],[];
        | V_char c -> [ENV_logic (sprintf "0x%x" (int_of_char c))],[];
        | V_logic s -> [ENV_logic (sprintf "0b%s" s)],[];
        | _ -> err (sprintf "Unexpected UC value found (parameter %d)" !n);
      end;
      | UA_data ud -> [ENV_sig [ENV_str (sprintf "%s" (ud_name ud))]],[ud]
      | _ -> err (sprintf "Unexpected UC argument found (parameter %d)" !n) in
      
    List.iter (fun uarg ->
      incr n;
      let arg,obj = get_val uarg in
      self#set_env [{env_name=sprintf "ARG%d" !n;
               env_val=arg;
               env_type=self#get_type (List.hd arg);
               env_range=[];
               env_obj=obj;
               env_attr=[]}];
      ) aco.eac_uargs

  (*
  ** Object parameter environment, for example $datawidth
  ** string*string list
  *)
  val mutable env: (emi_env list) =
    let clk =  ENV_sig [ENV_str "conpro_system_clk"] in
    let rst =  ENV_sig [ENV_str "conpro_system_reset"] in
    [
    {env_name="ARG1";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    {env_name="ARG2";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    {env_name="ARG3";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    {env_name="ARG4";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    {env_name="ARG5";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    {env_name="ARG6";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    {env_name="ARG7";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    {env_name="ARG8";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    {env_name="ARG9";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    {env_name="ARG9";env_val=[];env_type='$';env_range=[];env_obj=[];env_attr=[ENV_scalar]};
    ]
  method set_env new_env = 
    let err str = error 0 (sprintf "EMI<%s>#set_env: %s" self#info str) in
    let cmp_type k1 k2 = k1 = k2 in
       
    List.iter (fun eme ->
      try
      begin
        let eme' = List.find (fun eme' -> eme'.env_name = eme.env_name) env in
        match self#env_type eme' with
        | 'i' ->
        begin
          (*
          ** Check value wether in range or not
          *)
          if eme.env_name = "ARRAY" && eme.env_val = [ENV_int Int64.one] then is_array <- true;
          let checked = ref false in
          List.iter (fun em ->
            let v = self#eval_expr em in
            List.iter (fun r ->
              match r with
              | ENV_point (k,p) ->
                if not (cmp_type k 'i') then 
                  err (sprintf "Range type of parameter <%s> doesn't match parameter type, expected 'i', got '%c'." 
                               eme.env_name k); 
                let p = Int64.of_string p in
                checked := !checked or (v=p);
              | ENV_line ((k1,a),(k2,b)) -> 
                if not (cmp_type k1 'i') || not (cmp_type k2 'i') then 
                  err (sprintf "Range type of parameter <%s> doesn't match parameter type." eme.env_name); 
                let a = Int64.of_string a in
                let b = Int64.of_string b in
                checked := !checked or (v <= b && v >= a)) eme'.env_range ) eme.env_val;  
          if !checked  || eme'.env_range = [] then 
          begin
            if not (List.mem ENV_scalar eme'.env_attr) then 
            begin
              if List.mem ENV_filtered eme'.env_attr then
              begin
                List.iter (fun v ->
                  if not (List.mem v eme'.env_val) then
                    eme'.env_val <- v :: eme'.env_val;
                  ) eme.env_val;
              end
              else
                eme'.env_val <- eme.env_val @ eme'.env_val;
              if List.mem ENV_sorted eme'.env_attr then
              begin
                eme'.env_val <- (List.sort (fun a b ->
                  if a < b then (-1) else
                  if a > b then 1 else 0
                  ) eme'.env_val);
              end;              
            end
            else
              eme'.env_val <- eme.env_val;  
          end
          else
            err (sprintf "parameter <%s> value out of range." eme.env_name);
        end;
        | 's' ->
        begin
          (*
          ** Check value wether in range or not
          *)
          let checked = ref false in
          List.iter (fun em ->
            let v = self#eval_expr_str em in
            List.iter (fun r ->
              match r with
              | ENV_point (k,p) -> 
                if not (cmp_type k 's') then 
                  err (sprintf "Range type of parameter <%s> doesn't match parameter type, expected 's', got '%c'."
                               eme.env_name k); 
                checked := !checked or (v=p);
              | ENV_line _ ->
                err (sprintf "Unexpected range for parameter of type string found."); 
               ) eme'.env_range ) eme.env_val;  
          if !checked || eme'.env_range = [] then 
          begin
            if not (List.mem ENV_scalar eme'.env_attr) then 
            begin
              if List.mem ENV_filtered eme'.env_attr then
              begin
                List.iter (fun v ->
                  if not (List.mem v eme'.env_val) then
                    eme'.env_val <- v :: eme'.env_val;
                  ) eme.env_val;
              end
              else
                eme'.env_val <- eme.env_val @ eme'.env_val;
              if List.mem ENV_sorted eme'.env_attr then
              begin
                eme'.env_val <- (List.sort (fun a b ->
                  if a < b then (-1) else
                  if a > b then 1 else 0
                  ) eme'.env_val);
              end;              
            end
            else
              eme'.env_val <- eme.env_val;  
          end
          else
            err (sprintf "parameter <%s> value out of range." eme.env_name);
        end;
        | '?' -> 
          eme'.env_type <- eme.env_type;
          eme'.env_obj <- eme.env_obj;
          eme'.env_val <- eme.env_val;
        | 'U' -> 
          eme'.env_obj <- eme.env_obj;
          eme'.env_val <- eme.env_val;
        | _ -> 
          if not (List.mem ENV_scalar eme'.env_attr) then 
          begin
            
            if List.mem ENV_filtered eme'.env_attr then
            begin
              List.iter (fun v ->
                if not (List.mem v eme'.env_val) then
                  eme'.env_val <- v :: eme'.env_val;
                ) eme.env_val;
            end
            else
              eme'.env_val <- eme.env_val @ eme'.env_val;
            if List.mem ENV_sorted eme'.env_attr then
            begin
              eme'.env_val <- (List.sort (fun a b ->
                if a < b then (-1) else
                if a > b then 1 else 0
                ) eme'.env_val);
            end;              
          end
          else
          begin
            eme'.env_val <- eme.env_val;
          end;
      end
      with
        Not_found -> 
        begin
          env <- env @ [eme];      
        end;
      ) new_env
      
  (*
  ** Only temporary variables with one actual value
  *)
  method private set_env_name name v =
    match self#try_lookup_env name with
    | Some eme ->
      eme.env_val <- [v];
    | None ->
      let eme = {
        env_name = name;
        env_val = [v];
        env_range = [];
        env_type = self#get_type v;
        env_obj = [];
        env_attr=[];
      } in
      env <- env @ [eme]
      
  method private set_env_obj name o v =
    match self#try_lookup_env name with
    | Some eme ->
      eme.env_obj <- [o];
      eme.env_val <- [v];
    | None ->
      let eme = {
        env_name = name;
        env_val = [v];
        env_range = [];
        env_type = 'U';
        env_obj = [o];
        env_attr=[];
      } in
      env <- env @ [eme]
  
  method private del_env_name name =
    match name with
    | "O" | "P" | "CLK" | "RES" -> error 0 (sprintf "Can't delete variable $%s!!!" name);
    | _ -> env <- List.filter (fun eme -> eme.env_name <> name) env
        
  method get_env = env
  
  method copy_env =
    env <- List.map (fun e ->
        {e with env_name = sprintf "%s" e.env_name}
      ) env
      
      
  method print_env env =
    let rec pr tdv =
      match tdv with
      | ENV_str str -> sprintf "\"%s\"" str
      | ENV_logic str -> sprintf "%s" str
      | ENV_int n -> Int64.to_string n
      | ENV_bool b -> if true then "true" else "false"
      | ENV_sig _ -> sprintf "§%s" (self#get_sig tdv)
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
      | ENV_fun (ft,args) -> sprintf "%s()" (self#func_name ft)
      | ENV_set el -> sprintf "[%s]" (pl el) 
    and pl l  =
      match l with
      | [hd] -> pr hd;
      | hd::tl -> sprintf "%s;%s" (pr hd) (pl tl);
      | [] -> ""  in
    let first = ref true in
    let str = ref "" in
    List.iter (fun e ->
      if e.env_type = '$' then ()
      else 
      begin
        if !first then str := sprintf "%s(%c)=[%s]" e.env_name e.env_type (pl e.env_val)
        else 
          str := sprintf "%s,\n%s(%c)=[%s]" !str e.env_name e.env_type (pl e.env_val);
        first := false;
      end;
      ) env;
    !str
    
  method private lookup_env name = 
    let rec lookup_env envl name =
      match envl with
      | env :: tl ->
      begin
        if env.env_name = name then env
        else
          lookup_env tl name;  
      end;
      | _ -> 
        self#print 0 (self#print_env (env@ !top_env));
        error 0 (sprintf "EMI <%s>: parameter <$%s> not found." self#info name) in
    lookup_env (env @ !top_env) name

  method private try_lookup_env name = 
    let rec lookup_env envl name =
      match envl with
      | env :: tl ->
      begin
        if env.env_name = name then Some env
        else
          lookup_env tl name;  
      end;
      | _ -> None in
    lookup_env (env @ !top_env) name

  method private lookup_env_int name =
    match self#try_lookup_env name with
    | Some env ->
    begin
      try
        self#get_int (self#env_val env)
      with
        _ -> error 0 (sprintf "EMI <%s>#lookup_env_int: parameter <%s> with unexpected value <%s>." 
                              self#info name (self#format_expr (self#env_val env)))
    end
    | None -> 
      error 0 (sprintf "EMI <%s>#lookup_env_int: parameter <%s> not found." self#info name) 



  method private env_val eme =
    match eme.env_val with
    | hd :: _ -> hd;
    | [] -> error 0 (sprintf "Expected value not found. $%s has empty environment." eme.env_name)

  method private env_valn eme n =
    let n' = List.length eme.env_val in
    if n > n' then error 0 (sprintf "Parameter array access out of range, expected 1..%d, got %d" n' n);
    if List.mem ENV_scalar eme.env_attr then error 0 (sprintf "Array access of sclar parameter variable found.");
    List.nth  (List.rev eme.env_val) (n-1) 

  method private env_obj eme =
    match eme.env_obj with
    | ud :: _ -> 
    begin
      let pro = pro_of_ud ud in
      let vdp = vhdl_of_ud pro None ud None in
      ENV_sig [ENV_str vdp.dp_sig]   
    end;
    | [] -> error 0 (sprintf "Expected object not found. $%s has empty object environment." eme.env_name)

  method private is_obj eme =
    eme.env_obj <> []
    
  method private env_type eme =
    eme.env_type
  
  (*
  ** env_expr utils
  *)
  method  private get_str em =
    let err str = error 0 (sprintf "EMI <%s>#get_str: %s" self#info str) in
    match em with
    | ENV_str s -> s;
    | ENV_int i64 -> Int64.to_string i64
    | ENV_bool b -> if b then "true" else "false"
    | _ -> err ("Expected string value not found.")

  method  private get_int em =
    let err str = error 0 (sprintf "EMI <%s>#get_int: %s" self#info str) in
    match em with
    | ENV_str s 
    | ENV_logic s -> 
    begin
        try Int64.of_string s
        with _ -> err (sprintf "Invalid integer format <%s> found." s);
    end;
    | ENV_int i64 -> i64
    | ENV_bool b -> if b then Int64.one else Int64.zero
    | _ -> err ("Expected integer value not found.")
    
  method private get_var em =
    let err str = error 0 (sprintf "EMI <%s>#get_var: %s" self#info str) in
    match em with
    | ENV_var s -> s;
    | ENV_sel (em',_) -> self#get_var em';
    | _ -> err (sprintf "Expected variable not found.")
    
  method private get_var_sel em =
    let err str = error 0 (sprintf "EMI <%s>#get_var_sel: %s" self#info str) in
    match em with
    | ENV_var s -> s,"";
    | ENV_sel (em',sel') -> 
      self#get_var em',
      self#get_str sel';
    | _ -> err (sprintf "Expected variable not found.")

  method private get_var_sel_int em =
    let err str = error 0 (sprintf "EMI <%s>#get_var: %s" self#info str) in
    match em with
    | ENV_var s -> s,Int64.of_int (-1);
    | ENV_sel (em',sel') -> 
      self#get_var em',
      self#get_int sel';
    | _ -> err (sprintf "Expected variable not found.")
    
  method private conv_type ts tm =
    let err str = error 0 (sprintf "EMI <%s>#conv_type: %s" self#info str) in
    match tm with
    | ENV_logic s ->
    begin
      match ts with
      | "int" | "natural" -> ENV_int (Int64.of_string s)
      | "logic" -> tm
      | "bool" -> if s = "0x1" || s = "0b1" then ENV_bool true else ENV_bool false
      | _ -> err (sprintf "Can't convert value to type <%s>." ts);
    end;
    | ENV_int i64 ->
    begin
      match ts with
      | "int" | "natural" -> tm
      | "logic" -> ENV_logic (sprintf "0x%x" (Int64.to_int i64))
      | "bool" -> ENV_bool (i64 <> Int64.zero)
      | _ -> err (sprintf "Can't convert value to type <%s>." ts);
    end;
    | ENV_bool b ->
    begin
      match ts with
      | "int" | "natural" -> if b then ENV_int Int64.one else ENV_int Int64.zero
      | "logic" -> if b = true then ENV_logic "0b1" else ENV_logic "0b0" 
      | "bool" -> tm
      | _ -> err (sprintf "Can't convert value to type <%s>." ts);
    end;
    | _ -> err (sprintf "Can't convert value to type <%s>." ts)        
      
  
  (*
  ** Compiling
  *)
  method private tm_val tm =
    match tm with
    | TM_value (kind,pos,str) ->
      self#cur_pos pos;
      (match kind with
       | 'd' -> 'i';
       | 'x' | 'b' | 'B' -> 'l'
       | 'L' -> 'b'
       | _ -> kind),str;
    | TM_string (pos,str) ->
      self#cur_pos pos;
      's',str;
    | TM_var (pos,vname) -> 
      self#cur_pos pos;
      let eme = self#lookup_env vname in
      self#env_type eme, self#format_expr (self#env_val eme) 
    | _ -> error 0 (sprintf "EMI <%s>#tm_val: unexpected value." self#info) 

  method private tm_ident tm =
    match tm with
    | TM_ident (pos,str) ->
      self#cur_pos pos;
      str;
    | _ -> error 0 (sprintf "EMI <%s>#tm_val: Expected identifier not found." self#info) 
    
  method private tm_port_type pt_type =
    match pt_type with
    | TM_type ("in",TM_empty) -> Some PT_in 
    | TM_type ("out",TM_empty) -> Some PT_out 
    | TM_type ("bus",TM_empty) -> Some PT_bus 
    | TM_empty -> None
    | _ -> error 0 (sprintf "EMI <%s>#tm_port_type: unexpected argument port type specifier found." self#info) 

  method private tm_range tm =
    let t2i tm = 
      try
      begin
        let expr = self#expr_fold (self#compile_expr tm) in
        match expr with
        | ENV_int i -> i;
        | ENV_str v -> Int64.of_string v;
        | _ -> 
          error 0 (sprintf "EMI <%s>#tm_range: unexpected value." self#info);
      end
      with _ -> error 0 (sprintf "EMI <%s>#tm_range: unexpected value." self#info) in
    match tm with
    | TM_sub (_,tr) -> self#tm_range tr;
    | TM_range ('-',a,b) -> Some (t2i b,t2i a); 
    | TM_range ('+',a,b) -> Some (t2i a,t2i b); 
    | TM_range ('=',a,_) -> Some (t2i a,t2i a);  
    | TM_ident _ | TM_var _ | TM_value _ -> None;     
    | _ -> error 0 (sprintf "EMI <%s>#tm_range: unexpected statement found." self#info) 

  method private tm_param_range tm =
    let err str = error 0 (sprintf "EMI <%s>#tm_param_range: %s" self#info str)  in
    let conv_dt ts tv = 
      match tv with
      | 'l',s ->
      begin
        match ts with
        | "int" | "natural" -> 'i', (Int64.to_string (Int64.of_string s))
        | "logic" -> tv
        | "bool" -> if s = "0x1" || s = "0b1" then 'b',"true" else 'b',"false"
        | _ -> err (sprintf "Can't convert value to type <%s>." ts);
      end;
      | 'i',s ->
      begin
        match ts with
        | "int" | "natural" -> tv
        | "logic" -> 'l' ,(sprintf "0x%x" (int_of_string s))
        | "bool" -> 'b' ,(if s = "0" then "false" else "true")
        | _ -> err (sprintf "Can't convert value to type <%s>." ts);
      end;
      | _ -> err (sprintf "Can't convert value to type <%s>." ts) in      
      
      
    match tm with
    | TM_type (ts,tm') ->
    begin
      match self#tm_param_range tm' with
      | [ENV_line (ta,tb)] -> [ENV_line (conv_dt ts ta,conv_dt ts tb)]
      | [ENV_point (t)] -> [ENV_point (conv_dt ts t)]
      | tml -> List.map (fun tm ->
          match tm with
          | ENV_point t -> ENV_point (conv_dt ts t);
          | _ -> err "Invalid point list found")tml;
    end;
    | TM_sub (_,tr) -> self#tm_param_range tr;
    | TM_range ('-',a,b) -> [ENV_line (self#tm_val b,self#tm_val a)]; 
    | TM_range ('+',a,b) -> [ENV_line (self#tm_val a,self#tm_val b)]; 
    | TM_range ('=',a,_) -> [ENV_point (self#tm_val a)]; 
    | TM_list pl -> List.map (fun i -> ENV_point (self#tm_val i)) pl; 
    | TM_ident _ | TM_var _ | TM_value _ -> [];     
    | _ -> err (sprintf "unexpected statement found." ) 

  method private get_range_type r =
    let err str = error 0 (sprintf "EMI <%s>#get_range_type: %s" self#info str)  in
    match r with
    | [ENV_line ((t,v),_)] -> t
    | (ENV_point (t,v)) :: _ -> t
    |  _ -> err (sprintf "unexpected range/set found." ) 

  method private tm_env_range tm =
    let err str = error 0 (sprintf "EMI <%s>#tm_env_range: %s" self#info str)  in
    match tm with
    | TM_sub (_,tr) -> self#tm_env_range tr;
    | TM_range ('-',a,b) -> Some (ENV_line (self#tm_val b,self#tm_val a)); 
    | TM_range ('+',a,b) -> Some (ENV_line (self#tm_val a,self#tm_val b)); 
    | TM_range ('=',a,_) -> Some (ENV_point (self#tm_val a));  
    | TM_ident _ | TM_var _ | TM_value _ -> None;     
    | _ -> err (sprintf "unexpected statement found.") 

  method private tm_type_width tm =
    let err str = error 0 (sprintf "EMI <%s>#tm_type_width: %s" self#info str)  in
    match tm with
    | TM_empty -> Int64.zero
    | _ ->
      let em = self#compile_expr tm in
      self#eval_expr em

  method private tm_type tm_type =
    let i n = Int64.to_int n in
    match tm_type with
    | TM_type ("logic",tn) -> 
      DT_logic (max (i (self#tm_type_width tn)) 1) ;
    | TM_type ("int",tn) -> 
      DT_int (max (i (self#tm_type_width tn)) 1) ;
    | TM_type ("bool",tn) -> 
      DT_bool
    | TM_type ("natural",TM_empty) ->
      DT_natural 0;
    | TM_type ("string",TM_empty) ->
      DT_object "string";
    | TM_empty -> DT_logic 0;
    | TM_type ("type",tn) -> 
      DT_object (self#format_expr (self#compile_expr tn));
    | _ -> error 0 (sprintf "EMI <%s>#get_type: unexpected type specifier found." self#info) 

  method private tm_name tm =
    match tm with
    | TM_ident (pos,str) -> 
      self#cur_pos pos;
      str;
    | TM_var (pos,vname) -> 
      self#cur_pos pos;
      vname 
    | TM_string (pos,str) -> 
      self#cur_pos pos; 
      str
    | TM_sub (tn,_) -> self#tm_name tn;
    | TM_sel (tm,_) -> self#tm_name tm;
    | _ -> error 0 (sprintf "EMI <%s>#get_name: unexpected name found." self#info) 

  method private tm_port_type pt_type =
    match pt_type with
    | TM_type ("in",TM_empty) -> Some PT_in 
    | TM_type ("out",TM_empty) -> Some PT_out 
    | TM_type ("bus",TM_empty) -> Some PT_bus 
    | TM_empty -> None
    | _ -> error 0 (sprintf "EMI <%s>#get_port_type: unexpected argument port type specifier found." self#info) 
  (*
  ************************************************************
  ** #version section (optional)
  ************************************************************
  *)
  method private read_version = 
    let err str = error 0 (sprintf "EMI <%s>#read_version: %s" self#info str) in
    let tml = List.filter (fun tm -> 
      match tm with TM_version _ -> true | _ -> false ) tops in
    if tml = [] then
    begin
      info (sprintf "EMI <%s>: No #version section found." self#info);
    end
    else if (List.length tml) > 1 then
      error 0 (sprintf "EMI <%s>: More than one #version section found." self#info)
    else  match List.hd tml with
    | TM_version (pos,str) ->
      self#print 1 (sprintf "found #version %s..." str);
      version <- str;
    | _ -> err "unexpected statement in #vesion section found."
    
    
  (*
  ************************************************************
  ** #parameter section (optional)
  ************************************************************
  *)
  method private read_parameters = 
    let err str = error 0 (sprintf "EMI <%s>#read_parameters: %s" self#info str) in
    let print_env = self#print_env in
    let tml = List.filter (fun tm -> 
      match tm with TM_parameter _ -> true | _ -> false ) tops in
    if tml = [] then
    begin
      info (sprintf "EMI <%s>: No #parameter section found." self#info);
      []
    end
    else if (List.length tml) > 1 then
      err (sprintf "More than one #parameter section found.")
    else  match List.hd tml with
    | TM_parameter il ->
      self#print 1 (sprintf "compiling #parameter section...");
      ind_incr ();
      let ml = ref [] in
      let attrl = ref [] in
      List.iter (fun tm ->
        let rec iter tm =
          match tm with 
          | TM_param_def (tm',attrl') ->
            attrl := List.map (fun tm' ->
              match tm' with
              | TM_ident (pos,str) ->
              begin
                self#cur_pos pos;
                match str with
                | "scalar" -> ENV_scalar;
                | "filtered" -> ENV_filtered;
                | "sorted" -> ENV_sorted;
                | _ -> err (sprintf "Unexpected parameter attribute <%s> found." str);
              end;
              | _ -> err "Unexpected parameter attribute object found.";
              ) attrl';
            iter tm';
            attrl := [];
          | TM_assign (kind,lhs,rhs) ->
          begin
            let lhs' = self#compile_expr lhs in
            let env_name = self#get_name lhs' in
            try 
            begin
              let eme = List.find (fun eme -> eme.env_name = env_name) !ml in
              err (sprintf "Duplicated parameter <%s> defintion in #parameter section found." env_name);
            end
            with Not_found ->
            begin
              let env_range = self#tm_param_range lhs in
              let env_val = self#compile_expr rhs in
              if env_name = "NAME" then mname' <- (match env_val with ENV_str s -> s; | _ -> mname');
              let env_type = if env_range = [] then self#get_type env_val
                             else self#get_range_type env_range in
              ml := !ml @ [{
                  env_name = env_name;
                  env_val = [env_val];
                  env_type = env_type;
                  env_range = env_range;
                  env_obj = [];
                  env_attr = !attrl;
                }];  
            end;
          end;
          | TM_sub (ev,sub) ->
          begin
            let ev' = self#compile_expr ev in
            let env_name = self#get_name ev' in
            (*
            ** Parameter range specifier without default value found.
            *)
            try 
            begin
              let eme = List.find (fun eme -> eme.env_name = env_name) !ml in
              err (sprintf "duplicated void parameter <%s> defintion in #parameter section found."  env_name);
            end
            with Not_found ->
            begin
              let env_range = self#tm_param_range sub in
              ml := !ml @ [{
                  env_name = env_name;
                  env_val = [];
                  env_type = '?';
                  env_range = env_range;  
                  env_obj = [];
                  env_attr = !attrl;
                }];  

            end;
          end;
          | TM_var (pos,env_name) ->
          begin
            self#cur_pos pos;
            try 
            begin
              let eme = List.find (fun eme -> eme.env_name = env_name) !ml in
              err (sprintf "parameter <%s> exists already."  env_name);
            end
            with Not_found ->
            begin
              ml := !ml @ [{
                  env_name = env_name;
                  env_val = [];
                  env_type = '?';
                  env_range = [];
                  env_obj = [];
                  env_attr = !attrl;
                }];  

            end;
          end;
          | _ -> err "unexpected statement in #parameter section." in
        iter tm;
        ) il;
      self#print 1 (sprintf "[%s]" (print_env !ml));
      ind_decr ();
      !ml
    | _ ->
      err "unknown toplevel statement found." 
      
    
  (*
  ************************************************************
  ** #methods
  ************************************************************
  *)

  (*
  ** Available methods
  *)
  method set_methods m = methods <- m
  
  method read_methods = 
    let err str = error 0 (sprintf "EMI <%s>#read_methods: %s" self#info str) in
    let tml = List.filter (fun tm -> 
      match tm with 
      | TM_methods _ -> true
      | TM_conditional (expr,TM_methods _) ->
      begin
        try 
          self#eval_cond (self#compile_expr expr)
        with
          Not_found -> err "Invalid conditional expression found."; 
      end; 
      | _ -> false ) tops in
    if tml = [] then
      self#warn (sprintf "no #methods section found.");
       
    List.concat (List.map (fun tm ->
      match tm with
      | TM_conditional (_,TM_methods il) 
      | TM_methods il ->
        self#print 1 (sprintf "compiling #methods section [%s]..." (self#print_env (env@ !top_env)));
        ind_incr ();
        let ml = List.map (fun tm ->
          match tm with 
          | TM_func (meth,margs) ->
          begin
            let meth_name = self#tm_name meth in


            let rec get_meth_args margs =
              match margs with
              | [TM_empty] -> [];
              | marg :: tl ->
              begin
                match marg with
                | TM_arg (tm_kind,tm_type) ->
                (
                  match tm_kind with
                  | "lhs" -> arg_desc "" Arg_lhs (self#tm_type tm_type);
                  | "rhs" -> arg_desc "" Arg_rhs (self#tm_type tm_type);
                  | "lrhs" -> arg_desc "" Arg_lrhs (self#tm_type tm_type)
                  | _ -> err (sprintf "unexpected argument type specifier in method declaration.");
                ) :: (get_meth_args tl);
                | _ -> err (sprintf "unexpected argument specifier in method declaration." );
              end;
              | [] -> [] in

            self#print 0 (sprintf "added method <%s>." meth_name);
            {
                emm_name = meth_name;
                emm_args = get_meth_args margs;
                emm_procs = [];
            }   
          end;
          | _ -> err (sprintf "unexpected statement in #methods section.");
          ) il in
        ind_decr ();
        ml
      | _ ->
        err (sprintf "unknown toplevel statement found.")) tml)

  method private print_methods =
    let s = ref (sprintf "\nMethods <%s> {\n" self#info) in
    List.iter (fun emm ->
      s := sprintf "%s   %s (" !s emm.emm_name;
      let first = ref true in
      let last = ref (List.length emm.emm_args) in
      List.iter (fun ema ->
          decr last;
          if !first && !last > 0 then s := sprintf "%s%s," !s (sprint_arg_desc ema)
          else if !last = 0 then s := sprintf "%s%s" !s (sprint_arg_desc ema)
          else s := sprintf "%s,\n      %s" !s (sprint_arg_desc ema);
          first := false;
        ) emm.emm_args;
      s := sprintf "%s)\n" !s; 
      ) methods;
    sprintf "%s}\n" !s

  (*
  ** Methods used in processes
  *)
  method add_pro meth pro =
    if not (List.mem pro allprocs) then allprocs <- pro :: allprocs;
    let rec iter methods' =
      match methods' with
      | meth' :: tl -> 
        if meth = meth'.emm_name then
        begin
          if not (List.mem pro meth'.emm_procs) then
            meth'.emm_procs <- pro :: meth'.emm_procs;  
        end
        else
          iter tl;
      | [] -> error 0 (sprintf "EMI <%s>: can' find EMI object method <%s> used in process <%s>."  
                               self#info meth pro.pro_name); in
    iter methods 

  method private add_pro_array meth pro ot =
    (*
    ** Update all EMI objects of a
    ** dynamic accessed array
    *)
    match ot with
    | OT_array at ->
      let is_dyn = List.mem AT_dyn at.at_flags in
      Array.iter (fun obj ->
        match obj with
        | OT_object ao ->
          let rules = ao.ao_type.ta_rules in
          __(rules.rl_interp (sprintf "access %s %s" meth pro.pro_name));
        | _ -> ();  
        ) at.at_objs;
    | _ -> ()     
  (*
  ** Find a method descriptor
  *)
  method private find_method meth =
    try
      List.find (fun emm -> emm.emm_name = meth) methods
    with _ -> error 0 (sprintf"EMI.method <%s>: unknown object method <%s> found." self#info meth)


  (*
  ************************************************************
  ** #assert
  ************************************************************
  *)
  
  method private read_assert = 
    let err str =  error 0 (sprintf "EMI <%s>#read_assert: %s" self#info str) in 
    let tml = List.filter (fun tm -> 
      match tm with 
      | TM_check _ -> true
      | TM_conditional (expr,TM_check _) ->
      begin
        try 
          self#eval_cond (self#compile_expr expr)
        with
          Not_found -> err "Invalid conditional expression found."; 
      end; 
      | _ -> false ) tops in
    if tml = [] then
      self#warn (sprintf "no #assert section found.");
       
    List.iter (fun tm ->
      match tm with
      | TM_conditional (_,TM_check il)
      | TM_check il ->
        self#print 0 (sprintf "compiling #assert section [%s] with %d conditions..." 
                              (self#print_env (env@ !top_env))
                              (List.length il));
        ind_incr ();
        let rec eval tm = self#try_eval_cond (self#compile_expr tm) in

        List.iter (fun tt -> 
          let expr_ok,cond = eval tt in
          if not expr_ok then
            err (sprintf "Found invalid assertion expression.");
          if not cond then 
            err (
              sprintf "Assertion failed: %s"
              (
                let pos = an.a_curpos in
                let paths = ref ([""]@compiler.t_lib@compiler.t_incl@[""]) in
                let ic = ref Pervasives.stdin in
                while !paths <> []
                do
                  let file = sprintf "%s%s" (let p = List.hd !paths in if p <> "" then sprintf "%s/" p else "") pos.s_file in
                  try 
                  begin
                    ic := open_in file;
                    paths := []
                  end with
                    _ -> paths := List.tl !paths;
                done;
                if !ic = Pervasives.stdin then err (sprintf "Can't open module file <%s>" pos.s_file);
                let line = pos.s_line in 
                let s = ref "" in
                for i = 1 to line 
                do
                  s := input_line !ic;
                done;
                !s  
              )
              ) )il;
        ind_decr ();
      | _ -> err "unknown toplevel statement found.") tml


  (*
  ** Returns a set of processes (string list)
  *)
  method private foreach_set em procs =
    let err str = error 0 (sprintf "EMI <%s>#foreach_set: %s" self#info str) in
    let rec iter em =
      match em with
      | ENV_var "P" -> procs
      | ENV_sel (ENV_var "P",ENV_str methname) ->
        let procs' = ref [] in
         (List.iter (fun meth ->
                       procs' := !procs' @ (List.map (fun pro -> pro.pro_name) meth.emm_procs))
                         (List.filter (fun meth ->meth.emm_name = methname) methods));
         !procs';
      | ENV_sel (ENV_var v,ENV_str s) ->   
        err (sprintf "unexpected foreach-variable found <%s.%s>." v s);     
      | ENV_op (op1,op,op2) ->
      begin
        match op with
        | "or" -> 
          let pl1 = iter op1 in
          let pl2 = iter op2 in
          let pl = ref pl1 in
          List.iter (fun p -> if not (List.mem p !pl) then pl := !pl @ [p]) pl2;
          !pl
        | "and" -> 
          let pl1 = iter op1 in
          let pl2 = iter op2 in
          let pl = ref [] in
          List.iter (fun p -> if List.mem p pl1 then pl := !pl @ [p]) pl2;
          !pl
        | "xor" ->
          let pl1 = iter op1 in
          let pl2 = iter op2 in
          let pl = ref [] in
          List.iter (fun p -> if not (List.mem p pl1) then pl := !pl @ [p]) pl2;
          !pl     
        | _ -> err "unexpected foreach-variable set operation found.";     
      end;
      | _ -> err "unexpected foreach-statement found."
      in
    iter em 
    
        
  (*
  ******************************************************
  ** #interfaces
  ** Process port interface -- one for each process
  ******************************************************
  *) 
  val mutable interfaces:(emi_signal list) = []
  method private read_interfaces =
    let err str = error 0 (sprintf "EMI <%s>#read_interfaces: %s" self#info str) in
    let procs = ref (List.map (fun pro -> pro.pro_name) allprocs) in
    let tml = List.filter (fun tm -> 
      match tm with 
      | TM_interface _ -> true
      | TM_conditional (expr,TM_interface _) ->
      begin
        try 
          self#eval_cond (self#compile_expr expr)
        with
          Not_found -> err "Invalid conditional expression found."; 
      end; 
      | _ -> false ) tops in
    if tml = [] then
      self#warn (sprintf "no #interface section found.");
                
    let ml = ref [] in
    List.concat (List.map (fun tm ->
      match tm with
      | TM_conditional (_,TM_interface il)
      | TM_interface il ->
        self#print 1 (sprintf "compiling #interface section [%s]..." (self#print_env (env@ !top_env)));
        ind_incr ();
        if not is_local then List.iter (fun tm ->
          let rec iter substl tm =
            match tm with 
            | TM_signal (kind,sn,pt,st) ->
            begin
              let sig_name= self#compile_expr sn in
              let sig_type = self#tm_type st in
              let sig_dir = self#tm_port_type pt in

              (*
              ** Substitute object and process variables.
              ** In the case of $p, split interface signal into list of single process signals.
              *)
              ml := !ml @ [{
                  emi_sig_name = self#format_expr sig_name;
                  emi_sig_type = sig_type;
                  emi_sig_dir = sig_dir;
                  emi_procs = !procs;
                  emi_kind = kind;
                }];  
            end;
            | TM_foreach (vn,vna,mappings) ->
            begin
              let procs' = !procs in
              let vna' = self#compile_expr vna in
              procs := self#foreach_set vna' !procs;
              List.iter (iter []) mappings;
              procs := procs';
            end;
            | _ -> err "unexpected statement in #interface section."  in
          iter [] tm;
          ) il;
        ind_decr ();
        !ml    
      | _ ->
        err "unknown toplevel statement found." 
    ) tml)
    
  method private print_interfaces =
    let s = ref "" in
    List.iter (fun pro ->
      s := sprintf "%s\nProcess <%s.%s> port interface {\n" !s self#info pro.pro_name;
      let first = ref true in
      List.iter (fun emi ->
          if List.mem pro.pro_name emi.emi_procs then
          begin
            if !first then s := sprintf "%s      signal %s : %s;" !s emi.emi_sig_name (sprint_dt emi.emi_sig_type)
            else s := sprintf "%s,\n      signal %s : %s;" !s emi.emi_sig_name (sprint_dt emi.emi_sig_type);
            first := false;
          end;
        ) interfaces;
      s := sprintf "%s}\n" !s; 
      ) allprocs;
    !s
  (*
  ** Find an interface descriptor
  *)
  method private find_interface pro =
    try
      List.filter (fun emi -> List.mem pro emi.emi_procs) interfaces
    with _ -> error 0 (sprintf"EMI.interface <%s>: unknown process interface <%s> found." self#info pro)
    
  (*
  ** #mapping
  ** Process port mapping -- one for each process
  *) 
  val mutable mappings:(emi_mapping list) = []
  method private read_mappings =
    let err str = error 0 (sprintf "EMI <%s>#read_mappings: %s" self#info str) in
    let tml = List.filter (fun tm -> 
      match tm with 
      | TM_mappings _ -> true
      | TM_conditional (expr,TM_mappings _) ->
      begin
        try 
          self#eval_cond (self#compile_expr expr)
        with
          Not_found -> err "Invalid conditional expression found."; 
      end; 
      | _ -> false ) tops in
    if tml = [] then
      self#warn (sprintf "no #mapping section found.");
       
    let procs = List.map (fun pro -> pro.pro_name) allprocs in
    let pro = ref "" in
    
    let ml = ref [] in
    List.concat (List.map (fun tm ->
      match tm with
      | TM_conditional (_,TM_mappings il)
      | TM_mappings il ->
        self#print 1 (sprintf "compiling #mapping section [%s]..." (self#print_env (env@ !top_env)));
        ind_incr ();
        if not is_local then List.iter (fun tm ->
          let rec iter tm =
            match tm with 
            | TM_mapping (lhs,rhs) ->
            begin
              let lhs = self#compile_expr lhs in
              let rhs = self#compile_expr rhs in
              (*
              ** Substitute object and process variables.
              ** In the case of $p, split interface signal into list of single process signals.
              *)
              ml := !ml @ [{
                    ema_lhs = self#format_expr lhs;
                    ema_rhs = self#format_expr rhs;
                    ema_procs = [!pro];
                  }];
            end;
            | TM_foreach (vn,vna,mappings) ->
            begin
              let vn = self#compile_expr vn in
              let varname = self#get_var vn in
              let vna = self#compile_expr vna in
              let procs' = self#foreach_set vna procs in
              List.iter (fun pro' ->
                pro := pro';
                self#set_env_name varname (ENV_str pro');
                List.iter iter mappings;
                pro := "";
                self#del_env_name varname;
                ) procs';
            end;
            | _ -> err "unexpected statement in #mapping section."  in
          iter tm;
          ) il;
        ind_decr ();
        !ml    
      | _ ->
        error 0 (sprintf "EMI <%s>: unknown toplevel statement found." self#info);
    ) tml)
    
  method private print_mappings =
    let s = ref "" in
    List.iter (fun pro ->
      s := sprintf "%s\nProcess <%s.%s> port mapping {\n" !s self#info pro.pro_name;
      let first = ref true in
      List.iter (fun ema ->
          if List.mem pro.pro_name ema.ema_procs then
          begin
            if !first then s := sprintf "%s      %s => %s" !s ema.ema_lhs ema.ema_rhs
            else s := sprintf "%s,\n      %s => %s" !s ema.ema_lhs ema.ema_rhs;
            first := false;
          end;
        ) mappings;
      s := sprintf "%s}\n" !s; 
      ) allprocs;
    !s


  (*
  ** Find an mapping descriptor
  *)
  method private find_mapping pro =
    try
      List.filter (fun ema -> List.mem pro ema.ema_procs) mappings
    with _ -> error 0 (sprintf"EMI.mapping <%s>: unknown process mapping <%s> found." self#info pro)

  (*
  ******************************************************
  ** #signals
  ** Process and implementation signals 
  ******************************************************
  *) 
  val mutable signals:(emi_signal list  * emi_type list * emi_const list) = [],[],[]
  val mutable defaults: (unit -> string*string) list = []
  val mutable types: string list = []
  val mutable consts: string list = []
  
  method private read_signals =
    let err str = error 0 (sprintf "EMI <%s>#read_signals: %s" self#info str) in
    let tml = List.filter (fun tm -> 
      match tm with 
      | TM_signals _ -> true
      | TM_conditional (expr,TM_signals _) ->
      begin
        try 
          self#eval_cond (self#compile_expr expr)
        with
          Not_found -> err "Invalid conditional expression found."; 
      end; 
      | _ -> false ) tops in
    if tml = [] then
      self#warn (sprintf "no #signals section found.");
 
    let procs = List.map (fun pro -> pro.pro_name) allprocs in
     
    let sl,tl,cl = ref [],ref [], ref [] in
    List.iter (fun tm -> 
      match tm with
      | TM_conditional (_,TM_signals il)
      | TM_signals il ->
        self#print 1 (sprintf "compiling #signals section [%s]..." (self#print_env (env@ !top_env)));
        ind_incr ();
        List.iter (fun tm ->
          let rec iter tm =
            match tm with 
            | TM_signal (kind,sn,pt,st) ->
            begin
              let sig_name = self#compile_expr sn in
              let sig_type = self#tm_type st in
              let sig_dir = self#tm_port_type pt in

              (*
              ** Substitute user, object and process variables.
              ** Local objects carry no process extension in signal names! (is_local=true)
              *)
              sl := !sl @ [{
                  emi_sig_name = self#format_expr sig_name;
                  emi_sig_type = sig_type;
                  emi_sig_dir = sig_dir;
                  emi_procs = [];
                  emi_kind = kind;
                }]; 
            end;
            | TM_constant (sn,st,v) when st <> TM_empty && v <> TM_empty ->
            begin
              let sig_name = self#compile_expr sn in
              let sig_type = self#tm_type st in
              let sig_val = self#format_expr (self#compile_expr v) in

              (*
              ** Substitute user, object and process variables.
              ** Local objects carry no process extension in signal names! (is_local=true)
              *)
              consts <- consts @ [self#format_expr sig_name];
              cl := !cl @ [{
                  emi_const_name = self#format_expr sig_name;
                  emi_const_type = sig_type;
                  emi_const_val = sig_val;
                }]; 
            end;
            | TM_constant (sn,st,v) when st = TM_empty && v = TM_empty ->
            begin
              (*
              ** Import ConPro constant 
              *)
              let sig_name = self#compile_expr sn in
              let name = self#format_expr sig_name in
              let modu = 
                match my_module with
                | Some modu -> modu 
                | None -> err (sprintf "Module for import <%s> not found." name);  in
              if (sym_check_obj modu.mod_objs name) then 
              begin
                match sym_get_obj modu.mod_objs name with
                | OT_named_value (nv,v) ->
                  let dt = 
                    match v with
                    | V_int _ -> DT_natural 0
                    | V_logic s -> DT_logic (const_width v)
                    | V_char _ -> DT_natural 0
                    | _ -> err (sprintf "Constant <%s> with unsupported type found." name); in
                  (*
                  ** Substitute user, object and process variables.
                  ** Local objects carry no process extension in signal names! (is_local=true)
                  *)
                  consts <- consts @ [name];
                  cl := !cl @ [{
                     emi_const_name = name;
                     emi_const_type = dt;
                     emi_const_val = val_str dt v;
                   }]; 
                | _ -> err (sprintf "Not a constant: <%s>." name);
              end else err (sprintf "Unknown constant <%s> found." name);
            end;
            | TM_type_decl (kind,tn,tv,st) ->
            begin
              match kind with
              | 'e' when tv <> [] ->
                let type_name = self#format_expr (self#compile_expr tn) in
                let type_cont = List.map self#format_expr (List.map (fun n -> self#compile_expr n) tv) in
                types <- types @ [type_name] @ type_cont;
                tl := !tl @ [{
                  emt_type_name = type_name;
                  emt_type_type = None;
                  emt_type_range = None;
                  emt_type_cont = type_cont;
                  emt_kind = kind;
                }] 
              | 'e' when tv = [] ->
                let type_name = self#format_expr (self#compile_expr tn) in
                let modu = 
                  match my_module with
                  | Some modu -> modu 
                  | None -> err (sprintf "Module for import of <%s> not found." type_name);  in
                if (sym_check_type modu.mod_objs type_name) then 
                begin
                  match sym_get_type modu.mod_objs type_name with
                  | Type_const tc ->
                  begin
                    let consts' = List.map (fun el ->
                      match el with
                      | OT_const co -> co.co_name,co.co_init
                      | OT_named_value (nv,v) -> nv,v
                      | _ -> progerr "Type_const"
                      ) tc.tc_elems in
                    let dt = DT_natural 0 in
                    if consts' = [] then err "[]"; 
                    consts <- consts @ (List.map (fun (name,_) -> name) consts');
                    cl := !cl @ (List.map (fun (name,v) -> {
                      emi_const_name = name;
                      emi_const_type = dt;
                      emi_const_val = val_str dt v;
                      }) consts'); 
                  end;
                  | _ -> err (sprintf "Not a enumeration type: <%s>." type_name);
                end else err (sprintf "Unknown type <%s> found." type_name);
              | 'a' ->
                let type_name = self#format_expr (self#compile_expr tn) in
                let range = self#tm_range (List.hd tv) in
                let type_range =
                  match range with
                  | Some (a,b) -> Some (ENV_line (('i',Int64.to_string a),('i',Int64.to_string b)));
                  | None -> err (sprintf "no range in array <%s> type definition found." type_name) in
                  
                let type_type = Some (self#tm_type st) in 
                types <- types @ [type_name];
                tl := !tl @ [{
                  emt_type_name = type_name;
                  emt_type_type = type_type;
                  emt_type_range = type_range;
                  emt_type_cont = [];
                  emt_kind = kind;
                }] 
              | _ -> progerr "invalid type kind";
            end;
            | TM_foreach (vn,vna,mappings) ->
            begin
              let vn = self#compile_expr vn in
              let varname = self#get_var vn in
              match vna with
              | TM_expr _ ->
              begin
                let vna = self#compile_expr vna in
                let procs' = self#foreach_set vna procs in
                if is_local then 
                begin
                  self#set_env_name varname (ENV_str "");
                  List.iter iter mappings
                end
                else
                begin
                  List.iter (fun pro ->
                      self#set_env_name varname (ENV_str pro);
                      List.iter iter mappings;
                      self#del_env_name varname) (List.filter (fun p -> not (self#modu_pro p)) procs');                  
                end;
              end;
              | _ ->
              begin 
                let parname,parname_sel = self#get_var_sel (self#compile_expr vna) in
                match parname,parname_sel with
                | "P","" -> 
                  if is_local then 
                  begin
                    self#set_env_name varname (ENV_str ""); 
                    List.iter iter mappings
                  end
                  else
                  begin
                    List.iter (fun pro ->
                      self#set_env_name varname (ENV_str pro);
                      List.iter iter  mappings;
                      self#del_env_name varname) (List.filter (fun p -> not (self#modu_pro p)) procs);                  
                  end;
                | "P",methname -> 
                  let procs = ref [] in
                  List.iter (fun meth ->
                                procs := !procs @ (List.map (fun pro -> pro.pro_name) meth.emm_procs))
                                  (List.filter (fun meth ->meth.emm_name = methname) methods);
                  if is_local then 
                  begin
                    self#set_env_name varname (ENV_str "");
                    List.iter iter mappings 
                  end
                  else 
                    List.iter (fun pro ->
                      self#set_env_name varname (ENV_str pro);
                      List.iter iter mappings;
                      self#del_env_name varname) (List.filter (fun p -> not (self#modu_pro p)) !procs);
                | "_","" -> 
                  let eme = self#lookup_env parname in
                  List.iter (fun ev ->
                      self#set_env_name varname ev;
                      List.iter iter  mappings;
                      self#del_env_name varname;
                    ) (List.rev eme.env_val);
                | _ -> err "Invalid foreach expression found.";
              end;
            end;
            | _ -> err "unexpected statement in #signals section."  in
          iter tm;
          ) il;
        ind_decr ();

      | _ ->
        err "unknown toplevel statement found.";
      ) tml;
    !sl,!tl,!cl

  method private print_signals =
    let s = ref "" in
    s := sprintf "%s\nSignals <%s>{\n" !s self#info;
    let first = ref true in
    let sl,tl,cl = signals in
    List.iter (fun emi ->
          begin
            if !first then s := sprintf "%s      %s %s : %s;" 
                                        !s (if emi.emi_kind = 's' then "signal" else "variable")
                                        emi.emi_sig_name (sprint_dt emi.emi_sig_type)
            else s := sprintf "%s,\n      %s %s : %s;" 
                              !s (if emi.emi_kind = 's' then "signal" else "variable")
                              emi.emi_sig_name (sprint_dt emi.emi_sig_type);
            first := false;
          end;
        ) sl;
    s := sprintf "%s}\n" !s; 
    !s
  (*
  ** Find a signal descriptor
  *)
  method private find_signals pro =
    let sl,tl,cl = signals in
    try
      List.filter (fun emi -> List.mem pro emi.emi_procs) sl
    with _ -> error 0 (sprintf"EMI.find_signals <%s>: no signals found for process <%s>." self#info pro)

  method private find_signal em =
    let err str = error 0 (sprintf"EMI.find_signal <%s>: %s" self#info str) in 
    let rec get em =
      match em with
      | ENV_str s -> s;
      | ENV_sig _ -> self#get_sig em;
      | ENV_var vn ->
        let eme = self#lookup_env vn in
        get (self#env_val eme);
      | _ -> err (sprintf "Expected signal not found.") in
    let name = get em in
    let sl,tl,cl = signals in
    try
      List.find (fun emi -> name = emi.emi_sig_name) sl
    with 
      Not_found ->
      begin
        (*
        ** Lookup interfaces too...
        *)
        try
          List.find (fun emi -> name = emi.emi_sig_name) interfaces
        with
          Not_found ->
            self#print 0 self#print_signals;
            err (sprintf "Unknown implementation or process signal <%s> found." name) 
      end

  method private check_signal em =
    let rec get em =
      match em with
      | ENV_str s -> s;
      | ENV_sig _ -> self#get_sig em;
      | ENV_var vn ->
        let eme = self#lookup_env vn in
        get (self#env_val eme);
      | _ -> raise Exit  in
    let name = get em in
    let sl,tl,cl = signals in
    try
    begin
      __(List.find (fun emi -> name = emi.emi_sig_name) sl);
      true
    end
    with 
      _ ->
      begin
        (*
        ** Lookup interfaces too...
        *)
        try
        begin
          __(List.find (fun emi -> name = emi.emi_sig_name) interfaces);
          true
        end
        with
          _ -> false
      end
      
  method private find_constant em =
    let err str = error 0 (sprintf"EMI.find_constant <%s>: %s" self#info str) in 
    let rec get em =
      match em with
      | ENV_str s -> s;
      | ENV_sig _ -> self#get_sig em;
      | ENV_var vn ->
        let eme = self#lookup_env vn in
        get (self#env_val eme);
      | _ -> err (sprintf "Expected constant not found.") in
    let name = get em in
    let sl,tl,cl = signals in
    try
      List.find (fun emi -> name = emi.emi_const_name) cl
    with 
      Not_found ->
            err (sprintf "Unknown constant <%s> found." name) 

  method private check_constant em =
    let rec get em =
      match em with
      | ENV_str s -> s;
      | ENV_sig _ -> self#get_sig em;
      | ENV_var vn ->
        let eme = self#lookup_env vn in
        get (self#env_val eme);
      | _ -> raise Exit in
    let name = get em in
    let sl,tl,cl = signals in
    try
    begin
      __(List.find (fun emi -> name = emi.emi_const_name) cl);
      true
    end
    with 
      _ -> false

  method private find_default signame =
    let err str = error 0 (sprintf"EMI.find_default <%s>: %s" self#info str) in 
    let rec iter dl =
      match dl with
      | d :: tl ->
        let signame',def = d () in
        if signame' = signame then def
        else iter tl
      | [] -> err (sprintf "Signal <%s> not found." signame) in
    iter defaults

  (*
  **************************************************
  ** #access
  **************************************************
  *)
  
  val mutable access:(emi_access list) = []
  method set_access acc = access <- acc
  
  method  read_access =
    let rec pr tdv =
      match tdv with
      | ENV_str str -> sprintf "\"%s\"" str
      | ENV_logic str -> sprintf "0b%s" str
      | ENV_int n -> Int64.to_string n
      | ENV_bool b -> if true then "true" else "false"
      | ENV_sig _ -> sprintf "§%s" (self#get_sig tdv)
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
      | ENV_sel (tn,ts) -> sprintf "$%s[%s]" (pr tn) (pr ts)
      | ENV_fun (ft,args) -> sprintf "%s()" (self#func_name ft)
      | ENV_set el -> sprintf "[%s]" (pl el)
    and  pl l  =
      match l with
      | [hd] -> pr hd;
      | hd::tl -> sprintf "%s%s" (pr hd) (pl tl);
      | [] -> "" in  
 
    let err str = error 0 (sprintf "EMI <%s>#read_access: %s" self#info str) in
    let tml = List.filter (fun tm -> 
      match tm with 
      | TM_access _ -> true
      | TM_conditional (expr,TM_access _) ->
      begin
        try 
          self#eval_cond (self#compile_expr expr)
        with
          Not_found -> err "Invalid conditional expression found."; 
      end; 
      | _ -> false ) tops in
    if tml = [] then
      self#warn (sprintf "no #access section found.");
       

    let procs = ref (List.map (fun pro -> pro.pro_name) allprocs) in
 
    let guard_expr cl aco =
      let csl = 
        List.concat (
          List.map (fun ds ->
            List.filter (fun d ->
              match d with 
                | Data_cond_sens _ ->  true
                | _ ->  false) (ds aco)
            ) cl) in
      match csl with
      | (Data_cond_sens s) :: _ -> s
      | _ -> "" in
    let is_val ud =
      match ud with
      | UC_val _ -> true;
      | _ -> false;
      in
    let rec uo_arg arg = 
        match arg with
        | UA_data (UC_sig _)
        | UA_data (UC_val _) 
        | UA_data (UC_reg _) 
        | UA_data (UC_temp _) -> arg;
        | UA_data (UC_sel us) -> 
          let at = us.us_array in
          let is_dyn = List.mem AT_dyn at.at_flags in
          if is_dyn then
              err(sprintf "dynamic selected array object <%s> not supported." 
                               (Cp_printtypes.ui_sprint_uc (UC_sel us)));
          uo_arg (UA_data us.us_obj); 
        | UA_expr _ -> arg;        
        | _ -> err(sprintf "unexpected method argument found." );
        in

      
    let ml = ref [] in
    let rec iter tml =
      match tml with
      | TM_conditional (_,TM_access (label,il)) :: tl
      | (TM_access (label,il)) :: tl ->
        let label' = self#compile_expr label in
        let meth = self#get_name label' in
        self#print 1 (sprintf "compiling #access.%s section [%s]..." meth (self#print_env (env@ !top_env)));
        ind_incr ();
        let d = {
              emc_name = meth;
              emc_data = [];
              emc_control = [];
              emc_set = [];
          } in
        ml := !ml @ [d];
        
        List.iter (fun tm ->
          match tm with 
          | TM_data tdl ->
          begin
            List.iter (fun td ->
              match td with
              | TM_cond_assign (lhs,rhs1,cond,rhs2) ->

                let cond = self#compile_expr cond in
                  
                if cond <> (ENV_var "ACC") then
                  err "unexpected conditional assignment in #access.data section." ; 


                (*
                ** Substitute object and process variables.
                *)
                d.emc_data <- d.emc_data @ [
                  (fun  aco -> 
                    if is_array then self#set_env_name "O" (ENV_str (self#basename oname));
                    let lhs = self#compile_expr lhs in
                    let lhs_name = self#get_name lhs in
                    (*
                    ** Save defaults signal value
                    *)
                    if not (self#is_arg lhs) then
                      defaults <- defaults @ [fun () ->
                        self#format_expr lhs,
                        (
                          let rhs2' = self#format_expr (self#compile_expr rhs2) in
                          if rhs2' = "0" then default_by_dt (self#find_signal lhs).emi_sig_type else rhs2'
                        )
                      ];
                    
                    if aco <> None then self#set_arg_context (get_some aco);
                    if (self#is_arg lhs && aco <> None) then
                    begin
                      (*
                      ** Argument on LHS
                      *)
                      let rhs1 = self#compile_expr rhs1 in
                      let rhs2 = self#compile_expr rhs2 in
                      let rhs1' = self#format_expr rhs1 in
                      let rhs2' = self#format_expr rhs2 in
                      
                      let ac = get_some aco in
                      let ao = ac.eac_ao in
                      let pro = ac.eac_pro in
                      let args = ac.eac_uargs in
                      let n = self#get_arg_num lhs in
                      if n > List.length args then err (sprintf "invalid argument $ARG%d found." n);
                      let methn = List.find (fun m -> m.emm_name = meth) methods in
                      let parn = List.nth methn.emm_args (n-1) in
                      let argn = List.nth args (n-1) in
                      let dt = parn.arg_data_type in
                      let ud = 
                        match uo_arg argn with
                        | UA_data ud -> ud;
                        | _ -> err (sprintf "unexpected method argument #%d found." n) in
                      let gd_rd,gd_wr = ud_guard ud in
                      let gd_ao = guard_expr d.emc_control aco in
                      let lhs_guard = 
                        if not gd_rd && not gd_wr && gd_ao <> "" then
                          Some ("not",gd_ao)
                        else
                          None in
                      let vdl = vhdl_of_ud pro None ud lhs_guard in   
                      let rhs'' = sprintf (Obj.magic vdl.dp_conv) rhs1' in 
                      let loc = Cp_utils.is_local ud in 
                      if is_array then self#set_env_name "O" (ENV_str oname);

                      if not loc then
                        [Data_out (sprintf "%s <= %s;" vdl.dp_sig rhs'');] @
                        (List.map (fun v -> Data_out v) vdl.dp_aux) @
                        (List.map (fun v -> Data_def v) vdl.dp_aux_def) @
                        (List.map (fun sv -> Data_def sv) vdl.dp_def) @
                        (List.map (fun v -> Data_sens v) vdl.dp_aux_sen) @
                        (List.map (fun v -> Data_top v) vdl.top_expr) @
                        (List.map (fun v -> Data_top_def v) vdl.top_def) @
                        [Data_sens rhs1']
                      else
                        [Data_trans (sprintf "%s <= %s;" vdl.dp_sig rhs'');] @
                        (List.map (fun v -> Data_out v) vdl.dp_aux) @
                        (List.map (fun v -> Data_def v) vdl.dp_aux_def) @
                        (List.map (fun sv -> Data_trans_def sv) vdl.dp_def) @
                        (List.map (fun v -> Data_sens v) vdl.dp_aux_sen) @
                        (List.map (fun v -> Data_top v) vdl.top_expr) @
                        (List.map (fun v -> Data_top_def v) vdl.top_def) @
                        [Data_trans_sens rhs1']
                    end
                    else if  aco <> None then
                    begin      
                      let lhs' = self#format_expr lhs in
                      let rhs1 = self#compile_expr rhs1 in
                      let rhs2 = self#compile_expr rhs2 in
                      if (self#is_arg rhs1) then
                      begin
                        (*
                        ** Argument on RHS
                        *)
                        let rhs2' = self#format_expr rhs2 in
                        let ac = get_some aco in
                        let ao = ac.eac_ao in
                        let pro = ac.eac_pro in
                        let args = ac.eac_uargs in
                        let n = self#get_arg_num rhs1 in
                        let methn = List.find (fun m -> m.emm_name = meth) methods in
                        if n > List.length methn.emm_args then 
                          err (sprintf "invalid argument $ARG%d in method <%s> found, expected #%d." 
                                       n meth (List.length methn.emm_args));
                        if n > List.length args then 
                          err (sprintf "invalid argument $ARG%d in method <%s> found, expected #%d." 
                                       n meth (List.length args));
                        let parn = List.nth methn.emm_args (n-1) in
                        let argn = List.nth args (n-1) in
                        let dt = parn.arg_data_type in
                        let ao = ac.eac_ao in
                        let ud = 
                          match uo_arg argn with
                          | UA_data ud -> ud;
                          | _ -> err (sprintf "unexpected method argument #%d found." n) in
                        let gd_rd,gd_wr = ud_guard ud in
                        let gd_ao = guard_expr d.emc_control aco in
                        let lhs_guard = 
                          if not gd_rd && not gd_wr && gd_ao <> "" then
                            Some ("not",gd_ao)
                          else
                            None in
                        if is_array then self#set_env_name "O" (ENV_str oname);

                        let vdr = vhdl_of_ud pro None ud lhs_guard in   
                        [Data_out (sprintf "%s <= %s;" lhs' vdr.dp_sig);] @
                        (List.map (fun v -> Data_out v) vdr.dp_aux) @
                        (List.map (fun v -> Data_def v) vdr.dp_aux_def) @
                        (List.map (fun sv -> Data_sens sv) vdr.dp_sen) @
                        (List.map (fun v -> Data_sens v) vdr.dp_aux_sen) @ 
                        (List.map (fun v -> Data_top v) vdr.top_expr) @
                        (List.map (fun v -> Data_top_def v) vdr.top_def) @
                         [Data_def (lhs',(if rhs2' = "0" then default_by_dt (self#find_signal lhs).emi_sig_type else rhs2'))]
                      end
                      else if (self#is_value rhs1) then
                      begin
                        let rhs1' = self#format_expr rhs1 in
                        let rhs2' = self#format_expr rhs2 in
                        if is_array then self#set_env_name "O" (ENV_str oname);
                        [Data_out (sprintf "%s <= %s;" lhs' rhs1');
                         Data_def (lhs',(if rhs2' = "0" then to_default rhs1' else rhs2'))];
                      end
                      else
                      begin
                        let rhs1' = self#format_expr rhs1 in
                        let rhs2' = self#format_expr rhs2 in
                        if is_array then self#set_env_name "O" (ENV_str oname);
                        [Data_out (sprintf "%s <= %s;" lhs' rhs1');
                         Data_def (lhs',(if rhs2' = "0" then default_by_dt (self#find_signal lhs).emi_sig_type else rhs2'))];
                      end;
                    end
                    else
                    begin
                      let lhs' = self#format_expr lhs in                       
                      if is_array then self#set_env_name "O" (ENV_str oname);
                      [Data_out (sprintf "%s <= ?;" lhs')]
                    end;
                  )];
              | TM_empty -> 
                d.emc_data <- d.emc_data @ [
                  (fun _  -> [Data_out "null;"]);
                  ];
              | _ -> err "unexpected statement in #access.data section.";
              ) tdl;            
          end;
          | TM_control tdl ->
          begin
            List.iter (fun td ->
              match td with
              | TM_wait ("for",expr) ->

                
                (*
                ** Substitute object and process variables.
                *)
                
                d.emc_control <- d.emc_control @ [
                  (fun aco -> 
                      if is_array then self#set_env_name "O" (ENV_str (self#basename oname));
                      let expr = self#compile_expr expr in
                      let expr' = self#format_expr expr in
                      let objs' = self#get_objs expr in
                      if is_array then self#set_env_name "O" (ENV_str oname);
                      [Data_cond (sprintf "not(%s)" expr')]@
                      (List.map (fun name -> Data_cond_sens name) objs'));
                    ];
              | TM_empty ->  ();  (* state <= next_state! *)
              | _ -> err "unexpected statement in #access.control section.";
              ) tdl;            
          end;
         | TM_set tdl ->
          begin
            List.iter (fun td ->
              match td with
              | TM_assign (kind,lhs,rhs) ->
                let lhs = self#compile_ident lhs in
                let rhs = self#compile_expr rhs in
                let lhs_name = self#get_var lhs in
                let rhs_name = self#get_var rhs in
                (*
                ** Check if parameter is defined already in environment
                *)
                __(self#lookup_env lhs_name);
                if self#is_var rhs then __(self#lookup_env rhs_name);
                 
                d.emc_set <- d.emc_set @ [
                  (fun  aco -> 
                    if aco <> None then self#set_arg_context (get_some aco);
                    lhs,rhs)];
              | TM_func (fname,vargs) ->
              begin
                match self#tm_name fname with
                | "print" -> 
                  d.emc_set <- d.emc_set @ [
                    (fun  aco -> 
                      match aco with
                      | Some ac ->
                        self#set_arg_context ac;
                        let args = List.map (fun arg -> self#compile_expr arg) vargs in
                        self#print 0 (self#eval_expr_str (ENV_fun (F_PRINT,args)));
                        ENV_str "",ENV_str ""
                      | None ->
                        self#print 0 "PRINT";
                        ENV_str "",ENV_str ""
                    )];
                | fname -> err (sprintf "unexpected core function <%s> in set statement found." fname);
              end;
              | _ -> err "unexpected parameter set statement.";
                    
              ) tdl;            
          end;
          | _ -> err "unexpected statement in #access section.";
          ) il;
        ind_decr ();
        iter tl;
      
      | [] -> ();    
      | _ ->
        err "unknown toplevel statement found."  in
    iter tml;
    !ml


  method private print_access =
    let s = ref "" in
    List.iter (fun emc ->
      s := sprintf "%s\nMethod <%s.%s> access\n" !s self#info emc.emc_name;
      if emc.emc_data <> [] then
      begin
        s := sprintf "%s  data=[\n" !s;
        let first = ref true in
        List.iter (fun sdf ->
          let sdl = sdf None in
          List.iter (fun sd ->
            if !first then s := sprintf "%s      %s" !s (st_sprint_synth_data sd)
            else s := sprintf "%s,\n      %s" !s (st_sprint_synth_data sd);
            first := false;
            ) sdl;
          ) emc.emc_data;
        List.iter (fun sdf ->
          let sdl = sdf None in
          List.iter (fun sd ->
            if !first then s := sprintf "%s      %s" !s (st_sprint_synth_data sd)
            else s := sprintf "%s,\n      %s" !s (st_sprint_synth_data sd);
            first := false;
            ) sdl;
          ) emc.emc_control;
        s := sprintf "%s  ]\n" !s;
      end;
      if emc.emc_set <> [] then
      begin
        s := sprintf "%s  set=[\n" !s;
        List.iter (fun sdf ->
          let sn,sv = sdf None in 
          if (sn,sv) <> (ENV_str "",ENV_str "") then          
            s := sprintf "%s    %s <= %s\n" !s (self#format_expr sn) (self#format_expr sv);
          ) emc.emc_set;
        s := sprintf "%s  ]\n" !s;
      end;
      ) access;
    !s               


  (*
  ** Find an access descriptor
  *)
  method private find_access meth =
    try
      List.find (fun emc -> emc.emc_name = meth) access
    with _ -> error 0 (sprintf"EMI.access <%s>: unknown method call <%s> found." self#info meth)



  (*
  **************************************************
  ** #process
  **************************************************
  *)
  
  val mutable process:(emi_process list) = []
  method private set_processes prol = process <- prol
  
  val mutable shared_vars = []
  
  method private compile_vhdl name tml =
    let err str = error 0 (sprintf "EMI <%s>#compile_vhdl: %s" self#info str) in
    let stm_name tm =
      match tm with
      | TM_if _ -> "if";
      | _ -> "" in
      
    let procs = List.map (fun pro -> pro.pro_name) allprocs in
    let p = {
        emp_name = name;
        emp_code = [];
        emp_sens = [];
        emp_decl = [];
      } in
    let vars = ref [] in
    
    let add_sens expr =
      let sl = self#get_all_objs expr in
      List.iter (fun s -> 
        if not (List.mem s p.emp_sens) && 
           not (List.mem s !vars) && 
           not (List.mem s types) && 
           not (List.mem s consts) && 
           not (List.mem s shared_vars) then 
          p.emp_sens <- p.emp_sens @ [s]) sl in
    let decl = ref [] in
    
    let rec stat_expr em =
      match em with
      | ENV_op (op1,op,op2) ->
      begin
        stat_expr op1;
        stat_expr op2;
        match op with
        | "+" -> stat "arithmetic unit" "adder";
        | "-" -> stat "arithmetic unit" "subtractor";
        | "*" -> stat "arithmetic unit" "multiplier";
        | _ -> ();       
      end; 
      | _ -> () in
      
    let rec stm in_seq ind tm =
      let align str = sprintf "%s%s" (String.make ind ' ') str in

      match tm with
      | TM_if (expr,bl1,bl2) ->
      begin
        let expr' = 
          match expr with
          | TM_empty -> ENV_str "others" 
          | _ -> self#compile_expr expr in
        stat_expr expr';
        let is_const,not_skip = self#try_eval_cond expr' in
        if not is_const then
        begin
          add_sens expr';
          let expr'' = self#format_expr expr' in
          if in_seq = 0 then
          begin
            [align (sprintf "if %s then" expr'');]@
            (List.concat (List.map (stm in_seq (ind+2)) bl1)) @
            (if bl2 = [] then
               [align "end if;"]
             else 
               [align "else"]@
               (List.concat (List.map (stm in_seq (ind+2)) bl2)) @
               [align "end if;"];
            )    
          end
          else
          begin
            let expr'' = self#format_expr expr' in
            [align (sprintf "%s %s %s" 
                           (if expr' = (ENV_str "others") then "else" else if in_seq=1 then "if" else "elsif") 
                           (if expr' <> (ENV_str "others") then expr'' else "")
                           (if expr' <> (ENV_str "others") then "then" else ""));]@
            (List.concat (List.map (stm 0 (ind+2)) bl1)) @
            (if bl2 = [] then
               []
             else 
               [
                 align "else"
               ]@
               (List.concat (List.map (stm 0 (ind+2)) bl2));
            )                  
          end;
        end
        else if is_const && not_skip then
        begin
          List.concat (List.map (stm in_seq ind) bl1)
        end 
        else if is_const && not not_skip && bl2 <> [] then
        begin
          List.concat (List.map (stm in_seq ind) bl2)
        end
        else
          []
      end;
      | TM_case (expr,wl) ->
      begin
        let expr' = self#compile_expr expr in
        stat_expr expr';
        add_sens expr';
        let expr'' = self#format_expr expr' in
        [align (sprintf "case %s is" expr'');]@
        (List.concat (List.map (stm in_seq (ind+2)) wl)) @
        [align "end case;"];
      end;
      | TM_when (expr,il) ->
      begin
        match expr with
        | TM_value ('O',pos,"others") ->
          self#cur_pos pos;
          [align (sprintf "when others =>");]@
          (List.concat (List.map (stm in_seq (ind+2)) il)) 
        | _ ->
          let expr' = self#compile_expr expr in
          add_sens expr';
          let expr'' = Str.global_replace (Str.regexp " or ") " | " (self#format_expr expr') in
          [align (sprintf "when %s =>" expr'');]@
          (List.concat (List.map (stm in_seq (ind+2)) il)) 
      end;
      | TM_assign (kind,lhs,rhs) ->
        let lhs' = self#compile_expr lhs in
        let rhs' = self#compile_expr rhs in
        stat_expr rhs';
        add_sens rhs';
        let lhs'' = self#format_expr lhs' in
        let rhs'' = self#format_expr rhs' in 

        [
          align (sprintf "%s %s %s;" lhs'' (if kind = 's' then "<=" else ":=") rhs'');
        ] 
      | TM_cond_assign (lhs,rhs1,cond,rhs2) ->
        let lhs' = self#compile_expr lhs in
        let rhs1' = self#compile_expr rhs1 in
        let rhs2' = self#compile_expr rhs2 in
        let cond' = self#compile_expr cond in
        stat_expr rhs1';
        stat_expr rhs2';
        stat_expr cond';
        add_sens rhs1';
        add_sens rhs2';
        add_sens cond';
        let lhs'' = self#format_expr lhs' in
        let rhs1'' = self#format_expr rhs1' in 
        let rhs2'' = self#format_expr rhs2' in 
        let cond'' = self#format_expr cond' in 
        [
          align (sprintf "%s <= %s when %s else %s;" lhs'' rhs1'' cond'' rhs2'');
        ] 
      | TM_seq tml' when tml' <> [] ->
        let n = ref 0 in
        (List.concat (List.map (fun tm' -> 
          match tm' with
          | TM_if _ | TM_foreach _ | TM_seq _ ->
            incr n; stm !n ind tm'
          | _ -> err "Unexpected statement in sequence found!") tml'))@
        [align "end if;"];
      | TM_for (i,range,il) ->
      begin
        let i' = self#compile_expr i in
        let i'' = self#format_expr i' in
        let range' = self#compile_expr range in
        let range'' = self#format_expr range' in
        [
          align (sprintf "for %s in %s loop" i'' range'');
        ] @
        (List.concat (List.map (stm in_seq (ind+2)) il)) @
        [
          align "end loop;";
        ]
      end;
      | TM_foreach (vn,vna,tml) ->
      begin
        let vn' = self#compile_expr vn in
        let varname = self#get_var vn' in
        let vna' = self#compile_expr vna in
        match vna' with
        | ENV_var "P" ->
          let n = ref 0 in
          List.concat (
                List.map (fun pro ->
                  self#set_env_name varname (ENV_str pro);
                  let l = List.concat (
                    List.map (fun tm -> 
                        if in_seq > 0 && 
                           (match tm with TM_if _ | TM_seq _ | TM_foreach _ -> false | _ -> true)
                           then err "Unexpected statement in sequence found!"; 
                        let l = stm (if in_seq > 0 then (in_seq+ !n) else 0) ind tm in
                        incr n;
                        l
                      ) tml) in
                   self#del_env_name varname;
                   l
                ) (List.filter (fun p -> not (self#modu_pro p)) procs));
        | ENV_var vn ->
        begin
          let eme = self#lookup_env vn in
          let n = ref 0 in
          match eme.env_obj with
          | obj :: tl ->
            List.concat (
              List.map (fun (v,o) ->
                self#set_env_obj varname o v; 
                let l = List.concat (
                  List.map (fun tm ->
                    if in_seq > 0 && 
                       (match tm with TM_if _ | TM_seq _ | TM_foreach _ -> false | _ -> true)
                       then err "Unexpected statement in sequence found!"; 
                    let l = stm (if in_seq > 0 then (in_seq+ !n) else 0) ind tm in
                    incr n;
                    l) 
                    tml) in
                self#del_env_name varname;
                l
              ) (if eme.env_type <> '?' then 
                  List.rev (List.map2 (fun a b -> a,b) eme.env_val eme.env_obj) else []));
          | [] ->
            List.concat (
              List.map (fun v ->
                self#set_env_name varname v; 
                let l = List.concat (
                  List.map (fun tm ->
                    if in_seq > 0 && 
                       (match tm with TM_if _ | TM_seq _ | TM_foreach _ -> false | _ -> true)
                       then err "Unexpected statement in sequence found!"; 
                    let l = stm (if in_seq > 0 then (in_seq+ !n) else 0) ind tm in
                    incr n;
                    l) 
                    tml) in
                self#del_env_name varname;
                l
              ) (if eme.env_type <> '?' then List.rev eme.env_val else []));
        end;
        | ENV_sel (ENV_var "P",ENV_str asel) ->
    
          let procs = ref [] in
          List.iter (fun meth ->
                          procs := !procs @ (List.map (fun pro -> pro.pro_name) meth.emm_procs))
                            (List.filter (fun meth ->meth.emm_name = asel) methods);
          let n = ref 0 in
          List.concat (
              List.map (fun pro ->
                self#set_env_name varname (ENV_str pro);
                let l = List.concat (
                    List.map (fun tm ->
                      if in_seq > 0 && 
                         (match tm with TM_if _ | TM_seq _ | TM_foreach _ -> false | _ -> true)
                         then err "Unexpected statement in sequence found!"; 
                      let l = stm (if in_seq > 0 then (in_seq+ !n) else 0) ind tm in
                      incr n;
                      l)
                  tml) in
                self#del_env_name varname;
                l
                ) (List.filter (fun p -> not (self#modu_pro p)) !procs));
        | ENV_op o -> 
          let procs = List.map (fun pro -> pro.pro_name) allprocs in
          let procs' = self#foreach_set vna' procs in
          let n = ref 0 in
          List.concat (
              List.map (fun pro ->
                self#set_env_name varname (ENV_str pro);
                let l = List.concat (
                  List.map (
                      if in_seq > 0 && 
                         (match tm with TM_if _ | TM_seq _ | TM_foreach _ -> false | _ -> true)
                         then err "Unexpected statement in sequence found!"; 
                      let l = stm (if in_seq > 0 then (in_seq+ !n) else 0) ind in
                      incr n;
                      l
                    ) tml) in
                 self#del_env_name varname;
                 l
              ) (List.filter (fun p -> not (self#modu_pro p)) procs'));


        | _ -> err "Invalid foreach expression found.";
      end;
      | TM_dowith (expr,tml) ->
      begin
        match expr with
        | TM_expr ("=",TM_var (src,vn),op2) ->
          let op2 = self#expr_fold (self#compile_expr op2) in
          self#set_env_name vn op2;
          let tml' = List.concat (List.map (stm in_seq ind) tml) in
          self#del_env_name vn;
          tml'
        | _ -> err "Invalid dowith expression found.";
      end;
      | TM_signal (kind,sn,pt,st) ->
       begin
         let sn' = self#compile_expr sn in
         let sig_name = self#get_name sn' in
         let sig_type = self#tm_type st in
         let sig_dir = self#tm_port_type pt in

         (*
         ** Substitute object and process variables.
         ** In the case of $p, split interface signal into list of single process signals.
         *)
         decl := !decl @ [
                sprintf "%s %s: %s;"
                        (if kind = 's' then "signal" else if kind = 'v' then "variable" else "shared variable")
                        (sig_name)
                        (match sig_type with
                        | DT_object o -> o;
                        | _ -> (obj_decl_type sig_type))
            ];
          if kind = 'v' then vars := !vars @ [sig_name];
          if kind = 'V' then shared_vars <- shared_vars @ [sig_name];
          []
       end;
      | TM_empty -> [align "null;"];
      | _ -> [];
      in
    p.emp_code <-  (List.concat (List.map (stm 0 0) tml));  
    p.emp_decl <- !decl;
    p

  method private read_process =
    let err str = error 0 (sprintf "EMI <%s>#read_process: %s" self#info str) in
    let tml = List.filter (fun tm -> 
      match tm with 
      | TM_process _ -> true
      | TM_conditional (expr,TM_process _) ->
        self#eval_cond (self#compile_expr expr);  
      | _ -> false ) tops in
    if tml = [] then
      self#warn (sprintf "no #process section found.");
    let ml = ref [] in
    
    let rec iter tml =
      match tml with
      | (TM_conditional (_,TM_process (label,il))) :: tl
      | (TM_process (label,il)) :: tl ->
        let pro_name = self#get_name (self#compile_expr label) in
        self#print 0 (sprintf "compiling process <%s> ..." pro_name);
        ind_incr ();
        ml := !ml @ [self#compile_vhdl pro_name il];
        ind_decr ();
        iter tl;
      
      | [] -> ();    
      | _ ->
        err "unknown toplevel statement found." in
    iter tml;
    !ml 
    
  val mutable tops' = []
  method private set_top top = tops' <- top
  method private read_top = 
    let tml = List.filter (fun tm -> 
      match tm with 
      | TM_top _ -> true 
      | TM_conditional (expr,TM_top _) ->
        self#eval_cond (self#compile_expr expr);  
      | _ -> false ) tops in
    let ml = ref [] in
    
    let rec iter tml =
      match tml with
      | (TM_conditional (_,TM_top il)) :: tl 
      | (TM_top il) :: tl ->
        let pro_name = "" in
        self#print 1 (sprintf "compiling #top section [%s]..." (self#print_env (env@ !top_env)));
        ind_incr ();
        ml := !ml @ [self#compile_vhdl pro_name il];
        ind_decr ();
        iter tl;
      
      | [] -> ();    
      | _ ->
        error 0 (sprintf "EMI <%s>: unknown toplevel statement found." self#info) in
    iter tml;
    !ml 
    
  
  (*
  ** Required at object instantiation
  *)
  val mutable initialized = false
  method init =
    if not initialized then
    begin
      self#print 0 (sprintf "#init: Initializing module ..." );
      self#read_version;
      let defs = self#read_parameters in
      self#set_env defs;
      initialized <- true;
    end
     
    
  (*
  ** Required before synthesis and after analysis
  *)
  val mutable compiled = false
  method private compile =
    if not compiled then
    begin
      self#print 0 ("compiling external module interface...");
      self#set_env ([{
        env_name="P";
        env_val=List.map (fun pro -> ENV_str pro.pro_name) allprocs;
        env_range=[];
        env_type='s';
        env_obj=[];
        env_attr=[]}]@
        (List.map (fun meth ->
          {
            env_name=sprintf "P.%s" meth.emm_name;
            env_val=List.map (fun pro -> ENV_str pro.pro_name) meth.emm_procs;
            env_range=[];
            env_type='s';
            env_obj=[];
            env_attr=[];
          }) methods);
        );
        
        
      let clk =  ENV_op (ENV_sel (ENV_sig [ENV_str "conpro_system_clk"],ENV_str "event"),
                        "and",
                        (ENV_op (ENV_sig [ENV_str "conpro_system_clk"],
                                 "=",
                                ENV_logic (sprintf "0b%d" (get_env_int "clock_level"))))) in
      let rst =  ENV_op (ENV_sig [ENV_str "conpro_system_reset"], 
                        "=", 
                        ENV_logic (sprintf "0b%d" (get_env_int "reset_level"))) in

      self#set_env_name "CLK" clk;
      self#set_env_name "RES" rst;
      self#set_env_name "ACC" (ENV_bool true);
      
      let interf = self#read_interfaces in
      interfaces <- interf;
      let maps = self#read_mappings in
      mappings <- maps;
      let sigs = self#read_signals in
      signals <- sigs;
      
      self#print 0 (sprintf "Environment is:\n\n%s" (ibox 2 (self#print_env (env@ !top_env))));
      
      compiled <- true;
      if verbose_level = 2 then
      begin
        self#print 2 self#print_methods;
        self#print 2 self#print_interfaces;
        self#print 2 self#print_mappings;
        self#print 2 self#print_access;
        self#print 2 self#print_signals;
      end;
    end
     
  method private get_methods = methods
  
  method private get_type em =
    let err str = error 0 (sprintf "EMI <%s>#get_type: %s" self#info str) in
    match em with
    | ENV_sig _ -> 'S'
    | ENV_int _ -> 'i'
    | ENV_logic _ -> 'l'
    | ENV_str _ -> 's'
    | ENV_bool _ -> 'b'
    | ENV_var s -> 
      let eme = self#lookup_env s in
      eme.env_type
    | ENV_op (op1,op,op2) ->
    begin
      match op with
      | "<" | ">" | "=" | "/=" | "<=" | ">=" -> 'b'
      | "+.s" -> 's'
      | _ -> 
        let t1 = self#get_type op1 in
        let t2 = self#get_type op2 in
        let et = self#expr_type t1 t2 in
        if et = '?' then
          err (sprintf "Incompatible types in expression found ['%c','%s','%c']" t1 op t2);
        et
    end;
    | ENV_fun (ft,args) ->
    begin
      let rt,atl = self#func_type ft in
      match rt with
      | '1' .. '9' ->
        let n = (int_of_char rt)-49 in
        self#get_type (List.nth args n)
      | _ -> rt
    end;
    | ENV_sel (em',_) -> self#get_type em'
    | _ -> '?'
 
  method private get_sig em =
    let err str = error 0 (sprintf "EMI <%s>#get_sig: %s" self#info str) in
    match em with
    | ENV_sig el -> 
      let rec pl_ l  =
        match l with
        | [hd] -> (self#get_sig hd);
        | hd::tl -> sprintf "%s_%s" (self#get_sig hd) (pl_ tl);
        | [] -> "" in  
      pl_ el
    | ENV_str s -> s
    | ENV_var vn ->
      let eme = self#lookup_env vn in
      self#get_sig (self#env_val eme)
    | _ -> err (sprintf "Unexpected object in signal expression found.")
    
  (*
  ******************************************************** 
  ** Compile
  ********************************************************
  *)
          
  method private compile_ident tm =
    let err str = error 0 (sprintf "EMI <%s>#compile_ident: %s" self#info str) in
    let conv_dt ts tm = self#conv_type ts tm in
    match tm with
    | TM_string (pos,str) ->
      self#cur_pos pos;
      ENV_str str    
    | TM_value ('d',pos,str) -> 
      self#cur_pos pos;
      ENV_int (try Int64.of_string str with _ -> err (sprintf "Unexpected integer value <%s> found." str));      
    | TM_value ('x',pos,str) -> 
      self#cur_pos pos;
      ENV_logic str;      
    | TM_value ('b',pos,str) -> 
      self#cur_pos pos;
      ENV_logic str;      
    | TM_value ('B',pos,str) -> 
      self#cur_pos pos;
      ENV_logic (sprintf "0b%s" (String.sub str 1 ((String.length str)-2)));      
    | TM_value ('o',pos,str) -> 
      self#cur_pos pos;
      ENV_int (try Int64.of_string str with _ -> err (sprintf "Unexpected integer value <%s> found." str));      
    | TM_value ('L',pos,str) -> 
      self#cur_pos pos;
      ENV_bool (str = "true");      
    | TM_value ('O',pos,str) -> 
      self#cur_pos pos;
      ENV_logic (sprintf "(others => %s)" str);      
    | TM_ident (pos,str) -> 
      self#cur_pos pos;
      ENV_sig [ENV_str str];  (* signal or variable *)
    | TM_var (pos,str) ->
    begin
      self#cur_pos pos;
      match Str.split (Str.regexp "\.") str with
      | [v] ->
        ENV_var v;
      | [v;s] ->
        ENV_sel (ENV_var v,ENV_str s)
      | _ -> err (sprintf "Unexpected variable <%s> found." str)
    end;
    | TM_sub (tm',tm'') ->
      ENV_sel (self#compile_expr tm',
               self#compile_expr tm'')
    | TM_type (ts,tm') ->
    begin
      match self#compile_ident tm' with
      | ENV_sel (tm',ENV_range(dir,ta,tb)) -> ENV_sel (tm',ENV_range(dir,conv_dt ts ta,conv_dt ts tb))
      | ENV_sel (tm',ENV_set tml) -> ENV_sel (tm',ENV_set (List.map (conv_dt ts) tml))
      | _ -> err "Unsupported type casting found.";       
    end;
    | TM_sel (tm',tm'') ->
      ENV_sel (self#compile_expr tm',
               self#compile_expr tm'')
    | TM_range (dir,ta,tb) ->
      ENV_range (dir,
                 self#compile_expr ta,
                 self#compile_expr tb);
    | TM_list tml ->
      ENV_set (List.map self#compile_ident tml)
    | TM_expr (op,op1,op2) when op = "_" ->
      (*
      ** Identifier containing variables
      *)
      let rec map tm =
        match tm with
        | TM_ident (pos,str) -> 
          self#cur_pos pos; 
          [ENV_str str];
        | TM_var (pos,str) ->
          self#cur_pos pos;
          [ENV_var str];
        | TM_expr (op,op1,op2) when op = "_" ->
          (map op1)@(map op2)
        | _ -> err ("Unexpected object in signal identifier found.") in
        
      ENV_sig ((map op1)@(map op2))
    | _ -> err (sprintf "Unexpected identifier statement found.")   

  method private compile_expr tm =
    let err str = error 0 (sprintf "EMI <%s>#compile_expr: %s" self#info str) in
    match tm with
    | TM_expr (op,op1,op2) when (op1 = TM_empty) ->
      (*
      ** Unary operator
      *)
      let op2' = self#compile_expr op2 in
      let t2 = self#get_type op2' in
      (*
      ** Convert strings to binary logic values in signal expressions.
      *)
      if t2 = 's' then
      begin
        let op2' =
          match op2' with
          | ENV_str s -> ENV_logic (sprintf "0b%s" s)
          | _ ->  err (sprintf "Expected logic value (operand 2) not found in signal expression.") in
        ENV_op (op2',op,op2')
      end
      else
      begin
        ENV_op (op2',op,op2')        
      end;  
        
    | TM_expr (op,op1,op2) when (op <> "_") ->
      let op1' = self#compile_expr op1 in
      let op2' = self#compile_expr op2 in
      let t1 = self#get_type op1' in
      let t2 = self#get_type op2' in
      (*
      ** Convert strings to binary logic values in signal expressions.
      *)
      if t1 = 'S' && t2 = 's' then
      begin
        let op2' =
          match op2' with
          | ENV_str s -> ENV_logic (sprintf "0b%s" s)
          | _ ->  err (sprintf "Expected logic value (operand 2) not found in signal expression.") in
        ENV_op (op1',op,op2')
      end
      else if t2 = 'S' && t1 = 's' then
      begin
        let op1' =
          match op1' with
          | ENV_str s -> ENV_logic (sprintf "0b%s" s)
          | _ ->  err (sprintf "Expected logic value (operand 1) not found in signal expression.") in
        ENV_op (op1',op,op2')
      end
      else
      begin
        let et =       
          if self#cmp_type t1 t2 then
            self#expr_type t1 t2
          else
           err (sprintf "Different types in expression found ['%c','%s','%c']" t1 op t2) in
        
        ENV_op (op1',op,op2')
      end;
    | TM_func (fn,args) ->
      let fname = self#tm_ident fn in
      let ft,rt,atl = self#func_map fname in 
      let an = List.length args in
      let atl = 
        let n = List.length atl in
        if n > 9 && n > an then 
        (
          let i = ref 0 in
          List.filter (fun at -> incr i; !i <= an) atl
        ) 
        else atl in
        
       if (List.length args) <> (List.length atl) then
         err (sprintf "Function <%s>: Expected %d function argument(s), but got %d." fname (List.length atl) (List.length args));
       let tal = ref [] in
       let argn = ref 0 in
       ENV_fun (ft,List.map2 (fun at arg ->
                incr argn;
                let arg' = self#compile_expr arg in
                match at with
                | '1' .. '9' ->
                  let ta = 
                    if self#is_arg arg' then
                      at 
                    else
                      self#get_type arg' in
                  tal := !tal @ [ta];
                  let n = (int_of_char at)-49 in 
                  if (List.nth !tal n) = ta || (self#is_arg arg')  then arg'
                  else
                  begin
                    self#print 0 (self#print_env env);
                    err (sprintf "Function <%s>: Expect argument #%d with referenced type '%c', but got '%c'." fname !argn (List.nth !tal n) ta);
                  end;
                | '$' ->
                begin
                  match arg' with
                  | ENV_sel (ENV_var vn,_) 
                  | ENV_var vn -> 
                    let ta =
                      match self#try_lookup_env vn with
                      | Some eme -> eme.env_type;
                      | None -> '$' in
                    tal := !tal @ [ta];
                    arg';
                  | _ -> 
                    let ta = 
                      if self#is_arg arg' then
                        at 
                      else
                        self#get_type arg' in
                    tal := !tal @ [ta];
                    if ta = at then arg'
                    else
                      err (sprintf "Function <%s>: Expect argument #%d with variable type, but got '%c'." fname !argn ta);
                end;
                | '*' -> arg'
                | _ ->
                  let ta = 
                    if self#is_arg arg' then
                      at 
                    else
                      self#get_type arg' in
                  tal := !tal @ [ta];
                  if self#cmp_type at ta then arg'
                  else
                    err (sprintf "Function <%s>: Expect argument #%d with type '%c', but got '%c'." fname !argn  at ta);
              ) atl args);
    
    | _ -> self#compile_ident tm
    

  (*
  ********************************************************
  ** Analysis and Synthesis entry point functions - API
  ********************************************************
  *)
  
  
  (*
  ** Return true if object is handled by this rule module.
  *)
  method my sym =
    match sym with
    | Sym_obj obj ->
    begin
     match obj with
     | OT_object ao -> ao.ao_type.ta_name = (of_mod mname) 
     | _ -> false;
    end;
    | _ -> false
    
  (*
  ** Object port declaration in entity header.
  ** Return port declaration (in VHDL entity header) 
  **  [Module or Process level]
  **
  ** Returns: port list * aux (toplevel) list
  *)
  method obj_port sym modu proo =
    let is_mon,is_debug,sym = get_mon sym in
    
    match sym with
    | Sym_obj obj ->
    begin
      match obj with
      | OT_object ao ->
      begin
        match proo with
        | Some pro -> 
          debug "emi" with (sprintf "%s: obj_port: process <%s>" self#info pro.pro_name);
          let ports,aux = ref [], ref [] in
          List.iter (fun emi ->
            if List.mem pro.pro_name emi.emi_procs then
            begin
              ports := !ports @ [
                  sprintf "signal %s: %s %s;"
                          emi.emi_sig_name
                          (match emi.emi_sig_dir with
                            | Some PT_in -> "in"
                            | Some PT_out -> "out"
                            | Some PT_bus -> "inout"
                            | _ -> "" )
                          (obj_decl_type emi.emi_sig_type)
                ];
            end;
            ) interfaces;
          !ports,!aux
        | None -> [],[]
      end;
      | OT_array at ->
      begin
        let is_dyn = List.mem AT_dyn at.at_flags in
        match proo with
        | Some pro -> 
          debug "emi" with (sprintf "%s:obj_port: process <%s>, array, is_dyn=%b" 
                                     self#info pro.pro_name is_dyn);
          if is_dyn then
          begin
            let ports,aux = ref [], ref [] in
            List.iter (fun emi ->
              if List.mem pro.pro_name emi.emi_procs then
              begin
                ports := !ports @ [
                    sprintf "signal %s: %s %s;"
                            (self#basename emi.emi_sig_name)
                            (match emi.emi_sig_dir with
                              | Some PT_in -> "in"
                              | Some PT_out -> "out"
                              | Some PT_bus -> "inout"
                              | _ -> "" )
                            (obj_decl_type emi.emi_sig_type)
                  ];
              end;
              ) interfaces;
              ports := !ports @ [
                    sprintf "signal %s_%s_SEL: out integer;" 
                            mname' (self#basename oname)];
              !ports,!aux
            end
            else
            begin
                let s1,s2 = ref [], ref [] in
                Array.iter (fun ot ->
                    let s1',s2' = (get_rules ot).rl_obj_port (Sym_obj ot) modu (Some pro) in
                    s1 := !s1 @ s1';
                    s2 := !s2 @ s2';
                    ) at.at_objs;
                !s1, !s2
            end;
        | None -> [],[]        
      end;
      | _ -> progerr "EMI obj_port: invalid object";
    end;
    | _ -> progerr "EMI obj_port: invalid symbol"; 

  method obj_map sym modu proo =
    let is_mon,is_debug,sym = get_mon sym in
    match sym with
    | Sym_obj obj ->
    begin
      match obj with
      | OT_object ao ->
      begin
        match proo with
        | Some pro -> 
          debug "emi" with (sprintf "obj_map: process <%s>" pro.pro_name);
          let maps = ref [] in
          List.iter (fun ema ->
            if List.mem pro.pro_name ema.ema_procs then
            begin
              maps := !maps @ [
                  sprintf "%s => %s,"
                          ema.ema_lhs
                          ema.ema_rhs
                ];
            end;
            ) mappings;
          !maps
        | None -> []
      end;
      | OT_array at ->
      begin
        let is_dyn = List.mem AT_dyn at.at_flags in
        match proo with
        | Some pro -> 
          debug "emi" with (sprintf "obj_map: process <%s>" pro.pro_name);
          if is_dyn then
          begin
            let maps = ref [] in
            List.iter (fun ema ->
              if List.mem pro.pro_name ema.ema_procs then
              begin
                maps := !maps @ [
                    sprintf "%s => %s,"
                            (self#basename ema.ema_lhs)
                            (self#basename ema.ema_rhs)
                  ];
              end;
              ) mappings;
            maps := !maps @ [
                    sprintf "%s_%s_SEL => %s_%s_%s_SEL," 
                            mname' (self#basename oname)
                            mname' (self#basename oname) 
                            pro.pro_name];
            !maps
          end
          else
          begin
                let s = ref [] in
                Array.iter (fun ot ->
                    s := !s @ ((get_rules ot).rl_obj_map (Sym_obj ot) modu (Some pro));
                    ) at.at_objs;
                !s            
          end;
        | None -> []
      end;
      | _ -> progerr "EMI obj_map: invalid object";
    end;
    | _ -> progerr "EMI obj_map: invalid symbol"; 

  (*
  ** Return object declaration (in VHDL architecture header)
  ** -> creation of data objects and signals 
  **  [Module or Process level]
  **  
  **  decl list * toplevel list
  *)
  method obj_decl sym modu proo =
    let sl,tl,cl = signals in
    let is_mon,is_debug,sym = get_mon sym in
    match sym with
    | Sym_obj obj ->
    begin
      match obj with
      | OT_object ao ->
      begin
        match proo with
        | Some pro -> 
          debug "emi" with (sprintf "obj_decl: process <%s>" pro.pro_name);
          let ports,aux = ref [], ref [] in
          List.iter (fun emi ->
              ports := !ports @ [
                  sprintf "constant %s: %s := %s;"
                          (emi.emi_const_name)
                          (match emi.emi_const_type with
                           | DT_object o -> o;
                           | dt -> (obj_decl_type dt))
                          (emi.emi_const_val)
                ];
            ) cl;
          List.iter (fun emt ->
              ports := !ports @ [
                match emt.emt_kind with
                | 'e' ->
                  sprintf "type %s is (%s);"
                          (emt.emt_type_name)
                          (let s = ref "" in
                           let first = ref true in
                           let n = ref (List.length emt.emt_type_cont) in
                           List.iter (fun c ->
                             decr n;
                             if !first && !n > 0 then s := sprintf "%s," c
                             else if !first then s := c
                             else if !n > 0 then s := sprintf "%s\n    %s," !s c
                             else s := sprintf "%s\n    %s" !s c;
                             first := false;
                             ) emt.emt_type_cont;
                           !s)
                | 'a' ->
                  sprintf "type %s is array (%s) of %s;"
                          (emt.emt_type_name)
                          (match emt.emt_type_range with
                           | Some (ENV_line ((_,a),(_,b))) -> sprintf "%s to %s" a b;
                           | _ -> error 0 (sprintf "EMI <%s>#obj_decl: invalid array range." self#info); 
                           )
                           (match (get_some emt.emt_type_type) with
                            | DT_object o -> o;
                            | dt -> (obj_decl_type dt))
                | _ -> progerr "emt_kind";
                ];
            ) tl;
          List.iter (fun emi ->
              if emi.emi_kind = 'V' then shared_vars <- shared_vars @ [emi.emi_sig_name];
              ports := !ports @ [
                  sprintf "%s %s: %s;"
                          (if emi.emi_kind = 's' then "signal" else if emi.emi_kind = 'v'  then "variable" else "shared variable")
                          (emi.emi_sig_name)
                          (match emi.emi_sig_type with
                           | DT_object o -> o;
                           | dt -> (obj_decl_type dt))
                ];
            ) sl;
          !ports,!aux
        | None -> 
          let ports,aux = ref [], ref [] in
          List.iter (fun emi ->
              ports := !ports @ [
                  sprintf "constant %s: %s := %s;"
                          (emi.emi_const_name)
                          (match emi.emi_const_type with
                           | DT_object o -> o;
                           | dt -> (obj_decl_type dt))
                          (emi.emi_const_val)
                ];
            ) cl;
          List.iter (fun emt ->
              ports := !ports @ [
                match emt.emt_kind with
                | 'e' ->
                  sprintf "type %s is (%s);"
                          (emt.emt_type_name)
                          (let s = ref "" in
                           let first = ref true in
                           let n = ref (List.length emt.emt_type_cont) in
                           List.iter (fun c ->
                             decr n;
                             if !first && !n > 0 then s := sprintf "%s," c
                             else if !first then s := c
                             else if !n > 0 then s := sprintf "%s\n    %s," !s c
                             else s := sprintf "%s\n    %s" !s c;
                             first := false;
                             ) emt.emt_type_cont;
                           !s)
                | 'a' ->
                  sprintf "type %s is array (%s) of %s;"
                          (emt.emt_type_name)
                          (match emt.emt_type_range with
                           | Some (ENV_line ((_,a),(_,b))) -> sprintf "%s to %s" a b;
                           | _ -> error 0 (sprintf "EMI <%s>#obj_decl: invalid array range." self#info); 
                           )
                           (match (get_some emt.emt_type_type) with
                            | DT_object o -> o;
                            | dt -> (obj_decl_type dt))
                | _ -> progerr "emt_kind";
                ];
            ) tl;
          List.iter (fun emi ->
              if emi.emi_kind = 'V' then shared_vars <- shared_vars @ [emi.emi_sig_name];
              ports := !ports @ [
                  sprintf "%s %s: %s;"
                          (if emi.emi_kind = 's' then "signal" else if emi.emi_kind = 'v'  then "variable" else "shared variable")
                          (emi.emi_sig_name)
                          (match emi.emi_sig_type with
                           | DT_object o -> o;
                           | dt -> (obj_decl_type dt))
                ];
            ) sl;
          !ports,!aux
      end;
      | OT_array at ->
      begin
        let is_dyn = List.mem AT_dyn at.at_flags in
        let mod_pro = module_pro_main modu in
        match proo with
        | Some pro -> 
          debug "emi" with (sprintf "obj_decl: process <%s>" pro.pro_name);
          let ports,aux = ref [], ref [] in
          !ports,!aux
        | None -> 
          let ports,aux = ref [], ref [] in
          Array.iter (fun obj ->
            match obj with
            | OT_object ao ->
              let p,a = ao.ao_type.ta_rules.rl_obj_decl (Sym_obj obj) modu proo in
              ports := !ports @ p;
              aux := !aux @ a;
            | _ -> ();
            ) at.at_objs;
          if is_dyn then 
          begin
            (*
            ** Dynamic array selector
            *)
            List.iter (fun pro ->
              if pro.pro_name <> mod_pro.pro_name then
                ports := !ports @ [sprintf "signal %s_%s_%s_SEL: integer;" 
                                           mname' (self#basename oname) pro.pro_name];
              ) allprocs;
            (*
            ** MUXed and DEMUXed Process interconnect
            *)
            List.iter (fun emi ->
                (*
                ** Signal must appear in process port mapping!
                *)
                let found = ref false in
                let dir = ref PT_in in
                let pro_name = ref "" in
                let intsig = ref "" in

                List.iter (fun pro ->
                  List.iter (fun ema ->
                    if List.mem pro.pro_name ema.ema_procs && emi.emi_sig_name = ema.ema_rhs then
                    begin
                      pro_name := pro.pro_name;
                      List.iter (fun emi ->
                        if emi.emi_sig_name = ema.ema_lhs then
                        begin
                          dir := get_some emi.emi_sig_dir;
                          intsig := ema.ema_lhs;
                        end;
                        ) interfaces;
                      found := true;
                    end;
                    ) mappings;
                  ) allprocs;

                if !found then
                begin
                  if emi.emi_kind = 'V' then shared_vars <- shared_vars @ [emi.emi_sig_name];
                  ports := !ports @ [
                      sprintf "%s %s: %s;"
                              (if emi.emi_kind = 's' then "signal" else if emi.emi_kind = 'v'  then "variable" else "shared variable")
                              (self#basename emi.emi_sig_name)
                              (match emi.emi_sig_type with
                               | DT_object o -> o;
                               | dt -> (obj_decl_type dt))
                    ];
                  (*
                  ** Create MUX (PT_out) or DEMUX (PT_in) for signal for each object
                  ** found in array.
                  *)
                  match !dir with
                  | PT_out ->
                  begin
                    let n = Array.length at.at_objs in
                    for i = 0 to n-1
                    do
                      aux := !aux @ [
                        sprintf "%s <= %s when %s = %d else %s;" 
                          (self#basename_n i emi.emi_sig_name)
                          (self#basename emi.emi_sig_name)
                          (sprintf "%s_%s_%s_SEL" mname' (self#basename oname) !pro_name)
                          i
                          (self#find_default !intsig);
                        ];
                    done;
                  end;
                  | PT_in ->
                  begin
                    aux := !aux @ [
                        sprintf "%s <= " (self#basename emi.emi_sig_name);                    
                      ];
                    let n = Array.length at.at_objs in
                    for i = 0 to n-1
                    do
                      aux := !aux @ [
                        sprintf "  %s when %s = %d else" 
                          (self#basename_n i emi.emi_sig_name)
                          (sprintf "%s_%s_%s_SEL" mname' (self#basename oname) !pro_name)
                          i;
                        ];
                    done;
                    aux := !aux @ [
                        sprintf "  %s;" (default_by_dt (self#find_signal (ENV_str emi.emi_sig_name)).emi_sig_type)
                        ];
                  end;
                  | _ -> ();
                end;
              ) sl;
          end;
          !ports,!aux
      end;
      | _ -> progerr "EMI obj_decl: invalid object";
    end;
    | _ -> progerr "EMI obj_decl: invalid symbol"; 
    
  (*
  ** Return object implementation code [VHDL] (in architecture body)
  ** -> creation of data and control path of data objects 
  **  [Module or Process level]
  *)

  method obj_code sym modu proo =
    match sym with
    | Sym_obj (OT_object ao) ->
      let pros = self#read_process in
      self#set_processes pros;
      let tops' = self#read_top in
      self#set_top tops';
      self#read_assert;

      let print_sens emp =
        let sel = emp.emp_sens in
        let n = List.length sel in
        let m = ref 0 in
        let s = ref "" in
        List.iter (fun se ->
          incr m;
          if n = 1 or !m = 1 then s := se
          else s := sprintf "%s,\n    %s" !s se; 
          ) sel;
        !s in
      [
        "--";
        sprintf "--  ConPro V%s.%s EMI %s.%s V%s" Cp_version.major Cp_version.minor mname tname version;
        "--";
        "";
      ]@  
      (List.concat (List.map (fun emp ->
        ["--";sprintf "-- EMI <%s> Process" self#info;"--"] @
        [ 
          sprintf "%s: process(%s)" emp.emp_name (print_sens emp);
        ] @ (List.map (fun line -> sprintf "  %s" line) emp.emp_decl) @
        [
          "begin";
        ] @ (List.map (fun line -> sprintf "  %s" line) emp.emp_code) @
        [
          sprintf "end process %s;" emp.emp_name;
        ]
        ) process)) @ 
      (List.concat (List.map (fun emp ->
        ["--";sprintf "-- EMI <%s>" self#info;"--"] @
        emp.emp_code 
        ) tops'))@
      ["--";sprintf "-- End of <%s>" self#info;"--";""]
    | Sym_obj (OT_array at) ->
      let is_dyn = List.mem AT_dyn at.at_flags in
      let cl = ref [] in
      Array.iter (fun obj -> 
        match obj with
        | OT_object ao ->
          let rules = ao.ao_type.ta_rules in
          cl := !cl @ (rules.rl_obj_code (Sym_obj obj) modu proo);
        | _ -> ()) at.at_objs;
      !cl
    | _ -> progerr "EMI obj_code: invalid symbol"; 
    
  
  (*
  ** Synthesize one instruction or instructions from a block list
  ** and create linear MicroCode list. 
  *)

  method instr_ucode instr id pro =
    incr id;
    let id1 = !id in
    let label name = 
        {
            ui_code = Label (sprintf "i%d_%s" id1 name);
            ui_frame=List.hd pro.pro_frame;
        } in
    let next name = 
        let l = {
            ui_code = Label (sprintf "i%d_%s_end" id1 name);
            ui_frame=List.hd pro.pro_frame;
        } in
        let j = {
            ui_code = Jump UC_next;
            ui_frame=List.hd pro.pro_frame;
        } in
        [l;j]
        in
    match instr with
    | PI_fun (src,(opl,ot),sel,args) ->
    begin
      line src;
      let ao = ao_of_ot opl ot in
      let meth = self#find_method  sel in  
      let acc = self#find_access sel in
      let set = acc.emc_set in 
      let data = acc.emc_data in
      if (List.length args) <> (List.length meth.emm_args) then
        error 0 (sprintf "EMI <%s>: number of arguments doesn't match method <%s> interface." 
                         self#info meth.emm_name); 
      let name = sprintf "%s.%s" ao.ao_name sel in      
      let args =    
        let arg_num = ref 0 in
        List.map2 (fun iarg arg_desc ->
          incr arg_num;
          let uo_flags = 
            match arg_desc.arg_type with
            | Arg_lhs -> [UO_lhs]
            | Arg_rhs -> [UO_rhs]
            | Arg_lrhs -> [UO_lhs;UO_rhs] in
          let expr_dt = Some arg_desc.arg_data_type in 
          match fun_get_arg pro mname name args uo_flags !arg_num expr_dt with
          | UA_data uc -> 
            UA_data uc;
          | _ -> error 0 (sprintf "EMI <%s>: unexpected argument in <%s>" self#info name)
          ) args meth.emm_args in
      if data <> [] then 
        [label "fun";{ui_code=Fun ((opl,ot),sel,args);
                      ui_frame=List.hd pro.pro_frame;}] @ (next "fun")
      else [];
    end;
    | _ -> [label "unknown"] @ (next "unknwon")


  method fun_compile modu proo pi top =
    let pro = 
      match proo with
      | Some pro -> pro;
      | None ->
      begin
        match my_pro_main with
       | Some pro -> pro;
       | None -> 
          let pro =  module_pro_main modu in 
          my_pro_main <- Some pro;
          pro;
      end; in 
        
    match pi with
    | PI_fun (src,(opl,ot),sel,args) ->
    begin
      (*
      ** set methods must be compiled here!
      *)
      let is_array,is_dyn_array = 
        match ot with 
        | OT_array at -> 
          true,List.mem AT_dyn at.at_flags
        |_ -> 
          false,false in

      let ao = ao_of_ot opl ot in
      let name = sprintf "%s.%s" ao.ao_name sel in
      let meth = self#find_method sel in
      let acc = self#find_access sel in
      let set = acc.emc_set in

      if (List.length args) <> (List.length meth.emm_args) then
        error 0 (sprintf "EMI#fun_compile <%s>: unexpected number of arguments." self#info);
        
      debug "emi" with (sprintf "fun_compile: <%s.%s>: pro <%s>" self#info sel pro.pro_name);
      let args =    
        let arg_num = ref 0 in
        List.map2 (fun iarg arg_desc ->
          incr arg_num;
          let uo_flags = 
            match arg_desc.arg_type with
            | Arg_lhs -> [UO_lhs]
            | Arg_rhs -> [UO_rhs]
            | Arg_lrhs -> [UO_lhs;UO_rhs] in
          let expr_dt = Some arg_desc.arg_data_type in 
          match fun_get_arg pro mname name args uo_flags !arg_num expr_dt with
          | UA_data uc -> 
            uc;
          | _ -> error 0 (sprintf "EMI#fun_compile <%s>: unexpected argument in <%s> found." self#info name)
          ) args meth.emm_args in
      let aco = Some {eac_pro=pro;eac_ao=ao;eac_uargs=List.map (fun uc -> UA_data uc) args} in
      (*
      ** Update method access process list
      *)
      if not is_array then 
        self#add_pro sel pro
      else if is_dyn_array then
        self#add_pro_array sel pro ot
      else
      begin
        if is_sel opl then
        begin
          let at = match ot with OT_array at -> at; | _ -> progerr "array?" in
          let ind = at_index at (obj_sel opl) in
          __((get_rules at.at_objs.(ind)).rl_interp (sprintf "access %s %s" sel pro.pro_name));
        end
        else
          error 0 (sprintf "EMI#fun_compile <%s>: found dynamic or expression selector in non dynamic array." 
                           self#info);
      end;
      if set <> [] then
      begin
          
        List.iter (fun sdf ->
          let param,arg = sdf aco in
          if (param,arg) <> (ENV_str "",ENV_str "") then
          begin
            let param' = self#get_name param in
            let argn = self#get_arg_num arg in
            let arg' = List.nth args (argn-1) in 


           (match arg' with
            | UC_val uv ->
            begin
              let v,t = 
                match uv.uv_val with
                | V_int i64 -> 
                  let argv = Int64.to_string i64 in
                  ENV_int i64,'i'
                | V_time (v,u) ->
                begin
                  ENV_int (match u with
                  | Nsec -> v;
                  | Usec -> Int64.mul v (Int64.of_string "1000");
                  | Msec -> Int64.mul v (Int64.of_string "1000000");
                  | Sec -> Int64.mul v (Int64.of_string "1000000000");
                  | Cycles -> v),'i';
                end;
               | V_freq (v,u) ->
                begin
                  ENV_int (match u with
                  | Hz -> v;
                  | Khz -> Int64.mul v (Int64.of_string "1000");
                  | Mhz -> Int64.mul v (Int64.of_string "1000000");
                  | Ghz -> Int64.mul v (Int64.of_string "1000000000")),'i';
                end;
                | V_string str -> ENV_str str,'s'
                | _ -> error 0 (sprintf "EMI#fun_compile <%s>: unexpected value argument in <%s> found." self#info name) in

              (*
              ** Only add new value if actually not in value list!
              *)
              let exists =
                  match self#try_lookup_env param' with
                  | Some eme -> List.mem v eme.env_val
                  | None -> false in 
              (* if not exists then *) 
              self#set_env [{env_name=param';env_val=[v];env_type=t;env_range=[];env_obj=[];env_attr=[]}];
            end;
            | UC_reg uo when (List.mem UO_rhs uo.uo_flags) ->
              (*
              ** Object import!!!
              *)
              self#set_env [{env_name=param';env_val=[ENV_str (ud_name arg')];env_type='U';env_range=[];env_obj=[arg'];env_attr=[]}];
              info (sprintf "EMI#fun_compile <%s>: importing register (read only) <%s> and assigning it to parameter <%s>." 
                            self#info uo.uo_name param');

            | UC_sig uo  -> 
              (*
              ** Object import!!!
              *)
              self#set_env [{env_name=param';env_val=[ENV_str (ud_name arg')];env_type='U';env_range=[];env_obj=[arg'];env_attr=[]}];
              info (sprintf "EMI#fun_compile <%s>: importing signal <%s> and assigning it to parameter <%s>." 
                             self#info uo.uo_name param');
            | _ ->  error 0 (sprintf "EMI#fun_compile <%s>: unexpected argument object in <%s> found."
                                     self#info name));
            debug "emi" with (sprintf "fun_compile: <%s.%s>: set <%s> <%s>" self#info sel 
                             (self#format_expr param) (self#format_expr arg));

          end;
          ) set;
      end;
    end;
    | _ -> progerr "fun_compile" 
    
  (*
  ** Synthesize a function into state list.
  *)

  method fun_scode ui label next_label modu pro =
    let instr,block = ui.ui_code,ui.ui_frame in
    match instr with
    | Fun ((opl,ot),sel,args) ->
    begin
      let ao = ao_of_ot opl ot in
      let acc = self#find_access sel in
      let meth = self#find_method sel in
      let eac = Some {eac_pro=pro;eac_ao=ao;eac_uargs=args} in
      let name = sprintf "%s.%s" ao.ao_name sel in
      
      
      let is_array,is_dyn,size,ao_name,sel_i =
          match ao.ao_array with
          | at :: _ ->
          begin
            true, List.mem AT_dyn at.at_flags,at.at_dim.(0),
            (if List.mem AT_dyn at.at_flags then at.at_name else ao.ao_name),
            if is_sel opl then at_index at (obj_sel opl) else (-1)
          end;
          | [] -> false,false,0,ao.ao_name,0 in
      let is_val ud =
        match ud with
        | UC_val _ -> true;
        | _ -> false;
        in
      let rec uo_arg arg = 
          match arg with
          | UA_data (UC_sig _)
          | UA_data (UC_val _) 
          | UA_data (UC_reg _) 
          | UA_data (UC_temp _) -> arg;
          | UA_data (UC_sel us) -> 
            let at = us.us_array in
            let is_dyn = List.mem AT_dyn at.at_flags in
            if is_dyn then
                error 0 (sprintf "EMI <%s>: dynamic selected array object <%s> not supported." self#info
                                 (Cp_printtypes.ui_sprint_uc (UC_sel us)));
            uo_arg (UA_data us.us_obj); 
          | UA_expr _ -> arg;        
          | _ -> error 0 (sprintf "EMI <%s>: unexpected method argument in <%s>"
                                  self#info name);
          in
      let sel =
        (*
        ** Dynamic accessed object array?
        ** Additional selector is required.
        *)
        if is_sel opl && is_dyn then
        begin
          let s = sprintf "%s_%s_SEL" mname' (self#basename oname) in
          [
            Data_out (sprintf "%s <= %d;" s (proda (obj_sel opl)));
            Data_def (s,"0")
          ]
        end
        else if is_sel_obj opl then
        begin
          let s = sprintf "%s_%s_SEL" mname' (self#basename oname) in
          let sel_ot = 
            match obj_sel_obj opl with
            | PI_obj (opl',ot) -> ot;
            | _ -> error 118988 ""; in
          let sel_flags =   
            [
              UO_rhs;
            ]@ 
            (if is_ot_local (Some pro) sel_ot then
              [
                UO_loc; (* local or temporary register ! *)
              ]
            else []) in
          let sel_params =
            [
              OD_conv (DT_natural 0);
            ] in

          let expr_dt = get_some (dt_of_ot sel_ot) in
          let ud_sel = ud_of_ot sel_ot sel_params sel_flags expr_dt in

          let vdr = vhdl_of_ud pro None ud_sel None in
          let sel_dp = [
                         Data_out (sprintf "%s <= %s;"
                                           s
                                           vdr.dp_sig);
                         Data_def (s,"0");

               ] @ 
               (List.map (fun str -> Data_sens str) vdr.dp_sen) @
               (List.map (fun s -> Data_top s) vdr.top_expr)@
               (List.map (fun s -> Data_top_def s) vdr.top_def) in
          sel_dp
        end
        else [] in
      if acc.emc_data <> [] then
      begin
        let dp =  List.concat (List.map (fun ds -> ds eac) acc.emc_data) @ sel in
        let cp =  List.concat (List.map (fun ds -> ds eac) acc.emc_control) in
        
        let is_ctrl = cp <> [] in
        if not is_ctrl then
        begin
          let s = {
                  s_name = label;
                  s_next = Next next_label;
                  s_data = dp;
                  s_block = block;
                  } in
          [s]
        end
        else
        begin
          let rec s = {
                    s_name = label;
                    s_next = Branch (cp,  
                                     Next label,
                                     Next next_label);
                    s_data = dp;
                    s_block = block;
                  } in
          [s] 
        end         
      end
      else 
        []
    end;
    | _ -> progerr "fun_scode"
    
  method top_vcode pi modu proo = 
    [],[]

  method bf_time modu proo pi =
    match pi with
    | PI_fun (src,(opl,ot),sel,args) ->
    begin
      FT_0
    end;
    | _ -> error 869235 ""     
    
  method new_obj modu obj_name params =
    let is_val v = 
      match v with
      | ENV_int _ | ENV_str _ | ENV_logic _ -> true;
      | _ -> false in
    out (sprintf "EMI <%s>: creating object <%s>..." self#info obj_name);
    stat "object" tname;
    let obj = self#instance in 
    obj#set_oname obj_name;
    obj#set_module modu;
    obj#copy_env;
    let envl = obj#get_env in
    obj#set_env (List.map (fun (n,v) -> 
      let exists = List.mem n (List.map (fun env -> env.env_name) envl) in
      if not exists then self#err (sprintf "Parameter <%s> not found in object environment." n);
      let et = self#get_type v in
      {env_name=n;env_val=[v];env_type=et;env_range=[];env_obj=[];env_attr=[]}) 
      params);
    obj#set_env [{env_name="O";env_val=[ENV_str obj_name];env_type='s';env_range=[];env_obj=[];env_attr=[]}];
    let meths = obj#read_methods in
    obj#set_methods meths;
    let acc = obj#read_access in
    obj#set_access acc;
    obj#create_rules;
    obj#get_rules

  (*
  ** External access to this class object
  *)
  method interp cmd =
    match cmd with
    | "compile" -> self#compile; "ok";
    | "array?" -> "ok";
    | "verbose=0" -> verbose_level <- 0; "ok"; 
    | "verbose=1" -> verbose_level <- 1; "ok";
    | "verbose=2" -> verbose_level <- 2; "ok";
    | line -> 
    begin
      match Str.split (Str.regexp " ") line with
      | ["access";sel_name;pro_name] ->
        let rec find  prol =
          match prol with
          | pro' :: tl ->
            if pro'.pro_name = pro_name then pro' else
              find tl;
          | [] -> 
          begin
            match an.a_pro with
            | Some pro' when (pro'.pro_name = pro_name) -> 
              pro'
            | _ -> self#err (sprintf "Process <%s> not found." pro_name); raise Exit
          end in
            
        let pro = 
          match my_module with
          | Some modu -> 
            let mod_pro = module_pro_main modu in
            if mod_pro.pro_name = pro_name then
              mod_pro
            else
              find modu.mod_procs;
          | None -> self#err (sprintf "Module for process <%s> not found." pro_name); raise Exit  in
        self#add_pro sel_name pro;
        "ok";
      | ["get_access"] ->
        let s = ref "" in
        List.iter (fun emm ->
          List.iter (fun pro ->
            s := sprintf "%s %s:%s" !s emm.emm_name pro.pro_name;
            ) emm.emm_procs;
          ) methods;
        !s
      | ["set";env_name;v] ->
        let envl = self#get_env in
        let env_name = 
          if env_name.[0] = '$' then (String.sub env_name 1 ((String.length env_name)-1)) 
          else env_name in
        let exists = List.mem env_name (List.map (fun env -> env.env_name) envl) in
        if not exists then self#err (sprintf "Parameter <%s> not found in object environment." env_name);
        let env_val =
          if v <> "" then
          begin
            match v.[0] with
            | '$' ->  ENV_var (String.sub v 1 ((String.length v)-1))
            | '0' .. '9' -> 
              ENV_int (try Int64.of_string v with _-> 
                          self#err (sprintf "Unexpected format of parameter <%s>=<%s> found." env_name v); raise Exit)
            | _ -> ENV_str v
          end
          else ENV_str "" in
        let et = self#get_type env_val in
        self#set_env [{env_name=env_name;env_val=[env_val];env_type=et;env_range=[];env_obj=[];env_attr=[]}];

        "ok";
      | ["get";env_name] ->
        let envl = self#get_env in
        let env_name = 
          if env_name.[0] = '$' then (String.sub env_name 1 ((String.length env_name)-1)) 
          else env_name in
        let exists = List.mem env_name (List.map (fun env -> env.env_name) envl) in
        if not exists then "not found" else 
        begin
          let eme = self#lookup_env env_name in
          match self#env_val eme with
          | ENV_str s -> s;
          | ENV_int d -> Int64.to_string d;
          | _-> "?";          
        end;
      | _ -> "err"; 
    end
 
  method verbose level =
    verbose_level <- level
       
       
  (*
  ** Module rules -- global interface
  *)
  val mutable rules = None
  
  method create_rules =
    let rec r = {
      rl_name = mname;
      rl_my = self#my;
      rl_obj_port = self#obj_port;
      rl_obj_map = self#obj_map;
      rl_obj_decl = self#obj_decl;
      rl_obj_code = self#obj_code;
      rl_instr_ucode = self#instr_ucode;
      rl_types = [{
          ta_name = of_mod mname;
          ta_rules = r; 
          ta_flags = [];
      }];
      rl_methods = List.map (fun emm -> (emm.emm_name,emm.emm_args)) methods; (* root object: empty *)
      rl_fun_compile = self#fun_compile;
      rl_fun_scode = self#fun_scode;
      rl_top_vcode = self#top_vcode;
      rl_time = self#bf_time;
      rl_new_obj = self#new_obj;
      rl_interp = self#interp;
      rl_child=None;
    } in
    out (sprintf "EMI <%s>: creating rules for module..." self#info);
    rules <- Some r
       
  method get_rules =
    match rules with
    | Some r -> r;
    | None -> error 0 (sprintf "EMI <%s>: no rules available for module." self#info)
 
end

