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
**    $INITIAL:     (C) 2006-2010 BSSLAB
**    $CREATED:     5.3.2006
**    $VERSION:     2.17
**
**    $INFO:
**
**  Analysis part of compiling: syntax and structure compiling.
**
**    $ENDOFINFO
**
*)


open Cp_syntax
open Cp_types
open Cp_symbol
open Cp_common
open Cp_utils
open Cp_expr
open Cp_block_frame
open Cp_refstack
open Cp_stat

open Unix
open Printf

include Cp_analysis_1
include Cp_analysis_2
include Cp_analysis_3
include Cp_analysis_4
include Cp_analysis_5
include Cp_analysis_6

  


(*
** Read a CP file and return list of toplevel instructions and char
** position to line position mapping [(start char,end char, line #)].
*)
let rec syntax_of_file file =
  file_name := file;
  out (sprintf "Searching Conpro file <%s>..." file);
  an.a_curpos <- nilsrc();
  let tops = ref [] in
  let lmap = ref [] in
  try
  begin
    let paths = ref (""::compiler.t_incl) in
    let success = ref false in
    let buf = ref "" in
    let prog = 
      while !paths <> [] && not !success
      do
        let ok = protects (
            let file = 
                match !paths with
                | ""::tl -> paths := tl; 
                            file;
                | hd::tl -> paths := tl;
                            sprintf "%s/%s" hd file;
                | [] -> file in

            let stat = Unix.stat file in
            let size = ref stat.st_size in
            let off = ref 0 in
            let fd = Unix.openfile file [O_RDONLY] 0 in
            out (sprintf "Opening file <%s>..." file);

            buf := String.create !size;
            while (!size > 0)
            do
                let len = Unix.read fd !buf !off !size in
                off := !off + len;
                size := !size - len;
            done;
            Unix.close fd;
            ) in
        if not ok && !paths = [] then
        begin
            print_error (sprintf "Failed to read program file <%s>" file);
            exit 1;
        end;
        success := ok;
      done;
      !buf
      in

    (*
    ** File position to line mapping
    *)
    let line = ref 1 in
    let spos = ref 1 in
    let cpos = ref 1 in
    String.iter (fun c ->
            if c = '\n' then
            begin
                lmap := !lmap @ [file,!spos,!cpos,!line];
                incr line;
                spos := !cpos;
                incr cpos;
            end
            else
            begin
                incr cpos;
            end; 
        ) prog;

    let lb = Lexing.from_string prog in
    while true
    do
        try
        begin
            let res = Cp_parser.main Cp_lexer.token lb in
            match res with
            | T_include (T_string (pos,file')) ->
                let file' = file' ^ ".cp" in
                out (sprintf "Including file <%s>..." file');
                let tops',lmap' = syntax_of_file file' in
                file_name := file;
                tops := !tops @ tops';
                lmap := !lmap @ lmap';
            | _ -> tops := !tops @ [res];
        end
        with 
        | Cp_lexer.EOF -> raise Exit;
        | _ ->
            let pos = lb.Lexing.lex_curr_pos in
            let l,p = line_of_pos {f_name= !file_name; f_cpos=pos} !lmap in
            print_syntax_error file l p;
            exit 1;
    done;
    !tops,!lmap
  end
  with
  | Sys_error str when not compiler.t_trace ->  
    raise (Synthesis (sprintf "Source code parsing failed: Sys_error %s" str));
  | Exit -> !tops,!lmap

                
(*
** Convert syntax tree list 'synli' derived from module 'mname' to module
** structure. Only a first attempt is made to compile instructions and
** resolve dependencies, connections, guards and groups.
*)

let module_of_syntax mname synli filemap =
  an.a_curpos <- nilsrc ();
  let main_source = ref (nilsrc ()) in
  
  (*
  ** Analysis environment an
  *)
  init_analysis an;
  init_modu an mname filemap;

  try
  begin

    let get_instr1 instr = List.hd (get_instr instr) in

    if !main_module = None then
    begin
        an.a_modu.mod_flags <- [Mod_main];
        main_module := Some an.a_modu;
    end;

    out_ (sprintf "Compiling module <%s>..." mname);
    ind_incr ();
    
    (*
    ** Add new module to global symbol table.
    *)
    sym_add top_syms (Sym_mod an.a_modu);
 

    (*
    ** (Pre)compile process instruction syntax list of actual handled process. 
    ** Split result in private object and executive instruction lists.
    *)
    let compile_instr_list li = 
        let objs = ref [] in
        let instrs = ref [] in
        List.iter (fun instr ->
            let new_objs = get_objs instr in
            if new_objs = [] then
            begin
                (*
                ** Its an executive instruction which must be analyzed
                ** in detail. After the instruction was compiled, 
                ** pre transformations are applied.
                *)
                let instrl' = instr_frame (get_instr instr) None in
                let instrl' =
                    List.concat (List.map (fun instr' ->
                        apply_struct_transform an.a_pro instr') instrl') in
                if instrl' <> [PI_nop] then
                begin
                    instrs := !instrs @ instrl';
                end;
            end
            else
                objs := !objs @ new_objs;
          ) li;
        !objs,!instrs in

    (*
    ** Do additional program graph transformations, for exmaple arithmetic expression expansion. 
    ** Must be done after all objects and processes were compiled!
    *)
    let post_transform_instr_list pro li = 
        out (sprintf "Performing program transformations for process <%s>..." pro.pro_name);
        out_ (sprintf "Performing program transformations for process <%s>..." pro.pro_name);
        let pro = Some pro in
        an.a_pro <- pro;
        let instrl = ref [] in
        List.iter (fun instr ->
          let new_instrl = ref [] in
          let instrl' = 
              expand_arith_expr pro
                  (expand_sel_expr pro 
                      [instr]) in
          List.iter (fun instr' ->
              new_instrl := !new_instrl @
                      (apply_method_transform an.a_pro instr')
                      ) instrl';
          if !new_instrl <> [PI_nop] then
          begin
              instrl := !instrl @ !new_instrl;
          end;
          ) li;
        an.a_pro <- None;
        !instrl in

    (*
    ** Compiling of process instructions is delayed
    ** untill all processes and functions are known.
    *)
    let compile = ref [] in

    (*
    ** Analyze a new process from syntax tree. 
    ** First the import list, second all 
    ** objects found in the body with respect to the export list, and third
    ** all instructions. 
    *)
    let new_pro synp =
        let vname,vbody = synp in
        let name = get_name vname in
        an.a_pro_name <- name;

        info (sprintf "New process <%s>..." name);
        ind_incr ();
        out_ (sprintf "New process <%s>..." name);
        ind_incr ();
        
        if sym_check_pro an.a_modu.mod_objs name then
            error 0 (sprintf "\nprocess <%s> already exists." name);


        let p = {
            pro_name = name;
            pro_module = an.a_modu;
            pro_syntax = T_process synp;
            pro_objs = Hashtbl.create 100;
            pro_import = Hashtbl.create 100;
            pro_export = Hashtbl.create 100;
            pro_temps = [];
            pro_instr = [];
            pro_ucode = [];
            pro_states = [];
            pro_alu = [];
            pro_constr = [];
            pro_frame = [{nilbf with bf_name="PROCESS";
                                     bf_type=BF_compound;}];
            pro_ao = 
              {
                ao_name = name;
                ao_module = an.a_modu;
                ao_type = {
                    ta_name = "process";
                    ta_rules = (
                        match !process_rules with
                        | Some rl -> rl;
                        | None -> error 353393 "");
                    ta_flags = [];
                };
                ao_procs = [];
                ao_array = [];
                ao_struct = [];
                ao_obj = None;
                ao_flags = [];
                ao_objs = [];
                ao_domain = an.a_modu.mod_name;
                ao_env = [];
              };
            pro_domain = an.a_modu.mod_name;
            pro_control = {
              pro_start = [];
              pro_stop = [];
              pro_call = [];
              pro_raise = [];
              pro_catch = [];
              };
            } in

        (*
        ** Add exception register
        *)
        let r_ex = new_reg an.a_modu None (sprintf "PRO_%s_EXCEPTION" name) (DT_int 8) in
        r_ex.co_rules <- !core_rules;
        r_ex.co_reader <- [p];
        r_ex.co_writer <- [p];
    
        let block_level = an.a_block_level in
        let pro_num = an.a_pro_num in
        let pro_subst = an.a_pro_subst in

        compile := !compile @ [p,(fun p -> 
          an.a_block_level <- block_level;
          an.a_pro_num <- pro_num;
          an.a_pro_subst <- pro_subst;
          
          (*
          ** process symbol tables
          *)
          an.a_pro_syms <- Some p.pro_objs;
          an.a_pro_import <- Some p.pro_import;
          let pro_connect = Hashtbl.create 20 in
          an.a_pro_export <- Some p.pro_export;

          let pro_objs = ref [] in
          let pro_instrs = ref [] in

          an.a_pro <- Some p;

          let rec iter syn =
              match syn with
              | T_block (syn,vpo) -> 
                  let optl = get_param_list (match vpo with Some vp -> vp | None -> T_empty) in
                  let params = to_param_list optl in
                  p.pro_constr <- [constr_of_block "process" params 
                                                   [compiler.t_block_constr]];
                  iter syn;
              | T_list li ->
                  let objs,instrs = compile_instr_list li in
                  pro_objs := objs;
                  pro_instrs := instrs;
              | _ -> error 0 (sprintf
                          "\nMissing instruction list in process <%s>."
                          name);
              in
          info (sprintf "Analyzing instructions of process <%s>..." name);
          ind_incr ();
          out_ (sprintf "Analyzing instructions of process <%s>..." name);
          ind_incr ();

          an.a_curpos <- nilsrc ();
          an.a_funpos <- nilsrc ();



          iter vbody;

          if name = "main" then 
          begin
            an.a_curpos <- nilsrc();
            let objs,instrs = compile_instr_list an.a_main_aux in
            pro_instrs := instrs @ !pro_instrs;
            pro_objs := objs @ !pro_objs;
          end;

          let bf = create_frame !pro_instrs in
          bf.bf_name <- "MAIN";
          bf.bf_params <- (List.hd p.pro_constr).bc_params;
          p.pro_instr <- [PI_block (!pro_instrs,bf)];
          List.iter (fun pi ->
            match pi with
            | PI_block (_,bf') -> 
               bf'.bf_parent <- Some bf;
            | _ -> ()) !pro_instrs;
          __(instr_frame !pro_instrs (Some bf));


          (*
          ** Add process to module symbol table and resolve
          ** read and write operation object dependencies.
          *)

          (* done below ... resolve_obj_dep p; *)
          

          an.a_pro_syms <- None;  
          an.a_pro_import <- None;
          an.a_pro_export <- None;
          an.a_pro_name <- "";
          an.a_pro <- None;
          p.pro_temps <- p.pro_temps @ !(an.a_pro_temps);
          
          ind_decr ();
          ind_decr ();
          )];
        sym_add an.a_modu.mod_objs (Sym_pro p);

        ind_decr ();
        ind_decr ();

        p
        in

    (*
    ** Extract a process from a syntax tree.
    *)
    let get_pro synt =
        an.a_block_level <- 1;
        let prol = ref [] in
        let rec iter s =
            match s with
            (*
            ** Go deeper inside tree.
            *)
            | T_topinstr i -> iter i;
            | T_block (i,_) -> iter i;
            | T_list il -> List.iter iter il;
            | T_process (pn,pl) ->
              if pl <> T_empty then
              begin 
                let p = new_pro (pn,pl) in
                prol := !prol @ [p];
                (*
                ** Add abstract process object
                *)
                let otype = "process" in 
                let ta =
                  if sym_check_type an.a_modu.mod_objs otype then
                  begin
                      match sym_get_type an.a_modu.mod_objs otype with
                      | Type_abstract ta -> ta;
                      | _ -> error 0 (sprintf "\nUnknown abstract type <%s> found??" otype);
                  end
                  else
                      error 0 (sprintf "\nUnknown abstract type <%s> found??" otype);
                  in
                let ao = p.pro_ao in
                sym_add an.a_modu.mod_objs (Sym_obj (OT_object ao));
              end
              else
              begin
                (*
                ** Imported process (from microcode level)
                *)
                let name = sprintf "%s_%s" an.a_modu.mod_name (get_name pn) in
                info (sprintf "Importing process <%s>..." name);
                let p = Cp_uci.import_pro name in
                ()
              end;
            (*
            ** Stop.
            *)
            | _ -> ();
            in
        iter synt;
        !prol
        in

    (*
    ** Process arrays are compiled here, if any!
    *)
    let get_procs_to_compile () =
        let prol = ref [] in
        List.iter (fun (pro_arr_num,pro_name,pro_syn,pro_aoo) ->
            an.a_pro_num <- pro_arr_num;
            let p = new_pro (T_ident ({f_name="";f_cpos=0},pro_name),
                                     pro_syn) in
            prol := !prol @ [p];
            match pro_aoo with
            | Some ao -> p.pro_ao <- ao;
            | None -> ();
            ) an.a_procs_to_compile;
        an.a_procs_to_compile <- [];
        !prol
        in

    let get_loop_limits expr lim1 lim2 =
      let lim1 = expr_fold (get_instr1 lim1) in
      let lim2 = expr_fold (get_instr1 lim2) in
      let iname =
          match expr with
          | T_ident ind -> 
            let name = get_name expr in
            name
          | _ -> error 0 "\nNon constant expression found in for-loop to be unrolled.";   
          in
      let get_lim lim =
        match lim with
        | PI_obj (opl,ot) ->
        begin
          match ot with
          | OT_named_value (_,v)
          | OT_value v -> Some v;
          | OT_const co -> Some co.co_init;
          | _ -> None
          end;
        | _-> None
        in
      let val_a = get_lim lim1 in
      let val_b = get_lim lim2 in
      let a =
          match val_a with
          | Some (V_int w) -> Int64.to_int w;
          | _ -> error 0 "\nCan't unroll non static loop."; in
      let b =
          match val_b with
          | Some (V_int w) -> Int64.to_int w;
          | _ -> error 0 "\nCan't unroll non static loop."; in
      iname,a,b in
    
    let get_branch_state expr =
      let expr = expr_fold (get_instr1 expr) in
      let i = Int64.to_int (get_const_expr expr) in
      i <> 0 in
       
    (*
    ** First all toplevel objects...
    *)
    out_ "Searching toplevel objects and expanding toplevel loops and evaluating conditionals...";
    ind_incr ();
    
    let funs = ref [] in
    let pros = ref [] in
    
    an.a_block_level <- 0;
    an.a_toplevel <- true;
    let synli =
      let rec subst syn sl =
        (*
        ** Substitute identifiers in expressions...
        *)
        match syn with
        | T_ident (pos,name) ->
        begin
          if pos <> {f_name="";f_cpos=0} then an.a_curpos <- source_of_pos pos;
          let rec map sl = 
            match sl with
            | (name1,name2) :: tl -> 
              if name1 <> name then
                map tl
              else
                name2;
            | [] -> name in
          T_ident (pos, map sl);
        end;
        | T_assign (lhs,rhs) ->
          T_assign ((subst lhs sl),(subst rhs sl));
        | T_map (lhs,rhs) ->
          T_map ((subst lhs sl),(subst rhs sl));
        | T_OP_arith  (op,op1,op2) ->
          T_OP_arith  (op,(subst op1 sl),(subst op2 sl)); 
        | T_OP_bool  (kind,op,op1,op2) ->
          T_OP_bool  (kind,op,(subst op1 sl),(subst op2 sl)); 
        | T_sub (s1,s2) ->
          T_sub (subst s1 sl,subst s2 sl);
        | T_interval (s1,s2) ->
          T_interval (subst s1 sl,subst s2 sl);          
        | T_selector (s1,s2) ->
          T_selector (subst s1 sl,subst s2 sl);
        | T_Fun (s1,s2l,s3) ->
          T_Fun (subst s1 sl,
                 List.map (fun s -> subst s sl) s2l,
                 subst s3 sl);
        | _ -> syn in
        
        
      let rec expand synli' substl = 
        (*
        ** Find object definitions and do
        ** toplevel loop unrolling and
        ** toplevel conditional evaluation.
        *)
        match synli' with
        | syn :: tl' ->
        begin 
          match syn with
          | T_forloop (expr,dir,lim1,lim2,block) ->
          begin
            let expr = subst expr substl in
            let iname,a,b= get_loop_limits expr lim1 lim2 in
            let synli''' = ref [] in
            info (sprintf "Unrolling toplevel loop for <%s> = {%d,%d}" 
                         iname a b);
            match block with
            | T_block (T_list synli'',_) ->
              if a < b then
                for i = a to b
                do
                  let subst = (iname,sprintf "%d" i) in
                  synli''' := !synli''' @ (expand synli'' (subst :: substl)); 
                done
              else
                for i = a downto b
                do
                  let subst = (iname,sprintf "%d" i) in
                  synli''' := !synli''' @ (expand synli'' (subst :: substl)); 
                done;
              !synli''' @ (expand tl' substl);
            | syn'' ->
              if a < b then
                for i = a to b
                do
                  let subst = (iname,sprintf "%d" i) in
                  synli''' := !synli''' @ (expand [syn''] (subst :: substl)); 
                done
              else
                for i = a downto b
                do
                  let subst = (iname,sprintf "%d" i) in
                  synli''' := !synli''' @ (expand [syn''] (subst :: substl)); 
                done;
              !synli''' @ (expand tl' substl);
          end;
          | T_branch (expr,s1,s2) -> 
          begin
            let expr = subst expr substl in
            let cond = get_branch_state expr in
            info (sprintf "Expanding toplevel conditional branch: %b" cond);
            if cond then 
            begin
              let e1 = expand [s1] substl in
              let el = expand tl' substl in
              e1 @ el
            end
            else
            begin
              let e1 = expand [s2] substl in
              let el = expand tl' substl in
              e1 @ el
            end;
          end;
          | T_topinstr syn ->
            let e1 = expand [syn] substl in
            let el = expand tl' substl in
            e1 @ el
          | T_block (T_list synli',attr) ->
            let synli'' = expand synli' substl in
            let el = expand tl' substl in
            [T_block (T_list synli'',attr)] @ el
          | T_Fun_def (fname,_,_,_) ->
          begin
            funs := !funs @ [get_name fname];
            pros := !pros @ [sprintf "FUN_%s" (get_name fname)];
            __(get_objs syn);
            let s1 = subst syn substl in
            let el = expand tl' substl in
            s1 :: el            
          end;
          | T_process (pname,_) -> 
          begin
            pros := !pros @ [get_name pname];
            __(get_objs syn);
            let s1 = subst syn substl in
            let el = expand tl' substl in
            s1 :: el
          end
          | T_OT_array (anames,'p',_,_,_,_) ->
          begin
            (*
            ** Process array!
            *)
            pros := !pros @ (List.map (fun s -> sprintf "%s_[0-9]+" (get_name s)) anames);
            __(get_objs syn);
            let s1 = subst syn substl in
            let el = expand tl' substl in
            s1 :: el
          end            
          | _ -> 
          begin
            __(get_objs syn);
            let s1 = subst syn substl in
            let el = expand tl' substl in
            s1 :: el
          end
        end;
        | [] -> [] in
      expand synli [] in

    let rec is_fun syn =
      match syn with
      | T_Fun _ -> true
      | T_topinstr syn' -> is_fun syn'
      | T_block (syn',_) -> 
        is_fun syn'
      | T_list li ->
        let rec iter li =
          match li with
          | hd :: tl -> if is_fun hd then true else iter tl
          | [] -> false in
        iter li
      | T_branch (expr,s1,s2) -> 
        if is_fun s1 then true 
        else if is_fun s2 then true
        else false
      | _ -> false in
        

    ind_decr ();
    out_ "Analyzing toplevel method calls...";
    an.a_toplevel <- true;
    List.iter (fun syn ->
        (*
        ** Only few instructions are allowed in toplevel mode!
        *)
        match syn with
        | instr when is_fun instr -> an.a_modu.mod_instr  <- an.a_modu.mod_instr @ (get_instr syn);
        | _ -> ();
      ) synli;

    let fnum = ref 0 in
    out_ "Analyzing function blocks...";
    ind_incr ();
    
    (*
    ** Preserve function block definition order - reorder...
    ** Ordered: fnl (names)
    ** Unordered: sl (symbols)
    *)
    let rec sort_fun_blocks sl fnl =
      match fnl with
      | fname :: fnl_tl ->
      begin
        let rec search sl =
          match sl with
          | (Sym_fun f) :: sl_tl  ->
          begin
            if f.fun_name = fname then (Sym_fun f) 
              else
              search sl_tl; 
          end;
          | _ :: sl_tl -> search sl_tl;
          | [] -> error 0 (sprintf "Can't find function block symbol <%s>." fname) in
        (search sl) :: (sort_fun_blocks sl fnl_tl)        
      end;
      | [] -> [] in
      
    List.iter (fun sym ->
      match sym with
      | Sym_fun f ->
        if not f.fun_inline || compiler.t_ml || compiler.t_C then
        begin
            let name str = T_ident (pos_of_source !main_source,str) in
            let is_fun = f.fun_ret_obj <> [] in
            let ftype = if is_fun then "function" else "procedure" in
            let fname = f.fun_name in
            out (sprintf "%s <%s>..." ftype fname);
            an.a_toplevel <- false;
            let old_subst = an.a_pro_subst in
            an.a_pro_subst <- an.a_pro_subst @ (List.map (fun co ->
                  let n = (String.length fname) + 5 in
                  let sname' = co.co_name in
                  let len = String.length sname' in
                  let sname =  String.sub sname' (4+n) (len-n-4)  in
                  debug "module_of_syntax" with (sprintf "parameter: %s -> %s" sname sname');
                  sname,sname'
                ) (f.fun_args_obj@f.fun_ret_obj));
            if not compiler.t_ml && not compiler.t_C then
              an.a_main_aux <- an.a_main_aux @ [
              T_Fun (T_selector (name (sprintf "LOCK_FUN_%s" f.fun_name),
                               name "init"),[],T_empty)
            ];
        
            an.a_modu.mod_procs <- an.a_modu.mod_procs @ (get_pro f.fun_instr);
            an.a_toplevel <- false;
            an.a_pro_subst <- old_subst;
            incr fnum;
        end;
      | _ -> ();
      ) (sort_fun_blocks (List.filter (fun sym -> 
            match sym with
            | Sym_fun _ -> true;
            | _ -> false) (list_of_sym an.a_modu.mod_objs)) !funs);
    ind_decr ();
    
    (*
    ** Now all processes with private objects. Check import and export
    ** lists.
    *)
    out_ "Analyzing processes...";
    ind_incr ();
    
    let pronum = ref 0 in
    an.a_toplevel <- false;
    List.iter (fun syn ->
        let pro = get_pro syn in
        an.a_modu.mod_procs <- an.a_modu.mod_procs @ pro;
        (*
        ** Maybe we have process arrays to compile...
        *)
        an.a_modu.mod_procs <- an.a_modu.mod_procs @ 
                              (get_procs_to_compile ());
      ) synli;

    ind_decr ();

    out_ "Analyzing process instructions...";
    ind_incr ();
    
    List.iter ( fun (p,f) ->
      f p ) !compile;
      
    ind_decr ();

    out (sprintf "Performing program graph transformations and optimization for module <%s>..." 
                 an.a_modu.mod_name);
    ind_incr ();
    
    List.iter (fun pro ->
      out (sprintf "in process <%s>..." pro.pro_name);
      ind_incr ();
      pro.pro_instr <- post_transform_instr_list pro pro.pro_instr;
      resolve_obj_dep pro;
      ind_decr ();
      ) an.a_modu.mod_procs;
    ind_decr ();

    out_ "Analyzing toplevel instructions...";
    an.a_toplevel <- true;
    List.iter (fun syn ->
        (*
        ** Only few instructions are allowed in toplevel mode!
        *)
        match syn with
        | instr when is_fun instr ->  ();
        | _ -> an.a_modu.mod_instr  <- an.a_modu.mod_instr @ (get_instr syn);
      ) synli;
    (*
    ** Resolve object dependencies for module.
    ** A process wrap is required..
    *)
    let temp_pro_main = module_pro_main an.a_modu in
    resolve_obj_dep temp_pro_main;

    (*
    ** Reorder processes to orignal order:
    *)
    let pl = ref [] in
    
    List.iter (fun p ->
      pl := !pl @ (List.filter (fun pro ->
        (Str.global_replace (Str.regexp p) "" pro.pro_name)=""
        ) an.a_modu.mod_procs);
      ) !pros;
    if (List.length !pl) <> (List.length an.a_modu.mod_procs) then
      error (-1) "Inconsistent process re-ordering found.";
      
    an.a_modu.mod_procs <- !pl;

    (*
    ** Talk about the results...
    *)
    analyse_log "Default block parameters"
      (
        let s = ref "" in
        List.iter (fun bp ->
          match bp with
          | BP_expr ep -> 
          begin
            s := sprintf "%s [%s=%s]" !s "EXPR" (match ep with
            | EXPR_flat -> "FLAT"
            | EXPR_alu -> "ALU"
            | EXPR_binary -> "BIN"
            | EXPR_top -> "TOP")
          end;
          | BP_schedule sp -> 
          begin
            s := sprintf "%s [%s=%s]" !s "SCHD" (match sp with
            | Sched_auto -> "AUTO"
            | Sched_custom sl ->
              let s' = ref "" in
              List.iter (fun s ->
                match s with
                | Sched_refstack -> s' := sprintf "%s %s" !s' "REF"
                | Sched_expr -> s' := sprintf "%s %s" !s' "EXPR"
                | Sched_basicblock -> s' := sprintf "%s %s" !s' "BASBL"
                ) sl; !s'
            | Sched_def -> "DEF");
          end;
          | BP_temp ts ->
            s := sprintf "%s [%s=%s]" !s "TEMP" ts;
          | BP_alu_min_width n ->
            s := sprintf "%s [%s=%d]" !s "ALU_MIN_N" n;            
          | _ -> ();
          ) compiler.t_block_constr.bc_params;
        !s
      );
    analyse_log "Project Parameters" "";
    List.iter (fun (n,v) ->
      analyse_log "" (sprintf "%s=%s" n v) ) compiler.t_defs;
                
    analyse_log "Toplevel symbols" 
                (string_of_int (Cp_symbol.length an.a_modu.mod_objs));
    analyse_log "Processes" 
                (string_of_int (List.length an.a_modu.mod_procs)); 
    analyse_log "Functions"
                 (string_of_int !fnum); 
    analyse_log "Process order" "";
    List.iter (fun pro -> analyse_log "" pro.pro_name) an.a_modu.mod_procs;
    ind_decr ();
    analyse_summary ();
    an.a_modu
  end
  with
    | Not_found when not compiler.t_trace ->
        raise (Synthesis "Analysis failed: Aborting due to compiler error: Not found.");
    | Exit when not compiler.t_trace -> raise Exit;
    | _ when not compiler.t_trace ->
        raise (Synthesis "Analysis failed: Aborting due to compiler error: Internal error.");

