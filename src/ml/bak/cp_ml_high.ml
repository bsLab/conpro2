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
**    $CREATED:     21.9.2009
**    $VERSION:     2.20
**
**    $INFO:
**
**  ML backend: high-level synthesis of ConPro module, process, and object model to 
**              ML functional language model
**
**    Abstraction level: high (default)
**
**    $ENDOFINFO
**
*)
open Cp_types
open Cp_syntax
open Cp_common
open Cp_utils
open Cp_ml_types
open Cp_symbol
open Printf
open Cp_print
open Cp_expr
open Cp_data

let ml_et = ref []
let ml_exc_map = ref false
let ml_defs = ref [] 
let ml_evals = ref [] 
let ml_exc_env = ref 0 
let ml_mod_dep = ref []
let ml_expr_close = ref ""

let err str =
  error 0 (sprintf "ML: %s" str)
let rec env_search envl name = 
  match envl with
  | env :: tl -> if Hashtbl.mem env name then true else env_search tl name
  | [] -> false
  
let rec env_get envl name = 
  match envl with
  | env :: tl -> if Hashtbl.mem env name then Hashtbl.find env name else env_get tl name
  | [] -> err (sprintf "env_get: %s not found" name)
  
let env_add env name sym = 
  debug "c:env_add" with (sprintf "Adding <%s>." name);
  if not (Hashtbl.mem env name) then Hashtbl.add env name sym
let sprint_env env =
  match env with
  | ML_struct c -> "ML_struct()"
  | ML_array c -> "ML_array()"
  | ML_enum c -> "ML_enum()"
  | ML_type c -> "ML_type()"
  | ML_abstract_type c -> "ML_abstract_type()"
  | ML_exception c -> "ML_exception()"
  | ML_storage c -> "ML_storage()"
  | ML_abstract c -> "ML_abstract()"
  | ML_fun c -> "ML_fun()"
  | ML_unknown -> "ML_unknown"

let print_env envl =
  out_ "Environment:";
  List.iter (Hashtbl.iter (fun name env ->
    out_ (sprintf "%s:%s" name (sprint_env env))
    )) envl
  
(*
** Evaluate a boolean expression. Parameter list contains
** parameter values replaced by parameters inside expression.
*)

let eval expr paraml =
  try
  begin
    let s2i = int_of_string in
    let s op =
      let rec find paraml =
        match paraml with
        | (n,v) :: tl -> if n = op then v else find tl 
        | [] -> op in
      find paraml in
    let tokens = List.filter (fun token -> token <> "") 
                  (Str.split (Str.regexp " ") expr) in
    let rec eval tokenl =
      match tokenl with
      | [op1;"<";op2] -> (s2i (s op1)) < (s2i (s op2))
      | [op1;">";op2] -> (s2i (s op1)) > (s2i (s op2))
      | [op1;"<>";op2] -> (s2i (s op1)) <> (s2i (s op2))
      | [op1;"<=";op2] -> (s2i (s op1)) <= (s2i (s op2))
      | [op1;">=";op2] -> (s2i (s op1)) >= (s2i (s op2))
      | [op1;"=";op2] -> (s op1) = (s op2)
      | _ -> false in
    eval tokens;
  end
  with _ -> false
  
(*
** Convert uppercase symbols to lowercase symbols
*)
let fix_name name =
  if name = "" then ""
  else match name.[0] with
  | 'A'..'Z' -> 
  begin
    let name' = String.copy name in
    name'.[0] <- char_of_int ((int_of_char name.[0])+32);
    name'
  end; 
  | _ -> name
  
let oc = ref Pervasives.stdout   
let out_indent = ref 0
let out str = output_string !oc ((ibox !out_indent str)^"\n")
let ind_incr () = out_indent := !out_indent + 2  
let ind_decr () = out_indent := !out_indent - 2  

(*
** Check for some immutable storage, for example loop counters.
** Returns: mutable?,fun,reference, reduced original name (substitution)
*)
let ml_env_transform name =
  let check_loop = Str.string_match (Str.regexp "LOOP_") name 0 in
  let check_fun_arg = Str.string_match (Str.regexp "ARG_FUN_") name 0 in
  let check_fun_ret_arg = Str.string_match (Str.regexp "RET_FUN_") name 0 in
  let check_process_id = name = "#" in
  if check_loop then
  begin
    false,false,false,Str.global_replace (Str.regexp "\(LOOP_\)\(.\)\(_.+\)") "\2" name
  end
  else if check_fun_arg then
  begin
    true,true,false,Str.global_replace (Str.regexp "ARG_FUN_\(.+\)") "\1" name
  end
  else if check_fun_ret_arg then
  begin
    true,true,true,Str.global_replace (Str.regexp "RET_FUN_\(.+\)") "\1" name
  end
  else if check_process_id then
    false,false,false,"process_id"
  else
    true,false,false,""

    
(*
** Create object descriptor: Extract structure and static array selectors from
** object name and return environment object:
**
**  obj ... -> num_sel=[],sel=[]
**  obj_0 ... -> num_sel=[0]
**  obj_0_1 ... -> num_sel=[0;1]
**  obj_sel ... -> sel=[sel]
**  obj_sel_0 ... -> num_sel=[0],sel=[sel]
**
** Returns object descriptor or raises error <Not_found>
*)
let ml_obj_desc env name ar str =
  let sym_name = name in
  let tokens = Str.split (Str.regexp "_") sym_name in
  let num_sel = ref [] in
  let sel = ref "" in
  let is_num s = protects(__(int_of_string s)) in
  let get_num s = int_of_string s in
  let scan = ref true in
  let remains = ref tokens in
  let ar = List.filter (fun at ->
    not (List.mem AT_dyn at.at_flags)
    ) ar in

  (*
  ** Extract array selector
  *)
  if ar <> [] then 
  begin
    remains := [];
    List.iter (fun token ->
    if (is_num token) && !scan then
      num_sel := (get_num token) :: !num_sel
    else
    begin
      remains := token :: !remains;
      scan := false;
    end;
    ) (List.rev tokens);
  end;
  (*
  ** Incremental object lookup - find longest token list match!
  **  a_b_c matches both a_b and a_b_c!
  *)
  let rec find tokenl name last =
    match tokenl with
    | token :: tl ->
      let name = sprintf "%s%s%s" name (if name <> "" then "_" else "") token in
      if env_search env name then find tl name (Some (name,tl))
      else if last <> None then get_some last else find tl name None
    | [] -> if last <> None then get_some last else "",[] in
  let env_obj,remains' = find !remains "" None in
  (*
  ** Extract structure selector
  *)
  List.iter (fun token ->
    if !sel = "" then sel := token
    else sel := sprintf "%s_%s" !sel token
    ) remains';
    
  if not (env_search env env_obj) then
    err (sprintf "Environment object <%s> not found." sym_name)
  else match env_get env env_obj with
  | ML_storage sto -> 
  begin
    match sto.sto_type with
    | ML_type _ | ML_enum _ ->
      {desc_name=sto.sto_name;desc_obj=ML_storage sto;
       desc_num_sel=[];desc_sel=[];
       desc_array=false;desc_struct=false;
       desc_mutable=sto.sto_mutable;desc_export=sto.sto_export;desc_subst=sto.sto_subst;desc_ref=sto.sto_ref;}
    | ML_array ar ->
      {desc_name=ar.array_name;desc_obj=ML_storage sto;
       desc_num_sel= !num_sel;desc_sel=[!sel];
       desc_array=true;desc_struct=(match ar.array_type with ML_struct _ -> true|_ ->false);
       desc_mutable=sto.sto_mutable;desc_subst=sto.sto_subst;desc_export=sto.sto_export;desc_ref=sto.sto_ref;}
    | ML_struct st ->
      {desc_name=sto.sto_name;desc_obj=ML_storage sto;
       desc_num_sel=[];desc_sel=[!sel];
       desc_array=false;desc_struct=true;
       desc_mutable=sto.sto_mutable;desc_subst=sto.sto_subst;desc_export=sto.sto_export;desc_ref=sto.sto_ref;}
    | _ -> err (sprintf "Unexpected storage object type for <%s> found." env_obj)
  end;
  | ML_abstract ab ->
  begin
    match ab.abstr_type with
    | ML_abstract_type _ ->
      {desc_name=ab.abstr_name;desc_obj=ML_abstract ab;
       desc_num_sel=[];desc_sel=[];
       desc_array=false;desc_struct=false;
       desc_mutable=false;desc_subst=ab.abstr_subst;desc_export=false;desc_ref=false}
    | ML_array ar ->
      {desc_name=ar.array_name;desc_obj=ML_abstract ab;
       desc_num_sel= !num_sel;desc_sel=[!sel];
       desc_array=true;desc_struct=(match ar.array_type with ML_struct _ -> true|_ ->false);
       desc_mutable=false;desc_subst=ab.abstr_subst;desc_export=false;desc_ref=false}
    | ML_struct st ->
      {desc_name=ab.abstr_name;desc_obj=ML_abstract ab;
       desc_num_sel=[];desc_sel=[!sel];
       desc_array=false;desc_struct=true;
       desc_mutable=false;desc_subst=ab.abstr_subst;desc_export=false;desc_ref=false}
    | _ -> err (sprintf "Unexpected abstract object type for <%s> found." env_obj)
    
  end;
  | _ -> err (sprintf "Unexpected environment object <%s> found." env_obj)


let ml_get_type env name =
  if not (env_search env name) then
    err (sprintf "Can't find environment type <%s>." name)
  else match env_get env name with
  | ML_struct _ | ML_enum _ -> env_get env name
  | _ -> err (sprintf "Environment symbol <%s> is not a type." name)
   
let ml_get_exc env id =
  let name = sprintf "EXC%d" id in
  if not (env_search env name) then
    err (sprintf "Can't find environment exception <EXC%d>." id)
  else match env_get env name with
  | ML_exception exc -> exc
  | _ -> err (sprintf "Environment symbol <%s> is not an exception type." name)
  
(*
** Transform symbol table <objs> into
** ML environment
*)
let ml_env glob_env modu objs =
  let syms = list_of_sym objs in
  let types = List.flatten (List.map (fun sym -> match sym with Sym_type tp -> [tp] | _ -> []) syms) in
  let objs = List.flatten (List.map (fun sym -> match sym with Sym_obj ot -> [ot] | _ -> []) syms) in
  let funs = List.flatten (List.map (fun sym -> match sym with Sym_fun f -> [f] | _ -> []) syms) in
  let env = Hashtbl.create 100 in

  (*
  ** Incremental object lookup - find longest token list match!
  **  a_b_c matches both a_b and a_b_c!
  *)
  let rec find tokenl name last =
    match tokenl with
    | token :: tl ->
      let name = sprintf "%s%s%s" name (if name <> "" then "_" else "") token in
      if env_search (env::glob_env) name then find tl name (Some (name,tl))
      else if last <> None then get_some last else find tl name None
    | [] -> if last <> None then get_some last else "",[] in

  let rec name_ il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "_" else "")^(name_ tl)
    | [] -> "" in

  let add_ao name ao env_obj =
    let name,name' = name,fix_name name in
    let code = ref [] in
    let fname = sprintf "%s.ml" ao.ao_type.ta_name in
    let pathl = ""::(compiler.t_lib@compiler.t_incl) in        
    let rec f_open pathl =
      match pathl with
      | path :: tl ->
      begin
        try open_in (sprintf "%s%s%s" path (if path <> "" then "/" else "") fname)
        with _ -> f_open tl
      end;
      | [] -> err (sprintf "Can't open ML ADTO file <%s>." fname) in
    let ic = f_open pathl in
    let more = ref true in
    while !more 
    do
      more := protects (code := !code @ [input_line ic]);    
    done;
    close_in ic;

    (*
    ** Try to extract default environment parameter values
    *)
    let params = ref [] in
    List.iter (fun line ->
        let re = Str.regexp " *val mutable \(.+\) = \(.+\)" in
        let nv = Str.global_replace re "\1:\2" line in
        match Str.split (Str.regexp ":") nv with
        | [n;v] -> 
          params := !params @ [Str.global_replace (Str.regexp " ") "" n,
                               Str.global_replace (Str.regexp " ") "" v];

        | _ -> ();       
      ) !code;

    (*
    ** Extract environment depending values and methods
    *)

    let discard = ref false in
    let filter = ref false in
    let mclass = ref "" in
    
    let code = List.filter (fun line -> line <> "")
      (List.map (fun line ->
        let re1 = Str.regexp " *class *\([a-zA-Z_0-9]+\) .+ *when *\(.+\) *= *" in
        let re2 = Str.regexp " *class *\([a-zA-Z_0-9]+\) .+ *= *" in
        let re11 = Str.regexp " *class \['a\] *\([a-zA-Z_0-9]+\) .+ *when *\(.+\) *= *" in
        let re21 = Str.regexp " *class \['a\] *\([a-zA-Z_0-9]+\) .+ *= *" in
        let re3 = Str.regexp " *end class *" in
        let p1 = Str.global_replace re1 "\2" line in
        let p2 = Str.global_replace re2 "" line in
        let p11 = Str.global_replace re11 "\2" line in
        let p21 = Str.global_replace re21 "" line in
        let p3 = Str.global_replace re3 "" line in
        let in_discard = !discard in
        if p1 <> line then
        begin
          let v = eval p1 !params in
          discard := not v;
          if not !discard then 
          begin
            mclass := Str.global_replace re1 "\1" line;
            filter := true;
          end;
        end
        else if p11 <> line then
        begin
          let v = eval p11 !params in
          discard := not v;
          if not !discard then 
          begin
            mclass := Str.global_replace re11 "\1" line;
            filter := true;
          end;
        end
        else if p2 <> line  then
        begin
          mclass := Str.global_replace re2 "\1" line;
          discard := false;
        end
        else if p21 <> line  then
        begin
          mclass := Str.global_replace re21 "\1" line;
          discard := false;
        end
        else if p3 <> line  then
        begin
          discard := false;
          filter := true;
        end;
        if !discard || in_discard then "" 
        else if !filter then
        begin
          filter := true;
          let re1 = Str.regexp "\(.+\)when .+ \(=.*\)" in
          let line = Str.global_replace re1 "\1 \2" line in
          let re3 = Str.regexp " *end class *" in
          let line = Str.global_replace re3 "end" line in
          line
        end
        else line
        ) !code) in
    if !mclass = "" then err (sprintf "Can't find class for abstract object <%s>." name);
    env_add env name (ML_abstract {
        abstr_name = name;
        abstr_env = ao.ao_env;
        abstr_code = code;
        abstr_type = env_obj;
        abstr_class = !mclass;
        abstr_class_type = ao.ao_type.ta_name;
        abstr_subst = if name' <> name then name' else "";
      }) in

  List.iter (fun fdef ->
    env_add env fdef.fun_name (ML_fun {
      fn_name=fdef.fun_name;
      fn_args=List.map2 (fun a b -> a,b) fdef.fun_args fdef.fun_args_obj;
      fn_rets=List.map2 (fun a b -> a,b) fdef.fun_ret fdef.fun_ret_obj;
      fn_inline=fdef.fun_inline;
      })
    ) funs;
    
  List.iter (fun tp ->
    match tp with
    | Type_const tc ->
      env_add env tc.tc_name (ML_enum {
        enum_name=tc.tc_name;
        enum_elems=List.map (fun ot -> 
            match ot with
            | OT_const co -> co.co_name;
            | _ -> progerr "tc_elems/=OT_const") tc.tc_elems;
        enum_numbers=List.map (fun ot -> 
            match ot with
            | OT_const co -> int_of_val co.co_init;
            | _ -> progerr "tc_elems/=OT_const") tc.tc_elems;
      })
    | Type_struct ts ->
      env_add env ts.ts_name  (ML_struct {
        strut_name = ts.ts_name;
        strut_elems = List.map (fun te ->
          (te.te_name,(let dt,_ = te.te_type in dt))
          ) ts.ts_elems;
        strut_names = List.map (fun te ->
          sprintf "%s_%s" ts.ts_name te.te_name
          ) ts.ts_elems;
      })
    | Type_exc ex ->
      env_add env ex.tx_name (ML_exception {
        exc_name=ex.tx_name;
        exc_id=ex.tx_id;
      });
      env_add env (sprintf "EXC%d" ex.tx_id) (ML_exception {
        exc_name=ex.tx_name;
        exc_id=(-1);
      });
    | _ -> ();
    ) types;
  List.iter (fun ot ->
    match ot with
    | OT_signal co | OT_reg co | OT_var co ->
    begin
      match co.co_array with
      | [at] | [_;at] -> 
        let name,name' = at.at_name,fix_name at.at_name in
        env_add env at.at_name  (ML_storage {
          sto_name = name;
          sto_mutable = true;
          sto_export = sym_check_obj modu.mod_export at.at_name;
          sto_subst = if name' <> name then name' else "";
          sto_def = None;
          sto_ref = false;
          sto_sym = false;
          sto_type = ML_array {
            array_name = at.at_name;
            array_dims = at.at_dim;
            array_type = (
              match co.co_struct with
              | [st] -> 
                  if env_search (env::glob_env) st.st_type.ts_name then
                    env_get (env::glob_env) st.st_type.ts_name
                  else
                    err (sprintf "Unknown structure type <%s> found." st.st_type.ts_name)
              | _ ->
                  ML_type co.co_type);
            }});
      | _ -> 
      begin
        match co.co_struct with
        | [st] ->
          let name,name' = st.st_name,fix_name st.st_name in          
          env_add env st.st_name  (ML_storage {
            sto_name = name;
            sto_mutable = true;
            sto_export = sym_check_obj modu.mod_export st.st_name;
            sto_subst = if name' <> name then name' else "";
            sto_def = None;
            sto_ref = false;
            sto_sym = false;
            sto_type = (
              if env_search (env::glob_env) st.st_type.ts_name then
                env_get (env::glob_env) st.st_type.ts_name
              else
                err (sprintf "Unknown structure type <%s> found." st.st_type.ts_name));
            });
        | _ -> 
          let is_mut,is_fun,is_ref,subst = ml_env_transform co.co_name in
          let name,name' = 
            if is_fun then
            begin
              let namel = Str.split (Str.regexp "_") subst in
              let fun_name,namel' = find namel "" None in
              co.co_name,sprintf "%s" (name_ namel')
            end
            else if subst = "" then co.co_name,fix_name co.co_name
            else co.co_name,subst in                     
          env_add env co.co_name  (ML_storage {
            sto_name = name;
            sto_mutable = is_mut;
            sto_export = sym_check_obj modu.mod_export co.co_name;
            sto_subst = if name' <> name then name' else "";
            sto_type = (if co.co_type_name <> "" then
                          ml_get_type (env::glob_env) co.co_type_name
                        else ML_type co.co_type);
            sto_def = None;
            sto_sym = false;
            sto_ref = is_ref;
            })
      end;
    end;
    | OT_queue qu ->
    begin
      let co = qu.qu_obj in
      let ao = {qu.qu_ao with ao_type={qu.qu_ao.ao_type with ta_name="queue";}} in
      match co.co_array with
      | [at] | [_;at] -> 
        add_ao at.at_name ao (ML_array {
            array_name = at.at_name;
            array_dims = at.at_dim;
            array_type = (ML_abstract_type {ao with ao_obj=Some (OT_queue qu)});
            });
      | _ -> 
      begin
        add_ao co.co_name ao (ML_abstract_type ao); 
      end;
    end;
    | OT_array at ->
    begin
      match at.at_objs.(0) with
      | OT_signal co | OT_reg co | OT_var co ->
        let at' = match co.co_array with [at'] | [_;at'] -> at'
                  | _ -> err (sprintf "Array object <%s> without parent array <%s> link!" co.co_name at.at_name) in
        let name,name' = at'.at_name,fix_name at'.at_name in
        env_add env at'.at_name  (ML_storage {
          sto_name = name;
          sto_mutable = true;
          sto_export = sym_check_obj modu.mod_export name;
          sto_subst = if name' <> name then name' else "";
          sto_def = None;
          sto_ref = false;
          sto_sym = false;
           sto_type = ML_array {
            array_name = at'.at_name;
            array_dims = at'.at_dim;
            array_type = (
              match co.co_struct with
              | [st] -> 
                  if env_search (env::glob_env) st.st_type.ts_name then
                    env_get (env::glob_env) st.st_type.ts_name
                  else
                    err (sprintf "Unknown structure type <%s> found." st.st_type.ts_name)
              | _ ->
                  ML_type co.co_type);
            }}); 
      | OT_queue qu ->
      begin
        let co = qu.qu_obj in
        let ao = {qu.qu_ao with ao_type={qu.qu_ao.ao_type with ta_name="queue";}} in

        let name,name' = at.at_name,fix_name at.at_name in
        add_ao at.at_name ao (ML_array {
              array_name = at.at_name;
              array_dims = at.at_dim;
              array_type = (ML_abstract_type {ao with ao_obj=Some (OT_queue qu)});
              });
      end;
      | OT_object ao when (ao.ao_type.ta_name <> "process" && ao.ao_type.ta_name <> "function") ->
        let at' = match ao.ao_array with [at'] | [_;at'] -> at'
                  | _ -> err (sprintf "Array object <%s> without parent array <%s> link!" ao.ao_name at.at_name) in
        add_ao at'.at_name ao (ML_array {
            array_name = at'.at_name;
            array_dims = at'.at_dim;
            array_type = ML_abstract_type ao;});
      | _ -> ();     
    end;
    | OT_object ao when (ao.ao_type.ta_name <> "process" && ao.ao_type.ta_name <> "function") ->
      add_ao ao.ao_name ao (ML_abstract_type ao) ;
    | OT_named_value (nv,v) ->
    begin
      let _,_,_,subst = ml_env_transform nv in
      let dt = 
        match v with
        | V_int i64 -> 
          DT_int (const_width v)
        | V_bool _ -> DT_bool
        | V_char _ -> DT_char
        | V_logic s -> DT_logic (String.length s)
        | _ -> err (sprintf "Unsupported named value <%s> data type." nv) in
      let name,name' = 
        if subst = "" then nv,fix_name nv
        else nv,subst in                     
      env_add env nv  (ML_storage {
            sto_name = name;
            sto_subst = if name' <> name then name' else "";
            sto_type = ML_type dt;
            sto_def = Some v;
            sto_ref = false;
            sto_sym = false;
            sto_mutable = false;
            sto_export = false;
            })      
    end;
    | OT_const co ->
    begin
      let _,_,_,subst = ml_env_transform co.co_name in
      let dt = 
        match co.co_init with
        | V_int i64 -> 
          DT_int (const_width co.co_init)
        | V_bool _ -> DT_bool
        | V_char _ -> DT_char
        | V_logic s -> DT_logic (String.length s)
        | _ -> err (sprintf "Unsupported constant value <%s> data type." co.co_name) in
      let name,name' = 
        if subst = "" then 
          co.co_name,(if co.co_type_name = "" then fix_name co.co_name else co.co_name)
        else 
          co.co_name,subst in                     
      env_add env co.co_name  (ML_storage {
            sto_name = name;
            sto_subst = if name' <> name then name' else "";
            sto_type = (if co.co_type_name <> "" then
                          ml_get_type (env::glob_env) co.co_type_name 
                        else ML_type co.co_type);
            sto_def = Some co.co_init;
            sto_ref = false;
            sto_mutable = false;
            sto_sym = co.co_type_name <> "";
            sto_export = false;
            })      
    end;
    | _ -> ();
    ) objs;
  env 

(*
** Return string representation of default/initial value
** for object <obj>
*)
let ml_def  obj =
  let rec def with_ref obj =
    match obj with
    | ML_enum en -> sprintf "%s" (List.hd en.enum_elems)
    | ML_type dt  -> 
    begin
      match dt with
      | DT_logic n -> 
        if n < 31 then 
          "0"
        else 
          "Int64.zero" 
      | DT_int n -> 
        if n < 31 then 
          "0"
        else 
          "Int64.zero"
      | DT_char -> "' '"
      | DT_bool -> "false"
      | _ -> "?"
    end;
    | ML_array at ->
    begin
      sprintf "Array.init %d (fun i -> %s)"
              (proda at.array_dims)
              (def false at.array_type) 
    end;
    | ML_struct st ->
    begin
      let s = ref "{\n" in
      List.iter2 (fun (el,dt) el' ->
          s := sprintf "%s  %s=%s;\n" !s el'
                       (def false (ML_type dt));
        ) st.strut_elems st.strut_names;  
      s := sprintf "%s}" !s;
      !s
    end;
    | ML_abstract_type ao ->
    begin
      match ao.ao_obj with
      | Some (OT_queue qu) -> def false (ML_type qu.qu_obj.co_type)
      | Some (OT_channel ch) -> def false (ML_type ch.ch_obj.co_type)
      | _ -> err (sprintf "Can't provide default value for abstract object <%s>." ao.ao_name);
    end;
    | ML_abstract ab -> def false ab.abstr_type
    | _ -> err ("No rule to map default value.") in
  def true obj

let ml_val  v =
  match v with
  | V_int i64  ->
    let n = const_width v in
    if n < 31 then
      sprintf "%s" (Int64.to_string i64)
    else
      sprintf "(Int64.of_string \"%s\")" (Int64.to_string i64)
  | V_bool b  ->
    if b then "true" else "false"
  | V_char c  ->
    sprintf "'%c'" c
  | V_logic s ->
    if (String.length s) < 31 then
      sprintf "0b%s" s
    else
      sprintf "Int64.of_string \"%s\"" s
  | _ -> err ("No rule to map value.")
 
let ml_expr_type env pi =
  match pi with
  | PI_obj (opl,ot) ->
  begin
    let name = name_of_ot ot in
    if not (env_search env name) then 
    begin
      let _,et = expr_type pi false in
      et      
    end
    else match env_get env name with
    | ML_storage sto ->
    begin
      match sto.sto_type with
      | ML_enum en -> DT_object en.enum_name
      | _ -> 
        let _,et = expr_type pi false in
        et
    end;
    | _ -> 
      let _,et = expr_type pi false in
      et
  end; 
  | _ ->
    let _,et = expr_type pi false in
    et
  
     
(*
** Output all structures and type definitions 
*) 
let ml_emit_types env  =
  let header =  [] in
     
  List.iter out header;
  let objs =list_of_sym env in
  List.iter (fun obj ->
    match obj with
    | ML_struct st ->
      let s = ref (sprintf "type %s = {\n" (fix_name st.strut_name)) in
      List.iter2 (fun (el,dt) el' ->
        s := sprintf "%s  mutable %s: %s;\n" !s (fix_name el)
                    (match dt with
                      | DT_logic n -> if n < 31 then "int" else "int64"
                      | DT_int n  -> if n < 31 then "int" else "int64"
                      | DT_char  -> "char"
                      | DT_bool  -> "bool"
                      | _ -> err "No rule to map structure type."
                    );
                     
        ) st.strut_elems st.strut_names;
      s := sprintf "%s}" !s;
      out !s;
    | ML_enum en ->
      let s = ref (sprintf "type %s = \n" (fix_name en.enum_name)) in
      List.iter (fun el ->
        s := sprintf "%s  | %s\n" !s el;
        ) en.enum_elems;
      out !s;   
    | _ -> ();
    ) objs;
  List.iter (fun obj ->
    match obj with
    | ML_exception exc -> 
      if exc.exc_id <> (-1) then
        out (sprintf "exception %s" exc.exc_name);
    | _ -> ();
    ) objs

(*
** Output all object definitions
*)    
let ml_emit_objs env  local =
  let rec lst2 il =
    match il with
    | (n,v) :: tl -> (sprintf "\"%s\",\"%s\"" n v)^(if tl <> [] then ";" else "")^(lst2 tl)
    | [] -> "" in
    
  let defs = ref [] in
  let objs =list_of_sym env in
  List.iter (fun obj ->
    match obj with
    | ML_storage sto ->
      let is_fun =
          let check_fun_arg = Str.string_match (Str.regexp "ARG_FUN_") sto.sto_name 0 in
          let check_fun_ret_arg = Str.string_match (Str.regexp "RET_FUN_") sto.sto_name 0 in
          check_fun_arg || check_fun_ret_arg in
      let is_excep = Str.string_match (Str.regexp "PRO_.*_EXCEPTION") sto.sto_name 0 in
      let is_array,is_struct =
        match sto.sto_type with
        | ML_array _ -> true,false
        | ML_struct _ -> false,true
        | _ -> false,false in
      if sto.sto_mutable && not is_fun && not is_excep then
        defs := !defs @ [sprintf "let %s%s = %s%s"
                   (if not is_array && not is_struct then "mutable " else "")
                   (if sto.sto_subst <> "" then sto.sto_subst else sto.sto_name)
                   (match sto.sto_def with
                    | Some v -> ml_val  v
                    | None -> (ml_def  sto.sto_type))
                   (if local then " in" else "")];
    | ML_abstract ab ->
    begin
      match ab.abstr_type with
      | ML_abstract_type ao ->
        defs := !defs @ [sprintf "let %s = new %s%s%s%s"
                     (fix_name ab.abstr_name)
                     (ab.abstr_class)
                     (if ab.abstr_env <> [] then sprintf " [%s]" (lst2 ab.abstr_env) else " []")
                     (if local then " in" else "")
                     (if ab.abstr_class = "queue" then sprintf " %s" (ml_def  obj) else "")];
      | ML_array ar ->
        defs := !defs @ [sprintf "let %s = Array.init %d (fun i -> new %s%s%s%s)"
                     (fix_name ar.array_name)
                     (proda ar.array_dims)
                     (ab.abstr_class)
                     (if ab.abstr_env <> [] then sprintf " [%s]" (lst2 ab.abstr_env) else " []")
                     (if local then " in" else "")
                     (if ab.abstr_class = "queue" then sprintf " %s" (ml_def ar.array_type) else "")];
      | _ -> ();
    end;
    | _ -> ();
    ) objs;
  !defs
  

(*
** Output all object declarations
*)    
let ml_emit_objs_int env  local =
  let rec lst2 il =
    match il with
    | (n,v) :: tl -> (sprintf "\"%s\",\"%s\"" n v)^(if tl <> [] then ";" else "")^(lst2 tl)
    | [] -> "" in
  let rec get_type tp =
    match tp with
    | ML_type dt -> 
    begin
      match dt with
      | DT_logic n | DT_int n -> if n < 31 then "int" else "int64"
      | DT_char -> "char"
      | DT_bool -> "bool"
      | _ -> "?"
    end;
    | ML_array ar ->
      sprintf "%s array" (get_type ar.array_type)
    | ML_struct st ->
      st.strut_name
    | _ -> "?"  in
      
  let objs =list_of_sym env in
  List.iter (fun obj ->
    match obj with
    | ML_storage sto ->
      let is_fun =
          let check_fun_arg = Str.string_match (Str.regexp "ARG_FUN_") sto.sto_name 0 in
          let check_fun_ret_arg = Str.string_match (Str.regexp "RET_FUN_") sto.sto_name 0 in
          check_fun_arg || check_fun_ret_arg in
      let is_excep = Str.string_match (Str.regexp "PRO_.*_EXCEPTION") sto.sto_name 0 in
          
      if sto.sto_mutable && not is_fun && not is_excep then
        out (sprintf "val %s : %s%s"
                   (if sto.sto_subst <> "" then sto.sto_subst else sto.sto_name)
                   (get_type sto.sto_type)
                   (if sto.sto_mutable && 
                      (match sto.sto_type with ML_type _ -> true | _ -> false) 
                      then " variable" else ""));
    | ML_abstract ab ->
    begin
      match ab.abstr_type with
      | ML_abstract_type ao ->
        out (sprintf "val %s : %s%s"
                     (fix_name ab.abstr_name)
                     (match ab.abstr_class_type with
                      | "queue" | "channel" -> 
                      begin
                        match ao.ao_obj with
                        | Some (OT_queue qu) -> sprintf "%s " (get_type (ML_type qu.qu_obj.co_type))
                        | Some (OT_channel ch) -> sprintf "%s " (get_type (ML_type ch.ch_obj.co_type))
                        | _ -> ""
                      end;
                      | _ -> "")
                     (ab.abstr_class)
                     );
      | ML_array ar ->
        out (sprintf "val %s : %s%s array"
                     (fix_name ar.array_name)
                     (match ab.abstr_class_type with
                      | "queue" | "channel" -> 
                      begin
                        match ar.array_type with
                        | ML_abstract_type ao ->
                        begin
                          match ao.ao_obj with
                          | Some (OT_queue qu) -> sprintf "%s " (get_type (ML_type qu.qu_obj.co_type))
                          | Some (OT_channel ch) -> sprintf "%s " (get_type (ML_type ch.ch_obj.co_type))
                          | _ -> ""
                        end;
                        | _ -> "?"
                      end;
                      | _ -> "")
                    (ab.abstr_class)
                     );
      | _ -> ();
    end;
    | _ -> ();
    ) objs

(*
** Output all constant definitions
*)    
let ml_emit_const env  local =
  let rec lst2 il =
    match il with
    | (n,v) :: tl -> (sprintf "\"%s\",\"%s\"" n v)^(if tl <> [] then ";" else "")^(lst2 tl)
    | [] -> "" in
    
  let objs =list_of_sym env in
  List.iter (fun obj ->
    match obj with
    | ML_storage sto ->
      if not sto.sto_mutable && sto.sto_def <> None then
        out (sprintf "let %s = %s%s"
                   (if sto.sto_subst <> "" then sto.sto_subst else sto.sto_name)
                   (match sto.sto_def with
                    | Some v -> ml_val  v
                    | None -> (ml_def  sto.sto_type))
                   (if local then " in" else ""));
    | _ -> ();
    ) objs

(*
** Output all constant declarations
*)    
let ml_emit_const_int env  local =
  let rec lst2 il =
    match il with
    | (n,v) :: tl -> (sprintf "\"%s\",\"%s\"" n v)^(if tl <> [] then ";" else "")^(lst2 tl)
    | [] -> "" in
    
  let objs =list_of_sym env in
  List.iter (fun obj ->
    match obj with
    | ML_storage sto ->
      if not sto.sto_mutable && sto.sto_def <> None then
        out (sprintf "val %s : %s"
                   (if sto.sto_subst <> "" then sto.sto_subst else sto.sto_name)
                   (match sto.sto_type with
                    | ML_type dt -> 
                    begin
                      match dt with
                      | DT_logic n | DT_int n -> if n < 31 then "int" else "int64"
                      | DT_char -> "char"
                      | DT_bool -> "bool"
                      | _ -> "?"
                    end;
                    | _ -> "?"));
    | _ -> ();
    ) objs

(*
** Output all model (ADTO...) definitions
*)    
let ml_emit_models env   =
  let objs =list_of_sym env in
  let models = ref [] in
  
  let code = ref [] in
  let fname = "conpro.ml" in
  let pathl = ""::(compiler.t_lib@compiler.t_incl) in        
  let rec f_open pathl =
    match pathl with
    | path :: tl ->
    begin
      try open_in (sprintf "%s%s%s" path (if path <> "" then "/" else "") fname)
      with _ -> f_open tl
    end;
    | [] -> err (sprintf "Can't open file <%s>." fname) in
  let ic = f_open pathl in
  let more = ref true in
  while !more 
  do
    more := protects (code := !code @ [input_line ic]);    
  done;
  close_in ic;
  out "open Unix";
  List.iter out !code;
  
  List.iter (fun obj ->
    match obj with
    | ML_abstract ab ->
      if not (List.mem ab.abstr_class !models) then 
      begin
        List.iter out ab.abstr_code;
        models := !models @ [ab.abstr_class];
      end;
    | _ -> ();
    ) objs

(*
** Output all model (ADTO...) declarations
*)    
let ml_emit_models_int env   =
  let objs =list_of_sym env in
  let models = ref [] in
  
  let fname = "conpro.mli" in
  let pathl = ""::(compiler.t_lib@compiler.t_incl) in        
  let rec f_open pathl fname =
    match pathl with
    | path :: tl ->
    begin
      try open_in (sprintf "%s%s%s" path (if path <> "" then "/" else "") fname)
      with _ -> f_open tl fname
    end;
    | [] -> err (sprintf "Can't open file <%s>." fname) in
  let ic = f_open pathl fname in
  let read ic =
    let code = ref [] in
    let more = ref true in
    while !more 
    do
      more := protects (code := !code @ [input_line ic]);    
    done;
    close_in ic;
    !code
    in
  
  List.iter out (read ic);
  
  List.iter (fun obj ->
    match obj with
    | ML_abstract ab ->
      if not (List.mem ab.abstr_class !models) then 
      begin
        let fname = sprintf "%s.mli" ab.abstr_class_type in
        let ic = f_open pathl fname in
        let in_class = ref false in
        let code = List.filter (fun line -> line <> "") (List.map (fun line ->
            let re2 = Str.regexp " *class *\([a-zA-Z_0-9]+\) *: *" in
            let re21 = Str.regexp " *class \['a\] *\([a-zA-Z_0-9]+\) *: *" in
            let re3 = Str.regexp " *end class *" in
            let p2 = Str.global_replace re2 "" line in
            let p21 = Str.global_replace re21 "" line in
            let p3 = Str.global_replace re3 "" line in
            if p2 <> line then
            begin
              let mclass = Str.global_replace re2 "\1" line in
              if mclass = ab.abstr_class then
              begin
                in_class := true;
                line
              end
              else ""
            end
            else if p21 <> line then
            begin
              let mclass = Str.global_replace re21 "\1" line in
              if mclass = ab.abstr_class then
              begin
                in_class := true;
                line
              end
              else ""
            end
            else if p3 <> line then
            begin
              if !in_class then
              begin
                in_class := false;
                "end"
              end
              else
                ""
            end
            else if !in_class then line
            else ""
            ) (read ic)) in
        
        List.iter out code;
        models := !models @ [ab.abstr_class];
      end;
    | _ -> ();
    ) objs



(*
** Transform expression operation
*)
let rec ml_op et op op1 op2 =
  match et with
  | DT_logic n | DT_int n ->
    if n < 31 then
    begin
      match op with
      | OP_add -> sprintf  "%s + %s" op1 op2
      | OP_sub -> sprintf  "%s - %s" op1 op2
      | OP_mul -> sprintf  "%s * %s" op1 op2
      | OP_div -> sprintf  "%s / %s" op1 op2
      | OP_eq  -> sprintf  "%s = %s" op1 op2
      | OP_neq -> sprintf  "%s <> %s" op1 op2
      | OP_lt  -> sprintf  "%s < %s" op1 op2
      | OP_le  -> sprintf  "%s <= %s" op1 op2
      | OP_gt  -> sprintf  "%s > %s" op1 op2
      | OP_ge  -> sprintf  "%s >= %s" op1 op2
      | OP_land -> sprintf  "%s land %s" op1 op2
      | OP_lor  -> sprintf  "%s lor %s" op1 op2
      | OP_lxor  -> sprintf  "%s lxor %s" op1 op2
      | OP_lsl -> sprintf  "%s lsl %s" op1 op2
      | OP_lsr -> sprintf  "%s lsr %s" op1 op2
      | _ -> "?"
    end
    else
    begin
      match op with
      | OP_add -> sprintf  "Int64.add %s %s" op1 op2
      | OP_sub -> sprintf  "Int64.sub %s %s" op1 op2
      | OP_mul -> sprintf  "Int64.mul %s %s" op1 op2
      | OP_div -> sprintf  "Int64.div %s %s" op1 op2
      | OP_eq  -> sprintf  "%s = %s" op1 op2
      | OP_neq -> sprintf  "%s <> %s" op1 op2
      | OP_lt  -> sprintf  "%s < %s" op1 op2
      | OP_le  -> sprintf  "%s <= %s" op1 op2
      | OP_gt  -> sprintf  "%s > %s" op1 op2
      | OP_ge  -> sprintf  "%s >= %s" op1 op2
      | OP_land -> sprintf  "Int64.logand %s %s" op1 op2
      | OP_lor  -> sprintf  "Int64.logor %s %s" op1 op2
      | OP_lxor  -> sprintf  "Int64.logxor %s %s" op1 op2
      | OP_lsl  -> sprintf  "Int64.shift_left %s %s" op1 op2
      | OP_lsr  -> sprintf  "Int64.shift_right %s %s" op1 op2
      | _ -> "?"
    end;
  | DT_char -> "?"
  | DT_bool ->
    begin
      match op with
      | OP_band -> sprintf  "%s && %s" op1 op2
      | OP_bor  -> sprintf  "%s || %s" op1 op2
      | OP_bxor  -> sprintf  "(%s || %s) && not (%s & %s)" op1 op2 op1 op2
      | OP_eq  -> sprintf  "%s = %s" op1 op2
      | OP_neq -> sprintf  "%s <> %s" op1 op2
      | OP_lt  -> sprintf  "%s < %s" op1 op2
      | OP_le  -> sprintf  "%s <= %s" op1 op2
      | OP_gt  -> sprintf  "%s > %s" op1 op2
      | OP_ge  -> sprintf  "%s >= %s" op1 op2
      | _ -> "?"
    end;    
  | _ -> "?"

(*
** Map structure selector elements to C substitutes...
*)
let ml_sel_map desc = 
  let rec map namel1 namel2 name =
    match namel1,namel2 with
    | (name1::tl1),((name2,_)::tl2) ->
    begin
      if name = name2 then name1 else
        map tl1 tl2 name
    end;
    | _ -> name in
  match desc.desc_obj with
  | ML_storage sto ->
  begin
    match sto.sto_type with
    | ML_struct st ->
       desc.desc_sel
    | ML_array ar ->
    begin
      match ar.array_type with
      | ML_struct st ->
         desc.desc_sel
      | _ -> desc.desc_sel
    end;
    | _ -> desc.desc_sel
  end; 
  | _ -> desc.desc_sel

(*
** Returns string representation of object access (read & write)
*)
let rec ml_obj env  et lhs ot opl =
  let rec index il =
    match il with
    | hd :: tl -> (sprintf ".(%d)" hd)^(index tl)
    | [] -> "" in
  let rec sel il =
    match il with
    | hd :: tl -> (sprintf ".%s" hd)^(sel tl)
    | [] -> "" in
    
  let dt = 
    match dt_of_ot ot with
    | Some dt -> dt
    | None -> err "OT without real DT!" in
  let n = size_of_dt dt in
  
  let conv et desc expr =
    let rec get_dt obj = 
      match obj with
      | ML_storage sto ->
        get_dt sto.sto_type;
      | ML_abstract ab -> 
        get_dt ab.abstr_type;
      | ML_type dt -> dt;
      | ML_abstract_type ao -> 
      begin
        match ao.ao_obj with
        | Some ot -> get_some (dt_of_ot ot)
        | None -> err (sprintf "Abstract object with out real type <%s>." ao.ao_name);
      end;
      | ML_struct st ->
        let _,dt = List.find (fun (sel,_) -> [sel] = desc.desc_sel) st.strut_elems in
        dt
      | ML_array ar ->
        get_dt ar.array_type;
      | ML_enum en -> DT_object en.enum_name
      | _ -> err "Can't determine data type." in 
    let dt = get_dt desc.desc_obj in
    if lhs then expr else match et with
    | DT_logic n ->
    begin
        match dt with
        | DT_int n' | DT_logic n' ->
          if n < 31 && n' < 31 then expr
          else if n < 31 && n' > 30 then sprintf "(Int64.to_int %s)" expr
          else if n > 30 && n' < 31 then sprintf "(Int64.of_int %s)" expr
          else expr
        | DT_char -> 
          if n < 31 then sprintf "(int_of_char %s)" expr
          else sprintf "(Int64.of_int (int_of_char %s))" expr
        | DT_bool ->
          if n < 31 then sprintf "(if %s then 1 else 0)" expr
          else sprintf "(if %s then Int64.one else Int64.zero)" expr
        | _ -> "?"
    end;
    | DT_int n ->
    begin
        match dt with
        | DT_int n'  ->
          if n < 31 && n' < 31 then expr
          else if n < 31 && n' > 30 then sprintf "(Int64.to_int %s)" expr
          else if n > 30 && n' < 31 then sprintf "(Int64.of_int %s)" expr
          else expr
        | DT_logic n' ->
          if n < 31 && n' < 31 then sprintf "(sign %d %s)" n expr
          else if n < 31 && n' > 30 then sprintf "(Int64.to_int %s)" expr
          else if n > 30 && n' < 31 then sprintf "(Int64.of_int %s)" expr
          else expr
        | DT_char -> 
          if n < 31 then sprintf "(int_of_char %s)" expr
          else sprintf "(Int64.of_int (int_of_char %s))" expr
        | DT_bool ->
          if n < 31 then sprintf "(if %s then 1 else 0)" expr
          else sprintf "(if %s then Int64.one else Int64.zero)" expr
        | _ -> "?"
    end;
    | DT_char ->
    begin
        match dt with
        | DT_int n' | DT_logic n' ->
          if n' < 31 then sprintf "(char_of_int %s)" expr
          else sprintf "(char_of_int (Int64.to_int %s))" expr
        | DT_char -> expr
        | DT_bool ->
          sprintf "(if %s then 't' else 'f')" expr
        | _ -> "?"      
    end;
    | DT_bool ->
    begin
        match dt with
        | DT_int n' | DT_logic n' ->
          if n' < 31 then sprintf "(%s = 1)" expr
          else sprintf "(%s = Int64.one)" expr
        | DT_char -> sprintf "(%s = 't')" expr
        | DT_bool -> expr
        | _ -> "?"      
    end;
    | DT_object n1 ->
    begin
      match dt with
      | DT_object n2 -> 
        if n1 <> n2 then err (sprintf "Expected type <%s>, but got <%s>." n1 n2);
        expr
      | _ -> "?"
    end;
    | _ -> "?" in
  
  let sub =
    if is_sub opl then
    begin
      let a,b = obj_sub opl in
      sprintf "%d %d" a b
    end 
    else if is_index opl then
    begin
      let a = obj_index opl in
      sprintf "%d %d" a a
    end 
    else if is_index_obj opl then
    begin
      match obj_index_obj opl with
      | PI_obj (_,ot) ->
        ml_obj env  (DT_int 0) false ot []
      | _ -> "?"
    end
    else "" in    
    
  let neg =
    let rec is_neg opl = 
      match opl with 
      | op :: tl -> if op = OD_aneg || op = OD_lneg then true else is_neg tl
      | [] -> false in
    let rec obj_neg opl = 
      match opl with 
      | op :: tl -> if op = OD_aneg || op = OD_lneg then op else obj_neg tl
      | [] -> progerr "obj_neg" in
      
    if is_neg opl then
    begin
      match obj_neg opl with
      | OD_lneg -> if dt = DT_bool then "not" else  "lnot"
      | OD_aneg -> if n > 30 then "Int64.neg" else "-"
      | _ -> progerr "obj_neg"
    end
    else "" in
 
  match ot with
  | OT_signal co
  | OT_var co 
  | OT_reg co 
  | OT_const co -> 
  begin
    let desc = ml_obj_desc env co.co_name co.co_array co.co_struct in
    conv et desc (
    if not desc.desc_array && not desc.desc_struct && desc.desc_mutable then
      sprintf "%s%s%s%s%s%s%s"
        (if neg <> "" then sprintf "(%s" neg else "") 
        (if sub <> "" && not lhs then sprintf "(%s %s " (if n < 31 then "bit" else "bit64") sub 
         else "")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
        (if sub <> "" && not lhs then ")" else "")
        (if lhs then " <-" else "")
        (if sub <> "" && lhs then
           sprintf " %s %s !%s" 
              (if n < 31 then "bitw" else "bitw64")
              sub 
              (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
         else "")
        (if neg <> "" then ")" else "")
    else if not desc.desc_array && not desc.desc_struct && not desc.desc_mutable then 
      sprintf "%s%s%s%s%s"
        (if neg <> "" then sprintf "(%s" neg else "") 
        (if sub <> "" && not lhs then sprintf "(%s %s " (if n < 31 then "bit" else "bit64") sub 
         else "")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
        (if sub <> "" then ")" else "")
        (if neg <> "" then ")" else "")
    else if desc.desc_array && not desc.desc_struct then
      sprintf "%s%s%s%s%s%s%s%s"  
        (if neg <> "" then sprintf "(%s" neg else "") 
        (if sub <> "" && not lhs then sprintf "(%s %s " (if n < 31 then "bit" else "bit64") sub 
         else "")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (index desc.desc_num_sel)
        (if sub <> "" && not lhs then ")" else "")
        (if lhs then " <-" else "")
        (if sub <> "" && lhs then
           sprintf " %s %s %s%s" sub 
           (if n < 31 then "bitw" else "bitw64")
           (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
           (index desc.desc_num_sel)
         else "")
        (if neg <> "" then ")" else "")
    else if not desc.desc_array && desc.desc_struct then
      sprintf "%s%s%s%s%s%s%s%s"  
        (if neg <> "" then sprintf "(%s" neg else "") 
        (if sub <> "" && not lhs then sprintf "(%s %s " (if n < 31 then "bit" else "bit64") sub 
         else "")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (sel (ml_sel_map desc))
        (if sub <> "" && not lhs then ")" else "")
        (if lhs then " <-" else "")
        (if sub <> "" && lhs then
           sprintf " %s %s %s%s" sub 
           (if n < 31 then "bitw" else "bitw64")
           (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
           (sel (ml_sel_map desc))
         else "")
        (if neg <> "" then ")" else "")
    else err "No rule to map object.");
  end;
  | OT_queue qu ->
  begin
    let co = qu.qu_obj in
    let desc = ml_obj_desc env co.co_name co.co_array co.co_struct in
    conv et desc (
    if not desc.desc_array && not desc.desc_struct && desc.desc_mutable then 
      sprintf "%s%s" 
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
        (if lhs then "#write" else "#read")
    else if not desc.desc_array && not desc.desc_struct && not desc.desc_mutable then
      sprintf "%s%s"
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
        (if lhs then "#write" else "#read")
    else if desc.desc_array && not desc.desc_struct then
      sprintf "%s%s%s"  
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (index desc.desc_num_sel)
        (if lhs then "#write" else "#read")
    else if not desc.desc_array && desc.desc_struct then
      sprintf "%s%s%s"  
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        (sel (ml_sel_map desc))
        (if lhs then "#write" else "#read")
    else err "No rule to map object."); 
  end;  
  | OT_array at ->
  begin
    match at.at_objs.(0) with
    | OT_reg _ | OT_var _ | OT_signal _ ->
      let desc = ml_obj_desc env at.at_name [at] [] in
      let at_sel = 
        if is_sel opl then
          (index (Array.to_list (obj_sel opl)))
        else if is_sel_obj opl then
          sprintf ".(%s)" (ml_expr env  false (DT_int 0) (obj_sel_obj opl))
        else "?"  in
      conv et desc (sprintf "%s%s%s%s%s%s%s%s"
        (if neg <> "" then sprintf "(%s" neg else "") 
        (if sub <> "" && not lhs then sprintf "(bit %s " sub 
         else "")
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
        at_sel
        (if desc.desc_struct then (sel (ml_sel_map desc)) else "")
        (if lhs then " <-" else "")
        (if sub <> "" && lhs then
           sprintf " bitw %s %s%s" sub 
           (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name)
           at_sel
         else "")
         (if neg <> "" then ")" else ""))
    | OT_queue qu ->
      let desc = ml_obj_desc env at.at_name [at] [] in
      let at_sel = 
        if is_sel opl then
          (index (Array.to_list (obj_sel opl)))
        else if is_sel_obj opl then
          sprintf ".(%s)" (ml_expr env  false (DT_int 0) (obj_sel_obj opl))
        else "?"  in
      conv et desc (sprintf "%s%s%s%s" 
        (if desc.desc_subst <> "" then desc.desc_subst else desc.desc_name) 
        at_sel
        (if desc.desc_struct then (sel (ml_sel_map desc)) else "")
        (if lhs then "#write" else "#read"))
    | OT_object ao -> "?#?"
    | _ -> err (sprintf "Unexpected array element type found <%s>" at.at_name) 
  end;
  | OT_value v ->
  begin
    match v with
    | V_int i64 ->
    begin
      match et with
      | DT_logic n | DT_int n | DT_natural n ->
        if n < 31 then sprintf "%s%s%s" 
            (if neg <> "" then sprintf "(%s" neg else "")
            (Int64.to_string i64) 
            (if neg <> "" then ")" else "") 
        else if i64 = Int64.one then sprintf "%s%s%s" 
            (if neg <> "" then sprintf "(%s" neg else "")
             "Int64.one" 
            (if neg <> "" then ")" else "")
        else if i64 = Int64.zero then sprintf "%s%s%s" 
            (if neg <> "" then sprintf "(%s" neg else "")
            "Int64.zero"
            (if neg <> "" then ")" else "")            
        else sprintf "%s(Int64.of_string \"%s\")%s" 
            (if neg <> "" then sprintf "(%s" neg else "")
            (Int64.to_string i64)
            (if neg <> "" then ")" else "")            
      | DT_char ->
        sprintf "'%c'" (char_of_int (Int64.to_int i64))
      | DT_bool ->
        if i64=Int64.one then "true" else "false"
      | _ -> "?"
    end;
    | V_bool b ->
    begin
      match et with
      | DT_logic n | DT_int n | DT_natural n ->
        if n < 31 then (if b then "1" else "0") else 
                       (if b then "Int64.one" else "Int64.zero")
      | DT_char ->
        if b then "'t'" else "'f'"
      | DT_bool ->
        if b then "true" else "false"
      | _ -> "?"      
    end;
    | V_char c ->
    begin
      match et with
      | DT_logic n | DT_int n |  DT_natural n ->
        if n < 31 then sprintf "%d" (int_of_char c) else 
                       sprintf "(Int64.of_string \"%s\")" (Int64.to_string
                       (Int64.of_int (int_of_char c)))
      | DT_char ->
        sprintf "'%c'" c
      | DT_bool ->
        if c='t' then "true" else "false"
      | _ -> "?"      
    end;
    | V_logic s ->
    begin
      let n' = String.length s in
      match et with
      | DT_logic n | DT_int n | DT_natural n ->
        if n < 31 then sprintf "0b%s" s 
        else sprintf "(Int64.of_string 0b%s)" s
      | DT_char ->
        sprintf "(char_of_int 0b%s)" s
      | DT_bool ->
        sprintf "(0b%s=1)" s
      | _ -> "?"
    end;
    | _ -> "?"
  end;
  | OT_named_value (nv,v) ->
  begin
    match v with
    | V_int i64 ->
    begin
      let n' = const_width v in
      match et with
      | DT_logic n | DT_int n | DT_natural n ->
        if n < 31 && n' > 30 then sprintf "(Int64.to_int %s)" (fix_name nv) 
        else if n > 30 && n' < 31 then sprintf "(Int64.of_int %s)" (fix_name nv) 
        else (fix_name nv)
      | DT_char ->
        if n' < 31 then sprintf "(char_of_int %s)" (fix_name nv)
        else sprintf "(char_of_int (Int64.to_int %s))" (fix_name nv)
      | DT_bool ->
        sprintf "(%s=Int64.one)" (fix_name nv)
      | _ -> "?"
    end;
    | V_bool b ->
    begin
      match et with
      | DT_logic n | DT_int n | DT_natural n ->
        if n < 31 then (sprintf "(if %s then 1 else 0)" (fix_name nv)) else 
                       (sprintf "(if %s then  Int64.one else Int64.zero)" (fix_name nv))
      | DT_char ->
        (sprintf "(if %s then 't' else 'f')" (fix_name nv)) 
      | DT_bool ->
        (fix_name nv)
      | _ -> "?"      
    end;
    | V_char c ->
    begin
      match et with
      | DT_logic n | DT_int n |  DT_natural n ->
        if n < 31 then sprintf "(int_of_char %s)" (fix_name nv) else 
                       sprintf "(Int64.of_int (int_of_char %s))" (fix_name nv)
      | DT_char ->
        (fix_name nv)
      | DT_bool ->
        sprintf "(if %s='t' then true else false)" (fix_name nv)
      | _ -> "?"      
    end;
    | V_logic s ->
    begin
      let n' = String.length s in
      match et with
      | DT_logic n | DT_int n | DT_natural n ->
        if n < 31 && n' > 30 then sprintf "(Int64.to_int %s)" (fix_name nv) 
        else if n > 30 && n' < 31 then sprintf "(Int64.of_int %s)" (fix_name nv) 
        else (fix_name nv)
      | DT_char ->
        if n' < 31 then sprintf "(char_of_int %s)" (fix_name nv)
        else sprintf "(char_of_int (Int64.to_int %s))" (fix_name nv)
      | DT_bool ->
        sprintf "(%s=Int64.one)" (fix_name nv)
      | _ -> "?"
    end;
    | _ -> "?"
  end;
  | _ -> "OT?"

(*
** Returns string representation of (nested) expression
*)    
and ml_expr env  lhs et pi =
  match pi with
  | PI_obj (opl,ot) -> ml_obj env  et lhs ot opl
  | PI_arithm (op,op1,op2) ->
    let is_expr op = match op with | PI_arithm _ | PI_bool _ -> true|_ -> false in
    let par = (is_expr op1) || (is_expr op2) in
    sprintf "%s%s%s"
      (if par then "(" else "")
      (ml_op et op
        (ml_expr env  false et op1) 
        (ml_expr env  false et op2))
      (if par then ")" else "")
  | PI_bool (kind,op,op1,op2) -> 
    let is_expr op = match op with | PI_arithm _ | PI_bool _ -> true|_ -> false in
    let par = (is_expr op1) || (is_expr op2) in
    let et = 
      match kind with
      | Cp_syntax.Bool -> DT_bool
      | Cp_syntax.Relational -> let et = ml_expr_type env pi in et in
       
    sprintf "%s%s%s"
      (if par then "(" else "")
      (ml_op et op
        (ml_expr env  false et op1) 
        (ml_expr env  false et op2))
      (if par then ")" else "")
  | _ -> "?"    

(*
** Transform process instructions, returns
** string representation list
*)

let rec ml_instr envl  local indent pi =
  let rec index il =
    match il with
    | hd :: tl -> (sprintf ".(%d)" hd)^(index tl)
    | [] -> "" in
  let rec sel il =
    match il with
    | hd :: tl -> (sprintf ".%s" hd)^(sel tl)
    | [] -> "" in
  let rec args il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then " " else "")^(args tl)
    | [] -> "" in
  let rec cargs il =
    match il with
    | hd :: tl -> ("| "^hd)^(if tl <> [] then " " else "")^(args tl)
    | [] -> "" in
  let rec args' il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "," else "")^(args' tl)
    | [] -> "" in

  (*
  ** Remove nested blocks of single instructions
  *)
  let rec flatten pil =
    match pil with
    | [PI_block ([i],bf)] -> (flatten [i])
    | [PI_block (il,bf)] -> [PI_block (flatten il,bf)]
    | (PI_block ([i],_)) :: tl  -> (flatten [i])@(flatten tl)
    | (PI_block (il,bf)) :: tl  -> [PI_block (flatten il,bf)]@(flatten tl)
    | pi :: tl -> [pi]  @ (flatten tl) 
    | [] -> [] in
    
  
  let rec filter pil =
    let pil' = flatten pil in
    (*
    ** Search for user function call patterns....
    *)
    match pil' with
    | (PI_block (il,bf)) :: tl -> (filter (flatten il)) @ (filter tl);
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2;arg3] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_assign (_,_,arg4)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2;arg3;arg4] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_assign (_,_,arg4)) :: 
      (PI_assign (_,_,arg5)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2;arg3;arg4;arg5] [] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2;arg3] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_assign (_,_,arg4)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2;arg3;arg4] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_assign (_,_,arg4)) :: 
      (PI_assign (_,_,arg5)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2;arg3;arg4;arg5] [r1] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_assign (_,r2,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [] [r1;r2] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_assign (_,r2,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1] [r1;r2] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_assign (_,r2,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2] [r1;r2] b) @ (filter tl)
    | (PI_fun (_,_,"lock",_)) :: 
      (PI_assign (_,_,arg1)) :: 
      (PI_assign (_,_,arg2)) :: 
      (PI_assign (_,_,arg3)) :: 
      (PI_fun (a,b,"call",c)) :: 
      (PI_assign (_,r1,_)) :: 
      (PI_assign (_,r2,_)) :: 
      (PI_fun (_,_,"unlock",_)) :: tl ->
      (ml_emit_fun_call envl local indent [arg1;arg2;arg3] [r1;r2] b) @ (filter tl)
    | pi :: tl  -> (ml_instr envl local indent pi) @ (filter tl)
    | [] -> [] in

  match pi with
  | PI_assign (src,lhs,rhs) ->
  begin
    line src;
    let et = ml_expr_type envl lhs in
    let lhs' = 
      match lhs with
      | PI_obj (opl,ot) -> ml_obj envl  et true ot opl
      | _ -> err "Unexpected LHS object found." in
   [ibox indent (sprintf "%s %s;" lhs'  (ml_expr envl  false et rhs));]
  end;
  | PI_block (il,bf) -> 
    filter il
  | PI_list il -> List.flatten (List.map (ml_instr envl  local indent) il)
  | PI_forloop (src,expr,dir,lim1,lim2,block) ->
    line src;
    [
      ibox indent (sprintf "for %s = %s %s %s do"
              (ml_expr envl  false (DT_int 0) expr)
              (ml_expr envl  false (DT_int 0) lim1)
              (if dir = 'd' then "downto" else "to")
              (ml_expr envl  false (DT_int 0) lim2))
    ]@(ml_instr envl local (indent+2) block)@
    [
      ibox indent "done;"
    ]
  | PI_loop (src,Cp_syntax.Loop,expr,block)
  | PI_loop (src,Cp_syntax.Loop_while,expr,block) ->
    line src;
    let expr = 
      match expr with
      | PI_nop -> "true"
      | _ -> ml_expr envl  false DT_bool expr in
    [
      ibox indent (sprintf "while %s do" expr)
    ]@(ml_instr envl  local (indent+2)block)@
    [
      ibox indent "done;"
    ]
  | PI_branch (src,expr,b1,b2) ->
    line src;
    let expr = ml_expr envl  false DT_bool expr in
    [
      ibox indent (sprintf "if %s then" expr);
      ibox indent "begin";
    ]@(ml_instr envl  local (indent+2) b1)@
    (match b2 with
     | PI_nop | PI_block ([PI_nop],_) -> [ibox indent "end;"]
     | _ ->
     begin
      [
        ibox indent "end";
        ibox indent "else";
        ibox indent "begin";
      ]@(ml_instr envl  local (indent+2) b2)@
      [
        ibox indent "end;"
      ]
     end)
  | PI_select (src,expr,casel) ->
    line src;
    let et = ml_expr_type envl expr in
    ml_et := et :: !ml_et;
    let expr = ml_expr envl  false (List.hd !ml_et) expr in
    let complete =
      match casel with
      | PI_list cl | PI_block (cl,_) ->
        let rec iter el =
          match el with
          | (PI_case (_,[],_)) :: tl -> true;
          | _ :: tl -> iter tl
          | [] -> false in
        iter cl
      | _ -> true in
    let l = [
      ibox indent (sprintf "(match %s with" expr);
    ]@(ml_instr envl  local indent casel)@
    (if not complete then [ibox indent "| _ -> progerr \"unmatched value\""] else [])@
    [
      ibox indent ");"
    ] in
    ml_et := List.tl !ml_et;
    l
  | PI_case (src,exprl,b) ->
    line src;
    let exprl = 
      if not !ml_exc_map && exprl <> [] then
        List.map (ml_expr envl  false (List.hd !ml_et)) exprl
      else if exprl <> [] then
        List.map (fun s ->
          let id = int_of_string s in
          (ml_get_exc envl id).exc_name
          ) (List.map (ml_expr envl  false (List.hd !ml_et)) exprl)
      else ["_"];
       in
   [
      ibox indent (sprintf "%s ->" (cargs exprl));
      ibox indent "begin";
    ]@(ml_instr envl  local (indent+2) b)@
    [
      ibox indent "end;"
    ] 
    
  | PI_try (b,casel) ->
    let b' = ml_instr envl  local (indent+2) b in
    let complete =
      match casel with
      | PI_list cl | PI_block (cl,_) ->
        let rec iter el =
          match el with
          | (PI_case (_,[],_)) :: tl -> true;
          | _ :: tl -> iter tl
          | [] -> false in
        iter cl
      | _ -> true in
    let casel' = 
      ml_exc_map := true; 
      ml_et := (DT_int 0):: !ml_et;
      let l = ml_instr envl  local indent casel in
      ml_exc_map := false; 
      ml_et := List.tl !ml_et;
      l in
    let l = [
      ibox indent "(try begin";
    ]@b'@
    [
      ibox indent "end with"
    ]@casel'@
    [
      ibox indent ");";
    ] in
    l
  | PI_raise exn ->
  begin
    [
      ibox indent (sprintf "raise %s;" (ml_get_exc envl exn).exc_name );
    ]
  end;
  | PI_waitfor (src,expr,n,tu,ef,et) ->
    line src;
    if n > 0 then
      [
        sprintf "delay %d;" (
          match tu with
          | Nsec -> max 1 (n/1000)
          | Usec -> n 
          | Msec -> n*1000
          | Sec -> n*1000000
          | Cycles -> n
          );
      ]
    else
      [(
        let expr = 
          match expr with
          | PI_nop -> "true"
          | _ -> ml_expr envl  false DT_bool expr in
        ibox indent (sprintf "while not (%s) do (); done;" expr)
      )] 
  | PI_fun (src,(opl,ot),sel,argl) ->
  begin
    line src;
    let ao_instr ao name at_sel =
      let _,descl = List.find (fun (sel',_) -> sel=sel') ao.ao_type.ta_rules.rl_methods in
      let argl' = List.map2 (fun arg desc -> arg,desc) argl descl in
      let argl_lhs = List.filter (fun (arg,desc) -> (desc.arg_type = Arg_lhs) || (desc.arg_type = Arg_lrhs)) argl' in
      let argl_rhs = List.filter (fun (arg,desc) -> desc.arg_type = Arg_rhs) argl' in 
      let argl_lhs' = List.map (fun (arg,desc) ->
        let et = desc.arg_data_type in
        ml_expr envl  true et arg        
        ) argl_lhs in    
      let argl_rhs' = List.map (fun (arg,desc) ->
        let et = desc.arg_data_type in
        ml_expr envl  false et arg
        ) argl_rhs in  
      let resl = 
        if (List.length argl_lhs') > 1 then
        begin 
          let n = ref 0 in
          List.map (fun arg -> incr n; sprintf "r_%d" !n) argl_lhs'
        end
        else [] in
      
      List.map (ibox indent) ([
        sprintf "%s%s#%s%s%s" 
         (if resl <> [] then  sprintf "let %s = " (args' resl)  else 
          if argl_lhs' <> [] then sprintf "%s " (List.hd argl_lhs') else "")
         (if at_sel = "" then name else sprintf "%s%s" name at_sel) 
         sel 
         (if argl_rhs' <> [] then sprintf "(%s)" (args' argl_rhs') else "()")
         (if resl <> [] then " in" else ";");
      ] @
      (if resl <> [] then 
        List.map2 (fun r arg ->
           sprintf "%s %s;" arg r
          ) resl argl_lhs'
       else [])) in
      
    match ot with
    | OT_object ao when (ao.ao_type.ta_name = "function") -> 
      let name = Str.global_replace (Str.regexp "function_\(.+\)") "\1" ao.ao_name in
      ao_instr ao name ""
    | OT_object ao ->
      ao_instr ao ao.ao_name ""
    | OT_array at ->
    begin
      let at_sel = 
        if is_sel opl then
          (index (Array.to_list (obj_sel opl)))
        else if is_sel_obj opl then
          sprintf ".(%s)" (ml_expr envl  false (DT_int 0) (obj_sel_obj opl))
        else "?"  in
      let name =
        match at.at_objs.(0) with
        | OT_object ao when (ao.ao_type.ta_name = "function") ->
         Str.global_replace (Str.regexp "function_\(.+\)") "\1" at.at_name 
        | _ -> at.at_name in
      ao_instr (match at.at_objs.(0) with 
               | OT_object ao -> ao
               | OT_queue qu -> qu.qu_ao
               | _ -> err (sprintf "Invalid abstract object array <%s>" at.at_name))
               name at_sel
    end;
    | _ -> ["?#?;"];
  end;  
  | _ -> []
and ml_emit_fun_call envl local indent args rets func =
  let rec fargs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "," else "")^(fargs tl)
    | [] -> "" in
  let opl,ot = func in
  let fname = Str.global_replace (Str.regexp "FUN_\(.+\)") "\1" (name_of_ot ot) in
  let f = 
    match env_get envl fname with
    | ML_fun f-> f
    | _ -> error 0 ("ml_emit_fun_call: function environment name expected") in
  let args = fargs (List.map2 (fun arg (_,co) ->
    let et = co.co_type in
    ml_expr envl false et arg
    ) args f.fn_args) in
  let retl = fargs (List.map2 (fun arg (_,co) ->
    let et = co.co_type in
    sprintf "%s" (ml_expr envl false et arg)
    ) rets f.fn_rets) in
  let retl' = fargs (List.map2 (fun arg (_,co) ->
    let et = co.co_type in
    sprintf "%s'" (ml_expr envl false et arg)
    ) rets f.fn_rets) in
  match rets with
  | [] ->
    [ibox indent (sprintf "%s(%s);" fname args)]
  | [ret] ->
    [ibox indent (sprintf "%s <- %s(%s);" retl fname args)]
  | _ ->
    [ibox indent (sprintf "let %s = %s(%s) in" retl' fname args)]@(
      List.map2 (fun arg (_,co) ->
          let et = co.co_type in
          let r = ml_expr envl false et arg in        
          ibox indent (sprintf "%s <- %s';" r r);
        ) rets f.fn_rets;
    )
  
and ml_emit_instr envl  local pil =
  let instrl = ml_instr envl local 0 (PI_block(pil,Cp_block_frame.nilbf)) in
  List.iter out !ml_defs;
  List.iter out !ml_evals;
  List.iter out instrl



(*
** Extract process instruction depenendencies (function/process calls)
*)
let rec ml_instr_dep env  local  pi =
  match pi with
  | PI_assign (src,lhs,rhs) ->
  begin
    line src;
    []
  end;
  | PI_block (il,bf) -> 
    List.flatten (List.map (ml_instr_dep env  local) il)
  | PI_list il -> List.flatten (List.map (ml_instr_dep env local) il)
  | PI_forloop (src,expr,dir,lim1,lim2,block) ->
    line src;
    ml_instr_dep env  local block
  | PI_loop (src,Cp_syntax.Loop,expr,block)
  | PI_loop (src,Cp_syntax.Loop_while,expr,block) ->
    line src;
    ml_instr_dep env local block 
  | PI_branch (src,expr,b1,b2) ->
    line src;
    (ml_instr_dep env  local b1)@
    (match b2 with
     | PI_nop | PI_block ([PI_nop],_) -> []
     | _ ->
     begin
       ml_instr_dep env local b2
     end)
  | PI_select (src,expr,casel) ->
    line src;
    ml_instr_dep env  local casel
  | PI_case (src,exprl,b) ->
    line src;
    ml_instr_dep env  local  b
  | PI_try (b,casel) ->
    (ml_instr_dep env  local b)@ 
    (ml_instr_dep env  local casel)
  | PI_raise exn -> []
  | PI_waitfor (src,expr,n,tu,ef,et) ->
    line src;
    []
  | PI_fun (src,(opl,ot),sel,argl) ->
  begin
    line src;
      
    match ot with
    | OT_object ao when (ao.ao_type.ta_name = "function") -> 
      let name = Str.global_replace (Str.regexp "function_\(.+\)") "\1" ao.ao_name in
      [sprintf "FUN_%s" name]
    | OT_object ao when (ao.ao_type.ta_name = "process") -> 
      let name = Str.global_replace (Str.regexp "function_\(.+\)") "\1" ao.ao_name in
      [name]
    | OT_array at ->
    begin
      match at.at_objs.(0) with
      | OT_object ao when (ao.ao_type.ta_name = "function") ->
       [sprintf "FUN_%s" (Str.global_replace (Str.regexp "function_\(.+\)") "\1" at.at_name)]
      | OT_object ao when (ao.ao_type.ta_name = "process") ->
       [Str.global_replace (Str.regexp "function_\(.+\)") "\1" at.at_name ]
      | _ -> []
    end;
    | _ -> [];
  end;  
  | _ -> []
  
let ml_dep_instr env  local pil =
  List.flatten (List.map (ml_instr_dep env  local) pil)
  
(*
** Transform and emit a module
*)
let ml_emit modu =
  let main_defs = ref [] in
  let main_init = ref [] in
  ml_mod_dep := [];

  let rec args il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then " " else "")^(args tl)
    | [] -> "" in
  let rec targs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "," else "")^(targs tl)
    | [] -> "" in
  let rec largs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then "; " else "")^(largs tl)
    | [] -> "" in
  let rec fiargs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then " -> " else "")^(fiargs tl)
    | [] -> "" in
  let rec ftargs il =
    match il with
    | hd :: tl -> (hd)^(if tl <> [] then " * " else "")^(ftargs tl)
    | [] -> "" in

  let env_merge env1 env2 =
    let env1 = Hashtbl.copy env1 in
    Hashtbl.iter (fun k s ->
      Hashtbl.add env1 k s ) env2;
    env1 in
      
  let create fname =
   (try
     begin
      oc := open_out (sprintf "%s%s.ml" compiler.t_output fname);
      out_ (sprintf "Creating <%s.ml>..." fname);
     end
    with
    _ -> err (sprintf "Can't open file <%s%s.ml>." compiler.t_output fname)) in
    
  let finish fname =
    close_out !oc in

  let create_make fname =
   (try
     begin
      oc := open_out (sprintf "%s%s" compiler.t_output fname);
      out_ (sprintf "Creating <%s>..." fname);
     end
    with
    _ -> err (sprintf "Can't open file <%s%s>." compiler.t_output fname)) in
    
  let finish_make fname =
    close_out !oc in

  let create_int fname =
   (try
     begin
      oc := open_out (sprintf "%s%s.mli" compiler.t_output fname);
      out_ (sprintf "Creating <%s.mli>..." fname);
     end
    with
    _ -> err (sprintf "Can't open file <%s%s.mli>." compiler.t_output fname)) in
    
  let finish_int fname =
    close_out !oc in
    
  out_ (sprintf "Synthesis of ML model for module <%s>..." modu.mod_name);
  Cp_common.ind_incr ();
  
  let level = ML_level_high in

  let glob_env = ml_env [] modu modu.mod_objs in
  let defs = ml_emit_objs glob_env  false in
  main_defs := !main_defs @ defs;


  (*
  ** Process interface
  *)
  List.iter (fun pro ->
    let env = ml_env [glob_env] modu pro.pro_objs in
    let is_fun,fun_name,fun_def = 
      if Str.string_match (Str.regexp "FUN_.+") pro.pro_name 0 then
      begin
        let fname = Str.global_replace (Str.regexp "FUN_\(.+\)") "\1" pro.pro_name in
        if sym_check_fun modu.mod_objs fname then
          true,fname,Some (sym_get_fun modu.mod_objs fname)
        else false,"",None
      end
      else
        false,"",None in
    let is_array,aname,i,size =
      if sym_check_obj modu.mod_objs pro.pro_name then
        false,"",(-1),0
      else 
      begin
        let name = Str.global_replace (Str.regexp "\(.+\)_[0-9]+") "\1" pro.pro_name in
        match sym_get_obj modu.mod_objs name with
        | OT_array at -> 
          true,
          name,
          int_of_string (Str.global_replace (Str.regexp ".+_\([0-9]+\)") "\1" pro.pro_name),
          proda at.at_dim
        | OT_object ao -> false,"",(-1),0
        | _ -> false,"",(-1),0
      end in
    let fname = 
      if not is_array then sprintf "%s_%s" (of_mod modu.mod_name) pro.pro_name 
      else sprintf "%s_%s" (of_mod modu.mod_name) aname in
    if not is_array || (is_array && i = 0) then 
    begin
      create_int fname;  
      if not (List.mem "Core" !ml_mod_dep) then ml_mod_dep := !ml_mod_dep @ ["Core"];
      if not (List.mem "process" !ml_mod_dep) then ml_mod_dep := !ml_mod_dep @ ["process"];
      if not (List.mem "event" !ml_mod_dep) then ml_mod_dep := !ml_mod_dep @ ["event"];
      if is_fun && not (List.mem "mutex" !ml_mod_dep) then ml_mod_dep := !ml_mod_dep @ ["mutex"];
      out (sprintf "open %s_types" (to_mod modu.mod_name));
      out (sprintf "open %s_const" (to_mod modu.mod_name));
      out (sprintf "open %s_models" (to_mod modu.mod_name));
      out (sprintf "open %s_objs" (to_mod modu.mod_name));
      
      let deps = ml_dep_instr env false pro.pro_instr in
      let deps' = ref [] in
      List.iter (fun dep ->
        if not (List.mem dep !deps') then
          out (sprintf "#include \"%s_%s.h\"" (of_mod modu.mod_name) dep);
        deps' := dep :: !deps';
        ) deps;
    

      let fargs =
        if is_fun  then
        begin
          let f = get_some fun_def in
          sprintf "%s%s" 
            (if f.fun_args_obj = [] then "unit"
             else sprintf "%s" (ftargs (List.map2 (fun a co -> 
                  sprintf "%s" (match co.co_type with
                  | DT_int n | DT_logic n -> if n < 31 then "int" else "int64"
                  | DT_char -> "char"
                  | DT_bool -> "bool"
                  | _ -> "?")) f.fun_args f.fun_args_obj)))
            (if f.fun_ret_obj = [] then " -> unit"
             else sprintf " -> %s" 
              (ftargs (List.map2 (fun r co -> 
                  sprintf "%s" (match co.co_type with
                  | DT_int n | DT_logic n -> if n < 31 then "int" else "int64"
                  | DT_char -> "char"
                  | DT_bool -> "bool"
                  | _ -> "?")) f.fun_ret f.fun_ret_obj)));
        end
        else "" in
      if not is_array then
      begin
        if is_fun  then
          out (sprintf "val %s: %s"  (fix_name fun_name) fargs)   
        else
        begin
          out (sprintf "val pro_%s: int -> unit"   (fix_name pro.pro_name));   
          out (sprintf "val %s: process"   (fix_name pro.pro_name));   
        end;
      end
      else
      begin
        if is_fun  then
          out (sprintf "val %s : %s"  (fix_name fun_name) fargs)   
        else
        begin
          out (sprintf "val pro_%s: int -> unit"   (fix_name aname));   
          for i = 0 to size-1 
          do
            out (sprintf "val %s_%d: process"   (fix_name aname) i);   
          done;
          out (sprintf "val %s: process array"   (fix_name aname));   
        end;
      end; 
      
      finish_int fname;
    end;
    ) modu.mod_procs;

  let prol = ref [] in


  (*
  ** Process implementation
  *)
  List.iter (fun pro ->
    let env = ml_env [glob_env] modu pro.pro_objs in
    
    let is_fun,is_inline,fun_name,fun_def = 
      if Str.string_match (Str.regexp "FUN_.+") pro.pro_name 0 then
      begin
        let fname = Str.global_replace (Str.regexp "FUN_\(.+\)") "\1" pro.pro_name in
        if sym_check_fun modu.mod_objs fname then
        begin
          let f = sym_get_fun modu.mod_objs fname in
          true,f.fun_inline,fname,Some f
        end
        else false,false,"",None
      end
      else
        false,false,"",None in
      
    let is_array,aname,i,size =
      if sym_check_obj modu.mod_objs pro.pro_name then
        false,"",(-1),0
      else 
      begin
        let name = Str.global_replace (Str.regexp "\(.+\)_[0-9]+") "\1" pro.pro_name in
        match sym_get_obj modu.mod_objs name with
        | OT_array at -> 
          true,
          name,
          int_of_string (Str.global_replace (Str.regexp ".+_\([0-9]+\)") "\1" pro.pro_name),
          proda at.at_dim
        | OT_object ao -> false,"",(-1),0
        | _ -> false,"",(-1),0
      end in
    let fname = 
      if not is_array then sprintf "%s_%s" (of_mod modu.mod_name) pro.pro_name 
      else sprintf "%s_%s" (of_mod modu.mod_name) aname in
    let pname = 
      if not is_array then pro.pro_name 
      else  aname in
    if not is_array || (is_array && i = 0) then 
    begin
      create fname;  

      out "open Thread";
      out "open Event";
      out (sprintf "open %s_types" (to_mod modu.mod_name));
      out (sprintf "open %s_const" (to_mod modu.mod_name));
      out (sprintf "open %s_models" (to_mod modu.mod_name));
      out (sprintf "open %s_objs" (to_mod modu.mod_name));
      (* List.iter (fun name ->
        out (sprintf "open %s_%s" (to_mod modu.mod_name) name);
        ) !prol; *)
      let deps = ml_dep_instr env false pro.pro_instr in
      let deps' = ref [] in
      List.iter (fun dep ->
        if not (List.mem dep !deps') then
          out (sprintf "open %s_%s" (to_mod modu.mod_name) dep);
        deps' := dep :: !deps';
        ) deps;
      ml_emit_types env;
    end;
    if not is_array then prol := !prol @ [pro.pro_name];
    if is_array && i = 0 then prol := !prol @ [aname];

    let fargs =
      if is_fun  then
      begin
        let f = get_some fun_def in
        if f.fun_args_obj = [] then "()"
        else sprintf "%s" (targs f.fun_args)
      end
      else "" in
    let rargs =
      if is_fun  then
      begin
        let f = get_some fun_def in
          (if f.fun_ret_obj = [] then ""
           else sprintf "%s%s" 
              (if f.fun_args_obj = [] then "" else ",")
              (targs (List.map2 (fun r co -> 
                sprintf "%s* %s" (match co.co_type with
                | DT_int n | DT_logic n -> if n < 31 then "int" else "int64"
                | DT_char -> "char"
                | DT_bool -> "bool"
                | _ -> "?") r ) f.fun_ret f.fun_ret_obj)));
      end
      else "" in

    
    if is_fun then
    begin
      out ""; 
      let f = get_some fun_def in
      let s = sprintf "let %s(%s) =" (fix_name fun_name) fargs in
      out s;
      List.iter2 (fun r co ->
        let s = sprintf "  let mutable %s = %s in" r (
                match co.co_type with
                | DT_int n | DT_logic n -> if n < 31 then "0" else "Int64.zero"
                | DT_char -> "' '"
                | DT_bool -> "false"
                | _ -> "?") in
        out s;
        ) f.fun_ret f.fun_ret_obj;
    end
    else if not is_array then
    begin
      out ""; 
      out (sprintf "let pro_%s process_id =" (fix_name pname))
    end
    else if is_array && i = 0 then
    begin
      out ""; 
      out (sprintf "let pro_%s process_id =" pname);
    end;

    if not is_array || i = 0 then
    begin
      ind_incr ();
      let defs = ml_emit_objs env  true in
      ml_defs := defs;
      ml_evals := [];
      if is_fun && not is_inline then ml_evals := !ml_evals @ [sprintf "lock_fun_%s#lock();" fun_name];
      ml_emit_instr [env;glob_env]  true pro.pro_instr;    
      if not is_fun then out "process_end()";
      if is_fun && not is_inline then 
      begin
        let f = get_some fun_def in
        out (sprintf "lock_fun_%s#unlock();" fun_name);
        out (targs f.fun_ret);
      end;
      ind_decr ();
      out "";
    end;
    
    if not is_fun && not is_array then
      out (sprintf "let %s = new process [\"name\"=\"%s\"] pro_%s"  
            (fix_name pname) (fix_name pname) (fix_name pname));   
    if not is_fun && is_array then
      out (sprintf "let %s_%d = new process [\"name\"=\"%s\";\"#\"=\"%d\"] pro_%s" 
            (fix_name pname) i (fix_name pname) i (fix_name pname));   
      
    if not is_array  then  finish fname;
    if is_array && i = size-1 then
    begin
      out (sprintf "let %s = [|%s|]" aname (largs
      
            (Array.to_list (Array.init size (fun i -> sprintf "%s_%d" (fix_name pname) i)))));
      finish fname;
    end;
    ) modu.mod_procs;
  let fname = sprintf "%s" (of_mod modu.mod_name) in
  create fname;  
  out (sprintf "open %s_types" (to_mod modu.mod_name));
  out (sprintf "open %s_const" (to_mod modu.mod_name));
  out (sprintf "open %s_models" (to_mod modu.mod_name));
  out (sprintf "open %s_objs" (to_mod modu.mod_name));
  List.iter (fun pro ->
    out (sprintf "open %s_%s" (to_mod modu.mod_name) pro);
    ) !prol;
  out "let _ = ";
  out "  main#call; (* main call never return!!! *)";  
  finish fname;

  let fname = sprintf "%s_types" (of_mod modu.mod_name) in
  create fname;  
  ml_emit_types glob_env;
  finish fname;
  create_int fname;  
  ml_emit_types glob_env;
  finish_int fname;
  let fname = sprintf "%s_models" (of_mod modu.mod_name) in
  create fname;  
  out (sprintf "open %s_types" (to_mod modu.mod_name));
  ml_emit_models glob_env;
  finish fname;
  create_int fname;  
  ml_emit_models_int glob_env;
  finish_int fname;
  let fname = sprintf "%s_const" (of_mod modu.mod_name) in
  create fname;  
  out (sprintf "open %s_types" (to_mod modu.mod_name));
  ml_emit_const glob_env  false;
  finish fname;
  create_int fname;  
  ml_emit_const_int glob_env  false;
  finish_int fname;
  let fname = sprintf "%s_objs" (of_mod modu.mod_name) in
  create fname;  
  out (sprintf "open %s_types" (to_mod modu.mod_name));
  out (sprintf "open %s_const" (to_mod modu.mod_name));
  out (sprintf "open %s_models" (to_mod modu.mod_name));
  List.iter out !main_defs;  
  finish fname;
  create_int fname;  
  out (sprintf "open %s_types" (to_mod modu.mod_name));
  out (sprintf "open %s_models" (to_mod modu.mod_name));
  ml_emit_objs_int glob_env  false;
  finish_int fname;
  
  (*
  ** Makefile: core int impl
  *)
  out_ (sprintf "Creating makefile <makefile.ml>..."); 
  let fname = "makefile" in
  create_make fname;
  let m = of_mod modu.mod_name in
  List.iter out ([
    sprintf "PROJ=%s" m;
    sprintf "CC=vumc";
    sprintf "INCL=-I .";
    sprintf "%%.cmo: %%.ml";
    sprintf "\t$(CC) -c $(INCL) $*.ml";
    sprintf "%%.cmi: %%.mli";
    sprintf "\t$(CC) -c $(INCL) $*.mli";
    sprintf "clean:";
    sprintf "\trm *.cmi *.cmo";
    sprintf "core: %s_types.cmi %s_const.cmi %s_models.cmi %s_objs.cmi \\" m m m m;
    sprintf "\t%s_types.cmo %s_const.cmo %s_models.cmo %s_objs.cmo" m m m m;
    sprintf "int: \\";
    ]@(let i = ref (List.length modu.mod_procs) in
       List.filter (fun line -> line <> "") (List.map (fun pro ->
         let is_array,aname,n,size =
           if sym_check_obj modu.mod_objs pro.pro_name then
             false,"",(-1),0
           else 
           begin
             let name = Str.global_replace (Str.regexp "\(.+\)_[0-9]+") "\1" pro.pro_name in
             match sym_get_obj modu.mod_objs name with
             | OT_array at -> 
               true,
               name,
               int_of_string (Str.global_replace (Str.regexp ".+_\([0-9]+\)") "\1" pro.pro_name),
               proda at.at_dim
             | OT_object ao -> false,"",(-1),0
             | _ -> false,"",(-1),0
           end in
          decr i;
          let fname = 
            if not is_array then
              sprintf "%s_%s.cmi" (of_mod modu.mod_name) pro.pro_name
            else
              sprintf "%s_%s.cmi" (of_mod modu.mod_name) aname in          
          if not is_array || (is_array && n = 0) then
            sprintf "\t%s%s" fname (if !i > 0 then "\\" else "")
          else 
            "";
      ) modu.mod_procs))@
    [
      sprintf "impl: \\";    
    ]@(let i = ref (List.length modu.mod_procs) in
       List.filter (fun line -> line <> "") (List.map (fun pro ->
          let is_array,aname,n,size =
            if sym_check_obj modu.mod_objs pro.pro_name then
              false,"",(-1),0
            else 
            begin
              let name = Str.global_replace (Str.regexp "\(.+\)_[0-9]+") "\1" pro.pro_name in
              match sym_get_obj modu.mod_objs name with
              | OT_array at -> 
                true,
                name,
                int_of_string (Str.global_replace (Str.regexp ".+_\([0-9]+\)") "\1" pro.pro_name),
                proda at.at_dim
              | OT_object ao -> false,"",(-1),0
              | _ -> false,"",(-1),0
            end in
          decr i;
          let fname = 
            if not is_array then
              sprintf "%s_%s.cmo" (of_mod modu.mod_name) pro.pro_name
            else
              sprintf "%s_%s.cmo" (of_mod modu.mod_name) aname in          
          if not is_array || (is_array && n = 0) then
            sprintf "\t%s \\" fname 
          else 
            "";
      ) modu.mod_procs))@
    [
      sprintf "\t%s.cmo" (of_mod modu.mod_name);
    ]@      
    [
      sprintf "%s: core int impl" (of_mod modu.mod_name); 
      sprintf "\tvumc -o %s \\"  (of_mod modu.mod_name);
      sprintf "\t\t%s_types.cmo \\" (of_mod modu.mod_name);
      sprintf "\t\t%s_const.cmo \\" (of_mod modu.mod_name);
      sprintf "\t\t%s_models.cmo \\" (of_mod modu.mod_name);
      sprintf "\t\t%s_objs.cmo \\" (of_mod modu.mod_name);
      
    ]@(let i = ref (List.length modu.mod_procs) in
       List.filter (fun line -> line <> "") (List.map (fun pro ->
          let is_array,aname,n,size =
            if sym_check_obj modu.mod_objs pro.pro_name then
              false,"",(-1),0
            else 
            begin
              let name = Str.global_replace (Str.regexp "\(.+\)_[0-9]+") "\1" pro.pro_name in
              match sym_get_obj modu.mod_objs name with
              | OT_array at -> 
                true,
                name,
                int_of_string (Str.global_replace (Str.regexp ".+_\([0-9]+\)") "\1" pro.pro_name),
                proda at.at_dim
              | OT_object ao -> false,"",(-1),0
              | _ -> false,"",(-1),0
            end in
          decr i;
          let fname = 
            if not is_array then
              sprintf "%s_%s.cmo" (of_mod modu.mod_name) pro.pro_name
            else
              sprintf "%s_%s.cmo" (of_mod modu.mod_name) aname in          
          if not is_array || (is_array && n = 0) then
            sprintf "\t\t%s \\" fname 
          else 
            "";
      ) modu.mod_procs))@
    [
      sprintf "\t\t%s.cmo" (of_mod modu.mod_name);
    ]      
    );
  finish_make fname;
  Cp_common.ind_decr ();
  ()
