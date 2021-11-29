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
**    $VERSION:     2.04
**
**    $INFO:
**
**  External Module Interface
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
open Unix
open Printf
open Cp_fun
open Cp_emi_types
open Cp_emi_analysis
open Cp_emi_interp

(*
** Open an external module interface specification...
*)
let opened_modules = ref []

let open_module mname =
  let rec find modl =
    match modl with
    | (mname',emi') :: tl ->
      if mname = mname' then
        emi'
      else
        find tl
    | [] ->
      out (sprintf "Opening module <%s>..." mname);
      out_ (sprintf "Opening module <%s>..." mname);
      let old_pos = an.a_curpos in 
      an.a_curpos <- nilsrc ();  
      ind_incr ();
      let tops,lmap = syntax_of_file (of_mod mname) in
      an.a_modu.mod_fmap <- an.a_modu.mod_fmap @ lmap;

      let emi = new emi mname tops lmap in
      emi#init;

      emi#create_rules;
      let rules = emi#get_rules in
      sym_add top_syms (Sym_rule rules);
      an.a_modu.mod_rules <- an.a_modu.mod_rules @ [rules];

      out (sprintf "Added module <%s>." mname);
      (*
      ** Add abstract type definitions to module symbols.
      *)
      ind_incr ();
      List.iter (fun t ->
        out (sprintf "EMI <%s>: adding object type <%s>..." mname t.ta_name);
        sym_add an.a_modu.mod_objs (Sym_type (Type_abstract t));
        ) rules.rl_types;
      ind_decr ();
      ind_decr ();
      an.a_curpos <- old_pos;
      opened_modules := !opened_modules @ [mname,emi];
      emi in
      
  find !opened_modules


let compile_module modu =
    (*
    ** Compile objects first if necessary (required by EMI objects...)
    *)
    let compile obj =
      let stat = (get_rules obj).rl_interp "compile" in
      if (stat <> "ok") && (stat <> "") then
        error 0 (sprintf "Compilation of object <%s> failed: %s." 
                         (name_of_ot obj) stat) in 

    List.iter (fun sym ->
            match sym with
            | Sym_obj obj -> 
            begin
              match obj with 
              | OT_array at ->
              begin
                match at.at_objs.(0) with
                | OT_object ao (* when (List.mem AT_dyn at.at_flags) *) -> 
                begin
                  Array.iter compile at.at_objs;
                end;
                | _ -> compile obj; 
              end;
              | _ -> compile obj;
            end;
            | _ -> ();
          ) (list_of_sym modu.mod_objs)
          
          
