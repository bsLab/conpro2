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
**    $CREATED:     5.3.2006
**    $VERSION:     2.03
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
open Cp_data
open Cp_expr

open Unix
open Printf

let main_module = ref None

(*
** Print analysized module and process strcuture.
*)

let line_empty = ref true 
let maybe_nl = ref true 
let ind = ref 0 

let ind_incr () = ind := !ind + 2 
let ind_decr () = ind := !ind - 2; maybe_nl := true

let out_list = ref None
let out_cur_line = ref ""
           
let spaces n = String.make n ' ' 
let nl () =
  match !out_list with
  | None ->
    if not !line_empty then
    begin
        print_newline ();
        print_string (spaces !ind);
        line_empty := true;
        maybe_nl := false; 
    end;
  | Some ol ->
    if not !line_empty then
    begin
        out_list := Some (ol @ [!out_cur_line]);
        out_cur_line := spaces !ind;
        line_empty := true;
        maybe_nl := false; 
    end
    

let out str =
  match !out_list with
  | None ->
    if !maybe_nl then
    begin
        print_newline ();
        print_string (spaces !ind);
        maybe_nl := false;
    end;
    print_string str;
    line_empty := false;
  | Some ol ->
    if !maybe_nl then
    begin
        out_list := Some (ol @ [!out_cur_line]);
        out_cur_line := spaces !ind;
        maybe_nl := false;
    end;
    out_cur_line := !out_cur_line ^ str;
    line_empty := false


let print_dep sym ob =
        let main_obj name =
          sym_check (get_some !main_module).mod_export name in

        out "[";
        if ob.co_reader <> [] then
        begin
            out "RD{";
            List.iter (fun pro ->
                    out ("<"^pro.pro_name^">");
                ) ob.co_reader;
            if (main_obj ob.co_name) &&
               (ob.co_writer <> []) 
                then out "<TOP>";
            out "}";
        end 
        else if main_obj ob.co_name then
            out "RD{<TOP>}";

        if ob.co_reader <> [] &&
           ob.co_writer <> [] then
            out ",";

        if ob.co_writer <> [] then
        begin
            out "WR{";
            List.iter (fun pro ->
                out ("<"^pro.pro_name^">");
                ) ob.co_writer;
            out "}";
        end 
        else if main_obj ob.co_name then
            out "WR{<TOP>}";

        out "]";
        if List.mem Obj_local ob.co_flags then out "@L"

let rec print_sym sym =
            match sym with
            | Sym_obj ob ->
            begin
                let rec iter ob =
                    match ob with
                    | OT_const co ->
                        out (sprintf "CONST %s" co.co_name);
                        print_dep sym co;
                    | OT_signal co ->
                        out (sprintf "SIG %s" co.co_name);
                        print_dep sym co;
                    | OT_reg co ->
                        out (sprintf "REG %s" co.co_name);
                        print_dep sym co;
                    | OT_channel ch ->
                        let co = ch.ch_obj in
                        out (sprintf "CHANN %s" co.co_name);
                        print_dep sym co;
                    | OT_queue qu ->
                        let co = qu.qu_obj in
                        out (sprintf "QUEUE %s" co.co_name);
                        print_dep sym co;
                    | OT_var co ->
                        out (sprintf "VAR %s" co.co_name);
                        print_dep sym co;
                    | OT_array at ->
                        out (sprintf "ARRAY %s " at.at_name);
                        ind_incr ();
                        nl ();
                        Array.iter (fun ot -> iter ot; nl();) at.at_objs; 
                        ind_decr ();
                    | OT_array_sel (at,sel) ->
                        out (sprintf "ARRAYSEL %s[%d] " at.at_name sel);
                        iter at.at_objs.(sel); 
                    | OT_object ao ->
                        out (sprintf "OBJ %s[" ao.ao_name);
                        List.iter (fun (sel,pro) -> out 
                                              (sprintf "<%s:%s>" 
                                                sel pro.pro_name))
                                    ao.ao_procs;
                        out "]";
                    | OT_named_value _
                    | OT_value _ ->
                        out "OT_value";
                    | OT_component st ->
                        out (sprintf "COMPONENT %s" st.st_name);
                    | OT_struct st ->
                        out (sprintf "STRUCT %s" st.st_name);
                    | OT_reference (name,il) ->
                        out (sprintf "REF %s" name);
                    in
                iter ob; nl();
            end;
            | Sym_mod md ->
                out (sprintf "Module %s" md.mod_name); nl ();
            | Sym_rule rl ->
                out (sprintf "Rules %s" rl.rl_name); nl ();
            | Sym_pro pr ->
                out (sprintf "Process %s" pr.pro_name); nl ();
            | Sym_block bl ->
                out (sprintf "DataBlock %s" bl.db_name); nl ();
            | Sym_fun f ->
                let sarg = ref "" in
                let n = ref (List.length f.fun_args) in
                List.iter (fun arg ->
                  decr n;
                  sarg := !sarg ^ (sprintf "%s%s"
                                           arg
                                           (if !n > 0 then "," else ""));
                  ) f.fun_args;                
                out (sprintf "Procedure  %s(%s)" f.fun_name !sarg); nl ();
            | Sym_type tp ->
                let tp_name =
                    match tp with
                    | Type_const tc -> tc.tc_name;
                    | Type_struct ts -> ts.ts_name;
                    | Type_bit tb -> tb.tb_name;
                    | Type_abstract ta -> ta.ta_name;
                    | Type_exc tx -> sprintf "%s(%d)" tx.tx_name tx.tx_id;
                    in

                out (sprintf "Type %s" tp_name); nl ();
            | Sym_mon (dbg,mon) ->
                out (sprintf "MON%s:" (if dbg then "&DEB" else "")); 
                print_sym mon



                                                                 
(*
** Print PI instruction
*)
let rec print_instr instr =
    let main_obj name =
        sym_check (get_some !main_module).mod_export name
        in
    let rec print_obj ot =
        match ot with
        | OT_const co ->
            out ("CON{"^co.co_name^"}");
        | OT_signal co ->
            out ("SIG{"^co.co_name^"}");
        | OT_reg co ->
            out ("REG{"^co.co_name^"}");
        | OT_channel ch ->
            out ("CHAN{"^ch.ch_obj.co_name^"}");
        | OT_queue qu ->
            out ("QUEUE{"^qu.qu_obj.co_name^"}");
        | OT_var co ->
            out ("VAR{"^co.co_name^"}");
        | OT_array at ->
            out "ARRAY:"; print_obj at.at_objs.(0);
        | OT_array_sel (at,sel) ->
            out "ARRAYSEL:"; print_obj at.at_objs.(0);
        | OT_object ao ->
            out ("OBJ{"^ao.ao_name^"}");
        | OT_named_value (_,v)
        | OT_value v ->
        begin
            match v with
            | V_int i -> out (sprintf "VAL=%s" (Int64.to_string i));
            | V_char c -> out (sprintf "VAL=%c" c);
            | _ -> out "VAL";
        end;
        | OT_component _ ->
            out "<COMP>";
        | OT_struct _ ->
            out "<STRUCT>";
        | OT_reference (oname,dl) ->
            out (sprintf "REF{%s%s}" oname 
                                      (let s=ref "[" in
                                       let n=ref (List.length dl) in
                                       List.iter (fun d ->
                                         decr n;
                                         s := sprintf "%s%d%s" !s (d-1) (
                                                      if !n > 0 then "," else "]")) dl;
                                       !s));
        in

    let print_opl opl = out (sprint_opl opl) in

    let print_params bf =
      let pl = ref  "" in
      List.iter (fun bp ->
        match bp with
        | BP_unroll -> pl := !pl ^ "<UNROLL>";
        | BP_bind -> pl := !pl ^ "<BIND>";
        | BP_expr et ->  pl := !pl ^ (sprintf "<EXPR=%s>" (str_of_expr_type et));
        | BP_temp expr ->  pl := !pl ^ (sprintf "<TEMP=%s>" expr);
        | BP_alu_min_width expr ->  pl := !pl ^ (sprintf "<ALUMIN=%d>" expr);
        | BP_alu_ops opl ->
          pl := !pl ^ "<ALUOPS=";
          List.iter (fun op ->
            pl := !pl ^ (op_name op);
            ) opl;
          pl := !pl ^ ">";
        | BP_alu_type dil ->
          pl := !pl ^ "<ALUTYP=";
          List.iter (fun di ->
            match di with
            | T_logic -> pl := !pl ^ "L";
            | T_int -> pl := !pl ^ "I";
            | T_char -> pl := !pl ^ "C";
            | _ -> ();
            ) dil;
          pl := !pl ^ ">";
        | BP_schedule sm ->
          pl := !pl ^ "<SCHED=";
          begin
            match sm with
            | Sched_auto -> pl := !pl ^ "AUTO";
            | Sched_def -> pl := !pl ^ "DEF";
            | Sched_custom _ -> pl := !pl ^ "CUSTOM";
          end;
          pl := !pl ^ ">";
        | BP_inline -> pl := !pl ^ "<INLINE>";
        | BP_locked -> pl := !pl ^ "<LOCKED>";
        ) bf.bf_params;
      sprintf "PARAMS=%s." !pl in

      match instr with
        | PI_nop -> ();
        | PI_assign (src,lhs,rhs) -> out "ASSIGN("; 
                                 print_instr lhs;
                                 out "<-";
                                 print_instr rhs;
                                 out ")";
        | PI_map (src,lhs,rhs) -> out "MAP("; 
                                 print_instr lhs;
                                 out "<-";
                                 print_instr rhs;
                                 out ")";
        | PI_fun (src,(opl,ot),sel,il) -> 
                let ao = ao_of_ot opl ot in
                out (sprintf "FUN<%s.%s>("
                              ao.ao_name sel);
                           List.iter (fun i ->
                            print_instr i) il;
                           out ")";
        | PI_arithm (a,o1,o2) -> out "ARITH(";
                                 print_instr o1;
                                 out (op_name a);
                                 print_instr o2;
                                 out ")";
        | PI_bool (kind,a,o1,o2) ->   if kind = Relational then
                                        out "REL("
                                      else
                                        out "BOOL(";
                                 print_instr o1;
                                 out (op_name a);
                                 print_instr o2;
                                 out ")";
        | PI_concat (o1,o2) -> out "CONCAT(";
                                 print_instr o1;
                                 print_instr o2;
                                 out ")";
        | PI_waitfor (src,o,time,unit,o1,o2) -> 
            if time = 0 then
            begin
                out "WAIT(";
                print_instr o;
                out ")";
            end
            else
            begin
                if o1 = PI_nop then
                    out (sprintf "DELAY(%d %s)" time (print_timeunit unit))
                else
                    out (sprintf "APPLY(%d %s)" time (print_timeunit unit));
            end;
        
        | PI_branch (src,e,s1,s2) -> out "IF (";
                                 print_instr e;
                                 out ") THEN"; 
                                 ind_incr (); nl ();
                                 print_instr s1; 
                                 ind_decr ();
                                 out "ELSE"; 
                                 ind_incr (); nl ();
                                 print_instr s2; 
                                 ind_decr ();
                                 out "END IF";
        | PI_forloop (src,e,dir,lim1,lim2,bl) -> out "FOR ";
                                 print_instr e;
                                 out "=";
                                 print_instr lim1;
                                 (if dir = '+' then out "TO" else out "DOWNTO"); 
                                 print_instr lim2;
                                 out "DO"; 
                                 ind_incr (); nl ();
                                 print_instr bl; 
                                 ind_decr ();
                                 out "DONE";
        | PI_loop (src,_,e,bl) -> out "LOOP ";
                                 print_instr e;
                                 out "DO"; 
                                 ind_incr (); nl ();
                                 print_instr bl; 
                                 ind_decr ();
                                 out "DONE";
        | PI_select (src,e,csl) -> out "SELECT(";
                                   print_instr e;
                                   out ")"; nl ();
                                   print_instr csl; nl ();
        | PI_case (src,el,isl) -> out "CASE(";
                                   List.iter (fun e ->
                                     print_instr e;
                                     out ";") el;
                                   out ")"; nl ();
                                   print_instr isl; nl ();
        | PI_try (b,csl) -> out "TRY(";
                                   ind_incr ();
                                   print_instr b;
                                   ind_decr ();
                                   out ") with ("; nl ();
                                   ind_incr ();
                                   print_instr csl; nl ();
                                   ind_decr ();
                                   out ")";
        | PI_raise (ex) -> out (sprintf "RAISE(%d)" ex);
        | PI_block (il,bf) when not (List.mem BP_bind bf.bf_params) ->
            out (sprintf "BEGIN<%s%d>" bf.bf_name bf.bf_id); 
            ind_incr (); nl ();
            let n = ref (List.length il) in
            List.iter (fun i -> print_instr i;  
                                decr n;
                                if !n > 0 then nl (); ) il;
            ind_decr (); 
            out (sprintf "END<%s%d> [%s]" bf.bf_name bf.bf_id (print_params bf)); 
        | PI_block (il,bf) when (List.mem BP_bind bf.bf_params) ->
            out "BIND {"; 
            ind_incr (); nl ();
            let n = ref (List.length il) in
            List.iter (fun i -> print_instr i; 
                                decr n;
                                if !n > 1 then nl (); ) il;
            ind_decr ();
            out "}"; nl ();
        | PI_list il -> 
            let n = ref (List.length il) in
            List.iter (fun i -> print_instr i; 
                                decr n;
                                if !n > 1 then nl (); ) il;

        | PI_obj (opl,ot) -> 
            print_opl opl;
            print_obj ot;
        | PI_monitor (_,sym,dbg) ->
            out "MON("; print_sym sym; out ")"; nl();
        | PI_block (il,bf) ->
            out "BLOCK("; ind_incr (); nl ();
            List.iter print_instr il;
            out ")"; ind_decr (); nl () 
    

(*
** Module
*)
let rec print_module modu =
    an.a_curpos <- nilsrc ();
    let mn = modu.mod_name in


    out (sprintf "Module %s: " modu.mod_name); 
    ind_incr (); nl();
    out (sprintf "Number of toplevel symbols: %d" 
            (Cp_symbol.length modu.mod_objs)); 
    nl();
    out (sprintf "Number of processes: %d" 
            (List.length modu.mod_procs)); 
    nl();
    out (sprintf "Number of external modules: %d" 
            (List.length modu.mod_external)); 
    nl();
    out (sprintf "Number of rule modules: %d" 
            (List.length modu.mod_rules)); 
    print_newline (); nl ();

    (*
    ** Show all toplevel symbols
    *)
    let syms = list_of_sym modu.mod_objs in
    out "Module Symbols:"; ind_incr (); nl ();
    List.iter (fun sym ->
            print_sym sym;
        ) syms;

    ind_decr (); 

    let syms = list_of_sym modu.mod_export in
    out "Module Exported Symbols:"; ind_incr (); nl ();
    List.iter (fun sym ->
            print_sym sym;
        ) syms;

    ind_decr (); 

    out "Module instructions:"; ind_incr (); nl ();
    List.iter (fun instr ->
            print_instr instr; nl();
        ) modu.mod_instr;

    ind_decr (); 
    
    List.iter (fun pro ->
        out (sprintf "Process %s.%s:" mn pro.pro_name); 
        ind_incr (); nl ();
        out "Exported symbols:"; ind_incr (); nl ();
        let syms = list_of_sym pro.pro_export in
        List.iter (fun sym ->
                print_sym sym;
            ) syms;
        ind_decr (); nl ();
        out "Private symbols:"; ind_incr (); nl ();
        let syms = list_of_sym pro.pro_objs in
        List.iter (fun sym ->
                print_sym sym;
            ) syms;
        ind_decr (); nl ();
        out "Imported symbols:"; ind_incr (); nl ();
        let syms = list_of_sym pro.pro_import in
        List.iter (fun sym ->
                print_sym sym;
            ) syms;
        ind_decr (); nl ();
        out "Temporary registers:"; ind_incr (); nl ();
        List.iter (fun co ->
                print_sym (Sym_obj (OT_reg co));
            ) pro.pro_temps;
        ind_decr (); nl ();
        out "Instructions:"; ind_incr (); nl ();
        let instrs = pro.pro_instr in
        List.iter (fun instr ->
                print_instr instr; nl();
            ) instrs;
        ind_decr (); nl ();
        ind_decr (); nl ();
        ) modu.mod_procs;
    List.iter (fun md -> print_module md ) modu.mod_external

let print_instr instr =
    out_list := Some [];
    print_instr instr;
    nl ();
    let ol = get_some (!out_list) in
    out_list := None;
    List.iter (fun l ->
        Cp_common.out l;
        ) ol

let print_module modu =
    out_list := Some [];
    print_module modu;
    nl ();
    let ol = get_some (!out_list) in
    out_list := None;
    List.iter (fun l ->
        Cp_common.out l;
        ) ol
