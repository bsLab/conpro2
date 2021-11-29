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
**    $CREATED:     27.9.2007
**    $VERSION:     2.11
**
**    $INFO:
**
**  Print formatted output of internal types:
**
**  1. bf: Block Frame tree (type block_frame)
**  2. ast: Syntax tree (type syntax) 
**  3. pi: Process instruction tree: (type instr) list
**  3b. cp: Process instruction, ConPro language
**  4. uc: uCode instruction list: (type uc_instr) list
**  5. rtl: RTL state list (type state_block) list        
**
**    $ENDOFINFO
**
*)

open Cp_syntax
open Cp_common
open Cp_types
open Cp_utils
open Cp_data_core
open Cp_print
open Printf


(*
*************************
** Block Frame Type
*************************
*)
  
let bf_sprint_frame indent bf =
  box indent "{%s}" 
      (sprintf "bf_name=%s; bf_id=%d; bf_src=%s;\nbf_parent=%s\nbf_time=%s\nbf_type=%s\nbf_params=%s\nbf_childs=%s\n" 
         bf.bf_name bf.bf_id
         (if bf.bf_src_start.s_line <> bf.bf_src_end.s_line then
            sprintf "{file=%s; lines=%d..%d}"
                bf.bf_src_start.s_file
                bf.bf_src_start.s_line
                bf.bf_src_end.s_line
         else 
           sprintf "{file=%s; line=%d}"
               bf.bf_src_start.s_file
               bf.bf_src_start.s_line)
          (
            match bf.bf_parent with
            | Some bf' -> sprintf "Some {bf_name=%s;bf_id=%d}" bf'.bf_name bf'.bf_id;
            | None -> "None";
          )
          (
            let rec get_time ft =
              match ft with
              | FT_0 -> 
                "?"
              | FT_list fl ->
                let str = ref "(" in
                let n = ref (List.length fl) in
                List.iter (fun ft' -> decr n;
                                      str := !str ^ (sprintf "%s%s" 
                                                    (get_time ft')
                                                    (if !n > 0 then "+" else""))) fl;
                !str^")"
              | FT_n (ft_n,ft') ->
                sprintf "(%s*%s)" (get_time ft_n) (get_time ft');
              | FT_max n ->
                sprintf "<=%d" n;
              | FT_min n ->
                sprintf ">=%d" n;
              | FT_minmax (ft1,ft2) ->
                sprintf "(%s..%s)" (get_time ft1) (get_time ft2);
              | FT n ->
                sprintf "%d" n in

            let t1 = get_time bf.bf_time in
            t1
          )

          (
              match bf.bf_type with
              | BF_data -> "BF_data";
              | BF_branch -> "BF_branch";
              | BF_compound -> "BF_compound";
              | BF_empty -> "BF_empty";
              | BF_loop -> "BF_loop";
              | BF_conditional -> "BF_conditional";
          )
          (
            sprintf "[%s]" (hlist (fun bp ->
                match bp with
                | BP_unroll -> "BP_unroll";
                | BP_locked -> "BP_locked";
                | BP_bind -> "BP_bind";
                | BP_expr et ->  sprintf "BP_expr(%s)" (str_of_expr_type et);
                | BP_temp expr ->  sprintf "BP_temp(%s)" expr;
                | BP_alu_min_width expr -> sprintf "BP_alu_min_width(%d)" expr;
                | BP_alu_ops opl -> sprintf "BP_alu_ops [%s]" (hlist op_name opl);
                | BP_alu_type dil ->
                  sprintf "BP_alu_type [%s]" (
                    hlist (fun di ->
                    match di with
                    | T_logic -> "T_logic";
                    | T_int -> "T_int";
                    | T_char -> "T_char";
                    | _ -> "?";
                    ) dil);
                | BP_schedule sm ->
                  sprintf "BP_schedule(%s)" 
                  (
                    match sm with
                    | Sched_auto -> "Sched_auto";
                    | Sched_def -> "Sched_def";
                    | Sched_custom _ -> "Sched_custom";
                  );
                | BP_inline -> "BP_inline";
                ) bf.bf_params);
            )
            (
              sprintf "[%s]" (
                hlist (fun bf' ->
                    sprintf "%s%d" bf'.bf_name bf'.bf_id;
                    ) bf.bf_childs);
            )
      ) 
    

(*
*************************
** Abstract Syntax Tree
*************************
*)

let ast_sprint_timeunit vunit =
    match vunit with
    | Nsec -> "Nsec";
    | Usec -> "Usec";
    | Msec -> "Msec";
    | Sec -> "Sec";
    | Cycles -> "Cycles"
           
let ast_sprint_frequnit vunit =
    match vunit with
    | Ghz -> "Ghz";
    | Mhz -> "Mhz";
    | Khz -> "Khz";
    | Hz -> "Hz"

let ast_sprint_loop_kind kind =
    match kind with
    | Loop_while -> "Loop_while";
    | Loop -> "Loop"
     

let rec ast_sprint_syntax s =
  match s with
  | T_ident (pos,str)   -> sprintf "%s" str; 
  | T_unit   -> "()"; 
  | T_id   -> "T_id"; 
  | T_z    -> "T_z";
  | T_time (vname,vunit) -> 
    sprintf "T_time(%s,%s)" (ast_sprint_syntax vname) (ast_sprint_timeunit vunit);
  | T_freq (vname,vunit) -> 
    sprintf "T_freq(%s,%s)" (ast_sprint_syntax vname) (ast_sprint_frequnit vunit); 
  | T_character (pos,c)  -> sprintf "\'%c\'" c; 
  | T_string (pos,str)  -> sprintf "\"%s\"" str; 
  | T_empty -> "*";
  | T_OT (vnamel,vtypel) ->
    sprintf "T_OT([%s],[%s])" (hlist ast_sprint_syntax vnamel) (hlist ast_sprint_syntax vtypel); 
  | T_OT_const (vnamel,vtype,vinit) -> 
    sprintf "T_OT_const([%s],%s,%s)\n" (hlist ast_sprint_syntax vnamel) 
                                     (ast_sprint_syntax vtype)
                                     (ast_sprint_syntax vinit); 
  | T_OT_sig (vnamel,vtype,vinit) -> 
    sprintf "T_OT_sig([%s],%s,%s)\n" (hlist ast_sprint_syntax vnamel) 
                                     (ast_sprint_syntax vtype)
                                     (ast_sprint_syntax vinit); 
  | T_OT_reg (vnamel,vtype,vinit,vparams) -> 
    sprintf "T_OT_sig([%s],%s,%s,%s)\n" (hlist ast_sprint_syntax vnamel) 
                                     (ast_sprint_syntax vtype)
                                     (ast_sprint_syntax vinit)
                                     (ast_sprint_syntax vparams); 
  | T_OT_chan (vnamel,vtype,vparams) -> 
    sprintf "T_OT_chan([%s],%s,%s)\n" (hlist ast_sprint_syntax vnamel) 
                                     (ast_sprint_syntax vtype)
                                     (ast_sprint_syntax vparams); 
  | T_OT_que (vnamel,vtype,vparams) -> 
    sprintf "T_OT_que([%s],%s,%s)\n" (hlist ast_sprint_syntax vnamel) 
                                     (ast_sprint_syntax vtype)
                                     (ast_sprint_syntax vparams); 
  | T_OT_var (vnamel,vtype,vgroup,vinit,vparams) -> 
    sprintf "T_OT_var([%s],%s,%s,%s,%s)\n" (hlist ast_sprint_syntax vnamel) 
                                     (ast_sprint_syntax vtype)
                                     (ast_sprint_syntax vgroup)
                                     (ast_sprint_syntax vinit)
                                     (ast_sprint_syntax vparams); 
  | T_OT_array (vnamel,vkind,vobjrange,vtype,vgroup,vparams) -> 
    sprintf "T_OT_array([%s],%c,%s,%s,%s,%s)\n" (hlist ast_sprint_syntax vnamel) vkind
                                     (ast_sprint_syntax vobjrange)
                                     (ast_sprint_syntax vtype)
                                     (ast_sprint_syntax vgroup)
                                     (ast_sprint_syntax vparams); 
  | T_OT_comp (vnamel,vtype,vinit) -> 
    sprintf "T_OT_comp([%s],%s,%s)\n" (hlist ast_sprint_syntax vnamel) 
                                     (ast_sprint_syntax vtype)
                                     (if vinit <> [] then "{...}" else "{}")
  | T_OT_object (vnamel,vtype,vparams) -> 
    sprintf "T_OT_object([%s],%s,%s)\n" (hlist ast_sprint_syntax vnamel) 
                                     (ast_sprint_syntax vtype)
                                     (ast_sprint_syntax vparams); 
  | T_assign (lhs,rhs) -> 
    sprintf "T_assign(%s,%s)\n" (ast_sprint_syntax lhs) (ast_sprint_syntax rhs);
  | T_map (lhs,rhs) -> 
    sprintf "T_map(%s,%s)\n" (ast_sprint_syntax lhs) (ast_sprint_syntax rhs);
  | T_OP_arith (op,op1,op2) -> 
    sprintf "T_OP_arith(%s,%s,%s)" (ast_sprint_syntax op1) op (ast_sprint_syntax op2);
  | T_OP_bool (kind,op,op1,op2) -> 
    sprintf "T_OP_bool(%s,%s,%s)" (ast_sprint_syntax op1) op (ast_sprint_syntax op2);
  | T_Fun (vname,vargl,vret) ->
    sprintf "T_Fun(%s,[%s],%s)" (ast_sprint_syntax vname)
                                (hlist ast_sprint_syntax vargl) 
                                (ast_sprint_syntax vret); 
  | T_Fun_def (vname,vretl,vargl,vinstr) ->
    sprintf "T_Fun_def(%s,%s,[%s],\n%s)\n" (ast_sprint_syntax vname)
                                (hlist ast_sprint_syntax vretl)
                                (hlist ast_sprint_syntax vargl) 
                                (ast_sprint_syntax vinstr); 
  | T_typeconv (c,varg1) ->
    sprintf "T_typeconv(%c,%s)" c (ast_sprint_syntax varg1);
  | T_concat (v1,v2) ->
    sprintf "T_concat(%s,%s)" (ast_sprint_syntax v1) (ast_sprint_syntax v2);
  | T_connect vl ->
    sprintf "T_connect(%s)\n" (ast_sprint_syntax vl);
  | T_export vl ->
    sprintf "T_export[%s]\n" (hlist ast_sprint_syntax vl);
  | T_import vl ->
    sprintf "T_import(%s)\n" (hlist ast_sprint_syntax vl);
  | T_include vl ->
    sprintf "T_include(%s)\n" (ast_sprint_syntax vl);
  | T_data_block (v,pl) ->
    sprintf "T_data_block(%s,%s)\n" (ast_sprint_syntax v) (ast_sprint_syntax pl);
  | T_process (vname,vbody) -> 
    box 2 "T_process(%s)\n" (sprintf "%s,\n%s" (ast_sprint_syntax vname) (ast_sprint_syntax vbody));  
  | T_module_def (vname,vbody) -> 
    box 2 "T_module_def(%s)\n" (sprintf "%s,\n%s" (ast_sprint_syntax vname) (ast_sprint_syntax vbody));  
  | T_block (vexpr,vparams) ->
    box 2 "T_block(%s)\n" (sprintf "%s,\n%s" (if vparams <> None then ast_sprint_syntax (get_some vparams) else "*")
                                             (ast_sprint_syntax vexpr)); 
  | T_bind vexpr ->
    sprintf "T_bind(%s)" (ast_sprint_syntax vexpr);
  | T_topinstr vexpr ->
    box 2 "T_topinstr(%s)\n" (ast_sprint_syntax vexpr);
  | T_instr vexpr ->
    box 2 "T_instr(%s)\n" (ast_sprint_syntax vexpr);
  | T_sub (vname,vexpr) ->
    sprintf "T_sub(%s,%s)" (ast_sprint_syntax vname) (ast_sprint_syntax vexpr);
  | T_interval (va,vb) ->
    sprintf "T_interval(%s,%s)" (ast_sprint_syntax va) (ast_sprint_syntax vb);
  | T_selector (vname,vexpr) ->
    sprintf "T_selector(%s,%s)" (ast_sprint_syntax vname) (ast_sprint_syntax vexpr);
  | T_list l ->
    box 0 "T_list[%s]\n" (vlist ast_sprint_syntax l);
  | T_waitfor (vexpr,vcyc,v_false,v_true) ->
    box 2 "T_waitfor(%s)\n" (sprintf "%s,%s,\n%s,\n%s" (ast_sprint_syntax vexpr)
                                (ast_sprint_syntax vcyc)
                                (ast_sprint_syntax v_false) 
                                (ast_sprint_syntax v_true)); 
  | T_loop (kind,vexpr,vblock) ->
    box 2 "T_loop(%s)\n"  (sprintf "%s,%s,\n%s" (ast_sprint_loop_kind kind)
                                (ast_sprint_syntax vexpr)
                                (ast_sprint_syntax vblock)); 
  | T_branch (vexpr,vb1,vb2) ->
    box 2 "T_branch(%s)\n" (sprintf "%s,\n%s,\n%s" (ast_sprint_syntax vexpr)
                                (ast_sprint_syntax vb1) 
                                (ast_sprint_syntax vb2)); 
  | T_forloop (vexpr,dir,vlim1,vlim2,vb) ->
    box 2 "T_forloop(%s)\n" (sprintf "%s,%s,%c,%s,\n%s" (ast_sprint_syntax vexpr)
                                (ast_sprint_syntax vlim1)
                                dir
                                (ast_sprint_syntax vlim2)
                                (ast_sprint_syntax vb)); 

  | T_select (vexpr,vsl) ->
    box 2 "T_select(%s)\n" (sprintf "%s,\n%s" (ast_sprint_syntax vexpr)
                                (ast_sprint_syntax vsl)); 
  | T_case (vexprl,vi) ->
    box 2 "T_case(%s)\n" (sprintf "[%s],\n%s" (hlist ast_sprint_syntax vexprl)
                              (ast_sprint_syntax vi)); 
  | T_try (vb,vsl) ->
    box 2 "T_try(%s)\n" (sprintf "%s,\n%s" (ast_sprint_syntax vb)
                                           (ast_sprint_syntax vsl)); 

  | T_raise vi -> sprintf "T_raise(%s)" (ast_sprint_syntax vi);
  | T_exception vi -> sprintf "T_exception(%s)" (ast_sprint_syntax vi);
  | T_reg   -> "T_reg"; 
  | T_var   -> "T_var"; 
  | T_sig   -> "T_sig";
  | T_chan   -> "T_chan";
  | T_que   -> "T_que";
  | T_proc -> "T_proc";

  | T_module vexpr ->
    sprintf "T_module(%s)\n" (ast_sprint_syntax vexpr);
  | T_monitor (vobjl,dbg) ->
    sprintf "T_monitor([%s],%b)\n" (hlist ast_sprint_syntax vobjl) dbg;
  | T_typedef (vtype,vlist,vflags) ->
    sprintf "T_typedef(%s,%s,%s)\n" (ast_sprint_syntax vtype)
                                (ast_sprint_syntax vlist)
                                (ast_sprint_syntax vflags); 
  | T_domain _ -> "T_domain()\n";
  | T_objlist _ -> "T_objlist()"


let ast_print_syntax_list file syntaxl =
  out (sprintf "Emitting ast-dump (Abstract Syntax Tree) file <%s.ast>..." file);
  let oc = 
    try open_out (sprintf "%s%s.ast" compiler.t_output file) with
    _ -> error 0 (sprintf "Can't open file <%s%s>." compiler.t_output file) in
  List.iter (fun syntax -> let str = ast_sprint_syntax syntax in
                          output_string oc str) syntaxl;
  close_out oc
    
(*
**********************
** Process Instruction
**********************
*)  

let sprint_src src = 
  if src.s_file <> "" || src.s_line > 0 then
    sprintf "L%d" src.s_line
  else ""
 

let rec pi_ssprint_value va =
  match va with
  | V_int i64 -> sprintf "V_int(%s)" (Int64.to_string i64)
  | V_float flt -> sprintf "V_float(%f)" flt
  | V_string str -> sprintf "V_string(%s)" str
  | V_char ch -> sprintf "V_char(%c)" ch
  | V_logic lstr -> sprintf "V_logic(%s)" lstr
  | V_bool bo -> sprintf "V_bool(%b)" bo
  | V_list vl -> sprintf "V_list[%s]" (hlist pi_ssprint_value vl);
  | V_z -> "V_z"
  | V_time (i64,timeunit) -> sprintf "V_time(%s %s)" (Int64.to_string i64) (ast_sprint_timeunit timeunit)  
  | V_freq (i64,frequnit) -> sprintf "V_freq(%s %s)" (Int64.to_string i64) (ast_sprint_frequnit frequnit)
  | V_null -> "V_null"
  
   
let rec pi_sprint_object_type ot =
  match ot with
  | OT_const co -> sprintf "OT_const(%s)" co.co_name 
  | OT_named_value (name,va) -> sprintf "OT_named_value(%s,%s)" name (pi_ssprint_value va)  
  | OT_signal co -> sprintf "OT_signal(%s)" co.co_name
  | OT_reg co -> sprintf "OT_reg(%s)" co.co_name
  | OT_var co -> sprintf "OT_var(%s)" co.co_name
  | OT_component st -> sprintf "OT_component(%s)" st.st_name
  | OT_struct st -> sprintf "OT_struct(%s)" st.st_name
  | OT_array at -> sprintf "OT_array(%s)" at.at_name
  | OT_array_sel (at,n) -> sprintf "OT_array_sel(%s,%d)" at.at_name n
  | OT_object ao -> sprintf "OT_object(%s)" ao.ao_name
  | OT_value va -> sprintf "OT_value(%s)" (pi_ssprint_value va)
  | OT_channel ch -> sprintf "OT_channel(%s)" ch.ch_obj.co_name
  | OT_queue qu -> sprintf "OT_queue(%s)" qu.qu_obj.co_name
  | OT_reference (name,vl) -> sprintf "OT_reference(%s,[%s])" name (hlist (sprintf "%d") vl)

  
let pi_sprint_data_type dt =
  match dt with
  | DT_logic n -> sprintf "DT_logic(%d)" n
  | DT_int n -> sprintf "DT_int(%d)" n
  | DT_string n -> sprintf "DT_string(%d)" n
  | DT_char -> "DT_char"
  | DT_bool -> "DT_bool"
  | DT_object str -> sprintf "DT_object(%s)" str
  | DT_lneg -> "DT_lneg"
  | DT_aneg -> "DT_aneg"
  | DT_natural n -> sprintf "DT_natural(%d)" n

let rec pi_sprint_object_params op =
  match op with
  | OD_sub (a,b) -> sprintf "OD_sub(%d,%d)" a b
  | OD_index instr -> box 2 "OD_index(%s)" (pi_sprint_instr instr)
  | OD_conv dt -> sprintf "OD_conv(%s)" (pi_sprint_data_type dt)
  | OD_sel al -> sprintf "OD_sel[|%s|]" (hlist (sprintf "%d") (Array.to_list al));  
  | OD_sel_obj  instr -> box 2 "OD_sel_obj(%s)" (pi_sprint_instr instr)
  | OD_lneg -> "OD_lneg"
  | OD_aneg -> "OD_aneg"
 
and pi_sprint_instr instr = 
  match instr with
  | PI_obj (opl, ot) -> 
    box 2 "PI_obj(%s)" (sprintf "[%s]:%s" (hlist pi_sprint_object_params opl) (pi_sprint_object_type ot)) 
  | PI_tuple il -> 
    box 2 "PI_tuple(%s)" (sprintf "[%s]" (hlist pi_sprint_instr il)) 
  | PI_list il -> box 2 "PI_list[%s]\n" (vlist pi_sprint_instr il)
  | PI_block (il, bf) -> box 2 "PI_block(%s)\n" (sprintf "[\n%s],\n{bf_name=%s;bf_id=%d;bf_flags=%s}" 
                                                         (vlist pi_sprint_instr il)
                                                         bf.bf_name 
                                                         bf.bf_id 
                               (
                                 sprintf "[%s]" (hlist (fun bp ->
                                     match bp with
                                     | BP_unroll -> "BP_unroll";
                                     | BP_locked -> "BP_locked";
                                     | BP_bind -> "BP_bind";
                                     | BP_expr et ->  sprintf "BP_expr(%s)" (str_of_expr_type et);
                                     | BP_temp expr ->  sprintf "BP_temp(%s)" expr;
                                     | BP_alu_min_width expr -> sprintf "BP_alu_min_width(%d)" expr;
                                     | BP_alu_ops opl -> sprintf "BP_alu_ops [%s]" (hlist op_name opl);
                                     | BP_alu_type dil ->
                                       sprintf "BP_alu_type [%s]" (
                                         hlist (fun di ->
                                         match di with
                                         | T_logic -> "T_logic";
                                         | T_int -> "T_int";
                                         | T_char -> "T_char";
                                         | _ -> "?";
                                         ) dil);
                                     | BP_schedule sm ->
                                       sprintf "BP_schedule(%s)" 
                                       (
                                         match sm with
                                         | Sched_auto -> "Sched_auto";
                                         | Sched_def -> "Sched_def";
                                         | Sched_custom _ -> "Sched_custom";
                                       );
                                     | BP_inline -> "BP_inline";
                                     ) bf.bf_params);
                                 )
                                                         )
  | PI_arithm (op, op1, op2) -> sprintf "PI_arithm(%s,%s,%s)" (pi_sprint_instr op1) (op_name op) (pi_sprint_instr op2)
  | PI_bool (kind , op, op1, op2) -> sprintf "PI_bool(%s,%s,%s)" (pi_sprint_instr op1) (op_name op) (pi_sprint_instr op2)
  | PI_concat (op1, op2) -> sprintf "PI_concat(%s,%s)" (pi_sprint_instr op1) (pi_sprint_instr op2)
  | PI_assign (source, lhs, rhs) -> sprintf "PI_assign(%s,%s,%s)\n" (sprint_src source) (pi_sprint_instr lhs) (pi_sprint_instr rhs)
  | PI_waitfor (source, epxr, cyc, timeunit, b_0, b_1) -> ""
  | PI_branch (source, expr, b_0, b_1) -> 
    box 2 "PI_branch(%s)\n" (sprintf "%s,%s,\n%s,\n%s" (sprint_src source) 
                                             (pi_sprint_instr expr)
                                             (pi_sprint_instr b_0)
                                             (pi_sprint_instr b_1)) 

  | PI_forloop (source, expr, dir, lim1, lim2, b) -> 
    box 2 "PI_forloop(%s)\n" (sprintf "%s,%s,%s,%s,%s,\n%s" (sprint_src source) 
                                             (pi_sprint_instr expr)
                                             (pi_sprint_instr lim1)
                                             (if dir = '+' then "TO" else "DOWNTO")                                             
                                             (pi_sprint_instr lim2)                                             
                                             (pi_sprint_instr b)) 

  | PI_loop (source, kind, expr, b) -> 
    box 2 "PI_loop(%s)\n" (sprintf "%s,%s,%s,%s" (sprint_src source) 
                                             (ast_sprint_loop_kind kind) 
                                             (pi_sprint_instr expr)
                                             (pi_sprint_instr b)) 
  | PI_select (source, expr, cl) -> box 2 "PI_select(%s)\n" (sprintf "%s,%s,%s" (sprint_src source)
                                                                            (pi_sprint_instr expr)
                                                                            (pi_sprint_instr cl))
  | PI_case (source, exprl, b) -> box 2 "PI_case(%s)\n" (sprintf "%s,%s,%s" (sprint_src source)
                                                                        (if exprl <> [] then (vlist pi_sprint_instr exprl) else "others")
                                                                        (pi_sprint_instr b))
  | PI_try (b, cl) -> box 2 "PI_try(%s)\n" (sprintf "%s,%s" (pi_sprint_instr b)
                                                            (pi_sprint_instr cl))
  | PI_raise ex -> sprintf "PI_raise(%d)\n" ex; 
  | PI_map (source, lhs, rhs) ->  sprintf "PI_map(%s,%s,%s)" (sprint_src source) (pi_sprint_instr lhs) (pi_sprint_instr rhs)
  | PI_fun (source, (opl, ot), name, il) -> sprintf "PI_fun(%s,[%s]:%s,%s,%s)" (sprint_src source)
                                                                          (hlist pi_sprint_object_params opl)
                                                                          (pi_sprint_object_type ot)
                                                                          name
                                                                          (hlist pi_sprint_instr il) 
  | PI_monitor (source, sym_type, dbg) -> ""
  | PI_nop -> box 2 "%s" "PI_nop"  
  
let pi_print_instr_list file instrl =
  out (sprintf "Emitting pi-tree (Process Instrcution) file <%s.pi>..." file);
  let oc = 
    try open_out (sprintf "%s%s.pi" compiler.t_output file) with
    _ -> error 0 (sprintf "Can't open file <%s%s>." compiler.t_output file) in
  List.iter (fun instr -> let str = pi_sprint_instr instr in
                          output_string oc str) instrl;
  close_out oc
  



let rec cp_ssprint_value va =
  match va with
  | V_int i64 -> sprintf "%s" (Int64.to_string i64)
  | V_float flt -> sprintf "%f" flt
  | V_string str -> sprintf "\"%s\"" str
  | V_char ch -> sprintf "'%c'" ch
  | V_logic lstr -> sprintf "%s" lstr
  | V_bool bo -> sprintf "%b" bo
  | V_list vl -> sprintf "[%s]" (hlist cp_ssprint_value vl);
  | V_z -> "V_z"
  | V_time (i64,timeunit) -> sprintf "%s %s" (Int64.to_string i64) (ast_sprint_timeunit timeunit)  
  | V_freq (i64,frequnit) -> sprintf "%s %s" (Int64.to_string i64) (ast_sprint_frequnit frequnit)
  | V_null -> "NULL"
  
   
let rec cp_sprint_object_name ot =
  match ot with
  | OT_const co -> co.co_name 
  | OT_named_value (name,va) -> name  
  | OT_signal co -> co.co_name
  | OT_reg co -> co.co_name
  | OT_var co -> co.co_name
  | OT_component st -> st.st_name
  | OT_struct st -> st.st_name
  | OT_array at -> at.at_name
  | OT_array_sel (at,n) -> sprintf "%s.[%d]" at.at_name n
  | OT_object ao -> ao.ao_name
  | OT_value va -> cp_ssprint_value va
  | OT_channel ch -> ch.ch_obj.co_name
  | OT_queue qu -> qu.qu_obj.co_name
  | OT_reference (name,vl) -> sprintf "REF(%s,[%s])" name (hlist (sprintf "%d") vl)

  


let rec cp_sprint_object_params op =
  match op with
  | OD_sub (a,b) -> sprintf "[%d,%d]" a b
  | OD_index instr -> sprintf ".[%s]" (cp_sprint_instr instr)
  | OD_conv dt -> sprintf "%s:" 
    (match dt with
     | DT_logic _ -> "to_logic";
     | DT_int _ -> "to_int";
     | DT_char -> "to_char";
     | DT_bool -> "to_bool";
     | _ -> "TO");
  | OD_sel al -> sprintf ".%s" (hlist (sprintf "%d") (Array.to_list al));  
  | OD_sel_obj  instr -> sprintf ".%s" (cp_sprint_instr instr)
  | OD_lneg -> "not"
  | OD_aneg -> "-"
 
and cp_sprint_instr instr = 
  match instr with
  | PI_obj (opl, ot) -> 
    sprintf "%s%s" (hlist cp_sprint_object_params opl) (cp_sprint_object_name ot)
  | PI_tuple il -> 
    sprintf "{%s}" (hlist cp_sprint_instr il)
  | PI_list il -> ibox 2 (ilist pi_sprint_instr il)
  | PI_block (il, bf) ->  
      if (List.length il) > 1 then
            sprintf "begin\n%s\nend%s;\n"
                               (ibox 2 (ilist cp_sprint_instr il))
                               (
                                 let bpl = List.filter (fun bp ->
                                    match bp with
                                    | BP_unroll 
                                    | BP_locked 
                                    | BP_bind 
                                    | BP_inline -> true;
                                    | _ -> false) bf.bf_params in
                                 if bpl <> [] then 
                                 begin
                                  sprintf " with %s" (hlist (fun bp ->
                                     match bp with
                                     | BP_unroll -> "unroll";
                                     | BP_locked -> "locked";
                                     | BP_bind -> "bind";
                                     | BP_inline -> "inline";
                                     | _ -> ""
                                     ) bpl);
                                 end
                                 else "";
                                )
      else
        (ilist cp_sprint_instr il);
                                                         

  | PI_arithm (op, op1, op2) -> sprintf "(%s %s %s)" (cp_sprint_instr op1) (op_name op) (cp_sprint_instr op2)
  | PI_bool (kind , op, op1, op2) -> sprintf "(%s%s%s)" (cp_sprint_instr op1) (op_name op) (cp_sprint_instr op2)
  | PI_concat (op1, op2) -> sprintf "(%s@%s)" (cp_sprint_instr op1) (cp_sprint_instr op2)
  | PI_assign (source, lhs, rhs) -> sprintf "%s <- %s;"  (cp_sprint_instr lhs) (cp_sprint_instr rhs)
  | PI_waitfor (source, epxr, cyc, timeunit, b_0, b_1) -> ""
  | PI_branch (source, expr, b_0, b_1) -> 
    sprintf "if %s then\n%s\n%s" (cp_sprint_instr expr)
                                 (ibox 2 (cp_sprint_instr b_0))
                                 (if b_1 <> PI_nop then sprintf "else\n%s\n" (ibox 2 (cp_sprint_instr b_1))
                                                   else "") 

  | PI_forloop (source, expr, dir, lim1, lim2, b) -> 
    sprintf "for %s = %s %s %s do\n%s"  (cp_sprint_instr expr)
                                         (cp_sprint_instr lim1)
                                         (if dir = '+' then "TO" else "DOWNTO")                                             
                                         (cp_sprint_instr lim2)                                             
                                         (ibox 2 (cp_sprint_instr b)) 

  | PI_loop (source, kind, expr, b) -> 
    sprintf "%s do\n%s" (match kind with Loop -> "always"|Loop_while -> sprintf "while %s" (cp_sprint_instr expr))
                        (ibox 2 (cp_sprint_instr b)) 
  | PI_select (source, expr, cl) -> 
    sprintf "match %s with\n%s" (cp_sprint_instr expr)
                              (ibox 2 (cp_sprint_instr cl))
  | PI_case (source, exprl, b) -> 
    sprintf "when %s:\n%s"(if exprl <> [] then (vlist cp_sprint_instr exprl) else "others")
                           (ibox 2 (cp_sprint_instr b))
  | PI_try (b, cl) -> 
    sprintf "try\n%s\nwith\n%s" (ibox 2 (cp_sprint_instr b))
                              (ibox 2 (cp_sprint_instr cl))
  | PI_raise ex -> sprintf "raise %d;" ex;
  | PI_map (source, lhs, rhs) ->  
    sprintf "%s << %s;" (cp_sprint_instr lhs) (cp_sprint_instr rhs)
  | PI_fun (source, (opl, ot), name, il) -> 
    sprintf "%s(%s);" name (hlist cp_sprint_instr il) 
  | PI_monitor (source, sym_type, dbg) -> ""
  | PI_nop -> box 2 "%s" "NOP;"  
  
let cp_print_instr_list file instrl =
  out (sprintf "Emitting cp-code (Process Instrcution) file <%s.cp>..." file);
  let oc = 
    try open_out (sprintf "%s%s.cp" compiler.t_output file) with
    _ -> error 0 (sprintf "Can't open file <%s%s>." compiler.t_output file) in
  List.iter (fun instr -> let str = cp_sprint_instr instr in
                          output_string oc str) instrl;
  close_out oc
  
 
(*
**********************************
** MicroCode type...
**********************************
*)

let ui_sprint_range r =
    match r with
    | Some (a,b) -> sprintf "Some(%d,%d)" a b;
    | None -> "None"

let ui_sprint_index i =
    let datastr da =
          match da with
          | UC_reg uo -> sprintf "UC_reg(%s)" uo.uo_name;
          | UC_var uo -> sprintf "UC_var(%s)" uo.uo_name;
          | UC_sig uo -> sprintf "UC_sig(%s)" uo.uo_name;
          | UC_temp ut -> sprintf "UC_temp(%s)" ut.ut_name; 
          | _ -> "" in
    match i with
    | Some (UA_data (UC_val uv)) -> 
    begin
          match uv.uv_val with
          | V_int i64 -> sprintf "Some(%d)" (Int64.to_int i64);
          | _ -> "";
    end;
    | Some (UA_data da) -> sprintf "Some(%s)" (datastr da);
    | _ -> "None"

let ui_sprint_dt dt =
    match dt with
    | DT_logic n -> sprintf "DT_logic(%d)" n;
    | DT_int n -> sprintf "DT_int(%d)" n;
    | DT_char -> "DT_char";
    | DT_bool -> "DT_bool";
    | DT_string n -> sprintf "DT_string(%d)" n;
    | DT_object on -> sprintf "DT_object(%s)" on;
    | DT_lneg -> "DT_lneg";
    | DT_aneg -> "DT_aneg";
    | DT_bneg -> "DT_bneg";
    | DT_natural n -> sprintf "DT_natural(%d)" n

let ui_sprint_conv ct = 
  match ct with
  | Some dt -> ui_sprint_dt dt
  | None -> "" 

let ui_sprint_flags fl = 
  hlist (fun f->
          match  f with
          | UO_lhs -> "UO_lhs";
          | UO_rhs -> "UO_rhs";
          | UO_loc -> "UO_loc";
          | UO_ot ot -> sprintf "UO_ot(%s)" (name_of_ot ot);
          ) fl
 
let ui_sprint_type t =
  sprintf "DT=%s;ET=%s;TC=%s;\nPT=%s;LT=%s;ST=%s;NT=%s;" 
          (ui_sprint_dt t.uo_data_type)        
          (ui_sprint_dt t.uo_expr_type)        
          (
          if t.uo_conv <> None then
            ui_sprint_conv t.uo_conv
          else
            "*"
          )
          (if t.uo_phys_type <> None then ui_sprint_dt (get_some t.uo_phys_type) else "*")
          (if t.uo_log_type <> None then ui_sprint_dt (get_some t.uo_log_type) else "*")
          (if t.uo_sub_type <> None then ui_sprint_dt (get_some t.uo_sub_type) else "*")
          (if t.uo_sign <> None then ui_sprint_dt (get_some t.uo_sign) else "*")
let rec ui_sprint_sel sel =
  let str = ref "" in
  List.iter (fun s ->
	if !str = "" then str := ui_sprint_uc s 
	  	  	  	 else str := sprintf "%s,\n%s" !str (ui_sprint_uc s);
	) sel;
  !str
and ui_sprint_addr faddr =
  sprintf "(ind0 => %s)" (faddr 0)   
and ui_sprint_uo uo =
  box 1 "{%s}" (sprintf "uo_name=%s;uo_range=%s;uo_index=%s;\nuo_flags=[%s];\nuo_sel=%s;\nuo_addr=%s;\nuo_type=%s"
                    uo.uo_name
                    (ui_sprint_range uo.uo_range)
                    (ui_sprint_index uo.uo_index)
                    (ui_sprint_flags uo.uo_flags)
					(box 9 "[%s]" (ui_sprint_sel uo.uo_sel))
					(ui_sprint_addr uo.uo_addr)
                    (box 9 "{%s}" (ui_sprint_type uo.uo_type)))

and ui_sprint_uat uat =
  box 1 "{%s}" (sprintf "uat_name=%s;uat_rang=%s;\nuat_flags=[%s];\nuat_type=%s"
                    uat.uat_name
                    (ui_sprint_range uat.uat_range)
                    (ui_sprint_flags uat.uat_flags)
                    (box 10 "{%s}" (ui_sprint_type uat.uat_type)))

and ui_sprint_ut ut =
  box 1 "{%s}" (sprintf "ut_name=%s;ut_range=%s;\nut_flags=[%s];\nut_type=%s" 
                ut.ut_name 
                (ui_sprint_range ut.ut_range)
                (ui_sprint_flags ut.ut_flags)
                (box 9 "{%s}" (ui_sprint_type ut.ut_type)))
 

and ui_sprint_val v =
    match v with
    | V_int i -> Int64.to_string i;
    | _ -> ""

and ui_sprint_ul ul =
    sprintf "{ul_flags=[%s];ul_type=%s}" 
                (ui_sprint_flags ul.ul_flags)
                (ui_sprint_type ul.ul_type)

and ui_sprint_ua ua =
    sprintf "{ua_flags=[%s];ua_type=%s}"
                    (ui_sprint_flags ua.ua_flags)
                    (sprint_dt ua.ua_type) 

and ui_sprint_uc da =
    let datastr = ui_sprint_uc in
    match da with
    | UC_reg uo -> box 0 "UC_reg %s" (ui_sprint_uo uo);
    | UC_var uo -> box 0 "UC_var %s" (ui_sprint_uo uo);
    | UC_sig uo -> box 0 "UC_sig %s" (ui_sprint_uo uo);
    | UC_chan uo -> box 0 "UC_chan %s" (ui_sprint_uo uo);
    | UC_queue uo -> box 0 "UC_queue %s" (ui_sprint_uo uo);
    | UC_val uv -> box 0 "UC_val {%s}" (sprintf "uv_type={%s};uv_val=%s" (ui_sprint_type uv.uv_type) (ui_sprint_val uv.uv_val));
    | UC_alu ua -> box 0 "UC_alu %s" (ui_sprint_ua ua);
    | UC_immed id -> sprintf "UC_immed (%d)" id;
    | UC_bool id -> sprintf "UC_bool (%d)" id;
    | UC_temp ut -> box 0 "UC_temp %s" (ui_sprint_ut ut);
    | UC_array uat -> box 0 "UC_array %s" (ui_sprint_uat uat);
    | UC_list ul -> 
            let str = ref "" in
            List.iter (fun da -> str := !str ^ 
                              (if !str <> "" then "@" else "")
                              ^ (datastr da)) ul.ul_list;
            sprintf "UC_list [%s,%s]" (ui_sprint_ul ul) !str;
    | UC_sel us -> 
            let data,sel = 
                let ds,ss=ref "",ref "" in
                List.iter (fun sel ->
                    match sel with
                    | UC_val uv ->
                    let i =
                        match uv.uv_val with
                        | V_int w -> Int64.to_int w;
                        | _ -> error 505672 "" in
                    ds := !ds ^ " " ^ (datastr us.us_obj);
                    ss := !ss ^ " " ^ (string_of_int i);
                    | _ -> 
                    ds := !ds ^ " " ^ (datastr us.us_obj)^"...";
                    ss := !ss ^ " " ^ (datastr sel);
                  ) us.us_sel; 
                !ds, !ss 
                in
    box 0 "UC_sel {%s}" (sprintf "us_name=%s;\nus_sel=%s;\nus_data=%s}" us.us_name sel data)
 
  
let rec ui_sprint_instr instr =
  let locstr ul = 
        match ul with
        | UC_next -> "NEXT";
        | UC_label str -> str;
        in
  box 1 "{%s}\n" (sprintf "ui_code=%s;\nui_frame=%s"
    (match instr.ui_code with 
    | Label str -> 
      sprintf "Label (%s)" str
    | Bind n -> 
      sprintf "Bind (%d)" n
    | Move (dst,src) -> 
      box 14 "Move (%s)" (sprintf "%s,\n%s" 
                                  (ui_sprint_uc dst) 
                                  (ui_sprint_uc src));
    | Expr (opl,dst,src1,src2) -> 
      box 14 "Expr (%s)" (sprintf "%s,\n%s,\n[%s],\n%s"  
                                  (ui_sprint_uc dst) 
                                  (ui_sprint_uc src1)
                                  (hlist (fun x-> op_name x) opl)
                                  (ui_sprint_uc src2));
    | Falsejump (expr,label) -> sprintf "Falsejump (%s,%s)" (ui_sprint_uc expr) (locstr label);
    | Jump label -> sprintf "Jump (%s)" (locstr label)
    | Special instr -> box 8 "Special (%s)" (pi_sprint_instr instr); 
    | Fun ((opl,ot),name,uargl) -> sprintf "Fun ([%s]:%s.%s, [...])" 
                                           (hlist pi_sprint_object_params opl) 
                                           (pi_sprint_object_type ot) 
                                           name;
    | Nop -> "Nop")
    (bf_sprint_frame 10 instr.ui_frame))
 
 
let ui_print_instr_list file instrl =
  out (sprintf "Emitting ui-list (MicroCode) file <%s.ui>..." file);
  let oc = 
    try open_out (sprintf "%s%s.ui" compiler.t_output file) with
    _ -> error 0 (sprintf "Can't open file <%s%s>." compiler.t_output file) in
  List.iter (fun instr -> let str = ui_sprint_instr instr in
                          output_string oc str) instrl;
  close_out oc


(*
*********************************
** State type 
*********************************
*)
let rec st_sprint_synth_data sd =
  match sd with
  | Data_in str -> sprintf "Data_in <%s>" str
  | Data_sens str -> sprintf "Data_sens <%s>" str
  | Data_trans_sens str -> sprintf "Data_trans_sens <%s>" str
  | Data_cond_sens str -> sprintf "Data_cond_sens <%s>" str
  | Data_out str -> sprintf "Data_out <%s>" str
  | Data_signal str -> sprintf "Data_signal <%s>" str
  | Data_trans str -> sprintf "Data_trans <%s>" str
  | Data_top str -> sprintf "Data_top <%s>" str
  | Data_top_def str -> sprintf "Data_top_def <%s>" str
  | Data_cond str -> sprintf "Data_cond <%s>" str
  | Data_process strl -> box 0 "Data_process <%s>" (vlist (fun s -> s) strl)
  | Data_def (str1,str2) -> sprintf "Data_def <%s>,<%s>" str1 str2
  | Data_trans_def (str1,str2) -> sprintf "Data_trans_def <%s>,<%s>" str1 str2

let rec st_sprint_case_state cs =
  box 1 "{%s}" (sprintf "cs_data=%s;\ncs_next=%s\n"
                        ((align 8 (vlist st_sprint_synth_data cs.cs_data)))
                        (st_sprint_next cs.cs_next))
and st_sprint_next next =
  match next with
  | Next str -> sprintf "Next(%s)" str
  | Next_instr -> "Next_instr"
  | Branch (sdl,next1,next2) -> 
      box 14 "Branch(%s)" (sprintf "[%s],\n%s,\n%s\n"  
                                  (align 1 (vlist st_sprint_synth_data sdl))
                                  (align 0 (st_sprint_next next1))
                                  (align 0 (st_sprint_next next2)));
  | Select (sdl,csl) -> 
      box 14 "Select(%s)" (sprintf "[%s],\n[%s]\n"  
                                  (align 1 (vlist st_sprint_synth_data sdl))
                                  (align 1 (vlist st_sprint_case_state csl)))


let rec st_sprint_state state =
  box 2 "{%s}\n" (
    sprintf "\n\ns_name=%s;\ns_next=%s;\ns_data=%s;\ns_block=%s;"
            state.s_name
            (st_sprint_next state.s_next)
            (box 8 "[%s]" (vlist st_sprint_synth_data state.s_data))
            (bf_sprint_frame 9 state.s_block)
  
  )

let rec st_sprint_state_block stb =
    match stb with 
    | State st -> box 2 "State(%s)\n" (st_sprint_state st);
    | State_block stl -> box 2 "State_block[%s]\n" (sprintf "\n\n%s" (vlist st_sprint_state_block stl));
    | State_top st -> box 2 "State_top(%s)\n" (st_sprint_state st)
    

let st_print_state_block_list file stbl =
  out (sprintf "Emitting st-list (VHDL State) file <%s.st>..." file);
  let oc = 
    try open_out (sprintf "%s%s.st" compiler.t_output file) with
    _ -> error 0 (sprintf "Can't open file <%s%s>." compiler.t_output file)  in
  List.iter (fun stb -> let str = st_sprint_state_block stb in
                        output_string oc str) stbl;
  close_out oc


(*
** Modules
*)

let sprint_arg_desc ad =
  sprintf "%s:%s:%s" ad.arg_label 
    (match ad.arg_type with Arg_lhs -> "LHS" | Arg_rhs -> "RHS" | Arg_lrhs -> "LRS")
    (sprint_dt ad.arg_data_type) 
