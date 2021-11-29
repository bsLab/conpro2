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
**    $CREATED:     1.3.2006
**    $VERSION:     2.05
**
**    $INFO:
**
**  Arithmetic-Logic-Unit (ALU) implementation.
**  
**
**  The ALU consists of the following input signals:
**
**  alu_op1,alu_op2: ALU operands
**  alu_op: ALU function selecect
**
**
**  Output signals:
**
**  alu_res: result of actual operation evaluation
**  alu_bool_res: result of relational operation
**
**  Output registers:
**
**  alu_reg: holds last result
**  alu_bool_reg:
**
**    
**  Note: Distinguish relational operators resulting in a boolean
**        output signal alu_bool only, and additional boolean operators
**        like (b1 OR b2), evaluated with the actual alu_bool and
**        the last alu_bool_reg value. 
**        Boolean operations are not evaluated with the ALU!
**
**          (x = 100) or (x = 101) and (y = 102) 
**  
**    $ENDOFINFO
**
*)

open Cp_common
open Cp_symbol
open Cp_types
open Cp_syntax
open Cp_utils
open Cp_data
open Cp_analysis
open Printf
open Cp_vhdl

(*
** Arithmetic buildung block generators coupled to ALU.
*)

(*
** Create combinational reduced n*n=n bit booth-2 multiplier
*)
let emit_booth2_mul alu =    
    let vhdl_ind = ref 0 in
    let strl = ref [] in
    let vhdl str =
    let spaces n = String.make n ' ' in
        strl := !strl @ [(spaces !vhdl_ind)^str] 
        in
    let emit n =
      let aux = ref [
                sprintf "signal ALU_%s_MUL_x: %s;"
                        alu.alu_name
                        (obj_decl_type (DT_logic n));  
                sprintf "signal ALU_%s_MUL_y: %s;"
                        alu.alu_name
                        (obj_decl_type (DT_logic n));  
                sprintf "signal ALU_%s_MUL_p: %s;"
                        alu.alu_name
                        (obj_decl_type (DT_logic n));  
                sprintf "signal ALU_%s_MUL_aux_ex: %s;"
                        alu.alu_name
                        (obj_decl_type (DT_logic (n+1)));  
                sprintf "signal ALU_%s_MUL_aux_y2: %s;"
                        alu.alu_name 
                        (obj_decl_type (DT_logic n));
            ] in
      vhdl "-- BEGIN Booth-2 Multiplier Implementation";
      vhdl (sprintf "ALU_%s_MUL_aux_ex <= ALU_%s_MUL_x & '0';"
                    alu.alu_name alu.alu_name);
      vhdl (sprintf "ALU_%s_MUL_aux_y2 <= ALU_%s_MUL_y(%d downto 0) & '0';"
                      alu.alu_name alu.alu_name (n-2));
      vhdl (sprintf "ALU_%s_MUL_aux_p_0 <= %s;"
                      alu.alu_name 
                      (val_str (DT_logic n) (V_int Int64.zero)));
      let width = ref n in
      for i = 0 to (n+1)/2-1
      do
        (*
        ** Actual cell data width
        *)
        aux := !aux @ [
                    sprintf "signal ALU_%s_MUL_aux_p_%d: %s;"
                            alu.alu_name i
                            (obj_decl_type (DT_logic !width));
                    sprintf "signal ALU_%s_MUL_aux_xi_%d: %s;"
                            alu.alu_name i
                            (obj_decl_type (DT_logic (3)));
                    sprintf "signal ALU_%s_MUL_aux_s_%d: %s;"
                            alu.alu_name i
                            (obj_decl_type (DT_logic !width));
                ];    

        vhdl (sprintf "-- Booth-Cell %d" i);
        vhdl (sprintf "ALU_%s_MUL_aux_xi_%d <= ALU_%s_MUL_aux_ex(%d downto %d);"
                      alu.alu_name i alu.alu_name 
                      (2*i+2) (2*i));
        if i < (n+1)/2-1 then
            vhdl (sprintf "ALU_%s_MUL_aux_p_%d <= ALU_%s_MUL_aux_s_%d(%d downto %d);"
                      alu.alu_name (i+1) 
                      alu.alu_name i 
                      (!width-1) (2));

        vhdl (sprintf "ALU_%s_MUL_p(%d downto %d) <= ALU_%s_MUL_aux_s_%d(%d downto %d);"
                      alu.alu_name  
                      (2*i+1) (2*i)
                      alu.alu_name i
                      (1) (0));

        vhdl (sprintf "ALU_%s_MUL_MUX_%d: process("
                      alu.alu_name i);
        vhdl_ind := !vhdl_ind + 6;
        vhdl (sprintf "ALU_%s_MUL_aux_xi_%d," alu.alu_name i);
        vhdl (sprintf "ALU_%s_MUL_aux_p_%d," alu.alu_name i);
        vhdl (sprintf "ALU_%s_MUL_y," alu.alu_name);
        vhdl (sprintf "ALU_%s_MUL_aux_y2)" alu.alu_name);
        vhdl_ind := !vhdl_ind - 6;
        vhdl "begin";
        vhdl_ind := !vhdl_ind + 2;
        vhdl (sprintf "case ALU_%s_MUL_aux_xi_%d is"
                      alu.alu_name i);
        vhdl_ind := !vhdl_ind + 2;

        if i > 0 then
        begin
            vhdl (sprintf "when \"000\" | \"111\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d;"
                      alu.alu_name i  alu.alu_name i);
            vhdl (sprintf "when \"001\" | \"010\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d + ALU_%s_MUL_y(%d downto %d);"
                      alu.alu_name i  alu.alu_name i alu.alu_name
                      (!width-1) 0);
            vhdl (sprintf "when \"011\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d + ALU_%s_MUL_aux_y2(%d downto %d);"
                      alu.alu_name i  alu.alu_name i alu.alu_name
                      (!width-1) 0);
            vhdl (sprintf "when \"100\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d - ALU_%s_MUL_aux_y2(%d downto %d);"
                      alu.alu_name i  alu.alu_name i alu.alu_name
                      (!width-1) 0);
            vhdl (sprintf "when \"101\" | \"110\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d - ALU_%s_MUL_y(%d downto %d);"
                      alu.alu_name i  alu.alu_name i alu.alu_name
                      (!width-1) 0);
            vhdl "when others => null;"
        end
        else
        begin
            vhdl (sprintf "when \"000\" | \"111\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d;"
                      alu.alu_name i  alu.alu_name i);
            vhdl (sprintf "when \"001\" | \"010\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d + ALU_%s_MUL_y;"
                      alu.alu_name i  alu.alu_name i alu.alu_name);
            vhdl (sprintf "when \"011\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d + ALU_%s_MUL_aux_y2;"
                      alu.alu_name i  alu.alu_name i alu.alu_name);
            vhdl (sprintf "when \"100\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d - ALU_%s_MUL_aux_y2;"
                      alu.alu_name i  alu.alu_name i alu.alu_name);
            vhdl (sprintf "when \"101\" | \"110\" => ALU_%s_MUL_aux_s_%d <= ALU_%s_MUL_aux_p_%d - ALU_%s_MUL_y;"
                      alu.alu_name i  alu.alu_name i alu.alu_name);
            vhdl "when others => null;"
        end;

        vhdl_ind := !vhdl_ind - 2;
        vhdl "end case;";
        vhdl_ind := !vhdl_ind - 2;
        vhdl (sprintf "end process ALU_%s_MUL_MUX_%d;"
                      alu.alu_name i);
        width := !width - 2;
      done;
      vhdl "-- END Booth-2 Multiplier Implementation";
      !aux
        in

    match alu.alu_type with
    | DT_logic n ->
        let aux = emit n in
        aux, !strl
    | DT_int n ->
        let aux = emit n in
        aux, !strl
    | _ -> error 403424 ""

(*
** Emit ALU implementation
*)
let emit_alu pro =
  let tops = ref [] in
    let all_alu_ops = 
        let ops = ref [] in
        List.iter (fun alu ->
            ops := !ops @ (List.filter (fun op ->
                                not (List.mem op !ops) )
                                    alu.alu_ops);
            ) pro.pro_alu;
        !ops
        in

  List.iter (fun alu ->
    let alu_name = alu.alu_name in

    let use_relat = 
        List.mem OP_relat (List.map (fun op -> op_mode op) alu.alu_ops)
        in


    vhdl "-- Process Arithmetic Logic Boolean Unit: The Rechenwerk";
    if not (get_env_bool "alu_top_expr") then
        vhdl (sprintf "alu_%s: process(alu_%s_op1,alu_%s_op2,alu_%s_op)" 
                      alu_name alu_name alu_name alu_name)
    else
    begin
        vhdl (sprintf "alu_%s: process(" alu_name);
        vhdl_ind := !vhdl_ind + 8;
        vhdl (sprintf "alu_%s_op," alu_name);
        let len = List.length alu.alu_ops in
        let n = ref (List.length alu.alu_ops) in
        List.iter (fun op ->
                decr n;
                vhdl (sprintf "res_%s_%s%s" 
                              alu_name 
                              (alu_state_str op)
                              (if !n > 0 then "," else ""))
          ) alu.alu_ops;
        vhdl ")";
        vhdl_ind := !vhdl_ind - 8;
    end;
    vhdl "begin";
    vhdl_incr();
    (*
    ** Defaul values
    *)
    vhdl "-- Default values";
    vhdl (sprintf "alu_%s_res <= %s;" 
                  alu_name
                  (default_by_dt alu.alu_type));
    if use_relat then
       vhdl (sprintf "alu_%s_bool <= false;"
                     alu_name);


    vhdl (sprintf "case alu_%s_op is" alu_name);
    vhdl_incr();
    if not (get_env_bool "alu_top_expr") then
        List.iter (fun op ->
            match op_mode op with
            | OP_logic when (op = OP_lnot) -> ();
            | OP_logic
            | OP_arith ->
                vhdl (sprintf "when %s =>"
                          (alu_state_str op));
                vhdl_incr ();
                let op = op_vhdl op in
                if (vhdl_map op) <> op then
                  vhdl (sprintf "alu_%s_res <= %s(alu_%s_op1,alu_%s_op2);"
                              alu_name
                              (vhdl_map op)
                              alu_name
                              alu_name
                              )
                else
                  vhdl (sprintf "alu_%s_res <= alu_%s_op1 %s alu_%s_op2;"
                              alu_name
                              alu_name
                              op
                              alu_name
                              );
                vhdl_decr ();
            | OP_relat ->
                vhdl (sprintf 
                          "when %s =>"
                          (alu_state_str op));
                vhdl_incr ();
                let op = op_vhdl op in
                if (vhdl_map op) <> op then
                  vhdl (sprintf "alu_%s_bool <= %s(alu_%s_op1,alu_%s_op2);"
                              alu_name
                              (vhdl_map op)
                              alu_name
                              alu_name)
                else
                  vhdl (sprintf "alu_%s_bool <= alu_%s_op1 %s alu_%s_op2;"
                              alu_name
                              alu_name
                              op
                              alu_name);
                vhdl_decr ();
(*
            | _ -> error 966217 "Cp_alu.emit_alu: unexpected ALU mode";
*)
            | _ -> ();
          ) alu.alu_ops
    else
        (*
        ** All expressions are toplevel expressions with
        ** auxilliary signals!
        *)
        List.iter (fun op ->
            match op_mode op with
            | OP_logic when (op = OP_lnot) -> ();
            | OP_logic
            | OP_arith ->
            begin
                vhdl (sprintf "when %s => alu_%s_res <= res_%s_%s;"
                          (alu_state_str op)
                          alu_name
                          alu_name
                          (alu_state_str op));
                
                match op with
                | OP_mul when ((find_modgen "multiplier") = "booth")->
                    tops := !tops @ [ 
                       sprintf  "res_%s_%s <= ALU_%s_MUL_p;"
                                alu_name (alu_state_str op)
                                alu_name;
                       sprintf  "ALU_%s_MUL_x <= alu_%s_op1;"
                                alu_name 
                                alu_name;
                       sprintf  "ALU_%s_MUL_y <= alu_%s_op2;"
                                alu_name 
                                alu_name;
                     ];
                | op when (vhdl_map (op_vhdl op)) <> (op_vhdl op) ->
                    tops := !tops @ [ 
                      (sprintf 
                        "res_%s_%s <= %s(alu_%s_op1,alu_%s_op2);"
                        alu_name
                        (alu_state_str op)
                        (vhdl_map (op_vhdl op))
                        alu_name
                        alu_name);
                     ];
                | _ ->
                    tops := !tops @ [ 
                      (sprintf 
                        "res_%s_%s <= alu_%s_op1 %s alu_%s_op2;"
                        alu_name
                        (alu_state_str op)
                        alu_name
                        (op_vhdl op)
                        alu_name);
                     ];
            end;
            | OP_relat ->
                let op' = op_vhdl op in
                vhdl (sprintf 
                          "when %s => alu_%s_bool <= res_%s_%s;"
                          (alu_state_str op)
                          alu_name
                          alu_name
                          (alu_state_str op));
                if (vhdl_map op') <> op' then
                  tops := !tops @ [
                  (sprintf 
                    "res_%s_%s <= %s(alu_%s_op1,alu_%s_op2);"
                    alu_name
                    (alu_state_str op)
                    (vhdl_map op')
                    alu_name
                    alu_name);
                    ]
                else
                  tops := !tops @ [
                  (sprintf 
                    "res_%s_%s <= alu_%s_op1 %s alu_%s_op2;"
                    alu_name
                    (alu_state_str op)
                    alu_name
                    op'
                    alu_name);
                    ];
(*
            | _ -> error 759680 "Cp_alu.emit_alu: unexpected ALU mode";
*)
            | _ -> ();
          ) alu.alu_ops;

    vhdl "when Alu_nop => null;";
    let others = (List.length all_alu_ops) > (List.length alu.alu_ops) in
    if others then vhdl "when others => null;";
    vhdl_decr();
    vhdl "end case;";
    vhdl_decr();
    vhdl (sprintf "end process alu_%s;" alu_name);
    ) pro.pro_alu;
    !tops

(*
** ALU operations
*)
let emit_alu_states pro =
    let alu_ops = 
        let ops = ref [] in
        List.iter (fun alu ->
            ops := !ops @ (List.filter (fun op ->
                                not (List.mem op !ops) )
                                    alu.alu_ops);
            ) pro.pro_alu;
        !ops
        in
    let alu_names =
        let names = ref [] in
        List.iter (fun alu ->
            names := !names @ [alu.alu_name];
            ) pro.pro_alu;
        !names
        in


    vhdl "type alu_ops is (";
    vhdl_incr ();
    List.iter (fun op -> 
            vhdl (sprintf "%s," (alu_state_str op));
      ) alu_ops;
    vhdl (sprintf "%s" (alu_state_str OP_nop));
    vhdl ");";
    vhdl_decr ();
    
    List.iter (fun alu_name ->
        vhdl (sprintf "signal alu_%s_op: alu_ops;" alu_name);
      ) alu_names


(*
** ALU signals and registers
*)
    
let emit_aux pro =
  let auxl = ref [] in
  let topl = ref [] in
  let vhdl_ind = ref 0 in
  let strl = ref [] in
  let vhdl str =
    let spaces n = String.make n ' ' in
    auxl := !auxl @ [(spaces !vhdl_ind)^str] 
    in
  List.iter (fun alu ->
    let alu_name = alu.alu_name in
    let alu_type,alu_width,alu_vec =
      match alu.alu_type with
      | DT_logic n -> "std_logic_vector",n,
                      sprintf "std_logic_vector(%d downto 0)" (n-1)
      | DT_int n -> "signed",n,
                    sprintf "signed(%d downto 0)" (n-1);
      | _ -> error 821687 "Cp_alu.emit_aux: alu_type";
      in
    
    if (get_env_bool "alu_top_expr") then
    begin
        (*
        ** All expressions are moved to toplevel.
        *)

        List.iter (fun op ->
                match op_mode op with
                | OP_relat ->
                    vhdl (sprintf "signal res_%s_%s: boolean;"
                                  alu_name
                                  (alu_state_str op)
                                  );
                | OP_logic
                | OP_arith ->
                    vhdl (sprintf "signal res_%s_%s: %s;"
                                  alu_name
                                  (alu_state_str op)
                                  alu_vec);
                | _ -> ();
            ) alu.alu_ops;
    end;
    (*
    ** Now ALU signals and registers
    *)
    vhdl (sprintf "signal alu_%s_op1: %s;" alu_name alu_vec);
    vhdl (sprintf "signal alu_%s_op2: %s;" alu_name alu_vec);
    if (List.mem OP_relat (List.map (fun op -> op_mode op) alu.alu_ops)) then
    begin
      vhdl (sprintf "signal alu_%s_bool: boolean;"
                    alu_name
                    );
      vhdl (sprintf "signal alu_%s_bool_reg: boolean;"
                    alu_name
                    );
    end;
    vhdl (sprintf "signal alu_%s_res: %s;" alu_name alu_vec);
    vhdl (sprintf "signal alu_%s_reg: %s;" alu_name alu_vec);
    if (find_modgen "multiplier") = "booth" &&
       List.mem OP_mul alu.alu_ops then
    begin
        let auxl',topl' = emit_booth2_mul alu in
        auxl := !auxl @ auxl';
        topl := !topl @ topl';
    end;
    ) pro.pro_alu;
  !auxl, !topl
