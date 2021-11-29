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
**    $CREATED:     19.3.2006
**    $VERSION:     2.03
**
**    $INFO:
**
**  Core module.
**
**    $ENDOFINFO
**
*)
open Cp_common
open Cp_types
open Cp_symbol
open Cp_utils
open Cp_data
open Cp_analysis
open Cp_ucode
open Cp_expr
open Cp_syntax
open Printf
open Cp_Core_head

(*
** Emit monitor object, returns synth_data list
*)
let rec emit_mon instr modu pro sym =
  let top_defs = ref [] in
  match sym with
  | Sym_mon (bdebug,sym) ->
  begin
    match sym with
    | Sym_obj obj ->
    begin
        (*
        ** Monitor signals always of type logic!
        *)
        match obj with
        | OT_reg co ->
            let rec fifo l =
              match l with
              | (Obj_schedule m)::tl -> 
                m = "fifo"; 
              | _ :: tl -> fifo tl;
              | [] -> false in
            let fifo = fifo co.co_flags in

            let prd,pwr,pblocked,pall = get_procs co in
            let convl,convr = vhdl_convert co.co_type (DT_logic 0) in
            (*
            ** With auxilliary signals, too.
            *)
            let s_locked = 
                    if co.co_guard <> None then
                    [
                    Data_top
                    (sprintf "MON_REG_%s_locked <= REG_%s_locked;"
                             co.co_name co.co_name);     
                    ] else [] in
            let s_we = 
                    let l = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            Data_top
                            (sprintf "MON_REG_%s_%s_we <= REG_%s_%s_we;"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name);
                            ];
                            if co.co_guard <> None then
                            l := !l @ [ 
                            Data_top
                            (sprintf "MON_REG_%s_%s_gd <= REG_%s_%s_gd;"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name);
                            ];
                            ) co.co_writer;
                    !l
                    in
            let s_sched =
              if not fifo then [] else
              begin
                let n = List.length pall in
                let w = const_width (V_int (Int64.of_int n)) in
                let dt_prio = DT_logic w in
                let s = ref [] in
                List.iter (fun pro ->
                        s := !s @ [sprintf "MON_REG_%s_%s_QUEUED <= REG_%s_%s_QUEUED;"
                                   co.co_name pro.pro_name
                                   co.co_name pro.pro_name;
                           sprintf "MON_REG_%s_%s_PRIO <= REG_%s_%s_PRIO;"
                                   co.co_name pro.pro_name
                                   co.co_name pro.pro_name;
                        ];
                       ) pall;
                s := !s @ [sprintf "MON_REG_%s_HEAD <= REG_%s_HEAD;"
                                 co.co_name 
                                 co.co_name;
                        sprintf "MON_REG_%s_TAIL <= REG_%s_TAIL;"
                                 co.co_name 
                                 co.co_name;
                        sprintf "MON_REG_%s_OWNER <= REG_%s_OWNER;"
                                 co.co_name 
                                 co.co_name;
                     ];
                !s
              end in
            ([   
              Data_top 
              (sprintf "MON_REG_%s <= %s%s%s%s;"  
                    co.co_name convl 
                    (if List.mem Obj_local co.co_flags then "" else "REG_")
                    co.co_name convr);
              ] 
                @ s_locked @ s_we 
                @(List.map (fun s -> Data_top s) s_sched));
              
        | OT_channel ch ->
            let co = ch.ch_obj in
            let l = ref [] in
            let pro_gds = ref [] in
            let ao = ch.ch_ao in
            (*
            ** Determine connection model
            *)
            let rn = List.length co.co_reader in
            let wn = List.length co.co_writer in
            let uni = rn=1 && wn = 1 in
            let bi =
                (rn=2 && wn=2) &&
                  (
                    co.co_reader = co.co_writer ||
                    (List.rev co.co_reader) = co.co_writer ||
                    co.co_reader = (List.rev co.co_writer)
                  ) in
            let model_conn = 
                if uni  then Chan_uni
                    else if bi then Chan_bi
                    else Chan_multi in

            let model_buf =  List.hd ch.ch_model in
             
            List.iter (fun pro ->
                l := !l @ [Data_top
                            (sprintf "MON_%s_%s_WE <= CHAN_%s_%s_WE;"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                        ];
                if not (List.mem pro !pro_gds) then
                begin
                  l := !l @ [Data_top
                              (sprintf "MON_%s_%s_GD <= CHAN_%s_%s_GD;"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ] @ (if bi then [ 
                            Data_top (sprintf "MON_%s_%s_GD_A <= CHAN_%s_%s_GD_A;"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                            Data_top (sprintf "MON_%s_%s_GD_B <= CHAN_%s_%s_GD_B;"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                                ] else []); 
                  pro_gds := !pro_gds @ [pro];
                end;                              
              ) co.co_writer;
            List.iter (fun pro ->
                l := !l @ [Data_top
                            (sprintf "MON_%s_%s_RE<= CHAN_%s_%s_RE;"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                if not (List.mem pro !pro_gds) then
                begin
                  l := !l @ [Data_top
                              (sprintf "MON_%s_%s_GD <= CHAN_%s_%s_GD;"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ] @ (if bi then [ 
                            Data_top (sprintf "MON_%s_%s_GD_A <= CHAN_%s_%s_GD_A;"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                            Data_top (sprintf "MON_%s_%s_GD_B <= CHAN_%s_%s_GD_B;"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                                ] else []); 
                  pro_gds := !pro_gds @ [pro];
                end;
              ) co.co_reader;
            !l
        | OT_queue qu ->
            let co = qu.qu_obj in
            [
            Data_top
                (sprintf "MON_%s_empty <= QUEUE_%s_empty;" 
                     co.co_name co.co_name);  
            Data_top
                (sprintf "MON_%s_full <= QUEUE_%s_full;" 
                     co.co_name co.co_name);  
            Data_top
                (sprintf "MON_%s_addr_rd <= QUEUE_%s_addr_rd;" 
                     co.co_name co.co_name);  
            Data_top
                (sprintf "MON_%s_addr_wr <= QUEUE_%s_addr_wr;" 
                     co.co_name co.co_name);  
            ]
            
        | OT_signal co ->
            let convl,convr = vhdl_convert co.co_type (DT_logic 0) in
            [
            Data_top
            (sprintf "MON_%s <= %s%s%s;"  
                    co.co_name convl co.co_name convr);
            ]
        | OT_object ao ->
            let rl = ao.ao_type.ta_rules in
            let sl = ref [] in
            let el,dl = rl.rl_top_vcode instr modu pro in
            List.iter (fun e -> sl := !sl @ [Data_top e]) el;
            List.iter (fun d -> sl := !sl @ [Data_top_def d]) dl;
            !sl
        | OT_array at ->
        begin
            if List.mem AT_block at.at_flags then       
                error 0 "Monitors for array block not supported!";
            let dl = ref [] in
            (*
            ** Must be an abstract object. AO are extracted from
            ** instruction, not from Symbol! Therefore we need
            ** only to pass one object...
            *)
            let ot' = at.at_objs.(0) in
            match ot' with
            | OT_object ao ->
               let sym' = Sym_mon (bdebug,Sym_obj ot') in
               dl := !dl @ 
                    (emit_mon instr modu pro sym');
               !dl
            | _ -> error 0 "Monitor for unsupported array type found.";          
        end;
        | OT_struct st ->
        begin
          let dl = ref [] in
          List.iter (fun ot' -> 
            let sym' = Sym_mon (bdebug,Sym_obj ot') in
            dl := !dl @ (emit_mon instr modu pro sym')) st.st_objs;
          !dl;
        end;
        | _ -> error 825807 ""
    end;
    | Sym_pro pro -> 
        let emit_states () =
            let states = get_state_names pro in    
            let len = List.length states in
            let bits = const_width (V_int (i64 len)) in
            let dt = DT_logic bits in
            let i = ref Int64.zero in
            (List.map (fun s -> 
                    i := Int64.add !i Int64.one;
                    Data_top (sprintf "    %s when pro_state = %s else" 
                                (val_str dt (V_int !i))
                                s);
                ) states) @
            [
                Data_top
                (sprintf "    %s;"
                        (zeros bits)
                );

            ]
            in

        let aux_name = sprintf "aux_%s_enable" pro.pro_name in
        let aux =
            if sym_check_obj pro.pro_objs aux_name then
            begin
                match sym_get_obj pro.pro_objs aux_name with
                | OT_signal co -> co;
                | _ -> error 825686 "";
            end
            else
            begin
                debug "emit_mon" with (sprintf "creating aux_%s_enable for process %s..."
                             pro.pro_name pro.pro_name);
                let aux = new_sig modu (Some pro) 
                      (sprintf "aux_%s_enable" pro.pro_name) (DT_logic 1) in
                aux.co_rules <- !self;
                aux
            end in
        [
            Data_top
            (sprintf "MON_PRO_%s_state <= " pro.pro_name);
        ] @ (emit_states ()) @
        [
            Data_top 
            (sprintf "MON_PRO_%s_enable <= aux_%s_enable;" 
                     pro.pro_name pro.pro_name);
            Data_top 
            (sprintf "aux_%s_enable <= PRO_%s_enable;" 
                     pro.pro_name pro.pro_name);
        ];
    | Sym_block db ->
        (*
        ** Create process reader and writer lists
        *)
        let inline = List.mem Mp_inline db.db_params in
        let dualport = List.mem Mp_dualport db.db_params in

        let prd,pwr,pgd = (ref []),(ref []),(ref []) in
        List.iter (fun obj ->
            let add_rd pl = 
                List.iter (fun pro ->
                    if not (List.mem pro !prd) then prd := pro :: !prd;
                    ) pl;
                in
            let add_wr pl = 
                List.iter (fun pro ->
                    if not (List.mem pro !pwr) then pwr := pro :: !pwr;
                    ) pl;
                in
            let rec get_obj obj =
                match obj with
                | OT_var co -> 
                    add_rd co.co_reader;
                    add_wr co.co_writer;
                | OT_array at -> get_obj at.at_objs.(0);
                | _ -> error 171599 "";
                in
            get_obj obj;
          ) db.db_objs;
        List.iter (fun pro ->
            if not (List.mem pro !pgd) then pgd := pro :: !pgd;
            ) !prd;
        List.iter (fun pro ->
            if not (List.mem pro !pgd) then pgd := pro :: !pgd;
            ) !pwr;
        
        let ram_rd =
          (List.map (fun pro ->
                Data_top (
                sprintf "MON_RAM_%s_%s_RE <= RAM_%s_%s_RE;"
                    db.db_name pro.pro_name
                    db.db_name pro.pro_name);
            ) !prd) 
          in
        let ram_wr =
          (List.map (fun pro ->
            Data_top (
            sprintf "MON_RAM_%s_%s_WE <= RAM_%s_%s_WE;"
                    db.db_name pro.pro_name 
                    db.db_name pro.pro_name);
            ) !pwr ) 
          in
        let ram_addr =
           List.map (fun pro ->
            Data_top (
            sprintf "MON_RAM_%s_%s_ADDR <= RAM_%s_%s_ADDR;"
                            db.db_name
                            pro.pro_name 
                            db.db_name
                            pro.pro_name);
            ) !pgd in
        let ram_gd =
           List.map (fun pro ->
            Data_top (
            sprintf "MON_RAM_%s_%s_GD <= RAM_%s_%s_GD;"
                    db.db_name pro.pro_name
                    db.db_name pro.pro_name);
            ) !pgd
           in
        let ram_lock =
          if not dualport then
            [Data_top (
                sprintf "MON_RAM_%s_LOCKED <= RAM_%s_LOCKED;"
                    db.db_name db.db_name);
            ]
          else
            []
          in
        let ram_aux = 
          if not inline then
            ( 
              if not dualport then
              [
                Data_top (sprintf "MON_RAM_%s_WE_AUX <= RAM_%s_WE_AUX;"
                    db.db_name 
                    db.db_name);
                Data_top (sprintf "MON_RAM_%s_DIN_AUX <= RAM_%s_DIN_AUX;"
                    db.db_name 
                    db.db_name);
              ]
              else
              [
                Data_top (sprintf "MON_RAM_%s_WE_AUX_A <= RAM_%s_WE_AUX_A;"
                    db.db_name 
                    db.db_name);
                Data_top (sprintf "MON_RAM_%s_WE_AUX_B <= RAM_%s_WE_AUX_B;"
                    db.db_name 
                    db.db_name);
                Data_top (sprintf "MON_RAM_%s_DIN_AUX_A <= RAM_%s_DIN_AUX_A;"
                    db.db_name 
                    db.db_name);
                Data_top (sprintf "MON_RAM_%s_DIN_AUX_B <= RAM_%s_DIN_AUX_B;"
                    db.db_name 
                    db.db_name);
              ]
            ) @ (                    
              if not dualport then
              [
                Data_top (sprintf "MON_RAM_%s_DOUT_AUX <= RAM_%s_DOUT_AUX;"
                    db.db_name 
                    db.db_name);
              ]
              else
              [
                Data_top (sprintf "MON_RAM_%s_DOUT_AUX_A <= RAM_%s_DOUT_AUX_A;"
                    db.db_name 
                    db.db_name);
                Data_top (sprintf "MON_RAM_%s_DOUT_AUX_B <= RAM_%s_DOUT_AUX_B;"
                    db.db_name 
                    db.db_name);
              ];
            ) @ (
                if not dualport then
                [
                Data_top (sprintf "MON_RAM_%s_ADDR_AUX <= RAM_%s_ADDR_AUX;"
                    db.db_name 
                    db.db_name);
                ]
                else
                [
                Data_top (sprintf "MON_RAM_%s_ADDR_AUX_A <= RAM_%s_ADDR_AUX_A;"
                    db.db_name 
                    db.db_name);
                Data_top (sprintf "MON_RAM_%s_ADDR_AUX_B <= RAM_%s_ADDR_AUX_B;"
                    db.db_name 
                    db.db_name);
                ];
            )
          else [] in
        
        ram_rd @ ram_wr @ ram_gd @ ram_addr @ ram_lock @ ram_aux
    | _ -> error 18687 ""
  end;
  | _ -> error 556515 ""

(*
** Toplevel instructions of module modu or process pro. 
**  Returns -> (top_expr list, top_def list)
*)
let rec emit_top_code instr modu pro =
    let top_def = ref [] in
    let top_expr = ref [] in
    let mod_pro =
        match pro with
        | Some pro -> pro;
        | None -> List.hd modu.mod_procs in
    begin
      match instr with
      | PI_block (il,bf) ->
            List.iter (fun pi -> 
                let e,d = emit_top_code pi modu pro in
                top_expr := !top_expr @ e;
                top_def := !top_def @ d) il;
      | PI_forloop (_,_,_,_,_,bl) ->
            let e,d = emit_top_code bl modu pro in
            top_expr := !top_expr @ e;
            top_def := !top_def @ d;
      | PI_monitor (src,obj,_) ->
            List.iter (fun sd ->
                         match sd with
                         | Data_top s -> top_expr := !top_expr @ [s];
                         | _ -> ();
                         ) (emit_mon instr modu pro obj);
            List.iter (fun sd ->
                         match sd with
                         | Data_top_def s -> top_def := !top_def @ [s];
                         | _ -> ();
                         ) (emit_mon instr modu pro obj);
      | PI_map (_,lhs,rhs) ->
        let dst,dst_dt =
          let rec get_it obj =
            match obj with
            | PI_obj (opl,ot) ->
            begin
              match ot with
              | OT_signal co -> 
                let is_local = is_ot_local pro ot in
                let dst_dt = 
                    let dt = co.co_type in
                    if is_sub opl then
                    begin
                        let (a,b) = obj_sub opl in
                        dt_of_td (td_of_dt dt) (b-a+1);
                    end 
                    else if is_index opl then
                    begin
                        let a = obj_index opl in
                        dt_of_td (td_of_dt dt)  1;
                    end 
                    else dt in
                let expr_dt = dst_dt in
                let flags = 
                    if is_ot_local pro ot then [UO_loc;UO_lhs] 
                                          else [UO_lhs] in
                let ud = ud_of_ot ot opl flags expr_dt in
                let vd = vhdl_of_ud mod_pro None ud None in
                top_expr := !top_expr @ vd.top_expr;
                top_def := !top_def @ vd.top_def;
                vd.dp_sig,
                dst_dt;
              | OT_array at ->
                let is_dyn = List.mem AT_dyn at.at_flags in
                let is_block = List.mem AT_block at.at_flags in
                if not is_dyn && not is_block then
                begin
                    if is_sel opl then
                    begin
                        get_it (PI_obj (opl,at.at_objs.(at_index at (obj_sel opl))));    
                    end
                    else
                        error 189391 "";
                end
                else
                    error 669918 "";
              | _ -> error 772692 "";
            end;
            | _ -> error 459635 "";
            in
          get_it lhs;          
          in
        let src =
          let rec get_it obj =
            match obj with
            | PI_obj (opl,ot) ->
            begin
              match ot with
              | OT_array at ->
                let is_dyn = List.mem AT_dyn at.at_flags in
                let is_block = List.mem AT_block at.at_flags in
                if not is_dyn && not is_block then
                begin
                    if is_sel opl then
                    begin
                        get_it (PI_obj (opl,at.at_objs.(
                                            at_index at (obj_sel opl))));    
                    end
                    else
                        error 189392 "";
                end
                else
                    error 0 (sprintf "Array <%s> with dynamic selector can't be mapped!"
                                     at.at_name);
              | _ -> 
                  if is_conv opl then
                  begin
                    match ot with
                    | OT_signal co 
                    | OT_reg co -> 
                        let convl,convr = vhdl_convert co.co_type dst_dt in
                        let flags = 
                            if is_ot_local pro ot then [UO_loc;UO_rhs] 
                                                  else [UO_rhs] in
                        let expr_dt = dst_dt in
                        let ud = ud_of_ot ot opl flags expr_dt in
                        let vd = vhdl_of_ud mod_pro None ud None in
                        top_expr := !top_expr @ vd.top_expr;
                        top_def := !top_def @ vd.top_def;
                        sprintf "%s%s%s" convl vd.dp_sig convr;
                    | OT_value v ->
                        val_str dst_dt v;
                    | OT_named_value (_,v) ->
                        val_str dst_dt v;
                    | OT_const co ->
                        val_str dst_dt co.co_init;
                    | _ -> error 772693 "";
                  end
                  else
                  begin
                    match ot with
                    | OT_reg co
                    | OT_signal co -> 
                        let src_dt = 
                            let dt = co.co_type in
                            if is_sub opl then
                            begin
                                let (a,b) = obj_sub opl in
                                dt_of_td (td_of_dt dt) (b-a+1);
                            end 
                            else if is_index opl then
                            begin
                                let a = obj_index opl in
                                dt_of_td (td_of_dt dt) 1;
                            end else dt in
                        let convl,convr = vhdl_convert src_dt dst_dt in
                        let flags = 
                            if is_ot_local pro ot then [UO_loc;UO_rhs] 
                                                  else [UO_rhs] in
                        let expr_dt = dst_dt in
                        let ud = ud_of_ot ot opl flags expr_dt in
                        let vd = vhdl_of_ud mod_pro None ud None in
                        top_expr := !top_expr @ vd.top_expr;
                        top_def := !top_def @ vd.top_def;
                        sprintf "%s%s%s" convl vd.dp_sig convr;
                    | OT_value v ->
                        val_str dst_dt v;
                    | OT_named_value (_,v) ->
                        val_str dst_dt v;
                    | OT_const co ->
                        val_str dst_dt co.co_init;
                    | _ -> error 772693 "";
                  end;
            end;
            | _ -> error 459636 "";
            in
          get_it rhs;          
          in
        top_expr := !top_expr @ [sprintf "%s <= %s;" dst src;];
      | PI_fun (src,(opl,ot),sel,args) ->
      begin
        line src;
        let rec emit ot =
          match ot with
          | OT_object ao ->
              let at = ao.ao_type in
              (*
              ** redirect to target module
              *)
              let rules = at.ta_rules in
              if rules.rl_name <> "Core" then
              begin
                let e,d = at.ta_rules.rl_top_vcode instr modu pro in 
                top_expr := !top_expr @ e;
                top_def := !top_def @ d;
              end
              else
              begin
                ();
              end;
          | OT_array at -> emit at.at_objs.(0);
          | OT_queue qu -> emit (OT_object qu.qu_ao);
          | OT_channel ch -> emit (OT_object ch.ch_ao);
          | _ -> error 907461 sel;
          in
        emit ot;
      end;
      | PI_nop -> ();
      | _ -> ()
    end;
    !top_expr, !top_def
    
let top_code modu pro = 
    let top_expr = ref [] in
    let top_def = ref [] in
    let emit instr = 
      let e,d = emit_top_code instr modu pro in
      top_expr := !top_expr @ e;
      top_def := !top_def @ d; in
    match pro with
    | Some pro ->
        List.iter (fun instr -> emit instr) pro.pro_instr;
        !top_expr, !top_def
    | None ->
        List.iter (fun instr -> emit instr) modu.mod_instr;
        !top_expr, !top_def
