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
**    $INITIAL:     (C) 2006-2011 BSSLAB
**    $CREATED:     19.3.2006
**    $VERSION:     2.15
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
open Cp_vhdl
open Cp_stat

(*
** Object implementation in architecture body.
*)
let rec obj_code sym modu pro =
  let vhdl_ind = ref 0 in
  let strl = ref [] in
  let vhdl str =
    let spaces n = String.make n ' ' in
    strl := !strl @ [(spaces !vhdl_ind)^str] 
    in

  match sym with
  | Sym_obj obj ->
  begin
    if (is_mon sym) then    
        []
    else
    match obj with
    | OT_reg co when not (List.mem Obj_local co.co_flags) ->
        let is_array = co.co_array <> [] in
        let is_dyn = List.mem Obj_array co.co_flags in
        let at_name =
            match co.co_array with
            | at :: _ -> at.at_name;
            | [] -> "" in
         let rec fifo l =
            match l with
            | (Obj_schedule m)::tl -> 
              m = "fifo"; 
            | _ :: tl -> fifo tl;
            | [] -> false in
        let fifo = fifo co.co_flags in
        let prd,pwr,pblocked,pall = get_procs co in
        let n = List.length pall in
        let w = const_width (V_int (Int64.of_int n)) in
        let dt_prio = DT_logic w in   (* used for fifo=true *)

        (*
        ** Emit register implementation. Either guarded (shared by
        ** multiple processes) or not (process local register).
        *)
    
        debug "obj_code" with (sprintf "obj_code: OT_reg %s" co.co_name);        
        if co.co_guard = None then
            vhdl "-- Register"
        else
            vhdl "-- Guarded Register";

        out (sprintf "Creating global register <%s> [%s, scheduler=%s #rd=%d #wr=%d]..."
                     co.co_name
                     (sprint_dt co.co_type)
                     (if fifo then "FIFOdyn" else "PRIOstat")
                     (List.length co.co_reader)
                     (List.length co.co_writer));
                     
        vhdl (sprintf "IMPL_REG_%s: process(" co.co_name);
        vhdl_ind := 8;

        (*
        ** Build sensitivy list: XX_wr, XX_we
        *)
        List.iter (fun pro ->
            if not is_dyn then
            begin
                vhdl (sprintf "REG_%s_%s_WR," 
                              co.co_name
                              pro.pro_name);
                vhdl (sprintf "REG_%s_%s_WE,"
                              co.co_name  
                              pro.pro_name);
            end
            else
            begin
                vhdl (sprintf "ARRAY_%s_%s_WR," 
                              at_name
                              pro.pro_name);
                vhdl (sprintf "ARRAY_%s_%s_WEA,"
                              at_name  
                              pro.pro_name);
            end;
            
            ) co.co_writer;
        if co.co_guard <> None && not is_dyn then
            vhdl (sprintf "REG_%s_LOCKED,"
                           co.co_name)
        else if co.co_guard <> None then
            vhdl (sprintf "ARRAY_%s_LOCKED,"
                           at_name);


        if not is_dyn then
            vhdl (sprintf "REG_%s," co.co_name);

        if fifo then
        begin
            List.iter (fun pro ->  
                vhdl (sprintf "REG_%s_%s_QUEUED," co.co_name pro.pro_name);
                vhdl (sprintf "REG_%s_%s_PRIO," co.co_name pro.pro_name);
                ) pall;
            vhdl (sprintf "REG_%s_OWNER," co.co_name);
            vhdl (sprintf "REG_%s_HEAD," co.co_name);
            vhdl (sprintf "REG_%s_TAIL," co.co_name);
        end;

        (*
        ** Always present
        *)
        if pro <> None then
          vhdl (sprintf "PRO_%s_ENABLE,"
                (get_some pro).pro_name);
        vhdl (sprintf "conpro_system_clk,");
        vhdl (sprintf "conpro_system_reset");
        vhdl ")";
        vhdl_ind := 0; 
        vhdl "begin";
        vhdl_ind := 2;
        (*
        ** Register Process Body
        *)
        if co.co_reader <> [] && not is_dyn then
            vhdl (sprintf "REG_%s_RD <= REG_%s;" co.co_name
                                         co.co_name);
        if co.co_guard <> None then
        begin
            let gd_num = List.length co.co_writer in
            (*
            ** Guarded register.
            *)
            vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk='%d' then"
                          (get_env_int "clock_level"));
            incr vhdl_ind;
            if pro = None then
                  vhdl (sprintf "if conpro_system_reset='%d' then" 
                                (get_env_int "reset_level"))
            else
                vhdl (sprintf "if conpro_system_reset='%d' or PRO_%s_ENABLE = '0' then"
                      (get_env_int "reset_level") (get_some pro).pro_name);
            incr vhdl_ind;
            if not is_dyn then
                vhdl (sprintf "REG_%s <= %s;" co.co_name
                                          (val_str  co.co_type
                                                    co.co_init))
            else
                vhdl (sprintf "ARRAY_%s(%d) <= %s;" 
                                          at_name
                                          co.co_index
                                          (val_str  co.co_type
                                                    co.co_init));
            if not is_dyn then
                vhdl (sprintf "REG_%s_LOCKED <= '0';" co.co_name)
            else
                vhdl (sprintf "ARRAY_%s_LOCKED(%d) <= '0';" 
                              at_name co.co_index);
            
                
            if not is_dyn then
                List.iter (fun pro ->
                    vhdl (sprintf "REG_%s_%s_GD <= '1';" 
                                  co.co_name
                                  pro.pro_name);
                    ) co.co_writer
            else
                List.iter (fun pro ->
                    vhdl (sprintf "ARRAY_%s_%s_GDA(%d) <= '1';" 
                                  at_name
                                  pro.pro_name
                                  co.co_index);
                    ) co.co_writer;

            if fifo then
            begin
                List.iter (fun pro ->
                    vhdl (sprintf 
                          "REG_%s_%s_QUEUED <= '0';" co.co_name pro.pro_name);
                    vhdl (sprintf 
                          "REG_%s_%s_PRIO <= %s;" co.co_name pro.pro_name
                           (val_str dt_prio (V_int Int64.zero)));
                    ) pall;
                vhdl (sprintf 
                      "REG_%s_HEAD <= %s;" co.co_name 
                       (val_str dt_prio (V_int Int64.zero)));
                vhdl (sprintf 
                      "REG_%s_TAIL <= %s;" co.co_name 
                       (val_str dt_prio (V_int Int64.zero)));
                vhdl (sprintf 
                      "REG_%s_OWNER <= %s;" co.co_name 
                       (val_str dt_prio (V_int Int64.zero)));
            end;

            decr vhdl_ind;
            vhdl "else";
            incr vhdl_ind;
            List.iter (fun pro -> 
                    if not is_dyn then
                        vhdl (sprintf "REG_%s_%s_GD <= '1';" 
                                  co.co_name
                                  pro.pro_name)
                    else
                        vhdl (sprintf "ARRAY_%s_%s_GDA(%d) <= '1';" 
                                  at_name
                                  pro.pro_name
                                  co.co_index);
                    ) co.co_writer;

            if not fifo then
            begin
              let n = ref 1 in
              List.iter (fun pro -> 
                    if not is_dyn then
                        vhdl (sprintf "%s REG_%s_LOCKED = '0' and REG_%s_%s_WE='1' then" 
                                  (if !n = 1 then "if" else "elsif")
                                  co.co_name
                                  co.co_name
                                  pro.pro_name)
                    else
                        vhdl (sprintf "%s ARRAY_%s_LOCKED(%d) = '0' and ARRAY_%s_%s_WEA(%d)='1' then" 
                                  (if !n = 1 then "if" else "elsif")
                                  at_name co.co_index
                                  at_name pro.pro_name
                                  co.co_index
                        );

                    incr vhdl_ind;
                    if not is_dyn then
                    begin
                        vhdl (sprintf "REG_%s <= REG_%s_%s_WR;" 
                                  co.co_name 
                                  co.co_name
                                  pro.pro_name);
                        vhdl (sprintf "REG_%s_%s_GD <= '0';" 
                                  co.co_name
                                  pro.pro_name);
                        vhdl (sprintf "REG_%s_LOCKED <= '1';" co.co_name);
                    end
                    else
                    begin
                        vhdl (sprintf "ARRAY_%s(%d) <= ARRAY_%s_%s_WR;" 
                                  at_name 
                                  co.co_index
                                  at_name
                                  pro.pro_name);
                        vhdl (sprintf "ARRAY_%s_%s_GDA(%d) <= '0';" 
                                  at_name
                                  pro.pro_name
                                  co.co_index);
                        vhdl (sprintf "ARRAY_%s_LOCKED(%d) <= '1';" 
                                  at_name co.co_index);
                    end;
                    decr vhdl_ind;
                    incr n;
                    ) co.co_writer;
              if not is_dyn then
                vhdl (sprintf "elsif REG_%s_LOCKED = '1' then"
                              co.co_name)
              else
                vhdl (sprintf "elsif ARRAY_%s_LOCKED(%d) = '1' then"
                              at_name co.co_index);
              incr vhdl_ind;
              if not is_dyn then
                vhdl (sprintf "REG_%s_LOCKED <= '0';" co.co_name)
              else
                vhdl (sprintf "ARRAY_%s_LOCKED(%d) <= '0';" 
                              at_name co.co_index);

              List.iter (fun pro -> 
                    if not is_dyn then
                        vhdl (sprintf "REG_%s_%s_GD <= '1';" 
                                  co.co_name
                                  pro.pro_name)
                    else
                        vhdl (sprintf "ARRAY_%s_%s_GDA(%d) <= '1';" 
                                  at_name
                                  pro.pro_name
                                  co.co_index);
                    ) co.co_writer;

              decr vhdl_ind;
              vhdl "end if;";
            end   
            else
            begin
              (*
              ** Dynamic FIFO process scheduler
              *)
              let n = ref 1 in
              List.iter (fun pro -> 
                    if not is_dyn then
                        vhdl (sprintf "%s REG_%s_%s_WE = '1' and REG_%s_LOCKED = '0' then" 
                                  (if !n = 1 then "if" else "elsif")
                                  co.co_name pro.pro_name
                                  co.co_name)
                    else
                        vhdl (sprintf "elsif ARRAY_%s_%s_WEA(%d) = '1' and ARRAY_%s_LOCKED(%d) = '0' then" 
                                  at_name pro.pro_name co.co_index
                                  at_name co.co_index);

                    incr vhdl_ind;
                    if not is_dyn then
                    begin
                        vhdl (sprintf "REG_%s <= REG_%s_%s_WR;" 
                                  co.co_name 
                                  co.co_name
                                  pro.pro_name);
                        vhdl (sprintf "REG_%s_%s_GD <= '0';" 
                                  co.co_name
                                  pro.pro_name);
                        vhdl (sprintf "REG_%s_LOCKED <= '1';" co.co_name);
                    end
                    else
                    begin
                        vhdl (sprintf "ARRAY_%s(%d) <= ARRAY_%s_%s_WR;" 
                                  at_name 
                                  co.co_index
                                  at_name
                                  pro.pro_name);
                        vhdl (sprintf "ARRAY_%s_%s_GDA(%d) <= '0';" 
                                  at_name
                                  pro.pro_name
                                  co.co_index);
                        vhdl (sprintf "ARRAY_%s_LOCKED(%d) <= '1';" 
                                  at_name co.co_index);
                    end;
                    vhdl (sprintf "REG_%s_OWNER <= %s;"
                                   co.co_name
                                  (val_str dt_prio (V_int (Int64.of_int !n))));
                                                      
                    decr vhdl_ind;
                    vhdl (sprintf "elsif REG_%s_%s_WE = '1' and REG_%s_%s_QUEUED = '0' and REG_%s_OWNER /= %s then"
                          co.co_name pro.pro_name
                          co.co_name pro.pro_name
                          co.co_name
                          (val_str dt_prio (V_int (Int64.of_int !n))));
                    incr vhdl_ind;
                    vhdl (sprintf "REG_%s_%s_PRIO <= REG_%s_HEAD;" 
                          co.co_name pro.pro_name 
                          co.co_name);
                    vhdl (sprintf "REG_%s_HEAD <= REG_%s_HEAD + 1;" 
                          co.co_name 
                          co.co_name);
                    vhdl (sprintf "REG_%s_%s_QUEUED <= '1';" 
                                  co.co_name pro.pro_name);
                    decr vhdl_ind;
                    vhdl (sprintf "elsif REG_%s_%s_WE = '1' and REG_%s_%s_QUEUED = '1' and REG_%s_%s_PRIO = REG_%s_TAIL then"
                          co.co_name pro.pro_name
                          co.co_name pro.pro_name
                          co.co_name pro.pro_name
                          co.co_name);
                    incr vhdl_ind;
                    vhdl (sprintf "REG_%s_%s_GD <= '0';" 
                                  co.co_name pro.pro_name);
                    vhdl (sprintf "REG_%s_%s_QUEUED <= '0';" 
                                  co.co_name pro.pro_name);
                    vhdl (sprintf "REG_%s_OWNER <= %s;" 
                                  co.co_name
                                  (val_str dt_prio (V_int (Int64.of_int !n))));

                    if not is_dyn then
                    begin
                        vhdl (sprintf "REG_%s <= REG_%s_%s_WR;" 
                                  co.co_name 
                                  co.co_name
                                  pro.pro_name);
                    end
                    else
                    begin
                        vhdl (sprintf "ARRAY_%s(%d) <= ARRAY_%s_%s_WR;" 
                                  at_name 
                                  co.co_index
                                  at_name
                                  pro.pro_name);
                    end;
                    decr vhdl_ind;                    
                    incr n;
                    ) co.co_writer;
              if not is_dyn then
                vhdl (sprintf "elsif REG_%s_LOCKED = '1' then"
                              co.co_name)
              else
                vhdl (sprintf "elsif ARRAY_%s_LOCKED(%d) = '1' then"
                              at_name co.co_index);
              incr vhdl_ind;

              vhdl (sprintf "if REG_%s_HEAD = REG_%s_TAIL then"
                            co.co_name co.co_name);
              incr vhdl_ind;
              if not is_dyn then
                vhdl (sprintf "REG_%s_LOCKED <= '0';" co.co_name)
              else
                vhdl (sprintf "ARRAY_%s_LOCKED(%d) <= '0';" 
                              at_name co.co_index);
              vhdl (sprintf "REG_%s_OWNER <= %s;" co.co_name
                            (val_str dt_prio (V_int (Int64.of_int 0))));
            
              decr vhdl_ind;
              vhdl "else";
              incr vhdl_ind;
              vhdl (sprintf "REG_%s_TAIL <= REG_%s_TAIL + 1;" 
                            co.co_name 
                            co.co_name);
              decr vhdl_ind;
              vhdl "end if;";

              decr vhdl_ind;
              vhdl "end if;";
            end;
            decr vhdl_ind;
            vhdl "end if;";
            decr vhdl_ind ;
            vhdl "end if;";
        end
        else
        begin
                
            (*
            ** Simple case: no guard. Only one write source or explicit
            ** guarding by user.
            *)
                    
            vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk='%d' then"
                          (get_env_int "clock_level"));
            incr vhdl_ind;
            if pro = None then
                  vhdl (sprintf "if conpro_system_reset='%d' then"
                                (get_env_int "reset_level"))
            else
                vhdl (sprintf "if conpro_system_reset='%d' or PRO_%s_ENABLE = '0' then"
                              (get_env_int "reset_level") (get_some pro).pro_name);
            incr vhdl_ind;
            
            if not is_dyn then
                vhdl (sprintf "REG_%s <= %s;" co.co_name
                                          (val_str  co.co_type
                                                    co.co_init))
            else
                vhdl (sprintf "ARRAY_%s(%d) <= %s;" 
                                          at_name
                                          co.co_index
                                          (val_str  co.co_type
                                                    co.co_init));
            List.iter (fun pro ->
                decr vhdl_ind;
                let pro_name = pro.pro_name in
                if not is_dyn then
                        vhdl (sprintf "elsif REG_%s_%s_WE='1' then" 
                              co.co_name
                              pro_name)
                else
                    vhdl (sprintf "elsif ARRAY_%s_%s_WEA(%d)='1' then" 
                              at_name pro_name
                              co.co_index
                     );

                incr vhdl_ind;
            
                if not is_dyn then
                        vhdl (sprintf "REG_%s <= REG_%s_%s_WR;" 
                              co.co_name 
                              co.co_name
                              pro_name)
                else
                        vhdl (sprintf "ARRAY_%s(%d) <= ARRAY_%s_%s_WR;" 
                              at_name 
                              co.co_index
                              at_name
                              pro_name);
                ) co.co_writer;
            decr vhdl_ind;
            vhdl "end if;";
                
            decr vhdl_ind;
            vhdl "end if;";
        end;
        vhdl_ind := 0;
        vhdl (sprintf "end process IMPL_REG_%s;" co.co_name);
        vhdl "";
        !strl

    | OT_channel ch ->
    begin
      let co = ch.ch_obj in
      let ao = ch.ch_ao in
      let ao_name = ao.ao_name in
        
      if pro <> None then
          error 0 (sprintf "Found unexpected channel <%s> implementation inside process <%s>."
                           co.co_name
                           (get_some pro).pro_name);
          
      let model_conn =
          if List.mem Chan_uni ch.ch_model then Chan_uni else
          if List.mem Chan_bi ch.ch_model then Chan_bi else
          if List.mem Chan_multi ch.ch_model then Chan_multi else
          error 0 (sprintf "No connection model found for channel <%s>!"
                           co.co_name) in
      let model_buf =
          if List.mem Chan_buffered ch.ch_model then Chan_buffered else
          if List.mem Chan_unbuffered ch.ch_model then Chan_unbuffered else
          error 0 (sprintf "No buffer model found for channel <%s>!"
                           co.co_name) in
      let pro_gds = ref co.co_writer in
        List.iter (fun pro ->
          if not (List.mem pro !pro_gds) then pro_gds := !pro_gds @ [pro];
          ) co.co_reader;


      let unbuffered1 name rd wr =
            vhdl "-- Unidirectional unbuffered Channel with sync. write";
            vhdl (sprintf "IMPL_CHAN_%s%s: process(" co.co_name name);
            vhdl_ind := 8;
            (*
            ** Build sensitivy list: XX_wr, XX_we
            *)
            (match ch.ch_ot with
             | None ->
                vhdl (sprintf "CHAN_%s_%s_WR," 
                              co.co_name
                              wr.pro_name);
             | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                        vhdl (sprintf "CHAN_%s_%s_WR," 
                                      co.co_name
                                      wr.pro_name);
                    | None -> error 190234 "";
                 ) st.st_objs;
             | _ -> error 337554 "");
            vhdl (sprintf "CHAN_%s_%s_WE,"
                              co.co_name  
                              wr.pro_name);              
            vhdl (sprintf "CHAN_%s_%s_RE,"
                              co.co_name  
                              rd.pro_name);              
            (*
            ** Always present
            *)
            vhdl (sprintf "conpro_system_clk,");
            vhdl (sprintf "conpro_system_reset");
            vhdl ")";
            vhdl_ind := 0; 
            vhdl "begin";
            vhdl_ind := 2;

            (match ch.ch_ot with
             | None ->
                vhdl (sprintf "CHAN_%s_%s_RD <= CHAN_%s_%s_WR;"
                              co.co_name rd.pro_name
                              co.co_name wr.pro_name);
             | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                        vhdl (sprintf "CHAN_%s_%s_RD <= CHAN_%s_%s_WR;"
                                      co.co_name rd.pro_name
                                      co.co_name wr.pro_name);
                    | None -> error 712893 "";
                 ) st.st_objs;
             | _ -> error 854511 "");

            vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk='%d' then"
                          (get_env_int "clock_level"));
            vhdl_ind := 4;
            vhdl (sprintf "if conpro_system_reset='%d' then" 
                          (get_env_int "reset_level"));
            vhdl_ind := 6;
            
            vhdl (sprintf "CHAN_%s_%s_GD%s <= '1';"
                          co.co_name
                          rd.pro_name name);
            vhdl (sprintf "CHAN_%s_%s_GD%s <= '1';"
                          co.co_name
                          wr.pro_name name);

            vhdl_ind := 4;
            vhdl (sprintf "elsif CHAN_%s_%s_WE = '1' and CHAN_%s_%s_RE = '1' then"
                          co.co_name wr.pro_name
                          co.co_name rd.pro_name);
            vhdl_ind := 6;
            List.iter (fun pro ->
                vhdl (sprintf "CHAN_%s_%s_GD%s <= '0';"
                              co.co_name
                              pro.pro_name name);
              ) !pro_gds;
            vhdl_ind := 4;
            vhdl "else";
            vhdl_ind := 6;
            
            vhdl (sprintf "CHAN_%s_%s_GD%s <= '1';"
                          co.co_name
                          rd.pro_name name);
            vhdl (sprintf "CHAN_%s_%s_GD%s <= '1';"
                          co.co_name
                          wr.pro_name name);

            vhdl_ind := 4;
            vhdl "end if;";

            vhdl_ind := 2;
            vhdl "end if;";
            vhdl_ind := 0;
            vhdl (sprintf "end process IMPL_CHAN_%s%s;" co.co_name name);
            vhdl "";
            in

      let buffered1 name rd wr =
            vhdl "-- Unidirectional buffered Channel with async. write";
            vhdl (sprintf "IMPL_CHAN_%s%s: process(" co.co_name name);
            vhdl_ind := 8;
            (*
            ** Build sensitivy list: XX_wr, XX_we
            *)
            (match ch.ch_ot with
             | None ->
                vhdl (sprintf "CHAN_%s_%s_WR," 
                              co.co_name
                              wr.pro_name);
             | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                        vhdl (sprintf "CHAN_%s_%s_WR," 
                                      co.co_name
                                      wr.pro_name);
                    | None -> error 225655 "";
                 ) st.st_objs;
             | _ -> error 20314 "");
            vhdl (sprintf "CHAN_%s_%s_WE,"
                              co.co_name  
                              wr.pro_name);              
            vhdl (sprintf "CHAN_%s_%s_RE,"
                              co.co_name  
                              rd.pro_name);              
            (*
            ** Always present
            *)
            vhdl (sprintf "CHAN_%s_AVAIL%s,"
                          co.co_name name);
            vhdl (sprintf "CHAN_%s%s,"
                          co.co_name name);
            vhdl (sprintf "conpro_system_clk,");
            vhdl (sprintf "conpro_system_reset");
            vhdl ")";
            vhdl_ind := 0; 
            vhdl "begin";
            vhdl_ind := 2;
            
            (match ch.ch_ot with
             | None ->
                vhdl (sprintf "CHAN_%s_%s_RD <= CHAN_%s%s;"
                              co.co_name rd.pro_name
                              co.co_name name);
             | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                        vhdl (sprintf "CHAN_%s_%s_RD <= CHAN_%s%s;"
                                      co.co_name rd.pro_name
                                      co.co_name name);
                    | None -> error 79342 "";
                 ) st.st_objs;
             | _ -> error 413895 "");

            vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk='%d' then"
                          (get_env_int "clock_level"));
            vhdl_ind := 4;

            vhdl (sprintf "if conpro_system_reset='%d' then" 
                          (get_env_int "reset_level"));
            vhdl_ind := 6;
            List.iter (fun pro ->
                  vhdl (sprintf 
                        "CHAN_%s_%s_GD%s <= '1';"
                            co.co_name pro.pro_name name)
                  ) !pro_gds;
            (match ch.ch_ot with
             | None ->
                vhdl (sprintf "CHAN_%s%s <= %s;" 
                              co.co_name name
                              (val_str  co.co_type
                                        co.co_init));
             | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                        vhdl (sprintf "CHAN_%s%s <= %s;" 
                                      co.co_name name
                                      (val_str  co.co_type
                                                co.co_init));
                    | None -> error 507604 "";
                 ) st.st_objs;
             | _ -> error 352480 "");

            vhdl (sprintf "CHAN_%s_AVAIL%s <= '0';"
                          co.co_name name);
            vhdl_ind := 4;
            vhdl (sprintf "elsif CHAN_%s_%s_WE = '1' and CHAN_%s_AVAIL%s = '0' then"
                          co.co_name wr.pro_name
                          co.co_name name);
            vhdl_ind := 6;
            vhdl (sprintf "CHAN_%s_%s_GD%s <= '0';"
                              co.co_name
                              wr.pro_name name);
            vhdl (sprintf "CHAN_%s_AVAIL%s <= '1';"
                              co.co_name name);
            (match ch.ch_ot with
             | None ->
                vhdl (sprintf "CHAN_%s%s <= CHAN_%s_%s_WR;" 
                              co.co_name name
                              co.co_name wr.pro_name);
             | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                        vhdl (sprintf "CHAN_%s%s <= CHAN_%s_%s_WR;" 
                                      co.co_name name
                                      co.co_name wr.pro_name);
                    | None -> error 779514 "";
                 ) st.st_objs;
             | _ -> error 407917 "");
            vhdl_ind := 4;
            vhdl (sprintf "elsif CHAN_%s_%s_RE = '1' and CHAN_%s_AVAIL%s = '1' then"
                          co.co_name rd.pro_name
                          co.co_name name);
            vhdl_ind := 6;
            vhdl (sprintf "CHAN_%s_%s_GD%s <= '0';"
                              co.co_name
                              rd.pro_name name);
            vhdl (sprintf "CHAN_%s_AVAIL%s <= '0';"
                              co.co_name name);
            vhdl_ind := 4;
            vhdl "else";
            vhdl_ind := 6;
            
            vhdl (sprintf "CHAN_%s_%s_GD%s <= '1';"
                          co.co_name
                          rd.pro_name name);
            vhdl (sprintf "CHAN_%s_%s_GD%s <= '1';"
                          co.co_name
                          wr.pro_name name);

            vhdl_ind := 4;
            vhdl "end if;";
            vhdl_ind := 2;
            vhdl "end if;";
            vhdl_ind := 0;
            vhdl (sprintf "end process IMPL_CHAN_%s%s;" co.co_name name);
            vhdl "";
            in


      let bufferedn name rds wrs gds =
            vhdl "-- Multidirectional buffered Channel with async. write";
            vhdl (sprintf "IMPL_CHAN_%s%s: process(" co.co_name name);
            vhdl_ind := 8;
            (*
            ** Build sensitivy list: XX_wr, XX_we
            *)
            List.iter (fun wr ->
              vhdl (sprintf "CHAN_%s_%s_WR," 
                              co.co_name
                              wr.pro_name);
              (match ch.ch_ot with
               | None ->
                  vhdl (sprintf "CHAN_%s_%s_WR," 
                                  co.co_name
                                  wr.pro_name);
               | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                      vhdl (sprintf "CHAN_%s_%s_WR," 
                                      co.co_name
                                      wr.pro_name);
                    | None -> error 888925 "";
                 ) st.st_objs;
               | _ -> error 5739 "");
              vhdl (sprintf "CHAN_%s_%s_WE,"
                              co.co_name  
                              wr.pro_name);              
              ) wrs;
            List.iter (fun rd ->
              vhdl (sprintf "CHAN_%s_%s_RE,"
                              co.co_name  
                              rd.pro_name);              
              ) rds;
            (*
            ** Always present
            *)
            vhdl (sprintf "CHAN_%s_AVAIL%s,"
                          co.co_name name);


            (match ch.ch_ot with
               | None ->
                    vhdl (sprintf "CHAN_%s%s,"
                                  co.co_name name);
               | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                        vhdl (sprintf "CHAN_%s%s,"
                                      co.co_name name);
                    | None -> error 847143 "";
                 ) st.st_objs;
               | _ -> error 788478 "");


            vhdl (sprintf "conpro_system_clk,");
            vhdl (sprintf "conpro_system_reset");
            vhdl ")";
            vhdl_ind := 0; 
            vhdl "begin";
            vhdl_ind := 2;
            
            List.iter (fun rd ->
              (match ch.ch_ot with
               | None ->
                  vhdl (sprintf "CHAN_%s_%s_RD <= CHAN_%s%s;"
                              co.co_name rd.pro_name
                              co.co_name name);
               | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                      vhdl (sprintf "CHAN_%s_%s_RD <= CHAN_%s%s;"
                                  co.co_name rd.pro_name
                                  co.co_name name);
                    | None -> error 683095 "";
                 ) st.st_objs;
               | _ -> error 902995 "");
              ) rds;
            vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk='%d' then"
                          (get_env_int "clock_level"));
            vhdl_ind := 4;

            vhdl (sprintf "if conpro_system_reset='%d' then" 
                          (get_env_int "reset_level"));
            vhdl_ind := 6;
            List.iter (fun gd ->
              vhdl (sprintf "CHAN_%s_%s_GD%s <= '1';"
                              co.co_name
                              gd.pro_name name);
              ) gds;
            (match ch.ch_ot with
               | None ->
                    vhdl (sprintf "CHAN_%s%s <= %s;" 
                                  co.co_name name
                                  (val_str  co.co_type
                                            co.co_init));
               | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                        vhdl (sprintf "CHAN_%s%s <= %s;" 
                                      co.co_name name
                                      (val_str  co.co_type
                                                co.co_init));
                    | None -> error 707334 "";
                 ) st.st_objs;
               | _ -> error 917169 "");
            vhdl (sprintf "CHAN_%s_AVAIL%s <= '0';"
                          co.co_name name);
            vhdl_ind := 4;
            List.iter (fun wr ->
              vhdl (sprintf "elsif CHAN_%s_%s_WE = '1' and CHAN_%s_AVAIL%s = '0' then"
                          co.co_name wr.pro_name
                          co.co_name name);
              vhdl_ind := 6;
              vhdl (sprintf "CHAN_%s_%s_GD%s <= '0';"
                              co.co_name
                              wr.pro_name name);
              vhdl (sprintf "CHAN_%s_AVAIL%s <= '1';"
                              co.co_name name);
              (match ch.ch_ot with
               | None ->
                  vhdl (sprintf "CHAN_%s%s <= CHAN_%s_%s_WR;" 
                                  co.co_name name
                                  co.co_name wr.pro_name);
               | Some (OT_struct st) ->
                List.iter (fun ot ->
                    match co_of_ot ot with
                    | Some co ->  
                      vhdl (sprintf "CHAN_%s%s <= CHAN_%s_%s_WR;" 
                                      co.co_name name
                                      co.co_name wr.pro_name);
                    | None -> error 103864 "";
                 ) st.st_objs;
               | _ -> error 470260 "");
              vhdl_ind := 4;
              ) wrs;
           
            List.iter (fun rd -> 
              vhdl (sprintf "elsif CHAN_%s_%s_RE = '1' and CHAN_%s_AVAIL%s = '1' then"
                          co.co_name rd.pro_name
                          co.co_name name);
              vhdl_ind := 6;
              vhdl (sprintf "CHAN_%s_%s_GD%s <= '0';"
                              co.co_name
                              rd.pro_name name);
              vhdl (sprintf "CHAN_%s_AVAIL%s <= '0';"
                              co.co_name name);
              vhdl_ind := 4;
              ) rds;
            vhdl "end if;";
            vhdl_ind := 2;
            vhdl "end if;";
            vhdl_ind := 0;
            vhdl (sprintf "end process IMPL_CHAN_%s%s;" co.co_name name);
            vhdl "";
            in
    
      match model_conn with
      | Chan_uni -> 
      begin
          match model_buf with
          | Chan_unbuffered ->
            out (sprintf "Creating unidirectional unbuffered channel <%s> [%s]..."
                             co.co_name
                             (sprint_dt co.co_type));
            let rd = List.hd co.co_reader in
            let wr = List.hd co.co_writer in
            unbuffered1 "" rd wr;
            !strl;
          | Chan_buffered ->
            out (sprintf "Creating unidirectional buffered channel <%s> [%s]..."
                             co.co_name
                             (sprint_dt co.co_type));
            let rd = List.hd co.co_reader in
            let wr = List.hd co.co_writer in
            buffered1 "" rd wr;
            !strl;
          | _ -> error 241937 "";
      end;
      | Chan_bi ->
      begin
          match model_buf with
          | Chan_unbuffered ->
            out (sprintf "Creating bidirectional unbuffered channel <%s> [%s]..."
                             co.co_name
                             (sprint_dt co.co_type));
            if co.co_reader = co.co_writer then
                co.co_reader <- List.rev co.co_reader;
            let rd = List.nth co.co_reader 0 in
            let wr = List.nth co.co_writer 0 in
            unbuffered1 "_A" rd wr;
            let rd = List.nth co.co_reader 1 in
            let wr = List.nth co.co_writer 1 in
            unbuffered1 "_B" rd wr;
            List.iter (fun pro ->
                vhdl (sprintf "CHAN_%s_%s_GD <= CHAN_%s_%s_GD_A and CHAN_%s_%s_GD_B;"
                              co.co_name pro.pro_name
                              co.co_name pro.pro_name
                              co.co_name pro.pro_name);
                ) !pro_gds;
            !strl;
          | Chan_buffered ->
            out (sprintf "Creating bidirectional buffered channel <%s> [%s]..."
                             co.co_name
                             (sprint_dt co.co_type));
            if co.co_reader = co.co_writer then
                co.co_reader <- List.rev co.co_reader;
            let rd = List.nth co.co_reader 0 in
            let wr = List.nth co.co_writer 0 in
            buffered1 "_A" rd wr;
            let rd = List.nth co.co_reader 1 in
            let wr = List.nth co.co_writer 1 in
            buffered1 "_B" rd wr;
            List.iter (fun pro ->
                vhdl (sprintf "CHAN_%s_%s_GD <= CHAN_%s_%s_GD_A and CHAN_%s_%s_GD_B;"
                              co.co_name pro.pro_name
                              co.co_name pro.pro_name
                              co.co_name pro.pro_name);
                ) !pro_gds;
            !strl;
          | _ -> error 241938 "";
      end;
      | Chan_multi ->
      begin
          match model_buf with
          | Chan_unbuffered ->
            error 0 (sprintf "Multidirectional unbuffered channel <%s> not supported!"
                             co.co_name);
          | Chan_buffered ->
            out (sprintf "Creating multidirectional buffered channel <%s> [%s]..."
                             co.co_name
                             (sprint_dt co.co_type));
            bufferedn "" co.co_reader co.co_writer !pro_gds;
            !strl
          | _ -> error 241939 "";
      end;
      | _ -> error 241936 "";
    end;
    | OT_queue qu ->
    begin
      let co = qu.qu_obj in
      let ao = qu.qu_ao in
      let ao_name = ao.ao_name in
    

      let pro_gds = ref co.co_writer in
      List.iter (fun pro ->
                    if not (List.mem pro !pro_gds) then
                        pro_gds := !pro_gds @ [pro];
                    ) co.co_reader;
      let unlock = ref [] in
      let empty = ref [] in
      let full = ref [] in
      let init = ref [] in 
      List.iter (fun (sel,pro) ->
            match sel with
            | "unlock" -> unlock := !unlock @ [pro]; 
                          if not (List.mem pro !pro_gds) then pro_gds := !pro_gds @ [pro];
            | "empty" -> empty := !empty @ [pro];
            | "full" -> full := !full @ [pro];
            | _ -> error 0 
                (sprintf "Queue: unknown method <%s> found in object <%s> and process <%s>"
                         sel ao.ao_name pro.pro_name);
            ) ao.ao_procs;

      let gd_num = List.length !pro_gds in
      let depth = qu.qu_depth in
      let depth' = sel_width depth in
      let num_read = List.length co.co_reader in
      let num_write = List.length co.co_writer in


      (match qu.qu_ot with
       | None ->        
       begin
        let dt = co.co_type in
        stat "queue" (obj_decl_type co.co_type);
        let width = size_of_dt co.co_type in
        out (sprintf "Creating queue <%s> [%s, depth=%d width=%d #rd=%d #wr=%d]..."
                     co.co_name
                     (sprint_dt co.co_type)
                     depth
                     width
                     num_read
                     num_write);

        vhdl "-- Single Port Queue-RAM Implementation";
        vhdl (sprintf "IMPL_QRAM_%s: process(" co.co_name);
        vhdl_ind := 8;
        vhdl (sprintf "QRAM_%s_ADDR_AUX," co.co_name);
        vhdl (sprintf "QRAM_%s_DIN_AUX," co.co_name);
        vhdl (sprintf "QRAM_%s_WE_AUX," co.co_name);
        if pro <> None then
          vhdl (sprintf "PRO_%s_ENABLE,"
                (get_some pro).pro_name);
        vhdl (sprintf "conpro_system_clk,");
        vhdl (sprintf "conpro_system_reset");
        vhdl ")";
        vhdl_ind := 0; 
        vhdl "begin";
        vhdl_ind := 2;

        vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk='%d' then"
                      (get_env_int "clock_level"));
        vhdl_ind := 2;
        incr vhdl_ind;
        vhdl (sprintf "if QRAM_%s_WE_AUX = '1' then" co.co_name);
        incr vhdl_ind;
        let lc,rc = vhdl_convert (DT_logic depth') (DT_natural 0) in
        vhdl (sprintf "QRAM_%s(%sQRAM_%s_ADDR_AUX%s) <= QRAM_%s_DIN_AUX;"
                    co.co_name
                    lc co.co_name rc
                    co.co_name
                    );
        decr vhdl_ind;
        vhdl "end if;";
        vhdl (sprintf "QRAM_%s_RD_ADDR <= QRAM_%s_ADDR_AUX;"
                      co.co_name co.co_name);
        decr vhdl_ind;
        vhdl "end if;";
        vhdl_ind := 0;
        vhdl (sprintf "end process IMPL_QRAM_%s;" co.co_name);
        let lc,rc = vhdl_convert (DT_logic depth') (DT_natural 0) in
        vhdl (sprintf "QRAM_%s_DOUT_AUX <= QRAM_%s(%sQRAM_%s_RD_ADDR%s);" 
                    co.co_name
                    co.co_name
                    lc co.co_name rc
                    );

      end;
       | Some (OT_struct st) ->
       begin
       List.iter (fun ot ->
        let co =
          match co_of_ot ot with
          | Some co -> co;
          | None -> error 898127 "" in
        stat "queue" (obj_decl_type co.co_type);
        let dt = co.co_type in
        let width = size_of_dt dt in
        out (sprintf "Creating queue <%s> [%s, depth=%d width=%d #rd=%d #wr=%d]..."
                     co.co_name
                     (sprint_dt co.co_type)
                     depth
                     width
                     num_read
                     num_write);

        vhdl "-- Single Port Queue-RAM Implementation";
        vhdl (sprintf "IMPL_QRAM_%s: process(" co.co_name);
        vhdl_ind := 8;
        vhdl (sprintf "QRAM_%s_ADDR_AUX," co.co_name);
        vhdl (sprintf "QRAM_%s_DIN_AUX," co.co_name);
        vhdl (sprintf "QRAM_%s_WE_AUX," ao_name);
        if pro <> None then
          vhdl (sprintf "PRO_%s_ENABLE,"
                (get_some pro).pro_name);
        vhdl (sprintf "conpro_system_clk,");
        vhdl (sprintf "conpro_system_reset");
        vhdl ")";
        vhdl_ind := 0; 
        vhdl "begin";
        vhdl_ind := 2;

        vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk='%d' then"
                      (get_env_int "clock_level"));
        vhdl_ind := 2;
        incr vhdl_ind;
        vhdl (sprintf "if QRAM_%s_WE_AUX = '1' then" ao_name);
        incr vhdl_ind;
        let lc,rc = vhdl_convert (DT_logic depth') (DT_natural 0) in
        vhdl (sprintf "QRAM_%s(%sQRAM_%s_ADDR_AUX%s) <= QRAM_%s_DIN_AUX;"
                    co.co_name
                    lc co.co_name rc
                    co.co_name
                    );
        decr vhdl_ind;
        vhdl "end if;";
        vhdl (sprintf "QRAM_%s_RD_ADDR <= QRAM_%s_ADDR_AUX;"
                      co.co_name co.co_name);
        decr vhdl_ind;
        vhdl "end if;";
        vhdl_ind := 0;
        vhdl (sprintf "end process IMPL_QRAM_%s;" co.co_name);
        let lc,rc = vhdl_convert (DT_logic depth') (DT_natural 0) in
        vhdl (sprintf "QRAM_%s_DOUT_AUX <= QRAM_%s(%sQRAM_%s_RD_ADDR%s);" 
                    co.co_name
                    co.co_name
                    lc co.co_name rc
                    );
        vhdl (sprintf "QRAM_%s_ADDR_AUX <= QRAM_%s_ADDR_AUX;" 
                    co.co_name
                    ao_name);

       ) st.st_objs;  
       end;
       | _ -> error 59370 "");

      vhdl "-- Queue scheduler";
      vhdl (sprintf "IMPL_QUEUE_%s: process(" co.co_name);
      vhdl_ind := 8;
      List.iter (fun pro ->
                    vhdl (sprintf "QUEUE_%s_%s_RE," 
                              co.co_name
                              pro.pro_name);
            ) co.co_reader;
      List.iter (fun pro ->
                    vhdl (sprintf "QUEUE_%s_%s_WE," 
                              co.co_name
                              pro.pro_name);
            ) co.co_writer;
      List.iter (fun pro ->
                    vhdl (sprintf "QUEUE_%s_%s_INIT," 
                              co.co_name
                              pro.pro_name);
            ) !init;
      List.iter (fun pro ->
                    vhdl (sprintf "QUEUE_%s_%s_EMPTY_RE," 
                              co.co_name
                              pro.pro_name);
            ) !empty;
      List.iter (fun pro ->
                    vhdl (sprintf "QUEUE_%s_%s_FULL_RE," 
                              co.co_name
                              pro.pro_name);
            ) !full;
      List.iter (fun pro ->
                    vhdl (sprintf "QUEUE_%s_%s_UNLOCK," 
                              co.co_name
                              pro.pro_name);
            ) !unlock;
      vhdl (sprintf "QUEUE_%s_ADDR_RD," co.co_name);
      vhdl (sprintf "QUEUE_%s_ADDR_WR," co.co_name);

      (*
      ** Always present
      *)
      if pro <> None then
          vhdl (sprintf "PRO_%s_enable,"
                (get_some pro).pro_name);
      vhdl (sprintf "conpro_system_clk,");
      vhdl (sprintf "conpro_system_reset");
      vhdl ")";
      vhdl_ind := 2;
      vhdl (sprintf "variable wr_next: %s;"
                      (obj_decl_type (DT_logic depth')));
      vhdl (sprintf "variable rd_next: %s;"
                      (obj_decl_type (DT_logic depth')));
      vhdl_ind := 0; 
      vhdl "begin";
      vhdl_ind := 2;

      vhdl (sprintf "wr_next := QUEUE_%s_ADDR_WR + 1;"
                          co.co_name);
      vhdl (sprintf "rd_next := QUEUE_%s_ADDR_RD + 1;"
                          co.co_name);

      vhdl (sprintf "if conpro_system_clk'event and conpro_system_clk = '%d' then"
                      (get_env_int "clock_level"));
      vhdl_ind := 4;
      vhdl (sprintf "if conpro_system_reset = '%d'"
                      (get_env_int "reset_level"));
      vhdl_ind := 8;
      List.iter (fun pro ->
            vhdl (sprintf "or QUEUE_%s_%s_INIT = '1'"
                              co.co_name
                              pro.pro_name);
            ) !init;
      vhdl_ind := 4;
      vhdl "then";        
      vhdl_ind := 6;        
      List.iter (fun pro ->
            vhdl (sprintf 
                  "QUEUE_%s_%s_GD <= '1';"
                      co.co_name pro.pro_name)
            ) !pro_gds;
      vhdl (sprintf "QUEUE_%s_EMPTY <= '1';" co.co_name);
      vhdl (sprintf "QUEUE_%s_FULL <= '0';" co.co_name);
      vhdl (sprintf "QUEUE_%s_ADDR_RD <= %s;" 
                      co.co_name (val_str (DT_logic depth') (V_int Int64.zero)));
      vhdl (sprintf "QUEUE_%s_ADDR_WR <= %s;" 
                      co.co_name (val_str (DT_logic depth') (V_int Int64.zero)));
      vhdl (sprintf "QUEUE_%s_LOCKED <= '0';"
                      co.co_name);
      vhdl (sprintf "QRAM_%s_WE_AUX <= '0';"
                      co.co_name);
      vhdl (sprintf "QRAM_%s_ADDR_AUX <= %s;" 
                      co.co_name (val_str (DT_logic depth') (V_int Int64.zero)));
      (match qu.qu_ot with
       | None ->
          let dt = co.co_type in
          let width = size_of_dt dt in
          vhdl (sprintf "QRAM_%s_DIN_AUX <= %s;" 
                      co.co_name (val_str (DT_logic width) (V_int Int64.zero)));
       | Some (OT_struct st) ->
       List.iter (fun ot ->
          let co =
            match co_of_ot ot with
              | Some co -> co;
              | None -> error 628245 "" in
          let dt = co.co_type in
          let width = size_of_dt dt in
          vhdl (sprintf "QRAM_%s_DIN_AUX <= %s;" 
                      co.co_name (val_str (DT_logic width) (V_int Int64.zero)));
          ) st.st_objs;
       | _ -> error 855432 "");

      vhdl_ind := 4;
      vhdl "else";
      vhdl_ind := 6;
      vhdl (sprintf 
                "QRAM_%s_ADDR_AUX <= QUEUE_%s_ADDR_RD;"
                co.co_name 
                co.co_name);

      let i = ref 0 in
      let n = ref num_write in
      List.iter (fun pro ->
            incr i;
            decr n;
            vhdl (sprintf "%s QUEUE_%s_LOCKED = '0' and QUEUE_%s_%s_WE = '1' and QUEUE_%s_FULL = '0' then"
                          (if !i = 1 then "if" else "elsif")
                          co.co_name
                          co.co_name
                          pro.pro_name
                          co.co_name
                          );
            vhdl_ind := 8;
            vhdl (sprintf 
                  "if wr_next = QUEUE_%s_ADDR_RD then QUEUE_%s_FULL <= '1'; end if;"
                  co.co_name co.co_name);
            (match qu.qu_ot with
             | None ->
              let dt = co.co_type in
              let width = size_of_dt dt in
              vhdl (sprintf 
                    "QRAM_%s_DIN_AUX <= %s;"
                    co.co_name 
                    (match dt with
                     | DT_int n -> sprintf "std_logic_vector(QUEUE_%s_%s_WR)" co.co_name pro.pro_name;
                     | _ -> sprintf "QUEUE_%s_%s_WR" co.co_name pro.pro_name;
                    ));
             | Some (OT_struct st) ->
              List.iter (fun ot ->
                  let co =
                    match co_of_ot ot with
                      | Some co -> co;
                      | None -> error 313528 "" in
                  let dt = co.co_type in
                  let width = size_of_dt dt in
                  vhdl (sprintf 
                        "QRAM_%s_DIN_AUX <= %s;"
                        co.co_name 
                        (match dt with
                         | DT_int n -> sprintf "std_logic_vector(QUEUE_%s_%s_WR)" co.co_name pro.pro_name;
                         | _ -> sprintf "QUEUE_%s_%s_WR" co.co_name pro.pro_name;
                        ));
              ) st.st_objs;
             | _ -> error 73892 "");

            vhdl (sprintf "QRAM_%s_ADDR_AUX <= QUEUE_%s_ADDR_WR;"
                          co.co_name co.co_name);
            vhdl (sprintf "QRAM_%s_WE_AUX <= '1';"
                          co.co_name);
            vhdl (sprintf "QUEUE_%s_ADDR_WR <= wr_next;"
                          co.co_name);
            vhdl (sprintf "QUEUE_%s_LOCKED <= '1';"
                          co.co_name);
            vhdl (sprintf "QUEUE_%s_%s_GD <= '0';"
                          co.co_name pro.pro_name);
            vhdl (sprintf "QUEUE_%s_EMPTY <= '0';"
                          co.co_name);
            vhdl_ind := 6;
            ) co.co_writer;
            
      vhdl_ind := 6;

      let i = ref 0 in
      let n = ref num_read in
      List.iter (fun pro ->
            incr i;
            decr n;
            vhdl (sprintf "elsif QUEUE_%s_LOCKED = '0' and QUEUE_%s_%s_RE = '1' and QUEUE_%s_EMPTY = '0' then"
                          co.co_name
                          co.co_name
                          pro.pro_name
                          co.co_name
                          );
            vhdl_ind := 8;
            vhdl (sprintf 
                  "if rd_next = QUEUE_%s_ADDR_WR then QUEUE_%s_EMPTY <= '1'; end if;"
                  co.co_name co.co_name);
            vhdl (sprintf "QUEUE_%s_ADDR_RD <= rd_next;"
                          co.co_name);
            vhdl (sprintf "QUEUE_%s_LOCKED <= '1';"
                          co.co_name);
            vhdl (sprintf "QUEUE_%s_%s_GD <= '0';"
                          co.co_name pro.pro_name);
            vhdl (sprintf "QUEUE_%s_FULL <= '0';"
                          co.co_name);
            vhdl_ind := 6;
            ) co.co_reader;

      vhdl_ind := 6;
      let n = ref (List.length !unlock) in
      let i = ref 0 in
      List.iter (fun pro ->
            decr n;
            incr i;
            vhdl (sprintf "%s (QUEUE_%s_LOCKED = '0' and QUEUE_%s_%s_UNLOCK = '1') %s"
                          (if !i = 1 then "elsif" else "     ")
                          co.co_name
                          co.co_name pro.pro_name
                          (if !n > 0 then "or" else "then"));
            ) !unlock;
      vhdl_ind := 8;
      if !unlock <> [] then
      begin
        List.iter (fun pro ->
            vhdl (sprintf 
                  "QUEUE_%s_%s_GD <= '0';"
                      co.co_name pro.pro_name) 
            ) !pro_gds;
        vhdl (sprintf "QUEUE_%s_LOCKED <= '1';"
                      co.co_name);
      end;
      let n = ref (List.length !full) in
      let i = ref 0 in
      List.iter (fun pro ->
            decr n;
            incr i;
            vhdl_ind := 6;
            vhdl (sprintf "elsif QUEUE_%s_LOCKED = '0' and QUEUE_%s_%s_FULL_RE = '1' then"
                          co.co_name
                          co.co_name pro.pro_name);
            vhdl_ind := 8;
            vhdl (sprintf 
                  "QUEUE_%s_%s_GD <= '0';"
                      co.co_name pro.pro_name);
            vhdl (sprintf 
                  "QUEUE_%s_%s_FULL_RD <= QUEUE_%s_FULL;"
                      co.co_name pro.pro_name 
                      co.co_name); 
            vhdl (sprintf "QUEUE_%s_LOCKED <= '1';"
                      co.co_name);
            ) !full;
      let n = ref (List.length !empty) in
      let i = ref 0 in
      List.iter (fun pro ->
            decr n;
            incr i;
            vhdl_ind := 6;
            vhdl (sprintf "elsif QUEUE_%s_LOCKED = '0' and QUEUE_%s_%s_EMPTY_RE = '1' then"
                          co.co_name
                          co.co_name pro.pro_name);
            vhdl_ind := 8;
            vhdl (sprintf 
                  "QUEUE_%s_%s_GD <= '0';"
                      co.co_name pro.pro_name);
            vhdl (sprintf 
                  "QUEUE_%s_%s_EMPTY_RD <= QUEUE_%s_EMPTY;"
                      co.co_name pro.pro_name 
                      co.co_name); 
            vhdl (sprintf "QUEUE_%s_LOCKED <= '1';"
                      co.co_name);
            ) !empty;
            
      vhdl_ind := 6;
      vhdl (sprintf "elsif QUEUE_%s_LOCKED = '1' then"
                      co.co_name);
      vhdl_ind := 8;
      vhdl (sprintf "QUEUE_%s_LOCKED <= '0';"
                      co.co_name);
      vhdl (sprintf "QRAM_%s_WE_AUX <= '0';"
                      co.co_name);
      List.iter (fun pro ->
            vhdl (sprintf 
                  "QUEUE_%s_%s_GD <= '1';"
                      co.co_name pro.pro_name) 
            ) !pro_gds;
            
      vhdl "end if;";
      vhdl_ind := 4;
      vhdl "end if;";
      vhdl_ind := 2;
      vhdl "end if;";
      vhdl_ind := 0;
      vhdl (sprintf "end process IMPL_QUEUE_%s;" co.co_name);
      List.iter (fun pro ->
        match qu.qu_ot with
        | None ->
            let dt = co.co_type in
            vhdl (sprintf "QUEUE_%s_%s_RD <= %s;"
                          co.co_name pro.pro_name
                         (match dt with
                          | DT_int n -> sprintf "signed(QRAM_%s_DOUT_AUX)" co.co_name;
                          | _ -> sprintf "QRAM_%s_DOUT_AUX" co.co_name;
                         ));
        | Some (OT_struct st) ->
            List.iter (fun ot' ->
                match co_of_ot ot' with
                | Some co' ->
                  let dt = co'.co_type in
                  vhdl (sprintf "QUEUE_%s_%s_RD <= %s;"
                        co'.co_name pro.pro_name
                        (match dt with
                        | DT_int n -> sprintf "signed(QRAM_%s_DOUT_AUX)" co'.co_name;
                        | _ -> sprintf "QRAM_%s_DOUT_AUX" co'.co_name;
                        ));
                    
                | None -> error 492328 "";
              ) st.st_objs;
        | _ -> error 80354 "";
            ) co.co_reader;
      vhdl "";
      !strl
    end;
    | OT_array at -> 
    if not (List.mem AT_temp at.at_flags) then
    begin
    match pro with
    | None ->
      let is_block = List.mem AT_block at.at_flags in
      let is_dyn = List.mem AT_dyn at.at_flags in
      let size = proda at.at_dim in
      let ex = sym_check_sym modu.mod_export (Sym_obj obj) in
      if not is_dyn && not is_block then
      begin
        let s = ref [] in
        Array.iter (fun ot ->
                    let co =
                        match co_of_ot ot with
                        | Some co -> co;
                        | None -> error 594177 ""; in
                    let ex' = sym_check_sym modu.mod_export (Sym_obj ot) in
                    let rd = co.co_reader <> [] in
                    let wr = co.co_writer <> [] in
                    let sym = Sym_obj ot in
                    if (rd || ex || ex') && wr then 
                    begin
                        s := !s @ (obj_code sym modu pro);
                    end;
            ) at.at_objs;
            !s
      end
      else if not is_block then
      begin
        (*
        ** Dynamic selector access. OT dependent implementation...
        *)
        match at.at_objs.(0) with
        | OT_reg co ->

          let sel_size = proda at.at_dim in
          let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in

          let gd_rd,gd_wr = array_guard at in
          let rd = ref [] in
          let wr = ref [] in
          let gd = ref [] in
          let sel = ref [] in
          let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709868 "" in
          Array.iter (fun ot -> 
                        let co = 
                            match co_of_ot ot with
                            | Some co -> co;
                            | None -> error 709867 ""; in
                        List.iter (fun pro ->
                            if not (List.mem pro !rd) then
                                rd := !rd @ [pro];
                            ) co.co_reader;
                        List.iter (fun pro ->
                            if not (List.mem pro !wr) then
                                wr := !wr @ [pro];
                            ) co.co_writer;
                    )  at.at_objs;
          List.iter (fun pro ->
                    if not (List.mem pro !sel) then
                        sel := !sel @ [pro];
                    ) (!rd @ !wr);
          let rd,wr,gd,sel = !rd, !wr, !wr, !sel in
          let gd_rd,gd_wr=array_guard at in

          (*
          ** Read selector
          *)
          vhdl "-- Dynamic selector ARRAY implementation";
          vhdl "-- Read selector";
          let lc,rc = vhdl_convert (DT_logic sel_width) (DT_natural 0) in
          List.iter (fun pro ->
            vhdl (sprintf "ARRAY_%s_%s_RD <= ARRAY_%s(%sARRAY_%s_%s_SEL%s);" 
                              at.at_name pro.pro_name
                              at.at_name
                              lc at.at_name pro.pro_name rc)
            ) rd;
          vhdl "-- Operation selectors";
          List.iter (fun pro ->
            for i = 0 to (size-1)
            do    
                vhdl (sprintf 
                        "ARRAY_%s_%s_WEA(%d) <= '1' when ARRAY_%s_%s_WE='1' and ARRAY_%s_%s_SEL=%s else '0';" 
                        at.at_name pro.pro_name i
                        at.at_name pro.pro_name 
                        at.at_name pro.pro_name 
                        (val_str (DT_logic sel_width) (V_int (Int64.of_int i)))
                        );
            done;
            ) wr;

          if gd_wr then
          begin
              vhdl "-- Guard collectors";
              List.iter (fun pro ->
                vhdl (sprintf "ARRAY_%s_%s_GD <=" at.at_name pro.pro_name);

                
                for i = 0 to (size-1)
                do    
                    vhdl (sprintf "  ARRAY_%s_%s_GDA(%d) %s" 
                              at.at_name pro.pro_name
                              i
                              (if i <> (size-1) then "and" else ";")
                     );
                done;
                ) gd;
          end;

          vhdl "-- Write access";
          Array.iter (fun ot -> strl := !strl @
                        (obj_code (Sym_obj ot) modu pro))
            at.at_objs;
          vhdl "";
          !strl;
        | OT_channel ch ->
          let co = ch.ch_obj in

          let sel_size = proda at.at_dim in
          let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
          let sel_dt = DT_logic sel_width in

          let gd_rd,gd_wr = array_guard at in
          let rd = ref [] in
          let wr = ref [] in
          let gd = ref [] in
          let sel = ref [] in
          let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 719868 "" in
          Array.iter (fun ot -> 
                        let co = 
                            match co_of_ot ot with
                            | Some co -> co;
                            | None -> error 719867 ""; in
                        List.iter (fun pro ->
                            if not (List.mem pro !rd) then
                                rd := !rd @ [pro];
                            ) co.co_reader;
                        List.iter (fun pro ->
                            if not (List.mem pro !wr) then
                                wr := !wr @ [pro];
                            ) co.co_writer;
                    )  at.at_objs;
          List.iter (fun pro ->
                    if not (List.mem pro !sel) then
                        sel := !sel @ [pro];
                    ) (!rd @ !wr);
          let prd,pwr,pgd,psel = !rd, !wr, !sel, !sel in
          let gd_rd,gd_wr=array_guard at in
          let gd = gd_rd || gd_wr in

          vhdl "-- Channel array multiplexer";
          vhdl "-- Blocking collectors";
          List.iter (fun pro ->
                vhdl (sprintf "CHAN_%s_%s_GD <=" at.at_name pro.pro_name);
                for i = 0 to (size-1)
                do    
                    vhdl (sprintf "  CHAN_%s_%d_%s_GD %s" 
                              at.at_name i pro.pro_name
                              (if i <> (size-1) then "and" else ";")
                     );
                done;
                ) pgd;

          vhdl "-- Operation selectors";
          List.iter (fun pro ->
                for i = 0 to (size-1)
                do    
                  vhdl (sprintf 
                        "CHAN_%s_%d_%s_RE <= '1' when CHAN_%s_%s_RE='1' and CHAN_%s_%s_SEL=%s else '0';" 
                        at.at_name i pro.pro_name 
                        at.at_name pro.pro_name 
                        at.at_name pro.pro_name 
                        (val_str sel_dt (V_int (Int64.of_int i)))
                        );
                done;
                ) prd;
          List.iter (fun pro ->
                vhdl (sprintf  "CHAN_%s_%s_RD <= "
                               at.at_name pro.pro_name);
                for i = 0 to (size-1)
                do    
                  vhdl (sprintf 
                        "  CHAN_%s_%d_%s_RD when CHAN_%s_%s_SEL=%s else" 
                        at.at_name i pro.pro_name 
                        at.at_name pro.pro_name 
                        (val_str sel_dt (V_int (Int64.of_int i)))
                        );
                done;
                vhdl (sprintf "  %s;"
                              (val_str co.co_type (V_int Int64.zero)));
                ) prd;
          List.iter (fun pro ->
                for i = 0 to (size-1)
                do    
                  vhdl (sprintf 
                        "CHAN_%s_%d_%s_WE <= '1' when CHAN_%s_%s_WE='1' and CHAN_%s_%s_SEL=%s else '0';" 
                        at.at_name i pro.pro_name 
                        at.at_name pro.pro_name 
                        at.at_name pro.pro_name 
                        (val_str sel_dt (V_int (Int64.of_int i)))
                        );
                  vhdl (sprintf 
                        "CHAN_%s_%d_%s_WR <= CHAN_%s_%s_WR when CHAN_%s_%s_SEL=%s else %s;" 
                        at.at_name i pro.pro_name 
                        at.at_name pro.pro_name 
                        at.at_name pro.pro_name 
                        (val_str sel_dt (V_int (Int64.of_int i)))
                        (val_str co.co_type (V_int Int64.zero))
                        );
                done;
                ) pwr;


          vhdl "-- Channel object implementation";
          Array.iter (fun ot -> strl := !strl @
                        (obj_code (Sym_obj ot) modu pro))
            at.at_objs;
          vhdl "";
          !strl;
        | OT_queue qu ->
          let co = qu.qu_obj in

          let sel_size = proda at.at_dim in
          let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in
          let sel_dt = DT_logic sel_width in

          let gd_rd,gd_wr = array_guard at in
          let rd = ref [] in
          let wr = ref [] in
          let gd = ref [] in
          let sel = ref [] in
          let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 719868 "" in
          Array.iter (fun ot -> 
                        let co = 
                            match co_of_ot ot with
                            | Some co -> co;
                            | None -> error 719867 ""; in
                        List.iter (fun pro ->
                            if not (List.mem pro !rd) then
                                rd := !rd @ [pro];
                            ) co.co_reader;
                        List.iter (fun pro ->
                            if not (List.mem pro !wr) then
                                wr := !wr @ [pro];
                            ) co.co_writer;
                    )  at.at_objs;
          List.iter (fun pro ->
                    if not (List.mem pro !sel) then
                        sel := !sel @ [pro];
                    ) (!rd @ !wr);
          let prd,pwr,pgd,psel = !rd, !wr, !sel, !sel in
          let gd_rd,gd_wr=array_guard at in
          let gd = gd_rd || gd_wr in

          vhdl "-- Queue array multiplexer";
          vhdl "-- Blocking collectors";
          List.iter (fun pro ->
                vhdl (sprintf "QUEUE_%s_%s_GD <=" at.at_name pro.pro_name);
                for i = 0 to (size-1)
                do    
                    vhdl (sprintf "  QUEUE_%s_%d_%s_GD %s" 
                              at.at_name i pro.pro_name
                              (if i <> (size-1) then "and" else ";")
                     );
                done;
                ) pgd;

          vhdl "-- Operation selectors";
          List.iter (fun pro ->
                for i = 0 to (size-1)
                do    
                  vhdl (sprintf 
                        "QUEUE_%s_%d_%s_RE <= '1' when QUEUE_%s_%s_RE='1' and QUEUE_%s_%s_SEL=%s else '0';" 
                        at.at_name i pro.pro_name 
                        at.at_name pro.pro_name 
                        at.at_name pro.pro_name 
                        (val_str sel_dt (V_int (Int64.of_int i)))
                        );
                done;
                ) prd;
          List.iter (fun pro ->
                vhdl (sprintf  "QUEUE_%s_%s_RD <= "
                               at.at_name pro.pro_name);
                for i = 0 to (size-1)
                do    
                  vhdl (sprintf 
                        "  QUEUE_%s_%d_%s_RD when QUEUE_%s_%s_SEL=%s else" 
                        at.at_name i pro.pro_name 
                        at.at_name pro.pro_name 
                        (val_str sel_dt (V_int (Int64.of_int i)))
                        );
                done;
                vhdl (sprintf "  %s;"
                              (val_str co.co_type (V_int Int64.zero)));
                ) prd;
          List.iter (fun pro ->
                for i = 0 to (size-1)
                do    
                  vhdl (sprintf 
                        "QUEUE_%s_%d_%s_WE <= '1' when QUEUE_%s_%s_WE='1' and QUEUE_%s_%s_SEL=%s else '0';" 
                        at.at_name i pro.pro_name 
                        at.at_name pro.pro_name 
                        at.at_name pro.pro_name 
                        (val_str sel_dt (V_int (Int64.of_int i)))
                        );
                  vhdl (sprintf 
                        "QUEUE_%s_%d_%s_WR <= QUEUE_%s_%s_WR when QUEUE_%s_%s_SEL=%s else %s;" 
                        at.at_name i pro.pro_name 
                        at.at_name pro.pro_name 
                        at.at_name pro.pro_name 
                        (val_str sel_dt (V_int (Int64.of_int i)))
                        (val_str co.co_type (V_int Int64.zero))
                        );
                done;
                ) pwr;


          vhdl "-- Channel object implementation";
          Array.iter (fun ot -> strl := !strl @
                        (obj_code (Sym_obj ot) modu pro))
            at.at_objs;
          vhdl "";
          !strl;
        | _ -> error 434198 "";
      end
      else [];
   | Some pro ->
      let is_block = List.mem AT_block at.at_flags in
      let is_dyn = List.mem AT_dyn at.at_flags in
      let size = proda at.at_dim in

      if not is_dyn && not is_block then
      begin
        let s = ref [] in
        Array.iter (fun ot ->
                    let co =
                        match co_of_ot ot with
                        | Some co -> co;
                        | None -> error 294173 ""; in
                    let rd = co.co_reader <> [] in
                    let wr = co.co_writer <> [] in
                    let sym = Sym_obj ot in
                    if rd && wr then 
                    begin
                        s := !s @ (obj_code sym modu (Some pro));
                    end;
            ) at.at_objs;
            !s
      end
      else if not is_block then
      begin
        (*
        ** Dynamic selector access. OT dependent implementation...
        *)
        match at.at_objs.(0) with
        | OT_reg co ->

          let sel_size = proda at.at_dim in
          let sel_width = 
                    let w = const_width (V_int (Int64.of_int sel_size)) in
                    let s' = 1 lsl (w-1) in
                    if s' = sel_size then (w-1) else w in

          let gd_rd,gd_wr = array_guard at in
          let rd = ref [] in
          let wr = ref [] in
          let gd = ref [] in
          let sel = ref [] in
          let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 109868 "" in
              

          (*
          ** Read selector
          *)
          vhdl "-- Dynamic selector ARRAY implementation";
          vhdl "-- Read selector";
          let lc,rc = vhdl_convert (DT_logic sel_width) (DT_natural 0) in
          vhdl (sprintf "ARRAY_%s_RD <= ARRAY_%s(%sARRAY_%s_SEL%s);" 
                              at.at_name 
                              at.at_name
                              lc at.at_name rc); 
                    
          vhdl "";
          !strl;
        | _ -> error 434199 "";
      end
      else [];
    end else [];
    | OT_array_sel (at,sel) -> []; 
            (* ignored here - handled by OT_reg or Block *)
    | OT_struct st ->  [];
    | OT_component st -> 
    if not (sym_check_obj modu.mod_export st.st_name) then    
    begin
        let check_port te obj =
            let port = te.te_port in
            let name = te.te_name in
            let signal = name_of_ot obj in
            (*
            ** All ports must be mapped with still existing signals...
            *)
            match pro with
            | Some pro ->
                if not (sym_check_obj pro.pro_objs signal) then
                    error 0 (sprintf "<%s.%s>: Unconnected port <%s> in component <%s>"
                                     modu.mod_name 
                                     pro.pro_name 
                                     name st.st_name);
            | None -> 
                if not (sym_check_obj modu.mod_objs signal) then
                    error 0 (sprintf "<%s>: Unconnected port <%s> in component <%s>"
                                     modu.mod_name
                                     name st.st_name);
            in
        (*
        ** Map local signal to component port.
        *)
        let comp = ref [] in
        let st_type = st.st_type in
        comp := !comp @ [sprintf "COMP_%s: %s port map(" 
                                  st.st_name st_type.ts_name];
        let i = ref (List.length st_type.ts_elems) in
        let j = ref 0 in
        List.iter (fun te ->
            decr i;
            let obj = List.nth st.st_objs !j in 
            let port = 
                match te.te_port with
                | Some port -> port;
                | None -> error 390752 "";
                in
            check_port te obj;
            comp := !comp @ [sprintf "  %s => %s%s%s"
                    te.te_name 
                    (name_of_ot obj)
                    (if is_ot_local pro obj then "" else
                        match port with
                        | PT_in -> "_WR";
                        | PT_out -> "_RD";
                        | PT_bus -> "_BUS";
                    )         
                    (if !i = 0 then "" else ",")];
            incr j;
            ) st_type.ts_elems;
        comp := !comp @ [");"];
        !comp
    end else [];
    | _ ->  []
  end;
  | Sym_block db ->
  begin
    let db_rules = get_some db.db_rules in
    let emi = get_some db_rules.rl_child in
    emi.rl_obj_code (Sym_obj (ao_of_sym modu sym)) modu pro
  end;
  | _ -> error 420014 ""

