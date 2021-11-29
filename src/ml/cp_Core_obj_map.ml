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
*
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2006-2009 BSSLAB
**    $CREATED:     19.3.2006
**    $VERSION:     2.09
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

(*
** Object port mapping.
*)
let rec obj_map sym modu pro =
  let is_mon,is_debug,sym = get_mon sym in
  match sym with
  | Sym_obj obj ->
  begin
    match obj with
    | OT_signal co ->
    begin
        match pro with
        | Some pro ->
            if is_mon then
                error 291776 ""
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read or write access ?
                *)
                let rds,wrs=
                    (if (List.mem pro co.co_reader) then
                        [sprintf "%s_RD => %s_RD," 
                                 co.co_name
                                 co.co_name;
                        ]
                    else
                        []
                    ),
                    (if (List.mem pro co.co_writer) then
                        [sprintf "%s_WR => %s_WR," 
                                 co.co_name
                                 co.co_name;
                        ]
                    else    
                        []) in
                rds @ wrs
            end
            (* else error 166012 ""; *)
        | None -> 
            if is_mon then
                error 349159 ""
            else if not (List.mem Mod_main modu.mod_flags) then
            begin
                (*
                ** Route temporary signals up.
                *)
                let rds,wrs=
                    (if co.co_reader <> [] then
                        [sprintf "%s_RD => %s_RD," 
                                 co.co_name
                                 co.co_name;
                        ]
                    else
                        []
                    ),
                    (if co.co_writer <> [] then
                        [sprintf "%s_WR => %s_WR," 
                                 co.co_name
                                 co.co_name;
                        ]
                    else    
                        []) in
                rds @ wrs
            end
            else
            begin
                []; (* can't happen *)
            end;
    end;
    | OT_reg co ->
    begin
        match pro with
        | Some pro ->
            if is_mon then
            begin
                (*
                ** Monitors 
                *)
                [sprintf "MON_%s => MON_%s," 
                         co.co_name
                         co.co_name;
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let locked = 
                    if co.co_guard <> None then
                    [
                    (sprintf "MON_%s_LOCKED => MON_%s_LOCKED,"
                             co.co_name co.co_name);     
                    ] else [] in
                let pro_con = 
                    let l = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_WE => MON_%s_%s_WE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name);
                            ];
                            if co.co_guard <> None then
                            l := !l @ [ 
                            (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name);
                            ];
                            ) co.co_writer;
                    !l
                    in
                locked @ pro_con
                end
                else
                []);
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = co.co_guard <> None in
                let rds,wrs,gds =
                    (if rd then
                            [sprintf "REG_%s_RD => REG_%s_RD," 
                                     co.co_name
                                     co.co_name
                            ]
                     else
                            []
                     ),
                     (if wr then
                            [sprintf "REG_%s_WR => REG_%s_%s_WR," 
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                             sprintf "REG_%s_WE => REG_%s_%s_WE,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                            ]
                      else
                            []
                     ),
                     (if wr && gd then
                            [sprintf "REG_%s_GD => REG_%s_%s_GD,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds
            end
            (* else error 702622 ""; *)
        | None -> 
            if is_mon then
            begin
                (*
                ** Monitors
                *)
                [sprintf "MON_%s => MON_%s," 
                         co.co_name
                         co.co_name;
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let locked = 
                    if co.co_guard <> None then
                    [
                    (sprintf "MON_%s_LOCKED => MON_%s_LOCKED,"
                             co.co_name co.co_name);     
                    ] else [] in
                let pro_con = 
                    let l = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_WE => MON_%s_%s_WE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name);
                            ];
                            if co.co_guard <> None then
                            l := !l @ [ 
                            (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name);
                            ];
                            ) co.co_writer;
                    !l
                    in
                locked @ pro_con
                end
                else
                []);
            end
            else
                error 359021 ""
    end;
    | OT_channel ch ->
    begin
      let co = ch.ch_obj in
      let ao_p = 
        let ao = ch.ch_ao in
        match pro with
        | Some pro ->
          let unlock = ref [] in
          let empty = ref [] in
          let full = ref [] in
          List.iter (fun (sel,pro) ->
              match sel with
              | "unlock" -> unlock := !unlock @ [pro];
              | "empty" -> empty := !empty @ [pro];
              | "full" -> full := !full @ [pro];
              | _ -> error 0 
                          (sprintf "Channel: unknown method <%s> found in object <%s> and process <%s>"
                                    sel ao.ao_name pro.pro_name);
              ) ao.ao_procs;

          let s_unlock =
              if (List.mem pro !unlock) then
                  [
                  sprintf "CHANNEL_%s_UNLOCK => CHANNEL_%s_%s_UNLOCK," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  ]         
                  else [] in
          let s_empty =
              if (List.mem pro !empty) then
                  [
                  sprintf "CHANNEL_%s_EMPTY_RE => CHANNEL_%s_%s_EMPTY_RE," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  sprintf "CHANNEL_%s_EMPTY_RD => CHANNEL_%s_%s_EMPTY_RD," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  ]         
                  else [] in
          let s_full =
              if (List.mem pro !full) then
                  [
                  sprintf "CHANNEL_%s_FULL_RE => CHANNEL_%s_%s_FULL_RE," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  sprintf "CHANNEL_%s_FULL_RD => CHANNEL_%s_%s_FULL_RD," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  ]         
                  else [] in

          s_unlock@s_empty@s_full
        | None -> [] in
        
      let p = 
       match pro with
       | Some pro ->
       begin
          match ch.ch_ot with
          | None ->
          begin
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                [sprintf "MON_%s => MON_%s," 
                         co.co_name
                         co.co_name
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let locked = 
                    if co.co_guard <> None then
                    [
                    (sprintf "MON_%s_LOCKED => MON_%s_LOCKED,"
                             co.co_name 
                             co.co_name)
                    ] else [] in
                        
                let pro_con = 
                    let l = ref [] in
                    let pro_gds = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_WE => MON_%s_%s_WE, "
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;                              
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_RE => MON_%s_%s_RE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                locked @ pro_con
                end else [];
                );
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = rd || wr || ao_p <> [] in
                let rds,wrs,gds =
                    (if rd then
                            [sprintf "CHAN_%s_RD => CHAN_%s_%s_RD," 
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                             sprintf "CHAN_%s_RE => CHAN_%s_%s_RE,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                            ]
                     else
                            []
                     ),
                     (if wr then
                            [sprintf "CHAN_%s_WR => CHAN_%s_%s_WR," 
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                             sprintf "CHAN_%s_WE => CHAN_%s_%s_WE,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                            ]
                      else
                            []
                     ),
                     (if gd then
                            [sprintf "CHAN_%s_GD => CHAN_%s_%s_GD,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds
            end;
          end;
          | Some (OT_struct st) ->
          begin
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                [sprintf "MON_%s => MON_%s," 
                         co.co_name
                         co.co_name
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let locked = 
                    if co.co_guard <> None then
                    [
                    (sprintf "MON_%s_LOCKED => MON_%s_LOCKED,"
                             co.co_name 
                             co.co_name)
                    ] else [] in
                        
                let pro_con = 
                    let l = ref [] in
                    let pro_gds = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_WE => MON_%s_%s_WE, "
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;                              
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_RE => MON_%s_%s_RE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                locked @ pro_con
                end else [];
                );
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = rd || wr || ao_p <> [] in
                let rds,wrs,gds =
                    (if rd then
                       (List.map (fun ot ->
                        match co_of_ot ot with
                        | Some co ->
                            sprintf "CHAN_%s_RD => CHAN_%s_%s_RD," 
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                        | None -> error 32144 "" ;
                        ) st.st_objs )@
                        [sprintf "CHAN_%s_RE => CHAN_%s_%s_RE,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                        ]
                     else
                        []
                     ),
                     (if wr then
                       (List.map (fun ot ->
                        match co_of_ot ot with
                        | Some co ->
                            sprintf "CHAN_%s_WR => CHAN_%s_%s_WR," 
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                        | None -> error 927080 "" ;
                        ) st.st_objs )@
                        [sprintf "CHAN_%s_WE => CHAN_%s_%s_WE,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                        ]
                      else
                        []
                     ),
                     (if gd then
                            [sprintf "CHAN_%s_GD => CHAN_%s_%s_GD,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds
            end;
          end;
          | _ -> error 501738 "";
        end;
        | None -> 
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                [sprintf "MON_%s => MON_%s," 
                         co.co_name
                         co.co_name;
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let locked = 
                    [
                    (sprintf "MON_%s_LOCKED => MON_%s_LOCKED,"
                             co.co_name 
                             co.co_name)
                    ] in

                let pro_gds = ref [] in                        
                let pro_con = 
                    let l = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_WE => MON_%s_%s_WE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name)
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_RE => MON_%s_%s_RE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name)
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                locked @ pro_con
                end else [];
                )
            end
            else
                error 0  (sprintf "Channel <%s> can't be exported in toplevel module!" co.co_name);
       in
      ao_p@p
    end;
    | OT_queue qu ->
    begin
      let co = qu.qu_obj in
      let ao_p = 
        let ao = qu.qu_ao in
        match pro with
        | Some pro ->
          let unlock = ref [] in
          let empty = ref [] in
          let full = ref [] in
          List.iter (fun (sel,pro) ->
              match sel with
              | "unlock" -> unlock := !unlock @ [pro];
              | "empty" -> empty := !empty @ [pro];
              | "full" -> full := !full @ [pro];
              | _ -> error 0 
                          (sprintf "Queue: unknown method <%s> found in object <%s> and process <%s>"
                                    sel ao.ao_name pro.pro_name);
              ) ao.ao_procs;

          let s_unlock =
              if (List.mem pro !unlock) then
                  [
                  sprintf "QUEUE_%s_UNLOCK => QUEUE_%s_%s_UNLOCK," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  ]         
                  else [] in
          let s_empty =
              if (List.mem pro !empty) then
                  [
                  sprintf "QUEUE_%s_EMPTY_RE => QUEUE_%s_%s_EMPTY_RE," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  sprintf "QUEUE_%s_EMPTY_RD => QUEUE_%s_%s_EMPTY_RD," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  ]         
                  else [] in
          let s_full =
              if (List.mem pro !full) then
                  [
                  sprintf "QUEUE_%s_FULL_RE => QUEUE_%s_%s_FULL_RE," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  sprintf "QUEUE_%s_FULL_RD => QUEUE_%s_%s_FULL_RD," 
                          ao.ao_name
                          ao.ao_name pro.pro_name;
                  ]         
                  else [] in

          s_unlock@s_empty@s_full
        | None -> [] in
        
      let p = match pro with
        | Some pro ->
        begin
          match qu.qu_ot with
          | None ->
          begin
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                [sprintf "MON_%s => MON_%s," 
                         co.co_name
                         co.co_name
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let locked = 
                    if co.co_guard <> None then
                    [
                    (sprintf "MON_%s_LOCKED => MON_%s_LOCKED,"
                             co.co_name 
                             co.co_name)
                    ] else [] in
                        
                let pro_con = 
                    let l = ref [] in
                    let pro_gds = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_WE => MON_%s_%s_WE, "
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;                              
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_RE => MON_%s_%s_RE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                locked @ pro_con
                end else [];
                );
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = rd || wr || ao_p <> [] in
                let rds,wrs,gds =
                    (if rd then
                            [sprintf "QUEUE_%s_RD => QUEUE_%s_%s_RD," 
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                             sprintf "QUEUE_%s_RE => QUEUE_%s_%s_RE,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                            ]
                     else
                            []
                     ),
                     (if wr then
                            [sprintf "QUEUE_%s_WR => QUEUE_%s_%s_WR," 
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                             sprintf "QUEUE_%s_WE => QUEUE_%s_%s_WE,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                            ]
                      else
                            []
                     ),
                     (if gd then
                            [sprintf "QUEUE_%s_GD => QUEUE_%s_%s_GD,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds
            end;
          end;
          | Some (OT_struct st) ->
          begin
            (*
            ** Queue of structur type
            *)
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                [sprintf "MON_%s => MON_%s," 
                         co.co_name
                         co.co_name
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let locked = 
                    if co.co_guard <> None then
                    [
                    (sprintf "MON_%s_LOCKED => MON_%s_LOCKED,"
                             co.co_name 
                             co.co_name)
                    ] else [] in
                        
                let pro_con = 
                    let l = ref [] in
                    let pro_gds = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_WE => MON_%s_%s_WE, "
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;                              
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_RE => MON_%s_%s_RE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                       co.co_name pro.pro_name
                                       co.co_name pro.pro_name);
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                locked @ pro_con
                end else [];
                );
            end
            else (* if (sym_check_obj pro.pro_import co.co_name) then *)
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = List.mem pro co.co_reader in
                let wr = List.mem pro co.co_writer in
                let gd = rd || wr || ao_p <> [] in
                let rds,wrs,gds =
                    (if rd then
                       (List.map (fun ot ->
                        match co_of_ot ot with
                        | Some co ->
                            sprintf "QUEUE_%s_RD => QUEUE_%s_%s_RD," 
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                        | None -> error 874618 "" ;
                        ) st.st_objs )@
                        [sprintf "QUEUE_%s_RE => QUEUE_%s_%s_RE,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                        ]
                     else
                            []
                     ),
                     (if wr then
                       (List.map (fun ot ->
                        match co_of_ot ot with
                        | Some co ->
                            sprintf "QUEUE_%s_WR => QUEUE_%s_%s_WR," 
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                        | None -> error 874618 "" ;
                        ) st.st_objs )@
                        [sprintf "QUEUE_%s_WE => QUEUE_%s_%s_WE,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name;
                        ]
                      else
                            []
                     ),
                     (if gd then
                            [sprintf "QUEUE_%s_GD => QUEUE_%s_%s_GD,"
                                     co.co_name
                                     co.co_name
                                     pro.pro_name
                            ]
                      else
                            []
                      )
                      in
                rds @ wrs @ gds
            end;
          end;
          | _ -> error 609957 "";
        end;
        | None -> 
            if is_mon then
            begin
                (*
                ** Monitors are always of type logic!
                *)
                [sprintf "MON_%s => MON_%s," 
                         co.co_name
                         co.co_name;
                ] @ (if is_debug then
                begin
                (*
                ** With auxilliary signals, too.
                *)
                let locked = 
                    [
                    (sprintf "MON_%s_LOCKED => MON_%s_LOCKED,"
                             co.co_name 
                             co.co_name)
                    ] in

                let pro_gds = ref [] in                        
                let pro_con = 
                    let l = ref [] in
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_WE => MON_%s_%s_WE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name)
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_writer;
                    List.iter (fun pro ->
                            l := !l @ [
                            (sprintf "MON_%s_%s_RE => MON_%s_%s_RE,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name
                                     );
                            ];
                            if not (List.mem pro !pro_gds) then
                            begin
                              l := !l @ [ 
                              (sprintf "MON_%s_%s_GD => MON_%s_%s_GD,"
                                     co.co_name pro.pro_name
                                     co.co_name pro.pro_name)
                              ];
                              pro_gds := !pro_gds @ [pro];
                            end;
                            ) co.co_reader;
                    !l
                    in
                locked @ pro_con
                end else [];
                )
            end
            else
                error 0  (sprintf "Queue <%s> can't be exported in toplevel module!" co.co_name);
        in
      ao_p@p
    end;
    | OT_array at ->
    if not (List.mem AT_temp at.at_flags) then
    begin
        match pro with
        | Some pro ->
            let is_block = List.mem AT_block at.at_flags in
            let is_dyn = List.mem AT_dyn at.at_flags in
            if not is_dyn && not is_block then
            begin
                let s = ref [] in
                Array.iter (fun ot ->
                    s := !s @ (obj_map (Sym_obj ot) modu (Some pro));
                    ) at.at_objs;
                !s
            end
            else if not is_block then
            begin
              match at.at_objs.(0) with
              | OT_reg co ->
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = ref false in
                let wr = ref false in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709866 "" in
                Array.iter (fun ot -> 
                        let co = 
                            match co_of_ot ot with
                            | Some co -> co;
                            | None -> error 709865 ""; in
                        if List.mem pro co.co_reader then
                            rd := true;
                        if List.mem pro co.co_writer then
                            wr := true;
                    )  at.at_objs;
                let rd,wr = !rd, !wr in
                let gd_rd,gd_wr = array_guard at in
                let rds,wrs,gds,sel =
                    (if rd then
                            [sprintf "ARRAY_%s_RD => ARRAY_%s_%s_RD," 
                                     at.at_name 
                                     at.at_name pro.pro_name;]
                     else
                            []
                     ),
                    (if wr then
                            [sprintf "ARRAY_%s_WR => ARRAY_%s_%s_WR," 
                                     at.at_name 
                                     at.at_name pro.pro_name;
                             sprintf "ARRAY_%s_WE => ARRAY_%s_%s_WE,"
                                     at.at_name
                                     at.at_name pro.pro_name;
                            ]
                     else
                            []
                     ),
                     (if gd_wr && wr then
                            [sprintf "ARRAY_%s_GD => ARRAY_%s_%s_GD,"
                                     at.at_name
                                     at.at_name pro.pro_name;
                            ]
                      else
                            []
                      ),
                      [
                            sprintf "ARRAY_%s_SEL => ARRAY_%s_%s_SEL,"
                                    at.at_name
                                    at.at_name pro.pro_name; 
                      ]
                      in
                rds @ wrs @ gds @ sel
              | OT_channel ch ->
                (*
                ** Process port
                *)
                let co = ch.ch_obj in
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = ref false in
                let wr = ref false in
                let unlock = ref false in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709867 "" in
                Array.iter (fun ot -> 
                    match ot with
                    | OT_channel ch -> 
                        let co = ch.ch_obj in
                        let ao = ch.ch_ao in
                        if List.mem pro co.co_reader then
                            rd := true;
                        if List.mem pro co.co_writer then
                            wr := true;
                        List.iter (fun (sel',pro') ->
                          match sel' with
                          | "unlock" -> if pro' = pro then unlock := true;
                          | _ -> ()) ao.ao_procs; 
                    | _ -> ();
                    )  at.at_objs;
                let rd,wr = !rd, !wr in
                let gd_rd,gd_wr = array_guard at in
                let gd = gd_rd || gd_wr in
                let s_unlock =
                    if  !unlock then
                        [
                        sprintf "CHAN_%s_UNLOCK => CHAN_%s_%s_UNLOCK," 
                                at.at_name
                                at.at_name pro.pro_name;
                        ]         
                        else [] in
                let rds,wrs,gds,sel =
                    (if rd then
                            [sprintf "CHAN_%s_RD => CHAN_%s_%s_RD," 
                                     at.at_name 
                                     at.at_name pro.pro_name;
                             sprintf "CHAN_%s_RE => CHAN_%s_%s_RE,"
                                     at.at_name
                                     at.at_name pro.pro_name;
                            ]
                     else
                            []
                     ),
                    (if wr then
                            [sprintf "CHAN_%s_WR => CHAN_%s_%s_WR," 
                                     at.at_name 
                                     at.at_name pro.pro_name;
                             sprintf "CHAN_%s_WE => CHAN_%s_%s_WE,"
                                     at.at_name
                                     at.at_name pro.pro_name;
                            ]
                     else
                            []
                     ),
                     (if gd then
                            [sprintf "CHAN_%s_GD => CHAN_%s_%s_GD,"
                                     at.at_name
                                     at.at_name pro.pro_name;
                            ]
                      else
                            []
                      ),
                      [
                            sprintf "CHAN_%s_SEL => CHAN_%s_%s_SEL,"
                                    at.at_name
                                    at.at_name pro.pro_name; 
                      ]
                      in
                rds @ wrs @ gds @ sel @ s_unlock
              | OT_queue qu ->
                (*
                ** Process port
                *)
                let co = qu.qu_obj in
                (*
                ** Read and write access is handled with different
                ** signals! Write access requires guard and write
                ** enable signals.
                *)
                let rd = ref false in
                let wr = ref false in
                let unlock = ref false in
                let dt = 
                    match dt_of_ot at.at_objs.(0) with
                    | Some dt -> dt;
                    | None -> error 709868 "" in
                Array.iter (fun ot -> 
                    match ot with
                    | OT_queue qu -> 
                        let co = qu.qu_obj in
                        let ao = qu.qu_ao in
                        if List.mem pro co.co_reader then
                            rd := true;
                        if List.mem pro co.co_writer then
                            wr := true;
                        List.iter (fun (sel',pro') ->
                          match sel' with
                          | "unlock" -> if pro' = pro then unlock := true;
                          | _ -> ()) ao.ao_procs; 
                    | _ -> ();
                    )  at.at_objs;
                let rd,wr = !rd, !wr in
                let gd_rd,gd_wr = array_guard at in
                let gd = gd_rd || gd_wr in
                let s_unlock =
                    if  !unlock then
                        [
                        sprintf "QUEUE_%s_UNLOCK => QUEUE_%s_%s_UNLOCK," 
                                at.at_name
                                at.at_name pro.pro_name;
                        ]         
                        else [] in
                let rds,wrs,gds,sel =
                    (if rd then
                            [sprintf "QUEUE_%s_RD => QUEUE_%s_%s_RD," 
                                     at.at_name 
                                     at.at_name pro.pro_name;
                             sprintf "QUEUE_%s_RE => QUEUE_%s_%s_RE,"
                                     at.at_name
                                     at.at_name pro.pro_name;
                            ]
                     else
                            []
                     ),
                    (if wr then
                            [sprintf "QUEUE_%s_WR => QUEUE_%s_%s_WR," 
                                     at.at_name 
                                     at.at_name pro.pro_name;
                             sprintf "QUEUE_%s_WE => QUEUE_%s_%s_WE,"
                                     at.at_name
                                     at.at_name pro.pro_name;
                            ]
                     else
                            []
                     ),
                     (if gd then
                            [sprintf "QUEUE_%s_GD => QUEUE_%s_%s_GD,"
                                     at.at_name
                                     at.at_name pro.pro_name;
                            ]
                      else
                            []
                      ),
                      [
                            sprintf "QUEUE_%s_SEL => QUEUE_%s_%s_SEL,"
                                    at.at_name
                                    at.at_name pro.pro_name; 
                      ]
                      in
                rds @ wrs @ gds @ sel @ s_unlock
              | _ -> error 888789 "";
            end
            else
                []      (* emitted just only one time with Sym_block *)
        | None -> [];
    end else [];  
    | OT_array_sel (at,sel) -> 
        let obj = at.at_objs.(sel) in
        let is_dyn = List.mem AT_dyn at.at_flags in
        if not is_dyn then obj_map (Sym_obj obj) modu pro 
        else []
    | OT_struct st -> [];
    | OT_var co -> [];  (* emitted just only one time with Sym_block *)
    | OT_named_value _
    | OT_const _ -> []; (* ignored *)
    | OT_object ao ->
    begin
      match (ao.ao_obj,pro) with
      | (Some (OT_queue qu),Some pro) ->
        obj_map (Sym_obj (OT_queue qu)) modu (Some pro) 
      | (Some (OT_channel ch),Some pro) ->
        obj_map (Sym_obj (OT_channel ch)) modu (Some pro) 
      | _ -> error 194984 "";
    end;
    | _ -> 
        error 994294 "";
  end;
  | Sym_block db ->
  begin
    begin
        match pro with
        | Some pro ->
            if (sym_check_sym pro.pro_import sym) then
            begin
                (*
                ** Process port
                *)
                (*
                ** Read and write access is handled with different
                ** signals! Read and Write access requires guards and write
                ** enable signals.
                *)
                let rd = ref false in 
                let wr = ref false in 
                List.iter (fun obj ->
                  match obj with
                  | OT_var co -> if List.mem pro co.co_reader then
                                  rd := true;
                                 if List.mem pro co.co_writer then
                                  wr := true;
                  | _ -> ();
                  ) db.db_objs;
                let ram_in = 
                  if !wr  then
                  [
                    sprintf "RAM_%s_WR => RAM_%s_%s_WR,"
                            db.db_name db.db_name pro.pro_name;
                    sprintf "RAM_%s_WE => RAM_%s_%s_WE,"
                            db.db_name db.db_name pro.pro_name;
                  ]
                  else [] in
                let ram_out = 
                  if !rd then
                  [
                    sprintf "RAM_%s_RD => RAM_%s_%s_RD,"
                            db.db_name db.db_name pro.pro_name;
                    sprintf "RAM_%s_RE => RAM_%s_%s_RE,"
                            db.db_name db.db_name pro.pro_name;
                  ]
                  else [] in
                let ram_addr = 
                  [
                    sprintf "RAM_%s_ADDR => RAM_%s_%s_ADDR,"
                            db.db_name db.db_name pro.pro_name;
                    sprintf "RAM_%s_GD => RAM_%s_%s_GD,"
                            db.db_name db.db_name pro.pro_name;
                  ] in
                  
                ram_in @ ram_out @ ram_addr
            end
            else
              error 676430 "";
        | None -> error 649129 "";
    end;
  end;
  | _ -> error 264217 ""

