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
**    $CREATED:     26.9.2009
**    $VERSION:     2.04
**
**    $INFO:
**
**  Flexible License Manager Module
**
**    $ENDOFINFO
**
*)

open Crypto
open Cp_common
open Cp_version

open Printf

let hex s = transform_string (Hexa.decode()) s
let tohex s = transform_string (Hexa.encode()) s
let hash s = hash_string (Hash.md5()) s

let print_hostid () =
  out_ (sprintf "Hostid for this machine: %s" (system_hostid ()))
    
let license_check feature =
  let lp = sprintf "%s/license/license.dat" conpro_path in
  let hostid = system_hostid () in
  out (sprintf "Checking license file <%s> for host with hostid=<%s> and feature <%s>..." 
               lp hostid feature);

  let key hostid_mask =
    tohex (hash (sprintf "%s#%s#%s" feature major 
                         (Int64.format "%8.8x" (Int64.logand 
                                (Int64.of_string (sprintf "0x%s" hostid)) 
                                (Int64.of_string (sprintf "0x%s" hostid_mask)))))) in
  let ic = 
    try open_in lp with _ -> error 0 (sprintf "No license file <%s> found." lp) in
  let eof = ref false in
  let checked = ref false in
  let feature_found = ref false in
  while (not !eof) 
  do
    let line = ref "" in
    eof := not(protects(line := input_line ic));
    if not !eof then
    begin
      debug "license_check" with !line;
      if (String.length !line)>1 && !line.[0] <> '#' then
      begin
        let tokens = Str.split (Str.regexp " ") !line in
        match List.filter (fun token -> token <> "") tokens with
        | ["FEATURE";feature;"conpro";major;"permanent";"\\"]  
        | ["FEATURE";feature;"conpro";major;"permanent\\"] -> 
          feature_found := true;
          debug "license_check" with "feature found";
        | ["HOSTID";hostid_mask;"KEY";key'] ->
          let key = key hostid_mask in
          checked := key=key';
          debug "license_check" with (sprintf "checked=%b hostid=%s hostid_mask=%s key=%s key'=%s"
                                           !checked hostid hostid_mask key key');
          if !checked then eof := true else eof := false;
        | _ -> ();
      end;
    end;
  done;
  if not !feature_found then warning (sprintf "License feature <%s> not found in license file <%s>."
                                              feature lp);
  if not !checked then warning (sprintf "No valid license found for feature <%s> on host <%s>."
                                        feature hostid);
  !checked
  
let license_gen feature hostid file nocheck =
  if nocheck or (license_check "FLEXGEN") then
  begin
    let oc =
      try open_out_gen [Open_append;Open_wronly;Open_creat;Open_text] 0o666 file with _ -> error 0 (sprintf "Can't open license file <%s>." file) in
    let version = Str.global_replace (Str.regexp "--") "#" vhdl_version in
    output_string oc version;
    let key = tohex (hash (sprintf "%s#%s#%s" feature major hostid)) in
    List.iter (fun str -> output_string oc (str^"\n")) [
      sprintf "FEATURE %s conpro %s permanent \\" feature major;
      sprintf "        HOSTID %s KEY %s" hostid key;   
    ];
    out (sprintf "Addeded feature <%s> to license file <%s> for host with hostid=<%s>." feature file hostid);
  end
  else
    error 0 ("Access not authorized! Check license file and FLEXGEN feature.") 
