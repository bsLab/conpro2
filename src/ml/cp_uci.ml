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
**    $INITIAL:     (C) 2008 BSSLAB
**    $CREATED:     5.11.2008
**    $VERSION:     2.02
**
**    $INFO:
**
**  Micro Interface
**
**    $ENDOFINFO
**
*)


open Cp_version
open Cp_common
open Cp_symbol
open Cp_types
open Cp_syntax
open Cp_utils
open Cp_analysis
open Cp_Core
open Cp_synthesis
open Cp_resource
open Printf
open Cp_uci_analysis
open Cp_uci_types
open Cp_uci_in
open Cp_uci_out

(*
** Open and import an external process ..
*)
let import_pro pname =
  let pname = of_mod pname in
  out (sprintf "Opening process <%s>..." pname);
  let old_pos = an.a_curpos in 
  an.a_curpos <- nilsrc ();  
  ind_incr ();
  let tops,lmap = syntax_of_file (sprintf "%s.UC" pname) in
  an.a_modu.mod_fmap <- an.a_modu.mod_fmap @ lmap;
  
  let proo = new uci_in pname tops lmap in
  ind_decr ();
  an.a_curpos <- old_pos;
  proo#get_pro
