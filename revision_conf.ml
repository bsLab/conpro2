
(*
** Top directory and project name:
*)

let topdir = ref ("/home/sbosse/proj/conpro");;
let project = ref ("conpro");;


(*
** The revision dir (without leading slah relative to home path)
*)
let revisiondir = ref ("revision");;


(*
** Exclude the following components from the state list:
*)

let path_exclude = [
"*.o";
"*.a";
"*.cmo";
"*.cmi";
"*.cma";
"*.cmx";
"*.cmxa";
".Amake";
"conpro/build/ml/conpro";
];;

let (path_exclude_list: string list ref) = ref [];;


