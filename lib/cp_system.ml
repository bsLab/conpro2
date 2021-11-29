(*
** ConPro ML implementation library
** SYSTEM
*)

class system env =
  object (self)
  val mutable clock = 10000000
  val mutable clock_level = 1
  val mutable reset_level = 1
  val mutable reset_internal = false
  val mutable simu_cycles = 200
  val mutable simu_res = 50
  val mutable target = ([]:string list)
  val mutable expr_type = "flat"
  
  initializer 
    List.iter (fun (e,v) ->
      match e with
      | "clock" -> clock <-  int_of_string v;
      | _ -> () ) env
      
  method clock v = clock <- v
  method clock_level l = clock_level <- l
  method reset_level l = reset_level <- l
  method reset_internal b = reset_internal <- b
  method simu_cycles n = simu_cycles <- n
  method simu_res n = simu_res <- n
  method target s = target <- target @ [s]
  method expr_type s = expr_type <- s
end 
