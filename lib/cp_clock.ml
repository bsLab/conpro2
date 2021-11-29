(*
** ConPro ML implementation library
** CLOCK
*)

class clock (env:(string*string) list) =
  object (self)
  val version = "1.1"
  val mutable source = ""
  val mutable action = 1
  val mutable clock = 10000000
  val mutable signals = []
  
  initializer 
    self#config env
    
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | _ -> () ) env
      
  method source x = 
    source <- x
    
  method action x = 
    action <- x
    
  method clock x = 
    clock <- x

  method add (x:int) =
    signals <- signals @ [x]
end

