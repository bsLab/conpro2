(*
** ConPro ML implementation library
** IOPORT
*)

class ioport env  =
  object (self)
  val version = "1.2"
  val mutable datawidth = 8
  
  initializer 
    self#config env
    
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | "datawidth" -> datawidth <- int_of_string v;
      | _ -> () ) env
      
  method init () = 
    ()

  method interface (port:int)  =
    ()

  method read () =
    0
  
  method write (d:int) = ()
  
  method dir (d:int) = ()
  
end

