(*
** ConPro ML implementation library
** RANDOM
*)

class random1 (env) =
  object (self)
  val version = "1.4"
  val mutable seed_v = 0
  val mutable datawidth = 8
  val mutable datatype = "int"
  method selector = datawidth <= 16;
  
  initializer 
    self#config env
    
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | "seed" -> seed_v <- int_of_string v;
      | "datawidth" -> datawidth <- int_of_string v;
      | "datatype" -> datatype <- v;
      | _ -> () ) env
      
  method init () = 
    Random.init seed_v
  method seed (v) = 
    seed_v <- v;
    Random.init seed_v
  method read () =
    match datawidth with
    | 8 -> 128 - (Random.int 255);    
    | 10 -> 256 - (Random.int 511); 
    | 12 -> 1024 - (Random.int 2047);     
    | 14 -> 4096 - (Random.int 8191);
    | 16 -> 32768 - (Random.int 65535);
    | _ -> 0
end

class random2 (env) =
  object (self)
  val version = "1.4"
  val mutable seed_v = 0
  val mutable datawidth = 20
  val mutable datatype = "int"
  method selector = datawidth > 16;
  
  initializer 
    self#config env
    
  method config env =
    List.iter (fun (e,v) ->
      match e with
      | "seed" -> seed_v <- int_of_string v;
      | "datawidth" -> datawidth <- int_of_string v;
      | "datatype" -> datatype <- v;
      | _ -> () ) env
      
  method init () = 
    Random.init seed_v
  method seed (v) = 
    seed_v <- v;
    Random.init seed_v
  method read () =
    match datawidth with
    | 20 -> (Random.bits ()) land 0xfffff;    
    | 24 -> (Random.bits ()) land 0xffffff;
    | 32 -> (Random.bits ());
    | _ -> 0
end 


