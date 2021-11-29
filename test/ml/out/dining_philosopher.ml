open Dining_types
open Dining_const
open Dining_models
open Dining_objs
open Dining_FUN_eat

let pro_philosopher process_id =
  if process_id < 4 then
  begin
    ev#await();
    while true do
      fork.(process_id)#down();
      fork.(process_id + 1)#down();
      eat(process_id);
      fork.(process_id)#up();
      fork.(process_id + 1)#up();
    done;
  end
  else
  begin
    while true do
      fork.(4)#down();
      fork.(0)#down();
      eat(process_id);
      fork.(4)#up();
      fork.(0)#up();
    done;
  end;
  process_end()

let philosopher_0 = new process ["name","philosopher";"#","0"] pro_philosopher
let philosopher_1 = new process ["name","philosopher";"#","1"] pro_philosopher
let philosopher_2 = new process ["name","philosopher";"#","2"] pro_philosopher
let philosopher_3 = new process ["name","philosopher";"#","3"] pro_philosopher
let philosopher_4 = new process ["name","philosopher";"#","4"] pro_philosopher
let philosopher = [|philosopher_0; philosopher_1; philosopher_2; philosopher_3; philosopher_4|]
