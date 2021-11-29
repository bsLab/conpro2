open Core;
open Event;
open Process;

include "agent_node";

type agent_a_data : {
  dx0: int[8];
  dy0: int[8];
  len: int[8];
  data: int[32];
  dx: int[8];
  dy: int[8];
  gamma_x: int[2];
  gamma_y: int[2];
  dir : int[4];
  processed: bool;
};

array agents_a_data: var[TABLE_SIZE2] of agent_a_data;
queue transition_1,
      transition_2,
      transition_3,
      transition_4,
      transition_5,
      transition_6,
      transition_7
      : int[8] with depth=8;

include "agent_link";
include "agent_manager";


-- S1

process a_start:
begin
  reg t:int[8];
  always do
  begin
    t <- transition_1;
    agents_a_data.[t].dx <- agents_a_data.[t].dx0;
    agents_a_data.[t].dy <- agents_a_data.[t].dy0;
    agents_a_data.[t].gamma_x <- 0;
    agents_a_data.[t].gamma_y <- 0;
    agents_a_data.[t].dir <- 0;
    agents_a_data.[t].processed <- false;
    transition_2 <- t;
  end;    
end;


-- S2
process a_deliver:
begin
  reg t:int[8];
  always do
  begin
    t <- transition_2;
    if agents_a_data.[t].dx = 0 and
       agents_a_data.[t].dy = 0 then
    begin
      agents_a_data.[t].processed <- true;    
    end
    else
      agents_a_data.[t].processed <- false;
    if agents_a_data.[t].processed = true then
      transition_6 <- t
    else
      transition_3 <- t;
  end;  
end;

-- S3
process a_route_normal:
begin
  reg t:int[8];
  reg reversed: bool;
  
  always do
  begin
    t <- transition_3;
    for i = 1 to 2 do
    begin
      if agents_a_data.[t].processed = false then match i with
      begin
        when 1:
        begin
          reversed <- agents_a_data.[t].gamma_x <> 0 and
                      agents_a_data.[t].dx <> agents_a_data.[t].dx0;

          if agents_a_data.[t].dx < 0 and
             agents_a_data.[t].dir <> WEST and
             reversed = false and 
             linkQ(EAST) = true then
          begin
            agents_a_data.[t].dx <- agents_a_data.[t].dx + 1;
            agents_a_data.[t].dir <- EAST;
            agents_a_data.[t].gamma_y <- 0;         
            agents_a_data.[t].processed <- true;
          end
          else if agents_a_data.[t].dx > 0 and
                  agents_a_data.[t].dir <> EAST and
                  agents_a_data.[t].gamma_x = 0 and 
                  linkQ(WEST) = true then
          begin
            agents_a_data.[t].dx <- agents_a_data.[t].dx - 1;
            agents_a_data.[t].dir <- WEST;
            agents_a_data.[t].gamma_y <- 0;         
            agents_a_data.[t].processed <- true;
          end;        
        end;
        when 2:
        begin
          reversed <- agents_a_data.[t].gamma_y <> 0 and
                      agents_a_data.[t].dy <> agents_a_data.[t].dy0;

          if agents_a_data.[t].dy < 0 and
             agents_a_data.[t].dir <> SOUTH and
             reversed = false and 
             linkQ(NORTH) = true then
          begin
            agents_a_data.[t].dy <- agents_a_data.[t].dy + 1;
            agents_a_data.[t].dir <- NORTH;
            agents_a_data.[t].gamma_x <- 0;         
            agents_a_data.[t].processed <- true;
          end
          else if agents_a_data.[t].dy > 0 and
                  agents_a_data.[t].dir <> NORTH and
                  agents_a_data.[t].gamma_y = 0 and 
                  linkQ(SOUTH) = true then
          begin
            agents_a_data.[t].dy <- agents_a_data.[t].dy - 1;
            agents_a_data.[t].dir <- SOUTH;
            agents_a_data.[t].gamma_x <- 0;         
            agents_a_data.[t].processed <- true;
          end;        
        end;
      end; 
    end;
    if agents_a_data.[t].processed = true then
      transition_7 <- t
    else
      transition_4 <- t;
  end;
end;

-- S4 
process a_route_opposite:
begin
  reg t:int[8];

  always do
  begin
    t <- transition_4;
    for i = 1 to 2 do
    begin
      if agents_a_data.[t].processed = false then match i with
      begin
        when 1:
        begin       
          if agents_a_data.[t].gamma_x = 0 then
          begin
	        if  agents_a_data.[t].dir <> EAST  and linkQ(EAST) = true then
            begin
             agents_a_data.[t].dx <- agents_a_data.[t].dx +1;  
             agents_a_data.[t].dir <- EAST; 
             agents_a_data.[t].gamma_x <- -1;
             agents_a_data.[t].processed <- true;
            end
            else if agents_a_data.[t].dir <> WEST and linkQ(WEST) = true then
            begin
             agents_a_data.[t].dx <- agents_a_data.[t].dx - 1; 
             agents_a_data.[t].dir <- WEST; 
             agents_a_data.[t].gamma_x <- 1;
             agents_a_data.[t].processed <- true; 
            end; 
          end;
        end;
        when 2:
        begin
          if agents_a_data.[t].gamma_y = 0 then
          begin
	        if  agents_a_data.[t].dir <> NORTH  and linkQ(NORTH) = true then
            begin
             agents_a_data.[t].dy <- agents_a_data.[t].dy +1;  
             agents_a_data.[t].dir <- NORTH; 
             agents_a_data.[t].gamma_y <- -1;
             agents_a_data.[t].processed <- true;
            end
            else if agents_a_data.[t].dir <> SOUTH and linkQ(SOUTH) = true then
            begin
             agents_a_data.[t].dx <- agents_a_data.[t].dy - 1; 
             agents_a_data.[t].dir <- SOUTH; 
             agents_a_data.[t].gamma_y <- 1;
             agents_a_data.[t].processed <- true; 
            end; 
          end;        
        end;
      end;
    end;
    if agents_a_data.[t].processed = true then
      transition_7 <- t
    else
      transition_5 <- t;
  end;
end;

-- S5
process a_route_backward:
begin
  reg t:int[8];
  
  always do
  begin
    t <- transition_5;
    for i = 1 to 2 do
    begin
      if agents_a_data.[t].processed = false then match i with
      begin
        when 1:
        begin       
          if agents_a_data.[t].gamma_x = 0 then
          begin
	        if  (agents_a_data.[t].dir = EAST or agents_a_data.[t].dir = NORTH) 
                    and linkQ(EAST) = true then
            begin
                agents_a_data.[t].dx <- agents_a_data.[t].dx +1;  
                agents_a_data.[t].dir <- EAST; 
                agents_a_data.[t].gamma_x <- -1;
                agents_a_data.[t].processed <- true;
            end
            else if linkQ(WEST) = true then
            begin
                agents_a_data.[t].dx <- agents_a_data.[t].dx - 1; 
                agents_a_data.[t].dir <- WEST; 
                agents_a_data.[t].gamma_x <- 1;
                agents_a_data.[t].processed <- true;
            end; 
          end
          else
          begin
            if agents_a_data.[t].gamma_x = (-1) and linkQ(EAST) = true then
            begin
                agents_a_data.[t].dx <- agents_a_data.[t].dx +1;  
                agents_a_data.[t].dir <- EAST; 
                agents_a_data.[t].processed <- true;
            end
            else if agents_a_data.[t].gamma_x = 1 and linkQ(WEST) = true then 
            begin
                agents_a_data.[t].dx <- agents_a_data.[t].dx - 1; 
                agents_a_data.[t].dir <- WEST;
                agents_a_data.[t].processed <- true;
            end;
          end;        
        end;
        when 2:
        begin
          if agents_a_data.[t].gamma_y = 0 then
          begin
	        if agents_a_data.[t].dir = NORTH 
               and linkQ(NORTH) = true then
            begin
                agents_a_data.[t].dy <- agents_a_data.[t].dy + 1;  
                agents_a_data.[t].dir <- NORTH; 
                agents_a_data.[t].gamma_y <- -1;
                agents_a_data.[t].processed <- true;
            end
            else if linkQ(SOUTH) = true then
            begin
                agents_a_data.[t].dy <- agents_a_data.[t].dy - 1; 
                agents_a_data.[t].dir <- SOUTH; 
                agents_a_data.[t].gamma_y <- 1;
                agents_a_data.[t].processed <- true;
            end; 
          end
          else
          begin
            if agents_a_data.[t].gamma_y = (-1) and linkQ(NORTH) = true then
            begin
                agents_a_data.[t].dy <- agents_a_data.[t].dy + 1;  
                agents_a_data.[t].dir <- NORTH; 
                agents_a_data.[t].processed <- true;
            end
            else if agents_a_data.[t].gamma_y = 1 and linkQ(SOUTH) = true then 
            begin
                agents_a_data.[t].dy <- agents_a_data.[t].dy - 1; 
                agents_a_data.[t].dir <- SOUTH;
                agents_a_data.[t].processed <- true;
            end;
          end;        
        end;
      end;
    end;
    if agents_a_data.[t].processed = true then
      transition_7 <- t
    else
      transition_6 <- t;
  end;
end;

-- S6
process a_discard:
begin
  reg t:int[8];
  always do
  begin
    t <- transition_6;
    kill(t);
  end;     
end;

-- S7
process a_migrate:
begin
  reg t:int[8];
  always do
  begin
    t <- transition_7;
    migrate(t,agents_a_data.[t].dir,2);
  end;     
end;

process main:
begin
  reg t:int[8];
  try
  begin
    link_init();
    agent_init();
    a_start.start ();
    a_deliver.start ();
    a_route_normal.start ();
    a_route_opposite.start ();
    a_route_backward.start ();
    a_discard.start ();
    a_migrate.start ();

    for i = 1 to 10 do
    begin
      t <- new('A');
      agents_a_data.[t].dx0 <- i;
      agents_a_data.[t].dy0 <- i;
      agents_a_data.[t].len <- 4;
      agents_a_data.[t].data <- i;
      transition_1 <- t;
    end;
  end
  with
  begin
    when Agent_error: t<-0;
  end;
end;
