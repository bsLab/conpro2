open Uart;

type message : {
  MSG_REQ;
  MSG_REP;
  MSG_ALV;
  MSG_ACK;
};

type node_links : {
  port links_rx: input logic[NUM_LINKS];
  port links_tx: output logic[NUM_LINKS];
};

component DEVLN: node_links;
export DEVLN;

--
-- links [0,1,2,3]
-- eq. to directions [SOUTH,WEST,EAST,NORTH]
--
array links_alive: reg[NUM_LINKS] of int[4];
array links_act: reg[NUM_LINKS] of bool;
array rx_queue,tx_queue: queue[NUM_LINKS] of logic[8] with depth=8;
array links_com: object uart[NUM_LINKS];

for i = 1 to NUM_LINKS do
begin
  links_com.[i-1].interface(DEVLN.links_rx[i-1],DEVLN.links_tx[i-1]);
  links_com.[i-1].baud(115200);
end;

array link_rx: process[NUM_LINKS] of
begin
  reg d: logic[8];
  reg err: bool;
  
  always do 
  begin
    links_com.[#].read (d,err);
    if err = false then rx_queue.[#] <- d;      
  end;  
end;

array link_tx: process[NUM_LINKS] of
begin
  reg d: logic[8];
  reg err: bool;
  err <- false; 
  while err = false do
  begin
    d <- tx_queue.[#];
    links_com.[#].write (d,err);
  end;  
end;

function link_init():
begin
  for i = 0 to NUM_LINKS-1 do
  begin
    links_com.[i].init();
    links_com.[i].start ();
    link_rx.[i].start();
    link_tx.[i].start();
  end;
end;
