type dev_type : {
  port link_tty_rx: input logic;
  port link_tty_tx: output logic;
};

component DEV: dev_type;
export DEV;
