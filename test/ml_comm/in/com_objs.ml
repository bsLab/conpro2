open Com_types
open Com_const
open Com_models
let rnd = new random ["datawidth","8"]
let dEV = {
  dev_port_data_in=Int64.zero;
  dev_port_data_in_ack=0;
  dev_port_data_out=Int64.zero;
  dev_port_data_out_ack=0;
  dev_port_data=0;
  dev_port_data_en=0;
}
let ln = new link ["datawidth","16"]
