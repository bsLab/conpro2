open Com2_types
open Com2_const
open Com2_models
let rnd = new random ["datawidth","8"]
let dEV = {
  dev_port_data_in=0;
  dev_port_data_out=0;
  dev_port_data=0;
  dev_port_data_en=0;
}
let ln = new uart ["dev","/tmp/link"]
