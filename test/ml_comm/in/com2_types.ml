type process_state = PROC_init | PROC_start | PROC_run | PROC_end
type dev_port = {
  mutable dev_port_data_in: int;
  mutable dev_port_data_out: int;
  mutable dev_port_data: int;
  mutable dev_port_data_en: int;
}
exception RECV_err
exception SEND_err
