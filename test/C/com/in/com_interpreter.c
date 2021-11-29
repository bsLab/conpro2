#include "com_interpreter.h"
void PRO_interpreter()
{
  int d;
  d=0;
  com->init(com);
  com->start(com);
  com_rx->start(com_rx);
  com_tx->start(com_tx);
  sys_status = STATUS_OK;
  while(true){
    d = rx_q->read(rx_q);
    sys_status_next = STATUS_OK;
    switch (d) {
      case int_of_char(REQ):
      {
        com_tmo->start(com_tmo);
        d = rx_q->read(rx_q);
        com_tmo->stop(com_tmo);
        if (com_timeout == false)
        {
          tx_q->write(tx_q, int_of_char(REP));
          tx_q->write(tx_q, d);
        }
        else
        {
          sys_status = STATUS_ERR;
        };
        break;
      };
      case int_of_char(REP):
      {
        com_tmo->start(com_tmo);
        d = rx_q->read(rx_q);
        com_tmo->stop(com_tmo);
        if (com_timeout == false)
        {
          rep_d = d;
          rep->wakeup(rep);
        }
        else
        {
          sys_status = STATUS_ERR;
        };
        break;
      };
      default:
      {
        sys_status = STATUS_ERR;
        break;
      };
    };
  };
  process_end();
}
process_t* interpreter;
