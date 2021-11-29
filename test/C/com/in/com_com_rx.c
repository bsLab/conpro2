#include "com_com_rx.h"
void PRO_com_rx()
{
  int d;
  bool err;
  int d2;
  d=0;
  err=false;
  d2=0;
  err = false;
  while(err == false){
    com->read(com,&d,&err);
    sys_status = STATUS_EV;
    if (err == false)
    {
      rx_q->write(rx_q, d);
    };
  };
  process_end();
}
process_t* com_rx;
