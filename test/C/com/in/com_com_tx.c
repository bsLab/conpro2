#include "com_com_tx.h"
void PRO_com_tx()
{
  int d;
  bool err;
  d=0;
  err=false;
  err = false;
  while(err == false){
    d = tx_q->read(tx_q);
    com->write(com,d,&err);
  };
  process_end();
}
process_t* com_tx;
