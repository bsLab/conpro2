#include "com_FUN_request.h"
void request(int d1,int* d2)
{
  LOCK_FUN_request->lock(LOCK_FUN_request);
  tx_q->write(tx_q, int_of_char(REQ));
  tx_q->write(tx_q, d1);
  rep->await(rep);
  *d2 = rep_d;
  LOCK_FUN_request->unlock(LOCK_FUN_request);
}
