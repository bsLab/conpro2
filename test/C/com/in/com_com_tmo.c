#include "com_com_tmo.h"
void PRO_com_tmo()
{
  com_timeout = false;
  process_delay(10000);
  rx_q->unlock(rx_q);
  com_timeout = true;
  process_end();
}
process_t* com_tmo;
