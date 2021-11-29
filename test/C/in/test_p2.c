#include "test_p2.h"
void PRO_p2()
{
  int TEMPS_0;
  int i;
  TEMPS_0=0;
  for(i=1;i<=10;i=i+1){
    mu->lock(mu);
    x = x - 1;
    TEMPS_0 = a[i];
    a[i] = TEMPS_0 - 1;
    mu->unlock(mu);
  };
  p1->stop(p1);
  sm->up(sm);
  process_end();
}
process_t* p2;
