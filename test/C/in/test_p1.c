#include "test_p1.h"
void PRO_p1()
{
  int TEMPS_0;
  int i;
  TEMPS_0=0;
  for(i=1;i<=10;i=i+1){
    mu->lock(mu);
    x = x + 1;
    TEMPS_0 = a[i];
    a[i] = TEMPS_0 + 1;
    mu->unlock(mu);
  };
  sm->up(sm);
  process_end();
}
process_t* p1;
