#include "test_pa.h"
void PRO_pa(int process_id)
{
  int TEMPS_0;
  int i;
  TEMPS_0=0;
  for(i=1;i<=10;i=i+1){
    mu->lock(mu);
    x = x + 1;
    TEMPS_0 = a[process_id];
    a[i] = TEMPS_0 + process_id;
    mu->unlock(mu);
  };
  process_end();
}
process_t *pa[4];
