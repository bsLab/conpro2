#include "test_main.h"
void PRO_main()
{
  int i;
  LOCK_FUN_f->init(LOCK_FUN_f);
  mu->init(mu);
  sm->init(sm,1);
  f(3,4,&x);
  c1.real = 0;
  c1.imag = 1;
  for(i=0;i<=3;i=i+1){
    pa[i]->start(pa[i]);
  };
  for(i=1;i<=99;i=i+1){
    f(ca[i].real,i,&x);
    ca[i].real = ca[i].imag + i;
    ca[i].imag = ca[i].real + i;
  };
  p1->start(p1);
  p2->start(p2);
  for(i=1;i<=2;i=i+1){
    sm->down(sm);
  };
  process_end();
}
process_t* main;
