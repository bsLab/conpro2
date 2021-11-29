#include <stdio.h>
#include <genpat.h>
#define PERIOD       100
#define RES          5
#define CLKDIV       (PERIOD/(2*RES))
#define NS(clk,n)    ((clk*2)*PERIOD*500+(n*PERIOD*500)/CLKDIV)
#define NS2(clk,n)   ((clk*2+1)*PERIOD*500+(n*PERIOD*500)/CLKDIV)
#define N            100
char *i2s(int v)
  {
    char *str;
    str=(char *) mbkalloc(32);
    sprintf(str,%d,v);
    return str;
  };
  
