
#include <stdio.h>
#include <genpat.h>

/* Clock period in nano seconds with oversampling */
#define PERIOD      100
#define RES         5
#define CLKDIV      (PERIOD/(2*RES))   
#define NS(clk,n)    ((clk*2)*PERIOD*500+(n*PERIOD*500)/CLKDIV)
#define NS2(clk,n)    ((clk*2+1)*PERIOD*500+(n*PERIOD*500)/CLKDIV)
/* Number of clock cycles */
#define N   1000

char *i2s(int v)
{
  char *str;
  str=(char *) mbkalloc(32);
  sprintf(str,"%d",v);
  return str;
};

int main(int argc,char **argv)
{
  int clk;
  int i,j;

  /* Name of pattern file */
  DEF_GENPAT("t");
  /* Port declarations */
  DECLAR("x",":2","X",OUT,"9 downto 0","");
  DECLAR("y",":2","X",OUT,"9 downto 0","");
  DECLAR("clk",":2","B",IN,"","");
  DECLAR("reset",":2","B",IN,"","");
  for(clk=0;clk<N*2;clk++){
    for(j=0;j<CLKDIV;j++){
      AFFECT(i2s(NS(clk,j)),"clk",i2s(1));
      switch (clk) {
        case 0:
          AFFECT(i2s(NS(clk,j)),"reset",i2s(1));
          break;
      }
    };
    for(j=0;j<CLKDIV;j++){
      AFFECT(i2s(NS2(clk,j)),"clk",i2s(0));
      switch (clk) {
        case 2:
          AFFECT(i2s(NS2(clk,j)),"reset",i2s(0));
          break;
      };
    };
  };
  SAV_GENPAT();
  return 0;
};
