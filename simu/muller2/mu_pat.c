
#include <stdio.h>
#include <genpat.h>

/* Clock period in nano seconds with oversampling */
#define PERIOD      100
#define RES         1
#define CLKDIV      (PERIOD/(2*RES))   
#define NS(clk,n)    ((clk*2)*PERIOD*500+(n*PERIOD*500)/CLKDIV)
#define NS2(clk,n)    ((clk*2+1)*PERIOD*500+(n*PERIOD*500)/CLKDIV)
/* Number of clock cycles */
#define N   50

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
  DEF_GENPAT("mu");
  /* Port declarations */
  DECLAR("top_d_qe_0",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qd_0",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qe_1",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qd_1",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qe_2",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qd_2",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qe_3",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qd_3",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qe_4",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qd_4",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qe_5",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qd_5",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qe_6",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qd_6",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qe_7",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_qd_7",":2","X",OUT,"1 downto 0","");
  DECLAR("top_d_load",":2","B",OUT,"","");
  DECLAR("top_d_shift",":2","B",OUT,"","");
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
