/*
******************************************
** CLOCK
******************************************
*/

#include "clock.h"
#define VERSION_CLOCK "1.1"

static int clock_clock(struct cp_clock *,int d);
static int clock_action(struct cp_clock *,int d);
static int clock_add(struct cp_clock *,int *d);
static int cp_clockource(struct cp_clock *,char *str);
 
cp_clock_t *clock_new(char *params)
{
  int i;
  cp_clock_t *clock;

  clock=(cp_clock_t *)malloc(sizeof(cp_clock_t));
  mem_check((void*)clock,"clock_new");

  clock->Clock=10000000;
  
  clock->clock=clock_clock;
  clock->add=clock_add;
  clock->action=clock_action;
  clock->source=cp_clockource;
  return clock;
}

static int clock_clock(struct cp_clock *s,int d)
{
  s->Clock=d;
  return 0;  
}
static int clock_action(struct cp_clock *s,int d)
{
  return 0;  
}
static int cp_clockource(struct cp_clock *s,char *str)
{
  return 0;  
}
static int clock_add(struct cp_clock *s,int *d)
{
  return 0;  
}
