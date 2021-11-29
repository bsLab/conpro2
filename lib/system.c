/*
******************************************
** SYSTEM
******************************************
*/

#include "system.h"
#define VERSION_SYSTEM "1.1"

static int sys_clock(struct system *,int d);
static int sys_target(struct system *,char *str);
static int sys_reset_level(struct system *,int d);
static int sys_simu_cycles(struct system *,int d);
 
system_t *system_new(char *params)
{
  int i;
  system_t *sys;

  sys=(system_t *)malloc(sizeof(system_t));
  mem_check((void*)sys,"system_new");

    i=get_param(params,"clock");
  if (i != -1) sys->Clock=i; else sys->Clock=10000000;
  
  sys->clock=sys_clock;
  sys->target=sys_target;
  sys->reset_level=sys_reset_level;
  sys->simu_cycles=sys_simu_cycles;
  return sys;
}

static int sys_clock(struct system *s,int d)
{
  s->Clock=d;
  return 0;  
}
static int sys_reset_level(struct system *s,int d)
{
  return 0;  
}
static int sys_target(struct system *s,char *str)
{
  return 0;  
}
static int sys_simu_cycles(struct system *s,int d)
{
  return 0;  
}
