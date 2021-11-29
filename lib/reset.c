/*
******************************************
** RESET
******************************************
*/

#include "reset.h"
#define VERSION_RESET "1.1"

static int reset_reset(struct reset *,int d);
static int reset_action(struct reset *,int d);
static int reset_add(struct reset *,int *d);
static int reset_source(struct reset *,char *str);
 
reset_t *reset_new(char *params)
{
  int i;
  reset_t *reset;

  reset=(reset_t *)malloc(sizeof(reset_t));
  mem_check((void*)reset,"reset_new");

  reset->Action=1;
  
  reset->add=reset_add;
  reset->action=reset_action;
  reset->source=reset_source;
  return reset;
}

static int reset_action(struct reset *s,int d)
{
  s->Action=d;
  return 0;  
}
static int reset_source(struct reset *s,char *str)
{
  return 0;  
}
static int reset_add(struct reset *s,int *d)
{
  return 0;  
}
