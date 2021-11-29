#include "com_main.h"
extern event_t *_exit;
void PRO_Main()
{
  int d,i;
  d=0;
  /* sys_status_proc->start(sys_status_proc); */
  interpreter->start(interpreter);
  for(i='a';i<='z';i++)
  {
    X("request",i);
    request(i,&d);
    X("reply",d);
  };
  _exit->wakeup(_exit); 
  process_end();
}
process_t* Main;
