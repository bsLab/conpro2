#include "com_sys_status_proc.h"
void PRO_sys_status_proc()
{
  status_types last_status;
  int counter;
  bool onl;
  last_status=STATUS_OK;
  counter=0;
  onl=false;
  stat_led = 0x0; X("stat_led",(int)stat_led);
  counter = 0;
  onl = false;
  watch_timer->init(watch_timer);
  watch_timer->start(watch_timer);
  last_status = STATUS_DOWN;
  sys_status_next = last_status;
  while(true){
    last_status = sys_status_next;
    switch (sys_status) {
      case STATUS_OK:
      {
        onl = true;
        counter = 0;
        last_status = STATUS_OK;
        break;
      };
      case STATUS_DOWN:
      {
        onl = false;
        counter = 0;
        last_status = STATUS_DOWN;
        break;
      };
      case STATUS_ERR:
      {
        if ((onl == false && counter == 0))
        {
          onl = true;
          counter = 2;
          last_status = STATUS_ERR;
        }
        else
        {
          if ((onl == true && counter == 0))
          {
            onl = false;
            counter = 2;
          }
          else
          {
            counter = counter - 1;
          };
        };
        break;
      };
      case STATUS_ACT:
      {
        if ((onl == false && counter == 0))
        {
          onl = true;
          counter = 6;
          last_status = STATUS_ACT;
        }
        else
        {
          if ((onl == true && counter == 0))
          {
            onl = false;
            counter = 6;
          }
          else
          {
            counter = counter - 1;
          };
        };
        break;
      };
      case STATUS_EV:
      {
        if (counter == 0)
        {
          onl = true;
          counter = 3;
        }
        else
        {
          if (counter == 1)
          {
            counter = 0;
            sys_status = last_status;
          }
          else
          {
            onl = onl;
            counter = counter - 1;
          };
        };
        break;
      };
      default: conpro_err("unmatched value");
    };
    sys_status_next = last_status;
    if (onl == true)
    {
      stat_led = 1; X("stat_led",(int)stat_led);
    }
    else
    {
      stat_led = 0; X("stat_led",(int)stat_led);
    };
    watch_timer->await(watch_timer);
  };
  process_end();
}
process_t* sys_status_proc;
