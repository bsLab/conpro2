#ifndef __dining_objs
#define __dining_objs
#include "dining_types.h"
#include "dining_const.h"
#include "dining_models.h"
extern system_t* sys;
extern event_t* ev;
extern int* thinking;
extern char status;
extern semaphore_t* fork[];
extern int* eating;
#endif /* !__dining_objs */
