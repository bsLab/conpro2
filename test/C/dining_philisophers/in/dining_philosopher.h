#ifndef __dining_philosopher
#define __dining_philosopher
#include "dining_types.h"
#include "dining_const.h"
#include "dining_models.h"
#include "dining_objs.h"
#include "dining_FUN_eat.h"
extern void PRO_philosopher(int process_id);
extern process_t* philosopher[];
#endif /* !__dining_philosopher */
