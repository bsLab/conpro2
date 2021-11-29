#ifndef __test_objs
#define __test_objs
#include "conpro.h"
#include "test_types.h"
#include "test_const.h"
#include "test_models.h"
#include "mutex.h"
#include "semaphore.h"
#include "process.h"
extern int x;
extern struct complex c1;
extern struct complex c2;
extern mutex_t* LOCK_FUN_f;
extern struct complex* ca;
extern mutex_t* mu;
extern semaphore_t* sm;
extern int* a;
#endif /* !__test_objs */
