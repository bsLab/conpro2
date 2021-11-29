#include "dining_objs.h"
system_t* sys;
event_t* ev;
int* thinking;
char status;
semaphore_t* fork[5];
int* eating;
