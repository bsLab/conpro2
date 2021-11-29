#include "test_FUN_f.h"
void f(int x, int y,int* z)
{
  LOCK_FUN_f->lock(LOCK_FUN_f);
  *z = x + y;
  LOCK_FUN_f->unlock(LOCK_FUN_f);
}
