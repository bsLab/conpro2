
#ifndef __CONPROH
#define __CONPROH
/*
******************************************
** CONPRO
******************************************
*/

#ifdef H_STDIO
  #include <stdio.h>
#endif
#ifdef H_ERRNO
  #include <errno.h>
#endif
#ifdef H_STDLIB
  #include <stdlib.h>
#endif
#ifdef H_UCONTEXT
  #include <ucontext.h>
#endif
#ifdef H_SYS_TIME
  #include <sys/time.h>
#endif
#ifdef H_SYS_TYPES
  #include <sys/types.h>
#endif
#ifdef H_SYS_STAT
  #include <sys/stat.h>
#endif
#ifdef H_FCNTL
  #include <fcntl.h>
#endif
#ifdef H_TERMIOS
  #include <termios.h>
#endif
#ifdef H_STRING
  #include <string.h>
#endif
#ifdef H_UNISTD
  #include <unistd.h>
#endif
#ifdef H_STDINT
  #include <stdint.h>
#endif

extern int get_param(char* params,char *name);
extern int get_params(char* params,char *value, char *name);
extern void conpro_err(char *msg);
extern void mem_check(void *p,const char *msg);

typedef int bool;
#ifndef false
#define false 0
#endif
#ifndef true
#define true 1
#endif

#define int_of_char(x)  (int)(x)
#define char_of_int(x)  (char)(x)

#ifndef MAX
#define MAX(a,b)  (a>b?a:b)
#endif
#ifndef MIN
#define MIN(a,b)  (a<b?a:b)
#endif

#define ARCH_INT64_TYPE int64_t
#define ARCH_UINT64_TYPE uint64_t
#define ARCH_INT64_PRINTF_FORMAT "ll"

#ifdef ARCH_INT64_TYPE
/*
******************************************
** Int64
******************************************
*/

typedef ARCH_INT64_TYPE int64;
typedef ARCH_UINT64_TYPE uint64;

struct Int64_s {
  int64 zero;
  int64 one;
  int64 (*neg) ();
  int64 (*add) ();
  int64 (*sub) ();
  int64 (*mul) ();
  int64 (*div) ();
  int64 (*mod) ();
  int64 (*logand) ();
  int64 (*logor) ();
  int64 (*logxor) ();
  int64 (*lognot) ();
  int64 (*shift_left) ();
  int64 (*shift_right) ();
  int64 (*of_int) ();
  int (*to_int) ();
  char* (*to_string) ();
  int64 (*of_string) ();
  char* (*format) ();
};

typedef struct Int64_s Int64_t; 
extern Int64_t Int64;
extern int64 bit64(int a,int b,int64 x);
extern int64 bitw64(int a,int b,int64 x, int64 e);

#endif /* ARCH_INT64_TYPE */

#if !defined (NULL)
  #define NULL ((void *)0)
#endif


extern void mem_check(void *p,const char *msg);
extern double timeofday(void);

/*
** Exception handler
**
**  try(id)
**  {
**
**      .. raise(E);
**  };
**  with(id)
**  {
**      case E: ... break;
**  };
**  with_end(id);  
**  
**
*/
#ifndef __SUNPRO_C
     
#define try(id) process_table[process_running].catcher=&&try_with_label##id;\
            process_table[process_running].catcher_end=&&try_with_label_end##id;
#define with(id)  goto *(process_table[process_running].catcher_end); \
              try_with_label##id: switch (process_table[process_running].exception)
#define with_end(id)  try_with_label_end##id: \
                  process_table[process_running].catcher=(void **)0;\
                  process_table[process_running].catcher_end=(void **)0;
#define raise(e) process_raise(e); goto *(process_table[process_running].catcher);

#else /*__SUNPRO_C */

#define try(id) process_table[process_running].catcher=&&try_with_label/**/id;\
            process_table[process_running].catcher_end=&&try_with_label_end/**/id;
#define with(id)  goto *(process_table[process_running].catcher_end); \
              try_with_label/**/id: switch (process_table[process_running].exception)
#define with_end(id)  try_with_label_end/**/id: \
                  process_table[process_running].catcher=(void **)0;\
                  process_table[process_running].catcher_end=(void **)0;
#define raise(e) process_raise(e); goto *(process_table[process_running].catcher);

#endif 


#define DBG(a,b)  {}

#ifndef X
  #define X(a,b) {printf("[%s=%d]\n",a,b);fflush(stdout);}
#endif
#ifndef Xn
  #define Xn(a,s,b) {printf("[%s[%d]=%d]\n",a,s,b);fflush(stdout);}
#endif
#endif /* __CONPROH */

