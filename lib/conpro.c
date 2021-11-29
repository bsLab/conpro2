/*
******************************************
** CONPRO API layer
******************************************
*/

#include "conpro.h"

#define VERSION_CONPRO "1.2"

static char * parse_sign_and_base(char * p,
                                  /*out*/ int * base,
                                  /*out*/ int * sign)
{
  *sign = 1;
  if (*p == '-') {
    *sign = -1;
    p++;
  }
  *base = 10;
  if (*p == '0') {
    switch (p[1]) {
    case 'x': case 'X':
      *base = 16; p += 2; break;
    case 'o': case 'O':
      *base = 8; p += 2; break;
    case 'b': case 'B':
      *base = 2; p += 2; break;
    }
  }
  return p;
}

static int parse_digit(char * p)
{
  int c = *p;
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else
    return -1;
}

static long parse_long(char * p)
{
  unsigned long res;
  int sign, base, d;

  p = parse_sign_and_base(p, &base, &sign);
  d = parse_digit(p);
  if (d < 0 || d >= base) conpro_err("int_of_string");
  for (p++, res = d; /*nothing*/; p++) {
    d = parse_digit(p);
    if (d < 0 || d >= base) break;
    res = base * res + d;
  }
  if (*p != 0) conpro_err("int_of_string");
  return sign < 0 ? -((long) res) : (long) res;
}

int int_of_string(char  *s)
{
  return parse_long(s);
}

#define FORMAT_BUFFER_SIZE 32

static char * parse_format(char  *fmt,
                           char * suffix,
                           char format_string[],
                           char default_format_buffer[])
{
  int prec, lastletter;
  char * p;
  int len, len_suffix;

  /* Copy Caml format fmt to format_string,
     adding the suffix before the last letter of the format */
  len = strlen(fmt);
  len_suffix = strlen(suffix);
  if (len + len_suffix + 1 >= FORMAT_BUFFER_SIZE)
    conpro_err("format_int: format too long");
  memmove(format_string, fmt, len);
  p = format_string + len - 1;
  lastletter = *p;
  /* Compress two-letter formats, ignoring the [lnL] annotation */
  if (p[-1] == 'l' || p[-1] == 'n' || p[-1] == 'L') p--;
  memmove(p, suffix, len_suffix);  p += len_suffix;
  *p++ = lastletter;
  *p = 0;
  /* Determine space needed for result and allocate it dynamically if needed */
  prec = 22 + 5; /* 22 digits for 64-bit number in octal + 5 extra */
  for (p = fmt; *p != 0; p++) {
    if (*p >= '0' && *p <= '9') {
      prec = atoi(p) + 5;
      break;
    }
  }
  if (prec < FORMAT_BUFFER_SIZE)
    return default_format_buffer;
  else
  {
    p=(char *)malloc(prec + 1);
    mem_check(p,"parse_format");
    return p;
  }
}



void mem_check(void *p,const char *msg)
{
  static char _line[100];
  if (p==NULL)
  {  
   sprintf("%s: Out of memory",_line,msg);
   conpro_err(_line);
  };
  return;
}

int get_param(char* params,char *name)
{
  int v=0;
  char *p=params;
  char *n=name;
  char *e;
  char value[80];
  int found;
  while (*p != 0)
  {
    found=1;
    while (*n != 0 && *p != '=')
    {
      if (*n!=*p) {found=0;break;}
      else {n++;p++;};
    };
    if (found==1 && *n==0 && *p=='=')
    {
      e=value; p++;
      while (*p != 0 && *p != ':') 
      {
        *e=*p;
        e++;p++;
      }
      *e=0;
      return (int_of_string(value));
    }
    else
    {
      n=name;
      while (*p!=0 && *p != ':') p++;
      if (*p != 0) p++;
    };
  };
  return (-1);
}

int get_params(char* params,char *value,char *name)
{
  int v=0;
  char *p=params;
  char *n=name;
  char *e;
  int found;
  while (*p != 0)
  {
    found=1;
    while (*n != 0 && *p != '=')
    {
      if (*n!=*p) {found=0;break;}
      else {n++;p++;};
    };
    if (found==1 && *n==0 && *p=='=')
    {
      e=value; p++;
      while (*p != 0 && *p != ':') 
      {
        *e=*p;
        e++;p++;
      }
      *e=0;
      
      return 0;
    }
    else
    {
      n=name;
      while (*p!=0 && *p != ':') p++;
      if (*p != 0) p++;
    };
  };
  return (-1);
}

/*
******************************************
** Int64
******************************************
*/

#ifdef ARCH_INT64_TYPE

int64 int64_neg(int64 x)
{
  return (-x);
}

int64 int64_not(int64 x)
{
  return (!x);
}

int64 int64_add(int64 a,int64 b)
{
  return (a+b);
}

int64 int64_sub(int64 a,int64 b)
{
  return (a-b);
}

int64 int64_mul(int64 a,int64 b)
{
  return (a*b);
}

int64 int64_div(int64 a,int64 b)
{
  if (b==0) conpro_err ("Divisor is zero"); 
  return (a/b);
}

int64 int64_mod(int64 a,int64 b)
{
  if (b==0) conpro_err ("Divisor is zero"); 
  return (a%b);
}

int64 int64_and(int64 a,int64 b)
{
  return (a & b);
}

int64 int64_or(int64 a,int64 b)
{
  return (a | b);
}

int64 int64_xor(int64 a,int64 b)
{
  return (a ^ b);
}

int64 int64_shift_left(int64 a,int b)
{
  return (a << b);
}

int64 int64_shift_right(int64 a,int b)
{
  return (a >> b);
}

int64 int64_of_int(int x)
{
  return ((int64)x);
}

int int64_to_int(int64 x)
{
  return ((int)x);
}

char *int64_format(char* fmt, int64 arg)
{
  char format_string[FORMAT_BUFFER_SIZE];
  char default_format_buffer[FORMAT_BUFFER_SIZE];
  char * buffer;
  char *res;

  buffer = parse_format(fmt, ARCH_INT64_PRINTF_FORMAT,
                        format_string, default_format_buffer);
  sprintf(buffer, format_string, arg);
  res=(char *)malloc(strlen(buffer));
  mem_check(res,"int64_format");
  res = strcpy(res,buffer);
  if (buffer != default_format_buffer) free(buffer);
  return res;
}

char *int64_to_string (int64 x)
{
  return (int64_format("%d",x));
}

int64 int64_of_string(char *s)
{
  char * p;
  uint64 res;
  int sign, base, d;

  p = parse_sign_and_base(s, &base, &sign);
  d = parse_digit(p);
  if (d < 0 || d >= base) conpro_err("int_of_string");
  for (p++, res = d; /*nothing*/; p++) {
    d = parse_digit(p);
    if (d < 0 || d >= base) break;
    res = base * res + d;
  }
  if (*p != 0) conpro_err("int_of_string");
  return (sign < 0 ? -((int64) res) : (int64) res);
}

Int64_t Int64 = {
  (int64) 0,
  (int64)1,
  int64_neg,
  int64_add,
  int64_sub,
  int64_mul,
  int64_div,
  int64_mod,
  int64_and,  
  int64_not,  
  int64_or,  
  int64_xor,   
  int64_shift_left,   
  int64_shift_right, 
  int64_of_int,  
  int64_to_int, 
  int64_to_string,
  int64_of_string,
  int64_format, 
};

int64 bit64(int a,int b,int64 o)
{
  char m[80];
  int i;
  strcpy(m,"0b");
  m[b+1+2]=0;
  for(i=0;i<b+1;i++) m[i+2]='0';
  
  for(i=a;i<=b;i++) 
    m[b-i+2] = '1';
  return Int64.shift_right (Int64.logand(o,Int64.of_string(m)),a);
}

int64 bit64w(int a,int b,int64 o,int64 e)
{
  char m[80],m2[80];
  int64 d1,d2;
  int i;
  strcpy(m,"0b");
  strcpy(m2,"0b");
  m[b+1+2]=0;
  m2[b-a+1+2]=0;
  for(i=0;i<b+1;i++) m[i+2]='0';
  for(i=0;i<b-a+1;i++) m2[i+2]='1';
  
  for(i=a;i<=b;i++) 
    m[b-i+2] = '1';
  d1=Int64.of_string(m);
  d2=Int64.of_string(m2);
  return (Int64.logor (Int64.logand(o,Int64.lognot(m)),(Int64.shift_left (Int64.logand(e,m2),a))));
}

#endif /* ARCH_INT64_TYPE */


int bit(int a,int b,int o)
{
  char m[80];
  int i;
  strcpy(m,"0b");
  m[b+1+2]=0;
  for(i=0;i<b+1;i++) m[i+2]='0';
  
  for(i=a;i<=b;i++) 
    m[b-i+2] = '1';

  return ((o  & int_of_string(m)) >> a);
}

int bitw(int a,int b,int o,int e)
{
  char m[80],m2[80];
  int d1,d2;
  int i;
  strcpy(m,"0b");
  strcpy(m2,"0b");
  m[b+1+2]=0;
  m2[b-a+1+2]=0;
  for(i=0;i<b+1;i++) m[i+2]='0';
  for(i=0;i<b-a+1;i++) m2[i+2]='1';
  
  for(i=a;i<=b;i++) 
    m[b-i+2] = '1';
  d1=int_of_string(m);
  d2=int_of_string(m2);
  return ((o & (~d1)) | ((e & d2) << a));
}

int sign(int n,int e)
{
  if (bit((n-1),(n-1),e) == 1) 
  {
    int v = bit(0,(n-2),e);
    return -(bit(0,(n-2),(!(v-1))));
  }
  else
    return e;   
}

/*
** Return the current time as a floating-point number [sec]
*/

double timeofday(void)
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (double) tv.tv_sec + (double) tv.tv_usec * 1e-6;
}

void conpro_err(char *msg)
{
  printf("[CONPRO]: Error: %s\n",msg);
  fflush(stdout);
  exit(1);
}
