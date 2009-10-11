#include "app_cfg.h"

#if 1
void* memcpy(void *m0, const void *m1, u32 len)
{
  u8 *ret = m0;

  while(len--)
    *(u8*)m0++ = *(u8*)m1++;

  return ret;
}

u8 strcmp(const char *s0,const char *s1)
{
  for (;*s0 == *s1;++s0,++s1)
    if (*s0 == '\0')
      return 0;

  return (*(unsigned char *)s0 < *(unsigned char *)s1 ? -1 : +1);
}

u32 strlen(const char *s)
{
  const char *orig;

  for(orig = s;*orig;++orig);

  return (orig - s);
}
#endif
