#include "app_cfg.h"

#ifndef USE_BUILDIN_STRING_H
void* memcpy(void *m0, const void *m1, u32 len)
{
  u8 *ret = m0;

  while(len--)
    *(u8*)m0++ = *(u8*)m1++;

  return ret;
}
#endif
