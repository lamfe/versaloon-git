#if 0
#include <string.h>
#else
extern void* memcpy(void *m0, const void *m1, u32 len);
extern u8 strcmp(const char *s0,const char *s1);
extern u32 strlen(const char *s);
#endif
