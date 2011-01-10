#ifdef USE_BUILDIN_STRING_H
#include <string.h>
#else
extern void* memcpy(void *m0, const void *m1, u32 len);
extern int memcmp(const void *m0, const void *m1, u32 len);
extern void *memset(void *m, int c, u32 len);
#endif
