#ifdef USE_BUILDIN_STRING_H
#include <string.h>
#else
extern void* memcpy(void *m0, const void *m1, uint32_t len);
extern int memcmp(const void *m0, const void *m1, uint32_t len);
extern void *memset(void *m, int c, uint32_t len);
#endif
