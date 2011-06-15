#include "app_cfg.h"

#ifndef USE_BUILDIN_STRING_H
void* memcpy(void *m0, const void *m1, int len)
{
	char *ret = m0;

	while(len--)
		*(char*)m0++ = *(char*)m1++;

	return ret;
}

int memcmp(const void *m0, const void *m1, int len)
{
	const char *u8_0 = (const char *)m0;
	const char *u8_1 = (const char *)m1;

	for (; len > 0; ++u8_0, ++u8_1, --len)
		if (*u8_0 != *u8_1)
			return (*u8_0 - *u8_1);

	return 0;
}

void *memset(void *m, int c, int len)
{
	const char u8_c = (char)c;
	char *ptr = (char *)m;

	for (; len > 0; len--)
		*ptr++ = u8_c;
	return m;
}

#endif
