#include "app_cfg.h"

#ifndef USE_BUILDIN_STRING_H
void* memcpy(void *m0, const void *m1, u32 len)
{
	u8 *ret = m0;

	while(len--)
		*(u8*)m0++ = *(u8*)m1++;

	return ret;
}

int memcmp(const void *m0, const void *m1, u32 len)
{
	const u8 *u8_0 = (const uint8_t *)m0;
	const u8 *u8_1 = (const uint8_t *)m1;

	for (; len > 0; ++u8_0, ++u8_1, --len)
		if (*u8_0 != *u8_1)
			return (*u8_0 - *u8_1);

	return 0;
}

void *memset(void *m, int c, u32 len)
{
	const u8 u8_c = (u8)c;
	u8 *ptr = (u8 *)m;

	for (; len > 0; len--)
		*ptr++ = u8_c;
	return m;
}

#endif
