#include "app_cfg.h"

#ifndef USE_BUILDIN_STRING_H
void* memcpy(void *m0, const void *m1, uint32_t len)
{
	uint8_t *ret = m0;

	while(len--)
		*(uint8_t*)m0++ = *(uint8_t*)m1++;

	return ret;
}

int memcmp(const void *m0, const void *m1, uint32_t len)
{
	const uint8_t *u8_0 = (const uint8_t *)m0;
	const uint8_t *u8_1 = (const uint8_t *)m1;

	for (; len > 0; ++u8_0, ++u8_1, --len)
		if (*u8_0 != *u8_1)
			return (*u8_0 - *u8_1);

	return 0;
}

void *memset(void *m, int c, uint32_t len)
{
	const uint8_t u8_c = (uint8_t)c;
	u8 *ptr = (uint8_t *)m;

	for (; len > 0; len--)
		*ptr++ = u8_c;
	return m;
}

#endif
