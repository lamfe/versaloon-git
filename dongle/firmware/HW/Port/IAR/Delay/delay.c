#include "app_cfg.h"
#include "delay.h"

void DelayUS(volatile uint32 dly)
{
	dly = (dly << 3);
	while(dly--);
}

void DelayMS(uint32 dly)
{
	DelayUS(1000 * dly);
}

void DelayUSMS(uint16 dly)
{
	if(dly & 0x8000)
	{
		DelayMS(dly & 0x7FFF);
	}
	else
	{
		DelayUS(dly);
	}
}
