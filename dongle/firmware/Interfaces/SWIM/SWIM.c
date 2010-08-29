/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWIM.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWIM interface implementation file                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_SWIM_EN

#include "SWIM.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

static uint8 SWIM_Inited = 0;
static uint16 SWIM_PULSE_0;
static uint16 SWIM_PULSE_1;
static uint16 SWIM_PULSE_Threshold;
// max length is 1(header)+8(data)+1(parity)+1(ack from target)+1(empty)
static uint16 SWIM_DMA_IN_Buffer[12];
static uint16 SWIM_DMA_OUT_Buffer[12];
static uint16 SWIM_clock_div = 0;

#define SWIM_MAX_DLY					0xFFFFF
#define SWIM_MAX_RESEND_CNT				20

#define SWIM_CMD_BITLEN					3
#define SWIM_CMD_SRST					0x00
#define SWIM_CMD_ROTF					0x01
#define SWIM_CMD_WOTF					0x02

#define SWIM_SYNC_CYCLES				128

void SWIM_Init()
{
	if (!SWIM_Inited)
	{
		SWIM_Inited = 1;
		SYNCSWPWM_OUT_TIMER_INIT();
		SYNCSWPWM_PORT_OD_INIT();
	}
}

void SWIM_Fini()
{
	SYNCSWPWM_PORT_OD_FINI();
	SYNCSWPWM_OUT_TIMER_FINI();
	SYNCSWPWM_IN_TIMER_FINI();
	SWIM_Inited = 0;
}

void SWIM_EnableClockInput(void)
{
	SWIM_clock_div = 0;
	SYNCSWPWM_IN_TIMER_INIT();
}

uint8 SWIM_EnterProgMode(void)
{
	uint8 i;
	uint32 dly;

	SYNCSWPWM_IN_TIMER_DMA_INIT(10, SWIM_DMA_IN_Buffer);

	SWIM_CLR();
	DelayUS(1000);

	for (i = 0; i < 4; i++)
	{
		SWIM_SET();
		DelayUS(500);
		SWIM_CLR();
		DelayUS(500);
	}
	for (i = 0; i < 4; i++)
	{
		SWIM_SET();
		DelayUS(256);
		SWIM_CLR();
		DelayUS(256);
	}
	SWIM_SET();

	dly = SWIM_MAX_DLY;
	SYNCSWPWM_IN_TIMER_DMA_WAIT(dly);
	if (!dly)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

uint8 SWIM_Sync(uint8 mHz)
{
	uint32 dly;
	uint16 clock_div;
	uint16 arr_save;
	
	clock_div = _SYS_FREQUENCY / mHz;
	if ((_SYS_FREQUENCY % mHz) > (mHz / 2))
	{
		clock_div++;
	}
	
	SYNCSWPWM_IN_TIMER_DMA_INIT(2, SWIM_DMA_IN_Buffer);
	
	arr_save = SYNCSWPWM_OUT_TIMER_GetCycle();
	SYNCSWPWM_OUT_TIMER_SetCycle(SWIM_SYNC_CYCLES * clock_div + 1);

	SWIM_DMA_OUT_Buffer[0] = SWIM_SYNC_CYCLES * clock_div;
	SWIM_DMA_OUT_Buffer[1] = 0;
	SYNCSWPWM_OUT_TIMER_DMA_INIT(2, SWIM_DMA_OUT_Buffer);
	SYNCSWPWM_OUT_TIMER_DMA_WAIT();

	dly = SWIM_MAX_DLY;
	SYNCSWPWM_IN_TIMER_DMA_WAIT(dly);
	SYNCSWPWM_OUT_TIMER_SetCycle(arr_save);

	if (!dly)
	{
		return 1;
	}
	else
	{
		SWIM_clock_div = SWIM_DMA_IN_Buffer[1];
		return 0;
	}
}

uint8 SWIM_SetClockParam(uint8 mHz, uint8 cnt0, uint8 cnt1)
{
	uint16 clock_div;
	
	if (SWIM_clock_div)
	{
		clock_div = SWIM_clock_div;
	}
	else
	{
		clock_div = _SYS_FREQUENCY / mHz;
		if ((_SYS_FREQUENCY % mHz) >= (mHz / 2))
		{
			clock_div++;
		}
		clock_div *= SWIM_SYNC_CYCLES;
	}

	SWIM_PULSE_0 = cnt0 * clock_div / SWIM_SYNC_CYCLES;
	if ((cnt0 * clock_div % SWIM_SYNC_CYCLES) >= SWIM_SYNC_CYCLES / 2)
	{
		SWIM_PULSE_0++;
	}
	SWIM_PULSE_1 = cnt1 * clock_div / SWIM_SYNC_CYCLES;
	if ((cnt1 * clock_div % SWIM_SYNC_CYCLES) >= SWIM_SYNC_CYCLES / 2)
	{
		SWIM_PULSE_1++;
	}
	SWIM_PULSE_Threshold = SWIM_PULSE_0 + SWIM_PULSE_1;

	// 1.125 times
	SYNCSWPWM_OUT_TIMER_SetCycle(SWIM_PULSE_Threshold + (SWIM_PULSE_Threshold >> 3));

	SWIM_PULSE_Threshold >>= 1;
	return 0;
}

uint8 SWIM_HW_Out(uint8 cmd, uint8 bitlen, uint16 retry_cnt)
{
	int8 i, p;
	uint32 dly;
	uint16 *ptr = &SWIM_DMA_OUT_Buffer[0];

retry:

	SYNCSWPWM_IN_TIMER_DMA_INIT(bitlen + 3, SWIM_DMA_IN_Buffer);

	*ptr++ = SWIM_PULSE_0;

	p = 0;
	for (i = bitlen - 1; i >= 0; i--)
	{
		if ((cmd >> i) & 1)
		{
			*ptr++ = SWIM_PULSE_1;
			p++;
		}
		else
		{
			*ptr++ = SWIM_PULSE_0;
		}
	}
	// parity bit
	if (p & 1)
	{
		*ptr++ = SWIM_PULSE_1;
	}
	else
	{
		*ptr++ = SWIM_PULSE_0;
	}
	// wait for last waveform -- parity bit
	*ptr++ = 0;
	SYNCSWPWM_OUT_TIMER_DMA_INIT(bitlen + 3, SWIM_DMA_OUT_Buffer);
	SYNCSWPWM_OUT_TIMER_DMA_WAIT();

	dly = SWIM_MAX_DLY;
	SYNCSWPWM_IN_TIMER_DMA_WAIT(dly);
	SYNCSWPWM_IN_TIMER_DMA_INIT(10, SWIM_DMA_IN_Buffer + 1);

	if (!dly)
	{
		// timeout
		return 1;
	}
	else if (SWIM_DMA_IN_Buffer[bitlen + 2] > SWIM_PULSE_Threshold)
	{
		// nack
		if (retry_cnt)
		{
			retry_cnt--;
			goto retry;
		}
		else
		{
			return 1;
		}
	}
	else
	{
		return 0;
	}
}

uint8 SWIM_HW_In(uint8* data, uint8 bitlen)
{
	uint8 ret = 0;
	uint32 dly;

	dly = SWIM_MAX_DLY;
	SYNCSWPWM_IN_TIMER_DMA_WAIT(dly);
	*data = 0;
	if (dly && (SWIM_DMA_IN_Buffer[1] < SWIM_PULSE_Threshold))
	{
		for (dly = 0; dly < 8; dly++)
		{
			if (SWIM_DMA_IN_Buffer[2 + dly] < SWIM_PULSE_Threshold)
			{
				*data |= 1 << (7 - dly);
			}
		}
		SYNCSWPWM_IN_TIMER_DMA_INIT(11, SWIM_DMA_IN_Buffer);

		SWIM_DMA_OUT_Buffer[0] = SWIM_PULSE_1;
		SWIM_DMA_OUT_Buffer[1] = 0;
		SYNCSWPWM_OUT_TIMER_DMA_INIT(2, SWIM_DMA_OUT_Buffer);
		SYNCSWPWM_OUT_TIMER_DMA_WAIT();
	}
	else
	{
		ret = 1;
	}

	return ret;
}

uint8 SWIM_SRST(void)
{
	return SWIM_HW_Out(SWIM_CMD_SRST, SWIM_CMD_BITLEN, SWIM_MAX_RESEND_CNT);
}

uint8 SWIM_WOTF(uint32 addr, uint16 len, uint8 *data)
{
	uint16 processed_len;
	uint8 cur_len, i;
	uint32 cur_addr;

	if ((0 == len) || ((uint8*)0 == data))
	{
		return 1;
	}

	processed_len = 0;
	cur_addr = addr;
	while (processed_len < len)
	{
		if ((len - processed_len) > 255)
		{
			cur_len = 255;
		}
		else
		{
			cur_len = len - processed_len;
		}

		if(SWIM_HW_Out(SWIM_CMD_WOTF, SWIM_CMD_BITLEN, SWIM_MAX_RESEND_CNT))
		{
			return 1;
		}
		if (SWIM_HW_Out(cur_len, 8, 0))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 16) & 0xFF, 8, 0))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 8) & 0xFF, 8, 0))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 0) & 0xFF, 8, 0))
		{
			return 1;
		}
		for (i = 0; i < cur_len; i++)
		{
			if (SWIM_HW_Out(data[processed_len + i], 8, SWIM_MAX_RESEND_CNT))
			{
				return 1;
			}
		}

		cur_addr += cur_len;
		processed_len += cur_len;
	}

	return 0;
}

uint8 SWIM_ROTF(uint32 addr, uint16 len, uint8 *data)
{
	uint16 processed_len;
	uint8 cur_len, i;
	uint32 cur_addr;

	if ((0 == len) || ((uint8*)0 == data))
	{
		return 1;
	}

	processed_len = 0;
	cur_addr = addr;
	while (processed_len < len)
	{
		if ((len - processed_len) > 255)
		{
			cur_len = 255;
		}
		else
		{
			cur_len = len - processed_len;
		}

		if(SWIM_HW_Out(SWIM_CMD_ROTF, SWIM_CMD_BITLEN, SWIM_MAX_RESEND_CNT))
		{
			return 1;
		}
		if (SWIM_HW_Out(cur_len, 8, 0))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 16) & 0xFF, 8, 0))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 8) & 0xFF, 8, 0))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 0) & 0xFF, 8, 0))
		{
			return 1;
		}
		for (i = 0; i < cur_len; i++)
		{
			if (SWIM_HW_In(&data[processed_len + i], 8))
			{
				return 1;
			}
		}

		cur_addr += cur_len;
		processed_len += cur_len;
	}

	return 0;
}

#endif		// #if STM8_SWIM_EN
