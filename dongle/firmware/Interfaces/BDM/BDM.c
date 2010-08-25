/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       BDM.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    BDM interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_BDM_EN

#include "BDM.h"

uint8 BDM_Inited = 0;
static uint16 BDM_DMA_Buffer[112];
static uint16 BDM_clock_div = 0;

static uint16 BDM_PULSE_0;
static uint16 BDM_PULSE_1;
static uint16 BDM_PULSE_Threshold;

#define BDM_SYNC_CYCLES					128
#define BDM_MAX_DLY						0xFFFFF

void BDM_Init(void)
{
	if (!BDM_Inited)
	{
		BDM_Inited = 1;
		BDM_clock_div = 0;
		SYNCSWPWM_OUT_TIMER_INIT();
		SYNCSWPWM_PORT_OD_INIT();
		SYNCSWPWM_IN_TIMER_INIT();
	}
}

void BDM_Fini(void)
{
	SYNCSWPWM_PORT_OD_FINI();
	SYNCSWPWM_OUT_TIMER_FINI();
	SYNCSWPWM_IN_TIMER_FINI();
	BDM_Inited = 0;
}

uint8 BDM_Sync(uint16 *khz)
{
	uint32 dly;

	// reset MUST be released to perform sync
	dly = 0;
	while (!SW_GET())
	{
		DelayUS(1);
		if (++dly > 0xFFFF)
		{
			return 1;
		}
	}
	// more 10ms for stablity
	DelayMS(10);

	SYNCSWPWM_IN_TIMER_DMA_INIT(2, BDM_DMA_Buffer);

	SYNCSWPWM_OUT_TIMER_SetCycle(0xFFFF);
	SYNCSWPWM_OUT_TIMER_OutfirstRdy(0xFFFE);
	SYNCSWPWM_OUT_TIMER_OutRdy(0);

	dly = BDM_MAX_DLY;
	SYNCSWPWM_IN_TIMER_DMA_WAIT(dly);

	if (!dly)
	{
		return 1;
	}
	else
	{
		BDM_clock_div = BDM_DMA_Buffer[1];
		*khz = _SYS_FREQUENCY * 1000 * 128 / BDM_clock_div;
		BDM_PULSE_1 = BDM_clock_div * 4 / BDM_SYNC_CYCLES;
		if ((BDM_clock_div * 5 % BDM_SYNC_CYCLES) >= (BDM_SYNC_CYCLES / 2))
		{
			BDM_PULSE_1++;
		}
		BDM_PULSE_0 = BDM_clock_div * 13 / BDM_SYNC_CYCLES;
		if ((BDM_clock_div * 13 % BDM_SYNC_CYCLES) >= (BDM_SYNC_CYCLES / 2))
		{
			BDM_PULSE_0++;
		}
		BDM_PULSE_Threshold = BDM_clock_div * 9 / BDM_SYNC_CYCLES;
		SYNCSWPWM_OUT_TIMER_SetCycle(BDM_clock_div * 18 / BDM_SYNC_CYCLES);
		return 0;
	}
}

uint8 BDM_OutByte(uint8 data)
{
	int8 i;

	if (data & 0x80)
	{
		SYNCSWPWM_OUT_TIMER_OutfirstRdy(BDM_PULSE_1);
	}
	else
	{
		SYNCSWPWM_OUT_TIMER_OutfirstRdy(BDM_PULSE_0);
	}

	for (i = 6; i >= 0; i--)
	{
		if (data & (1 << i))
		{
			SYNCSWPWM_OUT_TIMER_Out(BDM_PULSE_1);
		}
		else
		{
			SYNCSWPWM_OUT_TIMER_Out(BDM_PULSE_0);
		}
		SYNCSWPWM_OUT_TIMER_WaitReady();
	}
	SYNCSWPWM_OUT_TIMER_OutRdy(0);

	return 0;
}

uint8 BDM_OutDly(void)
{
	uint8 i;

	for (i = 0; i < 16; i++)
	{
		SYNCSWPWM_OUT_TIMER_OutRdy(0);
	}

	return 0;
}

uint8 BDM_InByte(void)
{
	int8 i;

	SYNCSWPWM_OUT_TIMER_OutfirstRdy(BDM_PULSE_1);

	for (i = 0; i < 7; i++)
	{
		SYNCSWPWM_OUT_TIMER_OutRdy(BDM_PULSE_1);
	}
	SYNCSWPWM_OUT_TIMER_OutRdy(0);

	return 0;
}

uint8 BDM_Transact(uint8 token, uint8 *out, uint8 *in)
{
	uint32 dly;
	uint16 outlen, inlen, i, offset;

	outlen = BDM_OUT_LEN(token) + BDM_IN_LEN(token);
	if (!outlen)
	{
		return 1;
	}
	offset = 0;
	outlen *= 8;
	if (BDM_ACK(token))
	{
		outlen++;
		offset = 1;
	}
	SYNCSWPWM_IN_TIMER_DMA_INIT(outlen, BDM_DMA_Buffer);

	// out data
	outlen = BDM_OUT_LEN(token);
	for (i = 0; i < outlen; i++)
	{
		if (BDM_OutByte(out[i]))
		{
			return 1;
		}
	}

	// out delay
	if (BDM_OUT_DLY(token) && BDM_OutDly())
	{
		return 1;
	}

	// in data
	inlen = BDM_IN_LEN(token);
	for (i = 0; i < inlen; i++)
	{
		BDM_InByte();
	}

	// wait for in data
	dly = BDM_MAX_DLY;
	SYNCSWPWM_IN_TIMER_DMA_WAIT(dly);
	if (!dly)
	{
		return 1;
	}

	for (i = 0; i < inlen * 8; i++)
	{
		in[i / 8] <<= 1;
		if (BDM_DMA_Buffer[outlen * 8 + offset + i] >= BDM_PULSE_Threshold)
		{
			in[i / 8] &= ~1;
		}
		else
		{
			in[i / 8] |= 1;
		}
	}

	return 0;
}

#endif
