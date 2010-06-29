/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
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
static uint16 SWIM_DMA_Buffer[12];
static uint16 SWIM_clock_div = 0;

#define SWIM_MAX_DLY					0xFFFFF

#define SWIM_CMD_BITLEN					3
#define SWIM_CMD_SRST					0x00
#define SWIM_CMD_ROTF					0x01
#define SWIM_CMD_WOTF					0x02

#define SWIM_SYNC_CYCLES				128

static void SWIM_SetClockParamInit(void)
{
	TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;
	TIM_OCInitTypeDef TIM_OCInitStructure;

	/* SWIM_TIMER_OUT */
	TIM_TimeBaseStructInit(&TIM_TimeBaseStructure);
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = 0;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(SWIM_OUT_TIMER, &TIM_TimeBaseStructure);

	TIM_OCStructInit(&TIM_OCInitStructure);
	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_Pulse = 0;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_Low;
	TIM_OC1Init(SWIM_OUT_TIMER, &TIM_OCInitStructure);

	TIM_OC1PreloadConfig(SWIM_OUT_TIMER, TIM_OCPreload_Enable);
	TIM_ARRPreloadConfig(SWIM_OUT_TIMER, ENABLE);
	TIM_Cmd(SWIM_OUT_TIMER, ENABLE);
	SWIM_OUT_TIMER_PWMEN();
}

void SWIM_Init()
{
	if (!SWIM_Inited)
	{
		SWIM_Inited = 1;
		SWIM_OUT_TIMER_INIT();
		SWIM_SetClockParamInit();
		SWIM_PORT_INIT();
	}
}

void SWIM_Fini()
{
	SWIM_PORT_FINI();
	SWIM_OUT_TIMER_FINI();
	SWIM_IN_TIMER_FINI();
	SWIM_Inited = 0;
}

uint8 SWIM_Enable(void)
{
	uint8 i;
	uint32 dly;
	TIM_ICInitTypeDef TIM_ICInitStructure;

	SWIM_clock_div = 0;
	SWIM_IN_TIMER_INIT();

	/* SWIM_TIMER_IN */
	TIM_ICStructInit(&TIM_ICInitStructure);
	TIM_ICInitStructure.TIM_Channel = TIM_Channel_2;
	TIM_ICInitStructure.TIM_ICPolarity = TIM_ICPolarity_Rising;
	TIM_ICInitStructure.TIM_ICSelection = TIM_ICSelection_IndirectTI;
	TIM_ICInitStructure.TIM_ICPrescaler = TIM_ICPSC_DIV1;
	TIM_ICInitStructure.TIM_ICFilter = 0;
	TIM_PWMIConfig(SWIM_IN_TIMER, &TIM_ICInitStructure);

	TIM_SelectInputTrigger(SWIM_IN_TIMER, TIM_TS_TI1FP1);
	TIM_SelectSlaveMode(SWIM_IN_TIMER, TIM_SlaveMode_Reset);
	TIM_SelectMasterSlaveMode(SWIM_IN_TIMER, TIM_MasterSlaveMode_Enable);
	TIM_DMACmd(SWIM_IN_TIMER, TIM_DMA_CC2, ENABLE);
	TIM_Cmd(SWIM_IN_TIMER, ENABLE);

	SWIM_IN_TIMER_DMA_INIT(10, SWIM_DMA_Buffer);

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
	SWIM_IN_TIMER_DMA_WAIT(dly);
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
	
	SWIM_IN_TIMER_DMA_INIT(2, SWIM_DMA_Buffer);
	
	arr_save = SWIM_OUT_TIMER->ARR;
	SWIM_OUT_TIMER->ARR = SWIM_SYNC_CYCLES * clock_div + 1;
	SWIM_OUT_TIMER->EGR = TIM_PSCReloadMode_Immediate;
	SWIM_OUT_TIMER->CR1 |= ((uint16_t)0x0002);
	SWIM_OUT_TIMER->SR = (uint16_t)~TIM_FLAG_Update;
	SWIM_OUT_TIMER->CCR1 = SWIM_SYNC_CYCLES * clock_div;
	SWIM_OUT_TIMER->CR1 &= ((uint16_t)0x03FD);
	SWIM_WaitOutBitReady();
	SWIM_OUT_TIMER->SR = (uint16_t)~TIM_FLAG_Update;
	SWIM_OUT_TIMER->CCR1 = 0;
	SWIM_WaitOutBitReady();
	
	dly = SWIM_MAX_DLY;
	SWIM_IN_TIMER_DMA_WAIT(dly);
	SWIM_OUT_TIMER->ARR = arr_save;
	SWIM_OUT_TIMER->EGR = TIM_PSCReloadMode_Immediate;
	if (!dly)
	{
		return 1;
	}
	else
	{
		SWIM_clock_div = SWIM_DMA_Buffer[1];
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

	SWIM_OUT_TIMER->ARR = SWIM_PULSE_Threshold;
	SWIM_OUT_TIMER->EGR = TIM_PSCReloadMode_Immediate;

	SWIM_PULSE_Threshold >>= 1;
	return 0;
}

uint8 SWIM_HW_Out(uint8 cmd, uint8 bitlen)
{
	int8 i, p;
	uint32 dly;

	SWIM_IN_TIMER_DMA_INIT(bitlen + 3, SWIM_DMA_Buffer);

	SWIM_OUT_TIMER->CR1 |= ((uint16_t)0x0002);
	SWIM_OUT_TIMER->SR = (uint16_t)~TIM_FLAG_Update;
	// first bit MUST be '0' for output data
	SWIM_OUT_TIMER->CCR1 = SWIM_PULSE_0;
	SWIM_OUT_TIMER->CR1 &= ((uint16_t)0x03FD);
	SWIM_WaitOutBitReady();
	SWIM_OUT_TIMER->SR = (uint16_t)~TIM_FLAG_Update;

	p = 0;
	for (i = bitlen - 1; i >= 0; i--)
	{
		if ((cmd >> i) & 1)
		{
			SWIM_OUT_TIMER->CCR1 = SWIM_PULSE_1;
			p++;
		}
		else
		{
			SWIM_OUT_TIMER->CCR1 = SWIM_PULSE_0;
		}
		// wait for previous waveform
		SWIM_WaitOutBitReady();
		SWIM_OUT_TIMER->SR = (uint16_t)~TIM_FLAG_Update;
	}
	// parity bit
	if (p & 1)
	{
		SWIM_OUT_TIMER->CCR1 = SWIM_PULSE_1;
	}
	else
	{
		SWIM_OUT_TIMER->CCR1 = SWIM_PULSE_0;
	}
	// wait for last waveform -- parity bit
	SWIM_WaitOutBitReady();
	SWIM_OUT_TIMER->SR = (uint16_t)~TIM_FLAG_Update;
	SWIM_OUT_TIMER->CCR1 = 0;
	SWIM_WaitOutBitReady();

	dly = SWIM_MAX_DLY;
	SWIM_IN_TIMER_DMA_WAIT(dly);
	SWIM_IN_TIMER_DMA_INIT(10, SWIM_DMA_Buffer + 1);

	if (!dly || (SWIM_DMA_Buffer[bitlen + 2] > SWIM_PULSE_Threshold))
	{
		return 1;
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
	SWIM_IN_TIMER_DMA_WAIT(dly);
	*data = 0;
	if (dly && (SWIM_DMA_Buffer[1] < SWIM_PULSE_Threshold))
	{
		for (dly = 0; dly < 8; dly++)
		{
			if (SWIM_DMA_Buffer[2 + dly] < SWIM_PULSE_Threshold)
			{
				*data |= 1 << (7 - dly);
			}
		}
		SWIM_IN_TIMER_DMA_INIT(11, SWIM_DMA_Buffer);

		SWIM_OUT_TIMER->CR1 |= ((uint16_t)0x0002);
		SWIM_OUT_TIMER->SR = (uint16_t)~TIM_FLAG_Update;
		SWIM_OUT_TIMER->CCR1 = SWIM_PULSE_1;
		SWIM_OUT_TIMER->CR1 &= ((uint16_t)0x03FD);
		SWIM_WaitOutBitReady();
		SWIM_OUT_TIMER->SR = (uint16_t)~TIM_FLAG_Update;
		SWIM_OUT_TIMER->CCR1 = 0;
		SWIM_WaitOutBitReady();
	}
	else
	{
		ret = 1;
	}

	return ret;
}

uint8 SWIM_SRST(void)
{
	return SWIM_HW_Out(SWIM_CMD_SRST, SWIM_CMD_BITLEN);
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

		if (SWIM_HW_Out(SWIM_CMD_WOTF, SWIM_CMD_BITLEN))
		{
			return 1;
		}
		if (SWIM_HW_Out(cur_len, 8))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 16) & 0xFF, 8))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 8) & 0xFF, 8))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 0) & 0xFF, 8))
		{
			return 1;
		}
		for (i = 0; i < cur_len; i++)
		{
			if (SWIM_HW_Out(data[processed_len + i], 8))
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

		if (SWIM_HW_Out(SWIM_CMD_ROTF, SWIM_CMD_BITLEN))
		{
			return 1;
		}
		if (SWIM_HW_Out(cur_len, 8))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 16) & 0xFF, 8))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 8) & 0xFF, 8))
		{
			return 1;
		}
		if (SWIM_HW_Out((cur_addr >> 0) & 0xFF, 8))
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
