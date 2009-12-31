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

static uint16 SWIM_PULSE_0;
static uint16 SWIM_PULSE_1;
static uint16 SWIM_PULSE_Threshold;
// max length is 1(header)+8(data)+1(parity)+1(ack from target)+1(empty)
static uint16 SWIM_DMA_Buffer[12];

static void SWIM_SetClockParamInit(void)
{
	TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;
	TIM_OCInitTypeDef TIM_OCInitStructure;
	TIM_ICInitTypeDef TIM_ICInitStructure;

	/* SWIM_TIMER_OUT */
	TIM_TimeBaseStructInit(&TIM_TimeBaseStructure);
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = 1;
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

	/* SWIM_TIMER_IN */
	TIM_ICStructInit(&TIM_ICInitStructure);
	TIM_ICInitStructure.TIM_Channel = TIM_Channel_1;
	TIM_ICInitStructure.TIM_ICPolarity = TIM_ICPolarity_Rising;
	TIM_ICInitStructure.TIM_ICSelection = TIM_ICSelection_DirectTI;
	TIM_ICInitStructure.TIM_ICPrescaler = TIM_ICPSC_DIV1;
	TIM_ICInitStructure.TIM_ICFilter = 0;
	TIM_PWMIConfig(SWIM_IN_TIMER, &TIM_ICInitStructure);

	TIM_SelectInputTrigger(SWIM_IN_TIMER, TIM_TS_TI2FP2);
	TIM_SelectSlaveMode(SWIM_IN_TIMER, TIM_SlaveMode_Reset);
	TIM_SelectMasterSlaveMode(SWIM_IN_TIMER, TIM_MasterSlaveMode_Enable);
	TIM_DMACmd(SWIM_IN_TIMER, TIM_DMA_CC1, ENABLE);
	TIM_Cmd(SWIM_IN_TIMER, ENABLE);
}

void SWIM_Init()
{
	SWIM_OUT_TIMER_INIT();
	SWIM_IN_TIMER_INIT();
	SWIM_SetClockParamInit();
	SWIM_PORT_INIT();
}

void SWIM_Fini()
{
	SWIM_PORT_FINI();
	SWIM_OUT_TIMER_FINI();
	SWIM_IN_TIMER_FINI();
}

void SWIM_SetClockParam(uint8 mHz, uint8 cnt0, uint8 cnt1)
{
	uint8_t clock_div;
	
	clock_div = _SYS_FREQUENCY / mHz;
	if ((_SYS_FREQUENCY % mHz) > (mHz / 2))
	{
		clock_div++;
	}
	SWIM_PULSE_0 = (cnt0 + 3) * clock_div;
	SWIM_PULSE_1 = (cnt1 + 1) * clock_div;
	SWIM_PULSE_Threshold = (SWIM_PULSE_0 + SWIM_PULSE_1) >> 1;

	TIM_SetAutoreload(SWIM_OUT_TIMER, SWIM_PULSE_0 + SWIM_PULSE_1);
	TIM_ClearFlag(SWIM_OUT_TIMER, TIM_FLAG_Update);
	SWIM_WaitOutBitReady();
}

uint8 SWIM_Out(uint8 cmd, uint8 bitlen)
{
	int8 i, p;
	uint32 dly;

	SWIM_IN_TIMER_DMA_INIT(bitlen + 3, SWIM_DMA_Buffer);

	TIM_UpdateDisableConfig(SWIM_OUT_TIMER, ENABLE);
	TIM_ClearFlag(SWIM_OUT_TIMER, TIM_FLAG_Update);
	// first bit MUST be '0' for output data
	TIM_SetCompare1(SWIM_OUT_TIMER, SWIM_PULSE_0);
	TIM_UpdateDisableConfig(SWIM_OUT_TIMER, DISABLE);
	SWIM_WaitOutBitReady();
	TIM_ClearFlag(SWIM_OUT_TIMER, TIM_FLAG_Update);

	p = 0;
	for (i = bitlen - 1; i >= 0; i--)
	{
		if ((cmd >> i) & 1)
		{
			TIM_SetCompare1(SWIM_OUT_TIMER, SWIM_PULSE_1);
			p++;
		}
		else
		{
			TIM_SetCompare1(SWIM_OUT_TIMER, SWIM_PULSE_0);
		}
		// wait for previous waveform
		SWIM_WaitOutBitReady();
		TIM_ClearFlag(SWIM_OUT_TIMER, TIM_FLAG_Update);
	}
	// parity bit
	if (p & 1)
	{
		TIM_SetCompare1(SWIM_OUT_TIMER, SWIM_PULSE_1);
	}
	else
	{
		TIM_SetCompare1(SWIM_OUT_TIMER, SWIM_PULSE_0);
	}
	// wait for last waveform -- parity bit
	SWIM_WaitOutBitReady();
	TIM_ClearFlag(SWIM_OUT_TIMER, TIM_FLAG_Update);
	TIM_SetCompare1(SWIM_OUT_TIMER, 0);
	SWIM_WaitOutBitReady();

	dly = 0xFFFF;
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

uint8 SWIM_In(uint8* data, uint8 bitlen)
{
	uint8 ret = 0;
	uint32 dly;

	dly = 0xFFFFF;
	SWIM_IN_TIMER_DMA_WAIT(dly);
//	DelayUS(100);
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

		TIM_UpdateDisableConfig(SWIM_OUT_TIMER, ENABLE);
		TIM_ClearFlag(SWIM_OUT_TIMER, TIM_FLAG_Update);
		TIM_SetCompare1(SWIM_OUT_TIMER, SWIM_PULSE_1);
		TIM_UpdateDisableConfig(SWIM_OUT_TIMER, DISABLE);
		SWIM_WaitOutBitReady();
		TIM_ClearFlag(SWIM_OUT_TIMER, TIM_FLAG_Update);
		TIM_SetCompare1(SWIM_OUT_TIMER, 0);
		SWIM_WaitOutBitReady();
	}
	else
	{
		ret = 1;
	}

	return ret;
}

#endif		// #if STM8_SWIM_EN
