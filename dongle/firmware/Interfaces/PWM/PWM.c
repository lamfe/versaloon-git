/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       PWM.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    PWM header file                                           *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2011-03-06:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_PWM_EN

#include "interfaces.h"
#include "PWM.h"

static uint16_t pwm_cycle = 0;

RESULT pwm_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		SYNCSWPWM_OUT_TIMER_INIT();
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT pwm_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		SYNCSWPWM_OUT_TIMER_FINI();
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT pwm_config(uint8_t index, uint16_t kHz, uint8_t idle)
{
	switch (index)
	{
	case 0:
		pwm_cycle = SYNCSWPWM_OUT_TIMER_MHZ * 1000 / kHz;
		SYNCSWPWM_OUT_TIMER_SetCycle(pwm_cycle);
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT pwm_out(uint8_t index, uint16_t count, uint16_t *rate)
{
	uint16_t i;
	
	switch (index)
	{
	case 0:
		for (i = 0; i < count; i++)
		{
			rate[i] = rate[i] * pwm_cycle / 0xFFFF;
		}
		SYNCSWPWM_OUT_TIMER_DMA_INIT(count, rate);
		SYNCSWPWM_OUT_TIMER_DMA_WAIT();
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

#endif
