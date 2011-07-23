/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       PowerExt.c                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    Power output interface implementation file                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2009-06-20:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if POWER_OUT_EN

#include "app_interfaces.h"
#include "PowerExt.h"

static uint8_t PWREXT_EnableCount = 0;
uint16_t PWREXT_Vtarget = 0;

void PWREXT_Acquire(void)
{
	if(PWREXT_Vtarget < TVCC_SAMPLE_MIN_POWER)
	{
		if(!PWREXT_EnableCount)
		{
			PWREXT_ENABLE();
		}
	}
	PWREXT_EnableCount++;
}

void PWREXT_Release(void)
{
	if(PWREXT_EnableCount)
	{
		PWREXT_EnableCount--;
		if(!PWREXT_EnableCount)
		{
			PWREXT_DISABLE();
		}
	}
}

void PWREXT_ForceRelease(void)
{
	while(PWREXT_EnableCount)
	{
		PWREXT_Release();
	}
}

uint8_t PWREXT_GetState(void)
{
	return PWREXT_EnableCount;
}

RESULT target_voltage_set(uint8_t index, uint16_t voltage)
{
	static uint8_t power_state = 0;
	
	switch (index)
	{
	case 0:
		if (!PWREXT_GetState())
		{
			power_state = 0;
		}

		if(voltage == 3300)
		{
			// only support 3.3V
			if (!power_state)
			{
				power_state = 1;
				PWREXT_Acquire();
			}
			return ERROR_OK;
		}
		else if(voltage == 0)
		{
			if (power_state)
			{
				power_state = 0;
				PWREXT_ForceRelease();
			}
			return ERROR_OK;
		}
		else
		{
			return ERROR_FAIL;
		}
	default:
		return ERROR_FAIL;
	}
}

RESULT target_voltage_get(uint8_t index, uint16_t *voltage)
{
	if (voltage != NULL)
	{
		*voltage = PWREXT_Vtarget;
	}
	return ERROR_OK;
}

#endif /* USB_TO_POWER_EN || POWER_OUT_EN*/
