/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
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

#include "PowerExt.h"

uint8 PWREXT_EnableCount = 0;

void PWREXT_Acquire(void)
{
	if(Vtarget < TVCC_SAMPLE_MIN_POWER)
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

#endif /* USB_TO_POWER_EN || POWER_OUT_EN*/
