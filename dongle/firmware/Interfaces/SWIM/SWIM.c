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

static uint8 SWIM_BitLength;

void SWIM_Init()
{
	
}

void SWIM_Fini()
{
}

void SWIM_SendEntrySeq(void)
{
}

uint8 SWIM_MeasureSyncFrame(uint16 *speed)
{
	return 0;
}

void SWIM_SetTargetFreq(uint16 speed)
{
	
}

void SWIM_SetSpeed(uint8 hs_ls)
{
}

void SWIM_Reset(void)
{
}

uint8 SWIM_Out(uint8 cmd, uint8 len)
{
	return 0;
}

uint8 SWIM_In(uint8* data)
{
	return 0;
}

#endif		// #if STM8_SWIM_EN
