/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       CommandProcesor.c                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    CommandProcesor for STK500V2 from Atmel                   *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#include "STK500v2_Const.h"
#include "STK500v2.h"

void BeforeInit(void){}

void AfterInit(void){}

void ProcessCommand(uint8* dat, uint16 len)
{
	STK500V2_Process(dat, len);
}

void ProcessIdle(void)
{
	STK500V2_Poll();
}

uint8 CheckLocalHandler(void)
{
	return 0;
}

void ProcessLocalHandler(void)
{
}
