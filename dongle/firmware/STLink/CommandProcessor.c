/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STLink                                                    *
 *  File:       CommandProcesor.c                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    CommandProcesor for STLink from ST                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-09-01:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#include "STLink.h"

void ProcessCommand(uint8* dat, uint16 len)
{
	STLink_SWIM_Process(dat);
}

void ProcessIdle(void)
{
}

uint8 CheckLocalHandler(void)
{
	return 0;
}

void ProcessLocalHandler(void)
{
}
