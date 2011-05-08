/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    USBDM                                                     *
 *  File:       CommandProcesor.c                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    CommandProcesor for USBDM                                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-09-02:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "Common.h"
#include "Commands.h"
#include "BDM_USBDM.h"
#include "CmdProcessing.h"

void BeforeInit(void){}

void AfterInit(void){}

void ProcessCommand(uint8_t *dat, uint16_t len)
{
	memcpy(commandBuffer, dat, len);
	commandBuffer[0] = len;
	rep_len = commandExec();
	memcpy(dat, commandBuffer, rep_len);
}

void ProcessIdle(void)
{
}

uint8_t CheckLocalHandler(void)
{
	return 0;
}

void ProcessLocalHandler(void)
{
}
