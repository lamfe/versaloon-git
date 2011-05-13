/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       CommandProcesor.c                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    CommandProcesor for Versaloon/USB_TO_XXX                  *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "interfaces.h"
#include "usb_protocol.h"

#if USB_TO_XXX_EN
#	include "USB_TO_XXX.h"
#endif

uint8_t Versaloon_Ver[] = "Versaloon(" _HARDWARE_VER_STR ")by Simon(compiled on " __DATE__ ")";

void BeforeInit(void){}

void AfterInit(void)
{
	usb_protocol_init();
}

static void Versaloon_ProcessCommonCmd(uint8_t *dat, uint16_t len)
{
	switch(dat[0])
	{
	case VERSALOON_GET_INFO:
		SET_LE_U16(&dat[0], USB_DATA_BUFF_SIZE);
		memcpy(dat + 2, Versaloon_Ver, sizeof(Versaloon_Ver));
		rep_len = sizeof(Versaloon_Ver) + 2;
		break;
	case VERSALOON_GET_TVCC:
		SET_LE_U16(&dat[0], Vtarget);
		rep_len = 2;
		break;
	case VERSALOON_GET_HARDWARE:
		dat[0] = _HARDWARE_VER;
		rep_len = 1;
		break;
	}
}

void ProcessCommand(uint8_t* dat, uint16_t len)
{
	uint8_t cmd = 0;

	// first byte of the USB package is the command byte
	cmd = buffer_out[0];
	// check command and call corresponding module
	if(cmd <= VERSALOON_COMMON_CMD_END)
	{
		// Common Commands
		Versaloon_ProcessCommonCmd(buffer_out, cmd_len & 0xFFFF);
	}
#if USB_TO_XXX_EN
	else if(cmd <= VERSALOON_USB_TO_XXX_CMD_END)
	{
		buffer_reply = buffer_out;
		USB_TO_POLL_Index = -1;
		USB_TO_XXX_ProcessCmd(buffer_out, cmd_len & 0xFFFF);
	}
#endif		// #if USB_TO_XXX_EN
}

void ProcessIdle(void){}
uint8_t CheckLocalHandler(void){return 0;}
void ProcessLocalHandler(void){}
