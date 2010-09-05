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

#include "usb_lib.h"
#include "usb_pwr.h"
#include "usb_cdc.h"

#if USB_TO_XXX_EN
#	include "USB_TO_XXX.h"
#endif

#define FWU_KEY							0x55AA

uint8 Versaloon_Ver[] = "Versaloon(" _HARDWARE_VER_STR ")by Simon(compiled on " __DATE__ ")";

static void Versaloon_ProcessCommonCmd(uint8* dat, uint16 len)
{
#if VERSALOON_FW_UPDATE_EN
	uint32 key;
#endif

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
#if VERSALOON_FW_UPDATE_EN
	case VERSALOON_FW_UPDATE:
		key = LE_TO_SYS_U16(GET_LE_U16(&dat[1]));
		if(key == FWU_KEY)
		{
			RCC_APB1PeriphClockCmd(RCC_APB1Periph_BKP | RCC_APB1Periph_PWR, ENABLE);
			PWR_BackupAccessCmd(ENABLE);
			BKP_WriteBackupRegister(BKP_DR1, FWU_KEY);

			// Reset USB, and Reset MCU
			USB_Disable();

			USB_Disconnect();
			USB_D_CLR();
			USB_D_SETOUTPUT();
			DelayMS(10);
			USB_D_SET();

			NVIC_SystemReset();
		}
		break;
#endif
	}
}

void ProcessCommand(uint8* dat, uint16 len)
{
	uint8 cmd = 0;

	cmd = buffer_out[0];		// first byte of the USB package is the command byte
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

extern uint8 USBTOUSART_En;
void ProcessIdle(void)
{
	if (USBTOUSART_En)
	{
		CDC_Process();
	}
}

uint8 CheckLocalHandler(void)
{
	return CDC_enable;
}

void ProcessLocalHandler(void)
{
	while(CDC_enable)
	{
		CDC_Process();
		PWREXT_Check(0);
	}
}
