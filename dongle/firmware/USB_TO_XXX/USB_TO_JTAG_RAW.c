/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_JTAG_RAW.c                                         *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_JTAG_RAW                   *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_JTAG_RAW_EN

#include "USB_TO_XXX.h"
#include "JTAG_TAP.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_JTAG_RAW_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_JTAG_RAW_NUM)
		{
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_INDEX;
			return;
		}
		length = dat[index + 1] + (dat[index + 2] << 8);
		index += 3;

		switch(command)
		{
		case USB_TO_XXX_INIT:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;
			buffer_reply[rep_len++] = USB_TO_JTAG_RAW_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			JTAG_TAP_HS_Init(dat[index + 0] | (dat[index + 1] << 8), JTAG_TAP_DMA);
			JTAG_TAP_HS_SetTCKFreq(dat[index + 0] | (dat[index + 1] << 8));

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_FINI:
			JTAG_TAP_HS_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_IN_OUT:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;
			JTAG_TAP_HS_Operate_DMA(length / 2, &dat[index], &dat[index + length / 2], &buffer_reply[rep_len]);
			rep_len += length / 2;

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
