/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_SWIM.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_SWIM                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_SWIM_EN

#include "USB_TO_XXX.h"
#include "SWIM.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_SWIM_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;
	uint32 swim_addr, swim_len;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_SWIM_NUM)
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
			buffer_reply[rep_len++] = USB_TO_SWIM_NUM;
			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			SWIM_Init();
			if (SWIM_SetClockParam(dat[index + 0], dat[index + 1], dat[index + 2]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}

			break;
		case USB_TO_XXX_FINI:
			SWIM_Fini();
			PWREXT_Release();
			GLOBAL_OUTPUT_Release();
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_OUT:
			swim_addr = (dat[index + 2] << 0) + (dat[index + 3] << 8) + (dat[index + 4] << 16);
			swim_len = (dat[index + 0] << 0) + (dat[index + 1] << 8);
			if (SWIM_WOTF(swim_addr, swim_len, &dat[index + 5]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}

			break;
		case USB_TO_XXX_IN:
			swim_addr = (dat[index + 2] << 0) + (dat[index + 3] << 8) + (dat[index + 4] << 16);
			swim_len = (dat[index + 0] << 0) + (dat[index + 1] << 8);
			if (SWIM_ROTF(swim_addr, swim_len, &buffer_reply[rep_len + 1]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				rep_len += swim_len;
			}

			break;
		case USB_TO_XXX_RESET:
			if (SWIM_SRST())
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_SYNC:
			if (SWIM_Sync(dat[index + 0]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_ENABLE:
			SWIM_EnableClockInput();
			if (SWIM_EnterProgMode())
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
