/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_SWJ.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_SWJ                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_SWJ_EN

#include "USB_TO_XXX.h"
#include "SWJ.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_SWJ_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint16 cur_dat_len;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_SWJ_NUM)
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
			buffer_reply[rep_len++] = USB_TO_SWJ_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			SWJ_SetTurnaround(dat[index + 0]);
			SWJ_SetRetryCount(dat[index + 1] + (dat[index + 2] << 8));
			SWJ_SetDelay(dat[index + 3] + (dat[index + 4] << 8));
			SWJ_Init();

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_FINI:
			SWJ_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_SWJ_SEQOUT:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			cur_dat_len = dat[index] + (dat[index + 1] << 8);
			SWJ_SWDIO_SET();
			SWJ_SWDIO_SETOUTPUT();
			SWJ_SeqOut(&dat[index + 2], cur_dat_len);
			SWJ_SWDIO_SETINPUT();

			break;
		case USB_TO_SWJ_SEQIN:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			cur_dat_len = dat[index] + (dat[index + 1] << 8);
			SWJ_SeqIn(&buffer_reply[rep_len], cur_dat_len);
			rep_len += (cur_dat_len + 7) >> 3;

			break;
		case USB_TO_SWJ_Transact:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			buffer_reply[rep_len] = SWJ_Transaction(dat[index], (uint32*)(dat + index + 1));
			memcpy(buffer_reply + rep_len + 1, dat + index + 1, 4);
			rep_len += 5;

			 break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
