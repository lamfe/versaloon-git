/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_SWD.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_SWD                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_SWD_EN

#include "USB_TO_XXX.h"
#include "SWD.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_SWD_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;
	uint32 swd_data;

	uint16 cur_dat_len;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_SWD_NUM)
		{
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_INDEX;
			return;
		}
		length = LE_TO_SYS_U16(GET_LE_U16(&dat[index + 1]));
		index += 3;

		switch(command)
		{
		case USB_TO_XXX_INIT:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;
			buffer_reply[rep_len++] = USB_TO_SWD_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			SWD_SetTurnaround(dat[index + 0]);
			SWD_SetRetryCount(LE_TO_SYS_U16(GET_LE_U16(&dat[index + 1])));
			SWD_SetDelay(LE_TO_SYS_U16(GET_LE_U16(&dat[index + 3])));
			SWD_Init();

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_FINI:
			SWD_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_SWD_SEQOUT:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			cur_dat_len = LE_TO_SYS_U16(GET_LE_U16(&dat[index]));
			SWD_SWDIO_SET();
			SWD_SWDIO_SETOUTPUT();
			SWD_SeqOut(&dat[index + 2], cur_dat_len);
			SWD_SWDIO_SETINPUT();

			break;
		case USB_TO_SWD_SEQIN:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			cur_dat_len = LE_TO_SYS_U16(GET_LE_U16(&dat[index]));
			SWD_SeqIn(&buffer_reply[rep_len], cur_dat_len);
			rep_len += (cur_dat_len + 7) >> 3;

			break;
		case USB_TO_SWD_Transact:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			swd_data = LE_TO_SYS_U32(GET_LE_U32(&dat[index + 1]));
			buffer_reply[rep_len] = SWD_Transaction(dat[index], &swd_data);
			SET_LE_U32(buffer_reply + rep_len + 1, swd_data);
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
