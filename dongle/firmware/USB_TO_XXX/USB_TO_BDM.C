/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_BDM.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_BDM                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_BDM_EN

#include "USB_TO_XXX.h"
#include "BDM.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_BDM_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;
	uint16 token;
	uint16 processed_len;
	uint16 reply_ack_pos;
	uint8 err;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_BDM_NUM)
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
			buffer_reply[rep_len++] = USB_TO_BDM_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			BDM_Init();

			break;
		case USB_TO_XXX_FINI:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			BDM_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			break;
		case USB_TO_BDM_TRANSACT:
			reply_ack_pos = rep_len++;

			err = 0;
			processed_len = 0;
			while (processed_len < length)
			{
				token = LE_TO_SYS_U16(GET_LE_U16(&dat[index + processed_len]));
				processed_len += 2;
				if (BDM_Transact(token, &dat[index + processed_len], 
						&buffer_reply[rep_len]))
				{
					err = 1;
					break;
				}
				processed_len += BDM_OUT_LEN(token);
				rep_len += BDM_IN_LEN(token);
			}

			if (err)
			{
				buffer_reply[reply_ack_pos] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[reply_ack_pos] = USB_TO_XXX_OK;
			}

			break;
		case USB_TO_XXX_SYNC:
			if (BDM_Sync(&processed_len))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				SET_LE_U16(&buffer_reply[rep_len], processed_len);
				rep_len += sizeof(uint16);
			}
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_CMD;

			break;
		}
		index += length;
	}
}

#endif
