/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_C2.c                                               *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_C2                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_C2_EN

#include "USB_TO_XXX.h"
#include "C2.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_C2_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint8 tmp;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_C2_NUM)
		{
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_INDEX;
			return;
		}
		length = GET_LE_U16(&dat[index + 1]);
		index += 3;

		switch(command)
		{
		case USB_TO_XXX_INIT:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;
			buffer_reply[rep_len++] = USB_TO_C2_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			C2_Init();

			break;
		case USB_TO_XXX_FINI:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			C2_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			break;
		case USB_TO_C2_Data:
			tmp = dat[index + 0] & 0x07;
			if(dat[index + 0] & 0x80)
			{
				if(C2_ReadData(buffer_reply + rep_len + 1))
				{
					buffer_reply[rep_len] = USB_TO_XXX_FAILED;
				}
				else
				{
					buffer_reply[rep_len] = USB_TO_XXX_OK;
				}
				rep_len += 1 + tmp;
			}
			else
			{
				if(C2_WriteData(dat[index + 1]))
				{
					buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
				}
				else
				{
					buffer_reply[rep_len++] = USB_TO_XXX_OK;
				}
			}

			break;
		case USB_TO_C2_WriteAddr:
			C2_WriteAddr(dat[index]);
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_C2_ReadAddr:
			C2_ReadAddr(&buffer_reply[rep_len + 1]);
			buffer_reply[rep_len] = USB_TO_XXX_OK;
			rep_len += 2;

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_CMD;

			break;
		}
		index += length;
	}
}

#endif
