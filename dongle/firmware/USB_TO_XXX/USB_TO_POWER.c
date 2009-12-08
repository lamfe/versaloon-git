/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_POWER.c                                            *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_POWER                      *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_POWER_EN

#include "USB_TO_XXX.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

uint8 power_state = 0;

void USB_TO_POWER_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length, voltage;
	uint8 command;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_POWER_NUM)
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
			buffer_reply[rep_len++] = USB_TO_POWER_NUM;

			break;
		case USB_TO_XXX_CONFIG:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;
			// no need to configure in this implementation for they are already configured

			break;
		case USB_TO_XXX_FINI:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_OUT:
			voltage = dat[index + 0] + (dat[index + 1] << 8);
			if (!PWREXT_GetState())
			{
				power_state = 0;
			}

			if(voltage == 3300)
			{
				// only support 3.3V
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				if (!power_state)
				{
					power_state++;
					PWREXT_Acquire();
				}
			}
			else if(voltage == 0)
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				if (power_state)
				{
					power_state--;
					PWREXT_Release();
				}
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
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
