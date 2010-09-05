/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
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
	uint32 num_of_bits, num_of_databyte;
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
		length = LE_TO_SYS_U16(GET_LE_U16(&dat[index + 1]));
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
			JTAG_TAP_Init(LE_TO_SYS_U16(GET_LE_U16(&dat[index])), JTAG_TAP_RAW);

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_FINI:
			JTAG_TAP_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_IN_OUT:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			num_of_bits = LE_TO_SYS_U32(GET_LE_U32(&dat[index]));
			num_of_databyte = ((num_of_bits + 7) >> 3);

			JTAG_TAP_Operate_RAW(num_of_bits, &dat[index + 4], 
					&dat[index + 4 + num_of_databyte], &buffer_reply[rep_len]);

			rep_len += num_of_databyte;

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
