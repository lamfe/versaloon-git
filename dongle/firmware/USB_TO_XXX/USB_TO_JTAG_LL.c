/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_JTAG_LL.c                                          *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_JTAG_LL                    *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_JTAG_LL_EN

#include "USB_TO_XXX.h"
#include "JTAG_TAP.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_JTAG_LL_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint8 para;
	uint32 cur_dat_len, i;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_JTAG_LL_NUM)
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
			buffer_reply[rep_len++] = USB_TO_JTAG_LL_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			JTAG_TAP_Init(dat[index + 0] | (dat[index + 1] << 8), JTAG_TAP_ASYN);

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_FINI:
			JTAG_TAP_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_JTAG_LL_SCAN:
			cur_dat_len = dat[index + 0] + (dat[index + 1] << 8);
			para = cur_dat_len >> 15;
			cur_dat_len &= 0x7FFF;

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			JTAG_TAP_RW(buffer_reply + rep_len,						// tdo
						   &dat[index + 2 + para],					// tdi
						   dat[index + 2],							// tms_before
						   dat[index + 2 + cur_dat_len + para],		// tms_after0
						   dat[index + 2 + cur_dat_len + para + 1],	// tms_after1
						   cur_dat_len | ((uint16)para << 15));		// dat_byte_len
			rep_len += cur_dat_len;

			break;
		case USB_TO_JTAG_LL_TMS:
			cur_dat_len = length;									// in Byte

			for(i = 0; i < cur_dat_len; i++)
			{
				JTAG_TAP_WriteTMSByte_ASYN(dat[index + i]);
			}

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_JTAG_LL_TMS_CLOCKS:
			para = dat[index];
			cur_dat_len = (	(uint32)dat[index + 1] << 0) + 
						  (	(uint32)dat[index + 2] << 8) + 
						  (	(uint32)dat[index + 3] << 16) + 
						  (	(uint32)dat[index + 4] << 24);
			while(cur_dat_len--)
			{
				JTAG_TAP_WriteTMSByte_ASYN(para);
			}
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
