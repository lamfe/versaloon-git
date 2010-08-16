/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_JTAG_HL.c                                          *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_JTAG_HL                    *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_JTAG_HL_EN

#include "USB_TO_XXX.h"
#include "JTAG_TAP.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_JTAG_HL_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint16 cur_dat_len, i, len_tmp;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_JTAG_HL_NUM)
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
			buffer_reply[rep_len++] = USB_TO_JTAG_HL_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			JTAG_TAP_Init(dat[index + 0] | (dat[index + 1] << 8), JTAG_TAP_ASYN);
			JTAG_TAP_SetDaisyChainPos(dat[index + 2], dat[index + 3], dat[index + 4] + (dat[index + 5] << 8), dat[index + 6] + (dat[index + 7] << 8));

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_FINI:
			JTAG_TAP_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_JTAG_HL_IR_DR:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			len_tmp = 0;
			while(len_tmp < length)
			{
				i = dat[index + len_tmp + 0] + (dat[index + len_tmp + 1] << 8);		// in bit
				cur_dat_len = i & 0x7FFF;

				if(i & 0x8000)
				{
					JTAG_TAP_InstrPtr(dat + index + len_tmp + 3,		// intstr
									    buffer_reply + rep_len,			// tdo
										cur_dat_len,					// length_in_bits
										dat[index + len_tmp + 2]);		// idle
				}
				else
				{
					JTAG_TAP_DataPtr(dat + index + len_tmp + 3,			// tdi
									 buffer_reply + rep_len,			// tdo
									 cur_dat_len,						// length_in_bits
									 dat[index + len_tmp + 2]);			// idle
				}

				cur_dat_len = (cur_dat_len + 7) >> 3;
				rep_len += cur_dat_len;
				len_tmp += cur_dat_len + 3;
			}

			break;
		case USB_TO_JTAG_HL_TMS:
			cur_dat_len = dat[index + 0] + 1;							// in bit, at lease 1

			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			JTAG_TAP_TMS_Bit(&dat[index + 1], cur_dat_len);

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
