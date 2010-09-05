/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_MSP430_JTAG.c                                      *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_MSP430_JTAG                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_MSP430_JTAG_EN

#include "USB_TO_XXX.h"
#include "MSP430_JTAG.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_MSP430_JTAG_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint32 data, mask, value;
	uint16 len_tmp;
	uint8 byte_len, bit_len;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_MSP430_JTAG_NUM)
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
			buffer_reply[rep_len++] = USB_TO_MSP430_JTAG_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			MSP430_JTAG_Init(dat[index]);

			break;
		case USB_TO_XXX_FINI:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			MSP430_JTAG_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			break;
		case USB_TO_MSP430_JTAG_TCLK:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			MSP430_JTAG_TCLK(dat[index]);

			break;
		case USB_TO_MSP430_JTAG_IRDR:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			len_tmp = 0;
			while(len_tmp < length)
			{
				bit_len = dat[index + len_tmp + 0];
				byte_len = ((bit_len & 0x7F) + 7) >> 3;
				if(bit_len & 0x80)
				{
					// DR
					data = 0;
					memcpy((uint8*)&data, dat + index + len_tmp + 1, byte_len);
					value = MSP430_JTAG_DR(data, bit_len & 0x7F);
					memcpy(buffer_reply + rep_len, &value, byte_len);
					rep_len += byte_len;
					len_tmp += 1 + byte_len;
				}
				else
				{
					// IR
					buffer_reply[rep_len++] = MSP430_JTAG_IR(dat[index + len_tmp + 1], MSP430_JTAG_IR_LEN);
					len_tmp += 2;
				}
			}

			break;
		case USB_TO_MSP430_JTAG_Poll:
			len_tmp = GET_LE_U16(&dat[index + 1]);
			data = GET_LE_U32(&dat[index + 3]);
			mask = GET_LE_U32(&dat[index + 7]);
			value = GET_LE_U32(&dat[index + 11]);

			if(MSP430_JTAG_Poll(data, mask, value, dat[index], len_tmp))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}

			break;
		case USB_TO_MSP430_JTAG_TCLK_STROBE:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			MSP430_JTAG_TCLK_STROKE(GET_LE_U16(&dat[index]));

			break;
		case USB_TO_MSP430_JTAG_Reset:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			MSP430_JTAG_Reset();

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
