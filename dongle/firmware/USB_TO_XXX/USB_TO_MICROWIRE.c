/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_MICROWIRE.c                                        *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_MICROWIRE                  *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_MICROWIRE_EN

#include "USB_TO_XXX.h"
#include "interfaces.h"

void USB_TO_MICROWIRE_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, length;
	uint8 command, device_idx;
	
	uint8_t sel_polarity;
	uint16_t interval_us, retry_cnt;
	uint16_t opcode_bitlen, input_bitlen;
	uint16 frequency;
	
	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_idx = dat[index] & USB_TO_XXX_IDXMASK;
		length = GET_LE_U16(&dat[index + 1]);
		index += 3;
		
		switch(command)
		{
		case USB_TO_XXX_INIT:
			if (ERROR_OK == interfaces->microwire.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_CONFIG:
			sel_polarity = dat[index];
			frequency = GET_LE_U16(&dat[index + 1]);
			
			if (ERROR_OK == interfaces->microwire.config(device_idx, frequency, sel_polarity))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_FINI:
			if (ERROR_OK == interfaces->microwire.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_IN_OUT:
			opcode_bitlen = GET_LE_U16(&dat[index + 0]);
			input_bitlen = GET_LE_U16(&dat[index + 2]);
			
			if (ERROR_OK == interfaces->microwire.io(device_idx, &dat[index + 4], 
					opcode_bitlen, &buffer_reply[rep_len + 1], input_bitlen))
			{
				buffer_reply[rep_len] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len] = USB_TO_XXX_FAILED;
			}
			rep_len += 1 + ((input_bitlen + 7) / 8);
			break;
		case USB_TO_XXX_POLL:
			interval_us = GET_LE_U16(&dat[index + 0]);
			retry_cnt = GET_LE_U16(&dat[index + 2]);
			
			if (ERROR_OK == interfaces->microwire.poll(device_idx, interval_us, retry_cnt))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
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
