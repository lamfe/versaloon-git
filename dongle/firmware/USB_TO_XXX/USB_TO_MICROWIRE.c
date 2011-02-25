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
	uint16 frequency;
	
	uint16_t cmd_offset, reply_offset;
	uint8_t opcode_bitlen, addr_bitlen, data_bitlen, reply_bitlen;
	uint8_t reply_bytelen;
	uint32_t opcode = 0, addr = 0, data = 0, reply;
	bool result;
	
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
			reply_offset = cmd_offset = 0;
			result = true;
			
			while (cmd_offset < length)
			{
				opcode_bitlen = dat[index + cmd_offset++];
				addr_bitlen = dat[index + cmd_offset++];
				data_bitlen = dat[index + cmd_offset++];
				reply_bitlen = dat[index + cmd_offset++];
				
				if (opcode_bitlen)
				{
					opcode = GET_LE_U32(&dat[index + cmd_offset]);
					cmd_offset += 4;
				}
				if (addr_bitlen)
				{
					addr = GET_LE_U32(&dat[index + cmd_offset]);
					cmd_offset += 4;
				}
				if (data_bitlen)
				{
					data = GET_LE_U32(&dat[index + cmd_offset]);
					cmd_offset += 4;
				}
				
				reply = 0;
				if (ERROR_OK != interfaces->microwire.transport(device_idx, 
						opcode, opcode_bitlen, addr, addr_bitlen, data, data_bitlen, 
						(uint8_t *)&reply, reply_bitlen))
				{
					result = false;
					break;
				}
				reply_bytelen = (reply_bitlen + 7) / 8;
				memcpy(&buffer_reply[rep_len + 1 + reply_offset], &reply, reply_bytelen);
				reply_offset += reply_bytelen;
			}
			if (result)
			{
				buffer_reply[rep_len] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len] = USB_TO_XXX_FAILED;
			}
			rep_len += 1 + reply_offset;
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
