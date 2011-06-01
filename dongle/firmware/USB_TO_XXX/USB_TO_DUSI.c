/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_DUSI.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_DUSI                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_DUSI_EN

#include "USB_TO_XXX.h"
#include "interfaces.h"

void USB_TO_DUSI_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint16_t index, length;
	uint8_t command, device_idx;
	
	uint8_t attr;
	uint32_t frequency;
	uint16_t bitlen, bytelen;
	uint32_t recvpos;
	
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
			if (ERROR_OK == interfaces->dusi.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_CONFIG:
			attr = dat[index];
			frequency = GET_LE_U32(&dat[index + 1]);
			index += 3;
			
			if (ERROR_OK == interfaces->dusi.config(device_idx, frequency, 
											attr & USB_TO_DUSI_CPOL_MASK, 
											attr & USB_TO_DUSI_CPHA_MASK, 
											attr & USB_TO_DUSI_FIRSTBIT_MASK))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_FINI:
			if (ERROR_OK == interfaces->dusi.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_IN_OUT:
			bitlen = GET_LE_U16(&dat[index + 0]);
			bytelen = (bitlen + 7) / 8;
			recvpos = rep_len + 1;
			
			if (ERROR_OK == interfaces->dusi.io(device_idx, 
					&dat[index + 2], 			&buffer_reply[recvpos], 
					&dat[index + 2 + bytelen], 	&buffer_reply[recvpos + bytelen], 
					bitlen))
			{
				buffer_reply[rep_len] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len] = USB_TO_XXX_FAILED;
			}
			rep_len += 1 + bytelen * 2;
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;
			break;
		}
		index += length;
	}
}

#endif
