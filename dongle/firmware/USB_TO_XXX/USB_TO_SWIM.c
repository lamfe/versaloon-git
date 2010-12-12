/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_SWIM.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_SWIM                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_SWIM_EN

#include "USB_TO_XXX.h"
#include "interfaces.h"

void USB_TO_SWIM_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_idx, length;
	uint8 command;
	uint32 swim_addr;
	uint16 swim_len;

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
			if (ERROR_OK == interfaces->swim.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_CONFIG:
			if (ERROR_OK == interfaces->swim.config(device_idx, dat[index + 0], 
								dat[index + 1], dat[index + 2]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_FINI:
			if (ERROR_OK == interfaces->swim.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_OUT:
			swim_len = GET_LE_U16(&dat[index + 0]);
			swim_addr = GET_LE_U32(&dat[index + 2]);
			
			if (ERROR_OK == interfaces->swim.wotf(device_idx, &dat[index + 6], 
								swim_len, swim_addr))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_IN:
			swim_len = GET_LE_U16(&dat[index + 0]);
			swim_addr = GET_LE_U32(&dat[index + 2]);
			
			if (ERROR_OK == interfaces->swim.rotf(device_idx, &buffer_reply[rep_len + 1], 
								swim_len, swim_addr))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			rep_len += swim_len;
			break;
		case USB_TO_XXX_RESET:
			if (ERROR_OK == interfaces->swim.srst(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_SYNC:
			if (ERROR_OK == interfaces->swim.sync(device_idx, dat[index + 0]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_ENABLE:
			if (ERROR_OK == interfaces->swim.enable(device_idx))
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
