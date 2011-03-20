/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_IIC.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_IIC                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_IIC_EN

#include "USB_TO_XXX.h"
#include "interfaces.h"

void USB_TO_IIC_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, length;
	uint8 command, device_idx;
	
	uint16 data_len;
	bool nacklast;
	
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
			if (ERROR_OK == interfaces->i2c.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_CONFIG:
			nacklast = dat[index + 6] > 0;
			if (ERROR_OK == interfaces->i2c.config(device_idx, 
								GET_LE_U16(&dat[index + 0]), 
								GET_LE_U16(&dat[index + 2]), 
								GET_LE_U16(&dat[index + 4]), 
								nacklast))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_FINI:
			if (ERROR_OK == interfaces->i2c.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_IIC_Read:
			data_len = GET_LE_U16(&dat[index + 1]);
			
			if (ERROR_OK == interfaces->i2c.read(device_idx, dat[index + 0], 
								&buffer_reply[rep_len + 1], data_len, dat[index + 3]))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			rep_len += data_len;
			break;
		case USB_TO_IIC_Write:
			if (ERROR_OK == interfaces->i2c.write(device_idx, dat[index + 0], 
								&dat[index + 4], GET_LE_U16(&dat[index + 1]), 
								dat[index + 3]))
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
