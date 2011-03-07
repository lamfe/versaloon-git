/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_PWM.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_PWM                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_PWM_EN

#include "USB_TO_XXX.h"
#include "interfaces.h"

void USB_TO_PWM_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, length;
	uint8 command, device_idx;
	
	uint16_t kHz, count;
	
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
			if (ERROR_OK == interfaces->pwm.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_CONFIG:
			kHz = GET_LE_U16(&dat[index]);
			
			if (ERROR_OK == interfaces->pwm.config(device_idx, kHz))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_FINI:
			if (ERROR_OK == interfaces->pwm.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_OUT:
			count = GET_LE_U16(&dat[index + 0]);
			
			if (ERROR_OK == interfaces->pwm.out(device_idx, count, (uint16_t *)&dat[index + 2]))
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
