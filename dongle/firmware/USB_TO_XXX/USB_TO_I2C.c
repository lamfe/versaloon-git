/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_I2C.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_I2C                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_I2C_EN

#include "USB_TO_XXX.h"
#include "I2C.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_I2C_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint16 result, data_len, result_index;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_I2C_NUM)
		{
			buffer_out[rep_len++] = USB_TO_XXX_INVALID_INDEX;
			return;
		}
		length = dat[index + 1] + (dat[index + 2] << 8);
		index += 3;

		switch(command)
		{
		case USB_TO_XXX_INIT:
			buffer_out[rep_len++] = USB_TO_XXX_OK;
			buffer_out[rep_len++] = USB_TO_I2C_NUM;

			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			if(I2C_Init(dat[index + 0] + (dat[index + 1] << 8)) == 0)
			{
				buffer_out[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_out[rep_len++] = USB_TO_XXX_FAILED;
			}

			break;
		case USB_TO_XXX_FINI:
			buffer_out[rep_len++] = USB_TO_XXX_OK;
			I2C_Fini();

			break;
		case USB_TO_I2C_Read:
			result_index = rep_len;
			data_len = dat[index + 1] + (dat[index + 2] << 8);
			result = I2C_Read(	dat[index + 0],							// chip_addr
								&buffer_out[rep_len + 1],				// data
								data_len,								// data_len
								dat[index + 3]);						// stop

			if(result == data_len)
			{
				buffer_out[result_index] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_out[result_index] = USB_TO_XXX_FAILED;
			}
			rep_len += data_len + 1;

			break;
		case USB_TO_I2C_Write:
			result_index = rep_len;
			data_len = dat[index + 1] + (dat[index + 2] << 8);
			result = I2C_Write(	dat[index + 0],							// chip_addr
								&dat[index + 4],						// data
								data_len,								// data_len
								dat[index + 3]);						// stop

			if(result == data_len)
			{
				buffer_out[result_index] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_out[result_index] = USB_TO_XXX_FAILED;
			}
			rep_len += 1;

			break;
		default:
			buffer_out[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
