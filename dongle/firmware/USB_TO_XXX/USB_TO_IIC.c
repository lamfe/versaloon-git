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
#include "IIC.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

void USB_TO_IIC_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint16 result, data_len, result_index, actual_len;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_IIC_NUM)
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
			buffer_reply[rep_len++] = USB_TO_IIC_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			if(IIC_Init(GET_LE_U16(&dat[index + 0]), 
						GET_LE_U16(&dat[index + 2]), 
						GET_LE_U16(&dat[index + 4])) == 0)
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}

			break;
		case USB_TO_XXX_FINI:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			IIC_Fini();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			break;
		case USB_TO_IIC_Read:
			result_index = rep_len;
			data_len = GET_LE_U16(&dat[index + 1]);
			result = IIC_Read(	dat[index + 0],							// chip_addr
								&buffer_reply[rep_len + 1],				// data
								data_len,								// data_len
								dat[index + 3], 						// stop
								&actual_len);

			buffer_reply[result_index] = result;
			rep_len += data_len + 1;

			break;
		case USB_TO_IIC_Write:
			result_index = rep_len;
			data_len = GET_LE_U16(&dat[index + 1]);
			result = IIC_Write(	dat[index + 0],							// chip_addr
								&dat[index + 4],						// data
								data_len,								// data_len
								dat[index + 3], 						// stop
								&actual_len);

			buffer_reply[result_index] = result;
			rep_len += 1;

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
