/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_ISSP.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_ISSP                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_ISSP_EN

#include "USB_TO_XXX.h"
#include "ISSP.h"

void USB_TO_ISSP_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint8 para;
	uint16 vector_num, i;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_ISSP_NUM)
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
			buffer_out[rep_len++] = USB_TO_ISSP_NUM;

			break;
		case USB_TO_XXX_CONFIG:
			ISSP_Init();

			buffer_out[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_FINI:
			ISSP_Fini();

			buffer_out[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_ISSP_Vector:
			if(length % 3 != 0)
			{
				buffer_out[rep_len++] = USB_TO_XXX_INVALID_PARA;
				return;
			}
			vector_num = length / 3;

			buffer_out[rep_len++] = USB_TO_XXX_OK;
			for(i = 0; i < vector_num; i++)
			{
				para = dat[index + i * 3];
				if(para & USB_TO_ISSP_ATTR_0s)
				{
					ISSP_Vector_0s();
				}
				else
				{
					if(para & USB_TO_ISSP_ATTR_READ)
					{
						// Read
						buffer_out[rep_len++] = 
							ISSP_Vector(para & USB_TO_ISSP_ATTR_BANK,
										dat[index + i * 3 + 1],
										dat[index + i * 3 + 2],
										1,
										para & USB_TO_ISSP_ATTR_APPENDBIT);
					}
					else
					{
						// Write
						ISSP_Vector(para & USB_TO_ISSP_ATTR_BANK,
									dat[index + i * 3 + 1],
									dat[index + i * 3 + 2],
									0,
									para & USB_TO_ISSP_ATTR_APPENDBIT);
					}
				}
			}

			break;
		case USB_TO_ISSP_EnterProgMode:
			if((dat[index] == ISSP_PM_POWER_ON) && (Vtarget > TVCC_SAMPLE_MIN_POWER))
			{
				// No power should be on the target if using power-on mode
				buffer_out[rep_len++] = USB_TO_XXX_FAILED;
			}
			else
			{
				ISSP_EnterProgMode(dat[index]);
				buffer_out[rep_len++] = USB_TO_XXX_OK;
			}

			break;
		case USB_TO_ISSP_LeaveProgMode:
			ISSP_LeaveProgMode(dat[index]);
			buffer_out[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_ISSP_WaitAndPoll:
			if(ISSP_WaitAndPoll() == ISSP_WAP_OK)
			{
				buffer_out[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_out[rep_len++] = USB_TO_XXX_FAILED;
			}

			break;
		default:
			buffer_out[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
