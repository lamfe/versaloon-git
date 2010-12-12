/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_BDM.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_BDM                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_BDM_EN

#include "USB_TO_XXX.h"
#include "interfaces.h"
#include "BDM.h"

void USB_TO_BDM_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_idx, length;
	uint8 command;
	uint16 token;
	uint16 processed_len;
	uint16 rindex;
	bool fail;
	
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
			if (ERROR_OK == interfaces->bdm.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_XXX_FINI:
			if (ERROR_OK == interfaces->bdm.fini(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			break;
		case USB_TO_BDM_TRANSACT:
			rindex = rep_len++;
			fail = false;
			processed_len = 0;
			
			while (processed_len < length)
			{
				token = GET_LE_U16(&dat[index + processed_len]);
				processed_len += 2;
				
				if (ERROR_OK == interfaces->bdm.transact(device_idx, 
									&dat[index + processed_len], BDM_OUT_LEN(token), 
									&buffer_reply[rep_len], BDM_IN_LEN(token), 
									BDM_OUT_DLY_CNT(token), BDM_ACK(token)))
				{
					
				}
				else
				{
					fail = true;
					break;
				}
				processed_len += BDM_OUT_LEN(token);
				rep_len += BDM_IN_LEN(token);
			}
			if (fail)
			{
				buffer_reply[rindex] = USB_TO_XXX_FAILED;
			}
			else
			{
				buffer_reply[rindex] = USB_TO_XXX_OK;
			}
			break;
		case USB_TO_XXX_SYNC:
			if (ERROR_OK == interfaces->bdm.sync(device_idx, &processed_len))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				SET_LE_U16(&buffer_reply[rep_len], processed_len);
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			rep_len += 2;
			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_CMD;
			break;
		}
		index += length;
	}
}

#endif
