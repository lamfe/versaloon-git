/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_USART.c                                            *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_USART                      *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "USB_TO_XXX.h"
#include "interfaces.h"

#if USB_TO_USART_EN

void USB_TO_USART_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_idx, length;
	uint8 command;
	uint32 data_len;
	uint32 baudrate;
	uint8 datalength, paritybit, stopbit;
	struct usart_status_t status;

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
			if (ERROR_OK == interfaces->usart.init(device_idx))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}

			break;
		case USB_TO_XXX_CONFIG:
			baudrate = GET_LE_U32(&dat[index]);
			datalength = dat[index + 4];
			paritybit = dat[index + 5];
			stopbit = dat[index + 6];
			if (ERROR_OK == 
					interfaces->usart.config(device_idx, baudrate, datalength, 
												paritybit, stopbit, 0))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}

			break;
		case USB_TO_XXX_FINI:
			if (ERROR_OK == interfaces->usart.fini(device_idx))
			{
				
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}

			break;
		case USB_TO_XXX_IN:
			data_len = GET_LE_U16(&dat[index]);

			if (ERROR_OK == 
					interfaces->usart.receive(device_idx, &buffer_reply[rep_len + 1], data_len))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
				rep_len += data_len;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}

			break;
		case USB_TO_XXX_OUT:
			data_len = GET_LE_U16(&dat[index]);

			if (ERROR_OK == 
					interfaces->usart.send(device_idx, &dat[index + 2], data_len))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}

			break;
		case USB_TO_XXX_STATUS:
			if (ERROR_OK == interfaces->usart.status(device_idx, &status))
			{
				buffer_reply[rep_len++] = USB_TO_XXX_OK;

				SET_LE_U32(&buffer_reply[rep_len +  0], status.tx_buff_avail);
				SET_LE_U32(&buffer_reply[rep_len +  4], status.tx_buff_size);
				SET_LE_U32(&buffer_reply[rep_len +  8], status.rx_buff_avail);
				SET_LE_U32(&buffer_reply[rep_len + 12], status.rx_buff_size);
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
			rep_len += 12;

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
