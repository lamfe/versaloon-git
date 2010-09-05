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
#include "fifo.h"

#include "USB_TO_XXX.h"

#if USB_TO_USART_EN

extern FIFO CDC_OUT_fifo;
extern FIFO CDC_IN_fifo;

uint8 USBTOUSART_En = 0;

void USB_TO_USART_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;
	uint32 data_len, i;
	uint32 baudrate;
	uint8 datalength, parity, stopbit;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_USART_NUM)
		{
			buffer_reply[rep_len++] = USB_TO_XXX_INVALID_INDEX;
			return;
		}
		length = GET_LE_U16(&dat[index + 1]);
		index += 3;

		switch(command)
		{
		case USB_TO_XXX_INIT:
			USBTOUSART_En = 1;
			buffer_reply[rep_len++] = USB_TO_XXX_OK;
			buffer_reply[rep_len++] = USB_TO_USART_NUM;

			break;
		case USB_TO_XXX_CONFIG:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			baudrate = GET_LE_U32(&dat[index]);
			datalength = dat[index + 4];
			parity = dat[index + 5];
			stopbit = dat[index + 6];

			FIFO_Reset(&CDC_OUT_fifo);
			FIFO_Reset(&CDC_IN_fifo);
			CDC_IF_Setup(baudrate, datalength, parity, stopbit);

			break;
		case USB_TO_XXX_FINI:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;
			
			CDC_IF_Fini();
			USBTOUSART_En = 0;

			break;
		case USB_TO_XXX_IN:
			data_len = GET_LE_U16(&dat[index]);

			if (FIFO_Get_Length(&CDC_IN_fifo) >= data_len)
			{
				if (data_len == FIFO_Get_Buffer(&CDC_IN_fifo, &buffer_reply[rep_len + 1], data_len))
				{
					buffer_reply[rep_len++] = USB_TO_XXX_OK;
					rep_len += data_len;
				}
				else
				{
					buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
				}
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}

			break;
		case USB_TO_XXX_OUT:
			data_len = GET_LE_U16(&dat[index]);
			
#if 1
			for (i = 0; i < data_len; i++)
			{
//				while (USART_GetFlagStatus(USART_DEF_PORT, USART_FLAG_TC) != SET);
				while (USART_GetFlagStatus(USART_DEF_PORT, USART_FLAG_TXE) != SET);
				USART_SendData(USART_DEF_PORT, dat[index + 2 + i]);
			}
			buffer_reply[rep_len++] = USB_TO_XXX_OK;
#else
			// check memory available
			if (FIFO_Get_AvailableLength(&CDC_OUT_fifo) >= data_len)
			{
				// copy to fifo
				if (data_len == FIFO_Add_Buffer(&CDC_OUT_fifo, &dat[index + 2], data_len))
				{
					buffer_reply[rep_len++] = USB_TO_XXX_OK;
				}
				else
				{
					buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
				}
			}
			else
			{
				buffer_reply[rep_len++] = USB_TO_XXX_FAILED;
			}
#endif
			break;
		case USB_TO_XXX_STATUS:
			buffer_reply[rep_len++] = USB_TO_XXX_OK;

			data_len = FIFO_Get_AvailableLength(&CDC_OUT_fifo);
			SET_LE_U32(&buffer_reply[rep_len], data_len);
			rep_len += sizeof(uint32);

			data_len = FIFO_Get_Length(&CDC_OUT_fifo);
			SET_LE_U32(&buffer_reply[rep_len], data_len);
			rep_len += sizeof(uint32);

			data_len = FIFO_Get_AvailableLength(&CDC_IN_fifo);
			SET_LE_U32(&buffer_reply[rep_len], data_len);
			rep_len += sizeof(uint32);

			data_len = FIFO_Get_Length(&CDC_IN_fifo);
			SET_LE_U32(&buffer_reply[rep_len], data_len);
			rep_len += sizeof(uint32);

			break;
		default:
			buffer_reply[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
