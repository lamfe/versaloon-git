/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USART.c                                                   *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    USART interface implementation file                       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_USART_EN

#include "HW/HW.h"
#include "interfaces.h"
#include "fifo.h"

FIFO USART_OUT_fifo;
FIFO USART_IN_fifo;

RESULT usart_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		USART_IF_Fini();
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_config(uint8_t index, uint32_t baudrate, uint8_t datalength, 
					 char paritybit, char stopbit, char handshake)
{
	switch (index)
	{
	case 0:
		FIFO_Reset(&USART_OUT_fifo);
		FIFO_Reset(&USART_IN_fifo);
		USART_IF_Setup(baudrate, datalength, paritybit, stopbit);
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_send(uint8_t index, uint8_t *buf, uint16_t len)
{
	uint16_t i;

	switch (index)
	{
	case 0:
		if (NULL == buf)
		{
			return ERROR_FAIL;
		}
		
		for (i = 0; i < len; i++)
		{
//			while (USART_GetFlagStatus(USART_DEF_PORT, USART_FLAG_TC) != SET);
			while (USART_GetFlagStatus(USART_DEF_PORT, USART_FLAG_TXE) != SET);
			USART_SendData(USART_DEF_PORT, buf[i]);
		}
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_receive(uint8_t index, uint8_t *buf, uint16_t len)
{
	switch (index)
	{
	case 0:
		if (NULL == buf)
		{
			return ERROR_FAIL;
		}
		
		if (FIFO_Get_Length(&USART_IN_fifo) >= len)
		{
			if (len == FIFO_Get_Buffer(&USART_IN_fifo, buf, len))
			{
				return ERROR_OK;
			}
			else
			{
				return ERROR_FAIL;
			}
		}
		else
		{
			return ERROR_FAIL;
		}
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_status(uint8_t index, struct usart_status_t *status)
{
	switch (index)
	{
	case 0:
		if (NULL == status)
		{
			return ERROR_FAIL;
		}
		
		status->tx_buff_avail = FIFO_Get_AvailableLength(&USART_OUT_fifo);
		status->tx_buff_size = FIFO_Get_Length(&USART_OUT_fifo);
		status->rx_buff_avail = FIFO_Get_AvailableLength(&USART_IN_fifo);
		status->rx_buff_size = FIFO_Get_Length(&USART_IN_fifo);
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

#endif
