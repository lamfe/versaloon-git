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

#include "app_interfaces.h"
#include "tool/buffer/buffer.h"
#include "dal/usart_stream/usart_stream.h"

#include "USART.h"

extern uint8_t asyn_rx_buf[ASYN_DATA_BUFF_SIZE];
struct usart_stream_info_t usart_stream_p0 = 
{
	0,								// usart_index
	{{asyn_rx_buf, 1024}},			// fifo_tx
	{{asyn_rx_buf + 1024, 1024}}	// fifo_rx
};

RESULT usart_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return usart_stream_init(&usart_stream_p0);
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return usart_stream_fini(&usart_stream_p0);
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_config(uint8_t index, uint32_t baudrate, uint8_t datalength, 
					char paritybit, char stopbit, char handshake)
{
	REFERENCE_PARAMETER(handshake);
	
	switch (index)
	{
	case 0:
		usart_stream_p0.usart_info.datalength = datalength;
		usart_stream_p0.usart_info.baudrate = baudrate;
		usart_stream_p0.usart_info.mode = 0;
		switch(stopbit)
		{
		default:
		case 0:
			usart_stream_p0.usart_info.mode |= USART_STOPBITS_1;
			break;
		case 1:
			usart_stream_p0.usart_info.mode |= USART_STOPBITS_1P5;
			break;
		case 2:
			usart_stream_p0.usart_info.mode |= USART_STOPBITS_2;
			break;
		}
		switch(paritybit)
		{
		default:
		case 0:
			usart_stream_p0.usart_info.mode |= USART_PARITY_NONE;
			usart_stream_p0.usart_info.datalength = 8;
			break;
		case 1:
			usart_stream_p0.usart_info.mode |= USART_PARITY_ODD;
			usart_stream_p0.usart_info.datalength = 9;
			break;
		case 2:
			usart_stream_p0.usart_info.mode |= USART_PARITY_EVEN;
			usart_stream_p0.usart_info.datalength = 9;
			break;
		}
		return usart_stream_config(&usart_stream_p0);
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_send(uint8_t index, uint8_t *buf, uint16_t len)
{
	struct vsf_buffer_t buffer;
	
	switch (index)
	{
	case 0:
		if (NULL == buf)
		{
			return ERROR_FAIL;
		}
		
		buffer.buffer = buf;
		buffer.size = len;
		return usart_stream_tx(&usart_stream_p0, &buffer);
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_receive(uint8_t index, uint8_t *buf, uint16_t len)
{
	struct vsf_buffer_t buffer;
	
	switch (index)
	{
	case 0:
		if (NULL == buf)
		{
			return ERROR_FAIL;
		}
		
		buffer.buffer = buf;
		buffer.size = len;
		return usart_stream_rx(&usart_stream_p0, &buffer);
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_status(uint8_t index, struct usart_status_t *status)
{
	struct vsf_fifo_t *fifo_tx, *fifo_rx;
	
	switch (index)
	{
	case 0:
		if (NULL == status)
		{
			return ERROR_FAIL;
		}
		
		fifo_tx = &usart_stream_p0.fifo_tx;
		fifo_rx = &usart_stream_p0.fifo_rx;
		status->tx_buff_avail = vsf_fifo_get_avail_length(fifo_tx);
		status->tx_buff_size = vsf_fifo_get_data_length(fifo_tx);
		status->rx_buff_avail = vsf_fifo_get_avail_length(fifo_rx);
		status->rx_buff_size = vsf_fifo_get_data_length(fifo_rx);
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT usart_poll(uint8_t index)
{
	switch (index)
	{
	case 0:
		return usart_stream_poll(&usart_stream_p0);
	default:
		return ERROR_FAIL;
	}
}

#endif
