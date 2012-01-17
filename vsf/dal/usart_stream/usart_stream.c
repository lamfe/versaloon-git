/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include "app_cfg.h"
#include "app_type.h"

#include "tool/buffer/buffer.h"

#include "interfaces.h"
#include "usart_stream.h"

static void usart_stream_onrx(void *p, uint16_t data)
{
	struct usart_stream_info_t *usart_stream = (struct usart_stream_info_t *)p;
	
	if (vsf_fifo_push8(&usart_stream->fifo_rx, (uint8_t)data) != 1)
	{
		usart_stream->overflow = true;
	}
}

vsf_err_t usart_stream_init(struct usart_stream_info_t *usart_stream)
{
	usart_stream->overflow = false;
	vsf_fifo_init(&usart_stream->fifo_tx);
	vsf_fifo_init(&usart_stream->fifo_rx);
	if (usart_stream->usart_index != IFS_DUMMY_PORT)
	{
		return core_interfaces.usart.init(usart_stream->usart_index);
	}
	return VSFERR_NONE;
}

vsf_err_t usart_stream_fini(struct usart_stream_info_t *usart_stream)
{
	if (usart_stream->usart_index != IFS_DUMMY_PORT)
	{
		return core_interfaces.usart.fini(usart_stream->usart_index);
	}
	return VSFERR_NONE;
}

vsf_err_t usart_stream_config(struct usart_stream_info_t *usart_stream)
{
	if (usart_stream->usart_index != IFS_DUMMY_PORT)
	{
		core_interfaces.usart.config(usart_stream->usart_index, 
										usart_stream->usart_info.baudrate,
										usart_stream->usart_info.datalength, 
										usart_stream->usart_info.mode);
		return core_interfaces.usart.config_callback(usart_stream->usart_index,
					(void *)usart_stream, NULL, usart_stream_onrx);
	}
	return VSFERR_NONE;
}

vsf_err_t usart_stream_rx(struct usart_stream_info_t *usart_stream, 
						struct vsf_buffer_t *buffer)
{
	uint32_t rx_size;
	
	rx_size = vsf_fifo_pop(&usart_stream->fifo_rx, buffer->size, 
							buffer->buffer);
	buffer->size = rx_size;
	return VSFERR_NONE;
}

vsf_err_t usart_stream_tx(struct usart_stream_info_t *usart_stream, 
						struct vsf_buffer_t *buffer)
{
	uint32_t tx_size;
	
	tx_size = vsf_fifo_push(&usart_stream->fifo_tx, buffer->size, 
							buffer->buffer);
	if (tx_size != buffer->size)
	{
		buffer->size = tx_size;
	}
	return VSFERR_NONE;
}

vsf_err_t usart_stream_poll(struct usart_stream_info_t *usart_stream)
{
	if ((usart_stream->usart_index != IFS_DUMMY_PORT) &&
		!core_interfaces.usart.tx_isready(usart_stream->usart_index) && 
		vsf_fifo_get_data_length(&usart_stream->fifo_tx))
	{
		return core_interfaces.usart.tx(usart_stream->usart_index, 
							(uint16_t)vsf_fifo_pop8(&usart_stream->fifo_tx));
	}
	return VSFERR_NONE;
}
