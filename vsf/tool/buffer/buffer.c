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

#include "app_type.h"
#include "compiler.h"

#include "buffer.h"

//#define vsf_fifo_get_next_index(pos, size)	(((pos) + 1) % (size))
static uint32_t vsf_fifo_get_next_index(uint32_t pos, uint32_t size)
{
	if (++pos >= size)
	{
		pos = 0;
	}
	return pos;
}

RESULT vsf_fifo_init(struct vsf_fifo_t *fifo)
{
#if __VSF_DEBUG__
	if (NULL == fifo)
	{
		return ERROR_FAIL;
	}
#endif
	
	fifo->head = fifo->tail = 0;
	return ERROR_OK;
}

uint32_t vsf_fifo_get_data_length(struct vsf_fifo_t *fifo)
{
#if __VSF_DEBUG__
	if (NULL == fifo)
	{
		return 0;
	}
#endif
	if (fifo->head >= fifo->tail)
	{
		return fifo->head - fifo->tail;
	}
	else
	{
		return fifo->buffer.size - (fifo->tail - fifo->head);
	}
}

uint32_t vsf_fifo_get_avail_length(struct vsf_fifo_t *fifo)
{
#if __VSF_DEBUG__
	if (NULL == fifo)
	{
		return 0;
	}
#endif
	return fifo->buffer.size - vsf_fifo_get_data_length(fifo);
}

uint32_t vsf_fifo_push8(struct vsf_fifo_t *fifo, uint8_t data)
{
	if (vsf_fifo_get_avail_length(fifo) <= 1)
	{
		return 0;
	}
	
	fifo->buffer.buffer[fifo->head] = data;
	fifo->head = vsf_fifo_get_next_index(fifo->head, fifo->buffer.size);
	return 1;
}

uint8_t vsf_fifo_pop8(struct vsf_fifo_t *fifo)
{
	uint8_t data;
	
	if (vsf_fifo_get_data_length(fifo) <= 0)
	{
		return 0;
	}
	
	data = fifo->buffer.buffer[fifo->tail];
	fifo->tail = vsf_fifo_get_next_index(fifo->tail, fifo->buffer.size);
	return data;
}

uint32_t vsf_fifo_push(struct vsf_fifo_t *fifo, uint32_t size, uint8_t *data)
{
	uint32_t tmp32;
	
#if __VSF_DEBUG__
	if ((NULL == fifo) || (NULL == data))
	{
		return 0;
	}
#endif
	if (size >= vsf_fifo_get_avail_length(fifo))
	{
		return 0;
	}
	
	tmp32 = fifo->buffer.size - fifo->head;
	if (size > tmp32)
	{
		memcpy(&fifo->buffer.buffer[fifo->head], &data[0], tmp32);
		memcpy(&fifo->buffer.buffer[0], &data[tmp32], size - tmp32);
		fifo->head = size - tmp32;
	}
	else
	{
		memcpy(&fifo->buffer.buffer[fifo->head], data, size);
		fifo->head += size;
		if (fifo->head == fifo->buffer.size)
		{
			fifo->head = 0;
		}
	}
	return size;
}

uint32_t vsf_fifo_peek_consequent(struct vsf_fifo_t *fifo, uint32_t size, 
								uint8_t *data)
{
	uint32_t tmp32;
	uint32_t avail_len = vsf_fifo_get_avail_length(fifo);
	
#if __VSF_DEBUG__
	if (NULL == fifo)
	{
		return 0;
	}
#endif
	if (size > avail_len)
	{
		size = avail_len;
	}
	
	tmp32 = fifo->buffer.size - fifo->tail;
	if (size > tmp32)
	{
		size = tmp32;
		memcpy(&data[0], &fifo->buffer.buffer[fifo->tail], tmp32);
	}
	else
	{
		memcpy(data, &fifo->buffer.buffer[fifo->tail], size);
	}
	return size;
}

uint32_t vsf_fifo_peek(struct vsf_fifo_t *fifo, uint32_t size, uint8_t *data)
{
	uint32_t tmp32;
	uint32_t avail_len = vsf_fifo_get_avail_length(fifo);
	
#if __VSF_DEBUG__
	if (NULL == fifo)
	{
		return 0;
	}
#endif
	if (size > avail_len)
	{
		size = avail_len;
	}
	
	tmp32 = fifo->buffer.size - fifo->tail;
	if (size > tmp32)
	{
		memcpy(&data[0], &fifo->buffer.buffer[fifo->tail], tmp32);
		memcpy(&data[tmp32], &fifo->buffer.buffer[0], size - tmp32);
	}
	else
	{
		memcpy(data, &fifo->buffer.buffer[fifo->tail], size);
	}
	return size;
}

uint32_t vsf_fifo_pop(struct vsf_fifo_t *fifo, uint32_t size, uint8_t *data)
{
	uint32_t tmp32;
	uint32_t ret = vsf_fifo_peek(fifo, size, data);
	
	if (!ret)
	{
		return 0;
	}
	
	tmp32 = fifo->buffer.size - fifo->tail;
	if (size > tmp32)
	{
		fifo->tail = size - tmp32;
	}
	else
	{
		fifo->tail += size;
		if (fifo->tail == fifo->buffer.size)
		{
			fifo->tail = 0;
		}
	}
	return ret;
}

RESULT vsf_multibuf_init(struct vsf_multibuf_t *mbuffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	
	mbuffer->tail = mbuffer->head = mbuffer->length = 0;
	return ERROR_OK;
}

RESULT vsf_multibuf_get_empty(struct vsf_multibuf_t *mbuffer, 
								struct vsf_buffer_t *buffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	if (mbuffer->count <= mbuffer->length)
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = mbuffer->buffer_list[mbuffer->head];
	buffer->size = mbuffer->size;
	return ERROR_OK;
}

RESULT vsf_multibuf_push(struct vsf_multibuf_t *mbuffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	if (mbuffer->count <= mbuffer->length)
	{
		return ERROR_FAIL;
	}
	
	mbuffer->head = vsf_fifo_get_next_index(mbuffer->head, mbuffer->count);
	mbuffer->length++;
	return ERROR_OK;
}

RESULT vsf_multibuf_get_payload(struct vsf_multibuf_t *mbuffer, 
								struct vsf_buffer_t *buffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	if (!mbuffer->length)
	{
		return ERROR_FAIL;
	}
	
	buffer->buffer = mbuffer->buffer_list[mbuffer->tail];
	buffer->size = mbuffer->size;
	return ERROR_OK;
}

RESULT vsf_multibuf_pop(struct vsf_multibuf_t *mbuffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	if (!mbuffer->length)
	{
		return ERROR_FAIL;
	}
	
	mbuffer->tail = vsf_fifo_get_next_index(mbuffer->tail, mbuffer->count);
	mbuffer->length--;
	return ERROR_OK;
}

struct vsf_bufmgr_record_t
{
	uint32_t size;
	uint8_t *ptr;
};

void vsf_bufmgr_init(struct vsf_bufmgr_t *bufmgr)
{
#if __VSF_DEBUG__
	if (NULL == bufmgr)
	{
		return ERROR_FAIL;
	}
#endif
	
	bufmgr->num_of_buffer = 0;
}

void* vsf_bufmgr_malloc(struct vsf_bufmgr_t *bufmgr, uint32_t size)
{
#if __VSF_DEBUG__
	if (NULL == bufmgr)
	{
		return ERROR_FAIL;
	}
#endif
	
	
}

void vsf_bufmgr_free(struct vsf_bufmgr_t *bufmgr, void *ptr)
{
#if __VSF_DEBUG__
	if (NULL == bufmgr)
	{
		return ERROR_FAIL;
	}
#endif
	
	
}
