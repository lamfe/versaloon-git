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
#include "buffer.h"

RESULT vsf_fifo_buffer_init(struct vsf_fifo_buffer_t *fbuffer)
{
#if __VSF_DEBUG__
	if (NULL == fbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	fbuffer->head = fbuffer->tail = fbuffer->length = 0;
	return ERROR_OK;
}

RESULT vsf_fifo_buffer_push(struct vsf_fifo_buffer_t *fbuffer, uint32_t size, 
							uint8_t *data)
{
#if __VSF_DEBUG__
	if (NULL == fbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	if (fbuffer->buffer.size < (fbuffer->length + size))
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT vsf_fifo_buffer_pop(struct vsf_fifo_buffer_t *fbuffer, uint32_t size, 
							uint8_t *data)
{
#if __VSF_DEBUG__
	if (NULL == fbuffer)
	{
		return ERROR_FAIL;
	}
#endif
}

RESULT vsf_fifo_buffer_peek(struct vsf_fifo_buffer_t *fbuffer, uint32_t size, 
							uint8_t *data)
{
#if __VSF_DEBUG__
	if (NULL == fbuffer)
	{
		return ERROR_FAIL;
	}
#endif
}

RESULT vsf_fifo_buffer_peek_consequent(struct vsf_fifo_buffer_t *fbuffer, 
										uint32_t size, uint8_t *data)
{
#if __VSF_DEBUG__
	if (NULL == fbuffer)
	{
		return ERROR_FAIL;
	}
#endif
}

RESULT vsf_fifo_buffer_get_data_length(struct vsf_fifo_buffer_t *fbuffer)
{
#if __VSF_DEBUG__
	if (NULL == fbuffer)
	{
		return ERROR_FAIL;
	}
#endif
}

RESULT vsf_fifo_buffer_get_avail_length(struct vsf_fifo_buffer_t *fbuffer)
{
#if __VSF_DEBUG__
	if (NULL == fbuffer)
	{
		return ERROR_FAIL;
	}
#endif
}

RESULT vsf_multi_buffer_init(struct vsf_multi_buffer_t *mbuffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	
	mbuffer->tail = mbuffer->count - 1;
	mbuffer->head = 0;
	return ERROR_OK;
}

RESULT vsf_multi_buffer_get_empty(struct vsf_multi_buffer_t *mbuffer, 
									struct vsf_buffer_t *buffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	
	if (mbuffer->head == mbuffer->tail)
	{
		return ERROR_FAIL;
	}
	buffer->buffer = mbuffer->buffer_list[mbuffer->head];
	buffer->size = mbuffer->size;
	return ERROR_OK;
}

RESULT vsf_multi_buffer_push(struct vsf_multi_buffer_t *mbuffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	
	if (mbuffer->head == mbuffer->tail)
	{
		return ERROR_FAIL;
	}
	mbuffer->head = (mbuffer->head + 1) % mbuffer->count;
	return ERROR_OK;
}

RESULT vsf_multi_buffer_get_payload(struct vsf_multi_buffer_t *mbuffer, 
									struct vsf_buffer_t *buffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	
	if (mbuffer->head == ((mbuffer->head + 1) % mbuffer->count))
	{
		return ERROR_FAIL;
	}
}

RESULT vsf_multi_buffer_pop(struct vsf_multi_buffer_t *mbuffer)
{
#if __VSF_DEBUG__
	if (NULL == mbuffer)
	{
		return ERROR_FAIL;
	}
#endif
	
	
}

void vsf_bufmgr_init(struct vsf_bufmgr_t *bufmgr)
{
#if __VSF_DEBUG__
	if (NULL == bufmgr)
	{
		return ERROR_FAIL;
	}
#endif
	
	
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

void vsf_bufmgr_optimize(struct vsf_bufmgr_t *bufmgr)
{
#if __VSF_DEBUG__
	if (NULL == bufmgr)
	{
		return ERROR_FAIL;
	}
#endif
	
	
}

