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

#ifndef __BUFFER_H_INCLUDED__
#define __BUFFER_H_INCLUDED__

struct vsf_buffer_t
{
	uint8_t *buffer;
	uint32_t size;
};

struct vsf_transaction_buffer_t
{
	struct vsf_buffer_t buffer;
	uint32_t position;
};

// fifo
struct vsf_fifo_buffer_t
{
	struct vsf_buffer_t buffer;
	uint32_t head;
	uint32_t tail;
	uint32_t length;
};
RESULT vsf_fifo_buffer_init(struct vsf_fifo_buffer_t *fbuffer);
RESULT vsf_fifo_buffer_push(struct vsf_fifo_buffer_t *fbuffer, uint32_t size, 
							uint8_t *data);
RESULT vsf_fifo_buffer_pop(struct vsf_fifo_buffer_t *fbuffer, uint32_t size, 
							uint8_t *data);
RESULT vsf_fifo_buffer_peek(struct vsf_fifo_buffer_t *fbuffer, uint32_t size, 
							uint8_t *data);
RESULT vsf_fifo_buffer_peek_consequent(struct vsf_fifo_buffer_t *fbuffer, 
										uint32_t size, uint8_t *data);
RESULT vsf_fifo_buffer_get_data_length(struct vsf_fifo_buffer_t *fbuffer);
RESULT vsf_fifo_buffer_get_avail_length(struct vsf_fifo_buffer_t *fbuffer);

// multi_buffer
struct vsf_multi_buffer_t
{
	uint16_t count;
	uint32_t size;
	uint8_t **buffer_list;
	
	uint16_t head;
	uint16_t tail;
	uint16_t length;
};

RESULT vsf_multi_buffer_init(struct vsf_multi_buffer_t *mbuffer);
RESULT vsf_multi_buffer_get_empty(struct vsf_multi_buffer_t *mbuffer, 
									struct vsf_buffer_t *buffer);
RESULT vsf_multi_buffer_push(struct vsf_multi_buffer_t *mbuffer);
RESULT vsf_multi_buffer_get_payload(struct vsf_multi_buffer_t *mbuffer, 
									struct vsf_buffer_t *buffer);
RESULT vsf_multi_buffer_pop(struct vsf_multi_buffer_t *mbuffer);

// buffer_manager
struct vsf_bufmgr_t
{
	struct vsf_buffer_t buffer;
	uint16_t num_of_buffer;
};
void vsf_bufmgr_init(struct vsf_bufmgr_t *bufmgr);
void* vsf_bufmgr_malloc(struct vsf_bufmgr_t *bufmgr, uint32_t size);
void vsf_bufmgr_free(struct vsf_bufmgr_t *bufmgr, void *ptr);
void vsf_bufmgr_optimize(struct vsf_bufmgr_t *bufmgr);

#endif	// __BUFFER_H_INCLUDED__

