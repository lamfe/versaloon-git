/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       fifo.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    fifo implementation file                                  *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#include "app_type.h"

#include "fifo.h"

//#define FIFO_Get_Next_Idx(pos, size)		++(pos) % (size)
static uint32_t FIFO_Get_Next_Idx(uint32_t pos, uint32_t size)
{
	pos++;
	if(pos >= size)
	{
		pos = 0;
	}

	return pos;
}

uint32_t FIFO_Get_AvailableLength(FIFO *fifo)
{
	return fifo->size - FIFO_Get_Length(fifo);
}

uint32_t FIFO_Get_Length(FIFO *fifo)
{
	if (fifo->header >= fifo->tail)
	{
		return (fifo->header - fifo->tail);
	}
	else
	{
		return (fifo->size - (fifo->tail - fifo->header));
	}
}

uint32_t FIFO_Add_Byte(FIFO *fifo, uint8_t data)
{
	if (FIFO_Get_AvailableLength(fifo) > 1)
	{
		fifo->buffer[fifo->header] = data;
		fifo->header = FIFO_Get_Next_Idx(fifo->header, fifo->size);
		return 1;
	}
	else
	{
		return 0;
	}
}

uint32_t FIFO_Add_Buffer(FIFO *fifo, uint8_t *src_buff, uint32_t len)
{
	uint32_t actual_len = len;

	if(actual_len >= FIFO_Get_AvailableLength(fifo))
	{
		return 0;
	}

	if(actual_len > (fifo->size - fifo->header))
	{
		memcpy(&fifo->buffer[fifo->header], &src_buff[0], fifo->size - fifo->header);
		memcpy(&fifo->buffer[0], &src_buff[fifo->size - fifo->header], actual_len - (fifo->size - fifo->header));
		fifo->header = actual_len - (fifo->size - fifo->header);
	}
	else
	{
		memcpy(&fifo->buffer[fifo->header], &src_buff[0], actual_len);
		fifo->header += actual_len;
		if(fifo->header == fifo->size)
		{
			fifo->header = 0;
		}
	}

	return actual_len;
}

uint32_t FIFO_Get_Buffer(FIFO *fifo, uint8_t *dest_buff, uint32_t len)
{
	uint32_t actual_len = len, avail_len = FIFO_Get_Length(fifo);

	if(actual_len > avail_len)
	{
		actual_len = avail_len;
	}

	if(actual_len > (fifo->size - fifo->tail))
	{
		memcpy(&dest_buff[0], &fifo->buffer[fifo->tail], fifo->size - fifo->tail);
		memcpy(&dest_buff[fifo->size - fifo->tail], &fifo->buffer[0], actual_len - (fifo->size - fifo->tail));
		fifo->tail = actual_len - (fifo->size - fifo->tail);
	}
	else
	{
		memcpy(&dest_buff[0], &fifo->buffer[fifo->tail], actual_len);
		fifo->tail += actual_len;
		if(fifo->tail == fifo->size)
		{
			fifo->tail = 0;
		}
	}

	return actual_len;
}

// make sure there is data in the buffer, if no data is in the buffer, 0 is returned
uint8_t FIFO_Get_Byte(FIFO *fifo)
{
	uint8_t byte;

	if(FIFO_Get_Length(fifo) > 0)
	{
		byte = fifo->buffer[fifo->tail];
		fifo->tail = FIFO_Get_Next_Idx(fifo->tail, fifo->size);

		return byte;
	}
	return 0;
}

uint32_t FIFO_Get_Consequent_Buffer_Size(FIFO *fifo)
{
	if (fifo->header >= fifo->tail)
	{
		return (fifo->header - fifo->tail);
	}
	else
	{
		return (fifo->size - fifo->tail);
	}
}

uint32_t FIFO_Get_Consequent_Buffer(FIFO *fifo, uint8_t **buff)
{
	*buff = (uint8_t*)(fifo->buffer + fifo->tail);
	return FIFO_Get_Consequent_Buffer_Size(fifo);
}

uint32_t FIFO_Release_Consequent_Buffer(FIFO *fifo, uint32_t size)
{
	uint32_t actual_size = FIFO_Get_Consequent_Buffer_Size(fifo);

	if (size < actual_size)
	{
		actual_size = size;
	}

	fifo->tail += actual_size;
	if(fifo->tail == fifo->size)
	{
		fifo->tail = 0;
	}

	return actual_size;
}

void FIFO_Reset(FIFO *fifo)
{
	fifo->header = fifo->tail = 0;
}
