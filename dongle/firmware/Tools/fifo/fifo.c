/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
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
#include "fifo.h"

//#define FIFO_Get_Next_Idx(pos, size)		++(pos) % (size)
static uint32 FIFO_Get_Next_Idx(uint32 pos, uint32 size)
{
	pos++;
	if(pos >= size)
	{
		pos = 0;
	}

	return pos;
}

uint32 FIFO_Get_Length(FIFO *fifo)
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

uint32 FIFO_Add_Byte(FIFO *fifo, uint8 data)
{
	if ((fifo->size - FIFO_Get_Length(fifo)) > 1)
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

uint32 FIFO_Add_Buffer(FIFO *fifo, uint8 *src_buff, uint32 len)
{
	uint32 actual_len = len;

	if(actual_len >= (fifo->size - FIFO_Get_Length(fifo)))
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

uint32 FIFO_Get_Buffer(FIFO *fifo, uint8 *dest_buff, uint32 len)
{
	uint32 actual_len = len, avail_len = FIFO_Get_Length(fifo);

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
uint8 FIFO_Get_Byte(FIFO *fifo)
{
	uint8 byte;

	if(FIFO_Get_Length(fifo) > 0)
	{
		byte = fifo->buffer[fifo->tail];
		fifo->tail = FIFO_Get_Next_Idx(fifo->tail, fifo->size);

		return byte;
	}
	return 0;
}

uint32 FIFO_Get_Consequent_Buffer_Size(FIFO *fifo)
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

uint32 FIFO_Get_Consequent_Buffer(FIFO *fifo, uint8 **buff)
{
	*buff = (uint8*)(fifo->buffer + fifo->tail);
	return FIFO_Get_Consequent_Buffer_Size(fifo);
}

uint32 FIFO_Release_Consequent_Buffer(FIFO *fifo, uint32 size)
{
	uint32 actual_size = FIFO_Get_Consequent_Buffer_Size(fifo);

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
