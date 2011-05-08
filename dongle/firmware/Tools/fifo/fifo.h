/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       fifo.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    fifo header file                                          *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

typedef struct
{
	uint8_t *buffer;
	uint32_t size;
	uint32_t header;
	uint32_t tail;
}FIFO;

#define FIFO_Init(buff)			{(buff), sizeof(buff), 0, 0}

void FIFO_Reset(FIFO *fifo);
uint32_t FIFO_Add_Buffer(FIFO *fifo, uint8_t *src_buff, uint32_t len);
uint32_t FIFO_Get_Buffer(FIFO *fifo, uint8_t *dest_buff, uint32_t len);
uint32_t FIFO_Get_Length(FIFO *fifo);
uint32_t FIFO_Get_AvailableLength(FIFO *fifo);
// make sure there is data in the buffer than call this, if no data is in the buffer, 0 is returned
uint32_t FIFO_Add_Byte(FIFO *fifo, uint8_t data);
uint8_t FIFO_Get_Byte(FIFO *fifo);
uint32_t FIFO_Get_Consequent_Buffer(FIFO *fifo, uint8_t **buff);
uint32_t FIFO_Release_Consequent_Buffer(FIFO *fifo, uint32_t size);
