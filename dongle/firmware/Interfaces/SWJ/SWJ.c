/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWJ.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWJ interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_JTAG_EN

#include "SWJ.h"

uint8 SWJ_Trn = 1;
uint16 SWJ_Retry = 0;
uint16 SWJ_Delay = 0;

#define SWJ_Delay()		DelayUS(SWJ_Delay)

uint8 SWJ_SeqIn(uint8 *seq, uint16 num_of_bits)
{
	uint16 i;
	uint8 parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		SWJ_SWCLK_SET();
		SWJ_Delay();
		SWJ_SWCLK_CLR();
		if (SWJ_SWDIO_GET())
		{
			seq[i / 8] |= 1 << (i % 8);
			parity++;
		}
		else
		{
			seq[i / 8] &= ~(1 << (i % 8));
		}
		SWJ_Delay();
	}
	return parity & 1;
}

uint8 SWJ_SeqOut(uint8 *seq, uint16 num_of_bits)
{
	uint16 i;
	uint8 parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		if (seq[i / 8] & (1 << (i % 8)))
		{
			SWJ_SWDIO_SET();
			parity++;
		}
		else
		{
			SWJ_SWDIO_CLR();
		}
		SWJ_Delay();
		SWJ_SWCLK_SET();
		SWJ_Delay();
		SWJ_SWCLK_CLR();
	}

	return parity & 1;
}

void SWJ_StopClock(void)
{
	uint32 null = 0;

	// shift in at least 8 bits
	SWJ_SeqOut((uint8*)&null, 8);
}

uint8 SWJ_Transaction(uint8 request, uint32 *buff)
{
	uint32 reply;
	uint8 read = request & SWJ_TRANS_RnW, parity, data_parity;
	uint16 retry = 0;

SWJ_RETRY:
	// set swdio output to output request
	SWJ_SWDIO_SET();
	SWJ_SWDIO_SETOUTPUT();

	// send out request
	SWJ_SeqOut(&request, 8);

	// set swdio input to receive reply
	SWJ_SWDIO_SETINPUT();

	if (read)
	{
		// receive 3-bit reply
		SWJ_SeqIn((uint8*)&reply, 3);
		// receive data and parity
		parity = SWJ_SeqIn((uint8*)buff, 32);
		parity += SWJ_SeqIn((uint8*)&data_parity, 1);
		// trn
		SWJ_SeqIn((uint8*)&data_parity, SWJ_Trn);

		// set swdio output to output stop clock
		SWJ_SWDIO_SET();
		SWJ_SWDIO_SETOUTPUT();
	}
	else
	{
		// receive trn and 3-bit reply and then trn
		SWJ_SeqIn((uint8*)&reply, SWJ_Trn + 3);

		// set swdio output to output data
		SWJ_SWDIO_SET();
		SWJ_SWDIO_SETOUTPUT();

		// send data and parity
		parity = SWJ_SeqOut((uint8*)buff, 32);
		parity += SWJ_SeqOut(&parity, 1);
	}
	SWJ_StopClock();
	// set swdio input after clock is stopped
	SWJ_SWDIO_SETINPUT();
	reply &= 0x07; 
	switch (reply)
	{
	case SWJ_ACK_OK:
		if (parity & 1)
		{
			return SWJ_PARITY_ERROR | reply;
		}
		else
		{
			return SWJ_SUCCESS | reply;
		}
	case SWJ_ACK_WAIT:
		retry++;
		if (retry < SWJ_Retry)
		{
			goto SWJ_RETRY;
		}
		else
		{
			return SWJ_RETRY_OUT | reply;
		}
	case SWJ_ACK_FAULT:
		return SWJ_FAULT | reply;
	default:
		return SWJ_ACK_ERROR | reply;
	}
}

void SWJ_Init(void)
{
	SWJ_SWDIO_SETINPUT();
	SWJ_SWCLK_SET();
	SWJ_SWCLK_SETOUTPUT();
}

void SWJ_Fini(void)
{
	SWJ_SWDIO_SETINPUT();
	SWJ_SWCLK_SETINPUT();
}

void SWJ_SetDelay(uint16 dly)
{
	SWJ_Delay = dly;
}

void SWJ_SetTurnaround(uint8 cycles)
{
	if (cycles <= 14)
	{
		SWJ_Trn = cycles;
	}
}

void SWJ_SetRetryCount(uint16 retry)
{
	SWJ_Retry = retry;
}

#endif
