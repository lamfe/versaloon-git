/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SWD.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SWD interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_JTAG_EN

#include "SWD.h"

uint8 SWD_Trn = 1;
uint16 SWD_Retry = 0;
uint16 SWD_Delay = 0;

#define SWD_Delay()		DelayUS(SWD_Delay)

uint8 (*SWD_SeqIn)(uint8 *seq, uint16 num_of_bits);
uint8 (*SWD_SeqOut)(uint8 *seq, uint16 num_of_bits);

uint8 SWD_SeqIn_NoDelay(uint8 *seq, uint16 num_of_bits)
{
	uint16 i;
	uint8 parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		SWD_SWCLK_SET();
		SWD_SWCLK_CLR();
		if (SWD_SWDIO_GET())
		{
			seq[i / 8] |= 1 << (i % 8);
			parity++;
		}
		else
		{
			seq[i / 8] &= ~(1 << (i % 8));
		}
	}
	return parity & 1;
}

uint8 SWD_SeqOut_NoDelay(uint8 *seq, uint16 num_of_bits)
{
	uint16 i;
	uint8 parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		if (seq[i / 8] & (1 << (i % 8)))
		{
			SWD_SWDIO_SET();
			parity++;
		}
		else
		{
			SWD_SWDIO_CLR();
		}
		SWD_SWCLK_SET();
		SWD_SWCLK_CLR();
	}

	return parity & 1;
}

uint8 SWD_SeqIn_Delay(uint8 *seq, uint16 num_of_bits)
{
	uint16 i;
	uint8 parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		SWD_SWCLK_SET();
		SWD_Delay();
		SWD_SWCLK_CLR();
		if (SWD_SWDIO_GET())
		{
			seq[i / 8] |= 1 << (i % 8);
			parity++;
		}
		else
		{
			seq[i / 8] &= ~(1 << (i % 8));
		}
		SWD_Delay();
	}
	return parity & 1;
}

uint8 SWD_SeqOut_Delay(uint8 *seq, uint16 num_of_bits)
{
	uint16 i;
	uint8 parity = 0;

	for (i = 0; i < num_of_bits; i++)
	{
		if (seq[i / 8] & (1 << (i % 8)))
		{
			SWD_SWDIO_SET();
			parity++;
		}
		else
		{
			SWD_SWDIO_CLR();
		}
		SWD_Delay();
		SWD_SWCLK_SET();
		SWD_Delay();
		SWD_SWCLK_CLR();
	}

	return parity & 1;
}

void SWD_StopClock(void)
{
	uint32 null = 0;

	// shift in at least 8 bits
	SWD_SeqOut((uint8*)&null, 8);
}

uint8 SWD_Transaction(uint8 request, uint32 *buff)
{
	uint32 reply, dummy;
	uint8 read = request & SWD_TRANS_RnW, parity;
	uint16 retry = 0;

SWD_RETRY:
	// set swdio output to output request
	SWD_SWDIO_SET();
	SWD_SWDIO_SETOUTPUT();

	// send out request
	SWD_SeqOut(&request, 8);

	// set swdio input to receive reply
	SWD_SWDIO_SETINPUT();

	if (read)
	{
		// receive 3-bit reply
		SWD_SeqIn((uint8*)&reply, 3);
		// receive data and parity
		parity = SWD_SeqIn((uint8*)buff, 32);
		parity += SWD_SeqIn((uint8*)&dummy, 1);
		// trn
		SWD_SeqIn((uint8*)&dummy, SWD_Trn);

		// set swdio output to output stop clock
		SWD_SWDIO_SET();
		SWD_SWDIO_SETOUTPUT();
	}
	else
	{
		// receive trn and 3-bit reply and then trn
		SWD_SeqIn((uint8*)&reply, SWD_Trn + 3);

		// set swdio output to output data
		SWD_SWDIO_SET();
		SWD_SWDIO_SETOUTPUT();

		// send data and parity
		parity = SWD_SeqOut((uint8*)buff, 32);
		parity += SWD_SeqOut(&parity, 1);
	}
	SWD_StopClock();
	// set swdio input after clock is stopped
	SWD_SWDIO_SETINPUT();
	reply &= 0x07; 
	switch (reply)
	{
	case SWD_ACK_OK:
		if (parity & 1)
		{
			return SWD_PARITY_ERROR | reply;
		}
		else
		{
			return SWD_SUCCESS | reply;
		}
	case SWD_ACK_WAIT:
		retry++;
		if (retry < SWD_Retry)
		{
			goto SWD_RETRY;
		}
		else
		{
			return SWD_RETRY_OUT | reply;
		}
	case SWD_ACK_FAULT:
		return SWD_FAULT | reply;
	default:
		return SWD_ACK_ERROR | reply;
	}
}

void SWD_Init(void)
{
	SWD_SWDIO_SETINPUT();
	SWD_SWCLK_SET();
	SWD_SWCLK_SETOUTPUT();
}

void SWD_Fini(void)
{
	SWD_SWDIO_SETINPUT();
	SWD_SWCLK_SETINPUT();
}

void SWD_SetDelay(uint16 dly)
{
	if (!dly)
	{
		SWD_SeqIn = SWD_SeqIn_NoDelay;
		SWD_SeqOut = SWD_SeqOut_NoDelay;
	}
	else
	{
		SWD_SeqIn = SWD_SeqIn_Delay;
		SWD_SeqOut = SWD_SeqOut_Delay;
		SWD_Delay = dly - 1;
	}
}

void SWD_SetTurnaround(uint8 cycles)
{
	if (cycles <= 14)
	{
		SWD_Trn = cycles;
	}
}

void SWD_SetRetryCount(uint16 retry)
{
	SWD_Retry = retry;
}

#endif
