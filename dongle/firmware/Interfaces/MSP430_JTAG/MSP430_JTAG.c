/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       MSP430_JTAG.c                                             *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    MSP430_JTAG interface implementation file                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_MSP430_JTAG_EN

#include "MSP430_JTAG.h"

#define MSP430_JTAG_DELAY()				DelayUS(0)

void MSP430_JTAG_Init(uint8 has_test)
{
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TMS_SETOUTPUT();
	JTAG_TAP_TDI_CLR();
	JTAG_TAP_TDI_SETOUTPUT();
	JTAG_TAP_TDO_SETINPUT();
	JTAG_TAP_TCK_CLR();
	JTAG_TAP_TCK_SETOUTPUT();
	DelayMS(50);

	JTAG_TAP_TMS_SET();
	JTAG_TAP_TDI_SET();
	JTAG_TAP_TCK_SET();

	if(has_test)
	{
		// enter 4-wire JTAG mode
		// TEST :        _______         _________
		//       _______|       |_______|         
		// /RST :___________                ______
		//                  |______________|______
		{
			MSP430_JTAG_TEST_CLR();
			MSP430_JTAG_TEST_SETOUTPUT();
			JTAG_TAP_SRST_SET();
			JTAG_TAP_SRST_SETOUTPUT();
			DelayMS(1);		// wait for SRST stable

			// now TEST low, SRST high
			MSP430_JTAG_TEST_SET();	// BSL disabled
			DelayMS(1);
			JTAG_TAP_SRST_CLR();
			DelayMS(1);
			MSP430_JTAG_TEST_CLR();
			DelayMS(1);
			MSP430_JTAG_TEST_SET();
			DelayMS(1);
		}

		JTAG_TAP_SRST_SETINPUT();	// release SRST
		DelayMS(1);
	}
	else
	{
		JTAG_TAP_SRST_SET();
		JTAG_TAP_SRST_SETINPUT();
	}
}

void MSP430_JTAG_Fini(void)
{
#if 1
	MSP430_JTAG_TEST_CLR();
	DelayUS(50);
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TDI_CLR();
	JTAG_TAP_TCK_CLR();
#else
	JTAG_TAP_TMS_SETINPUT();
	JTAG_TAP_TDI_SETINPUT();
	JTAG_TAP_TDO_SETINPUT();
	JTAG_TAP_TCK_SETINPUT();
	MSP430_JTAG_TEST_CLR();
	MSP430_JTAG_TEST_SETINPUT();
	JTAG_TAP_SRST_SETINPUT();
#endif
}

void MSP430_JTAG_TCLK(uint8 tclk)
{
	if(tclk)
	{
		JTAG_TAP_TDI_SET();
	}
	else
	{
		JTAG_TAP_TDI_CLR();
	}
}

void MSP430_JTAG_TCLK_STROKE(uint16 cnt)
{
	while(cnt--)
	{
		MSP430_JTAG_TCLK(1);
		MSP430_JTAG_DELAY();
		MSP430_JTAG_TCLK(0);
	}
}

static uint32 MSP430_JTAG_Shift(uint32 data, uint8 len)
{
	uint32 tclk = JTAG_TAP_TDI_GET();
	uint32 tdo = 0, mask = 1 << (len - 1);
	uint8 i;

	for(i = len; i > 0; i--)
	{
		if((data & mask) > 0)
		{
			JTAG_TAP_TDI_SET();
		}
		else
		{
			JTAG_TAP_TDI_CLR();
		}
		if(1 == i)
		{
			JTAG_TAP_TMS_SET();
		}
		JTAG_TAP_TCK_CLR();
		data <<= 1;
		tdo <<= 1;
		MSP430_JTAG_DELAY();
		JTAG_TAP_TCK_SET();
		MSP430_JTAG_DELAY();

		if(JTAG_TAP_TDO_GET() > 0)
		{
			tdo |= 1;
		}
	}

	if(tclk > 0)
	{
		JTAG_TAP_TDI_SET();
	}
	else
	{
		JTAG_TAP_TDI_CLR();
	}

	// ED
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// UD
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// RTI

	return tdo;
}

uint32 MSP430_JTAG_DR(uint32 dr, uint8 len)
{
	// RTI
	JTAG_TAP_TMS_SET();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// SDS
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// CD
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();

	return MSP430_JTAG_Shift(dr, len);
}

uint8 MSP430_JTAG_IR(uint8 ir, uint8 len)
{
	// RTI
	JTAG_TAP_TMS_SET();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// SDS
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// SIS
	JTAG_TAP_TMS_CLR();
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	// CD
	JTAG_TAP_TCK_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();

	return (uint8)MSP430_JTAG_Shift(ir, len);
}

void MSP430_JTAG_Reset(void)
{
	uint8 i;

	JTAG_TAP_TDI_SET();
	JTAG_TAP_TMS_SET();

	// shift out 6 tms '1'
	for(i = 7; i > 0; i--)
	{
		JTAG_TAP_TCK_SET();
		MSP430_JTAG_DELAY();
		JTAG_TAP_TCK_CLR();
		MSP430_JTAG_DELAY();
	}

	JTAG_TAP_TMS_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TMS_SET();
	MSP430_JTAG_DELAY();

	// check fuse
	JTAG_TAP_TMS_CLR();
	DelayMS(1);
	JTAG_TAP_TMS_SET();
	DelayUS(5);
	JTAG_TAP_TMS_CLR();
	DelayMS(1);
	JTAG_TAP_TMS_SET();

	for(i = 7; i > 0; i--)
	{
		JTAG_TAP_TCK_SET();
		MSP430_JTAG_DELAY();
		JTAG_TAP_TCK_CLR();
		MSP430_JTAG_DELAY();
	}
	JTAG_TAP_TMS_CLR();
	MSP430_JTAG_DELAY();
	JTAG_TAP_TCK_SET();
	MSP430_JTAG_DELAY();
}

uint8 MSP430_JTAG_Poll(uint32 data, uint32 mask, uint32 value, uint8 len, uint16 poll_cnt)
{
	uint8 toggle_tclk = (poll_cnt & 0x8000) > 0;

	poll_cnt &= 0x7FFF;
	while(poll_cnt-- > 0)
	{
		if((MSP430_JTAG_DR(data, len) & mask) == value)
		{
			return 0;
		}
		if(toggle_tclk)
		{
			JTAG_TAP_TDI_CLR();
			JTAG_TAP_TDI_SET();
		}
	}
	return 1;
}

#endif
