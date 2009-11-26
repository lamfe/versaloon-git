/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       C2.c                                                      *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    C2 interface implementation file                          *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_C2_EN

#include "C2.h"

#define C2_Delay()						DelayUS(0)
#define C2_STROKE()						do{ C2_C2CK_CLR(); C2_Delay(); C2_C2CK_SET(); C2_Delay();}while(0)
#define C2_START()						C2_STROKE()
#define C2_END()						C2_STROKE()

void C2_Init(void)
{
	C2_C2D_SETINPUT();
	// Reset here
	C2_C2CK_CLR();
	C2_C2CK_SETOUTPUT();
	DelayMS(1);
	C2_C2CK_SET();
}

void C2_Fini(void)
{
	C2_C2D_SETINPUT();
	C2_C2CK_SETINPUT();
}

void C2_ReadAddr(uint8 *ir)
{
	uint8 tmp, i;

	C2_START();

	// INS, 0B10, LSB first
	C2_C2D_CLR();
	C2_C2D_SETOUTPUT();
	C2_STROKE();
	C2_C2D_SET();
	C2_STROKE();

	// ADDR
	C2_C2D_SETINPUT();
	tmp = 0;
	for(i = 0; i < 8; i++)
	{
		tmp >>= 1;
		C2_STROKE();
		if(C2_C2D_GET())
		{
			tmp |= 0x80;
		}
	}
	*ir = tmp;

	C2_END();
}

void C2_WriteAddr(uint8 ir)
{
	uint8 i;

	C2_START();

	// INS, 0B11, LSB first
	C2_C2D_SET();
	C2_C2D_SETOUTPUT();
	C2_STROKE();
	C2_STROKE();

	// ADDR
	for(i = 0; i < 8; i++)
	{
		if(ir & 0x01)
		{
			C2_C2D_SET();
		}
		else
		{
			C2_C2D_CLR();
		}
		C2_STROKE();
		ir >>= 1;
	}

	C2_C2D_SETINPUT();
	C2_END();
}

uint8 C2_ReadData(uint8 *data)
{
	uint8 tmp, i;

	C2_START();

	// INS, 0B00, LSB first
	C2_C2D_CLR();
	C2_C2D_SETOUTPUT();
	C2_STROKE();
	C2_STROKE();

	// LENGTH
	C2_STROKE();
	C2_STROKE();

	// WAIT
	C2_C2D_SETINPUT();
	do{
		C2_STROKE();
		DelayUS(0);
	}while(!C2_C2D_GET());

	// DATA
	tmp = 0;
	for(i = 0; i < 8; i++)
	{
		tmp >>= 1;
		C2_STROKE();
		if(C2_C2D_GET())
		{
			tmp |= 0x80;
		}
	}
	*data = tmp;

	C2_END();

	return 0;
}

uint8 C2_WriteData(uint8 data)
{
	uint8 i;

	C2_START();

	// INS, 0B01, LSB first
	C2_C2D_SET();
	C2_C2D_SETOUTPUT();
	C2_STROKE();
	C2_C2D_CLR();
	C2_STROKE();

	// LENGTH
	C2_STROKE();
	C2_STROKE();

	// DATA
	for(i = 0; i < 8; i++)
	{
		if(data & 0x01)
		{
			C2_C2D_SET();
		}
		else
		{
			C2_C2D_CLR();
		}
		C2_STROKE();
		data >>= 1;
	}

	// WAIT
	C2_C2D_SETINPUT();
	do{
		C2_STROKE();
	}while(!C2_C2D_GET());

	C2_END();

	return 0;
}

#endif
