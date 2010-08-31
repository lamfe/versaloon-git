/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       AVR_ISP.c                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation for AVR_ISP support                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "SPI.h"
#include "AVR_ISP.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

/// ISP Communicate
/// @param[in]		data	data to send
/// @param[out]		ret		data received
void AVRISP_CommInt(uint8 *data, uint8 *ret, uint32 len)
{
	uint8 i;

	for(i = 0; i < len; i++)
	{
		ret[i] = SPI_RW(data[i]);
	}
}

/// ISP Wait Ready
/// @return		Success or not
uint8 AVRISP_RDY_Wait()
{
	uint8 dly = 255;
	uint8 cmd[4];

poll:
	cmd[0] = 0xF0;
	cmd[1] = 0x00;
	cmd[2] = 0x00;
	cmd[3] = 0x00;
	AVRISP_Comm((uint8*)cmd, (uint8*)cmd);

	if((cmd[3] & 1) && --dly)
	{
		DelayUS(80);
		goto poll;
	}

	if(dly)
	{
		return AVRISP_Success;
	}
	else
	{
		return AVRISP_Error;
	}
}

/// ISP Initialization
/// @return
void AVRISP_Init(uint32 freq)
{
	GLOBAL_OUTPUT_Acquire();
	PWREXT_Acquire();
	DelayMS(1);

	RST_SETOUTPUT();
	SPI_Config(freq, SPI_FirstBit_MSB, SPI_CPOL_Low, SPI_CPHA_1Edge);
}

/// ISP Finilization
/// @return
void AVRISP_Fini(void)
{
	SPI_Disable();
	SPI_AllInput();
	RST_SETINPUT();

	PWREXT_Release();
	GLOBAL_OUTPUT_Release();
}
