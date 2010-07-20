/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SPI.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    SPI interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_SPI_EN

#include "SPI.h"

uint8 SPI_Emu = 0;
uint32 SPI_Dly;

void SPI_Delay(uint8 dly)
{
	DelayUS(dly);
}

void SPI_Config(uint32 freq_hz, uint32 firstbit, uint32 cpol, uint32 cpha)
{
	if(freq_hz < SPI_MIN_SPEED * 1000)
	{
		SPI_Emu = 1;

		SPI_Dly = 250000 / freq_hz;

		SPI_Disable();

		SPI_SCK_CLR();
		SPI_AllSPIIO();
	}
	else
	{
		SPI_Emu = 0;
		SPI_AllSPIHW();
		SPI_Conf(SPI_GetSCKDiv(freq_hz / 1000), firstbit, cpol, cpha);
	}
}

uint8 SPI_GetSCKDiv(uint16 freq_khz)
{
	// Set Speed
	if(freq_khz >= _SYS_FREQUENCY * 500 / 2)
	{
		freq_khz = 0;
	}
	else if(freq_khz >= _SYS_FREQUENCY * 500 / 4)
	{
		freq_khz = 1;
	}
	else if(freq_khz >= _SYS_FREQUENCY * 500 / 8)
	{
		freq_khz = 2;
	}
	else if(freq_khz >= _SYS_FREQUENCY * 500 / 16)
	{
		freq_khz = 3;
	}
	else if(freq_khz >= _SYS_FREQUENCY * 500 / 32)
	{
		freq_khz = 4;
	}
	else if(freq_khz > _SYS_FREQUENCY * 500 / 64)
	{
		freq_khz = 5;
	}
	else if(freq_khz > _SYS_FREQUENCY * 500 / 128)
	{
		freq_khz = 6;
	}
	else
	{
		freq_khz = 7;
	}

	return (uint8)(freq_khz << 3);
}

/// spi Read and Write emulated by IO
/// @param[in]	data	data to write
/// @return		data read
uint8 SPI_RW_Emu(uint8 data)
{
	uint8 tmp,ret = 0;

	for(tmp = SPI_DATA_LEN; tmp; tmp--)
	{
		if(data & SPI_MSB)
		{
			SPI_MOSI_SET();
		}
		else
		{
			SPI_MOSI_CLR();
		}
		data <<= 1;

		SPI_Delay(SPI_Dly);

		SPI_SCK_SET();

		SPI_Delay(SPI_Dly);

		ret <<= 1;
		if(SPI_MISO_GET())
		{
			ret |= 1;
		}

		SPI_Delay(SPI_Dly);

		SPI_SCK_CLR();

		SPI_Delay(SPI_Dly);
	}
	return ret;
}

/// spi Read and Write by spi hardware
/// @param[in]	data	data to write
/// @return		data read
uint8 SPI_RW_HW(uint8 data)
{
	uint8 ret;

	SPI_WaitReady();

	ret =  SPI_GetData();

	SPI_SetData(data);

	return ret;
}

/// spi Read and Write
/// @param[in]	data	data to write
/// @return		data read
uint8 SPI_RW(uint8 data)
{
	uint8 ret;

	if(SPI_Emu)
	{
		ret = SPI_RW_Emu(data);
	}
	else
	{
		SPI_SetData(data);

		SPI_WaitRxReady();
		ret = SPI_GetData();
		SPI_WaitReady();
	}

	return ret;
}

#endif
