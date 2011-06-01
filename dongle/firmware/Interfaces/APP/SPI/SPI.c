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

#include "interfaces.h"
#include "SPI.h"

uint8_t SPI_Emu = 0;
uint32_t SPI_Dly;

static void SPI_Delay(uint8_t dly)
{
	DelayUS(dly);
}

static void SPI_Config(uint32_t freq_hz, uint32_t firstbit, uint32_t cpol, uint32_t cpha)
{
	if(freq_hz < SPI_MIN_KHZ * 1000)
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

uint8_t SPI_GetSCKDiv(uint32_t freq_khz)
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

	return (uint8_t)(freq_khz << 3);
}

/// spi Read and Write emulated by IO
/// @param[in]	data	data to write
/// @return		data read
#define SPI_DATA_LEN			8
#define SPI_MSB					(1 << (SPI_DATA_LEN - 1))
static uint8_t SPI_RW_Emu(uint8_t data)
{
	uint8_t tmp,ret = 0;

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

/// spi Read and Write
/// @param[in]	data	data to write
/// @return		data read
static uint8_t SPI_RW(uint8_t data)
{
	uint8_t ret;

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

RESULT spi_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT spi_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		SPI_I2S_DeInit(SPI_Interface);
		SPI_AllInput();
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT spi_config(uint8_t index, uint32_t kHz, uint8_t cpol, uint8_t cpha, 
					 uint8_t first_bit)
{
	switch (index)
	{
	case 0:
		if(cpol & SPI_CPOL_MASK)
		{
			cpol = SPI_CPOL_High;
		}
		else
		{
			cpol = SPI_CPOL_Low;
		}
		if(cpha & SPI_CPHA_MASK)
		{
			// 2 edge
			cpha = SPI_CPHA_2Edge;
		}
		else
		{
			// 1 edge
			cpha = SPI_CPHA_1Edge;
		}
		if(first_bit & SPI_FIRSTBIT_MASK)
		{
			// msb first
			first_bit = SPI_FirstBit_MSB;
		}
		else
		{
			// lsb first
			first_bit = SPI_FirstBit_LSB;
		}
		SPI_Config(kHz * 1000, first_bit, cpol, cpha);
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint16_t len)
{
	uint16_t i;

	switch (index)
	{
	case 0:
		if ((NULL == out) || (NULL == in))
		{
			return ERROR_FAIL;
		}
		
		for(i = 0; i < len; i++)
		{
			in[i] = SPI_RW(out[i]);
		}
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

#endif
