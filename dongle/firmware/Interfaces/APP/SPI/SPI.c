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

#include "app_interfaces.h"
#include "SPI.h"

static uint8_t SPI_Emu = 0;
static uint32_t SPI_Dly;

static void SPI_Delay(uint8_t dly)
{
	app_interfaces.delay.delayus(dly);
}

static vsf_err_t SPI_Config_Emu(uint32_t freq_hz, uint8_t mode)
{
	SPI_Dly = 250000 / freq_hz;
	
	JTAG_TAP_TCK_CLR();
	JTAG_TAP_TCK_SETOUTPUT();
	JTAG_TAP_TDI_SETOUTPUT();
	JTAG_TAP_TDO_SETINPUT();
	return VSFERR_NONE;
}

#define SPI_DATA_LEN			8
#define SPI_MSB					(1 << (SPI_DATA_LEN - 1))
static uint8_t SPI_RW_Emu(uint8_t data)
{
	uint8_t tmp,ret = 0;

	for(tmp = SPI_DATA_LEN; tmp; tmp--)
	{
		if(data & SPI_MSB)
		{
			JTAG_TAP_TDI_SET();
		}
		else
		{
			JTAG_TAP_TDI_CLR();
		}
		data <<= 1;

		SPI_Delay(SPI_Dly);

		JTAG_TAP_TCK_SET();

		SPI_Delay(SPI_Dly);

		ret <<= 1;
		if(JTAG_TAP_TDO_GET())
		{
			ret |= 1;
		}

		SPI_Delay(SPI_Dly);

		JTAG_TAP_TCK_CLR();

		SPI_Delay(SPI_Dly);
	}
	return ret;
}

vsf_err_t spi_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return interfaces->spi.fini(SPI_Interface_Idx);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_config(uint8_t index, uint32_t kHz, uint8_t mode)
{
	struct spi_ability_t spi_ability;
	uint32_t min_khz;
	
	if (interfaces->spi.get_ability(SPI_Interface_Idx, &spi_ability))
	{
		return VSFERR_FAIL;
	}
	min_khz = spi_ability.min_freq_hz / 1000;
	
	switch (index)
	{
	case 0:
		if(kHz < min_khz)
		{
			SPI_Emu = 1;
			interfaces->spi.fini(SPI_Interface_Idx);
			return SPI_Config_Emu(kHz * 1000, mode);
		}
		else
		{
			SPI_Emu = 0;
			interfaces->spi.init(SPI_Interface_Idx);
			return interfaces->spi.config(SPI_Interface_Idx, kHz, 
											mode | SPI_MASTER);
		}
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint32_t len)
{
	uint32_t i;

	switch (index)
	{
	case 0:
		if ((NULL == out) || (NULL == in))
		{
			return VSFERR_INVALID_PTR;
		}
		
		if(SPI_Emu)
		{
			for(i = 0; i < len; i++)
			{
				in[i] = SPI_RW_Emu(out[i]);
			}
		}
		else
		{
			interfaces->spi.io(SPI_Interface_Idx, out, in, len);
		}
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
