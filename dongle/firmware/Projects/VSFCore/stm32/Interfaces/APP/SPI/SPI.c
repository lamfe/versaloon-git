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
		return core_interfaces.spi.fini(SPI_Interface_Idx);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_config(uint8_t index, uint32_t kHz, uint8_t mode)
{
	switch (index)
	{
	case 0:
		core_interfaces.spi.init(SPI_Interface_Idx);
		return core_interfaces.spi.config(SPI_Interface_Idx, kHz, 
											mode | SPI_MASTER);
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint32_t len)
{
	switch (index)
	{
	case 0:
		core_interfaces.spi.io(SPI_Interface_Idx, out, in, len);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
