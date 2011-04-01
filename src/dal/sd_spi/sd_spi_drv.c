/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>

#include "app_cfg.h"
#include "app_type.h"

#include "../dal_internal.h"
#include "../mal/mal_internal.h"
#include "../mal/mal.h"
#include "sd_spi_drv_cfg.h"
#include "sd_spi_drv.h"

static struct sd_spi_drv_param_t sd_spi_drv_param;

static RESULT sd_spi_drv_init(void *param)
{
	if (NULL == param)
	{
		return ERROR_FAIL;
	}
	memcpy(&sd_spi_drv_param, param, sizeof(sd_spi_drv_param));
	
	if (!sd_spi_drv_param.spi_khz)
	{
		sd_spi_drv_param.spi_khz = 100;
	}
	interfaces->gpio.init(SD_CS_PORT);
	interfaces->gpio.config(SD_CS_PORT, SD_CS_PIN, 0, SD_CS_PIN, SD_CS_PIN);
	interfaces->spi.init(SD_SPI_IDX);
	interfaces->spi.config(SD_SPI_IDX, sd_spi_drv_param.spi_khz, 
							SPI_CPOL_HIGH, SPI_CPHA_2EDGE, SPI_MSB_FIRST);
	
	// SD Init
	
	return ERROR_OK;
}

static RESULT sd_spi_drv_fini(void)
{
	interfaces->gpio.fini(SD_CS_PORT);
	interfaces->spi.fini(SD_SPI_IDX);
	return ERROR_OK;
}

static RESULT sd_spi_getinfo(void *info)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_readblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_readblock_nb(uint64_t address, uint8_t *buff)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_readblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_readblock_nb_end(void)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_writeblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_writeblock_nb(uint64_t address, uint8_t *buff)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_writeblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT sd_spi_drv_writeblock_nb_end(void)
{
	return ERROR_OK;
}

struct mal_driver_t sd_spi_drv = 
{
	MAL_IDX_SD_SPI,
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK,
	{0, 0},
	
	sd_spi_drv_init,
	sd_spi_drv_fini,
	sd_spi_getinfo,
	
	NULL,
	NULL,
	NULL,
	NULL,
	
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	
	sd_spi_drv_readblock_nb_start,
	sd_spi_drv_readblock_nb,
	sd_spi_drv_readblock_nb_isready,
	NULL,
	sd_spi_drv_readblock_nb_end,
	
	sd_spi_drv_writeblock_nb_start,
	sd_spi_drv_writeblock_nb,
	sd_spi_drv_writeblock_nb_isready,
	NULL,
	sd_spi_drv_writeblock_nb_end
};

