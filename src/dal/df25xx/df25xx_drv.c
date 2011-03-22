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
#include "df25xx_drv_cfg.h"
#include "df25xx_drv.h"

#define DF25XX_CMD_WRSR						0x01
#define DF25XX_CMD_PGWR						0x02
#define DF25XX_CMD_PGRD						0x03
#define DF25XX_CMD_WRDI						0x04
#define DF25XX_CMD_RSDR						0x05
#define DF25XX_CMD_WREN						0x06
#define DF25XX_CMD_ER4K						0x20
#define DF25XX_CMD_CHER						0x60
#define DF25XX_CMD_RDID						0x9F

static struct df25xx_drv_param_t df25xx_drv_param;

static RESULT df25xx_drv_cs_assert(void)
{
	interfaces->gpio.config(DF25XX_CS_GPIO_IDX, DF25XX_CS_GPIO_PIN, 
							DF25XX_CS_GPIO_PIN, 0, 0);
	return ERROR_OK;
}

static RESULT df25xx_drv_cs_deassert(void)
{
	interfaces->gpio.config(DF25XX_CS_GPIO_IDX, DF25XX_CS_GPIO_PIN, 0, 
							DF25XX_CS_GPIO_PIN, DF25XX_CS_GPIO_PIN);
	return ERROR_OK;
}

static RESULT df25xx_drv_io(uint8_t *out, uint8_t *in, uint16_t len)
{
	df25xx_drv_cs_assert();
	interfaces->spi.io(DF25XX_SPI_IDX, out, in, len);
	df25xx_drv_cs_deassert();
	return ERROR_OK;
}

static RESULT df25xx_drv_init(void *param)
{
	if (NULL == param)
	{
		return ERROR_FAIL;
	}
	memcpy(&df25xx_drv_param, param, sizeof(df25xx_drv_param));
	
	if (!df25xx_drv_param.spi_khz)
	{
		df25xx_drv_param.spi_khz = 9000;
	}
	interfaces->gpio.init(DF25XX_CS_GPIO_IDX);
	interfaces->gpio.config(DF25XX_CS_GPIO_IDX, DF25XX_CS_GPIO_PIN, 0, 
							DF25XX_CS_GPIO_PIN, DF25XX_CS_GPIO_PIN);
	interfaces->spi.init(DF25XX_SPI_IDX);
	interfaces->spi.config(DF25XX_SPI_IDX, df25xx_drv_param.spi_khz, 
							SPI_CPOL_HIGH, SPI_CPHA_2EDGE, SPI_MSB_FIRST);
	
	return ERROR_OK;
}

static RESULT df25xx_drv_getinfo(void *info)
{
	struct df25xx_drv_info_t *pinfo = (struct df25xx_drv_info_t *)info;
	uint8_t out_buff[13], in_buff[13];
	
	out_buff[0] = DF25XX_CMD_RDID;
	df25xx_drv_io(out_buff, in_buff, 4);
	if (ERROR_OK != interfaces->peripheral_commit())
	{
		return ERROR_FAIL;
	}
	
	pinfo->manufacturer_id = in_buff[1];
	pinfo->device_id = GET_BE_U16(&in_buff[2]);
	return ERROR_OK;
}

static RESULT df25xx_drv_fini(void)
{
	interfaces->gpio.fini(DF25XX_CS_GPIO_IDX);
	interfaces->spi.fini(DF25XX_SPI_IDX);
	return ERROR_OK;
}

static RESULT df25xx_drv_eraseall_nb_start(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_eraseall_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_eraseall_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_eraseall_nb_end(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_eraseblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_eraseblock_nb(uint64_t address)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_eraseblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_eraseblock_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_eraseblock_nb_end(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_readblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_readblock_nb(uint64_t address, uint8_t *buff)
{
	uint8_t cmd[4];
	
	df25xx_drv_cs_assert();
	
	cmd[0] = DF25XX_CMD_PGRD;
	cmd[1] = (address >> 16) & 0xFF;
	cmd[2] = (address >> 8 ) & 0xFF;
	cmd[3] = (address >> 0 ) & 0xFF;
	interfaces->spi.io(DF25XX_SPI_IDX, cmd, cmd, 4);
	interfaces->spi.io(DF25XX_SPI_IDX, buff, buff, 
						(uint16_t)df25xx_drv.capacity.block_size);
	
	df25xx_drv_cs_deassert();
	
	return ERROR_OK;
}

static RESULT df25xx_drv_readblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_readblock_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_readblock_nb_end(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_writeblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_writeblock_nb(uint64_t address, uint8_t *buff)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_writeblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_writeblock_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT df25xx_drv_writeblock_nb_end(void)
{
	return ERROR_OK;
}

struct mal_driver_t df25xx_drv = 
{
	MAL_IDX_DF25XX,
	MAL_SUPPORT_READBLOCK,
	{0, 0},
	
	df25xx_drv_init,
	df25xx_drv_fini,
	df25xx_drv_getinfo,
	
	df25xx_drv_eraseall_nb_start,
	df25xx_drv_eraseall_nb_isready,
	df25xx_drv_eraseall_nb_waitready,
	df25xx_drv_eraseall_nb_end,
	
	df25xx_drv_eraseblock_nb_start,
	df25xx_drv_eraseblock_nb,
	df25xx_drv_eraseblock_nb_isready,
	df25xx_drv_eraseblock_nb_waitready,
	df25xx_drv_eraseblock_nb_end,
	
	df25xx_drv_readblock_nb_start,
	df25xx_drv_readblock_nb,
	df25xx_drv_readblock_nb_isready,
	df25xx_drv_readblock_nb_waitready,
	df25xx_drv_readblock_nb_end,
	
	df25xx_drv_writeblock_nb_start,
	df25xx_drv_writeblock_nb,
	df25xx_drv_writeblock_nb_isready,
	df25xx_drv_writeblock_nb_waitready,
	df25xx_drv_writeblock_nb_end
};

