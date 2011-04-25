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

#include <string.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"

#include "../mal/mal.h"
#include "../mal/mal_internal.h"
#include "df45xx_drv_cfg.h"
#include "df45xx_drv.h"

static struct df45xx_drv_interface_t df45xx_drv_ifs;
static struct df45xx_drv_param_t df45xx_drv_param;

static RESULT df45xx_drv_config_interface(void *ifs)
{
	if (NULL == ifs)
	{
		return ERROR_FAIL;
	}
	
	memcpy(&df45xx_drv_ifs, ifs, sizeof(df45xx_drv_ifs));
	return ERROR_OK;
}

static RESULT df45xx_drv_init(void *param)
{
	if (NULL == param)
	{
		return ERROR_FAIL;
	}
	memcpy(&df45xx_drv_param, param, sizeof(df45xx_drv_param));
	
	return ERROR_OK;
}

static RESULT df45xx_drv_fini(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_getinfo(void *info)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseall_nb_start(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseall_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseall_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseall_nb_end(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb(uint64_t address)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb_end(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb(uint64_t address, uint8_t *buff)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb_end(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb(uint64_t address, uint8_t *buff)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb_end(void)
{
	return ERROR_OK;
}

#if DAL_INTERFACE_PARSER_EN
static RESULT df45xx_drv_parse_interface(uint8_t *buff)
{
	if (NULL == buff)
	{
		return ERROR_FAIL;
	}
	df45xx_drv_ifs.spi_port = buff[0];
	df45xx_drv_ifs.cs_port = buff[1];
	df45xx_drv_ifs.cs_pin = *(uint32_t *)&buff[2];
	return ERROR_OK;
}
#endif

struct mal_driver_t df45xx_drv = 
{
	{
		"df45xx",
#if DAL_INTERFACE_PARSER_EN
		"spi:%1dcs:%1d,%4x",
		df45xx_drv_parse_interface,
#endif
		df45xx_drv_config_interface,
	},
	
	MAL_IDX_DF45XX,
	0,
	{0, 0},
	
	df45xx_drv_init,
	df45xx_drv_fini,
	df45xx_drv_getinfo,
	NULL,
	
	df45xx_drv_eraseall_nb_start,
	df45xx_drv_eraseall_nb_isready,
	df45xx_drv_eraseall_nb_waitready,
	df45xx_drv_eraseall_nb_end,
	
	df45xx_drv_eraseblock_nb_start,
	df45xx_drv_eraseblock_nb,
	df45xx_drv_eraseblock_nb_isready,
	df45xx_drv_eraseblock_nb_waitready,
	df45xx_drv_eraseblock_nb_end,
	
	df45xx_drv_readblock_nb_start,
	df45xx_drv_readblock_nb,
	df45xx_drv_readblock_nb_isready,
	df45xx_drv_readblock_nb_waitready,
	df45xx_drv_readblock_nb_end,
	
	df45xx_drv_writeblock_nb_start,
	df45xx_drv_writeblock_nb,
	df45xx_drv_writeblock_nb_isready,
	df45xx_drv_writeblock_nb_waitready,
	df45xx_drv_writeblock_nb_end
};

