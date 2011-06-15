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

#include "app_cfg.h"
#include "app_type.h"

#include "../mal/mal.h"
#include "../mal/mal_driver.h"
#include "df45xx_drv_cfg.h"
#include "df45xx_drv.h"

static RESULT df45xx_drv_init(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_fini(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_getinfo(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseall_nb_start(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseall_nb_isready(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseall_nb_waitready(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseall_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb(struct dal_info_t *info, 
										uint64_t address)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb_isready(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb_waitready(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_eraseblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb_isready(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb_waitready(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_readblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb_start(struct dal_info_t *info, 
											uint64_t address, uint64_t count)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(count);
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb(struct dal_info_t *info, 
										uint64_t address, uint8_t *buff)
{
	REFERENCE_PARAMETER(info);
	REFERENCE_PARAMETER(address);
	REFERENCE_PARAMETER(buff);
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb_isready(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb_waitready(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

static RESULT df45xx_drv_writeblock_nb_end(struct dal_info_t *info)
{
	REFERENCE_PARAMETER(info);
	return ERROR_OK;
}

#if DAL_INTERFACE_PARSER_EN
static RESULT df45xx_drv_parse_interface(struct dal_info_t *info, uint8_t *buff)
{
	struct df45xx_drv_interface_t *ifs = 
									(struct df45xx_drv_interface_t *)info->ifs;
	
	ifs->spi_port = buff[0];
	ifs->cs_port = buff[1];
	ifs->cs_pin = *(uint32_t *)&buff[2];
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
	},
	
	MAL_IDX_DF45XX,
	0,
	
	df45xx_drv_init,
	df45xx_drv_fini,
	df45xx_drv_getinfo,
	NULL,
	
	NULL, NULL, NULL, NULL,
	
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

