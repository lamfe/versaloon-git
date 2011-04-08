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

#include "../dal_internal.h"
#include "../mal/mal_internal.h"
#include "../mal/mal.h"
#include "ee24cxx_drv_cfg.h"
#include "ee24cxx_drv.h"

static struct ee24cxx_drv_param_t ee24cxx_drv_param;

static RESULT ee24cxx_drv_init(void *param)
{
	if (NULL == param)
	{
		return ERROR_FAIL;
	}
	memcpy(&ee24cxx_drv_param, param, sizeof(ee24cxx_drv_param));
	
	if (!ee24cxx_drv_param.iic_khz)
	{
		ee24cxx_drv_param.iic_khz = 100;
	}
	interfaces->i2c.init(EE24CXX_IIC_IDX);
	interfaces->i2c.config(EE24CXX_IIC_IDX, ee24cxx_drv_param.iic_khz, 0, 
							10000);
	
	return ERROR_OK;
}

static RESULT ee24cxx_drv_fini(void)
{
	interfaces->i2c.fini(EE24CXX_IIC_IDX);
	return interfaces->peripheral_commit();
}

static RESULT ee24cxx_drv_readblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT ee24cxx_drv_readblock_nb(uint64_t address, uint8_t *buff)
{
	uint16_t addr_word = (uint16_t)address;
	
	addr_word = SYS_TO_BE_U16(addr_word);
	if ((ERROR_OK != interfaces->i2c.write(EE24CXX_IIC_IDX, 
			ee24cxx_drv_param.iic_addr, (uint8_t *)&addr_word, 2, 0)) || 
		(ERROR_OK != interfaces->i2c.read(EE24CXX_IIC_IDX, 
			ee24cxx_drv_param.iic_addr, buff, 
			(uint16_t)ee24cxx_drv.capacity.block_size, 1, true)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static RESULT ee24cxx_drv_readblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT ee24cxx_drv_readblock_nb_end(void)
{
	return ERROR_OK;
}

static RESULT ee24cxx_drv_writeblock_nb_start(uint64_t address, uint64_t count)
{
	return ERROR_OK;
}

static RESULT ee24cxx_drv_writeblock_nb(uint64_t address, uint8_t *buff)
{
	return ERROR_OK;
}

static RESULT ee24cxx_drv_writeblock_nb_isready(void)
{
	return ERROR_OK;
}

static RESULT ee24cxx_drv_writeblock_nb_waitready(void)
{
	return ERROR_OK;
}

static RESULT ee24cxx_drv_writeblock_nb_end(void)
{
	return ERROR_OK;
}

struct mal_driver_t ee24cxx_drv = 
{
	MAL_IDX_EE24CXX,
	MAL_SUPPORT_READBLOCK | MAL_SUPPORT_WRITEBLOCK,
	{0, 0},
	
	ee24cxx_drv_init,
	ee24cxx_drv_fini,
	NULL,
	
	NULL,
	NULL,
	NULL,
	NULL,
	
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	
	ee24cxx_drv_readblock_nb_start,
	ee24cxx_drv_readblock_nb,
	ee24cxx_drv_readblock_nb_isready,
	NULL,
	ee24cxx_drv_readblock_nb_end,
	
	ee24cxx_drv_writeblock_nb_start,
	ee24cxx_drv_writeblock_nb,
	ee24cxx_drv_writeblock_nb_isready,
	ee24cxx_drv_writeblock_nb_waitready,
	ee24cxx_drv_writeblock_nb_end
};

