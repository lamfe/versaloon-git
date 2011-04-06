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
#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"

#include "dal.h"
#include "mal/mal.h"
#include "df25xx/df25xx_drv.h"

#include "df25xx.h"
#include "df25xx_internal.h"

#define CUR_TARGET_STRING			DF25XX_STRING

struct program_area_map_t df25xx_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t df25xx_program_mode[] = 
{
	{'*', SET_FREQUENCY, SPI | GPIO},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(df25xx);
LEAVE_PROGRAM_MODE_HANDLER(df25xx);
ERASE_TARGET_HANDLER(df25xx);
WRITE_TARGET_HANDLER(df25xx);
READ_TARGET_HANDLER(df25xx);
const struct program_functions_t df25xx_program_functions = 
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(df25xx), 
	LEAVE_PROGRAM_MODE_FUNCNAME(df25xx), 
	ERASE_TARGET_FUNCNAME(df25xx), 
	WRITE_TARGET_FUNCNAME(df25xx), 
	READ_TARGET_FUNCNAME(df25xx)
};

VSS_HANDLER(df25xx_help)
{
	VSS_CHECK_ARGC(1);
	printf("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>              set IIC frequency, in KHz\n\n", 
			CUR_TARGET_STRING);
	return ERROR_OK;
}

const struct vss_cmd_t df25xx_notifier[] = 
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				df25xx_help),
	VSS_CMD_END
};





static struct interfaces_info_t *interfaces = NULL;
#define commit()					interfaces->peripheral_commit()

ENTER_PROGRAM_MODE_HANDLER(df25xx)
{
	struct chip_param_t *param = context->param;
	struct program_info_t *pi = context->pi;
	struct df25xx_drv_param_t drv_param;
	
	interfaces = context->prog;
	if (ERROR_OK != dal_init(interfaces))
	{
		return ERROR_FAIL;
	}
	
	drv_param.spi_khz = pi->frequency;
	if (ERROR_OK != mal.init(MAL_IDX_DF25XX, &drv_param))
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != mal.setcapacity(MAL_IDX_DF25XX, 
			param->chip_areas[APPLICATION_IDX].page_size, 
			param->chip_areas[APPLICATION_IDX].page_num))
	{
		return ERROR_FAIL;
	}
	
	return commit();
}

LEAVE_PROGRAM_MODE_HANDLER(df25xx)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	mal.fini(MAL_IDX_DF25XX);
	return commit();
}

ERASE_TARGET_HANDLER(df25xx)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	// no need to erase
	return ERROR_OK;
}

WRITE_TARGET_HANDLER(df25xx)
{
	struct chip_param_t *param = context->param;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (size % param->chip_areas[APPLICATION_IDX].page_size)
		{
			return ERROR_FAIL;
		}
		size /= param->chip_areas[APPLICATION_IDX].page_size;
		
		if (ERROR_OK != mal.writeblock(MAL_IDX_DF25XX, addr, buff, size))
		{
			return ERROR_FAIL;
		}
		return commit();
		break;
	default:
		return ERROR_FAIL;
	}
}

READ_TARGET_HANDLER(df25xx)
{
	struct chip_param_t *param = context->param;
	struct df25xx_drv_info_t drv_info;
	
	switch (area)
	{
	case CHIPID_CHAR:
		if (ERROR_OK != mal.getinfo(MAL_IDX_DF25XX, &drv_info))
		{
			return ERROR_FAIL;
		}
		SET_LE_U16(&buff[0], drv_info.device_id);
		buff[2] = drv_info.manufacturer_id;
		return ERROR_OK;
		break;
	case APPLICATION_CHAR:
		if (size % param->chip_areas[APPLICATION_IDX].page_size)
		{
			return ERROR_FAIL;
		}
		size /= param->chip_areas[APPLICATION_IDX].page_size;
		
		if (ERROR_OK != mal.readblock(MAL_IDX_DF25XX, addr, buff, size))
		{
			return ERROR_FAIL;
		}
		return ERROR_OK;
		break;
	default:
		return ERROR_FAIL;
	}
}

