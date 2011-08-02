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
#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"
#include "app_scripts.h"

#include "dal/mal/mal.h"
#include "dal/cfi/cfi_drv.h"

#include "cfi.h"

#define CUR_TARGET_STRING			CFI_STRING

struct program_area_map_t cfi_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t cfi_program_mode[] =
{
	{'*', "", IFS_EBI},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(cfi);
LEAVE_PROGRAM_MODE_HANDLER(cfi);
ERASE_TARGET_HANDLER(cfi);
WRITE_TARGET_HANDLER(cfi);
READ_TARGET_HANDLER(cfi);
const struct program_functions_t cfi_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(cfi),
	LEAVE_PROGRAM_MODE_FUNCNAME(cfi),
	ERASE_TARGET_FUNCNAME(cfi),
	WRITE_TARGET_FUNCNAME(cfi),
	READ_TARGET_FUNCNAME(cfi)
};

VSS_HANDLER(cfi_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>              set IIC frequency, in KHz\n\n",
			CUR_TARGET_STRING);
	return ERROR_OK;
}

const struct vss_cmd_t cfi_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				cfi_help),
	VSS_CMD_END
};






static struct cfi_drv_info_t cfi_drv_info;
static struct cfi_drv_param_t cfi_drv_param;
static struct cfi_drv_interface_t cfi_drv_ifs;
static struct mal_info_t cfi_mal_info =
{
	{0, 0}, NULL, 0, 0, 0
};
static struct dal_info_t cfi_dal_info =
{
	&cfi_drv_ifs,
	&cfi_drv_param,
	&cfi_drv_info,
	&cfi_mal_info,
};

ENTER_PROGRAM_MODE_HANDLER(cfi)
{
	struct chip_param_t *param = context->param;
	struct program_info_t *pi = context->pi;
	
	if (ERROR_OK != dal_init(context->prog))
	{
		return ERROR_FAIL;
	}
	
	if (pi->ifs_indexes != NULL)
	{
		if (ERROR_OK != dal_config_interface(CFI_STRING, pi->ifs_indexes,
												&cfi_dal_info))
		{
			return ERROR_FAIL;
		}
	}
	else
	{
		cfi_drv_ifs.ebi_port = 0;
		cfi_drv_ifs.nor_index = 1;
	}
	
	cfi_drv_param.nor_info.common_info.data_width = 16;
	cfi_drv_param.nor_info.common_info.wait_signal = EBI_WAIT_NONE;
	cfi_drv_param.nor_info.param.addr_multiplex = false;
	cfi_drv_param.nor_info.param.clock_hz_r = 
		cfi_drv_param.nor_info.param.clock_hz_w = 0;
	cfi_drv_param.nor_info.param.address_setup_cycle_r = 
		cfi_drv_param.nor_info.param.address_setup_cycle_w = 2;
	cfi_drv_param.nor_info.param.address_hold_cycle_r = 
		cfi_drv_param.nor_info.param.address_hold_cycle_w = 0;
	cfi_drv_param.nor_info.param.data_setup_cycle_r = 
		cfi_drv_param.nor_info.param.data_setup_cycle_w = 7;
	
	if ((ERROR_OK != mal.init(MAL_IDX_CFI, &cfi_dal_info)) || 
		(ERROR_OK != mal.getinfo(MAL_IDX_CFI, &cfi_dal_info)))
	{
		return ERROR_FAIL;
	}
	
	param->chip_areas[APPLICATION_IDX].page_num = 
								(uint32_t)cfi_mal_info.capacity.block_number;
	param->chip_areas[APPLICATION_IDX].page_size = 
								(uint32_t)cfi_mal_info.capacity.block_size;
	param->chip_areas[APPLICATION_IDX].size = 
								param->chip_areas[APPLICATION_IDX].page_num * 
								param->chip_areas[APPLICATION_IDX].page_size;
	LOG_INFO("CFI device detected: %dKB(%d blocks)", 
					param->chip_areas[APPLICATION_IDX].size / 1024, 
					param->chip_areas[APPLICATION_IDX].page_num);
	return dal_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(cfi)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	mal.fini(MAL_IDX_CFI, &cfi_dal_info);
	return dal_commit();
}

ERASE_TARGET_HANDLER(cfi)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (ERROR_OK != mal.eraseblock(MAL_IDX_CFI, &cfi_dal_info, addr, 1))
		{
			return ERROR_FAIL;
		}
		return dal_commit();
	default:
		return ERROR_FAIL;
	}
}

WRITE_TARGET_HANDLER(cfi)
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
		
		if (ERROR_OK != mal.writeblock(MAL_IDX_CFI, &cfi_dal_info,addr, buff, 
										size))
		{
			return ERROR_FAIL;
		}
		return dal_commit();
		break;
	default:
		return ERROR_FAIL;
	}
}

READ_TARGET_HANDLER(cfi)
{
	struct chip_param_t *param = context->param;
	
	switch (area)
	{
	case CHIPID_CHAR:
		if (ERROR_OK != mal.getinfo(MAL_IDX_CFI, &cfi_dal_info))
		{
			return ERROR_FAIL;
		}
		buff[3] = cfi_drv_info.manufacturer_id;
		buff[2] = (uint8_t)cfi_drv_info.device_id[0];
		buff[1] = (uint8_t)cfi_drv_info.device_id[1];
		buff[0] = (uint8_t)cfi_drv_info.device_id[2];
		return ERROR_OK;
		break;
	case APPLICATION_CHAR:
		if (size % param->chip_areas[APPLICATION_IDX].page_size)
		{
			return ERROR_FAIL;
		}
		size /= param->chip_areas[APPLICATION_IDX].page_size;
		
		if (ERROR_OK != mal.readblock(MAL_IDX_CFI, &cfi_dal_info, addr, buff, 
										size))
		{
			return ERROR_FAIL;
		}
		return ERROR_OK;
		break;
	default:
		return ERROR_FAIL;
	}
}

