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

#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"
#include "app_scripts.h"

#include "dal/mal/mal.h"
#include "dal/nand/nand_drv.h"

#include "nand.h"
#include "nand_internal.h"

#define CUR_TARGET_STRING			NAND_STRING

struct program_area_map_t nand_program_area_map[] =
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t nand_program_mode[] =
{
	{'*', "", IFS_EBI},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(nand);
LEAVE_PROGRAM_MODE_HANDLER(nand);
ERASE_TARGET_HANDLER(nand);
WRITE_TARGET_HANDLER(nand);
READ_TARGET_HANDLER(nand);
const struct program_functions_t nand_program_functions =
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(nand),
	LEAVE_PROGRAM_MODE_FUNCNAME(nand),
	ERASE_TARGET_FUNCNAME(nand),
	WRITE_TARGET_FUNCNAME(nand),
	READ_TARGET_FUNCNAME(nand)
};

VSS_HANDLER(nand_help)
{
	VSS_CHECK_ARGC(1);
	PRINTF("Usage of %s:\n\n", CUR_TARGET_STRING);
	return VSFERR_NONE;
}

const struct vss_cmd_t nand_notifier[] =
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				nand_help,
				NULL),
	VSS_CMD_END
};






static struct nand_drv_info_t nand_drv_info;
static struct nand_drv_param_t nand_drv_param;
static struct nand_drv_interface_t nand_drv_ifs;
static struct mal_info_t nand_mal_info =
{
	{0, 0}, NULL, 0, 0, 0
};
static struct dal_info_t nand_dal_info =
{
	&nand_drv_ifs,
	&nand_drv_param,
	&nand_drv_info,
	&nand_mal_info,
};

ENTER_PROGRAM_MODE_HANDLER(nand)
{
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	
	if (pi->ifs_indexes != NULL)
	{
		if (dal_config_interface(NAND_STRING, pi->ifs_indexes, &nand_dal_info))
		{
			return VSFERR_FAIL;
		}
	}
	else
	{
		nand_drv_ifs.ebi_port = 0;
		nand_drv_ifs.nand_index = 1;
	}
	nand_mal_info.capacity.block_number = 
								param->chip_areas[APPLICATION_IDX].page_num;
	nand_mal_info.capacity.block_size = 
								param->chip_areas[APPLICATION_IDX].page_size;
	nand_mal_info.read_page_size = param->param[NAND_PARAM_READ_PAGE_SIZE];
	nand_mal_info.write_page_size = param->param[NAND_PARAM_WRITE_PAGE_SIZE];
	nand_mal_info.erase_page_size = param->param[NAND_PARAM_ERASE_PAGE_SIZE];
	
	nand_drv_param.nand_info.common_info.data_width = 8;
	nand_drv_param.nand_info.common_info.wait_signal = EBI_WAIT_NONE;
	nand_drv_param.nand_info.param.clock_hz = 0;
	nand_drv_param.nand_info.param.ecc.ecc_enable = true;
	nand_drv_param.nand_info.param.ecc.ecc_page_size = 512;
	nand_drv_param.nand_info.param.timing.ale_to_re_cycle = 1;
	nand_drv_param.nand_info.param.timing.cle_to_re_cycle = 1;
	nand_drv_param.nand_info.param.timing.setup_cycle = 10;
	nand_drv_param.nand_info.param.timing.wait_cycle = 10;
	nand_drv_param.nand_info.param.timing.hold_cycle = 10;
	nand_drv_param.nand_info.param.timing.hiz_cycle = 10;
	nand_drv_param.nand_info.param.timing.setup_cycle_attr = 10;
	nand_drv_param.nand_info.param.timing.wait_cycle_attr = 10;
	nand_drv_param.nand_info.param.timing.hold_cycle_attr = 10;
	nand_drv_param.nand_info.param.timing.hiz_cycle_attr = 10;
	nand_drv_param.nand_info.param.addr.cmd = 0x00010000;
	nand_drv_param.nand_info.param.addr.addr = 0x00020000;
	nand_drv_param.nand_info.param.addr.data = 0x00000000;
	nand_drv_param.small_page = param->param[NAND_PARAM_SMALL_PAGE];
	
	if (mal.init(MAL_IDX_NAND, &nand_dal_info) || 
		mal.getinfo(MAL_IDX_NAND, &nand_dal_info))
	{
		return VSFERR_FAIL;
	}
	
	return dal_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(nand)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	mal.fini(MAL_IDX_NAND, &nand_dal_info);
	return dal_commit();
}

ERASE_TARGET_HANDLER(nand)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (mal.eraseblock(MAL_IDX_NAND, &nand_dal_info, addr, 1))
		{
			return VSFERR_FAIL;
		}
		return dal_commit();
	default:
		return VSFERR_FAIL;
	}
}

WRITE_TARGET_HANDLER(nand)
{
	struct chip_param_t *param = context->param;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (size % param->chip_areas[APPLICATION_IDX].page_size)
		{
			return VSFERR_FAIL;
		}
		size /= param->chip_areas[APPLICATION_IDX].page_size;
		
		if (mal.writeblock(MAL_IDX_NAND, &nand_dal_info,addr, buff, size))
		{
			return VSFERR_FAIL;
		}
		return dal_commit();
		break;
	default:
		return VSFERR_FAIL;
	}
}

READ_TARGET_HANDLER(nand)
{
	struct chip_param_t *param = context->param;
	
	switch (area)
	{
	case CHIPID_CHAR:
		if (mal.getinfo(MAL_IDX_NAND, &nand_dal_info))
		{
			return VSFERR_FAIL;
		}
		buff[3] = nand_drv_info.manufacturer_id;
		buff[2] = (uint8_t)nand_drv_info.device_id[0];
		buff[1] = (uint8_t)nand_drv_info.device_id[1];
		buff[0] = (uint8_t)nand_drv_info.device_id[2];
		return VSFERR_NONE;
		break;
	case APPLICATION_CHAR:
		if (size % param->chip_areas[APPLICATION_IDX].page_size)
		{
			return VSFERR_FAIL;
		}
		size /= param->chip_areas[APPLICATION_IDX].page_size;
		
		if (mal.readblock(MAL_IDX_NAND, &nand_dal_info, addr, buff, size))
		{
			return VSFERR_FAIL;
		}
		return VSFERR_NONE;
		break;
	default:
		return VSFERR_FAIL;
	}
}

