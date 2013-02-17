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

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"
#include "scripts.h"

#include "pgbar.h"

#include "cm.h"
#include "cm_kinetis.h"

#include "adi_v5p1.h"
#include "cm_common.h"

#include "cm_internal.h"
#include "kinetis_internal.h"

ENTER_PROGRAM_MODE_HANDLER(kinetisswj);
LEAVE_PROGRAM_MODE_HANDLER(kinetisswj);
ERASE_TARGET_HANDLER(kinetisswj);
WRITE_TARGET_HANDLER(kinetisswj);
READ_TARGET_HANDLER(kinetisswj);
const struct program_functions_t kinetisswj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(kinetisswj),
	LEAVE_PROGRAM_MODE_FUNCNAME(kinetisswj),
	ERASE_TARGET_FUNCNAME(kinetisswj),
	WRITE_TARGET_FUNCNAME(kinetisswj),
	READ_TARGET_FUNCNAME(kinetisswj)
};

ENTER_PROGRAM_MODE_HANDLER(kinetisswj)
{
	struct chip_area_info_t *sram_info = NULL;
//	uint32_t reg, flash_obr, flash_wrpr;
	
	sram_info = target_get_chip_area(context->param, SRAM_IDX);
	if (NULL == sram_info)
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(kinetisswj)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	return VSFERR_NONE;
}

ERASE_TARGET_HANDLER(kinetisswj)
{
	struct chip_area_info_t *flash_info = NULL;
	vsf_err_t err = VSFERR_NONE;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if (NULL == flash_info)
		{
			return VSFERR_FAIL;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

WRITE_TARGET_HANDLER(kinetisswj)
{
	struct chip_area_info_t *flash_info = NULL;
//	static uint8_t tick_tock = 0;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		flash_info = target_get_chip_area(context->param, APPLICATION_IDX);
		if ((NULL == flash_info) || (size != flash_info->page_size))
		{
			return VSFERR_FAIL;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	
	return err;
}

READ_TARGET_HANDLER(kinetisswj)
{
	struct program_info_t *pi = context->pi;
	struct program_area_t *sram_area = NULL, *flash_area = NULL;
	uint32_t mcuid = 0, fcfg1 = 0;
	uint32_t cur_block_size;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case CHIPID_CHAR:
		if ((adi_memap_read_reg32(KINETIS_SIM_SDID, &mcuid, 1)) ||
			(adi_memap_read_reg32(KINETIS_SIM_FCFG1, &fcfg1, 1)))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint32_t *)buff = mcuid;
		kinetis_print_device(mcuid, fcfg1);
		
		sram_area = target_get_program_area(pi, SRAM_IDX);
		if (sram_area != NULL)
		{
			sram_area->size = kinetis_get_sram_size(mcuid);
		}
		
		flash_area = target_get_program_area(pi, APPLICATION_IDX);
		if (flash_area != NULL)
		{
			flash_area->size = kinetis_get_flash_size(fcfg1);
		}
		break;
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		while (size)
		{
			// cm_get_max_block_size return size in dword(4-byte)
			cur_block_size = cm_get_max_block_size(addr);
			if (cur_block_size > (size >> 2))
			{
				cur_block_size = size;
			}
			else
			{
				cur_block_size <<= 2;
			}
			if (adi_memap_read_buf(addr, buff, cur_block_size))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "read flash block",
							addr);
				err = ERRCODE_FAILURE_OPERATION_ADDR;
				break;
			}
			
			size -= cur_block_size;
			addr += cur_block_size;
			buff += cur_block_size;
		}
		break;
	case UNIQUEID_CHAR:
		if (adi_memap_read_buf(KINETIS_SIM_UID, buff, 12))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

