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

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "memlist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "cm3.h"
#include "cm3_lm3s.h"

#include "cm3_internal.h"
#include "lm3s_internal.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

#include "timer.h"

ENTER_PROGRAM_MODE_HANDLER(lm3sswj);
LEAVE_PROGRAM_MODE_HANDLER(lm3sswj);
ERASE_TARGET_HANDLER(lm3sswj);
WRITE_TARGET_HANDLER(lm3sswj);
READ_TARGET_HANDLER(lm3sswj);
const struct program_functions_t lm3sswj_program_functions = 
{
	NULL, 
	ENTER_PROGRAM_MODE_FUNCNAME(lm3sswj), 
	LEAVE_PROGRAM_MODE_FUNCNAME(lm3sswj), 
	ERASE_TARGET_FUNCNAME(lm3sswj), 
	WRITE_TARGET_FUNCNAME(lm3sswj), 
	READ_TARGET_FUNCNAME(lm3sswj)
};

ENTER_PROGRAM_MODE_HANDLER(lm3sswj)
{
	uint32_t reg;
	
	REFERENCE_PARAMETER(context);
	
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt lm3s");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// disable flash programming interrupts
	reg = 0;
	if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FCIM, &reg, 0))
	{
		return ERROR_FAIL;
	}
	reg = LM3S_FLASHCTL_INT_PROGRAMMING | LM3S_FLASHCTL_INT_ACCESS;
	if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FCMISC, &reg, 1))
	{
		return ERROR_FAIL;
	}
	
	// unlock
	// 0xFFFFFFFF to FMPRE and FMPPE
	reg = 0xFFFFFFFF;
	if (ERROR_OK != adi_memap_write_reg(LM3S_SYSCTL_FMPRE, &reg, 0))
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != adi_memap_write_reg(LM3S_SYSCTL_FMPPE, &reg, 1))
	{
		return ERROR_FAIL;
	}
	// commit FMPRE
	reg = 0;
	if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FMA, &reg, 1))
	{
		return ERROR_FAIL;
	}
	reg = LM3S_FLASHCTL_FMC_COMT;
	if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FMC, &reg, 1))
	{
		return ERROR_FAIL;
	}
	// commit EMPPE
	reg = 1;
	if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FMA, &reg, 1))
	{
		return ERROR_FAIL;
	}
	reg = LM3S_FLASHCTL_FMC_COMT;
	if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FMC, &reg, 1))
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

LEAVE_PROGRAM_MODE_HANDLER(lm3sswj)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	return ERROR_OK;
}

ERASE_TARGET_HANDLER(lm3sswj)
{
	uint32_t reg;
	uint32_t retry;
	RESULT ret= ERROR_OK;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		// halt target first
		if (ERROR_OK != cm3_dp_halt())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt lm3");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		reg = LM3S_FLASHCTL_FMC_KEY | LM3S_FLASHCTL_FMC_MERASE;
		if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FMC, &reg, 1))
		{
			ret = ERROR_FAIL;
			break;
		}
		retry = 100;				// max delay is 1S
		while (reg & LM3S_FLASHCTL_FMC_MERASE)
		{
			if (ERROR_OK != adi_memap_read_reg(LM3S_FLASHCTL_FMC, &reg, 1))
			{
				ret = ERROR_FAIL;
				break;
			}
			if (0 == --retry)
			{
				// timeout
				ret = ERROR_FAIL;
				break;
			}
			sleep_ms(10);
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

WRITE_TARGET_HANDLER(lm3sswj)
{
	uint32_t reg;
	uint32_t retry;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (size % 4)
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "size value");
			return ERROR_FAIL;
		}
		
		while (size)
		{
			reg = *(uint32_t*)buff;
			if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FMD, &reg, 0))
			{
				ret = ERROR_FAIL;
				break;
			}
			reg = addr;
			if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FMA, &reg, 0))
			{
				ret = ERROR_FAIL;
				break;
			}
			reg = LM3S_FLASHCTL_FMC_KEY | LM3S_FLASHCTL_FMC_WRITE;
			if (ERROR_OK != adi_memap_write_reg(LM3S_FLASHCTL_FMC, &reg, 0))
			{
				ret = ERROR_FAIL;
				break;
			}
			retry = 1000;
			while (reg & LM3S_FLASHCTL_FMC_WRITE)
			{
				if (ERROR_OK != adi_memap_read_reg(LM3S_FLASHCTL_FMC, &reg, 1))
				{
					ret = ERROR_FAIL;
					break;
				}
				if (0 == --retry)
				{
					// timeout
					ret = ERROR_FAIL;
					break;
				}
			}
			size -= 4;
			buff += 4;
			addr += 4;
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	
	return ret;
}

READ_TARGET_HANDLER(lm3sswj)
{
	struct lm3s_device_info_t lm3s_device;
	uint32_t cur_block_size;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		if ((ERROR_OK != 
				adi_memap_read_reg(LM3S_SYSCTL_DID0, &lm3s_device.did0, 0)) 
			|| (ERROR_OK != 
				adi_memap_read_reg(LM3S_SYSCTL_DID1, &lm3s_device.did1, 0)) 
			|| (ERROR_OK != 
				adi_memap_read_reg(LM3S_SYSCTL_DC0, &lm3s_device.dc0, 0)) 
			|| (ERROR_OK != 
				adi_memap_read_reg(LM3S_SYSCTL_DC1, &lm3s_device.dc1, 0)) 
			|| (ERROR_OK != 
				adi_memap_read_reg(LM3S_SYSCTL_DC2, &lm3s_device.dc2, 0)) 
			|| (ERROR_OK != 
				adi_memap_read_reg(LM3S_SYSCTL_DC3, &lm3s_device.dc3, 1)) 
			|| (ERROR_OK != lm3s_check_device(&lm3s_device)))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint32_t*)buff = (lm3s_device.did1 >> 16) & 0xFFFF;
		break;
	case APPLICATION_CHAR:
		while (size)
		{
			// cm3_get_max_block_size return size in dword(4-byte)
			cur_block_size = cm3_get_max_block_size(addr);
			if (cur_block_size > (size >> 2))
			{
				cur_block_size = size;
			}
			else
			{
				cur_block_size <<= 2;
			}
			if (ERROR_OK != adi_memap_read_buf(addr, buff, 
												   cur_block_size))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "write flash block", 
							addr);
				ret = ERRCODE_FAILURE_OPERATION_ADDR;
				break;
			}
			
			size -= cur_block_size;
			addr += cur_block_size;
			buff += cur_block_size;
			pgbar_update(cur_block_size);
		}
		break;
	default:
		ret = ERROR_OK;
		break;
	}
	return ret;
}

