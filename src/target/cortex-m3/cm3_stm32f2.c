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

#include "port.h"
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

#include "cm3.h"
#include "cm3_stm32f2.h"

#include "cm3_internal.h"
#include "stm32f2_internal.h"

#include "adi_v5p1.h"
#include "cm3_common.h"
#include "cm3_stm32_fl.h"

#define STM32F2_FL_PAGE_SIZE	1024
#define	STM32F2_FL_PAGE0_ADDR	(STM32F2_SRAM_ADDR + STM32_FL_BUFFER_OFFSET)
#define STM32F2_FL_PAGE1_ADDR	(STM32F2_FL_PAGE0_ADDR + STM32F2_FL_PAGE_SIZE)

ENTER_PROGRAM_MODE_HANDLER(stm32f2swj);
LEAVE_PROGRAM_MODE_HANDLER(stm32f2swj);
ERASE_TARGET_HANDLER(stm32f2swj);
WRITE_TARGET_HANDLER(stm32f2swj);
READ_TARGET_HANDLER(stm32f2swj);
const struct program_functions_t stm32f2swj_program_functions = 
{
	NULL, 
	ENTER_PROGRAM_MODE_FUNCNAME(stm32f2swj), 
	LEAVE_PROGRAM_MODE_FUNCNAME(stm32f2swj), 
	ERASE_TARGET_FUNCNAME(stm32f2swj), 
	WRITE_TARGET_FUNCNAME(stm32f2swj), 
	READ_TARGET_FUNCNAME(stm32f2swj)
};

ENTER_PROGRAM_MODE_HANDLER(stm32f2swj)
{
	uint32_t reg;
	
	REFERENCE_PARAMETER(context);
	
	// unlock flash and option bytes
	reg = STM32F2_FLASH_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32F2_FLASH_KEYR, &reg, 0);
	reg = STM32F2_FLASH_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32F2_FLASH_KEYR, &reg, 0);
	reg = STM32F2_OPT_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32F2_FLASH_OPTKEYR, &reg, 0);
	reg = STM32F2_OPT_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32F2_FLASH_OPTKEYR, &reg, 0);
	
	if (ERROR_OK != adi_memap_read_reg32(STM32F2_FLASH_OPTCR, &reg, 1))
	{
		return ERROR_FAIL;
	}
	if (((reg & STM32F2_FLASH_OPT_RDP_MASK) != STM32F2_FLASH_OPT_RDP_LVL0) || 
		((reg & STM32F2_FLASH_OPT_WRP_MASK) != STM32F2_FLASH_OPT_WRP_MASK))
	{
		LOG_WARNING("STM32F2 locked, to unlock, run "
					"vsprog -cstm32f2_XX -mX -oeu -owu -tu0xFFFFAAFF");
	}
	
	return stm32swj_fl_init(STM32F2_SRAM_ADDR);
}

LEAVE_PROGRAM_MODE_HANDLER(stm32f2swj)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	return ERROR_OK;
}

ERASE_TARGET_HANDLER(stm32f2swj)
{
	RESULT ret= ERROR_OK;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	cmd.sr_busy_mask = STM32F2_FLASH_SR_BSY;
	cmd.sr_err_mask = STM32F2_FLASH_SR_ERRMSK;
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		cmd.cr_addr = STM32F2_FLASH_CR;
		cmd.sr_addr = STM32F2_FLASH_SR;
		cmd.cr_value1 = STM32F2_FLASH_CR_MER;
		cmd.cr_value2 = cmd.cr_value1 | STM32F2_FLASH_CR_STRT;
		cmd.data_type = 0;
		cmd.data_size = 1;
		if (ERROR_OK != stm32swj_fl_call(&cmd, &result, true))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

WRITE_TARGET_HANDLER(stm32f2swj)
{
	uint8_t tick_tock;
	uint32_t cur_size;
	RESULT ret = ERROR_OK;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
	bool last;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case FUSE_CHAR:
		cmd.cr_addr = STM32F2_FLASH_OPTCR;
		cmd.sr_addr = STM32F2_FLASH_SR;
		cmd.cr_value1 = GET_LE_U32(buff);
		cmd.cr_value2 = cmd.cr_value1 | STM32F2_FLASH_OPTCR_START;
		cmd.target_addr = 0;
		cmd.ram_addr = 0;
		cmd.data_type = 0;
		cmd.data_size = 1;
		if ((ERROR_OK != adi_memap_write_buf(cmd.ram_addr, buff, 4)) || 
			(ERROR_OK != stm32swj_fl_call(&cmd, &result, true)))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	case APPLICATION_CHAR:
		last = false;
		if (size <= STM32F2_FL_PAGE_SIZE)
		{
			cur_size = size;
			last = true;
		}
		else
		{
			cur_size = STM32F2_FL_PAGE_SIZE;
		}
		cmd.cr_addr = STM32F2_FLASH_CR;
		cmd.sr_addr = STM32F2_FLASH_SR;
		cmd.cr_value1 = STM32F2_FLASH_CR_PG | STM32F2_FLASH_CR_PSIZE_32;
		cmd.cr_value2 = 0;
		cmd.target_addr = addr;
		cmd.ram_addr = STM32F2_FL_PAGE0_ADDR;
		cmd.data_type = 4;
		cmd.data_size = cur_size / 4;
		if ((ERROR_OK != adi_memap_write_buf(cmd.ram_addr, buff, cur_size)) || 
			(ERROR_OK != stm32swj_fl_call(&cmd, &result, last)))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		size -= cur_size;
		buff += cur_size;
		addr += cur_size;
		pgbar_update(cur_size);
		tick_tock = 1;
		
		while (size)
		{
			if (size <= STM32F2_FL_PAGE_SIZE)
			{
				cur_size = size;
				last = true;
			}
			else
			{
				cur_size = STM32F2_FL_PAGE_SIZE;
			}
			cmd.target_addr = addr;
			if (tick_tock & 1)
			{
				cmd.ram_addr = STM32F2_FL_PAGE1_ADDR;
			}
			else
			{
				cmd.ram_addr = STM32F2_FL_PAGE0_ADDR;
			}
			if ((ERROR_OK != 
					adi_memap_write_buf(cmd.ram_addr, buff, cur_size)) || 
				(ERROR_OK != stm32swj_fl_call(&cmd, &result, last)))
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			
			size -= cur_size;
			buff += cur_size;
			addr += cur_size;
			pgbar_update(cur_size);
			tick_tock++;
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	
	return ret;
}

READ_TARGET_HANDLER(stm32f2swj)
{
	uint32_t reg;
	uint32_t cur_block_size;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read MCU ID at STM32_REG_MCU_ID
		if (ERROR_OK != adi_memap_read_reg32(STM32F2_REG_MCU_ID, &reg, 1))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		reg = LE_TO_SYS_U32(reg);
		stm32f2_print_device(reg);
		reg &= STM32F2_DEN_MSK;
		*(uint32_t *)buff = reg;
		break;
	case FUSE_CHAR:
		if (ERROR_OK != adi_memap_read_reg32(STM32F2_FLASH_OPTCR, &reg, 1))
		{
			return ERROR_FAIL;
		}
		reg &= STM32F2_FLASH_OPT_MASK;
		memcpy(buff, &reg, 4);
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
	case UNIQUEID_CHAR:
		if ((ERROR_OK != adi_memap_read_reg32(STM32F2_UID_ADDR + 0, 
											(((uint32_t *)buff) + 0), 0)) || 
			(ERROR_OK != adi_memap_read_reg32(STM32F2_UID_ADDR + 4, 
											(((uint32_t *)buff) + 1), 0)) || 
			(ERROR_OK != adi_memap_read_reg32(STM32F2_UID_ADDR + 8, 
											(((uint32_t *)buff) + 2), 1)))
		{
			ret = ERROR_FAIL;
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

