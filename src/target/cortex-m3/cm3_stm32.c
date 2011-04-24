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
#include "cm3_stm32.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

#include "cm3_internal.h"
#include "stm32_internal.h"
#include "cm3_stm32_fl.h"

#define STM32_FL_PAGE_SIZE			1024
#define	STM32_FL_PAGE0_ADDR			(STM32_SRAM_ADDR + STM32_FL_BUFFER_OFFSET)
#define STM32_FL_PAGE1_ADDR			(STM32_FL_PAGE0_ADDR + STM32_FL_PAGE_SIZE)

ENTER_PROGRAM_MODE_HANDLER(stm32swj);
LEAVE_PROGRAM_MODE_HANDLER(stm32swj);
ERASE_TARGET_HANDLER(stm32swj);
WRITE_TARGET_HANDLER(stm32swj);
READ_TARGET_HANDLER(stm32swj);
const struct program_functions_t stm32swj_program_functions = 
{
	NULL, 
	ENTER_PROGRAM_MODE_FUNCNAME(stm32swj), 
	LEAVE_PROGRAM_MODE_FUNCNAME(stm32swj), 
	ERASE_TARGET_FUNCNAME(stm32swj), 
	WRITE_TARGET_FUNCNAME(stm32swj), 
	READ_TARGET_FUNCNAME(stm32swj)
};

ENTER_PROGRAM_MODE_HANDLER(stm32swj)
{
	uint32_t reg, flash_obr, flash_wrpr;
	
	REFERENCE_PARAMETER(context);
	
	// unlock flash and option bytes
	reg = STM32_FLASH_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32_FLASH_KEYR, &reg, 0);
	reg = STM32_FLASH_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32_FLASH_KEYR, &reg, 0);
	reg = STM32_OPT_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32_FLASH_OPTKEYR, &reg, 0);
	reg = STM32_OPT_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32_FLASH_OPTKEYR, &reg, 0);
	
	adi_memap_read_reg32(STM32_FLASH_WRPR, &flash_wrpr, 0);
	if (ERROR_OK != adi_memap_read_reg32(STM32_FLASH_OBR, &flash_obr, 1))
	{
		return ERROR_FAIL;
	}
	LOG_INFO(INFOMSG_REG_08X, "FLASH_OBR", flash_obr);
	LOG_INFO(INFOMSG_REG_08X, "FLASH_WRPR", flash_wrpr);
	
	if ((flash_obr & STM32_FLASH_OBR_RDPRT) || (flash_wrpr != 0xFFFFFFFF))
	{
		LOG_WARNING("STM32 locked, to unlock, run "
					"vsprog -cstm32_XX -mX -oeu -owu -tu0xFFFFFFFFFFFFFFA5");
	}
	
	return stm32swj_fl_init(STM32_SRAM_ADDR);
}

LEAVE_PROGRAM_MODE_HANDLER(stm32swj)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	return ERROR_OK;
}

ERASE_TARGET_HANDLER(stm32swj)
{
	struct chip_param_t *param = context->param;
	struct program_info_t *pi = context->pi;
	struct operation_t *op = context->op;
	RESULT ret= ERROR_OK;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	cmd.sr_busy_mask = STM32_FLASH_SR_BSY;
	cmd.sr_err_mask = STM32_FLASH_SR_ERRMSK;
	switch (area)
	{
	case FUSE_CHAR:
		cmd.cr_addr = STM32_FLASH_CR;
		cmd.sr_addr = STM32_FLASH_SR;
		cmd.cr_value1 = STM32_FLASH_CR_OPTER | STM32_FLASH_CR_OPTWRE;
		cmd.cr_value2 = cmd.cr_value1 | STM32_FLASH_CR_STRT;
		cmd.data_type = 0;
		cmd.data_size = 1;
		if (ERROR_OK != stm32swj_fl_call(&cmd, &result, true))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		// if fuse write will not be performed, 
		// we MUST write a default non-lock value(0xFFFFFFFFFFFFFFA5) to fuse, 
		// or STM32 will be locked
		if (!(op->write_operations & FUSE))
		{
			uint64_t fuse = 0xFFFFFFFFFFFFFFA5;
			
			// TODO: fix here for big-endian
			memcpy(pi->program_areas[FUSE_IDX].buff, &fuse, 8);
			op->write_operations |= FUSE;
		}
		break;
	case APPLICATION_CHAR:
		cmd.cr_addr = STM32_FLASH_CR;
		cmd.sr_addr = STM32_FLASH_SR;
		cmd.cr_value1 = STM32_FLASH_CR_MER;
		cmd.cr_value2 = cmd.cr_value1 | STM32_FLASH_CR_STRT;
		cmd.data_type = 0;
		cmd.data_size = 1;
		if (ERROR_OK != stm32swj_fl_call(&cmd, &result, true))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		if (param->chip_areas[APPLICATION_IDX].size > STM32_FLASH_BANK_SIZE)
		{
			cmd.cr_addr = STM32_FLASH_CR2;
			cmd.sr_addr = STM32_FLASH_SR2;
			if (ERROR_OK != stm32swj_fl_call(&cmd, &result, true))
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

WRITE_TARGET_HANDLER(stm32swj)
{
	uint8_t i;
	uint8_t fuse_buff[STM32_OB_SIZE];
	uint8_t tick_tock;
	uint32_t cur_size;
	RESULT ret = ERROR_OK;
	struct stm32_fl_cmd_t cmd;
	struct stm32_fl_result_t result;
	bool last;
	
	REFERENCE_PARAMETER(context);
	
	cmd.sr_busy_mask = STM32_FLASH_SR_BSY;
	cmd.sr_err_mask = STM32_FLASH_SR_ERRMSK;
	switch (area)
	{
	case FUSE_CHAR:
		if (size != STM32_OB_SIZE / 2)
		{
			return ERROR_FAIL;
		}
		
		for (i = 0; i < STM32_OB_SIZE / 2; i++)
		{
			fuse_buff[2 * i] = buff[i];
			fuse_buff[2 * i + 1] = ~buff[i];
		}
		
		cmd.cr_addr = STM32_FLASH_CR;
		cmd.sr_addr = STM32_FLASH_SR;
		cmd.cr_value1 = STM32_FLASH_CR_OPTPG | STM32_FLASH_CR_OPTWRE;
		cmd.cr_value2 = 0;
		cmd.target_addr = STM32_OB_ADDR;
		cmd.ram_addr = STM32_FL_PAGE0_ADDR;
		cmd.data_type = 2;
		cmd.data_size = STM32_OB_SIZE / 2;
		if ((ERROR_OK != adi_memap_write_buf(cmd.ram_addr, fuse_buff, 
												STM32_OB_SIZE)) || 
			(ERROR_OK != stm32swj_fl_call(&cmd, &result, true)))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	case APPLICATION_CHAR:
		last = false;
		if (size <= STM32_FL_PAGE_SIZE)
		{
			cur_size = size;
			last = true;
		}
		else
		{
			cur_size = STM32_FL_PAGE_SIZE;
		}
		if (addr >= STM32_FLASH_BANK2_ADDR)
		{
			cmd.cr_addr = STM32_FLASH_CR2;
			cmd.sr_addr = STM32_FLASH_SR2;
		}
		else
		{
			cmd.cr_addr = STM32_FLASH_CR;
			cmd.sr_addr = STM32_FLASH_SR;
		}
		cmd.cr_value1 = STM32_FLASH_CR_PG;
		cmd.cr_value2 = 0;
		cmd.target_addr = addr;
		cmd.ram_addr = STM32_FL_PAGE0_ADDR;
		cmd.data_type = 2;
		cmd.data_size = cur_size / 2;
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
			if (size <= STM32_FL_PAGE_SIZE)
			{
				cur_size = size;
				last = true;
			}
			else
			{
				cur_size = STM32_FL_PAGE_SIZE;
			}
			if (addr >= STM32_FLASH_BANK2_ADDR)
			{
				cmd.cr_addr = STM32_FLASH_CR2;
				cmd.sr_addr = STM32_FLASH_SR2;
			}
			else
			{
				cmd.cr_addr = STM32_FLASH_CR;
				cmd.sr_addr = STM32_FLASH_SR;
			}
			cmd.target_addr = addr;
			if (tick_tock & 1)
			{
				cmd.ram_addr = STM32_FL_PAGE1_ADDR;
			}
			else
			{
				cmd.ram_addr = STM32_FL_PAGE0_ADDR;
			}
			cmd.data_size = cur_size / 2;
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

READ_TARGET_HANDLER(stm32swj)
{
	struct program_info_t *pi = context->pi;
	uint8_t option_bytes[STM32_OB_SIZE], i;
	uint32_t mcu_id = 0, flash_sram_size, flash_obr;
	uint32_t cur_block_size;
	uint16_t den, flash_size;
	RESULT ret = ERROR_OK;
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read MCU ID at STM32_REG_MCU_ID
		if (ERROR_OK != adi_memap_read_reg32(STM32_REG_MCU_ID, &mcu_id, 1))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		mcu_id = LE_TO_SYS_U32(mcu_id);
		den = mcu_id & STM32_DEN_MSK;
		stm32_print_device(mcu_id);
		mcu_id &= STM32_DEN_MSK;
		*(uint32_t *)buff = mcu_id;
		
		if (ERROR_OK != adi_memap_read_reg32(STM32_FLASH_OBR, &flash_obr, 1))
		{
			return ERROR_FAIL;
		}
		if (flash_obr & STM32_FLASH_OBR_RDPRT)
		{
			// read protected, flash size and sram size is not readable
			return ERROR_OK;
		}
		
		// read flash and ram size
		if (ERROR_OK != 
			adi_memap_read_reg32(STM32_REG_FLASH_RAM_SIZE, &flash_sram_size, 1))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read stm32 flash_ram size");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		flash_sram_size = LE_TO_SYS_U32(flash_sram_size);
		flash_size = stm32_get_flash_size(mcu_id, flash_sram_size);
		pi->program_areas[APPLICATION_IDX].size = flash_size * 1024;
		
		LOG_INFO("Flash memory size: %i KB", flash_size);
		if ((flash_sram_size >> 16) != 0xFFFF)
		{
			LOG_INFO("SRAM memory size: %i KB", flash_sram_size >> 16);
		}
		break;
	case FUSE_CHAR:
		if (ERROR_OK != 
				adi_memap_read_buf(STM32_OB_ADDR, option_bytes, STM32_OB_SIZE))
		{
			return ERROR_FAIL;
		}
		for (i = 0; i < size; i++)
		{
			buff[i] = option_bytes[i * 2];
		}
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
		if ((ERROR_OK != adi_memap_read_reg32(STM32_UID_ADDR + 0, 
											(((uint32_t *)buff) + 0), 0)) || 
			(ERROR_OK != adi_memap_read_reg32(STM32_UID_ADDR + 4, 
											(((uint32_t *)buff) + 1), 0)) || 
			(ERROR_OK != adi_memap_read_reg32(STM32_UID_ADDR + 8, 
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

