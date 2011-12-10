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

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"

#include "pgbar.h"

#include "cm3.h"
#include "cm3_stm32l1.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

#include "cm3_internal.h"
#include "stm32l1_internal.h"

ENTER_PROGRAM_MODE_HANDLER(stm32l1swj);
LEAVE_PROGRAM_MODE_HANDLER(stm32l1swj);
ERASE_TARGET_HANDLER(stm32l1swj);
WRITE_TARGET_HANDLER(stm32l1swj);
READ_TARGET_HANDLER(stm32l1swj);
const struct program_functions_t stm32l1swj_program_functions =
{
	NULL,
	ENTER_PROGRAM_MODE_FUNCNAME(stm32l1swj),
	LEAVE_PROGRAM_MODE_FUNCNAME(stm32l1swj),
	ERASE_TARGET_FUNCNAME(stm32l1swj),
	WRITE_TARGET_FUNCNAME(stm32l1swj),
	READ_TARGET_FUNCNAME(stm32l1swj)
};

ENTER_PROGRAM_MODE_HANDLER(stm32l1swj)
{
	uint32_t reg, flash_obr, flash_wrpr;
	
	REFERENCE_PARAMETER(context);
	
	// unlock flash and option bytes
	reg = STM32L1_EE_PECR_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32L1_FLASH_PEKEYR, &reg, 0);
	reg = STM32L1_EE_PECR_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32L1_FLASH_PEKEYR, &reg, 0);
	reg = STM32L1_FLASH_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32L1_FLASH_PRGKEYR, &reg, 0);
	reg = STM32L1_FLASH_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32L1_FLASH_PRGKEYR, &reg, 0);
	reg = STM32L1_OPT_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32L1_FLASH_OPTKEYR, &reg, 0);
	reg = STM32L1_OPT_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32L1_FLASH_OPTKEYR, &reg, 0);
	
	adi_memap_read_reg32(STM32L1_FLASH_WRPR, &flash_wrpr, 0);
	if (adi_memap_read_reg32(STM32L1_FLASH_OBR, &flash_obr, 1))
	{
		return VSFERR_FAIL;
	}
	LOG_INFO(INFOMSG_REG_08X, "FLASH_OBR", flash_obr);
	LOG_INFO(INFOMSG_REG_08X, "FLASH_WRPR", flash_wrpr);
	
	if (((flash_obr & STM32L1_FLASH_OBR_RDPRT) != STM32L1_FLASH_OBR_RDPRT_LV0) ||
		(flash_wrpr != 0))
	{
		LOG_WARNING("STM32L1 locked");
	}
	
	return VSFERR_NONE;
}

LEAVE_PROGRAM_MODE_HANDLER(stm32l1swj)
{
	uint32_t reg;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	// wait busy
	do {
		if (adi_memap_read_reg32(STM32L1_FLASH_SR, &reg, 1))
		{
			return VSFERR_FAIL;
		}
	} while (reg & STM32L1_FLASH_SR_BSY);
	if (reg & STM32L1_FLASH_SR_ERRMSK)
	{
		return VSFERR_FAIL;
	}
	
	return VSFERR_NONE;
}

ERASE_TARGET_HANDLER(stm32l1swj)
{
	vsf_err_t err = VSFERR_NONE;
	uint32_t reg;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		// wait busy
		do {
			if (adi_memap_read_reg32(STM32L1_FLASH_SR, &reg, 1))
			{
				return VSFERR_FAIL;
			}
		} while (reg & STM32L1_FLASH_SR_BSY);
		if (reg & STM32L1_FLASH_SR_ERRMSK)
		{
			return VSFERR_FAIL;
		}
		
		reg = STM32L1_FLASH_PECR_ERASE;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = STM32L1_FLASH_PECR_ERASE | STM32L1_FLASH_PECR_PROG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = 0;
		if (adi_memap_write_reg32(addr, &reg, 1))
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

WRITE_TARGET_HANDLER(stm32l1swj)
{
	struct chip_param_t *param = context->param;
	vsf_err_t err = VSFERR_NONE;
	uint32_t reg;
	
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		if (size != param->chip_areas[APPLICATION_IDX].page_size)
		{
			return VSFERR_FAIL;
		}
		
		// wait busy
		do {
			if (adi_memap_read_reg32(STM32L1_FLASH_SR, &reg, 1))
			{
				return VSFERR_FAIL;
			}
		} while (reg & STM32L1_FLASH_SR_BSY);
		if (reg & STM32L1_FLASH_SR_ERRMSK)
		{
			return VSFERR_FAIL;
		}
		
		reg = STM32L1_FLASH_PECR_FPRG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = STM32L1_FLASH_PECR_FPRG | STM32L1_FLASH_PECR_PROG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = 0;
		if (adi_memap_write_buf(addr, buff, size / 2))
		{
			return VSFERR_FAIL;
		}
		addr += size / 2;
		buff += size / 2;
		
		// wait busy
		do {
			if (adi_memap_read_reg32(STM32L1_FLASH_SR, &reg, 1))
			{
				return VSFERR_FAIL;
			}
		} while (reg & STM32L1_FLASH_SR_BSY);
		if (reg & STM32L1_FLASH_SR_ERRMSK)
		{
			return VSFERR_FAIL;
		}
		
		reg = STM32L1_FLASH_PECR_FPRG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = STM32L1_FLASH_PECR_FPRG | STM32L1_FLASH_PECR_PROG;
		adi_memap_write_reg32(STM32L1_FLASH_PECR, &reg, 0);
		reg = 0;
		if (adi_memap_write_buf(addr, buff, size / 2))
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

READ_TARGET_HANDLER(stm32l1swj)
{
	struct program_info_t *pi = context->pi;
	uint32_t mcu_id = 0, flash_sram_size, flash_obr;
	uint32_t cur_block_size;
	uint16_t flash_size, sram_size;
	vsf_err_t err = VSFERR_NONE;
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read MCU ID at STM32L1_REG_MCU_ID
		if (adi_memap_read_reg32(STM32L1_REG_MCU_ID, &mcu_id, 1))
		{
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		mcu_id = LE_TO_SYS_U32(mcu_id);
		stm32l1_print_device(mcu_id);
		mcu_id &= STM32L1_DEN_MSK;
		*(uint32_t *)buff = mcu_id;
		
		if (adi_memap_read_reg32(STM32L1_FLASH_OBR, &flash_obr, 1))
		{
			return VSFERR_FAIL;
		}
		if ((flash_obr & STM32L1_FLASH_OBR_RDPRT) !=
				STM32L1_FLASH_OBR_RDPRT_LV0)
		{
			// read protected, flash size and sram size is not readable
			return VSFERR_NONE;
		}
		
		// read flash and ram size
		if (adi_memap_read_reg32(STM32L1_REG_FLASH_RAM_SIZE, &flash_sram_size, 1))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read stm32l1 flash_ram size");
			err = ERRCODE_FAILURE_OPERATION;
			break;
		}
		flash_sram_size = LE_TO_SYS_U32(flash_sram_size);
		flash_size = stm32l1_get_flash_size(mcu_id, flash_sram_size);
		sram_size = flash_sram_size >> 16;
		pi->program_areas[APPLICATION_IDX].size = flash_size * 1024;
		
		LOG_INFO("Flash memory size: %i KB", flash_size);
		if ((sram_size != 0xFFFF) && (sram_size != 0))
		{
			LOG_INFO("SRAM memory size: %i KB", flash_sram_size >> 16);
		}
		break;
	case FUSE_CHAR:
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
			if (adi_memap_read_buf(addr, buff, cur_block_size))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION_ADDR, "write flash block",
							addr);
				err = ERRCODE_FAILURE_OPERATION_ADDR;
				break;
			}
			
			size -= cur_block_size;
			addr += cur_block_size;
			buff += cur_block_size;
			pgbar_update(cur_block_size);
		}
		break;
	case UNIQUEID_CHAR:
		if (adi_memap_read_reg32(STM32L1_UID_ADDR + 0,
									(((uint32_t *)buff) + 0), 0) ||
			adi_memap_read_reg32(STM32L1_UID_ADDR + 4,
									(((uint32_t *)buff) + 1), 0) ||
			adi_memap_read_reg32(STM32L1_UID_ADDR + 8,
									(((uint32_t *)buff) + 2), 1))
		{
			err = VSFERR_FAIL;
		}
		break;
	default:
		err = VSFERR_FAIL;
		break;
	}
	return err;
}

