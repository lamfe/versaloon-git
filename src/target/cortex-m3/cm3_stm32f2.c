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

#include "timer.h"

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

static RESULT stm32f2_wait_status_busy(uint32_t *status, uint32_t timeout)
{
	uint32_t reg;
	
	if (ERROR_OK != adi_memap_read_reg32(STM32F2_FLASH_SR, &reg, 1))
	{
		return ERROR_FAIL;
	}
	reg = LE_TO_SYS_U32(reg);
	while ((reg & STM32F2_FLASH_SR_BSY) && timeout)
	{
		timeout--;
		sleep_ms(1);
		if (ERROR_OK != adi_memap_read_reg32(STM32F2_FLASH_SR, &reg, 1))
		{
			return ERROR_FAIL;
		}
		reg = LE_TO_SYS_U32(reg);
	}
	*status = reg;
	if (reg & STM32F2_FLASH_SR_ERRMSK)
	{
		reg = STM32F2_FLASH_SR_ERRMSK;
		adi_memap_write_reg32(STM32F2_FLASH_SR, &reg, 1);
	}
	
	return ERROR_OK;
}

static RESULT stm32f2_mass_erase(void)
{
	uint32_t reg;
	
	reg = STM32F2_FLASH_CR_MER | STM32F2_FLASH_CR_STRT;
	adi_memap_write_reg32(STM32F2_FLASH_CR, &reg, 0);
	
	// wait busy
	if ((ERROR_OK != stm32f2_wait_status_busy(&reg, 20000)) || 
		(reg & (STM32F2_FLASH_SR_BSY | STM32F2_FLASH_SR_ERRMSK)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

ENTER_PROGRAM_MODE_HANDLER(stm32f2swj)
{
	uint32_t reg;
	
	REFERENCE_PARAMETER(context);
	
	// unlock flash and option bytes
	reg = STM32F2_FLASH_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32F2_FLASH_KEYR, &reg, 0);
	reg = STM32F2_FLASH_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32F2_FLASH_KEYR, &reg, 1);
	
	return ERROR_OK;
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
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
/*
		// halt target first
		if (ERROR_OK != cm3_dp_halt())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt stm32f2");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
*/		// erase all flash
		if (ERROR_OK != stm32f2_mass_erase())
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
	RESULT ret = ERROR_OK;
	uint32_t i;
	uint32_t reg;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case FUSE_CHAR:
		break;
	case APPLICATION_CHAR:
		if (ERROR_OK != cm3_dp_halt())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt stm32");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		for (i = 0; i < size / 4; i++)
		{
			reg = STM32F2_FLASH_CR_PG | STM32F2_FLASH_CR_PSIZE_32;
			adi_memap_write_reg32(STM32F2_FLASH_CR, &reg, 0);
			reg = ((uint32_t *)buff)[i];
			adi_memap_write_reg32(addr, (uint32_t *)&reg, 0);
			addr += 4;
			
			if ((ERROR_OK != stm32f2_wait_status_busy(&reg, 50)) || 
				(reg & STM32F2_FLASH_SR_ERRMSK))
			{
				ret = ERROR_FAIL;
				break;
			}
			pgbar_update(4);
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
	uint8_t option_bytes[STM32F2_OB_SIZE], i;
	uint32_t mcu_id = 0;
	uint32_t cur_block_size;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read MCU ID at STM32_REG_MCU_ID
		if (ERROR_OK != adi_memap_read_reg32(STM32F2_REG_MCU_ID, &mcu_id, 1))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		mcu_id = LE_TO_SYS_U32(mcu_id);
		stm32f2_print_device(mcu_id);
		mcu_id &= STM32F2_DEN_MSK;
		*(uint32_t *)buff = mcu_id;
		break;
	case FUSE_CHAR:
		if (ERROR_OK != 
				adi_memap_read_buf(STM32F2_OB_ADDR, option_bytes, STM32F2_OB_SIZE))
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

