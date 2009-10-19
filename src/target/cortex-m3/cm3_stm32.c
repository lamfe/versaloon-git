/***************************************************************************
 *   Copyright (C) 2009 by Simon Qian <SimonQian@SimonQian.com>            *
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
#include "cm3_stm32.h"

#include "cm3_internal.h"

#define CUR_TARGET_STRING			CM3_STRING
#define cur_chip_param				cm3_chip_param

RESULT stm32_wait_status_busy(uint32 *status, uint32 timeout)
{
	uint32 reg;
	
	if (ERROR_OK != adi_memap_read_reg(STM32_FLASH_SR, &reg, 1))
	{
		return ERROR_FAIL;
	}
	while ((reg & STM32_FLASH_SR_BSY) && timeout)
	{
		timeout--;
		sleep_ms(1);
		if (ERROR_OK != adi_memap_read_reg(STM32_FLASH_SR, &reg, 1))
		{
			return ERROR_FAIL;
		}
	}
	*status = reg;
	if (reg & (STM32_FLASH_SR_PGERR | STM32_FLASH_SR_WRPRTERR))
	{
		reg = STM32_FLASH_SR_PGERR | STM32_FLASH_SR_WRPRTERR;
		adi_memap_write_reg(STM32_FLASH_SR, &reg, 1);
	}
	
	return ERROR_OK;
}

RESULT stm32_mass_erase(void)
{
	uint32 reg;
	
	// mass erase flash memory
	reg = STM32_FLASH_CR_MER;
	adi_memap_write_reg(STM32_FLASH_CR, &reg, 0);
	reg = STM32_FLASH_CR_MER | STM32_FLASH_CR_STRT;
	adi_memap_write_reg(STM32_FLASH_CR, &reg, 0);
	
	// wait busy
	if (ERROR_OK != stm32_wait_status_busy(&reg, 10))
	{
		return ERROR_FAIL;
	}
	if (reg & STM32_FLASH_SR_WRPRTERR)
	{
		return ERROR_FAIL;
	}
	else
	{
		return ERROR_OK;
	}
}

RESULT stm32_program(operation_t operations, program_info_t *pi, 
					 adi_dp_info_t *dp_info)
{
	RESULT ret = ERROR_OK;
	memlist *ml_tmp;
	uint32 reg;
	
	pi = pi;
	dp_info = dp_info;
	
	// unlock flash registers
	reg = STM32_FLASH_UNLOCK_KEY1;
	adi_memap_write_reg(STM32_FLASH_KEYR, &reg, 0);
	reg = STM32_FLASH_UNLOCK_KEY2;
	adi_memap_write_reg(STM32_FLASH_KEYR, &reg, 0);
	
	// erase
	if (operations.erase_operations > 0)
	{
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "chip");
		pgbar_init("erasing chip |", "|", 0, 1, PROGRESS_STEP, '=');
		
/*		if (pi->app_size_valid > 0)
		{
			// erase flash according to data
		}
		else
*/
		{
			// erase all flash
			if (ERROR_OK != stm32_mass_erase())
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "chip");
	}
	
	if (operations.write_operations & APPLICATION)
	{
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
		ml_tmp = 0;
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), "flash", 
				 pi->app_size_valid);
	}
	
	if (operations.write_operations & LOCK)
	{
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "lock");
		pgbar_init("writing lock |", "|", 0, 1, PROGRESS_STEP, '=');
		
		
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO("lock programmed\n");
	}
	
leave_program_mode:
	// lock flash
	reg = STM32_FLASH_CR_LOCK;
	adi_memap_write_reg(STM32_FLASH_CR, &reg, 0);
	return ret;
}

