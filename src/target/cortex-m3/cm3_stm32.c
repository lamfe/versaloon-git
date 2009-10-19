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

#include "adi_v5p1.h"
#include "cm3_common.h"

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
	uint32 i;
	uint32 reg;
	uint32 cur_block_size, block_size, block_size_tmp, cur_address;
	
	uint8 stm32x_flash_write_code[] = {
									/* init: */
		0x14, 0x4C,					/* ldr.n	r4, STM32_FLASH_CR */
		0x15, 0x4D,					/* ldr.n	r5, STM32_FLASH_SR */
		0x15, 0x4F,					/* ldr.n	r7, result */
									/* update: */
		0x16, 0x48,					/* ldr.n	r0, ram_ptr */
		0x16, 0x49,					/* ldr.n	r1, flash_ptr */
		0x4F, 0xF0, 0x00, 0x06,		/* mov.w	r6, #0 */
		0x16, 0x4A,					/* ldr.n	r2, number_of_words */
									/* write: */
		0x4F, 0xF0, 0x01, 0x03,		/* mov.w	r3, #1 */
		0x23, 0x60,					/* str		r3, [r4, #0] */
		0x30, 0xF8, 0x02, 0x3B,		/* ldrh.w	r3, [r0], #2 */
		0x21, 0xF8, 0x02, 0x3B,		/* strh.w	r3, [r1], #2 */
									/* busy: */
		0x2B, 0x68,					/* ldr 		r3, [r5, #0] */
		0x13, 0xF0, 0x01, 0x0F,		/* tst 		r3, #0x01 */
		0xFB, 0xD0,					/* beq 		busy */
		0x13, 0xF0, 0x14, 0x0F,		/* tst		r3, #0x14 */
		0x0F, 0xD1,					/* bne		exit */
		0x06, 0xF1, 0x01, 0x06,		/* add		r6, r6, #1 */
		0x96, 0x42,					/* cmp		r2, r6 */
		0xED, 0xD3,					/* bcc		write */
		0x13, 0x00,					/* movs		r3, r2 */
		0x3A, 0x60,					/* str		r2, [r7] */
									/* wait_data */
		0x0B, 0x4A,					/* ldr.n	r2, number_of_words */
		0x93, 0x42,					/* cmp		r3, r2 */
		0xFC, 0xD2,					/* bcs.n	wait_data */
		0x12, 0x04,					/* lsls		r2, r2, #16 */
		0xE5, 0xD5,					/* bpl.n	write */
		0x52, 0x00,					/* lsls		r2, r2, #1 */
		0x52, 0x0C,					/* lsrs		r2, r2, #17 */
		0x47, 0xF8, 0x04, 0x2C,		/* str.w	r2, [r7, #-4] */
		0xDC, 0xE7,					/* b		update */
									/* exit: */
		0x6F, 0xF0, 0x00, 0x02,		/* mvn.w	r2, #0 */
		0x3A, 0x60,					/* str		r2, [r7] */
		0xFE, 0xE7,					/* b $ */
		0x10, 0x20, 0x02, 0x40,		/* STM32_FLASH_CR:	.word 0x40022010 */
		0x0C, 0x20, 0x02, 0x40,		/* STM32_FLASH_SR:	.word 0x4002200C */
		0xE4, 0x03, 0x00, 0x20,		/* address of result */
		0x00, 0x04, 0x00, 0x20, 	/* ram address */
		0x00, 0x00, 0x00, 0x80,		/* flash address */
		0x00, 0x01, 0x00, 0x00,		/* number_of_words(2-byte) */
		0x00, 0x00, 0x00, 0x00,		/* result */
	};
	uint8 page_buf[4 * 1024 + sizeof(stm32x_flash_write_code)];
	
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
		// halt target first
		if (ERROR_OK != cm3_dp_halt())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "halt stm32");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
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
		// halt target first
		if (ERROR_OK != cm3_dp_halt())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "halt stm32");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		// prepare code data
		
		
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
		ml_tmp = 0;
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), "flash", 
				 pi->app_size_valid);
	}
	
	if (operations.read_operations & APPLICATION)
	{
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "flash");
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READING), "flash");
		}
		pgbar_init("reading flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
		ml_tmp = pi->app_memlist;
		while (ml_tmp != NULL)
		{
			block_size = ml_tmp->len;
			cur_address = ml_tmp->addr;
			while (block_size)
			{
				// cm3_get_max_block_size return size in dword(4-byte)
				cur_block_size = cm3_get_max_block_size(ml_tmp->addr);
				if (cur_block_size > (ml_tmp->len >> 2))
				{
					cur_block_size = ml_tmp->len;
				}
				else
				{
					cur_block_size <<= 2;
				}
				block_size_tmp = (cur_block_size + 3) & 0xFFFFFFFC;
				if (ERROR_OK != adi_memap_read_buf(cur_address, page_buf, 
												   cur_block_size))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							  "write flash block", ml_tmp->addr);
					ret = ERRCODE_FAILURE_OPERATION_ADDR;
					goto leave_program_mode;
				}
				for (i = 0; i < cur_block_size; i++)
				{
					if (page_buf[i] != pi->app[cur_address + i 
												- STM32_FLASH_START_ADDRESS])
					{
						pgbar_fini();
						LOG_ERROR(
							_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
							"flash", cur_address, page_buf[i], 
							pi->app[cur_address + i 
									- STM32_FLASH_START_ADDRESS]);
						ret = ERRCODE_FAILURE_VERIFY_TARGET;
						goto leave_program_mode;
					}
				}
				
				block_size -= cur_block_size;
				cur_address += cur_block_size;
				pgbar_update(cur_block_size);
			}
			
			ml_tmp = ml_tmp->next;
		}
		
		pgbar_fini();
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), "flash", 
				 pi->app_size_valid);
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ), "flash");
		}
	}
	
leave_program_mode:
	// lock flash
	reg = STM32_FLASH_CR_LOCK;
	adi_memap_write_reg(STM32_FLASH_CR, &reg, 0);
	return ret;
}

