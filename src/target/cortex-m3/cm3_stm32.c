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

#include "timer.h"

#define CUR_TARGET_STRING			CM3_STRING
#define cur_chip_param				cm3_chip_param
#define cur_buffer_size				cm3_buffer_size

RESULT stm32_wait_status_busy(uint32_t *status, uint32_t timeout)
{
	uint32_t reg;
	
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
	uint32_t reg;
	
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
	uint32_t i;
	uint32_t reg;
	uint32_t cur_block_size, block_size, block_size_tmp, cur_address;
	uint32_t cur_run_size;
	uint32_t mcu_id;
	char rev;
	uint32_t start_time, run_time;
	
#define FL_PARA_ADDR_BASE			\
					(STM32_SRAM_START_ADDRESS + sizeof(stm32_fl_code) - 4 * 4)
#define FL_ADDR_RAM_SRC				(FL_PARA_ADDR_BASE + 0)
#define FL_ADDR_FLASH_DEST			(FL_PARA_ADDR_BASE + 4)
#define FL_ADDR_WORD_LENGTH			(FL_PARA_ADDR_BASE + 8)
#define FL_ADDR_RESULT				(FL_PARA_ADDR_BASE + 12)
#define FL_ADDR_DATA				(STM32_SRAM_START_ADDRESS + 1024)
	uint8_t stm32_fl_code[] = {
									/* init: */
		0x16, 0x4C,					/* ldr.n	r4, STM32_FLASH_CR */
		0x17, 0x4D,					/* ldr.n	r5, STM32_FLASH_SR */
		0x17, 0x4F,					/* ldr.n	r7, result */
									/* wait_start: */
		0x57, 0xF8, 0x04, 0x3C,		/* ldr.w	r3, [r7, #-4] */
		0x00, 0x2B,					/* cmp		r3, #0 */
		0xFB, 0xD0,					/* beq 		wait_start */
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
		0xEC, 0x03, 0x00, 0x20,		/* address of result */
		0x00, 0x00, 0x00, 0x00, 	/* ram address */
		0x00, 0x00, 0x00, 0x00,		/* flash address */
		0x00, 0x00, 0x00, 0x00,		/* number_of_words(2-byte), set to 0 */
		0x00, 0x00, 0x00, 0x00		/* result, set to 0 */
	};
	uint8_t page_buf[STM32_PAGE_SIZE_RW];
	
	pi = pi;
	dp_info = dp_info;
	
	// check buffer size
	if (0 == cur_buffer_size)
	{
		// use default
		cur_buffer_size = 512;
	}
	if (cur_buffer_size & 0x03)
	{
		LOG_ERROR(_GETTEXT("STM32 buffer_size can only be aligned to 4.\n"));
		ret = ERRCODE_INVALID_OPTION;
		goto leave_program_mode;
	}
	if (cur_buffer_size > STM32_PAGE_SIZE_RW)
	{
		LOG_WARNING(_GETTEXT("Max buffer_size for STM32 is %d\n"), 
					STM32_PAGE_SIZE_RW);
		cur_buffer_size = STM32_PAGE_SIZE_RW;
	}
	
	// read MCU ID at STM32_REG_MCU_ID
	if (ERROR_OK != adi_memap_read_reg(STM32_REG_MCU_ID, &mcu_id, 1))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read stm32 MCU_ID");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	LOG_INFO(_GETTEXT("STM32 MCU_ID: 0x%08X\n"), mcu_id);
	switch (mcu_id & STM32_REV_MSK)
	{
	case STM32_REV_A:
		rev = 'A';
		break;
	case STM32_REV_B:
		rev = 'B';
		break;
	case STM32_REV_Z:
		rev = 'Z';
		break;
	case STM32_REV_Y:
		rev = 'Y';
		break;
	default :
		rev = '?';
		break;
	}
	LOG_INFO(_GETTEXT("STM32 revision: %c\n"), rev);
	switch (mcu_id & STM32_DEN_MSK)
	{
	case STM32_DEN_LOW:
		LOG_INFO(_GETTEXT("STM32 type: low-density device\n"));
		break;
	case STM32_DEN_MEDIUM:
		LOG_INFO(_GETTEXT("STM32 type: medium-density device\n"));
		break;
	case STM32_DEN_HIGH:
		LOG_INFO(_GETTEXT("STM32 type: high-density device\n"));
		break;
	case STM32_DEN_CONNECTIVITY:
		LOG_INFO(_GETTEXT("STM32 type: connectivity device\n"));
		break;
	default:
		LOG_INFO(_GETTEXT("STM32 type: unknown device\n"));
		break;
	}
	
	// read flash and ram size
	if (ERROR_OK != adi_memap_read_reg(STM32_REG_FLASH_RAM_SIZE, &mcu_id, 1))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "read stm32 flash_ram size");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	if ((mcu_id & 0xFFFF) <= 1024)
	{
		pi->app_size = (mcu_id & 0xFFFF) * 1024;
		LOG_INFO(_GETTEXT("STM32 flash_size: %dK Bytes\n"), mcu_id & 0xFFFF);
	}
	else
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), mcu_id & 0xFFFF, 
					"stm32 flash size");
		return ERRCODE_INVALID;
	}
	if ((mcu_id >> 16) != 0xFFFF)
	{
		LOG_INFO(_GETTEXT("STM32 sram_size: %dK Bytes\n"), mcu_id >> 16);
	}
	
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
		
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash_loader");
		pgbar_init("writing flash_loader |", "|", 0, 1, PROGRESS_STEP, '=');
		
		block_size = sizeof(stm32_fl_code);
		// last_but_three dword is RAM address for data, set to 1K at SRAM
		*(uint32_t *)(stm32_fl_code + block_size - 4 * 4) = FL_ADDR_DATA;
		// last_but_four dword is SRAM address of last dword
		*(uint32_t *)(stm32_fl_code + block_size - 4 * 5) = FL_ADDR_RESULT;
		
		// write code to target SRAM
		if (ERROR_OK != adi_memap_write_buf(STM32_SRAM_START_ADDRESS, 
											stm32_fl_code, block_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "load flash_loader to SRAM");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), "flash_loader", block_size);
		
		LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "flash_loader");
		pgbar_init("reading flash_loader |", "|", 0, 1, PROGRESS_STEP, '=');
		
		// read back for verify
		memset(page_buf, 0, block_size);
		if (ERROR_OK != adi_memap_read_buf(STM32_SRAM_START_ADDRESS, 
										   page_buf, block_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "read flash_loader for verify");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		for (i = 0; i < block_size; i++)
		{
			if (stm32_fl_code[i] != page_buf[i])
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "verify flash_loader");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), "flash_loader", block_size);
		
		reg = STM32_SRAM_START_ADDRESS;
		if (ERROR_OK != cm3_write_core_register(CM3_COREREG_PC, &reg))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write PC");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		reg = 0;
		if ((ERROR_OK != cm3_read_core_register(CM3_COREREG_PC, &reg)) 
			|| (reg != STM32_SRAM_START_ADDRESS))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "verify written PC");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		LOG_DEBUG(_GETTEXT("PC is set to run flash_loader.\n"));
		
		// run target
		if (ERROR_OK != cm3_dp_run())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "run flash_loader");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		LOG_DEBUG(_GETTEXT("flash_loader is running.\n"));
		
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, pi->app_size_valid, 
				   PROGRESS_STEP, '=');
		
		ml_tmp = pi->app_memlist;
		while (ml_tmp != NULL)
		{
			block_size = ml_tmp->len;
			cur_address = ml_tmp->addr;
			block_size_tmp = 0;
			cur_run_size = 0;
			i = 0;
			while (block_size)
			{
				if (block_size > cur_buffer_size)
				{
					cur_block_size = cur_buffer_size;
				}
				else
				{
					cur_block_size = block_size;
				}
				
				// write flash content to FL_PARA_ADDR_DATA
				if (ERROR_OK != adi_memap_write_buf(FL_ADDR_DATA + cur_run_size, 
						&pi->app[cur_address - STM32_FLASH_START_ADDRESS], 
						cur_block_size))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							  "download flash data");
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				// write flash address
				if (0 == cur_run_size)
				{
					// first run, update flash address
					adi_memap_write_reg(FL_ADDR_FLASH_DEST, &cur_address, 0);
				}
				// write word length
				cur_run_size += cur_block_size;
				reg = cur_run_size / 2;
				if (i)
				{
					i = 0;
					// not the first run
					// or the length by 0x8000 to indicate reload addresses
					reg |= 0x8000;
				}
				if (ERROR_OK != adi_memap_write_reg(FL_ADDR_WORD_LENGTH, 
													&reg, 1))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							  "download parameters to flash_loader");
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				// wait ready
				start_time = get_time_in_ms();
				if ((cur_run_size >=  STM32_PAGE_SIZE_RW) 
					|| (block_size <= cur_buffer_size))
				{
					reg = 0;
					do{
						if (ERROR_OK != adi_memap_read_reg(FL_ADDR_RESULT, 
															&reg, 1))
						{
							pgbar_fini();
							LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
									  "read result from flash_loader");
							ret = ERRCODE_FAILURE_OPERATION;
							goto leave_program_mode;
						}
						
						run_time = get_time_in_ms();
						if ((run_time - start_time) > 1000)
						{
							pgbar_fini();
							LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
									  "wait OK from flash_loader");
							ret = ERRCODE_FAILURE_OPERATION;
							goto leave_program_mode;
						}
					} while(reg != cur_run_size / 2);
					cur_run_size = 0;
					i = 1;		// need to update settings
				}
				
				block_size -= cur_block_size;
				cur_address += cur_block_size;
				pgbar_update(cur_block_size);
			}
			
			ml_tmp = MEMLIST_GetNext(ml_tmp);
		}
		
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
				cur_block_size = cm3_get_max_block_size(cur_address);
				if (cur_block_size > (block_size >> 2))
				{
					cur_block_size = block_size;
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
							"flash", cur_address + i, page_buf[i], 
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
			
			ml_tmp = MEMLIST_GetNext(ml_tmp);
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
	adi_memap_write_reg(STM32_FLASH_CR, &reg, 1);
	return ret;
}

