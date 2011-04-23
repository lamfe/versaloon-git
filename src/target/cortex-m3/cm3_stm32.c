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

#include "cm3_internal.h"
#include "stm32_internal.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

#include "timer.h"

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

struct stm32_iap_cmd_t
{
	uint32_t cr_addr;
	uint32_t cr_value1;
	uint32_t cr_value2;
	
	uint32_t sr_addr;
	uint32_t sr_busy_mask;
	uint32_t sr_err_mask;
	
	uint32_t target_addr;
	uint32_t ram_addr;
	
	uint32_t data_type;
	uint32_t data_size;
};

struct stm32_iap_result_t
{
	uint32_t result;
};

static uint32_t stm32swj_iap_cnt = 0;

#define STM32_IAP_BASE				STM32_SRAM_ADDR
#define STM32_IAP_COMMAND_ADDR		(STM32_IAP_BASE + 0x80)
#define STM32_IAP_SYNC_ADDR			(STM32_IAP_BASE + 0xA4)
// buffer_size MUST be 2048 bytes to hold 2 flash pages
#define STM32_IAP_BUFFER_SIZE		(2048)
#define STM32_IAP_BUFFER_ADDR		(STM32_IAP_BASE + 1024)
static uint8_t iap_code[] = {
								/* wait_start: */
	0x28, 0x48,					/* ldr.n	r0, [PC, #0xA0]	;load SYNC */
	0x00, 0x28,					/* cmp		r0, #0 */
	0xFC, 0xD0,					/* beq.n	wait_start */
								/* update command: */
	0x1E, 0x48,					/* ldr.n	r0, [PC, #0x78]	;load CR_ADDR */
	0x1E, 0x49,					/* ldr.n	r1, [PC, #0x78] ;load CR_VALUE1 */
	0x1F, 0x4A,					/* ldr.n	r2, [PC, #0x7C] ;load CR_VALUE2 */
								/* write_cr: */
	0x01, 0x60,					/* str		r1, [r0]		;write CR */
	0x12, 0x42,					/* tst		r2, r2 */
	0x00, 0xD0,					/* beq.n	load_parameter */
	0x02, 0x60,					/* str		r2, [r0] */
								/* load_parameter: */
	0x1D, 0x48,					/* ldr.n	r0, [PC, #0x74] ;load SR_ADDR */
	0x1E, 0x49,					/* ldr.n	r1, [PC, #0x78] ;load SR_BUSY_MASK */
	0x1E, 0x4A,					/* ldr.n	r2, [PC, #0x78] ;load TARGET_ADDR */
	0x1F, 0x4B,					/* ldr.n	r3, [PC, #0x7C] ;load RAM_ADDR */
	0x1F, 0x4C,					/* ldr.n	r4, [PC, #0x7C] ;load DATA_TYPE */
	0x20, 0x4D,					/* ldr.n	r5, [PC, #0x80] ;load DATA_SIZE */
								/* clear_sync: */
	0x00, 0x26,					/* movs		r6, #0 */
	0x20, 0xA7,					/* adr.n	r7, PC, #0x84 */
	0x3E, 0x60,					/* str		r6, [r7] */
								/* check_data_size */
	0x2D, 0x42,					/* tst		r5, r5 */
	0xEA, 0xD0,					/* beq.n	wait_start */
								/* check_data_type */
	0xA6, 0x1B,					/* subs		r6, r4, r6 */
	0x00, 0xD1,					/* bne.n	write_data */
	0x0F, 0xE0,					/* b		wait_busy */
								/* write_data: */
	0x01, 0x26,					/* movs		r1, #1 */
	0xA6, 0x1B,					/* subs		r6, r4, r6 */
	0x02, 0xD1,					/* be.n		write_word_dword */
	0x1F, 0x78,					/* ldrb		r7, [r3] */
	0x17, 0x70,					/* strb		r7, [r2] */
	0x07, 0xE0,					/* b.n		adjust_addr */
								/* write_word_dword: */
	0x02, 0x26,					/* movs		r6, #2 */
	0xA6, 0x1B,					/* subs		r6, r4, r6 */
	0x02, 0xD1,					/* be.n		write_dword */
	0x1F, 0x88,					/* ldrh		r7, [r3] */
	0x17, 0x80,					/* str		r7, [r2] */
	0x01, 0xE0,					/* b.n		adjust_addr */
								/* wrire_dword: */
	0x1F, 0x68,					/* ldr		r7, [r3] */
	0x17, 0x60,					/* str		r7, [r2] */
								/* adjust_addr: */
	0x22, 0x44,					/* add		r2, r2, r4 */
	0x23, 0x44,					/* add		r3, r3, r4 */
								/* wait_busy: */
	0x07, 0x68,					/* ldr		r7, [r0] */
	0x0F, 0x40,					/* ands		r7, r7, r1 */
	0xFC, 0xD1,					/* bne.n	wait_busy */
								/* check_all_written: */
	0x6D, 0x1E,					/* subs		r5, r5, #1 */
	0xEA, 0xD1,					/* bne.n	write_data */
								/* increase_result */
	0x13, 0x48,					/* ldr.n	r0, [PC, #0x4C] */
	0x40, 0x1C,					/* adds		r0, r0, #1 */
	0x12, 0xA1,					/* adr.n	r1, PC, #0x48 */
	0x08, 0x60,					/* str		r0, [r1] */
	0xCD, 0xE7,					/* b.n		wait_start */
								/* exit: */
								/* write_result_false: */
	0x00, 0x20,					/* movs		r0, #0 */
	0x20, 0xA1,					/* adr.n	r1, PC, #0x80 */
	0x08, 0x60,					/* str		r0, [r1] */
								/* dead_loop: */
	0xFE, 0xE7,					/* b $ */
	
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	
	0x00, 0x00, 0x00, 0x00,		/* cr_addr */
	0x00, 0x00, 0x00, 0x00,		/* cr_value1 */
	0x00, 0x00, 0x00, 0x00,		/* cr_value2 */
	0x00, 0x00, 0x00, 0x00,		/* sr_addr */
	0x00, 0x00, 0x00, 0x00,		/* sr_busy_mask */
	0x00, 0x00, 0x00, 0x00,		/* target_addr */
	0x00, 0x00, 0x00, 0x00,		/* ram_addr */
	0x00, 0x00, 0x00, 0x00,		/* data_type */
	0x00, 0x00, 0x00, 0x00,		/* data_size */
	
	0x00, 0x00, 0x00, 0x00,		/* sync */
	
	0x00, 0x00, 0x00, 0x00,		/* result */
};

static RESULT stm32swj_iap_init(void)
{
	uint32_t reg;
	uint8_t verify_buff[sizeof(iap_code)];
	
	// download flash_loader
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt stm32");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write code to target SRAM
	if (ERROR_OK != adi_memap_write_buf(STM32_IAP_BASE, iap_code, 
											sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load flash_loader to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	// verify iap_code
	memset(verify_buff, 0, sizeof(iap_code));
	if (ERROR_OK != adi_memap_read_buf(STM32_IAP_BASE, verify_buff, 
											sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read flash_loader");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (memcmp(verify_buff, iap_code, sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "verify flash_loader");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	reg = STM32_IAP_BASE + 1;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_PC, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write PC");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (ERROR_OK != cm3_dp_resume())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run flash_loader");
		return ERRCODE_FAILURE_OPERATION;
	}
	stm32swj_iap_cnt = 0;
	return ERROR_OK;
}

static RESULT stm32swj_iap_run(struct stm32_iap_cmd_t *cmd)
{
	uint32_t buff_tmp[10];
	
	buff_tmp[0] = SYS_TO_LE_U32(cmd->cr_addr);
	buff_tmp[1] = SYS_TO_LE_U32(cmd->cr_value1);
	buff_tmp[2] = SYS_TO_LE_U32(cmd->cr_value2);
	buff_tmp[3] = SYS_TO_LE_U32(cmd->sr_addr);
	buff_tmp[4] = SYS_TO_LE_U32(cmd->sr_busy_mask);
	buff_tmp[5] = SYS_TO_LE_U32(cmd->target_addr);
	buff_tmp[6] = SYS_TO_LE_U32(cmd->ram_addr);
	buff_tmp[7] = SYS_TO_LE_U32(cmd->data_type);
	buff_tmp[8] = SYS_TO_LE_U32(cmd->data_size);
	buff_tmp[9] = SYS_TO_LE_U32(1);
	
	// write iap command with sync to target SRAM
	// sync is 4-byte AFTER command in sram
	if (ERROR_OK != adi_memap_write_buf(STM32_IAP_COMMAND_ADDR, 
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	stm32swj_iap_cnt++;
	
	return ERROR_OK;
}

static RESULT stm32swj_iap_poll_result(struct stm32_iap_result_t *result, bool *failed)
{
	uint32_t buff_tmp[2];
	uint8_t i;
	
	*failed = false;
	
	// read result and sync
	// sync is 4-byte BEFORE result
	if (ERROR_OK != adi_memap_read_buf(STM32_IAP_SYNC_ADDR, 
										(uint8_t *)buff_tmp, sizeof(buff_tmp)))
	{
		*failed = true;
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	for (i = 0; i < dimof(buff_tmp); i++)
	{
		buff_tmp[i] = LE_TO_SYS_U32(buff_tmp[i]);
	}
	
	if (0 == buff_tmp[0])
	{
		result->result = buff_tmp[1];
		return ERROR_OK;
	}
	
	return ERROR_FAIL;
}

static RESULT stm32swj_iap_wait_ready(struct stm32_iap_result_t *result, 
										bool last)
{
	bool failed;
	uint32_t start, end;
	
	start = get_time_in_ms();
	while (1)
	{
		if ((ERROR_OK != stm32swj_iap_poll_result(result, &failed)) || 
			(last && (result->result != stm32swj_iap_cnt)))
		{
			if (failed)
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll iap result");
				return ERROR_FAIL;
			}
			else
			{
				end = get_time_in_ms();
				// wait 1s at most
				if ((end - start) > 1000)
				{
					cm3_dump(STM32_IAP_BASE, sizeof(iap_code));
					LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap ready");
					return ERRCODE_FAILURE_OPERATION;
				}
			}
		}
		else
		{
			break;
		}
	}
	
	return ERROR_OK;
}

static RESULT stm32swj_iap_call(struct stm32_iap_cmd_t *cmd, 
								struct stm32_iap_result_t *result, bool last)
{
	if ((ERROR_OK != stm32swj_iap_run(cmd)) 
		|| (ERROR_OK != stm32swj_iap_wait_ready(result, last)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap command");
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

ENTER_PROGRAM_MODE_HANDLER(stm32swj)
{
	uint32_t reg, flash_obr, flash_wrpr;
	
	REFERENCE_PARAMETER(context);
	
	// unlock flash and option bytes
	reg = STM32_FLASH_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32_FLASH_KEYR, &reg, 0);
	reg = STM32_FLASH_UNLOCK_KEY2;
	adi_memap_write_reg32(STM32_FLASH_KEYR, &reg, 0);
	reg = STM32_FLASH_UNLOCK_KEY1;
	adi_memap_write_reg32(STM32_FLASH_OPTKEYR, &reg, 0);
	reg = STM32_FLASH_UNLOCK_KEY2;
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
	
	return stm32swj_iap_init();
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
	struct stm32_iap_cmd_t cmd;
	struct stm32_iap_result_t result;
	
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
		if (ERROR_OK != stm32swj_iap_call(&cmd, &result, true))
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
		if (ERROR_OK != stm32swj_iap_call(&cmd, &result, true))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		if (param->chip_areas[APPLICATION_IDX].size > STM32_FLASH_BANK_SIZE)
		{
			cmd.cr_addr = STM32_FLASH_CR2;
			cmd.sr_addr = STM32_FLASH_SR2;
			if (ERROR_OK != stm32swj_iap_call(&cmd, &result, true))
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
	RESULT ret = ERROR_OK;
	struct stm32_iap_cmd_t cmd;
	struct stm32_iap_result_t result;
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
		cmd.ram_addr = STM32_IAP_BUFFER_ADDR;
		cmd.data_type = 2;
		cmd.data_size = STM32_OB_SIZE / 2;
		if ((ERROR_OK != adi_memap_write_buf(cmd.ram_addr, fuse_buff, 
												STM32_OB_SIZE)) || 
			(ERROR_OK != stm32swj_iap_call(&cmd, &result, true)))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	case APPLICATION_CHAR:
#define STM32_IAP_PAGE_SIZE			(STM32_IAP_BUFFER_SIZE / 2)
#define	STM32_IAP_PAGE0_ADDR		STM32_IAP_BUFFER_ADDR
#define STM32_IAP_PAGE1_ADDR		(STM32_IAP_BUFFER_ADDR + STM32_IAP_PAGE_SIZE)
		if (size % STM32_IAP_PAGE_SIZE)
		{
			return ERROR_FAIL;
		}
		
		last = false;
		if (size == STM32_IAP_PAGE_SIZE)
		{
			last = true;
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
		cmd.ram_addr = STM32_IAP_PAGE0_ADDR;
		cmd.data_type = 2;
		cmd.data_size = STM32_IAP_PAGE_SIZE / 2;
		if ((ERROR_OK != adi_memap_write_buf(cmd.ram_addr, buff, STM32_IAP_PAGE_SIZE)) || 
			(ERROR_OK != stm32swj_iap_call(&cmd, &result, last)))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		size -= STM32_IAP_PAGE_SIZE;
		buff += STM32_IAP_PAGE_SIZE;
		addr +=STM32_IAP_PAGE_SIZE;
		pgbar_update(STM32_IAP_PAGE_SIZE);
		tick_tock = 1;
		
		while (size)
		{
			if (size == STM32_IAP_PAGE_SIZE)
			{
				last = true;
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
				cmd.ram_addr = STM32_IAP_PAGE1_ADDR;
			}
			else
			{
				cmd.ram_addr = STM32_IAP_PAGE0_ADDR;
			}
			if ((ERROR_OK != adi_memap_write_buf(cmd.ram_addr, buff, STM32_IAP_PAGE_SIZE)) || 
				(ERROR_OK != stm32swj_iap_call(&cmd, &result, last)))
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			
			size -= STM32_IAP_PAGE_SIZE;
			buff += STM32_IAP_PAGE_SIZE;
			addr +=STM32_IAP_PAGE_SIZE;
			pgbar_update(STM32_IAP_PAGE_SIZE);
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

