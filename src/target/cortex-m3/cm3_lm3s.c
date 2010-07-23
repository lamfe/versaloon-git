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

#define LM3S_IAP_BASE			LM3S_SRAM_ADDR
#define LM3S_IAP_COMMAND_OFFSET	100
#define LM3S_IAP_COMMAND_ADDR	(LM3S_IAP_BASE + LM3S_IAP_COMMAND_OFFSET)
#define LM3S_IAP_SYNC_ADDR		(LM3S_IAP_BASE + 120)
static uint8_t iap_code[] = 
{
							// wait_start:
	0x1D, 0x48,				// ldr		r0, [PC, #XX]		// load sync
	0x00, 0x28,				// cmp		r0, #0
	0xFC, 0xD0,				// beq 		wait_start
	// 6 bytes above
							// init:
	0x18, 0x4A,				// ldr		r2, [PC, #XX]		// laod tgt_addr
	0x18, 0x4B,				// ldr		r3, [PC, #XX]		// load src_addr
	0x19, 0x4C,				// ldr		r4, [PC, #XX]		// load command
	0x19, 0x4D,				// ldr		r5, [PC, #XX]		// load cnt
	// 14 bytes above
							// clear_sync:
	0x1A, 0xA0,				// add		r0, PC, #XX			// load address of sync
	0x00, 0x21,				// mov		r1, #0
	0x01, 0x60,				// str		r1, [r0]
	// 20 bytes above
							// do_operation:
	0x13, 0x48,				// ldr		r0, [PC, #XX]		// load address of FMA
	0x02, 0x60,				// str		r2, [r0]			// FMA = tgt_addr
	0x00, 0x1D,				// adds		r0, r0, #4			// &FMD = &FMA + 4
	0x19, 0x68,				// ldr		r1, [r3]			// load src_data
	0x01, 0x60,				// str		r1, [r0]			// FMD = *str_data
	0x00, 0x1D,				// adds		r0, r0, #4			// &FMC = &FMD + 4
	0x04, 0x60,				// str		r4, [r0]			// FMC = command
	// 34 bytes above
							// wait_operation_finish:
	0x01, 0x68,				// ldr		r1, [r0]			// r1 = FMC
	0x21, 0x40,				// ands		r1, r1, r4			// r1 = r1 & command
	0xFC, 0xD1,				// bne		wait_operation_finish
	// 40 bytes above
							// adjust_param:
	0x12, 0x1D,				// adds		r2, r2, #4			// tgt_addr + 4
	0x1B, 0x1D,				// adds		r3, r3, #4			// src_addr + 4
	0x6D, 0x1E,				// subs		r5, r5, #1			// cnt - 1
	// 46 bytes above
	0xF0, 0xD1,				// bne		do_operation
							// clear_busy:
	0x12, 0xA0,				// add		r0, PC, #XX			// load address of sync
	0x00, 0x21,				// mov		r1, #0
	0x01, 0x60,				// str		r1, [r0]
							// wait_next_run:
	0xE3, 0xE7,				// b		wait_start
	0xFE, 0xE7,				// b		$
	// 52 bytes above
	// fill
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00,
	// 100 bytes above
	// parameters
	0x00, 0x00, 0x00, 0x00,	// FMA_addr
	0x00, 0x00, 0x00, 0x00,	// tgt_addr
	0x00, 0x00, 0x00, 0x00,	// src_addr
	0x00, 0x00, 0x00, 0x00,	// command
	0x00, 0x00, 0x00, 0x00,	// cnt
	0x00, 0x00, 0x00, 0x00,	// sync
	0x00, 0x00, 0x00, 0x00	// idle(0)/busy(1)/error(>1)
};

struct lm3sswj_iap_cmd_t
{
	uint32_t FMA_addr;
	uint32_t tgt_addr;
	uint32_t src_addr;
	uint32_t command;
	uint32_t cnt;
};

static RESULT lm3sswj_debug_info(void)
{
	uint32_t reg;
	uint8_t i;
	uint8_t *buffer;
	RESULT ret = ERROR_OK;
	
	buffer = (uint8_t *)malloc(sizeof(iap_code));
	if (NULL == buffer)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		ret = ERRCODE_NOT_ENOUGH_MEMORY;
		goto end;
	}
	
	LOG_INFO("report to author on this message.");
	
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt lpc1000");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	
	for (i = 0; i < 13; i++)
	{
		reg = 0;
		if (ERROR_OK != cm3_read_core_register(i, &reg))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read register");
			ret = ERRCODE_FAILURE_OPERATION;
			goto end;
		}
		LOG_INFO("r%d: %08X", i, reg);
	}
	reg = 0;
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_SP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read sp");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_INFO(INFOMSG_REG_08X, "sp", reg);
	reg = 0;
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_LR, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read lr");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_INFO(INFOMSG_REG_08X, "lr", reg);
	reg = 0;
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_PC, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read pc");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_INFO(INFOMSG_REG_08X, "pc", reg);
	
	LOG_INFO("SRAM dump at 0x%08X:", LM3S_IAP_BASE);
	if (ERROR_OK != adi_memap_read_buf(LM3S_IAP_BASE, buffer, 
													sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read sram");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_BYTE_BUF(buffer, sizeof(iap_code), LOG_INFO, "%02X", 16);
	
end:
	if (buffer != NULL)
	{
		free(buffer);
		buffer = NULL;
	}
	
	return ret;
}

static RESULT lm3sswj_iap_run(struct lm3sswj_iap_cmd_t * cmd)
{
	uint32_t buff_tmp[7];
	
	memset(buff_tmp, 0, sizeof(buff_tmp));
	buff_tmp[0] = cmd->FMA_addr;
	buff_tmp[1] = cmd->tgt_addr;
	buff_tmp[2] = cmd->src_addr;
	buff_tmp[3] = cmd->command;
	buff_tmp[4] = cmd->cnt;
	buff_tmp[5] = 1;				// sync
	buff_tmp[6] = 1;				// busy
	
	// write iap command with sync to target SRAM
	// sync is 4-byte AFTER command in sram
	if (ERROR_OK != adi_memap_write_buf(LM3S_IAP_COMMAND_ADDR, 
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

static RESULT lm3sswj_iap_poll_result(uint32_t *result, uint8_t *fail)
{
	uint32_t buff_tmp[2];
	
	*fail = 0;
	
	// read result and sync
	// sync is 4-byte BEFORE result
	if (ERROR_OK != adi_memap_read_buf(LM3S_IAP_SYNC_ADDR, 
								(uint8_t *)buff_tmp, sizeof(buff_tmp)))
	{
		*fail = 1;
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (0 == buff_tmp[0])
	{
		if (buff_tmp[1] > 1)
		{
			// error
			*fail = 1;
			lm3sswj_debug_info();
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRCODE, "call iap", 
						buff_tmp[1]);
			return ERRCODE_FAILURE_OPERATION;
		}
		else
		{
			*result = buff_tmp[1];
			
			if (1 == buff_tmp[1])
			{
				// busy
				return ERROR_FAIL;
			}
			else
			{
				// idle
				return ERROR_OK;
			}
		}
	}
	
	return ERROR_FAIL;
}

static RESULT lm3sswj_iap_wait_ready(uint32_t *result)
{
	uint8_t fail = 0;
	uint32_t start, end;
	
	start = get_time_in_ms();
	while (1)
	{
		if (ERROR_OK != lm3sswj_iap_poll_result(result, &fail))
		{
			if (fail)
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
					lm3sswj_debug_info();
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

static RESULT lm3sswj_iap_call(struct lm3sswj_iap_cmd_t *cmd, uint32_t *result)
{	
	if ((ERROR_OK != lm3sswj_iap_run(cmd)) 
		|| (ERROR_OK != lm3sswj_iap_wait_ready(result)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap command");
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

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
	
	// write iap_code to target SRAM
	if (ERROR_OK != adi_memap_write_buf(LM3S_IAP_BASE, (uint8_t*)iap_code, 
											sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap_code to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write pc
	reg = LM3S_IAP_BASE + 1;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_PC, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write PC");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (ERROR_OK != cm3_dp_run())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
		return ERRCODE_FAILURE_OPERATION;
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
	RESULT ret= ERROR_OK;
	uint32_t result;
	struct lm3sswj_iap_cmd_t cmd;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		cmd.FMA_addr = LM3S_FLASHCTL_FMA;
		cmd.tgt_addr = 0;
		cmd.src_addr = 0;
		cmd.command = LM3S_FLASHCTL_FMC_MERASE | LM3S_FLASHCTL_FMC_KEY;
		cmd.cnt = 1;
		result = 0;
		ret = lm3sswj_iap_call(&cmd, &result);
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

WRITE_TARGET_HANDLER(lm3sswj)
{
	RESULT ret = ERROR_OK;
	uint32_t result;
	struct lm3sswj_iap_cmd_t cmd;
	uint32_t page_size = 512;
	uint8_t ticktock = 0;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		// check alignment
		if ((size % 4) || (addr %4))
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "flash addr and/or size");
			return ERROR_FAIL;
		}
		
		// write first buff to target SRAM
		if (ERROR_OK != adi_memap_write_buf(LM3S_SRAM_ADDR + 1024, buff, page_size))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
			return ERRCODE_FAILURE_OPERATION;
		}
		cmd.FMA_addr = LM3S_FLASHCTL_FMA;
		cmd.tgt_addr = addr;
		cmd.src_addr = LM3S_SRAM_ADDR + 1024;
		cmd.command = LM3S_FLASHCTL_FMC_WRITE | LM3S_FLASHCTL_FMC_KEY;
		cmd.cnt = page_size / 4;
		result = 0;
		if (ERROR_OK != lm3sswj_iap_run(&cmd))
		{
			ret = ERROR_FAIL;
			break;
		}
		size -= page_size;
		
		while (size)
		{
			buff += page_size;
			addr += page_size;
			ticktock++;
			
			// write buff to target SRAM
			if (ticktock & 1)
			{
				if (ERROR_OK != adi_memap_write_buf(
						LM3S_SRAM_ADDR + 1024 + page_size, buff, page_size))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
					return ERRCODE_FAILURE_OPERATION;
				}
			}
			else
			{
				if (ERROR_OK != adi_memap_write_buf(
						LM3S_SRAM_ADDR + 1024, buff, page_size))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
					return ERRCODE_FAILURE_OPERATION;
				}
			}
			
			// wait ready
			result = 0;
			if (ERROR_OK != lm3sswj_iap_wait_ready(&result))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			cmd.FMA_addr = LM3S_FLASHCTL_FMA;
			cmd.tgt_addr = addr;
			if (ticktock & 1)
			{
				cmd.src_addr = LM3S_SRAM_ADDR + 1024 + page_size;	// Source RAM address
			}
			else
			{
				cmd.src_addr = LM3S_SRAM_ADDR + 1024;				// Source RAM address
			}
			cmd.command = LM3S_FLASHCTL_FMC_WRITE | LM3S_FLASHCTL_FMC_KEY;
			cmd.cnt = page_size / 4;
			result = 0;
			if (ERROR_OK != lm3sswj_iap_run(&cmd))
			{
				ret = ERROR_FAIL;
				break;
			}
			
			size -= page_size;
			pgbar_update(page_size);
		}
		// wait ready
		result = 0;
		if (ERROR_OK != lm3sswj_iap_wait_ready(&result))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
			return ERRCODE_FAILURE_OPERATION;
		}
		pgbar_update(page_size);
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

