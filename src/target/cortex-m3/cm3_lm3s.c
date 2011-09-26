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
#include <time.h>

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
#include "cm3_lm3s.h"

#include "cm3_internal.h"
#include "lm3s_internal.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

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

static uint32_t lm3sswj_iap_cnt = 0;

#define LM3S_IAP_BASE			LM3S_SRAM_ADDR
#define LM3S_IAP_COMMAND_OFFSET	100
#define LM3S_IAP_COMMAND_ADDR	(LM3S_IAP_BASE + LM3S_IAP_COMMAND_OFFSET)
#define LM3S_IAP_SYNC_ADDR		(LM3S_IAP_BASE + 120)
#define LM3S_IAP_CNT_ADDR		(LM3S_IAP_BASE + 124)
static uint8_t iap_code[] =
{
							// wait_start:
	0x1D, 0x48,				// ldr		r0, [PC, #116]		// load sync
	0x00, 0x28,				// cmp		r0, #0
	0xFC, 0xD0,				// beq 		wait_start
							// init:
	0x18, 0x4A,				// ldr		r2, [PC, #96]		// laod tgt_addr
	0x18, 0x4B,				// ldr		r3, [PC, #96]		// load src_addr
	0x19, 0x4C,				// ldr		r4, [PC, #100]		// load command
	0x19, 0x4D,				// ldr		r5, [PC, #100]		// load cnt
	0x15, 0x4E,				// ldr		r6, [PC, #84]		// laod address of FMA
							// clear_sync:
	0x19, 0xA0,				// add		r0, PC, #100		// load address of sync
	0x00, 0x21,				// mov		r1, #0
	0x01, 0x60,				// str		r1, [r0]
							// do_operation:
	0x30, 0x46,				// mov		r0, r6				// load address of FMA
	0x02, 0x60,				// str		r2, [r0]			// FMA = tgt_addr
	0x00, 0x1D,				// adds		r0, r0, #4			// &FMD = &FMA + 4
	0x19, 0x68,				// ldr		r1, [r3]			// load src_data
	0x01, 0x60,				// str		r1, [r0]			// FMD = *str_data
	0x00, 0x1D,				// adds		r0, r0, #4			// &FMC = &FMD + 4
	0x04, 0x60,				// str		r4, [r0]			// FMC = command
							// wait_operation_finish:
	0x01, 0x68,				// ldr		r1, [r0]			// r1 = FMC
	0x21, 0x40,				// ands		r1, r1, r4			// r1 = r1 & command
	0xFC, 0xD1,				// bne		wait_operation_finish
							// adjust_param:
	0x12, 0x1D,				// adds		r2, r2, #4			// tgt_addr + 4
	0x1B, 0x1D,				// adds		r3, r3, #4			// src_addr + 4
	0x6D, 0x1E,				// subs		r5, r5, #1			// cnt - 1
	0xF1, 0xD1,				// bne		do_operation
							// adjust_iap_cnt:
	0x12, 0xA0,				// add		r0, PC, #72			// load address of iap_cnt
	0x01, 0x68,				// ldr		r1, [r0]
	0x49, 0x1C,				// adds		r1, r1, #1
	0x01, 0x60,				// str		r1, [r0]
							// wait_next_run:
	0xE1, 0xE7,				// b		wait_start
	0xFE, 0xE7,				// b		$
	// 62 bytes above
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
	0x00, 0x00,
	// 100 bytes above
	// parameters
	0x00, 0x00, 0x00, 0x00,	// FMA_addr
	0x00, 0x00, 0x00, 0x00,	// tgt_addr
	0x00, 0x00, 0x00, 0x00,	// src_addr
	0x00, 0x00, 0x00, 0x00,	// command
	0x00, 0x00, 0x00, 0x00,	// cnt
	0x00, 0x00, 0x00, 0x00,	// sync
	0x00, 0x00, 0x00, 0x00	// iap_cnt
};

struct lm3sswj_iap_cmd_t
{
	uint32_t FMA_addr;
	uint32_t tgt_addr;
	uint32_t src_addr;
	uint32_t command;
	uint32_t cnt;
};

static RESULT lm3sswj_iap_poll_finish(uint32_t cnt_idx, uint8_t *fail)
{
	uint32_t iap_cnt;
	
	*fail = 0;
	
	// read busy
	if (ERROR_OK != adi_memap_read_reg32(LM3S_IAP_CNT_ADDR, &iap_cnt, 1))
	{
		*fail = 1;
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	iap_cnt = LE_TO_SYS_U32(iap_cnt);
	if (iap_cnt > lm3sswj_iap_cnt)
	{
		*fail = 1;
		cm3_dump(LM3S_IAP_BASE, sizeof(iap_code));
		return ERROR_FAIL;
	}
	if (iap_cnt == cnt_idx)
	{
		return ERROR_OK;
	}
	
	return ERROR_FAIL;
}

static RESULT lm3sswj_iap_poll_param_taken(uint8_t *fail)
{
	uint32_t sync;
	
	*fail = 0;
	
	// read sync
	if (ERROR_OK != adi_memap_read_reg32(LM3S_IAP_SYNC_ADDR, &sync, 1))
	{
		*fail = 1;
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	sync = LE_TO_SYS_U32(sync);
	if (0 == sync)
	{
		return ERROR_OK;
	}
	
	return ERROR_FAIL;
}

static RESULT lm3sswj_iap_wait_param_taken(void)
{
	uint8_t fail = 0;
	uint32_t start, end;
	
	start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	while (1)
	{
		if (ERROR_OK != lm3sswj_iap_poll_param_taken(&fail))
		{
			if (fail)
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll iap param taken");
				return ERROR_FAIL;
			}
			else
			{
				end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
				// wait 1s at most
				if ((end - start) > 1000)
				{
					cm3_dump(LM3S_IAP_BASE, sizeof(iap_code));
					LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap param taken");
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

static RESULT lm3sswj_iap_wait_finish(uint32_t cnt_idx)
{
	uint8_t fail = 0;
	uint32_t start, end;
	
	start = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
	while (1)
	{
		if (ERROR_OK != lm3sswj_iap_poll_finish(cnt_idx, &fail))
		{
			if (fail)
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "poll iap finish");
				return ERROR_FAIL;
			}
			else
			{
				end = (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
				// wait 1s at most
				if ((end - start) > 1000)
				{
					cm3_dump(LM3S_IAP_BASE, sizeof(iap_code));
					LOG_ERROR(ERRMSG_TIMEOUT, "wait for iap finish");
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

static RESULT lm3sswj_iap_run(struct lm3sswj_iap_cmd_t * cmd)
{
	uint32_t buff_tmp[6];
	
	if (ERROR_OK != lm3sswj_iap_wait_param_taken())
	{
		return ERROR_FAIL;
	}
	
	memset(buff_tmp, 0, sizeof(buff_tmp));
	buff_tmp[0] = cmd->FMA_addr;
	buff_tmp[1] = cmd->tgt_addr;
	buff_tmp[2] = cmd->src_addr;
	buff_tmp[3] = cmd->command;
	buff_tmp[4] = cmd->cnt;
	buff_tmp[5] = 1;				// sync
	
	// write iap command with sync to target SRAM
	// sync is 4-byte AFTER command in sram
	if (ERROR_OK != adi_memap_write_buf(LM3S_IAP_COMMAND_ADDR,
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	lm3sswj_iap_cnt++;
	
	return ERROR_OK;
}

ENTER_PROGRAM_MODE_HANDLER(lm3sswj)
{
	uint32_t reg;
	uint8_t verify_buff[sizeof(iap_code)];
	
	REFERENCE_PARAMETER(context);
	lm3sswj_iap_cnt = 0;
	
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt lm3s");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// disable flash programming interrupts
	reg = 0;
	if (ERROR_OK != adi_memap_write_reg32(LM3S_FLASHCTL_FCIM, &reg, 0))
	{
		return ERROR_FAIL;
	}
	reg = LM3S_FLASHCTL_INT_PROGRAMMING | LM3S_FLASHCTL_INT_ACCESS;
	if (ERROR_OK != adi_memap_write_reg32(LM3S_FLASHCTL_FCMISC, &reg, 1))
	{
		return ERROR_FAIL;
	}
	
	// unlock
	// 0xFFFFFFFF to FMPRE and FMPPE
	reg = 0xFFFFFFFF;
	if (ERROR_OK != adi_memap_write_reg32(LM3S_SYSCTL_FMPRE, &reg, 0))
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != adi_memap_write_reg32(LM3S_SYSCTL_FMPPE, &reg, 1))
	{
		return ERROR_FAIL;
	}
	// commit FMPRE
	reg = 0;
	if (ERROR_OK != adi_memap_write_reg32(LM3S_FLASHCTL_FMA, &reg, 1))
	{
		return ERROR_FAIL;
	}
	reg = LM3S_FLASHCTL_FMC_COMT;
	if (ERROR_OK != adi_memap_write_reg32(LM3S_FLASHCTL_FMC, &reg, 1))
	{
		return ERROR_FAIL;
	}
	// commit EMPPE
	reg = 1;
	if (ERROR_OK != adi_memap_write_reg32(LM3S_FLASHCTL_FMA, &reg, 1))
	{
		return ERROR_FAIL;
	}
	reg = LM3S_FLASHCTL_FMC_COMT;
	if (ERROR_OK != adi_memap_write_reg32(LM3S_FLASHCTL_FMC, &reg, 1))
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
	// verify iap_code
	memset(verify_buff, 0, sizeof(iap_code));
	if (ERROR_OK != adi_memap_read_buf(LM3S_IAP_BASE, verify_buff,
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
	
	// write pc
	reg = LM3S_IAP_BASE + 1;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_PC, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write PC");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (ERROR_OK != cm3_dp_resume())
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
	
	return lm3sswj_iap_wait_finish(lm3sswj_iap_cnt);
}

ERASE_TARGET_HANDLER(lm3sswj)
{
	RESULT ret= ERROR_OK;
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
		if ((ERROR_OK != lm3sswj_iap_run(&cmd)) ||
			(ERROR_OK != lm3sswj_iap_wait_finish(lm3sswj_iap_cnt)))
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

WRITE_TARGET_HANDLER(lm3sswj)
{
	RESULT ret = ERROR_OK;
	struct lm3sswj_iap_cmd_t cmd;
	uint32_t page_size = 512;
	static uint8_t ticktock = 0;
	uint32_t ram_addr;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		// check alignment
		if ((size % 4) || (addr % 4))
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "flash addr and/or size");
			return ERROR_FAIL;
		}
		
		cmd.FMA_addr = LM3S_FLASHCTL_FMA;
		cmd.command = LM3S_FLASHCTL_FMC_WRITE | LM3S_FLASHCTL_FMC_KEY;
		cmd.cnt = page_size / 4;
		while (size)
		{
			if (ticktock & 1)
			{
				ram_addr = LM3S_SRAM_ADDR + 1024 + page_size;
			}
			else
			{
				ram_addr = LM3S_SRAM_ADDR + 1024;
			}
			cmd.tgt_addr = addr;
			cmd.src_addr = ram_addr;
			if ((ERROR_OK != adi_memap_write_buf(ram_addr, buff, page_size)) ||
				(ERROR_OK != lm3sswj_iap_run(&cmd)))
			{
				ret = ERROR_FAIL;
				break;
			}
			ticktock++;
			size -= page_size;
			buff += page_size;
			addr += page_size;
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
				adi_memap_read_reg32(LM3S_SYSCTL_DID0, &lm3s_device.did0, 0))
			|| (ERROR_OK !=
				adi_memap_read_reg32(LM3S_SYSCTL_DID1, &lm3s_device.did1, 0))
			|| (ERROR_OK !=
				adi_memap_read_reg32(LM3S_SYSCTL_DC0, &lm3s_device.dc0, 0))
			|| (ERROR_OK !=
				adi_memap_read_reg32(LM3S_SYSCTL_DC1, &lm3s_device.dc1, 0))
			|| (ERROR_OK !=
				adi_memap_read_reg32(LM3S_SYSCTL_DC2, &lm3s_device.dc2, 0))
			|| (ERROR_OK !=
				adi_memap_read_reg32(LM3S_SYSCTL_DC3, &lm3s_device.dc3, 1)))
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		lm3s_device.did0 = LE_TO_SYS_U32(lm3s_device.did0);
		lm3s_device.did1 = LE_TO_SYS_U32(lm3s_device.did1);
		lm3s_device.dc0 = LE_TO_SYS_U32(lm3s_device.dc0);
		lm3s_device.dc1 = LE_TO_SYS_U32(lm3s_device.dc1);
		lm3s_device.dc2 = LE_TO_SYS_U32(lm3s_device.dc2);
		lm3s_device.dc3 = LE_TO_SYS_U32(lm3s_device.dc3);
		if (ERROR_OK != lm3s_check_device(&lm3s_device))
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

