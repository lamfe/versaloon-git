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

#include "timer.h"

#include "memlist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "cm3.h"
#include "cm3_lpc1000.h"
#include "lpc1000.h"

#include "cm3_internal.h"
#include "lpc1000_internal.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

#include "timer.h"

ENTER_PROGRAM_MODE_HANDLER(lpc1000swj);
LEAVE_PROGRAM_MODE_HANDLER(lpc1000swj);
ERASE_TARGET_HANDLER(lpc1000swj);
WRITE_TARGET_HANDLER(lpc1000swj);
READ_TARGET_HANDLER(lpc1000swj);
const struct program_functions_t lpc1000swj_program_functions = 
{
	NULL, 
	ENTER_PROGRAM_MODE_FUNCNAME(lpc1000swj), 
	LEAVE_PROGRAM_MODE_FUNCNAME(lpc1000swj), 
	ERASE_TARGET_FUNCNAME(lpc1000swj), 
	WRITE_TARGET_FUNCNAME(lpc1000swj), 
	READ_TARGET_FUNCNAME(lpc1000swj)
};

#define LPC1000_IAP_BASE				LPC1000_SRAM_ADDR
#define LPC1000_IAP_COMMAND_OFFSET		64
#define LPC1000_IAP_COMMAND_ADDR		(LPC1000_IAP_BASE + LPC1000_IAP_COMMAND_OFFSET)
#define LPC1000_IAP_RESULT_OFFSET		92
#define LPC1000_IAP_RESULT_ADDR			(LPC1000_IAP_BASE + LPC1000_IAP_RESULT_OFFSET)
#define LPC1000_IAP_PARAM_OFFSET		32
#define LPC1000_IAP_SYNC_ADDR			(LPC1000_IAP_BASE + 88)
static uint8_t iap_code[] = {
							// wait_start:
	0x15, 0x48,				// ldr		r0, [PC, #XX]		// load sync
	0x00, 0x28,				// cmp		r0, #0
	0xFC, 0xD0,				// beq 		wait_start
	0x00, 0x00,
							// init:
	0x05, 0x4A,				// ldr		r2, [PC, #XX]		// laod address of iap_entry
	0x06, 0x48,				// ldr		r0, [PC, #XX]		// load address of command
	0x06, 0x49,				// ldr		r1, [PC, #XX]		// load address of result
	
	0x90, 0x47,				// blx		r2
	0x00, 0x00,
	0x06, 0x48,				// ldr		r0, [PC, #XX]		// load address of sync
	0x00, 0x21,				// mov		r1, #0
	0x00, 0x00, 
	0x01, 0x60,				// str		r1, [r0, #0]
	0xF1, 0xE7,				// b		wait_start
	0xFE, 0xE7,				// b		$
	0, 0,					// fill
							// parameter
	0, 0, 0, 0,				// address of iap_entry
	0, 0, 0, 0,				// address of command
	0, 0, 0, 0,				// address of result
	(LPC1000_IAP_SYNC_ADDR >> 0) & 0xFF,	// address of aync
	(LPC1000_IAP_SYNC_ADDR >> 8) & 0xFF,
	(LPC1000_IAP_SYNC_ADDR >> 16) & 0xFF,
	(LPC1000_IAP_SYNC_ADDR >> 24) & 0xFF,
	0, 0, 0, 0,				// reserved0
	0, 0, 0, 0,				// reserved1
	0, 0, 0, 0,				// reserved2
	0, 0, 0, 0,				// reserved3
	
	0, 0, 0, 0,				// command, offset is 2
	0, 0, 0, 0,				// param[0]
	0, 0, 0, 0,				// param[1]
	0, 0, 0, 0,				// param[2]
	0, 0, 0, 0,				// param[3]
	0, 0, 0, 0,				// param[4]
	
	0, 0, 0, 0,				// sync
	
	0, 0, 0, 0,				// result
	0, 0, 0, 0,				// reply[0]
	0, 0, 0, 0,				// reply[1]
	0, 0, 0, 0,				// reply[2]
	0, 0, 0, 0				// reply[3]
};

static RESULT lpc1000swj_debug_info(void)
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
	
	LOG_INFO("SRAM dump at 0x%08X:", LPC1000_IAP_BASE);
	if (ERROR_OK != adi_memap_read_buf(LPC1000_IAP_BASE, buffer, 
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

static RESULT lpc1000swj_iap_run(uint32_t cmd, uint32_t param_table[5])
{
	uint32_t buff_tmp[7];
	
	memset(buff_tmp, 0, sizeof(buff_tmp));
	buff_tmp[0] = cmd;				// iap command
	buff_tmp[1] = param_table[0];	// iap parameters
	buff_tmp[2] = param_table[1];
	buff_tmp[3] = param_table[2];
	buff_tmp[4] = param_table[3];
	buff_tmp[5] = param_table[4];
	buff_tmp[6] = 1;				// sync
	
	// write iap command with sync to target SRAM
	// sync is 4-byte AFTER command in sram
	if (ERROR_OK != adi_memap_write_buf(LPC1000_IAP_COMMAND_ADDR, 
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

static RESULT lpc1000swj_iap_poll_result(uint32_t result_table[4], uint8_t *fail)
{
	uint32_t buff_tmp[6];
	
	*fail = 0;
	
	// read result and sync
	// sync is 4-byte BEFORE result
	if (ERROR_OK != adi_memap_read_buf(LPC1000_IAP_SYNC_ADDR, 
										(uint8_t *)buff_tmp, 24))
	{
		*fail = 1;
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (0 == buff_tmp[0])
	{
		if (buff_tmp[1] != 0)
		{
			*fail = 1;
			lpc1000swj_debug_info();
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRCODE, "call iap", 
						buff_tmp[1]);
			return ERRCODE_FAILURE_OPERATION;
		}
		
		memcpy(result_table, &buff_tmp[2], 16);
		return ERROR_OK;
	}
	
	return ERROR_FAIL;
}

static RESULT lpc1000swj_iap_wait_ready(uint32_t result_table[4])
{
	uint8_t fail = 0;
	uint32_t start, end;
	
	start = get_time_in_ms();
	while (1)
	{
		if (ERROR_OK != lpc1000swj_iap_poll_result(result_table, &fail))
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
					lpc1000swj_debug_info();
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

static RESULT lpc1000swj_iap_call(uint32_t cmd, uint32_t param_table[5], 
									uint32_t result_table[4])
{	
	if ((ERROR_OK != lpc1000swj_iap_run(cmd, param_table)) 
		|| (ERROR_OK != lpc1000swj_iap_wait_ready(result_table)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap command");
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

ENTER_PROGRAM_MODE_HANDLER(lpc1000swj)
{
	uint32_t *para_ptr = (uint32_t*)&iap_code[LPC1000_IAP_PARAM_OFFSET];
	uint32_t reg;
	
	REFERENCE_PARAMETER(context);
	
	para_ptr[0] = LPC1000_IAP_ENTRY;
	para_ptr[1] = LPC1000_IAP_COMMAND_ADDR;
	para_ptr[2] = LPC1000_IAP_RESULT_ADDR;
	
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt lpc1000");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write sp
	reg = LPC1000_SRAM_ADDR + 1024;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_SP, &reg))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write SP");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write iap_code to target SRAM
	if (ERROR_OK != adi_memap_write_buf(LPC1000_IAP_BASE, (uint8_t*)iap_code, 
											sizeof(iap_code)))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load iap_code to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write pc
	reg = LPC1000_IAP_BASE + 1;
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

LEAVE_PROGRAM_MODE_HANDLER(lpc1000swj)
{
	uint32_t reg;
	
	if (cm3_execute_flag && success 
		&& (context->op->write_operations & APPLICATION))
	{
		if (ERROR_OK != cm3_dp_halt())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt lpc1000");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (ERROR_OK != 
				cm3_write_core_register(CM3_COREREG_PC, &cm3_execute_addr))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write PC");
			return ERRCODE_FAILURE_OPERATION;
		}
		reg = 0;
		if ((ERROR_OK != cm3_read_core_register(CM3_COREREG_PC, &reg)) 
			|| (reg != cm3_execute_addr))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "verify written PC");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (ERROR_OK != cm3_dp_run())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run code");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	return ERROR_OK;
}

ERASE_TARGET_HANDLER(lpc1000swj)
{
	struct program_info_t *pi = context->pi;
	RESULT ret= ERROR_OK;
	uint32_t iap_cmd_param[5], iap_reply[4];
	uint32_t sector = lpc1000_get_sector_idx_by_addr(context, 
								pi->program_areas[APPLICATION_IDX].size - 1);
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		iap_cmd_param[0] = 0;				// Start Sector Number
		iap_cmd_param[1] = sector;			// End Sector Number
		iap_cmd_param[2] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_PREPARE_SECTOR, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "prepare sectors");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		iap_cmd_param[0] = 0;				// Start Sector Number
		iap_cmd_param[1] = sector;			// End Sector Number
		iap_cmd_param[2] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_ERASE_SECTOR, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "erase sectors");
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

WRITE_TARGET_HANDLER(lpc1000swj)
{
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	uint32_t iap_cmd_param[5], iap_reply[4];
	uint32_t start_sector;
	uint8_t pingpong = 0;
	uint16_t page_size = (uint16_t)param->chip_areas[APPLICATION_IDX].page_size;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (param->chip_areas[SRAM_IDX].size < (uint32_t)(1024 + 2 * page_size))
		{
			// pingpong mode is not available
			
			// check
			if ((addr & 0xFF) 
				|| ((size != 256) && (size != 512) && (size != 1024) && (size != 4096)))
			{
				LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
				return ERROR_FAIL;
			}
			
			memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
			memset(iap_reply, 0, sizeof(iap_reply));
			start_sector = lpc1000_get_sector_idx_by_addr(context, addr);
			iap_cmd_param[0] = start_sector;	// Start Sector Number
			iap_cmd_param[1] = start_sector;	// End Sector Number
			iap_cmd_param[2] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
			if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_PREPARE_SECTOR, 
													iap_cmd_param, iap_reply))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "prepare sectors");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			// write buff to target SRAM
			if (ERROR_OK != adi_memap_write_buf(LPC1000_SRAM_ADDR + 1024, buff, size))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
			memset(iap_reply, 0, sizeof(iap_reply));
			iap_cmd_param[0] = addr;			// Destination flash address
			iap_cmd_param[1] = LPC1000_SRAM_ADDR + 1024;	// Source RAM address
			iap_cmd_param[2] = size;			// Number of bytes to be written
			iap_cmd_param[3] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
			if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_RAM_TO_FLASH, 
													iap_cmd_param, iap_reply))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "download flash");
				return ERRCODE_FAILURE_OPERATION;
			}
			break;
		}
		
		// check
		if ((addr % page_size) || (size % page_size))
		{
			LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
			return ERROR_FAIL;
		}
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		start_sector = lpc1000_get_sector_idx_by_addr(context, addr);
		iap_cmd_param[0] = start_sector;	// Start Sector Number
		iap_cmd_param[1] = start_sector;	// End Sector Number
		iap_cmd_param[2] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_PREPARE_SECTOR, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "prepare sectors");
			return ERRCODE_FAILURE_OPERATION;
		}
		
		// write first buff to target SRAM
		if (ERROR_OK != adi_memap_write_buf(LPC1000_SRAM_ADDR + 1024, buff, page_size))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
			return ERRCODE_FAILURE_OPERATION;
		}
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		iap_cmd_param[0] = addr;			// Destination flash address
		iap_cmd_param[1] = LPC1000_SRAM_ADDR + 1024;	// Source RAM address
		iap_cmd_param[2] = page_size;		// Number of bytes to be written
		iap_cmd_param[3] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
		if (ERROR_OK != lpc1000swj_iap_run(LPC1000_IAPCMD_RAM_TO_FLASH, 
											iap_cmd_param))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
			return ERRCODE_FAILURE_OPERATION;
		}
		size -= page_size;
		
		while (size)
		{
			buff += page_size;
			addr += page_size;
			pingpong++;
			
			// write buff to target SRAM
			if (pingpong & 1)
			{
				if (ERROR_OK != adi_memap_write_buf(
						LPC1000_SRAM_ADDR + 1024 + page_size, buff, page_size))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
					return ERRCODE_FAILURE_OPERATION;
				}
			}
			else
			{
				if (ERROR_OK != adi_memap_write_buf(
						LPC1000_SRAM_ADDR + 1024, buff, page_size))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "load data to SRAM");
					return ERRCODE_FAILURE_OPERATION;
				}
			}
			
			// wait ready
			memset(iap_reply, 0, sizeof(iap_reply));
			if (ERROR_OK != lpc1000swj_iap_wait_ready(iap_reply))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
			memset(iap_reply, 0, sizeof(iap_reply));
			start_sector = lpc1000_get_sector_idx_by_addr(context, addr);
			iap_cmd_param[0] = start_sector;	// Start Sector Number
			iap_cmd_param[1] = start_sector;	// End Sector Number
			iap_cmd_param[2] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
			if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_PREPARE_SECTOR, 
													iap_cmd_param, iap_reply))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "prepare sectors");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
			iap_cmd_param[0] = addr;			// Destination flash address
			if (pingpong & 1)
			{
				iap_cmd_param[1] = LPC1000_SRAM_ADDR + 1024 + page_size;	// Source RAM address
			}
			else
			{
				iap_cmd_param[1] = LPC1000_SRAM_ADDR + 1024;	// Source RAM address
			}
			iap_cmd_param[2] = page_size;		// Number of bytes to be written
			iap_cmd_param[3] = pi->kernel_khz;	// CPU Clock Frequency(in kHz)
			if (ERROR_OK != lpc1000swj_iap_run(LPC1000_IAPCMD_RAM_TO_FLASH, 
													iap_cmd_param))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
				return ERRCODE_FAILURE_OPERATION;
			}
			
			size -= page_size;
			pgbar_update(page_size);
		}
		// wait ready
		memset(iap_reply, 0, sizeof(iap_reply));
		if (ERROR_OK != lpc1000swj_iap_wait_ready(iap_reply))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run iap");
			return ERRCODE_FAILURE_OPERATION;
		}
		pgbar_update(page_size);
		break;
	default:
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

READ_TARGET_HANDLER(lpc1000swj)
{
	RESULT ret = ERROR_OK;
	uint32_t cur_block_size;
	uint32_t iap_cmd_param[5], iap_reply[4];
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_READ_BOOTVER, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read bootver");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		LOG_INFO(INFOMSG_BOOTLOADER_VERSION, (iap_reply[0] >> 8) & 0xFF, 
					(iap_reply[0] >> 0) & 0xFF);
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_READ_SERIAL, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read serialnum");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		LOG_INFO("Serian Number: %08X%08X%08X%08X", 
					iap_reply[3], iap_reply[2], iap_reply[1], iap_reply[0]);
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_READ_ID, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read id");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		*(uint32_t*)buff = iap_reply[0];
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

