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
#include "cm3_lpc1000.h"
#include "lpc1000.h"

#include "cm3_internal.h"
#include "lpc1000_internal.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

#include "timer.h"

#define LPC1000_IAPCMD_PREPARE_SECTOR	50
#define LPC1000_IAPCMD_RAM_TO_FLASH		51
#define LPC1000_IAPCMD_ERASE_SECTOR		52
#define LPC1000_IAPCMD_BLANK_CHECK		53
#define LPC1000_IAPCMD_READ_ID			54
#define LPC1000_IAPCMD_READ_BOOTVER		55
#define LPC1000_IAPCMD_COMPARE			56
#define LPC1000_IAPCMD_REINVOKE			57
#define LPC1000_IAPCMD_READ_SERIAL		58

RESULT lpc1000swj_enter_program_mode(struct program_context_t *context);
RESULT lpc1000swj_leave_program_mode(struct program_context_t *context, 
									uint8_t success);
RESULT lpc1000swj_erase_target(struct program_context_t *context, char area, 
								uint32_t addr, uint32_t size);
RESULT lpc1000swj_write_target(struct program_context_t *context, char area, 
								uint32_t addr, uint8_t *buff, uint32_t size);
RESULT lpc1000swj_read_target(struct program_context_t *context, char area, 
								uint32_t addr, uint8_t *buff, uint32_t size);
const struct program_functions_t lpc1000swj_program_functions = 
{
	NULL, 
	lpc1000swj_enter_program_mode, 
	lpc1000swj_leave_program_mode, 
	lpc1000swj_erase_target, 
	lpc1000swj_write_target, 
	lpc1000swj_read_target
};

static RESULT lpc1000swj_iap_call(uint32_t cmd, uint32_t param_table[5], 
									uint32_t result_table[4])
{
	uint32_t reg;
#define LPC1000_IAP_OFFSET				0
#define LPC1000_IAP_COMMAND_OFFSET		8
#define LPC1000_IAP_REPLY_OFFSET			32
	uint8_t iap_code[] = {
		0x60, 0x47, 0x60, 0x47,	// bx r12
//		0x00, 0xbe, 0x00, 0xbe,	// bkpt
		0xFE, 0xE7,				// b $
		0x00, 0x00,				// fill
		0, 0, 0, 0,				// command, offset is 2
		0, 0, 0, 0,				// param[0]
		0, 0, 0, 0,				// param[1]
		0, 0, 0, 0,				// param[2]
		0, 0, 0, 0,				// param[3]
		0, 0, 0, 0,				// param[4]
		0, 0, 0, 0,				// result
		0, 0, 0, 0,				// reply[0]
		0, 0, 0, 0,				// reply[1]
		0, 0, 0, 0,				// reply[2]
		0, 0, 0, 0				// reply[3]
	};
	uint32_t reply_tmp[5];
	
	*(uint32_t*)&iap_code[LPC1000_IAP_COMMAND_OFFSET + 0 * 4] = cmd;
	*(uint32_t*)&iap_code[LPC1000_IAP_COMMAND_OFFSET + 1 * 4] = param_table[0];
	*(uint32_t*)&iap_code[LPC1000_IAP_COMMAND_OFFSET + 2 * 4] = param_table[1];
	*(uint32_t*)&iap_code[LPC1000_IAP_COMMAND_OFFSET + 3 * 4] = param_table[2];
	*(uint32_t*)&iap_code[LPC1000_IAP_COMMAND_OFFSET + 4 * 4] = param_table[3];
	*(uint32_t*)&iap_code[LPC1000_IAP_COMMAND_OFFSET + 5 * 4] = param_table[4];
	
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "halt lpc1000");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write iap_code to target SRAM
	if (ERROR_OK != adi_memap_write_buf(LPC1000_SRAM_ADDR + LPC1000_IAP_OFFSET, 
										(uint8_t*)iap_code, sizeof(iap_code)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "load iap_code to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write r0 = commmand address
	reg = LPC1000_SRAM_ADDR + LPC1000_IAP_OFFSET + LPC1000_IAP_COMMAND_OFFSET;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_R0, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write R0");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write r1 = reply address
	reg = LPC1000_SRAM_ADDR + LPC1000_IAP_OFFSET + LPC1000_IAP_REPLY_OFFSET;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_R1, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write R1");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write r12 = iap_entry_point
	reg = LPC1000_IAP_ENTRY;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_R12, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write R12");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write sp
	reg = LPC1000_SRAM_ADDR + 1024;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_SP, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write SP");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write lr
	reg = LPC1000_SRAM_ADDR + LPC1000_IAP_OFFSET + 4 + 1;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_LR, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write LR");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write pc
	reg = LPC1000_SRAM_ADDR + LPC1000_IAP_OFFSET + 1;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_PC, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write PC");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (ERROR_OK != cm3_dp_run())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "run iap");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write iap result from target SRAM
	if (ERROR_OK != adi_memap_read_buf(
			LPC1000_SRAM_ADDR + LPC1000_IAP_OFFSET + LPC1000_IAP_REPLY_OFFSET, 
			(uint8_t*)reply_tmp, 20))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read iap result");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (reply_tmp[0] != 0)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ERRCODE), 
				  "call iap", reply_tmp[0]);
		return ERRCODE_FAILURE_OPERATION;
	}
	memcpy(result_table, &reply_tmp[1], 16);
	
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "halt lpc1000");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

RESULT lpc1000swj_enter_program_mode(struct program_context_t *context)
{
	REFERENCE_PARAMETER(context);
	return ERROR_OK;
}

RESULT lpc1000swj_leave_program_mode(struct program_context_t *context, 
									uint8_t success)
{
	uint32_t reg;
	
	if (cm3_execute_flag && success 
		&& (context->op->write_operations & APPLICATION))
	{
		if (ERROR_OK != cm3_dp_halt())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "halt lpc1000");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (ERROR_OK != 
				cm3_write_core_register(CM3_COREREG_PC, &cm3_execute_addr))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write PC");
			return ERRCODE_FAILURE_OPERATION;
		}
		reg = 0;
		if ((ERROR_OK != cm3_read_core_register(CM3_COREREG_PC, &reg)) 
			|| (reg != cm3_execute_addr))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "verify written PC");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (ERROR_OK != cm3_dp_run())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "run code");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	return ERROR_OK;
}

RESULT lpc1000swj_erase_target(struct program_context_t *context, char area, 
								uint32_t addr, uint32_t size)
{
	RESULT ret= ERROR_OK;
	uint32_t iap_cmd_param[5], iap_reply[4];
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		iap_cmd_param[0] = 0;		// Start Sector Number
		iap_cmd_param[1] = 21;		// End Sector Number
		iap_cmd_param[2] = 4000;	// CPU Clock Frequency(in kHz)
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_PREPARE_SECTOR, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "prepare sectors");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		iap_cmd_param[0] = 0;		// Start Sector Number
		iap_cmd_param[1] = 21;		// End Sector Number
		iap_cmd_param[2] = 4000;	// CPU Clock Frequency(in kHz)
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_ERASE_SECTOR, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase sectors");
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

RESULT lpc1000swj_write_target(struct program_context_t *context, char area, 
								uint32_t addr, uint8_t *buff, uint32_t size)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(size);
	return ERROR_OK;
}

RESULT lpc1000swj_read_target(struct program_context_t *context, char area, 
								uint32_t addr, uint8_t *buff, uint32_t size)
{
	RESULT ret = ERROR_OK;
	uint32_t cur_block_size;
	uint32_t iap_cmd_param[5], iap_reply[4];
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case CHIPID_CHAR:
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_READ_BOOTVER, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read bootver");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		LOG_INFO(_GETTEXT("Bootloader Version: %d.%d\n"), 
					(iap_reply[0] >> 8) & 0xFF, 
					(iap_reply[0] >> 0) & 0xFF);
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_READ_SERIAL, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read serialnum");
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		LOG_INFO(_GETTEXT("Serian Number: %08X%08X%08X%08X\n"), 
					iap_reply[3], iap_reply[2], iap_reply[1], iap_reply[0]);
		
		memset(iap_cmd_param, 0, sizeof(iap_cmd_param));
		memset(iap_reply, 0, sizeof(iap_reply));
		if (ERROR_OK != lpc1000swj_iap_call(LPC1000_IAPCMD_READ_ID, 
												iap_cmd_param, iap_reply))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read id");
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
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
						  "write flash block", addr);
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

