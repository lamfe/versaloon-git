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

#include "timer.h"

#include "memlist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "cm3.h"
#include "cm3_at91sam3.h"
#include "at91sam3.h"

#include "cm3_internal.h"
#include "at91sam3_internal.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

#include "timer.h"

RESULT at91sam3swj_enter_program_mode(struct program_context_t *context);
RESULT at91sam3swj_leave_program_mode(struct program_context_t *context, 
									uint8_t success);
RESULT at91sam3swj_erase_target(struct program_context_t *context, char area, 
								uint32_t addr, uint32_t size);
RESULT at91sam3swj_write_target(struct program_context_t *context, char area, 
								uint32_t addr, uint8_t *buff, uint32_t size);
RESULT at91sam3swj_read_target(struct program_context_t *context, char area, 
								uint32_t addr, uint8_t *buff, uint32_t size);
const struct program_functions_t at91sam3swj_program_functions = 
{
	NULL, 
	at91sam3swj_enter_program_mode, 
	at91sam3swj_leave_program_mode, 
	at91sam3swj_erase_target, 
	at91sam3swj_write_target, 
	at91sam3swj_read_target
};

#define AT91SAM3_IAP_BASE				AT91SAM3_SRAM_ADDR
#define AT91SAM3_IAP_PARAM_OFFSET		96
#define AT91SAM3_IAP_COMMAND_ADDR		(AT91SAM3_IAP_BASE + 112)
#define AT91SAM3_IAP_SYNC_ADDR			(AT91SAM3_IAP_BASE + 144)
#define AT91SAM3_IAP_DATA_ADDR			(AT91SAM3_IAP_BASE + 148)
#define AT91SAM3_IAP_TYPE				0
static uint8_t iap_code[] = {
							// wait_start:
	0x23, 0x48,				// ldr		r0, [PC, #XX]		// load sync
	0x00, 0x28,				// cmp		r0, #0
	0xFC, 0xD1,				// bne 		wait_start
	0x00, 0x00,
							// init:
	0x1D, 0x48,				// ldr		r0, [PC, #XX]		// load number of data
	0x00, 0x28,				// cmp		r0, #0
	0x0D, 0xD0,				// beq		call_iap
	0x00, 0x00,
	
	0x1C, 0x49,				// ldr		r1, [PC, #XX]		// load target addr to write data
	0x1D, 0x4A,				// ldr		r2, [PC, #XX]		// load address of data
							// copy_data:
	0x13, 0x68,				// ldr		r3, [r2]
	0x0B, 0x60,				// str		r3, [r1]
	0x02, 0xF1, 0x04, 0x02,	// add		r2, r2, #4
	0x01, 0xF1, 0x04, 0x01,	// add		r1, r1, #4
	0xA0, 0xF1, 0x01, 0x00,	// sub		r0, r0, #1
	0x00, 0x28,				// cmp		r0, #0
	0xF5, 0xD1,				// bne		copy_data
	0x00, 0x00,
							// call_iap:
	0x0D, 0x4A,				// ldr		r2, [PC, #XX]		// laod address of iap_entry
	0x10, 0x48,				// ldr		r0, [PC, #XX]		// load flash plane index
	0x11, 0x49,				// ldr		r1, [PC, #XX]		// load command
	
	0x90, 0x47,				// blx		r2
	0x00, 0x00,
	
	0x0B, 0x49,				// ldr		r1, [PC, #XX]		// load address of sync
	0x10, 0x4A,				// ldr		r2, [PC, #XX]		// load number of result
	0x00, 0x2A,				// cmp		r2, #0
	0x0E, 0xD0,				// beq		exit
	0x00, 0x00,
	0x0F, 0x4B,				// ldr		r3, [PC, #XX]		// load address of EEFC_FRR
	0x4F, 0xF0, 0x00, 0x04,	// mov		r4, #0
	0x10, 0x4D,				// ldr		r5, [PC, #XX]		// load address of data
							// read_result:
	0x1E, 0x68,				// ldr		r6, [r3]			// read EEFC_FRR
	0x4F, 0xEA, 0x84, 0x07,	// lsl		r7, r4, #2
	0x2F, 0x44,				// add		r7, r7, r5
	0x3E, 0x60,				// str		r6, [r7]
	0x04, 0xF1, 0x01, 0x04,	// add		r4, r4, #1
	0x94, 0x42,				// cmp		r4, r2
	0xF6, 0xD1,				// bne		read_result
	0x00, 0x00,
							// exit:
	0x08, 0x60,				// str		r0, [r1]			// write iap_result to sync
	
	0xD0, 0xE7,				// b		wait_start
	0xFE, 0xE7,				// b		$
							// fill
							// parameter
	0, 0, 0, 0,				// address of iap_entry
	0, 0, 0, 0,				// address of sync
	0, 0, 0, 0,				// reserved0
	0, 0, 0, 0,				// reserved1
	
							// iap_command parameter:
	0, 0, 0, 0,				// flash plane index
	0, 0, 0, 0x5A,				// command
	0x04, 0, 0, 0,				// number of result
	0x0C, 0x08, 0x0E, 0x40,				// address of EEFC_FRR
	0, 0, 0, 0,				// number of data
	0, 0, 0, 0,				// target addr to write data
	(AT91SAM3_IAP_DATA_ADDR >> 0) & 0xFF,				// address of data
	(AT91SAM3_IAP_DATA_ADDR >> 8) & 0xFF,
	(AT91SAM3_IAP_DATA_ADDR >> 16) & 0xFF,
	(AT91SAM3_IAP_DATA_ADDR >> 24) & 0xFF,
	0, 0, 0, 0,				// reserved0
	
							// iap_reply
	1, 0, 0, 0,				// result status(sync)
	0, 0, 0, 0,				// data from here
};

static RESULT at91sam3swj_debug_info(void)
{
	uint32_t reg;
	uint8_t i;
	uint8_t *buffer;
	RESULT ret = ERROR_OK;
	
	buffer = (uint8_t *)malloc(sizeof(iap_code) + 256);
	if (NULL == buffer)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		ret = ERRCODE_NOT_ENOUGH_MEMORY;
		goto end;
	}
	
	LOG_INFO(_GETTEXT("report to author on this message.\n"));
	
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "halt at91sam3");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	
	for (i = 0; i < 13; i++)
	{
		reg = 0;
		if (ERROR_OK != cm3_read_core_register(i, &reg))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read register");
			ret = ERRCODE_FAILURE_OPERATION;
			goto end;
		}
		LOG_INFO("r%d: %08X\n", i, reg);
	}
	reg = 0;
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_SP, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read sp");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_INFO("sp: %08X\n", reg);
	reg = 0;
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_LR, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read lr");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_INFO("lr: %08X\n", reg);
	reg = 0;
	if (ERROR_OK != cm3_read_core_register(CM3_COREREG_PC, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read pc");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_INFO("pc: %08X\n", reg);
	
	LOG_INFO("SRAM dump at 0x%08X:\n", AT91SAM3_IAP_BASE);
	if (ERROR_OK != adi_memap_read_buf(AT91SAM3_IAP_BASE, buffer, 
												sizeof(iap_code) + 256))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read sram");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_BYTE_BUF(buffer, sizeof(iap_code) + 256, LOG_INFO, "%02X", 16);
	
end:
	if (buffer != NULL)
	{
		free(buffer);
		buffer = NULL;
	}
	
	return ret;
}

struct at91sam3swj_iap_command_t
{
	uint32_t flash_plane;
	uint32_t iap_command;
	uint32_t result_num;
	uint32_t address_of_frr;
	uint32_t data_num;
	uint32_t target_addr;
	uint32_t address_of_data;
};
struct at91sam3swj_iap_reply_t
{
	uint32_t *data;
	uint32_t data_num;
};

static RESULT at91sam3swj_iap_run(struct at91sam3swj_iap_command_t *cmd)
{
	uint32_t buff_tmp[9];
	
	memset(buff_tmp, 0, sizeof(buff_tmp));
	buff_tmp[0] = cmd->flash_plane;
	buff_tmp[1] = cmd->iap_command | AT91SAM3_EEFC_FKEY;
	buff_tmp[2] = cmd->result_num;
	buff_tmp[3] = cmd->address_of_frr;
	buff_tmp[4] = cmd->data_num;
	buff_tmp[5] = cmd->target_addr;
	buff_tmp[6] = cmd->address_of_data;
	buff_tmp[7] = 0;				// reserved0
	buff_tmp[8] = 0;				// sync
	
	// write iap command with sync to target SRAM
	// sync is 4-byte AFTER command in sram
	if (ERROR_OK != adi_memap_write_buf(AT91SAM3_IAP_COMMAND_ADDR, 
										(uint8_t*)buff_tmp, sizeof(buff_tmp)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "load iap cmd to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
//	at91sam3swj_debug_info();
	
	return ERROR_OK;
}

static RESULT at91sam3swj_iap_poll_result(struct at91sam3swj_iap_reply_t *reply, 
											uint8_t *fail)
{
	uint32_t *buff_tmp = NULL;
	uint32_t data_size;
	
	data_size = (1 + reply->data_num) * sizeof(uint32_t);
	buff_tmp = (uint32_t *)malloc(data_size);
	if (NULL == buff_tmp)
	{
		return ERROR_FAIL;
	}
	
	*fail = 0;
	
	// read result and sync
	// sync is 4-byte BEFORE result
	if (ERROR_OK != adi_memap_read_buf(AT91SAM3_IAP_SYNC_ADDR, 
										(uint8_t *)buff_tmp, data_size))
	{
		*fail = 1;
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read iap sync");
		return ERRCODE_FAILURE_OPERATION;
	}
	// buff_tmp[0] is sync, which is eefc_frr
	if (buff_tmp[0] != 0)
	{
		if (buff_tmp[0] != 1)
		{
			*fail = 1;
			at91sam3swj_debug_info();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ERRCODE), 
					  "call iap", buff_tmp[1]);
			return ERRCODE_FAILURE_OPERATION;
		}
		
		memcpy(reply->data, &buff_tmp[1], reply->data_num * sizeof(uint32_t));
		return ERROR_OK;
	}
	
	return ERROR_FAIL;
}

static RESULT at91sam3swj_iap_wait_ready(struct at91sam3swj_iap_reply_t *reply)
{
	uint8_t fail = 0;
	uint32_t start, end;
	
	start = get_time_in_ms();
	while (1)
	{
		if (ERROR_OK != at91sam3swj_iap_poll_result(reply, &fail))
		{
			if (fail)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "pool iap result");
				return ERROR_FAIL;
			}
			else
			{
				end = get_time_in_ms();
				// wait 1s at most
				if ((end - start) > 1000)
				{
					at91sam3swj_debug_info();
					LOG_ERROR(_GETTEXT("Time out when wait for iap ready\n"));
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

static RESULT at91sam3swj_iap_call(struct at91sam3swj_iap_command_t *cmd, 
							struct at91sam3swj_iap_reply_t *reply)
{	
	if ((ERROR_OK != at91sam3swj_iap_run(cmd)) 
		|| (ERROR_OK != at91sam3swj_iap_wait_ready(reply)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "run iap command");
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT at91sam3swj_enter_program_mode(struct program_context_t *context)
{
	uint32_t *para_ptr = (uint32_t*)&iap_code[AT91SAM3_IAP_PARAM_OFFSET];
	uint32_t reg, chip_id;
	struct at91sam3swj_iap_command_t command;
	struct at91sam3swj_iap_reply_t reply;
	uint32_t flash_descriptor[128];
	
	REFERENCE_PARAMETER(context);
	
	if (ERROR_OK != cm3_dp_halt())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "halt	at91sam3");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// read address of iap_entry
	chip_id = 0;
	if (ERROR_OK != adi_memap_read_reg(AT91SAM3_CHIPID_CIDR, &chip_id, 1))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "address of chip_id");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// get iap_entry_ptr
	if (ERROR_OK != at91sam3_get_iap_entry_ptr(chip_id, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "get iap_entry_ptr");
		return ERRCODE_FAILURE_OPERATION;
	}
	// get iap_entry
	if (ERROR_OK != adi_memap_read_reg(reg, &reg, 1))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "get iap_entry");
		return ERRCODE_FAILURE_OPERATION;
	}
	para_ptr[0] = reg;
	para_ptr[1] = AT91SAM3_IAP_SYNC_ADDR;
	
	// write sp
	reg = AT91SAM3_SRAM_ADDR + 1024;
	if (ERROR_OK != cm3_write_core_register(CM3_COREREG_SP, &reg))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write SP");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write iap_code to target SRAM
	if (ERROR_OK != adi_memap_write_buf(AT91SAM3_IAP_BASE, (uint8_t*)iap_code, 
											sizeof(iap_code)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "load iap_code to SRAM");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// write pc
	reg = AT91SAM3_IAP_BASE + 1;
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
	
	command.flash_plane = 0;
	command.iap_command = AT91SAM3_EEFC_CMD_GETD;
	command.result_num = dimof(flash_descriptor);
	command.address_of_frr = 0x400E080C;
	command.data_num = 0;
	command.target_addr = 0;
	command.address_of_data = AT91SAM3_IAP_DATA_ADDR;
	reply.data = flash_descriptor;
	reply.data_num = dimof(flash_descriptor);
	if (ERROR_OK != at91sam3swj_iap_call(&command, &reply))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read flash descriptor");
		return ERRCODE_FAILURE_OPERATION;
	}
	at91sam3_print_memory_info(flash_descriptor);
	
	{
		uint8_t buff[256];
		memset(buff, 0, sizeof(buff));
		if (ERROR_OK != adi_memap_write_buf(AT91SAM3_IAP_DATA_ADDR, buff, sizeof(buff)))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "load flash to SRAM");
			return ERRCODE_FAILURE_OPERATION;
		}
//		at91sam3swj_debug_info();
#if 1
		command.flash_plane = 0;
		command.iap_command = AT91SAM3_EEFC_CMD_EWP;
		command.result_num = 0;
		command.address_of_frr = 0x400E080C;
		command.data_num = 256;
		command.target_addr = 0x00080000;
		command.address_of_data = AT91SAM3_IAP_DATA_ADDR;
		reply.data = NULL;
		reply.data_num = 0;
		if (ERROR_OK != at91sam3swj_iap_call(&command, &reply))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read flash descriptor");
			return ERRCODE_FAILURE_OPERATION;
		}
#endif
	}
	
	return ERROR_OK;
}

RESULT at91sam3swj_leave_program_mode(struct program_context_t *context, 
									uint8_t success)
{
	uint32_t reg;
	
	if (cm3_execute_flag && success 
		&& (context->op->write_operations & APPLICATION))
	{
		if (ERROR_OK != cm3_dp_halt())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "halt at91sam3");
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

RESULT at91sam3swj_erase_target(struct program_context_t *context, char area, 
								uint32_t addr, uint32_t size)
{
	struct operation_t *op = context->op;
	RESULT ret = ERROR_OK;
	struct at91sam3swj_iap_command_t command;
	struct at91sam3swj_iap_reply_t reply;
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (0 == (op->write_operations & APPLICATION))
		{
			command.flash_plane = 0;
			command.iap_command = AT91SAM3_EEFC_CMD_EA;
			command.result_num = 0;
			command.address_of_frr = 0x400E080C;
			command.data_num = 0;
			command.target_addr = 0;
			command.address_of_data = AT91SAM3_IAP_DATA_ADDR;
			reply.data = NULL;
			reply.data_num = 0;
			if (ERROR_OK != at91sam3swj_iap_call(&command, &reply))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

RESULT at91sam3swj_write_target(struct program_context_t *context, char area, 
								uint32_t addr, uint8_t *buff, uint32_t size)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		break;
	default:
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT at91sam3swj_read_target(struct program_context_t *context, char area, 
								uint32_t addr, uint8_t *buff, uint32_t size)
{
	RESULT ret = ERROR_OK;
	uint32_t cur_block_size;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		if (ERROR_OK != adi_memap_read_reg(AT91SAM3_CHIPID_CIDR, (uint32_t*)buff, 1))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "address of iap_entry");
			return ERRCODE_FAILURE_OPERATION;
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

