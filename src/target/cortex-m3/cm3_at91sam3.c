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
#define AT91SAM3_IAP_SYNC_ADDR			(AT91SAM3_IAP_BASE + 88)
static uint8_t iap_code[] = {
							// wait_start:
	0x15, 0x48,				// ldr		r0, [PC, #XX]		// load sync
	0x00, 0x28,				// cmp		r0, #0
	0xFC, 0xD0,				// bne 		wait_start(*)
	0x00, 0x00,
							// init:
	0x05, 0x4A,				// ldr		r2, [PC, #XX]		// laod address of iap_entry
	0x06, 0x48,				// ldr		r0, [PC, #XX]		// load flash plan index
	0x06, 0x49,				// ldr		r1, [PC, #XX]		// load command
	
	0x90, 0x47,				// blx		r2
	0x00, 0x00,
	0x06, 0x48,				// ldr		r1, [PC, #XX](*)		// load address of sync
	0x01, 0x60,				// str		r0, [r1, #0](*)
	0xF1, 0xE7,				// b		wait_start
	0xFE, 0xE7,				// b		$
	0, 0,					// fill
							// parameter
	0, 0, 0, 0,				// address of iap_entry
	0, 0, 0, 0,				// address of sync
	
							// iap_command parameter:
	0, 0, 0, 0,				// flash plan index
	0, 0, 0, 0,				// command
	0, 0, 0, 0,				// number of result
	
	0, 0, 0, 0,				// sync
	
							// iap_reply
	0, 0, 0, 0,				// result status
	0, 0, 0, 0,				// data from here
};

static RESULT at91sam3swj_debug_info(void)
{
	uint32_t reg;
	uint8_t i;
	uint8_t *buffer;
	RESULT ret = ERROR_OK;
	
	buffer = (uint8_t *)malloc(sizeof(iap_code));
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
													sizeof(buffer)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read sram");
		ret = ERRCODE_FAILURE_OPERATION;
		goto end;
	}
	LOG_BYTE_BUF(buffer, sizeof(buffer), LOG_INFO, "%02X", 16);
	
end:
	if (buffer != NULL)
	{
		free(buffer);
		buffer = NULL;
	}
	
	return ret;
}

RESULT at91sam3swj_enter_program_mode(struct program_context_t *context)
{
	REFERENCE_PARAMETER(context);
	
	at91sam3swj_debug_info();
	
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
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case APPLICATION_CHAR:
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

