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

#include "msp430.h"
#include "JTAGfunc.h"
#include "msp430_internal.h"

RESULT msp430jtagsbw_enter_program_mode(struct program_context_t *context);
RESULT msp430jtagsbw_leave_program_mode(struct program_context_t *context, 
								uint8_t success);
RESULT msp430jtagsbw_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size);
RESULT msp430jtagsbw_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size);
RESULT msp430jtagsbw_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size);
const struct program_functions_t msp430jtagsbw_program_functions = 
{
	NULL,			// execute
	msp430jtagsbw_enter_program_mode, 
	msp430jtagsbw_leave_program_mode, 
	msp430jtagsbw_erase_target, 
	msp430jtagsbw_write_target, 
	msp430jtagsbw_read_target
};

RESULT msp430jtagsbw_enter_program_mode(struct program_context_t *context)
{
	struct programmer_info_t *prog = context->prog;
	uint8_t tmp8, i;
	uint8_t ir;
	uint32_t dr;
	
	msp430_jtag_init();
	if (DeviceHas_TestPin())
	{
		// TEST pin
		msp430_jtag_config(1);
	}
	else
	{
		// no TEST pin
		msp430_jtag_config(0);
	}
	
	for (i = 10; i > 0; i--)
	{
		ResetTAP();
		// read ir return value, should be 0x89(MSP430_JTAG_ID)
		IR_Shift_Read(IR_BYPASS, &tmp8);
		if (ERROR_OK != commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "init chip");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (MSP430_JTAG_ID == tmp8)
		{
			break;
		}
		else
		{
			// re-init
			msp430_jtag_fini();
			msp430_jtag_init();
			if (DeviceHas_TestPin())
			{
				// TEST pin
				msp430_jtag_config(1);
			}
			else
			{
				// no TEST pin
				msp430_jtag_config(0);
			}
		}
	}
	if (0 == i)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "detect target chip");
		return ERROR_FAIL;
	}
	
	ResetTAP();
	// check fuse blown
	for (i = 3; i > 0; i--)
	{
		IR_Shift(IR_CNTRL_SIG_CAPTURE);
		DR_Shift16_Read(0xAAAA, &dr);
		if (ERROR_OK != commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "init programming");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (0x5555 == dr)
		{
			LOG_ERROR(_GETTEXT("fuse of current chip is blown\n"));
			return ERROR_FAIL;
		}
	}
	return ERROR_OK;
}

RESULT msp430jtagsbw_leave_program_mode(struct program_context_t *context, 
								uint8_t success)
{
	struct programmer_info_t *prog = context->prog;
	
	REFERENCE_PARAMETER(success);
	
	ReleaseDevice(V_RESET);
	msp430_jtag_fini();
	if (ERROR_OK != commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "exit program mode");
		return ERRCODE_FAILURE_OPERATION;
	}
	return ERROR_OK;
}

RESULT msp430jtagsbw_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size)
{
	struct programmer_info_t *prog = context->prog;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(page_size);
	REFERENCE_PARAMETER(addr);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if (DeviceHas_CpuX())
		{
			// Global-Erase Flash
			// (for all devices with CPU-X)
			EraseFLASH(ERASE_GLOB, 0xFE00);
		}
		else
		{
			// Mass-Erase Flash (all types)
			EraseFLASH(ERASE_MASS, 0xFE00);
			// NOTE: the INFO memory in F2xx device will be not erased,
			// if the memory still locked.
			// For more info See EraseFLASH() in JTAGfunc.c
		}
		
		if (ERROR_OK != commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
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

RESULT msp430jtagsbw_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	struct programmer_info_t *prog = context->prog;
	RESULT ret = ERROR_OK;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		WriteFLASH((word)addr, (word)(page_size / 2), (word*)buff);
		if (ERROR_OK != commit())
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
						"write flash", addr);
			ret = ERRCODE_FAILURE_OPERATION_ADDR;
			break;
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

RESULT msp430jtagsbw_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	struct program_info_t *pi = context->pi;
	struct programmer_info_t *prog = context->prog;
	struct operation_t *op = context->op;
	
	uint16_t chip_id;
	uint8_t ir;
	uint32_t dr;
	RESULT ret = ERROR_OK;
	
	switch (area)
	{
	case CHIPID_CHAR:
		IR_Shift(IR_CNTRL_SIG_16BIT);
		DR_Shift16(0x2401);
		IR_Shift(IR_CNTRL_SIG_CAPTURE);
		// wait until CPU is synchronized
		msp430_jtag_dr16_poll(0x0000, 0x0200, 0x0200, 50, 0);
		// read chip_id in 0x0FF0
		ReadMem(F_WORD, 0x0FF0, &chip_id);
		// perform PUC, includes target watchdog disable
		ExecutePOR();
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		pi->chip_id = ((chip_id << 8) + (chip_id >> 8)) & 0x0000FFFF;
		break;
	case APPLICATION_CHAR:
		if (op->verify_operations & APPLICATION)
		{
			word CRC_check, CRC_calc;
			
			CRC_calc = CRC_check = 0;
			CRC_calc = VerifyMem((word)addr, (word)(page_size / 2), 
									(word*)buff, &CRC_check);
			if (ERROR_OK != commit())
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							"read crc check");
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			if (CRC_calc != CRC_check)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
							"verify flash", addr);
				ret = ERRCODE_FAILURE_OPERATION_ADDR;
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

