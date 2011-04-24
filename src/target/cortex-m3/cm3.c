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
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"

#include "cm3.h"
#include "cm3_stm32.h"
#include "cm3_lpc1000.h"
#include "cm3_at91sam3.h"
#include "cm3_lm3s.h"
#include "cm3_stm32f2.h"

#include "cm3_internal.h"

#include "adi_v5p1.h"
#include "cm3_common.h"

#define CUR_TARGET_STRING			CM3_STRING

ENTER_PROGRAM_MODE_HANDLER(cm3);
LEAVE_PROGRAM_MODE_HANDLER(cm3);
struct program_functions_t cm3_program_functions = 
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(cm3), 
	LEAVE_PROGRAM_MODE_FUNCNAME(cm3), 
	NULL, 
	NULL, 
	NULL
};

const struct cm3_param_t cm3_chips_param[CM3_PARAM_TARGET_NUM] = {
	{
		"cm3_stm32",					// chip_name
		STM32_IRC_KHZ / 6,				// jtag_khz
		{0,1,0,5},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&stm32swj_program_functions		// program_functions
	},
	{
		"cm3_stm32f2",					// chip_name
		STM32F2_IRC_KHZ / 6,			// jtag_khz
		{0,1,0,5},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&stm32f2swj_program_functions	// program_functions
	},
	{
		"cm3_lpc1000",					// chip_name
		LPC1000_IRC_KHZ / 6,			// jtag_khz
		{0,0,0,0},						// jtag_pos
		2,								// swd_trn
		1,								// swd_delay
		&lpc1000swj_program_functions	// program_functions
	},
	{
		"cm3_at91sam3",					// chip_name
		AT91SAM3_IRC_KHZ / 6,			// jtag_khz
		{0,0,0,0},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&at91sam3swj_program_functions	// program_functions
	},
	{
		"cm3_lm3s",						// chip_name
		LM3S_IRC_KHZ / 6,				// jtag_khz
		{0,0,0,0},						// jtag_pos
		2,								// swd_trn
		0,								// swd_delay
		&lm3sswj_program_functions		// program_functions
	}
};
static uint8_t cm3_chip_index = 0;

uint8_t cm3_mode_offset = 0;

VSS_HANDLER(cm3_chip)
{
	uint8_t i;
	VSS_CHECK_ARGC(2);
	for (i = 0; i < dimof(cm3_chips_param); i++)
	{
		if (!strcmp(cm3_chips_param[i].chip_name, argv[1]))
		{
			cm3_chip_index = i;
			memcpy(&cm3_program_functions, 
					cm3_chips_param[i].program_functions, 
					sizeof(cm3_program_functions));
			cm3_program_functions.enter_program_mode = 
				ENTER_PROGRAM_MODE_FUNCNAME(cm3);
			cm3_program_functions.leave_program_mode = 
				LEAVE_PROGRAM_MODE_FUNCNAME(cm3);
			return ERROR_OK;
		}
	}
	return ERROR_FAIL;
}

const struct vss_cmd_t cm3_notifier[] = 
{
	VSS_CMD(	"chip",
				"select target chip for internal call",
				cm3_chip),
	VSS_CMD_END
};

ENTER_PROGRAM_MODE_HANDLER(cm3)
{
	struct adi_dpif_t dp;
	struct program_info_t *pi = context->pi;
	const struct program_functions_t *pf = 
		cm3_chips_param[cm3_chip_index].program_functions;
	
	if (cm3_chip_index >= dimof(cm3_chips_param))
	{
		return ERROR_FAIL;
	}
	
	// jtag/swd init
	if (((context->pi->mode - cm3_mode_offset) != ADI_DP_JTAG) 
		&& ((context->pi->mode - cm3_mode_offset) != ADI_DP_SWD))
	{
		LOG_WARNING("debug port not defined, use JTAG by default.");
		context->pi->mode = ADI_DP_JTAG + cm3_mode_offset;
	}
	dp.type = context->pi->mode - cm3_mode_offset;
	
	switch(dp.type)
	{
	case ADI_DP_JTAG:
		if (context->pi->frequency)
		{
			dp.dpif_setting.dpif_jtag_setting.jtag_khz = 
				context->pi->frequency;
		}
		else
		{
			dp.dpif_setting.dpif_jtag_setting.jtag_khz = 
				cm3_chips_param[cm3_chip_index].jtag_khz;
		}
		dp.dpif_setting.dpif_jtag_setting.ub = 
			cm3_chips_param[cm3_chip_index].jtag_pos.ub + pi->jtag_pos.ub;
		dp.dpif_setting.dpif_jtag_setting.ua = 
			cm3_chips_param[cm3_chip_index].jtag_pos.ua + pi->jtag_pos.ua;
		dp.dpif_setting.dpif_jtag_setting.bb = 
			cm3_chips_param[cm3_chip_index].jtag_pos.bb + pi->jtag_pos.bb;
		dp.dpif_setting.dpif_jtag_setting.ba = 
			cm3_chips_param[cm3_chip_index].jtag_pos.ba + pi->jtag_pos.ba;
		
		break;
	case ADI_DP_SWD:
		dp.dpif_setting.dpif_swd_setting.swd_trn = 
				cm3_chips_param[cm3_chip_index].swd_trn;
		if (context->pi->wait_state)
		{
			dp.dpif_setting.dpif_swd_setting.swd_dly = 
				context->pi->wait_state;
		}
		else
		{
			dp.dpif_setting.dpif_swd_setting.swd_dly = 
				cm3_chips_param[cm3_chip_index].swd_delay;
		}
		dp.dpif_setting.dpif_swd_setting.swd_retry = 0;
		
		break;
	}
	
	// mode independent
	if (ERROR_OK != cm3_dp_init(context->prog, &dp))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize cm3");
		LOG_ERROR("Maybe your last firmware disable the JTAG/SWD port"
							", try using OpenOCD to erase the firmware.");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if ((pf->enter_program_mode != NULL) 
		&& (ERROR_OK != pf->enter_program_mode(context)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

LEAVE_PROGRAM_MODE_HANDLER(cm3)
{
	struct program_info_t *pi = context->pi;
	const struct program_functions_t *pf = 
		cm3_chips_param[cm3_chip_index].program_functions;
	RESULT ret = ERROR_OK;
	
	if (cm3_chip_index >= dimof(cm3_chips_param))
	{
		return ERROR_FAIL;
	}
	
	if (pf->leave_program_mode != NULL)
	{
		ret = pf->leave_program_mode(context, success);
	}
	
	if (pi->execute_flag && success 
		&& (context->op->write_operations & APPLICATION))
	{
		uint32_t reg;
		
		if (ERROR_OK != cm3_dp_halt())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "halt cm3");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (ERROR_OK != 
				cm3_write_core_register(CM3_COREREG_PC, &pi->execute_addr))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "write PC");
			return ERRCODE_FAILURE_OPERATION;
		}
		reg = 0;
		if ((ERROR_OK != cm3_read_core_register(CM3_COREREG_PC, &reg)) 
			|| (reg != pi->execute_addr))
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "verify written PC");
			return ERRCODE_FAILURE_OPERATION;
		}
		if (ERROR_OK != cm3_dp_resume())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "run code");
			return ERRCODE_FAILURE_OPERATION;
		}
	}
	
	// jtag/swd fini
	cm3_reset();
	cm3_dp_fini();
	return ret;
}

