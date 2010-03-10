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
#include <ctype.h>

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

#define CUR_TARGET_STRING			CM3_STRING

RESULT cm3_enter_program_mode(struct program_context_t *context);
RESULT cm3_leave_program_mode(struct program_context_t *context, 
									uint8_t success);
struct program_functions_t cm3_program_functions = 
{
	NULL,			// execute
	cm3_enter_program_mode, 
	cm3_leave_program_mode, 
	NULL, 
	NULL, 
	NULL
};

const struct cm3_param_t cm3_chips_param[] = {
//	chip_name,		jtag_khz,		pos			swj_trn,	program_functions
	{"cm3_stm32",	STM32_JTAG_KHZ,	{0,1,0,5},	2,			&stm32swj_program_functions},
};
static uint8_t cm3_chip_index = 0;

uint8_t cm3_mode_offset = 0;
uint8_t cm3_execute_flag = 0;
uint32_t cm3_execute_addr = 0;

RESULT cm3_parse_argument(char cmd, const char *argu)
{
	uint8_t i;
	
	switch (cmd)
	{
	case 'c':
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		for (i = 0; i < dimof(cm3_chips_param); i++)
		{
			if (!strcmp(cm3_chips_param[i].chip_name, argu))
			{
				cm3_chip_index = i;
				memcpy(&cm3_program_functions, 
						cm3_chips_param[i].program_functions, 
						sizeof(cm3_program_functions));
				cm3_program_functions.enter_program_mode = 
					cm3_enter_program_mode;
				cm3_program_functions.leave_program_mode = 
					cm3_leave_program_mode;
				break;
			}
		}
		break;
	case 'x':
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		cm3_execute_addr = (uint32_t)strtoul(argu, NULL, 0);
		cm3_execute_flag = 1;
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT cm3_enter_program_mode(struct program_context_t *context)
{
	adi_dp_if_t dp;
	const struct program_functions_t *pf = 
		cm3_chips_param[cm3_chip_index].program_functions;
	
	if (cm3_chip_index >= dimof(cm3_chips_param))
	{
		return ERROR_FAIL;
	}
	
	// jtag/swj init
	context->pi->mode -= cm3_mode_offset;
	if ((context->pi->mode != ADI_DP_JTAG) 
		&& (context->pi->mode != ADI_DP_SWD))
	{
		LOG_WARNING(_GETTEXT("debug port not defined, use JTAG by default.\n"));
		context->pi->mode = ADI_DP_JTAG;
	}
	dp.type = context->pi->mode;
	
	switch(context->pi->mode)
	{
	case ADI_DP_JTAG:
		if (context->pi->frequency)
		{
			dp.adi_dp_if_info.adi_dp_jtag.jtag_khz = context->pi->frequency;
		}
		else
		{
			dp.adi_dp_if_info.adi_dp_jtag.jtag_khz = 
				cm3_chips_param[cm3_chip_index].jtag_khz;
		}
		dp.adi_dp_if_info.adi_dp_jtag.ub = 
				cm3_chips_param[cm3_chip_index].pos.ub;
		dp.adi_dp_if_info.adi_dp_jtag.ua = 
				cm3_chips_param[cm3_chip_index].pos.ua;
		dp.adi_dp_if_info.adi_dp_jtag.bb = 
				cm3_chips_param[cm3_chip_index].pos.bb;
		dp.adi_dp_if_info.adi_dp_jtag.ba = 
				cm3_chips_param[cm3_chip_index].pos.ba;
		
		break;
	case ADI_DP_SWD:
		dp.adi_dp_if_info.adi_dp_swj.swj_trn = 
				cm3_chips_param[cm3_chip_index].swj_trn;
		dp.adi_dp_if_info.adi_dp_swj.swj_dly = 0;
		dp.adi_dp_if_info.adi_dp_swj.swj_retry = 0;
		
		break;
	}
	
	// mode independent
	if (ERROR_OK != cm3_dp_init(context, &dp))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "initialize cm3");
		LOG_ERROR(_GETTEXT("Maybe your last firmware disable the JTAG/SWD port"
							", try using OpenOCD to erase the firmware.\n"));
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if ((pf->enter_program_mode != NULL) 
		&& (ERROR_OK != pf->enter_program_mode(context)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

RESULT cm3_leave_program_mode(struct program_context_t *context, 
									uint8_t success)
{
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
	
	// jtag/swj fini
	cm3_reset();
	cm3_dp_fini();
	if (!(context->op->read_operations & CHIPID))
	{
		cm3_execute_flag = 0;
	}
	return ret;
}

