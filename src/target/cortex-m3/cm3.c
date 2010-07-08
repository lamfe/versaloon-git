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
#include "cm3_lpc1000.h"
#include "cm3_at91sam3.h"

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

const struct cm3_param_t cm3_chips_param[] = {
//	chip_name,		jtag_khz,				pos			swd_trn,swd_delay,	program_functions
	{"cm3_stm32",	STM32_IRC_KHZ / 6,		{0,1,0,5},	2,		0,			&stm32swj_program_functions},
	{"cm3_lpc1000",	LPC1000_IRC_KHZ / 6,	{0,0,0,0},	2,		1,			&lpc1000swj_program_functions},
	{"cm3_at91sam3",AT91SAM3_IRC_KHZ / 6,	{0,0,0,0},	2,		0,			&at91sam3swj_program_functions}
};
static uint8_t cm3_chip_index = 0;

uint8_t cm3_mode_offset = 0;
uint8_t cm3_execute_flag = 0;
uint32_t cm3_execute_addr = 0;

PARSE_ARGUMENT_HANDLER(cm3)
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
					ENTER_PROGRAM_MODE_FUNCNAME(cm3);
				cm3_program_functions.leave_program_mode = 
					LEAVE_PROGRAM_MODE_FUNCNAME(cm3);
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

ENTER_PROGRAM_MODE_HANDLER(cm3)
{
	adi_dp_if_t dp;
	const struct program_functions_t *pf = 
		cm3_chips_param[cm3_chip_index].program_functions;
	
	if (cm3_chip_index >= dimof(cm3_chips_param))
	{
		return ERROR_FAIL;
	}
	
	// jtag/swd init
	context->pi->mode -= cm3_mode_offset;
	if ((context->pi->mode != ADI_DP_JTAG) 
		&& (context->pi->mode != ADI_DP_SWD))
	{
		LOG_WARNING(_GETTEXT("debug port not defined, use JTAG by default.\n"));
		context->pi->mode = ADI_DP_JTAG;
	}
	dp.type = context->pi->mode;
	dp.core = ADI_DP_INVALID;
	
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
		dp.adi_dp_if_info.adi_dp_swd.swd_trn = 
				cm3_chips_param[cm3_chip_index].swd_trn;
		if (context->pi->wait_state)
		{
			dp.adi_dp_if_info.adi_dp_swd.swd_dly = context->pi->wait_state;
		}
		else
		{
			dp.adi_dp_if_info.adi_dp_swd.swd_dly = 
				cm3_chips_param[cm3_chip_index].swd_delay;
		}
		dp.adi_dp_if_info.adi_dp_swd.swd_retry = 0;
		
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

LEAVE_PROGRAM_MODE_HANDLER(cm3)
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
	
	// jtag/swd fini
	cm3_reset();
	cm3_dp_fini();
	if (!(context->op->read_operations & CHIPID))
	{
		cm3_execute_flag = 0;
	}
	return ret;
}

