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

const struct cm3_param_t cm3_chips_param[] = {
//	chip_name,		default_char,		flash_start_addr,	flash_max_size,	jtag_khz,		pos			swj_trn
	{"cm3_stm32",	STM32_FLASH_CHAR,	0x08000000,			512 * 1024,		STM32_JTAG_KHZ,	{0,1,0,5},	2},
};
static int8_t cm3_chip_index = 0;
struct cm3_param_t *cm3_chip_param = NULL;

uint8_t cm3_execute_flag = 0;
uint32_t cm3_execute_addr = 0;
uint16_t cm3_buffer_size = 0;

RESULT cm3_parse_argument(char cmd, const char *argu)
{
	switch (cmd)
	{
	case 'b':
		// set buffer size
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		cm3_buffer_size = (uint16_t)strtoul(argu, NULL, 0);
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






#define get_target_voltage(v)					prog->get_target_voltage(v)
RESULT cm3_program(struct operation_t operations, struct program_info_t *pi, 
					  struct programmer_info_t *prog)
{
	RESULT ret = ERROR_OK;
	adi_dp_if_t dp;
	uint16_t voltage;
	uint8_t i;
	
	pi = pi;
	
	cm3_chip_index = -1;
	cm3_chip_param = NULL;
	for (i = 0; i < dimof(cm3_chips_param); i++)
	{
		if (!strcmp(cm3_chips_param[i].chip_name, pi->chip_name))
		{
			cm3_chip_index = i;
			cm3_chip_param = (struct cm3_param_t *)&cm3_chips_param[i];
			break;
		}
	}
	if ((cm3_chip_index < 0) || (NULL == cm3_chip_param))
	{
		return ERROR_FAIL;
	}
	
	// get target voltage
	if (ERROR_OK != get_target_voltage(&voltage))
	{
		return ERROR_FAIL;
	}
	LOG_DEBUG(_GETTEXT(INFOMSG_TARGET_VOLTAGE), voltage / 1000.0);
	if (voltage < 2700)
	{
		LOG_WARNING(_GETTEXT(INFOMSG_TARGET_LOW_POWER));
	}
	
	if ((program_mode != ADI_DP_JTAG) && (program_mode != ADI_DP_SWJ))
	{
		LOG_WARNING(_GETTEXT("debug port not defined, use JTAG by default.\n"));
		program_mode = ADI_DP_JTAG;
	}
	
	dp.type = program_mode;
	switch(program_mode)
	{
	case ADI_DP_JTAG:
		if (program_frequency)
		{
			dp.adi_dp_if_info.adi_dp_jtag.jtag_khz = program_frequency;
		}
		else
		{
			dp.adi_dp_if_info.adi_dp_jtag.jtag_khz = cm3_chip_param->jtag_khz;
		}
		dp.adi_dp_if_info.adi_dp_jtag.ub = cm3_chip_param->pos.ub;
		dp.adi_dp_if_info.adi_dp_jtag.ua = cm3_chip_param->pos.ua;
		dp.adi_dp_if_info.adi_dp_jtag.bb = cm3_chip_param->pos.bb;
		dp.adi_dp_if_info.adi_dp_jtag.ba = cm3_chip_param->pos.ba;
		
		break;
	case ADI_DP_SWJ:
		dp.adi_dp_if_info.adi_dp_swj.swj_trn = cm3_chip_param->swj_trn;
		dp.adi_dp_if_info.adi_dp_swj.swj_dly = 0;
		dp.adi_dp_if_info.adi_dp_swj.swj_retry = 0;
		
		break;
	}
	
	if (ERROR_OK != cm3_dp_init(prog, &dp))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "initialize cm3");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	
	switch (cm3_chip_index)
	{
	case CM3_STM32:
		ret = stm32jtagswj_program(operations, pi, &adi_dp_info);
		break;
	default:
		// invalid target
		LOG_BUG(_GETTEXT(ERRMSG_INVALID), TO_STR(cm3_chip_index), 
				CUR_TARGET_STRING);
		return ERRCODE_INVALID;
	}
	
	cm3_reset();
leave_program_mode:
	cm3_dp_fini();
	cm3_execute_flag = 0;
	return ret;
}

