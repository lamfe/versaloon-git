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
#define cur_chip_param				cm3_chip_param
#define cur_chips_param				cm3_chips_param
#define cur_flash_offset			cm3_flash_offset
#define cur_prog_mode				cm3_prog_mode
#define cur_frequency				cm3_frequency
#define cur_buffer_size				cm3_buffer_size

#define CM3_MAX_BUFSIZE				(1024 * 1024)

const program_area_map_t cm3_program_area_map[] = 
{
	{APPLICATION, 'f', 1},
//	{LOCK, 'l', 0},
	{0, 0, 0}
};

#define CM3_STM32		0
#define CM3_LPC1700		1
#define CM3_LM3S		2
#define CM3_SAM3		3
const cm3_param_t cm3_chips_param[] = {
//	chip_name,		default_char,		flash_start_addr,	jtag_khz,		pos			swj_trn
	{"cm3_stm32",	STM32_FLASH_CHAR,	0x08000000,			STM32_JTAG_KHZ,	{0,1,0,5},	2},
};
static uint8 cm3_chip_index = 0;
cm3_param_t cm3_chip_param;

adi_dp_if_type_t cm3_prog_mode = 0;
uint8 cm3_execute_flag = 0;
uint32 cm3_execute_addr = 0;
uint16 cm3_frequency = 0;
uint16 cm3_buffer_size = 0;

static uint32 cm3_flash_offset = 0;

static void cm3_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                set mode<j|s>\n\
  -F,  --frequency <FREQUENCY>      set JTAG/SWJ frequency, in KHz\n\
  -b,  --buffsize <BUFFSIZE>        set JTAG buffer size\n", 
		   CUR_TARGET_STRING);
}

static void cm3_support(void)
{
	uint32 i;

	printf("Support list of %s:\n", CUR_TARGET_STRING);
	for (i = 0; i < dimof(cur_chips_param); i++)
	{
		printf("\
%s: \n",
				cur_chips_param[i].chip_name);
	}
	printf("\n");
}

RESULT cm3_parse_argument(char cmd, const char *argu)
{
	switch (cmd)
	{
	case 'h':
		cm3_usage();
		break;
	case 'S':
		cm3_support();
		break;
	case 'b':
		// set buffer size
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		cur_buffer_size = (uint16)strtoul(argu, NULL, 0);
		break;
	case 'm':
		// program Mode
		if ((NULL == argu) || (strlen(argu) != 1))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		switch (argu[0])
		{
		case 'j':
			// JTAG mode
			cur_prog_mode = ADI_DP_JTAG;
			break;
		case 's':
			// SWJ mode
			cur_prog_mode = ADI_DP_SWJ;
			break;
		default:
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHARACTER_MESSAGE), cmd, 
					  "c8051f program mode", "MUST be 'c' or 'j'!!");
			return ERRCODE_INVALID;
			break;
		}
		
		break;
	case 'F':
		// set Frequency
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		cur_frequency = (uint16)strtoul(argu, NULL, 0);
		
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT cm3_probe_chip(char *chip_name)
{
	uint32 i;
	
	for (i = 0; i < dimof(cur_chips_param); i++)
	{
		if (!strcmp(cur_chips_param[i].chip_name, chip_name))
		{
			return ERROR_OK;
		}
	}
	
	return ERROR_FAIL;
}

RESULT cm3_prepare_buffer(program_info_t *pi)
{
	if (pi->app != NULL)
	{
		memset(pi->app, cur_chip_param.default_char, pi->app_size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT cm3_write_buffer_from_file_callback(uint32 address, uint32 seg_addr, 
										   uint8* data, uint32 length, 
										   void* buffer)
{
	program_info_t *pi = (program_info_t *)buffer;
	uint32 mem_addr = address, page_size;
	RESULT ret;
	
	seg_addr = seg_addr;
	
#ifdef PARAM_CHECK
	if ((length > 0) && (NULL == data))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if (NULL == pi->app)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), 
				  "pi->app");
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	mem_addr += cur_flash_offset;
	if (((mem_addr - cur_chip_param.flash_start_addr) >= CM3_MAX_BUFSIZE) 
		|| (length > CM3_MAX_BUFSIZE) 
		|| ((mem_addr - cur_chip_param.flash_start_addr + length) 
			> CM3_MAX_BUFSIZE))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "flash memory");
		return ERRCODE_INVALID;
	}
	memcpy(pi->app + mem_addr - cur_chip_param.flash_start_addr, 
		   data, length);
	pi->app_size_valid += (uint32)length;
	
	switch (cm3_chip_index)
	{
	case CM3_STM32:
		page_size = STM32_PAGE_SIZE;
		break;
	default:
		// invalid target
		LOG_BUG(_GETTEXT(ERRMSG_INVALID), TO_STR(cm3_chip_index), 
				CUR_TARGET_STRING);
		return ERRCODE_INVALID;
	}
	
	ret = MEMLIST_Add(&pi->app_memlist, mem_addr, length, page_size);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

RESULT cm3_fini(program_info_t *pi, programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	return ERROR_OK;
}

RESULT cm3_init(program_info_t *pi, const char *dir, 
				   programmer_info_t *prog)
{
	uint8 i;
	
	dir = dir;
	prog = prog;
	
	if (strcmp(pi->chip_type, CUR_TARGET_STRING))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_HANDLER), CUR_TARGET_STRING, 
				pi->chip_type);
		return ERRCODE_INVALID_HANDLER;
	}
	
	if (NULL == pi->chip_name)
	{
		// auto detect
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "Auto-detect", 
				  CUR_TARGET_STRING);
		return ERRCODE_NOT_SUPPORT;
	}
	else
	{
		for (i = 0; i < dimof(cur_chips_param); i++)
		{
			if (!strcmp(cur_chips_param[i].chip_name, pi->chip_name))
			{
				cm3_chip_index = i;
				memcpy(&cur_chip_param, cur_chips_param + cm3_chip_index, 
					   sizeof(cur_chip_param));
				
				pi->app_size = CM3_MAX_BUFSIZE;
				pi->app_size_valid = 0;
				
				return ERROR_OK;
			}
		}
		
		return ERROR_FAIL;
	}
}

uint32 cm3_interface_needed(void)
{
	return JTAG_HL | SWJ;
}










RESULT cm3_program(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog)
{
	RESULT ret = ERROR_OK;
	adi_dp_if_t dp;
	
	pi = pi;
	
	if ((cur_prog_mode != ADI_DP_JTAG) && (cur_prog_mode != ADI_DP_SWJ))
	{
		LOG_WARNING(_GETTEXT("debug port not defined, use JTAG by default.\n"));
		cur_prog_mode = ADI_DP_JTAG;
	}
	
	dp.type = cur_prog_mode;
	switch(cur_prog_mode)
	{
	case ADI_DP_JTAG:
		if (cm3_frequency)
		{
			dp.adi_dp_if_info.adi_dp_jtag.jtag_khz = cm3_frequency;
		}
		else
		{
			dp.adi_dp_if_info.adi_dp_jtag.jtag_khz = cur_chip_param.jtag_khz;
		}
		dp.adi_dp_if_info.adi_dp_jtag.ub = cur_chip_param.pos.ub;
		dp.adi_dp_if_info.adi_dp_jtag.ua = cur_chip_param.pos.ua;
		dp.adi_dp_if_info.adi_dp_jtag.bb = cur_chip_param.pos.bb;
		dp.adi_dp_if_info.adi_dp_jtag.ba = cur_chip_param.pos.ba;
		
		break;
	case ADI_DP_SWJ:
		dp.adi_dp_if_info.adi_dp_swj.swj_trn = cur_chip_param.swj_trn;
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
		ret = stm32_program(operations, pi, &adi_dp_info);
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
	return ret;
}

