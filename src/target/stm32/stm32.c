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

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "memlist.h"
#include "filelist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "stm32.h"
#include "stm32_internal.h"
#include "comisp.h"
#include "comisp_internal.h"
#include "cm3.h"
#include "cm3_internal.h"
#include "adi_v5p1.h"

#define CUR_TARGET_STRING			STM32_STRING
#define cur_chip_param				target_chip_param
#define cur_chips_param				target_chips.chips_param
#define cur_prog_mode				program_mode
#define cur_target_defined			target_defined

const struct program_area_map_t stm32_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0x08000000, 0, 0},
	{0, 0, 0, 0, 0, 0}
};

static void stm32_usage(void)
{
	printf("\
Usage of %s:\n\
  -C,  --comport <COMM_ATTRIBUTE>           set com port\n\
  -m,  --mode <MODE>                        set mode<j|s|i>\n\
  -x,  --execute <ADDRESS>                  execute program\n\
  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz\n\n",
			CUR_TARGET_STRING);
}

RESULT stm32_parse_argument(char cmd, const char *argu)
{
	switch (cmd)
	{
	case 'h':
		stm32_usage();
		break;
	case 'E':
		comisp_print_comm_info(COMISP_STM32);
		break;
	case 'C':
	case 'x':
		return comisp_parse_argument(cmd, argu);
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT stm32_prepare_buffer(struct program_info_t *pi)
{
	if (pi->program_areas[APPLICATION_IDX].buff != NULL)
	{
		memset(pi->program_areas[APPLICATION_IDX].buff, STM32_FLASH_DEFAULT, 
				pi->program_areas[APPLICATION_IDX].size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT stm32_write_buffer_from_file_callback(uint32_t address, 
			uint32_t seg_addr, uint8_t* data, uint32_t length, void* buffer)
{
	struct program_info_t *pi = (struct program_info_t *)buffer;
	uint32_t mem_addr, page_size;
	RESULT ret;
	uint8_t *tbuff;
	struct chip_area_info_t *areas;
	
#ifdef PARAM_CHECK
	if ((length > 0) && (NULL == data))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (seg_addr != 0)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), 
				  "segment address", CUR_TARGET_STRING);
		return ERRCODE_NOT_SUPPORT;
	}
	if (address < STM32_FLASH_ADDR)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_HEX), 
				  address, CUR_TARGET_STRING " flash");
		return ERRCODE_INVALID;
	}
	
	mem_addr = address - STM32_FLASH_ADDR;
	areas = cur_chip_param.chip_areas;
	tbuff = pi->program_areas[APPLICATION_IDX].buff;
	if (NULL == tbuff)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "pi->app");
		return ERRCODE_INVALID_BUFFER;
	}
	
	if ((0 == areas[APPLICATION_IDX].page_num) 
		|| (0 == areas[APPLICATION_IDX].page_size))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID), "Flash", 
				  cur_chip_param.chip_name);
		return ERRCODE_INVALID;
	}
	
	if ((mem_addr >= areas[APPLICATION_IDX].size) 
		|| (length > areas[APPLICATION_IDX].size) 
		|| ((mem_addr + length) > areas[APPLICATION_IDX].size))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "flash memory");
		return ERRCODE_INVALID;
	}
	cur_target_defined |= APPLICATION;
	
	memcpy(tbuff + mem_addr, data, length);
	
	page_size = areas[APPLICATION_IDX].page_size;
	
	ret = MEMLIST_Add(&pi->program_areas[APPLICATION_IDX].memlist, 
							mem_addr + STM32_FLASH_ADDR, length, page_size);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

RESULT stm32_fini(struct program_info_t *pi, struct programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	return ERROR_OK;
}

RESULT stm32_init(struct program_info_t *pi, struct programmer_info_t *prog)
{
	prog = prog;
	
	memcpy(&cur_chip_param, cur_chips_param, sizeof(cur_chip_param));
	pi ->program_areas[APPLICATION_IDX].size = 
							cur_chip_param.chip_areas[APPLICATION_IDX].size;
	
	return ERROR_OK;
}

uint32_t stm32_interface_needed(void)
{
	switch (cur_prog_mode)
	{
	case STM32_JTAG:
	case STM32_SWJ:
		return cm3_interface_needed();
	case STM32_ISP:
		return comisp_interface_needed();
	default:
		return 0;
	}
}

RESULT stm32_program(struct operation_t operations, 
					struct program_info_t *pi, struct programmer_info_t *prog)
{
	RESULT ret = ERROR_OK;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	operations = operations;
	pi = pi;
	prog = prog;
	
	switch (cur_prog_mode)
	{
	case STM32_JTAG:
		cur_prog_mode = ADI_DP_JTAG;
		goto stm32jtagswj_program;
	case STM32_SWJ:
		cur_prog_mode = ADI_DP_SWJ;
stm32jtagswj_program:
		pi->chip_name = (char *)cm3_chips_param[CM3_STM32].chip_name;
		pi->chip_type = "cm3";
		ret = cm3_init(pi, prog);
		if (ERROR_OK != ret)
		{
			goto exit_stm32jtagswj;
		}
		ret = cm3_program(operations, pi, prog);
		
exit_stm32jtagswj:
		cm3_fini(pi, prog);
		break;
	case STM32_ISP:
		pi->chip_name = (char *)comisp_chips_param[COMISP_STM32].chip_name;
		pi->chip_type = "comisp";
		ret = comisp_init(pi, prog);
		if (ERROR_OK != ret)
		{
			goto exit_stm32isp;
		}
		ret = comisp_program(operations, pi, prog);
		
exit_stm32isp:
		comisp_fini(pi, prog);
		break;
	}
	
	return ret;
}

