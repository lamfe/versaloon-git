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

#include "at91sam3.h"
#include "at91sam3_internal.h"
#include "comisp.h"
#include "cm3.h"
#include "adi_v5p1.h"

#define CUR_TARGET_STRING			AT91SAM3_STRING

struct program_area_map_t at91sam3_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR/* | AREA_ATTR_RAE*/},
	{LOCK_CHAR, 0, 0, 0, 0, 0},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t at91sam3_program_mode[] = 
{
	{'j', SET_FREQUENCY, JTAG_HL},
	{'s', "", SWJ},
	{'i', USE_COMM, 0},
	{0, NULL, 0}
};

struct program_functions_t at91sam3_program_functions;

uint8_t at91sam3_wait_state = 0;
uint8_t at91sam3_plane_idx = 0;

static void at91sam3_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<j|s>\n\
  -x,  --execute <ADDRESS>                  execute program\n\
  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz\n\
  -B,  --block-idx <0|1>                    define plane index\n\n",
			CUR_TARGET_STRING);
}

RESULT at91sam3_parse_argument(char cmd, const char *argu)
{
	uint8_t mode;
	
	switch (cmd)
	{
	case 'h':
		at91sam3_usage();
		break;
	case 'm':
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		mode = (uint8_t)strtoul(argu, NULL,0);
		switch (mode)
		{
		case AT91SAM3_JTAG:
		case AT91SAM3_SWD:
			at91sam3_program_area_map[0].attr |= AREA_ATTR_RNP;
			cm3_mode_offset = 0;
			cm3_parse_argument('c', "cm3_at91sam3");
			memcpy(&at91sam3_program_functions, &cm3_program_functions, 
					sizeof(at91sam3_program_functions));
			break;
		}
		break;
	case 'E':
		comisp_print_comm_info(COMISP_LPCARM);
		break;
	case 'b':
		cm3_parse_argument(cmd, argu);
		break;
	case 'C':
		comisp_parse_argument(cmd, argu);
		break;
	case 'x':
		cm3_parse_argument(cmd, argu);
		comisp_parse_argument(cmd, argu);
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT at91sam3_get_flash_page_info(struct program_context_t *context, 
					uint32_t addr, uint32_t *controller, uint16_t *page_num)
{
	uint32_t i;
	uint32_t base, size;
	struct chip_param_t *param = context->param;
	struct chip_area_info_t *flash_info = &param->chip_areas[APPLICATION_IDX];
	
	base = param->param[8];
	for (i = 0; i < param->param[AT91SAM3_PARAM_PLANE_NUMBER]; i++)
	{
		size = param->param[9 + i * 2];
		if ((addr >= base) && (addr < (base + size)))
		{
			*controller = param->param[1 + i];
			*page_num = (uint16_t)((addr - base) / flash_info->page_size);
			return ERROR_OK;
		}
		base += size;
	}
	
	return ERROR_FAIL;
}

RESULT at91sam3_adjust_setting(struct program_info_t *pi, 
							struct chip_param_t *param, uint32_t program_mode)
{
	uint32_t base, size, page_size;
	
	REFERENCE_PARAMETER(program_mode);
	
	at91sam3_plane_idx = (uint8_t)pi->block_idx;
	if (at91sam3_plane_idx >= param->param[AT91SAM3_PARAM_PLANE_NUMBER])
	{
		LOG_ERROR(_GETTEXT("flash plane%d is not supported by current target"), 
					at91sam3_plane_idx);
		return ERROR_FAIL;
	}
	base = param->param[8 + at91sam3_plane_idx * 2];
	size = param->param[9 + at91sam3_plane_idx * 2];
	page_size = param->chip_areas[APPLICATION_IDX].page_size;
	param->chip_areas[APPLICATION_IDX].addr = base;
	param->chip_areas[APPLICATION_IDX].size = size;
	param->chip_areas[APPLICATION_IDX].page_num = size / page_size;
	
	return ERROR_OK;
}
