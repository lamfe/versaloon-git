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
#include "cm3.h"
#include "adi_v5p1.h"

#define CUR_TARGET_STRING			STM32_STRING

struct program_area_map_t stm32_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t stm32_program_mode[] = 
{
	{'j', SET_FREQUENCY, JTAG_HL},
	{'s', "", SWJ},
	{'i', USE_COMM, 0},
	{0, NULL, 0}
};

struct program_functions_t stm32_program_functions;

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
	uint8_t mode;
	
	switch (cmd)
	{
	case 'h':
		stm32_usage();
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
		case STM32_JTAG:
		case STM32_SWD:
			stm32_program_area_map[0].attr |= AREA_ATTR_NP;
			cm3_mode_offset = 0;
			cm3_parse_argument('c', "cm3_stm32");
			memcpy(&stm32_program_functions, &cm3_program_functions, 
					sizeof(stm32_program_functions));
			break;
		case STM32_ISP:
			stm32_program_area_map[0].attr &= ~AREA_ATTR_NP;
			comisp_mode_offset = STM32_ISP;
			comisp_parse_argument('c', "comisp_stm32");
			memcpy(&stm32_program_functions, &comisp_program_functions, 
					sizeof(stm32_program_functions));
			break;
		}
		break;
	case 'E':
		comisp_print_comm_info(COMISP_STM32);
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

void stm32_print_device(uint32_t mcuid)
{
	char rev_char = 0;
	uint16_t den, rev;
	
	den = mcuid & STM32_DEN_MSK;
	rev = (mcuid & STM32_REV_MSK) >> 16;
	switch (den)
	{
	case STM32_DEN_LOW:
		LOG_INFO(_GETTEXT("STM32 type: low-density device\n"));
		switch (rev)
		{
		case 0x1000:
			rev_char = 'A';
			break;
		}
		break;
	case STM32_DEN_MEDIUM:
		LOG_INFO(_GETTEXT("STM32 type: medium-density device\n"));
		switch (rev)
		{
		case 0x0000:
			rev_char = 'A';
			break;
		case 0x2000:
			rev_char = 'B';
			break;
		case 0x2001:
			rev_char = 'Z';
			break;
		case 0x2003:
			rev_char = 'Y';
			break;
		}
		break;
	case STM32_DEN_HIGH:
		LOG_INFO(_GETTEXT("STM32 type: high-density device\n"));
		switch (rev)
		{
		case 0x1000:
			rev_char = 'A';
			break;
		case 0x1001:
			rev_char = 'Z';
			break;
		}
		break;
	case STM32_DEN_CONNECTIVITY:
		LOG_INFO(_GETTEXT("STM32 type: connectivity device\n"));
		switch (rev)
		{
		case 0x1000:
			rev_char = 'A';
			break;
		case 0x1001:
			rev_char = 'Z';
			break;
		}
		break;
	default:
		LOG_INFO(_GETTEXT("STM32 type: unknown device\n"));
		break;
	}
	if (rev_char != 0)
	{
		LOG_INFO(_GETTEXT("STM32 revision: %c\n"), rev_char);
	}
}

