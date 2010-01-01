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

#include "c8051f.h"
#include "c8051f_internal.h"

#define CUR_TARGET_STRING			C8051F_STRING

const struct program_area_map_t c8051f_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, AREA_ATTR_EWR},
	{0, 0, 0, 0, 0}
};

const struct program_mode_t c8051f_program_mode[] = 
{
	{'j', SET_FREQUENCY, JTAG_HL},
	{'c', SET_FREQUENCY, C2 | GPIO},
	{0, NULL, 0}
};

static void c8051f_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<j|c>\n\n", 
			CUR_TARGET_STRING);
}

RESULT c8051f_parse_argument(char cmd, const char *argu)
{
	argu = argu;
	
	switch (cmd)
	{
	case 'h':
		c8051f_usage();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}





#define get_target_voltage(v)					prog->get_target_voltage(v)
RESULT c8051f_program(struct operation_t operations, struct program_info_t *pi, 
					  struct programmer_info_t *prog)
{
	uint16_t voltage;

#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if ((   (operations.write_operations & APPLICATION) 
			|| (operations.verify_operations & APPLICATION)) 
		&& (NULL == pi->program_areas[APPLICATION_IDX].buff))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for flash");
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
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
	
	switch (program_mode)
	{
	case C8051F_C2:
		if (target_chip_param.program_mode & (1 << C8051F_C2))
		{
			return c8051f_c2_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "C2", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	case C8051F_JTAG:
		if (target_chip_param.program_mode & (1 << C8051F_JTAG))
		{
			return c8051f_jtag_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "JTAG", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	default:
		// invalid mode
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), program_mode, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID_PROG_MODE;
	}
}

