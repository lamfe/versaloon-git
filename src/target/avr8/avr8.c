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

#include "avr8.h"
#include "avr8_internal.h"

#define CUR_TARGET_STRING			AVR8_STRING

const struct program_area_map_t avr8_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, AREA_ATTR_EWR},
	{EEPROM_CHAR, 1, 2, 0, AREA_ATTR_EWR},
	{FUSE_CHAR, 0, 0, 0, AREA_ATTR_WR},
	{LOCK_CHAR, 0, 0, 0, AREA_ATTR_WR},
	{CALIBRATION_CHAR, 0, 0, 0, AREA_ATTR_R},
	{0, 0, 0, 0, 0}
};

const struct program_mode_t avr8_program_mode[] = 
{
	{'i', SET_FREQUENCY, SPI | GPIO},
	{'j', SET_FREQUENCY, JTAG_HL},
	{'p', "", 0},
	{'s', "", 0},
	{0, NULL, 0}
};


static void avr8_usage(void)
{
	printf("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>              set ISP frequency, in KHz\n\
  -m,  --mode <MODE>                        set mode<b|p>\n\n", 
			CUR_TARGET_STRING);
}

RESULT avr8_parse_argument(char cmd, const char *argu)
{
	argu = argu;
	
	switch (cmd)
	{
	case 'h':
		avr8_usage();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}





#define get_target_voltage(v)					prog->get_target_voltage(v)
RESULT avr8_program(struct operation_t operations, struct program_info_t *pi, 
					struct programmer_info_t *prog)
{
	uint16_t voltage;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if ((   (operations.read_operations & APPLICATION) 
			&& (NULL == pi->program_areas[APPLICATION_IDX].buff)) 
		|| ((   (operations.write_operations & APPLICATION) 
				|| (operations.verify_operations & APPLICATION)) 
			&& (NULL == pi->program_areas[APPLICATION_IDX].buff)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for flash");
		return ERRCODE_INVALID_BUFFER;
	}
	if ((   (operations.read_operations & EEPROM) 
			&& (NULL == pi->program_areas[EEPROM_IDX].buff)) 
		|| ((   (operations.write_operations & EEPROM) 
				|| (operations.verify_operations & EEPROM)) 
			&& (NULL == pi->program_areas[EEPROM_IDX].buff)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for eeprom");
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
	case AVR8_ISP:
		if (target_chip_param.program_mode & (1 << AVR8_ISP))
		{
			return avr8_isp_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "ISP", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	case AVR8_JTAG:
		if (target_chip_param.program_mode & (1 << AVR8_JTAG))
		{
			return avr8_jtag_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "JTAG", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	case AVR8_HVPP:
		if (target_chip_param.program_mode & (1 << AVR8_HVPP))
		{
			return avr8_hvpp_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "HVPP", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	case AVR8_HVSP:
		if (target_chip_param.program_mode & (1 << AVR8_HVSP))
		{
			return avr8_hvsp_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "HVSP", 
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

