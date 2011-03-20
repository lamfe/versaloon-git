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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"

#include "24cxx.h"
#include "24cxx_internal.h"

#define CUR_TARGET_STRING			EE24CXX_STRING

struct program_area_map_t ee24cxx_program_area_map[] = 
{
	{EEPROM_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t ee24cxx_program_mode[] = 
{
	{'*', SET_FREQUENCY, I2C},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(ee24cxx);
LEAVE_PROGRAM_MODE_HANDLER(ee24cxx);
ERASE_TARGET_HANDLER(ee24cxx);
WRITE_TARGET_HANDLER(ee24cxx);
READ_TARGET_HANDLER(ee24cxx);
const struct program_functions_t ee24cxx_program_functions = 
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(ee24cxx), 
	LEAVE_PROGRAM_MODE_FUNCNAME(ee24cxx), 
	ERASE_TARGET_FUNCNAME(ee24cxx), 
	WRITE_TARGET_FUNCNAME(ee24cxx), 
	READ_TARGET_FUNCNAME(ee24cxx)
};

static uint8_t ee24cxx_addr = 0xAC;

VSS_HANDLER(ee24cxx_help)
{
	VSS_CHECK_ARGC(1);
	printf("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>              set IIC frequency, in KHz\n\n", 
			CUR_TARGET_STRING);
	return ERROR_OK;
}

const struct vss_cmd_t ee24cxx_notifier[] = 
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				ee24cxx_help),
	VSS_CMD_END
};





static struct interfaces_info_t *interfaces = NULL;
#define commit()					interfaces->peripheral_commit()

ENTER_PROGRAM_MODE_HANDLER(ee24cxx)
{
	struct program_info_t *pi = context->pi;
	
	interfaces = &(context->prog->interfaces);
	
	if (!pi->frequency)
	{
		pi->frequency = 100;
	}
	interfaces->i2c.init(0);
	interfaces->i2c.config(0, pi->frequency, 0, 10000, true);
	return commit();
}

LEAVE_PROGRAM_MODE_HANDLER(ee24cxx)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	interfaces->i2c.fini(0);
	return commit();
}

ERASE_TARGET_HANDLER(ee24cxx)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	return commit();
}

WRITE_TARGET_HANDLER(ee24cxx)
{
	uint16_t addr_word = (uint16_t)addr;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case EEPROM_CHAR:
		addr_word = SYS_TO_LE_U16(addr_word);
//		interfaces->i2c.write(0, ee24cxx_addr, (uint8_t *)&addr_word, 2, 0);
//		interfaces->i2c.read(0, ee24cxx_addr, buff, (uint16_t)size, 1);
//		interfaces->peripheral_commit();
		break;
	default:
		break;
	}
	return ret;
}

READ_TARGET_HANDLER(ee24cxx)
{
	uint16_t addr_word = (uint16_t)addr;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case EEPROM_CHAR:
		addr_word = SYS_TO_BE_U16(addr_word);
		interfaces->i2c.write(0, ee24cxx_addr, (uint8_t *)&addr_word, 2, 0);
		interfaces->i2c.read(0, ee24cxx_addr, buff, (uint16_t)size, 1);
		ret = interfaces->peripheral_commit();
		break;
	default:
		break;
	}
	return ret;
}

