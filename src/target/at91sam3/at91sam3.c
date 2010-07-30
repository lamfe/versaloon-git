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
#include <ctype.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"

#include "at91sam3.h"
#include "at91sam3_internal.h"
#include "cm3.h"
#include "adi_v5p1.h"

#define CUR_TARGET_STRING			AT91SAM3_STRING

struct program_area_map_t at91sam3_program_area_map[] = 
{
	// flash plane0
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR/* | AREA_ATTR_RAE*/},
	// eeprom is used to emulate flash plane1
	{EEPROM_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR/* | AREA_ATTR_RAE*/},
	{LOCK_CHAR, 0, 0, 0, 0, 0},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t at91sam3_program_mode[] = 
{
	{'j', SET_FREQUENCY, JTAG_HL},
	{'s', "", SWD},
	{0, NULL, 0}
};

struct program_functions_t at91sam3_program_functions;

MISC_HANDLER(at91sam3_help)
{
	MISC_CHECK_ARGC(1);
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<j|s>\n\
  -x,  --execute <ADDRESS>                  execute program\n\
  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz\n\n",
			CUR_TARGET_STRING);
	return ERROR_OK;
}

MISC_HANDLER(at91sam3_mode)
{
	uint8_t mode;
	
	MISC_CHECK_ARGC(2);
	mode = (uint8_t)strtoul(argv[1], NULL,0);
	switch (mode)
	{
	case AT91SAM3_JTAG:
	case AT91SAM3_SWD:
		at91sam3_program_area_map[0].attr |= AREA_ATTR_RNP;
		cm3_mode_offset = 0;
		misc_call_notifier(cm3_notifier, "chip", "cm3_at91sam3");
		memcpy(&at91sam3_program_functions, &cm3_program_functions, 
				sizeof(at91sam3_program_functions));
		break;
	}
	return ERROR_OK;
}

const struct misc_cmd_t at91sam3_notifier[] = 
{
	MISC_CMD(	"help",
				"print help information of current target for internal call",
				at91sam3_help),
	MISC_CMD(	"mode",
				"set programming mode of target for internal call",
				at91sam3_mode),
	MISC_CMD_END
};

