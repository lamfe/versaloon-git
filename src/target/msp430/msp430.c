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

#include "msp430.h"
#include "msp430_internal.h"

#include "JTAGfunc.h"

#define CUR_TARGET_STRING			MSP430_STRING

struct program_area_map_t msp430_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EW | AREA_ATTR_V},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t msp430_program_mode[] = 
{
	{'j', "", MSP430_JTAG},
	{'s', "", MSP430_SBW},
	{'b', USE_COMM, 0},
	{0, NULL, 0}
};

struct program_functions_t msp430_program_functions;

void msp430_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<j|s|b>\n\n", 
			CUR_TARGET_STRING);
}

RESULT (*msp430jtagsbw_init)(void);
RESULT (*msp430jtagsbw_fini)(void);
RESULT (*msp430jtagsbw_config)(uint8_t has_test);
RESULT (*msp430jtagsbw_ir)(uint8_t *ir, uint8_t want_ret);
RESULT (*msp430jtagsbw_dr)(uint32_t *dr, uint8_t len, uint8_t want_ret);
RESULT (*msp430jtagsbw_tclk)(uint8_t value);
RESULT (*msp430jtagsbw_tclk_strobe)(uint16_t cnt);
RESULT (*msp430jtagsbw_reset)(void);
RESULT (*msp430jtagsbw_poll)(uint32_t dr, uint32_t mask, uint32_t value, 
						uint8_t len, uint16_t poll_cnt, uint8_t toggle_tclk);

RESULT msp430_parse_argument(char cmd, const char *argu)
{
	uint8_t mode;
	
	switch (cmd)
	{
	case 'h':
		msp430_usage();
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
		case MSP430_MODE_JTAG:
			msp430jtagsbw_init = cur_programmer->msp430jtag_init;
			msp430jtagsbw_fini = cur_programmer->msp430jtag_fini;
			msp430jtagsbw_config = cur_programmer->msp430jtag_config;
			msp430jtagsbw_ir = cur_programmer->msp430jtag_ir;
			msp430jtagsbw_dr = cur_programmer->msp430jtag_dr;
			msp430jtagsbw_tclk = cur_programmer->msp430jtag_tclk;
			msp430jtagsbw_tclk_strobe = cur_programmer->msp430jtag_tclk_strobe;
			msp430jtagsbw_reset = cur_programmer->msp430jtag_reset;
			msp430jtagsbw_poll = cur_programmer->msp430jtag_poll;
			
			memcpy(&msp430_program_functions, &msp430jtagsbw_program_functions, 
					sizeof(msp430_program_functions));
			break;
		case MSP430_MODE_SBW:
			msp430jtagsbw_init = cur_programmer->msp430sbw_init;
			msp430jtagsbw_fini = cur_programmer->msp430sbw_fini;
			msp430jtagsbw_config = cur_programmer->msp430sbw_config;
			msp430jtagsbw_ir = cur_programmer->msp430sbw_ir;
			msp430jtagsbw_dr = cur_programmer->msp430sbw_dr;
			msp430jtagsbw_tclk = cur_programmer->msp430sbw_tclk;
			msp430jtagsbw_tclk_strobe = cur_programmer->msp430sbw_tclk_strobe;
			msp430jtagsbw_reset = cur_programmer->msp430sbw_reset;
			msp430jtagsbw_poll = cur_programmer->msp430sbw_poll;
			
			memcpy(&msp430_program_functions, &msp430jtagsbw_program_functions, 
					sizeof(msp430_program_functions));
			break;
		case MSP430_MODE_BSL:
			return ERROR_FAIL;
			break;
		}
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

