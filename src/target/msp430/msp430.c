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
	{APPLICATION_CHAR, 1, 0, 0, AREA_ATTR_EWR},
	{0, 0, 0, 0, 0}
};

const struct program_mode_t msp430_program_mode[] = 
{
	{'j', "", MSP430_JTAG},
	{'s', "", MSP430_SBW},
	{'b', USE_COMM, 0},
	{0, NULL, 0}
};

void msp430_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<j|s|b>\n\n", 
			CUR_TARGET_STRING);
}

RESULT msp430_parse_argument(char cmd, const char *argu)
{
	argu = argu;
	
	switch (cmd)
	{
	case 'h':
		msp430_usage();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
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


#define get_target_voltage(v)					prog->get_target_voltage(v)
RESULT msp430_program(struct operation_t operations, struct program_info_t *pi, 
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
	
	switch(program_mode)
	{
	case MSP430_MODE_JTAG:
		if (target_chip_param.program_mode & (1 << MSP430_MODE_JTAG))
		{
			msp430jtagsbw_init = prog->msp430jtag_init;
			msp430jtagsbw_fini = prog->msp430jtag_fini;
			msp430jtagsbw_config = prog->msp430jtag_config;
			msp430jtagsbw_ir = prog->msp430jtag_ir;
			msp430jtagsbw_dr = prog->msp430jtag_dr;
			msp430jtagsbw_tclk = prog->msp430jtag_tclk;
			msp430jtagsbw_tclk_strobe = prog->msp430jtag_tclk_strobe;
			msp430jtagsbw_reset = prog->msp430jtag_reset;
			msp430jtagsbw_poll = prog->msp430jtag_poll;
			
			return msp430_jtag_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "JTAG", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		break;
	case MSP430_MODE_SBW:
		if (target_chip_param.program_mode & (1 << MSP430_MODE_SBW))
		{
			msp430jtagsbw_init = prog->msp430sbw_init;
			msp430jtagsbw_fini = prog->msp430sbw_fini;
			msp430jtagsbw_config = prog->msp430sbw_config;
			msp430jtagsbw_ir = prog->msp430sbw_ir;
			msp430jtagsbw_dr = prog->msp430sbw_dr;
			msp430jtagsbw_tclk = prog->msp430sbw_tclk;
			msp430jtagsbw_tclk_strobe = prog->msp430sbw_tclk_strobe;
			msp430jtagsbw_reset = prog->msp430sbw_reset;
			msp430jtagsbw_poll = prog->msp430sbw_poll;
			
			return msp430_jtag_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "SBW", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		
		break;
	case MSP430_MODE_BSL:
		if (target_chip_param.program_mode & (1 << MSP430_MODE_BSL))
		{
			return msp430_bsl_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "BSL", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		
		break;
	default:
		// invalid mode
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), program_mode, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID_PROG_MODE;
		break;
	}
}

