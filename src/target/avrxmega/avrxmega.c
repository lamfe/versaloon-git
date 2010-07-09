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

#include "timer.h"

#include "memlist.h"
#include "filelist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "avrxmega.h"
#include "avrxmega_internal.h"

#define CUR_TARGET_STRING			AVRXMEGA_STRING

struct program_area_map_t avrxmega_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_RNP},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t avrxmega_program_mode[] = 
{
	{'j', SET_FREQUENCY, JTAG_HL},
	{'p', 0, PDI},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(avrxmega);
LEAVE_PROGRAM_MODE_HANDLER(avrxmega);
ERASE_TARGET_HANDLER(avrxmega);
WRITE_TARGET_HANDLER(avrxmega);
READ_TARGET_HANDLER(avrxmega);
struct program_functions_t avrxmega_program_functions = 
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(avrxmega), 
	LEAVE_PROGRAM_MODE_FUNCNAME(avrxmega), 
	ERASE_TARGET_FUNCNAME(avrxmega), 
	WRITE_TARGET_FUNCNAME(avrxmega), 
	READ_TARGET_FUNCNAME(avrxmega)
};

static void avrxmega_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<j>\n\
  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz\n\n",
			CUR_TARGET_STRING);
}

PARSE_ARGUMENT_HANDLER(avrxmega)
{
	uint8_t mode;
	
	switch (cmd)
	{
	case 'h':
		avrxmega_usage();
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
		case AVRXMEGA_JTAG:
			break;
		case AVRXMEGA_PDI:
			break;
		}
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

#define jtag_init()					interfaces->jtag_hl.jtag_hl_init()
#define jtag_fini()					interfaces->jtag_hl.jtag_hl_fini()
#define jtag_config(kHz,a,b,c,d)	\
	interfaces->jtag_hl.jtag_hl_config((kHz), (a), (b), (c), (d))
#define jtag_runtest(len)			interfaces->jtag_hl.jtag_hl_runtest(len)
#define jtag_ir_write(ir, len)		\
	interfaces->jtag_hl.jtag_hl_ir((uint8_t*)(ir), (len), AVRXMEGA_JTAG_RTI_CYCLE, 0)
#define jtag_dr_write(dr, len)		\
	interfaces->jtag_hl.jtag_hl_dr((uint8_t*)(dr), (len), AVRXMEGA_JTAG_RTI_CYCLE, 0)
#define jtag_dr_read(dr, len)		\
	interfaces->jtag_hl.jtag_hl_dr((uint8_t*)(dr), (len), AVRXMEGA_JTAG_RTI_CYCLE, 1)
#define jtag_register_callback(s,r)	\
	interfaces->jtag_hl.jtag_hl_register_callback((s), (r))

// retry 1000 times with 0 interval
#define poll_start()				interfaces->poll.poll_start(1000, 0)
#define poll_end()					interfaces->poll.poll_end()
#define poll_check(o, m, v)			\
	interfaces->poll.poll_checkbyte((o), (m), (v))
#define poll_checkfail(o, m, v)		\
	interfaces->poll.poll_checkfail((o), (m), (v))

#define delay_ms(ms)				interfaces->delay.delayms((ms) | 0x8000)
#define delay_us(us)				interfaces->delay.delayus((us) & 0x7FFF)
#define jtag_commit()				interfaces->peripheral_commit()

#define avrxmega_jtag_ir(ir)		jtag_ir_write((ir), AVRXMEGA_JTAG_INS_Len)

static struct interfaces_info_t *interfaces = NULL;

ENTER_PROGRAM_MODE_HANDLER(avrxmega)
{
	struct program_info_t *pi = context->pi;
	
	interfaces = &(context->prog->interfaces);
	
	if (!pi->frequency)
	{
		pi->frequency = 4500;
	}
	
	// init
	jtag_init();
	jtag_config(pi->frequency, pi->jtag_pos.ub, pi->jtag_pos.ua, 
					pi->jtag_pos.bb, pi->jtag_pos.ba);
	if (ERROR_OK != jtag_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "init jtag");
		return ERROR_FAIL;
	}
	
	return jtag_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(avrxmega)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	jtag_fini();
	return jtag_commit();
}

ERASE_TARGET_HANDLER(avrxmega)
{
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	return ret;
}

WRITE_TARGET_HANDLER(avrxmega)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

READ_TARGET_HANDLER(avrxmega)
{
	uint8_t ir;
	uint32_t dr;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(buff);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read jtag_id use IDCODE
		// this should always work
		ir = AVRXMEGA_JTAG_INS_IDCODE;
		avrxmega_jtag_ir(&ir);
		dr = 0;
		jtag_dr_read(&dr, 32);
		if (ERROR_OK != jtag_commit())
		{
			return ERROR_FAIL;
		}
		LOG_DEBUG("JTAGID is 0x%08X\n", dr);
		
		dr = ((dr >> 12) & 0xFFFF) | 0x1E0000;
		memcpy(buff, &dr, 4);
		break;
	case APPLICATION_CHAR:
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}
