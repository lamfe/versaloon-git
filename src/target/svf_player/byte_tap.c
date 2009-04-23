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

#include "programmer.h"

#include "byte_tap.h"

const char *tap_state_name[TAP_NUM_OF_STATE] = 
{
	"RESET",
	"IDLE",
	"DRSHIFT",
	"DRPAUSE",
	"IRSHIFT",
	"IRPAUSE",
	"DRSELECT",
	"DRCAPTURE",
	"DREXIT1",
	"DREXIT2",
	"DRUPDATE",
	"IRSELECT",
	"IRCAPTURE",
	"IREXTI1",
	"IREXIT2",
	"IRUPDATE",
};

// tap_move[from_state][to_state]
const uint8 tap_move[6][6] = 
{
//	RESET,	IDLE,	DRSHIFT,DRPAUSE,IRSHIFT,IRPAUSE
	{0xff,	0x7f,	0x2f,	0x0a,	0x37,	0x16},	// RESET
	{0xff,	0x00,	0x45,	0x05,	0x4b,	0x0b},	// IDLE
	{0x00,	0x00,	0x00,	0x00,	0x00,	0x00},	// DRSHIFT
	{0xff,	0x60,	0x40,	0x5c,	0x3c,	0x5e},	// DRPAUSE
	{0x00,	0x00,	0x00,	0x00,	0x00,	0x00},	// IRSHIFT
	{0xff,	0x60,	0x38,	0x5c,	0x40,	0x5e}	// IRPAUSE
};

tap_state_t end_state = IDLE;
tap_state_t cur_state = IDLE;
uint8 tap_tms_remain = 0;

uint8 tap_state_is_stable(tap_state_t state)
{
	return ((RESET == state) || (IDLE == state) 
			|| (DRPAUSE == state) || (IRPAUSE == state));
}

uint8 tap_state_is_valid(tap_state_t state)
{
	return (state < TAP_NUM_OF_STATE);
}

RESULT tap_end_state(tap_state_t state)
{
	if (tap_state_is_valid(state))
	{
		if (state < 6)
		{
			end_state = state;
			return ERROR_OK;
		}
		else
		{
			LOG_BUG(_GETTEXT("can not shift to %s\n"), tap_state_name[state]);
			return ERROR_FAIL;
		}
	}
	else
	{
		LOG_BUG(_GETTEXT("%d is not a valid state\n"), state);
		return ERROR_FAIL;
	}
}

RESULT tap_state_move(void)
{
	uint16 tms_16bit;

	if ((cur_state == DRSHIFT) || (cur_state == IRSHIFT))
	{
		LOG_BUG(_GETTEXT("move from %s is invalid\n"), 
				tap_state_name[cur_state]);
		return ERROR_FAIL;
	}
	
	if (tap_tms_remain > 0)
	{
		// state is in IDLE, tap_tms_remain clocks should be shifted first
		tms_16bit = tap_move[cur_state][end_state];
		if (tms_16bit & 0x80)
		{
			tms_16bit |= 0xFF << 8;
		}
		tms_16bit <<= tap_tms_remain;
		tap_tms_remain = 0;
		cur_state = end_state;
		return jtag_tms((uint8*)&tms_16bit, 2);
	}
	else
	{
		if (ERROR_OK != jtag_tms((uint8*)&tap_move[cur_state][end_state], 1))
		{
			return ERROR_FAIL;
		}
		cur_state = end_state;
	}
	
	return ERROR_OK;
}

RESULT tap_path_move(uint32 num_states, tap_state_t *path)
{
	if ((2 == num_states) && (RESET == path[0]) && (IDLE == path[1]))
	{
		if (ERROR_OK != tap_end_state(path[0]))
		{
			return ERROR_FAIL;
		}
		if (ERROR_OK != tap_state_move())
		{
			return ERROR_FAIL;
		}
		if (ERROR_OK != tap_end_state(path[1]))
		{
			return ERROR_FAIL;
		}
		if (ERROR_OK != tap_state_move())
		{
			return ERROR_FAIL;
		}
		
		return ERROR_OK;
	}
	else
	{
		LOG_ERROR("path move is not supported\n");
		return ERROR_FAIL;
	}
}

RESULT tap_runtest(tap_state_t run_state, tap_state_t end_state, 
				   uint32 num_cycles)
{
	uint8 buffer0, tms;
	
	if ((IDLE == run_state) || (DRPAUSE == run_state) 
		|| (IRPAUSE == run_state))
	{
		tms = 0;
	}
	else if (RESET == run_state)
	{
		tms = 1;
	}
	else
	{
		if (tap_state_is_valid(run_state))
		{
			LOG_ERROR(_GETTEXT("unstable run_state: %s"), 
					  tap_state_name[run_state]);
		}
		else
		{
			LOG_ERROR(_GETTEXT("invalid run_state: %d"), run_state);
		}
		
		return ERROR_FAIL;
	}
	if (!tap_state_is_stable(end_state))
	{
		if (tap_state_is_valid(end_state))
		{
			LOG_ERROR(_GETTEXT("unstable end_state: %s"), 
					  tap_state_name[end_state]);
		}
		else
		{
			LOG_ERROR(_GETTEXT("invalid end_state: %d"), end_state);
		}
		
		return ERROR_FAIL;
	}
	
	if (cur_state != run_state)
	{
		if (ERROR_OK != tap_end_state(run_state))
		{
			return ERROR_FAIL;
		}
		if (ERROR_OK != tap_state_move())
		{
			return ERROR_FAIL;
		}
	}
	
	if (ERROR_OK != jtag_tms_clocks(num_cycles >> 3, tms))
	{
		return ERROR_FAIL;
	}
	if (num_cycles > 0xFFFF)
	{
		// it will take to much time, commit here
		if (ERROR_OK != tap_commit())
		{
			return ERROR_FAIL;
		}
	}
	num_cycles &= 7;
	
	tap_tms_remain = (uint8)num_cycles;
	if (run_state != IDLE)
	{
		// not runtest in IDLE, just shift out 1 byte
		buffer0 = 0;
		if (ERROR_OK != jtag_tms(&buffer0, 1))
		{
			return ERROR_FAIL;
		}
		tap_tms_remain = 0;
	}
	if (end_state != run_state)
	{
		// runtest in IDLE, but end_state is not IDLE
		if (ERROR_OK != tap_end_state(end_state))
		{
			return ERROR_FAIL;
		}
		if (ERROR_OK != tap_state_move())
		{
			return ERROR_FAIL;
		}
	}
	if (tap_tms_remain > 0)
	{
		cur_state = run_state;
	}
	else
	{
		cur_state = end_state;
	}
	
	return ERROR_OK;
}

RESULT tap_scan_ir(uint8 *buffer, uint32 bit_size)
{
	tap_state_t last_end_state = end_state;
	RESULT ret;
	
	if (tap_tms_remain > 0)
	{
		if (ERROR_OK != tap_end_state(IRPAUSE))
		{
			return ERROR_FAIL;
		}
		if (ERROR_OK != tap_state_move())
		{
			return ERROR_FAIL;
		}
	}
	
	if (ERROR_OK != tap_end_state(IRSHIFT))
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != tap_state_move())
	{
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != tap_end_state(last_end_state))
	{
		return ERROR_FAIL;
	}
	if (end_state != IRPAUSE)
	{
		ret = jtag_xr(buffer, (uint16)bit_size, 0, 0, 
					  1 << ((bit_size - 1) % 8), 
					  tap_move[IRPAUSE][end_state]);
		if (ret != ERROR_OK)
		{
			return ERROR_FAIL;
		}
	}
	else
	{
		ret = jtag_xr(buffer, (uint16)bit_size, 0, 0, 
					  1 << ((bit_size - 1) % 8), 0);
		if (ret != ERROR_OK)
		{
			return ERROR_FAIL;
		}
	}
	cur_state = end_state;
	
	return ERROR_OK;
}

RESULT tap_scan_dr(uint8 *buffer, uint32 bit_size)
{
	tap_state_t last_end_state = end_state;
	RESULT ret;
	
	if (tap_tms_remain > 0)
	{
		if (ERROR_OK != tap_end_state(DRPAUSE))
		{
			return ERROR_FAIL;
		}
		if (ERROR_OK != tap_state_move())
		{
			return ERROR_FAIL;
		}
	}
	
	if (ERROR_OK != tap_end_state(DRSHIFT))
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != tap_state_move())
	{
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != tap_end_state(last_end_state))
	{
		return ERROR_FAIL;
	}
	if (end_state != DRPAUSE)
	{
		ret = jtag_xr(buffer, (uint16)bit_size, 0, 0, 
					  1 << ((bit_size - 1) % 8), 
					  tap_move[DRPAUSE][end_state]);
		if (ret != ERROR_OK)
		{
			return ERROR_FAIL;
		}
	}
	else
	{
		ret = jtag_xr(buffer, (uint16)bit_size, 0, 0, 
					  1 << ((bit_size - 1) % 8), 0);
		if (ret != ERROR_OK)
		{
			return ERROR_FAIL;
		}
	}
	cur_state = end_state;
	
	return ERROR_OK;
}

RESULT tap_commit(void)
{
	if (ERROR_OK != jtag_commit())
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

RESULT tap_init(void)
{
	if (ERROR_OK != jtag_init())
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != tap_end_state(RESET))
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != tap_state_move())
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != jtag_commit())
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

