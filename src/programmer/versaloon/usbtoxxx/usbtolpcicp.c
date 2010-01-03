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

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "programmer.h"
#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

uint8_t usbtolpcicp_num_of_interface = 0;

RESULT usbtolpcicp_init(void)
{
	return usbtoxxx_init_command(USB_TO_LPCICP, &usbtolpcicp_num_of_interface);
}

RESULT usbtolpcicp_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_LPCICP);
}

RESULT usbtolpcicp_config(uint8_t interface_index)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_conf_command(USB_TO_LPCICP, interface_index, NULL, 0);
}

RESULT usbtolpcicp_enter_program_mode(uint8_t interface_index)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_inout_command(USB_TO_LPCICP, interface_index, NULL, 
									0, 0, NULL, 0, 0, 0);
}

RESULT usbtolpcicp_in(uint8_t interface_index, uint8_t *buff, uint16_t len)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_in_command(USB_TO_LPCICP, interface_index, buff, len, len, 
							   buff, 0, len, 0);
}

RESULT usbtolpcicp_out(uint8_t interface_index, uint8_t *buff, uint16_t len)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_out_command(USB_TO_LPCICP, interface_index, buff, len, 0);
}

RESULT usbtolpcicp_poll_ready(uint8_t interface_index, uint8_t *ret, 
			uint8_t data, uint8_t setmask, uint8_t clearmask, uint16_t pollcnt)
{
	uint8_t cmdbuf[5];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	cmdbuf[0] = data;
	cmdbuf[1] = setmask;
	cmdbuf[2] = clearmask;
	cmdbuf[3] = (pollcnt >> 0) & 0xFF;
	cmdbuf[4] = (pollcnt >> 8) & 0xFF;
	
	return usbtoxxx_poll_command(USB_TO_LPCICP, interface_index, cmdbuf, 5, 
								 ret, 1);
}

