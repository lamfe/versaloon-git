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

#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

uint8_t usbtoc2_num_of_interface = 0;

RESULT usbtoc2_init(void)
{
	return usbtoxxx_init_command(USB_TO_C2, &usbtoc2_num_of_interface);
}

RESULT usbtoc2_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_C2);
}

RESULT usbtoc2_config(uint8_t interface_index)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_conf_command(USB_TO_C2, interface_index, NULL, 0);
}

RESULT usbtoc2_readaddr(uint8_t interface_index, uint8_t *data)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	if (data != NULL)
	{
		return usbtoxxx_in_command(USB_TO_C2, interface_index, NULL, 0, 1, 
									data, 0, 1, 0);
	}
	else
	{
		return usbtoxxx_in_command(USB_TO_C2, interface_index, NULL, 0, 1, 
									NULL, 0, 0, 0);
	}
}

RESULT usbtoc2_writeaddr(uint8_t interface_index, uint8_t addr)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_out_command(USB_TO_C2, interface_index, &addr, 1, 0);
}

RESULT usbtoc2_data(uint8_t interface_index, uint8_t r, uint8_t len, 
						uint8_t *buf)
{
	uint8_t cmdbuf[5];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
	if ((len > 4) || (0 == len))
	{
		LOG_BUG(_GETTEXT("C2 max data length is from 1 to 4\n"));
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (r)
	{
		cmdbuf[0] = 0x80 | len;
		memset(cmdbuf + 1, 0, len);
		
		return usbtoxxx_inout_command(USB_TO_C2, interface_index, cmdbuf, 
									  1 + len, len, buf, 0, len, 0);
	}
	else
	{
		cmdbuf[0] = len;
		memcpy(cmdbuf + 1, buf, len);
		
		return usbtoxxx_inout_command(USB_TO_C2, interface_index, cmdbuf, 
									  1 + len, 0, NULL, 0, 0, 0);
	}
}

