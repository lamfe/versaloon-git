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

uint8 usbtoissp_num_of_interface = 0;


RESULT usbtoissp_init(void)
{
	return usbtoxxx_init_command(USB_TO_ISSP, &usbtoissp_num_of_interface);
}

RESULT usbtoissp_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_ISSP);
}

RESULT usbtoissp_config(uint8 interface_index)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_conf_command(USB_TO_ISSP, interface_index, NULL, 0);
}

RESULT usbtoissp_vector(uint8 interface_index, uint8 operate, uint8 addr, 
						uint8 data, uint8 *buf)
{
	uint8 cmd_buf[3];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	cmd_buf[0] = operate;
	cmd_buf[1] = addr;
	cmd_buf[2] = data;
	
	if (operate & ISSP_VECTOR_ATTR_READ)
	{
		return usbtoxxx_inout_command(USB_TO_ISSP, interface_index, cmd_buf, 
									  3, 1, buf, 0, 1, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_ISSP, interface_index, cmd_buf, 
									  3, 0, NULL, 0, 0, 1);
	}
}

RESULT usbtoissp_enter_program_mode(uint8 interface_index, uint8 mode)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_in_command(USB_TO_ISSP, interface_index, &mode, 1, 0, 
							   NULL, 0, 0, 0);
}

RESULT usbtoissp_leave_program_mode(uint8 interface_index, uint8 mode)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_out_command(USB_TO_ISSP, interface_index, &mode, 1, 0);
}

RESULT usbtoissp_wait_and_poll(uint8 interface_index)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_poll_command(USB_TO_ISSP, interface_index, NULL, 0);
}

