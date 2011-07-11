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

#include <string.h>

#include "../versaloon_include.h"
#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

RESULT usbtolpcicp_init(uint8_t interface_index)
{
	return usbtoxxx_init_command(USB_TO_LPCICP, interface_index);
}

RESULT usbtolpcicp_fini(uint8_t interface_index)
{
	return usbtoxxx_fini_command(USB_TO_LPCICP, interface_index);
}

RESULT usbtolpcicp_config(uint8_t interface_index)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
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
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
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
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
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
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_out_command(USB_TO_LPCICP, interface_index, buff, len, 0);
}

RESULT usbtolpcicp_poll_ready(uint8_t interface_index, uint8_t data,
			uint8_t *ret, uint8_t setmask, uint8_t clearmask, uint16_t pollcnt)
{
	uint8_t cmdbuf[5];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	cmdbuf[0] = data;
	cmdbuf[1] = setmask;
	cmdbuf[2] = clearmask;
	SET_LE_U16(&cmdbuf[3], pollcnt);
	
	return usbtoxxx_poll_command(USB_TO_LPCICP, interface_index, cmdbuf, 5,
								 ret, 1);
}

