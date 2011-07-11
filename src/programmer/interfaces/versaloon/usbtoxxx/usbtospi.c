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

RESULT usbtospi_init(uint8_t interface_index)
{
	return usbtoxxx_init_command(USB_TO_SPI, interface_index);
}

RESULT usbtospi_fini(uint8_t interface_index)
{
	return usbtoxxx_fini_command(USB_TO_SPI, interface_index);
}

RESULT usbtospi_config(uint8_t interface_index, uint32_t kHz, uint8_t mode)
{
	uint8_t conf[5];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	conf[0] = mode;
	SET_LE_U32(&conf[1], kHz);
	
	return usbtoxxx_conf_command(USB_TO_SPI, interface_index, conf, 5);
}

RESULT usbtospi_io(uint8_t interface_index, uint8_t *out, uint8_t *in,
				   uint16_t bytelen)
{
	uint8_t *cmd_ptr;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	if (NULL == out)
	{
		cmd_ptr = versaloon_cmd_buf;
		memset(cmd_ptr, 0xFF, bytelen);
	}
	else
	{
		cmd_ptr = out;
	}
	
	return usbtoxxx_inout_command(USB_TO_SPI, interface_index, cmd_ptr,
									bytelen, bytelen, in, 0, bytelen, 1);
}

