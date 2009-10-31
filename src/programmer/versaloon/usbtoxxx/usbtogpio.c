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

uint8_t usbtogpio_num_of_interface;

RESULT usbtogpio_init(void)
{
	return usbtoxxx_init_command(USB_TO_GPIO, &usbtogpio_num_of_interface);
}

RESULT usbtogpio_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_GPIO);
}

RESULT usbtogpio_config(uint8_t interface_index, uint16_t mask, uint16_t direction)
{
	uint8_t conf[3];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	conf[0] = (mask >> 0) & 0xFF;
	conf[1] = (mask >> 8) & 0xFF;
	if (direction > 0)
	{
		conf[2] = USB_TO_GPIO_DIR_MSK;
	}
	else
	{
		conf[2] = 0;
	}
	
	return usbtoxxx_conf_command(USB_TO_GPIO, interface_index, conf, 3);
}

RESULT usbtogpio_in(uint8_t interface_index, uint16_t mask, uint16_t *value)
{
	uint8_t buf[2];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	buf[0] = (mask >> 0) & 0xFF;
	buf[1] = (mask >> 8) & 0xFF;
	
	return usbtoxxx_in_command(USB_TO_GPIO, interface_index, buf, 2, 2, 
							   (uint8_t*)value, 0, 2, 0);
}

RESULT usbtogpio_out(uint8_t interface_index, uint16_t mask, uint16_t value)
{
	uint8_t buf[4];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	buf[0] = (mask >> 0) & 0xFF;
	buf[1] = (mask >> 8) & 0xFF;
	buf[2] = (value >> 0) & 0xFF;
	buf[3] = (value >> 8) & 0xFF;
	
	return usbtoxxx_out_command(USB_TO_GPIO, interface_index, buf, 4, 0);
}

