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

uint8 usbtoi2c_num_of_interface = 0;
uint8 usbtoi2c_buffer[256 + 6];

RESULT usbtoi2c_init(void)
{
	return usbtoxxx_init_command(USB_TO_I2C, &usbtoi2c_num_of_interface);
}

RESULT usbtoi2c_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_I2C);
}

RESULT usbtoi2c_set_speed(uint8 interface_index, uint16 kHz)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	usbtoi2c_buffer[0] = (kHz >> 0) & 0xFF;
	usbtoi2c_buffer[1] = (kHz >> 8) & 0xFF;
	
	return usbtoxxx_conf_command(USB_TO_I2C, interface_index, 
								 usbtoi2c_buffer, 2);
}

RESULT usbtoi2c_read(uint8 interface_index, uint16 chip_addr, 
					 uint8 chip_addr_len, uint8 *data, uint16 data_len, 
					 uint8 stop)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	if (data_len > 256)
	{
		LOG_BUG(_GETTEXT("too many data to transfer on I2C\n"));
		return ERROR_FAIL;
	}
	if ((chip_addr_len > 2) || (chip_addr_len < 1))
	{
		LOG_BUG(_GETTEXT("chip_addr_len can only be 1 or 2\n"));
		return ERROR_FAIL;
	}
	
	usbtoi2c_buffer[0] = (chip_addr >> 0) & 0xFF;
	usbtoi2c_buffer[1] = (chip_addr >> 8) & 0xFF;
	usbtoi2c_buffer[2] = chip_addr_len;
	usbtoi2c_buffer[3] = (data_len >> 0) & 0xFF;
	usbtoi2c_buffer[4] = (data_len >> 8) & 0xFF;
	usbtoi2c_buffer[5] = stop;
	memset(&usbtoi2c_buffer[6], 0, data_len);
	
	return usbtoxxx_in_command(USB_TO_I2C, interface_index, usbtoi2c_buffer, 
							   data_len + 6, data_len, data, 0, data_len, 0);
}

RESULT usbtoi2c_write(uint8 interface_index, uint16 chip_addr, 
					  uint8 chip_addr_len, uint8 *data, 
					  uint16 data_len, uint8 stop)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	if (data_len > 256)
	{
		LOG_BUG(_GETTEXT("too many data to transfer on I2C\n"));
		return ERROR_FAIL;
	}
	if ((chip_addr_len > 2) || (chip_addr_len < 1))
	{
		LOG_BUG(_GETTEXT("chip_addr_len can only be 1 or 2\n"));
		return ERROR_FAIL;
	}
	
	usbtoi2c_buffer[0] = (chip_addr >> 0) & 0xFF;
	usbtoi2c_buffer[1] = (chip_addr >> 8) & 0xFF;
	usbtoi2c_buffer[2] = chip_addr_len;
	usbtoi2c_buffer[3] = (data_len >> 0) & 0xFF;
	usbtoi2c_buffer[4] = (data_len >> 8) & 0xFF;
	usbtoi2c_buffer[5] = stop;
	memcpy(&usbtoi2c_buffer[6], data, data_len);
	
	return usbtoxxx_out_command(USB_TO_I2C, interface_index, usbtoi2c_buffer, 
								data_len + 6, 0);
}

