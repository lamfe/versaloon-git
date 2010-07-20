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

#include <stdio.h>
#include <string.h>

#include "../versaloon_include.h"
#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

uint8_t usbtoi2c_num_of_interface = 0;

RESULT usbtoi2c_init(void)
{
	return usbtoxxx_init_command(USB_TO_I2C, &usbtoi2c_num_of_interface);
}

RESULT usbtoi2c_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_I2C);
}

RESULT usbtoi2c_config(uint8_t interface_index, uint16_t kHz, 
							uint16_t dead_cnt, uint16_t byte_interval)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = (kHz >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (kHz >> 8) & 0xFF;
	versaloon_cmd_buf[2] = (dead_cnt >> 0) & 0xFF;
	versaloon_cmd_buf[3] = (dead_cnt >> 8) & 0xFF;
	versaloon_cmd_buf[4] = (byte_interval >> 0) & 0xFF;
	versaloon_cmd_buf[5] = (byte_interval >> 8) & 0xFF;
	
	return usbtoxxx_conf_command(USB_TO_I2C, interface_index, 
								 versaloon_cmd_buf, 6);
}

RESULT usbtoi2c_read(uint8_t interface_index, uint16_t chip_addr, 
						uint8_t *data, uint16_t data_len, uint8_t stop)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	if (data_len > 256)
	{
		LOG_BUG(ERRMSG_INVALID_VALUE, data_len, "I2C data size(1..256)");
		return ERROR_FAIL;
	}
	
	versaloon_cmd_buf[0] = (chip_addr >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (data_len >> 0) & 0xFF;
	versaloon_cmd_buf[2] = (data_len >> 8) & 0xFF;
	versaloon_cmd_buf[3] = stop;
	memset(&versaloon_cmd_buf[4], 0, data_len);
	
	return usbtoxxx_in_command(USB_TO_I2C, interface_index, versaloon_cmd_buf, 
							   data_len + 4, data_len, data, 0, data_len, 0);
}

RESULT usbtoi2c_write(uint8_t interface_index, uint16_t chip_addr, 
						uint8_t *data, uint16_t data_len, uint8_t stop)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	if (data_len > 256)
	{
		LOG_BUG(ERRMSG_INVALID_VALUE, data_len, "I2C data size(1..256)");
		return ERROR_FAIL;
	}
	
	versaloon_cmd_buf[0] = (chip_addr >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (data_len >> 0) & 0xFF;
	versaloon_cmd_buf[2] = (data_len >> 8) & 0xFF;
	versaloon_cmd_buf[3] = stop;
	memcpy(&versaloon_cmd_buf[4], data, data_len);
	
	return usbtoxxx_out_command(USB_TO_I2C, interface_index, versaloon_cmd_buf, 
								data_len + 4, 0);
}

