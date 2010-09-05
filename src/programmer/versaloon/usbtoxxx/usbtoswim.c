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

uint8_t usbtoswim_num_of_interface = 0;

RESULT usbtoswim_init(void)
{
	return usbtoxxx_init_command(USB_TO_SWIM, &usbtoswim_num_of_interface);
}

RESULT usbtoswim_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_SWIM);
}

RESULT usbtoswim_config(uint8_t interface_index, uint8_t mHz, uint8_t cnt0, 
						uint8_t cnt1)
{
	uint8_t buff[3];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	buff[0] = mHz;
	buff[1] = cnt0;
	buff[2] = cnt1;
	
	return usbtoxxx_conf_command(USB_TO_SWIM, interface_index, buff, 3);
}

RESULT usbtoswim_srst(uint8_t interface_index)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_reset_command(USB_TO_SWIM, interface_index, NULL, 0);
}

RESULT usbtoswim_wotf(uint8_t interface_index, uint8_t *data, uint16_t bytelen, 
						uint32_t addr)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = (bytelen >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (bytelen >> 8) & 0xFF;
	versaloon_cmd_buf[2] = (addr >> 0) & 0xFF;
	versaloon_cmd_buf[3] = (addr >> 8) & 0xFF;
	versaloon_cmd_buf[4] = (addr >> 16) & 0xFF;
	versaloon_cmd_buf[5] = 0;
	memcpy(&versaloon_cmd_buf[6], data, bytelen);
	
	return usbtoxxx_out_command(USB_TO_SWIM, interface_index, 
									versaloon_cmd_buf, bytelen + 6, 0);
}

RESULT usbtoswim_rotf(uint8_t interface_index, uint8_t *data, uint16_t bytelen, 
						uint32_t addr)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = (bytelen >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (bytelen >> 8) & 0xFF;
	versaloon_cmd_buf[2] = (addr >> 0) & 0xFF;
	versaloon_cmd_buf[3] = (addr >> 8) & 0xFF;
	versaloon_cmd_buf[4] = (addr >> 16) & 0xFF;
	versaloon_cmd_buf[5] = 0;
	memset(&versaloon_cmd_buf[5], 0, bytelen);
	
	return usbtoxxx_in_command(USB_TO_SWIM, interface_index, 
				versaloon_cmd_buf, bytelen + 6, bytelen, data, 0, bytelen, 0);
}

RESULT usbtoswim_sync(uint8_t interface_index, uint8_t mHz)
{
	uint8_t buff[1];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	buff[0] = mHz;
	
	return usbtoxxx_sync_command(USB_TO_SWIM, interface_index, buff, 1, 
									0, NULL);
}

RESULT usbtoswim_enable(uint8_t interface_index)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	return usbtoxxx_enable_command(USB_TO_SWIM, interface_index, NULL, 0);
}

