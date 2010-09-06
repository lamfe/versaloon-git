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

uint8_t usbtojtagll_num_of_interface = 0;

RESULT usbtojtagll_init(void)
{
	return usbtoxxx_init_command(USB_TO_JTAG_LL, &usbtojtagll_num_of_interface);
}

RESULT usbtojtagll_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_JTAG_LL);
}

RESULT usbtojtagll_config(uint8_t interface_index, uint16_t kHz)
{
	uint8_t cfg_buf[2];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	SET_LE_U16(&cfg_buf[0], kHz);
	
	return usbtoxxx_conf_command(USB_TO_JTAG_LL, interface_index, cfg_buf, 2);
}

RESULT usbtojtagll_tms(uint8_t interface_index, uint8_t *tms, uint8_t bytelen)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	return usbtoxxx_out_command(USB_TO_JTAG_LL, interface_index, tms, 
									bytelen, 1);
}

RESULT usbtojtagll_tms_clocks(uint8_t interface_index, uint32_t bytelen, 
								uint8_t tms)
{
	uint8_t buff[5];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	if (tms)
	{
		buff[0] = 0xFF;
	}
	else
	{
		buff[0] = 0x00;
	}
	SET_LE_U32(&buff[1], bytelen);
	
	return usbtoxxx_poll_command(USB_TO_JTAG_LL, interface_index, buff, 
									5, NULL, 0);
}

RESULT usbtojtagll_scan(uint8_t interface_index, uint8_t* r, uint16_t bitlen, 
						uint8_t tms_before_valid, uint8_t tms_before, 
						uint8_t tms_after0, uint8_t tms_after1)
{
	uint16_t bytelen = (bitlen + 7) >> 3;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	if (tms_before_valid)
	{
		tms_before_valid = 1;
		bytelen |= 0x8000;
		versaloon_cmd_buf[2] = tms_before;
	}
	SET_LE_U16(&versaloon_cmd_buf[0], bytelen);
	bytelen &= 0x7FFF;
	
	memcpy(&versaloon_cmd_buf[2 + tms_before_valid], r, bytelen);
	versaloon_cmd_buf[2 + tms_before_valid + bytelen + 0] = tms_after0;
	versaloon_cmd_buf[2 + tms_before_valid + bytelen + 1] = tms_after1;
	
	return usbtoxxx_inout_command(USB_TO_JTAG_LL, interface_index, 
							versaloon_cmd_buf, bytelen + tms_before_valid + 4, 
							bytelen, r, 0, bytelen, 0);
}

