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

uint8 usbtomsp430jtag_num_of_interface = 0;

RESULT usbtomsp430jtag_init(void)
{
	return usbtoxxx_init_command(USB_TO_MSP430_JTAG, 
								 &usbtomsp430jtag_num_of_interface);
}

RESULT usbtomsp430jtag_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_MSP430_JTAG);
}

RESULT usbtomsp430jtag_config(uint8 interface_index, uint8 has_test)
{
	return usbtoxxx_conf_command(USB_TO_MSP430_JTAG, interface_index, 
								 &has_test, 1);
}

RESULT usbtomsp430jtag_ir(uint8 interface_index, uint8 *ir, uint8 want_ret)
{
	uint8 buff[2];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	buff[0] = 8;
	buff[1] = *ir;
	
	if (want_ret)
	{
		return usbtoxxx_inout_command(USB_TO_MSP430_JTAG, interface_index, 
									  buff, 2, 1, ir, 0, 1, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_MSP430_JTAG, interface_index, 
									  buff, 2, 1, NULL, 0, 0, 1);
	}
}

RESULT usbtomsp430jtag_dr(uint8 interface_index, uint32 *dr, uint8 len, 
						  uint8 want_ret)
{
	uint8 buff[5], byte_len = (len + 7) >> 3;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	buff[0] = len | 0x80;
	memcpy(buff + 1, dr, byte_len);
	
	if (want_ret)
	{
		return usbtoxxx_inout_command(USB_TO_MSP430_JTAG, interface_index, 
									  buff, byte_len + 1, byte_len, 
									  (uint8*)dr, 0, byte_len, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_MSP430_JTAG, interface_index, 
									  buff, byte_len + 1, byte_len, 
									  NULL, 0, 0, 1);
	}
}

RESULT usbtomsp430jtag_tclk(uint8 interface_index, uint8 value)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_out_command(USB_TO_MSP430_JTAG, interface_index, &value, 
								1, 0);
}

RESULT usbtomsp430jtag_tclk_strobe(uint8 interface_index, uint16 cnt)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_special_command(USB_TO_MSP430_JTAG, interface_index, 
									(uint8*)&cnt, 2, 0, NULL, 0, 0, 0);
}

RESULT usbtomsp430jtag_reset(uint8 interface_index)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_reset_command(USB_TO_MSP430_JTAG, interface_index, 
								  NULL, 0);
}

RESULT usbtomsp430jtag_poll(uint8 interface_index, uint32 dr, uint32 mask, 
							uint32 value, uint8 len, uint16 poll_cnt, 
							uint8 toggle_tclk)
{
	uint8 buff[15];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	buff[0] = len;
	if (toggle_tclk)
	{
		poll_cnt |= 0x8000;
	}
	memcpy(buff + 1, (uint8*)&poll_cnt, 2);
	memcpy(buff + 3, (uint8*)&dr, 4);
	memcpy(buff + 7, (uint8*)&mask, 4);
	memcpy(buff + 11, (uint8*)&value, 4);
	
	return usbtoxxx_poll_command(USB_TO_MSP430_JTAG, interface_index, 
								 buff, 15);
}

