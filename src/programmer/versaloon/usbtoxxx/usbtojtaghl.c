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

uint8 usbtojtaghl_num_of_interface = 0;

// MAX size is 1(cmd) + 2(length) + data(length is 16-bit in bits, 8192 bytes)
uint8 usbtojtaghl_cmd_buf[8192 + 3];

RESULT usbtojtaghl_init(void)
{
	return usbtoxxx_init_command(USB_TO_JTAG_HL, 
								 &usbtojtaghl_num_of_interface);
}

RESULT usbtojtaghl_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_JTAG_HL);
}

RESULT usbtojtaghl_config(uint8 interface_index, uint16 kHz, uint8 ub, 
						  uint8 ua, uint16 bb, uint16 ba)
{
	uint8 cfg_buf[8];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	cfg_buf[0] = (kHz >> 0) & 0xFF;
	cfg_buf[1] = (kHz >> 8) & 0xFF;
	cfg_buf[2] = ub;
	cfg_buf[3] = ua;
	cfg_buf[4] = (bb >> 0) & 0xFF;
	cfg_buf[5] = (bb >> 8) & 0xFF;
	cfg_buf[6] = (ba >> 0) & 0xFF;
	cfg_buf[7] = (ba >> 8) & 0xFF;
	
	return usbtoxxx_conf_command(USB_TO_JTAG_HL, interface_index, cfg_buf, 8);
}

RESULT usbtojtaghl_ir(uint8 interface_index, uint8 *ir, uint16 bitlen, 
					  uint8 idle, uint8 want_ret)
{
	uint16 bytelen = (bitlen + 7) >> 3;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	bitlen |= 0x8000;		// indicate ir
	usbtojtaghl_cmd_buf[0] = (bitlen >> 0) & 0xFF;
	usbtojtaghl_cmd_buf[1] = (bitlen >> 8) & 0xFF;
	usbtojtaghl_cmd_buf[2] = idle;
	memcpy(usbtojtaghl_cmd_buf + 3, ir, bytelen);
	
	if (want_ret)
	{
		return usbtoxxx_inout_command(USB_TO_JTAG_HL, interface_index, 
									  usbtojtaghl_cmd_buf, bytelen + 3, 
									  bytelen, ir, 0, bytelen, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_JTAG_HL, interface_index, 
									  usbtojtaghl_cmd_buf, bytelen + 3, 
									  bytelen, NULL, 0, 0, 1);
	}
}

RESULT usbtojtaghl_dr(uint8 interface_index, uint8 *dr, uint16 bitlen, 
					  uint8 idle, uint8 want_ret)
{
	uint16 bytelen = (bitlen + 7) >> 3;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	usbtojtaghl_cmd_buf[0] = (bitlen >> 0) & 0xFF;
	usbtojtaghl_cmd_buf[1] = (bitlen >> 8) & 0xFF;
	usbtojtaghl_cmd_buf[2] = idle;
	memcpy(usbtojtaghl_cmd_buf + 3, dr, bytelen);
	
	if (want_ret)
	{
		return usbtoxxx_inout_command(USB_TO_JTAG_HL, interface_index, 
									  usbtojtaghl_cmd_buf, bytelen + 3, 
									  bytelen, dr, 0, bytelen, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_JTAG_HL, interface_index, 
									  usbtojtaghl_cmd_buf, bytelen + 3, 
									  bytelen, NULL, 0, 0, 1);
	}
}

RESULT usbtojtaghl_tmsbyte(uint8 interface_index, uint8 *tms, uint8 bytelen)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	usbtojtaghl_cmd_buf[0] = bytelen;
	memcpy(usbtojtaghl_cmd_buf + 1, tms, bytelen);
	
	return usbtoxxx_out_command(USB_TO_JTAG_HL, interface_index, 
								usbtojtaghl_cmd_buf, bytelen + 1, 0);
}

