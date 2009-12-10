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

uint8_t usbtojtaghl_num_of_interface = 0;

jtag_callback_t usbtojtaghl_receive_callback = NULL;
jtag_callback_t usbtojtaghl_send_callback = NULL;
uint32_t usbtojtaghl_ir_backup = 0;

RESULT usbtojtaghl_callback(void *p, uint8_t *src, uint8_t *processed)
{
	versaloon_pending_t *cur_pending = (versaloon_pending_t *)p;
	uint16_t processed_len = 0;
	RESULT ret;
	
	if (NULL == usbtojtaghl_receive_callback)
	{
		return ERROR_OK;
	}
	
	if (cur_pending->id & 0x80000000)
	{
		// DR
		ret = usbtojtaghl_receive_callback(JTAG_SCANTYPE_DR, 
									cur_pending->id & 0x7FFFFFFF, 
									cur_pending->data_buffer, 
									src, 
									cur_pending->actual_data_size, 
									&processed_len);
	}
	else
	{
		// IR
		ret = usbtojtaghl_receive_callback(JTAG_SCANTYPE_IR, 
									cur_pending->id & 0x7FFFFFFF, 
									cur_pending->data_buffer, 
									src, 
									cur_pending->actual_data_size, 
									&processed_len);
	}
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					"call callback");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (processed_len)
	{
		*processed = 1;
	}
	
	return ERROR_OK;
}

RESULT usbtojtaghl_register_callback(jtag_callback_t send_callback, 
									 jtag_callback_t receive_callback)
{
	usbtojtaghl_send_callback = send_callback;
	usbtojtaghl_receive_callback = receive_callback;
	return ERROR_OK;
}

RESULT usbtojtaghl_init(void)
{
	return usbtoxxx_init_command(USB_TO_JTAG_HL, 
								 &usbtojtaghl_num_of_interface);
}

RESULT usbtojtaghl_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_JTAG_HL);
}

RESULT usbtojtaghl_config(uint8_t interface_index, uint16_t kHz, uint8_t ub, 
						  uint8_t ua, uint16_t bb, uint16_t ba)
{
	uint8_t cfg_buf[8];
	
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

RESULT usbtojtaghl_ir(uint8_t interface_index, uint8_t *ir, uint16_t bitlen, 
					  uint8_t idle, uint8_t want_ret)
{
	uint16_t bytelen = (bitlen + 7) >> 3;
	uint16_t processed_len = 0;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	usbtojtaghl_ir_backup = 0;
	if (bytelen > 4)
	{
		memcpy(&usbtojtaghl_ir_backup, ir, 4);
	}
	else
	{
		memcpy(&usbtojtaghl_ir_backup, ir, bytelen);
	}
	
	bitlen |= 0x8000;		// indicate ir
	versaloon_cmd_buf[0] = (bitlen >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (bitlen >> 8) & 0xFF;
	versaloon_cmd_buf[2] = idle;
	
	if (usbtojtaghl_send_callback != NULL)
	{
		usbtojtaghl_send_callback(JTAG_SCANTYPE_IR, usbtojtaghl_ir_backup, 
									versaloon_cmd_buf + 3, ir, bytelen, 
									&processed_len);
	}
	
	if (processed_len)
	{
		bytelen = processed_len;
	}
	else
	{
		memcpy(versaloon_cmd_buf + 3, ir, bytelen);
	}
	
	// clear MSB to indicate IR
	versaloon_set_pending_id(usbtojtaghl_ir_backup & 0x7FFFFFFF);
	versaloon_set_callback(usbtojtaghl_callback);
	if (want_ret)
	{
		return usbtoxxx_inout_command(USB_TO_JTAG_HL, interface_index, 
									  versaloon_cmd_buf, bytelen + 3, 
									  bytelen, ir, 0, bytelen, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_JTAG_HL, interface_index, 
									  versaloon_cmd_buf, bytelen + 3, 
									  bytelen, NULL, 0, 0, 1);
	}
}

RESULT usbtojtaghl_dr(uint8_t interface_index, uint8_t *dr, uint16_t bitlen, 
					  uint8_t idle, uint8_t want_ret)
{
	uint16_t bytelen = (bitlen + 7) >> 3;
	uint16_t processed_len = 0;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = (bitlen >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (bitlen >> 8) & 0xFF;
	versaloon_cmd_buf[2] = idle;
	
	if (usbtojtaghl_send_callback != NULL)
	{
		usbtojtaghl_send_callback(JTAG_SCANTYPE_DR, usbtojtaghl_ir_backup, 
									versaloon_cmd_buf + 3, dr, bytelen, 
									&processed_len);
	}
	
	if (processed_len)
	{
		bytelen = processed_len;
	}
	else
	{
		memcpy(versaloon_cmd_buf + 3, dr, bytelen);
	}
	
	// set MSB to indicate DR
	versaloon_set_pending_id(usbtojtaghl_ir_backup | 0x80000000);
	versaloon_set_callback(usbtojtaghl_callback);
	if (want_ret)
	{
		return usbtoxxx_inout_command(USB_TO_JTAG_HL, interface_index, 
									  versaloon_cmd_buf, bytelen + 3, 
									  bytelen, dr, 0, bytelen, 1);
	}
	else
	{
		return usbtoxxx_inout_command(USB_TO_JTAG_HL, interface_index, 
									  versaloon_cmd_buf, bytelen + 3, 
									  bytelen, NULL, 0, 0, 1);
	}
}

RESULT usbtojtaghl_tms(uint8_t interface_index, uint8_t *tms, uint16_t bitlen)
{
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
	if (bitlen > 256)
	{
		LOG_BUG(_GETTEXT("bitlen is 256 in max\n"));
		return ERROR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = (uint8_t)(bitlen - 1);
	memcpy(versaloon_cmd_buf + 1, tms, bitlen);
	
	return usbtoxxx_out_command(USB_TO_JTAG_HL, interface_index, 
							versaloon_cmd_buf, ((bitlen + 7) >> 3) + 1, 0);
}

RESULT usbtojtaghl_runtest(uint8_t interface_index, uint32_t cycles)
{
	uint8_t tms[256 / 8];
	uint16_t cur_cycles;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	memset(tms, 0, sizeof(tms));
	while (cycles > 0)
	{
		if (cycles > 256)
		{
			cur_cycles = 256;
		}
		else
		{
			cur_cycles = (uint8_t)cycles;
		}
		
		if (ERROR_OK != usbtojtaghl_tms(interface_index, tms, cur_cycles))
		{
			return ERROR_FAIL;
		}
		
		cycles -= cur_cycles;
	}
	return ERROR_OK;
}

