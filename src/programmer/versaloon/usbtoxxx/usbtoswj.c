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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "programmer.h"
#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

uint8_t usbtoswj_num_of_interface = 0;
uint8_t usbtoswj_last_ack = 0x01;		// SWJ_ACK_OK

RESULT usbtoswj_callback(void *p, uint8_t *src, uint8_t *processed)
{
	p = p;
	processed = processed;
	
	usbtoswj_last_ack = src[0];
	
	return ERROR_OK;
}

uint8_t usbtoswj_get_last_ack(void)
{
	return usbtoswj_last_ack;
}

RESULT usbtoswj_init(void)
{
	return usbtoxxx_init_command(USB_TO_SWJ, 
								 &usbtoswj_num_of_interface);
}

RESULT usbtoswj_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_SWJ);
}

RESULT usbtoswj_config(uint8_t interface_index, uint8_t trn, uint16_t retry, 
					   uint16_t dly)
{
	uint8_t cfg_buf[5];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	cfg_buf[0] = trn;
	cfg_buf[1] = (retry >> 0) & 0xFF;
	cfg_buf[2] = (retry >> 8) & 0xFF;
	cfg_buf[3] = (dly >> 0) & 0xFF;
	cfg_buf[4] = (dly >> 8) & 0xFF;
	
	return usbtoxxx_conf_command(USB_TO_SWJ, interface_index, cfg_buf, 5);
}

RESULT usbtoswj_seqout(uint8_t interface_index, uint8_t *data, uint16_t bitlen)
{
	uint16_t bytelen = (bitlen + 7) >> 3;
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = (bitlen >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (bitlen >> 8) & 0xFF;
	memcpy(versaloon_cmd_buf + 2, data, bytelen);
	
	return usbtoxxx_out_command(USB_TO_SWJ, interface_index, 
								versaloon_cmd_buf, bytelen + 2, 0);
}

RESULT usbtoswj_seqin(uint8_t interface_index, uint8_t *data, uint16_t bitlen)
{
	uint16_t bytelen = (bitlen + 7) >> 3;
	uint8_t buff[2];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	buff[0] = (bitlen >> 0) & 0xFF;
	buff[1] = (bitlen >> 8) & 0xFF;
	
	return usbtoxxx_in_command(USB_TO_SWJ, interface_index, buff, 2, bytelen, 
								data, 0, bytelen, 0);
}

RESULT usbtoswj_transact(uint8_t interface_index, uint8_t request, uint32_t *data)
{
	uint8_t parity;
	uint8_t buff[5];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	parity = (request >> 1) & 1;
	parity += (request >> 2) & 1;
	parity += (request >> 3) & 1;
	parity += (request >> 4) & 1;
	parity &= 1;
	buff[0] = (request | 0x81 | (parity << 5)) & ~0x40;
	memcpy(buff + 1, (uint8_t*)data, 4);
	
	versaloon_set_callback(usbtoswj_callback);
	if (request & 0x04)
	{
		// read
		return usbtoxxx_inout_command(USB_TO_SWJ, interface_index, buff, 5, 5, 
										(uint8_t *)data, 1, 4, 0);
	}
	else
	{
		// write
		return usbtoxxx_inout_command(USB_TO_SWJ, interface_index, buff, 5, 5, 
										NULL, 0, 0, 0);
	}
}

