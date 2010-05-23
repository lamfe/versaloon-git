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

#include "programmer.h"
#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

uint8_t usbtousart_num_of_interface;

RESULT usbtousart_init(void)
{
	return usbtoxxx_init_command(USB_TO_USART, &usbtousart_num_of_interface);
}

RESULT usbtousart_fini(void)
{
	return usbtoxxx_fini_command(USB_TO_USART);
}

RESULT usbtousart_config(uint8_t interface_index, uint32_t baudrate, 
			uint8_t datalength, char paritybit, char stopbit, char handshake)
{
	uint8_t conf[8];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	conf[0] = (baudrate >> 0) & 0xFF;
	conf[1] = (baudrate >> 8) & 0xFF;
	conf[2] = (baudrate >> 16) & 0xFF;
	conf[3] = (baudrate >> 24) & 0xFF;
	conf[4] = datalength;
	conf[5] = paritybit;
	conf[6] = stopbit;
	conf[7] = handshake;
	
	return usbtoxxx_conf_command(USB_TO_GPIO, interface_index, conf, 8);
}

RESULT usbtousart_send(uint8_t interface_index, uint8_t *buf, uint16_t len)
{
#if PARAM_CHECK
	if ((interface_index > 7) || (0 == len) || (NULL == buf))
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	versaloon_cmd_buf[0] = (len >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (len >> 8) & 0xFF;
	memset(&versaloon_cmd_buf[2], 0, len);
	
	return usbtoxxx_in_command(USB_TO_USART, interface_index, 
				versaloon_cmd_buf, 2 + len, len, buf, 0, len, 1);
}

RESULT usbtousart_receive(uint8_t interface_index, uint8_t *buf, uint16_t len)
{
#if PARAM_CHECK
	if ((interface_index > 7) || (0 == len) || (NULL == buf))
	{
		LOG_BUG(_GETTEXT("invalid inteface_index %d.\n"), interface_index);
		return ERROR_FAIL;
	}
#endif
	
	versaloon_cmd_buf[0] = (len >> 0) & 0xFF;
	versaloon_cmd_buf[1] = (len >> 8) & 0xFF;
	memcpy(&versaloon_cmd_buf[2], buf, len);
	
	return usbtoxxx_out_command(USB_TO_USART, interface_index, 
										versaloon_cmd_buf, 2 + len, 1);
}

