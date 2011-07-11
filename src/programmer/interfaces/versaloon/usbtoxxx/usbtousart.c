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

#include <string.h>

#include "../versaloon_include.h"
#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

RESULT usbtousart_init(uint8_t interface_index)
{
	return usbtoxxx_init_command(USB_TO_USART, interface_index);
}

RESULT usbtousart_fini(uint8_t interface_index)
{
	return usbtoxxx_fini_command(USB_TO_USART, interface_index);
}

RESULT usbtousart_config(uint8_t interface_index, uint32_t baudrate,
							uint8_t datalength, uint8_t mode)
{
	uint8_t conf[6];
	
#if PARAM_CHECK
	if (interface_index > 7)
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	SET_LE_U32(&conf[0], baudrate);
	conf[4] = datalength;
	conf[5] = mode;
	
	return usbtoxxx_conf_command(USB_TO_USART, interface_index, conf, 6);
}

RESULT usbtousart_receive(uint8_t interface_index, uint8_t *buf, uint16_t len)
{
#if PARAM_CHECK
	if ((interface_index > 7) || (0 == len) || (NULL == buf))
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	SET_LE_U16(&versaloon_cmd_buf[0], len);
	memset(&versaloon_cmd_buf[2], 0, len);
	
	return usbtoxxx_in_command(USB_TO_USART, interface_index,
				versaloon_cmd_buf, 2 + len, len, buf, 0, len, 1);
}

RESULT usbtousart_send(uint8_t interface_index, uint8_t *buf, uint16_t len)
{
#if PARAM_CHECK
	if ((interface_index > 7) || (0 == len) || (NULL == buf))
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	SET_LE_U16(&versaloon_cmd_buf[0], len);
	memcpy(&versaloon_cmd_buf[2], buf, len);
	
	return usbtoxxx_out_command(USB_TO_USART, interface_index,
										versaloon_cmd_buf, 2 + len, 1);
}

RESULT usbtousart_status(uint8_t interface_index,
							struct usart_status_t *status)
{
#if PARAM_CHECK
	if ((interface_index > 7) || (NULL == status))
	{
		LOG_BUG(ERRMSG_INVALID_INTERFACE_NUM, interface_index);
		return ERROR_FAIL;
	}
#endif
	
	return usbtoxxx_status_command(USB_TO_USART, interface_index,
			sizeof(struct usart_status_t), (uint8_t *)status, 0,
			sizeof(struct usart_status_t), 0);
}
