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

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"

#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "usbtoxxx.h"
#include "usbtoxxx_internal.h"

#define N_A		"n/a"
const char* types_name[96] = 
{
"usbtousart", "usbtospi", "usbtoi2c", "usbtogpio", "usbtocan", "usbtopwm",
													"usbtoadc", "usbtodac", 
N_A, N_A, N_A, N_A, N_A, N_A, "usbtopower", "usbtodelay", 
N_A, N_A, N_A, N_A, N_A, N_A, N_A, N_A, N_A, 
N_A, N_A, N_A, N_A, N_A, N_A, N_A, 
"usbtojtagll", "usbtojtaghl", "usbtoissp", "usbtoc2", "usbtosbw", 
													"usbtolpcicp", N_A, N_A, 
N_A, N_A, N_A, N_A, N_A, N_A, N_A, N_A, 
N_A, N_A, N_A, N_A, N_A, N_A, N_A, N_A, 
"usbtomsp430jtag", N_A, N_A, N_A, N_A, N_A, N_A, N_A, 
"usbtopower", "usbtodelay", N_A, N_A, N_A, N_A, N_A, N_A, 
N_A, N_A, N_A, N_A, N_A, N_A, N_A, "usbtoall"
};

#define usbtoxxx_get_type_name(type)	\
			types_name[((type) - VERSALOON_USB_TO_XXX_CMD_START) \
					   % (sizeof(types_name) / sizeof(types_name[0]))]

static uint8_t type_pre = 0;
static uint16_t usbtoxxx_buffer_index = 0;
static uint16_t usbtoxxx_current_cmd_index = 0;
static uint8_t *usbtoxxx_buffer = NULL;
uint8_t null_char = 0;

uint16_t collect_index = 0;
uint8_t collect_cmd;

RESULT usbtoxxx_validate_current_command_type(void)
{
	if (type_pre > 0)
	{
		// not the first command
		if (NULL == usbtoxxx_buffer)
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_BUFFER), TO_STR(usbtoxxx_buffer));
			return ERRCODE_INVALID_BUFFER;
		}
		
		usbtoxxx_buffer[0] = type_pre;
		usbtoxxx_buffer[1] = (usbtoxxx_current_cmd_index >> 0) & 0xFF;
		usbtoxxx_buffer[2] = (usbtoxxx_current_cmd_index >> 8) & 0xFF;
		
		usbtoxxx_buffer_index += usbtoxxx_current_cmd_index;
	}
	else
	{
		// first command
		usbtoxxx_buffer_index = 3;
	}
	
	// prepare for next command
	usbtoxxx_current_cmd_index = 3;
	usbtoxxx_buffer = versaloon_buf + usbtoxxx_buffer_index;
	
	collect_index = 0;
	collect_cmd = 0;
	
	return ERROR_OK;
}



RESULT usbtoxxx_execute_command(void)
{
	uint16_t i;
	uint16_t inlen;
	uint8_t result = ERROR_OK;
	
	if (ERROR_OK != usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(_GETTEXT("Fail to validate previous commands?\n"));
		return ERROR_FAIL;
	}
	
	versaloon_buf[0] = USB_TO_ALL;
	versaloon_buf[1] = (usbtoxxx_buffer_index >> 0) & 0xFF;
	versaloon_buf[2] = (usbtoxxx_buffer_index >> 8) & 0xFF;
	
	if (ERROR_OK != versaloon_send_command(usbtoxxx_buffer_index, &inlen))
	{
		return ERROR_FAIL;
	}
	
	// process return data
	usbtoxxx_buffer_index = 0;
	for (i = 0; i < versaloon_pending_idx; i++)
	{
		// check result
		if ((0 == i) || !((versaloon_pending[i].collect) 
							&& (versaloon_pending[i - 1].collect) 
							&& (versaloon_pending[i].cmd 
								== versaloon_pending[i - 1].cmd)))
		{
			if (USB_TO_XXX_CMD_NOT_SUPPORT 
				== versaloon_buf[usbtoxxx_buffer_index])
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), 
						  usbtoxxx_get_type_name(versaloon_pending[i].type), 
						  "current dongle");
				return ERROR_FAIL;
			}
			else if (USB_TO_XXX_OK != versaloon_buf[usbtoxxx_buffer_index])
			{
				LOG_ERROR(_GETTEXT("%s command 0x%02x failed with 0x%02x\n"), 
					usbtoxxx_get_type_name(versaloon_pending[i].type), 
					versaloon_pending[i].cmd, 
					versaloon_buf[usbtoxxx_buffer_index]);
				result = ERROR_FAIL;
			}
			usbtoxxx_buffer_index++;
		}
		
		// get result data
		if ((versaloon_pending[i].want_data_size > 0) 
			&& (versaloon_pending[i].data_buffer != NULL))
		{
			memcpy(versaloon_pending[i].data_buffer, 
				   versaloon_buf + usbtoxxx_buffer_index 
								 + versaloon_pending[i].want_data_pos, 
				   versaloon_pending[i].want_data_size);
		}
		usbtoxxx_buffer_index += versaloon_pending[i].actual_data_size;
		if (usbtoxxx_buffer_index > inlen)
		{
			LOG_BUG(_GETTEXT("%s command 0x%02x process error\n"), 
					usbtoxxx_get_type_name(versaloon_pending[i].type), 
					versaloon_pending[i].cmd);
			result = ERROR_FAIL;
		}
	}
	
	// data is not the right size
	if (inlen != usbtoxxx_buffer_index)
	{
		LOG_ERROR(_GETTEXT("length of return data invalid\n"));
		return ERROR_FAIL;
	}
	
	if (versaloon_pending_idx > 0)
	{
		versaloon_pending_idx = 0;
	}
	else
	{
		// no receive data, avoid collision
		sleep_ms(10);
	}
	
	type_pre = 0;
	collect_cmd = 0;
	collect_index = 0;
	
	return result;
}

RESULT usbtoxxx_init(void)
{
	versaloon_pending_idx = 0;
	return ERROR_OK;
}

void usbtoxxx_fini(void)
{
	usbtoxxx_buffer = NULL;
	type_pre = 0;
}



RESULT usbtoxxx_add_command(uint8_t type, uint8_t cmd, uint8_t *cmdbuf, 
							uint16_t cmdlen, uint16_t retlen, uint8_t *wantbuf, 
							uint16_t wantpos, uint16_t wantlen, uint8_t collect)
{
	uint16_t len_tmp;
	
	// check free space, commit if not enough
	if ((3 + usbtoxxx_buffer_index + usbtoxxx_current_cmd_index + 3 + cmdlen) 
		>= versaloon_buf_size)
	{
		if (usbtoxxx_execute_command() != ERROR_OK)
		{
			return ERROR_FAIL;
		}
	}
	
	if ((type_pre != type) || (NULL == usbtoxxx_buffer))
	{
		if (ERROR_OK != usbtoxxx_validate_current_command_type())
		{
			LOG_BUG(_GETTEXT("Fail to validate previous commands?\n"));
			return ERROR_FAIL;
		}
		type_pre = type;
	}
	
	if ((0 == collect_index) || (collect_cmd != cmd))
	{
		usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = cmd;
		
		if (collect)
		{
			collect_index = usbtoxxx_current_cmd_index;
			collect_cmd = cmd;
		}
		else
		{
			collect_index = 0;
			collect_cmd = 0;
		}
		usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (cmdlen >> 0) & 0xFF;
		usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (cmdlen >> 8) & 0xFF;
	}
	else
	{
		len_tmp = usbtoxxx_buffer[collect_index + 0] 
				  + (usbtoxxx_buffer[collect_index + 1] << 8) + cmdlen;
		usbtoxxx_buffer[collect_index + 0] = (len_tmp >> 0) & 0xFF;
		usbtoxxx_buffer[collect_index + 1] = (len_tmp >> 8) & 0xFF;
	}
	
	if (cmdbuf != NULL)
	{
		memcpy(usbtoxxx_buffer + usbtoxxx_current_cmd_index, cmdbuf, cmdlen);
		usbtoxxx_current_cmd_index += cmdlen;
	}
	
	return versaloon_add_pending(type, cmd, retlen, wantpos, wantlen, 
								 wantbuf, collect, 0);
}





RESULT usbtodelay_delay(uint16_t dly)
{
	if (ERROR_OK != usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(_GETTEXT("Fail to validate previous commands?\n"));
		return ERROR_FAIL;
	}
	type_pre = USB_TO_DELAY;
	
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (dly >> 0) & 0xFF;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (dly >> 8) & 0xFF;
	
	return versaloon_add_pending(USB_TO_DELAY, 0, 0, 0, 0, NULL, 0, 0);
}

