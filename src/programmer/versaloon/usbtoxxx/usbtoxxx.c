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

#include "programmer.h"
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
											"usbtolpcicp", "usbtoswj", N_A, 
N_A, N_A, N_A, N_A, N_A, N_A, N_A, N_A, 
N_A, N_A, N_A, N_A, N_A, N_A, N_A, N_A, 
"usbtomsp430jtag", N_A, N_A, N_A, N_A, N_A, N_A, N_A, 
"usbtopower", "usbtodelay", "usbtopoll", N_A, N_A, N_A, N_A, N_A, 
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
static uint8_t poll_nesting = 0;

struct usbtoxxx_context_t
{
	uint8_t type_pre;
	uint8_t *usbtoxxx_buffer;
	uint16_t usbtoxxx_current_cmd_index;
	uint16_t usbtoxxx_buffer_index;
	uint16_t versaloon_pending_idx;
};
static struct usbtoxxx_context_t poll_context;

static void usbtoxxx_save_context(struct usbtoxxx_context_t *c)
{
	c->type_pre = type_pre;
	c->usbtoxxx_buffer = usbtoxxx_buffer;
	c->usbtoxxx_buffer_index = usbtoxxx_buffer_index;
	c->usbtoxxx_current_cmd_index = usbtoxxx_current_cmd_index;
	c->versaloon_pending_idx = versaloon_pending_idx;
}

static void usbtoxxx_pop_context(struct usbtoxxx_context_t *c)
{
	type_pre = c->type_pre;
	usbtoxxx_buffer = c->usbtoxxx_buffer;
	usbtoxxx_buffer_index = c->usbtoxxx_buffer_index;
	usbtoxxx_current_cmd_index = c->usbtoxxx_current_cmd_index;
	versaloon_pending_idx = c->versaloon_pending_idx;
}

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
	uint8_t processed;
	uint8_t result = ERROR_OK;
	
	if (poll_nesting)
	{
		LOG_BUG(_GETTEXT("Invalid use of USB_TO_POLL.\n"));
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				"validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (3 == usbtoxxx_buffer_index)
	{
		return ERROR_OK;
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
				result = ERROR_FAIL;
				break;
			}
			else if (USB_TO_XXX_OK != versaloon_buf[usbtoxxx_buffer_index])
			{
				LOG_ERROR(_GETTEXT("%s command 0x%02x failed with 0x%02x\n"), 
					usbtoxxx_get_type_name(versaloon_pending[i].type), 
					versaloon_pending[i].cmd, 
					versaloon_buf[usbtoxxx_buffer_index]);
				result = ERROR_FAIL;
				break;
			}
			usbtoxxx_buffer_index++;
		}
		
		// get result data
		processed = 0;
		if (versaloon_pending[i].callback != NULL)
		{
			versaloon_pending[i].callback(&versaloon_pending[i], 
						versaloon_buf + usbtoxxx_buffer_index, &processed);
		}
		if (!processed && (versaloon_pending[i].want_data_size > 0) 
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
			break;
		}
	}
	
	// data is not the right size
	if (inlen != usbtoxxx_buffer_index)
	{
		LOG_ERROR(_GETTEXT("length of return data invalid\n"));
		result = ERROR_FAIL;
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



RESULT usbtoxxx_ensure_buffer_size(uint16_t cmdlen)
{
	// check free space, commit if not enough
	if (((usbtoxxx_buffer_index + usbtoxxx_current_cmd_index + cmdlen) 
			>= versaloon_buf_size)
		|| (versaloon_pending_idx >= VERSALOON_MAX_PENDING_NUMBER))
	{
		struct usbtoxxx_context_t context_tmp;
		uint8_t poll_nesting_tmp = 0;
		
		memset(&context_tmp, 0, sizeof(context_tmp));
		if (poll_nesting)
		{
			if (0 == poll_context.type_pre)
			{
				LOG_BUG(_GETTEXT("USB_TO_POLL toooooo long\n"));
				return ERROR_OK;
			}
			
			usbtoxxx_save_context(&context_tmp);
			usbtoxxx_pop_context(&poll_context);
			poll_nesting_tmp = poll_nesting;
			poll_nesting = 0;
		}
		
		if (usbtoxxx_execute_command() != ERROR_OK)
		{
			return ERROR_FAIL;
		}
		
		if (poll_nesting_tmp)
		{
			uint16_t newlen, oldlen;
			
			newlen = context_tmp.versaloon_pending_idx 
									- poll_context.versaloon_pending_idx;
			memcpy(&versaloon_pending[0], 
					&versaloon_pending[poll_context.versaloon_pending_idx], 
					sizeof(versaloon_pending[0]) * newlen);
			context_tmp.versaloon_pending_idx = newlen;
			oldlen = poll_context.usbtoxxx_buffer_index 
									+ poll_context.usbtoxxx_current_cmd_index;
			newlen = context_tmp.usbtoxxx_buffer_index
									+ context_tmp.usbtoxxx_current_cmd_index;
			memcpy(versaloon_buf + 3, versaloon_buf + oldlen, newlen - oldlen);
			oldlen -= 3;
			context_tmp.usbtoxxx_buffer -= oldlen;
			context_tmp.usbtoxxx_buffer_index -= oldlen;
			usbtoxxx_pop_context(&context_tmp);
			poll_nesting = poll_nesting_tmp;
		}
	}
	return ERROR_OK;
}

RESULT usbtoxxx_add_command(uint8_t type, uint8_t cmd, uint8_t *cmdbuf, 
							uint16_t cmdlen, uint16_t retlen, uint8_t *wantbuf, 
							uint16_t wantpos, uint16_t wantlen, uint8_t collect)
{
	uint16_t len_tmp;
	
	// 3 more bytes by usbtoxxx_validate_current_command_type
	// 3 more bytes when ((0 == collect_index) || (collect_cmd != cmd))
	if (ERROR_OK != usbtoxxx_ensure_buffer_size(cmdlen + 6))
	{
		return ERROR_FAIL;
	}
	
	if ((type_pre != type) || (NULL == usbtoxxx_buffer))
	{
		if (ERROR_OK != usbtoxxx_validate_current_command_type())
		{
			LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				"validate previous commands");
			return ERRCODE_FAILURE_OPERATION;
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
								 wantbuf, collect);
}





RESULT usbtopoll_start(uint16_t retry, uint16_t interval_us)
{
	if (ERROR_OK != usbtoxxx_ensure_buffer_size(3 + 5))
	{
		return ERROR_FAIL;
	}
	if (!poll_nesting)
	{
		usbtoxxx_save_context(&poll_context);
	}
	
	if (ERROR_OK != usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				"validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	poll_nesting++;
	type_pre = USB_TO_POLL;
	
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = USB_TO_POLL_START;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (retry >> 0) & 0xFF;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (retry >> 8) & 0xFF;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (interval_us >> 0) & 0xFF;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (interval_us >> 8) & 0xFF;
	
	return versaloon_add_pending(USB_TO_POLL, 0, 0, 0, 0, NULL, 0);
}

RESULT usbtopoll_end(void)
{
	if (!poll_nesting)
	{
		LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "check poll nesting");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (ERROR_OK != usbtoxxx_ensure_buffer_size(3 + 1))
	{
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				"validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	poll_nesting--;
	type_pre = USB_TO_POLL;
	
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = USB_TO_POLL_END;
	
	return versaloon_add_pending(USB_TO_POLL, 0, 0, 0, 0, NULL, 0);
}

RESULT usbtopoll_checkbyte(uint8_t offset, uint8_t mask, uint8_t value)
{
	if (!poll_nesting)
	{
		LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "check poll nesting");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (ERROR_OK != usbtoxxx_ensure_buffer_size(3 + 4))
	{
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				"validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	type_pre = USB_TO_POLL;
	
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = USB_TO_POLL_CHECKBYTE;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = offset;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = mask;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = value;
	
	return ERROR_OK;
}

RESULT usbtopoll_checkfail(uint8_t offset, uint8_t mask, uint8_t value)
{
	if (!poll_nesting)
	{
		LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), "check poll nesting");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (ERROR_OK != usbtoxxx_ensure_buffer_size(3 + 4))
	{
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				"validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	type_pre = USB_TO_POLL;
	
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = USB_TO_POLL_CHECKFAIL;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = offset;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = mask;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = value;
	
	return ERROR_OK;
}




RESULT usbtodelay_delay(uint16_t dly)
{
	if (ERROR_OK != usbtoxxx_ensure_buffer_size(3 + 2))
	{
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != usbtoxxx_validate_current_command_type())
	{
		LOG_BUG(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					"validate previous commands");
		return ERRCODE_FAILURE_OPERATION;
	}
	type_pre = USB_TO_DELAY;
	
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (dly >> 0) & 0xFF;
	usbtoxxx_buffer[usbtoxxx_current_cmd_index++] = (dly >> 8) & 0xFF;
	
	return versaloon_add_pending(USB_TO_DELAY, 0, 0, 0, 0, NULL, 0);
}

