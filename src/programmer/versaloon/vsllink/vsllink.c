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
#include <stdlib.h>
#include <string.h>

#include "port.h"

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "programmer.h"

#include "../versaloon.h"
#include "../versaloon_internal.h"
#include "vsllink.h"
#include "vsllink_internal.h"

static uint16 vsllink_buffer_index = 0;
static uint8 *buf_tmp = NULL;

RESULT vsllink_connect(uint8 mode)
{
	uint16 ret;
	
	if (buf_tmp != NULL)
	{
		LOG_BUG(_GETTEXT("unmatch vslllink_connect and vsllink_disconnect.\n"));
		return ERROR_FAIL;
	}
	
	buf_tmp = (uint8*)malloc(versaloon_buf_size);
	if (NULL == buf_tmp)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	vsllink_buffer_index = 0;
	
	versaloon_buf[0] = VSLLINK_CMD_CONN;
	versaloon_buf[1] = mode;
	
	if (ERROR_OK != versaloon_send_command(2, &ret) || (ret < 3))
	{
		return ERROR_FAIL;
	}
	else
	{
		versaloon_buf[ret] = 0;
		LOG_INFO("%s\n", versaloon_buf + 2);
		sleep_ms(10);
		
		return ERROR_OK;
	}
}

RESULT vsllink_disconnect(void)
{
	RESULT ret;
	
	versaloon_buf[0] = VSLLINK_CMD_DISCONN;
	
	ret = versaloon_send_command(1, NULL);
	sleep_ms(10);
	
	if (buf_tmp != NULL)
	{
		free(buf_tmp);
		buf_tmp = NULL;
	}
	
	return ret;
}

RESULT vsllink_jtag_connect(void)
{
	return vsllink_connect(VSLLINK_MODE_JTAG | VSLLINK_MODE_NORMAL);
}

RESULT vsllink_jtag_set_freq(uint16 kHz)
{
	if (vsllink_buffer_index > 0)
	{
		LOG_BUG(_GETTEXT("there is jtag_tap command uncommited\n"));
		return ERROR_FAIL;
	}
	
	versaloon_buf[0] = VSLLINK_CMD_SET_SPEED;
	versaloon_buf[1] = (kHz >> 0) & 0xFF;
	versaloon_buf[2] = (kHz >> 8) & 0xFF;
	
	if (ERROR_OK !=  versaloon_send_command(3, NULL))
	{
		LOG_INFO(_GETTEXT("fail to set jtag speed\n"));
		return ERROR_FAIL;
	}
	sleep_ms(10);
	
	return ERROR_OK;
}

static uint8 vsllink_jtag_get_pin_remap(uint8 mask)
{
	if (mask & ~(JTAG_SRST | JTAG_TRST | JTAG_USR1 | JTAG_USR2))
	{
		LOG_BUG(_GETTEXT("unsupported bits in mask, ignored\n"));
	}
	
	return mask;
}
static uint8 vsllink_jtag_get_pin_map(uint8 mask)
{
	if (mask & ~(JTAG_SRST | JTAG_TRST | JTAG_USR1 | JTAG_USR2))
	{
		LOG_BUG(_GETTEXT("unsupported bits in mask, ignored\n"));
	}
	
	return mask;
}

RESULT vsllink_jtag_auxio_config(uint8 pin_mask, uint8 io)
{
	if (vsllink_buffer_index > 0)
	{
		LOG_BUG(_GETTEXT("there is jtag_tap command uncommited\n"));
		return ERROR_FAIL;
	}
	
	versaloon_buf[0] = VSLLINK_CMD_SET_PORTDIR;
	versaloon_buf[1] = vsllink_jtag_get_pin_remap(pin_mask);
	versaloon_buf[2] = vsllink_jtag_get_pin_remap(io);
	
	if (ERROR_OK !=  versaloon_send_command(3, NULL))
	{
		LOG_INFO(_GETTEXT("fail to set jtag daisy chain\n"));
		return ERROR_FAIL;
	}
	sleep_ms(10);
	
	return ERROR_OK;
}

RESULT vsllink_jtag_auxio_out(uint8 pin_mask, uint8 value)
{
	if (vsllink_buffer_index > 0)
	{
		LOG_BUG(_GETTEXT("there is jtag_tap command uncommited\n"));
		return ERROR_FAIL;
	}
	
	versaloon_buf[0] = VSLLINK_CMD_SET_PORT;
	versaloon_buf[1] = vsllink_jtag_get_pin_remap(pin_mask);
	versaloon_buf[2] = vsllink_jtag_get_pin_remap(value);
	
	if (ERROR_OK !=  versaloon_send_command(3, NULL))
	{
		LOG_INFO(_GETTEXT("fail to set jtag daisy chain\n"));
		return ERROR_FAIL;
	}
	sleep_ms(10);
	
	return ERROR_OK;
}

RESULT vsllink_jtag_auxio_in(uint8 pin_mask, uint8 *value)
{
	uint8 port_value = 0;
	uint16 inlen;
	
	if (vsllink_buffer_index > 0)
	{
		LOG_BUG(_GETTEXT("there is jtag_tap command uncommited\n"));
		return ERROR_FAIL;
	}
	
	versaloon_buf[0] = VSLLINK_CMD_GET_PORT;
	versaloon_buf[1] = vsllink_jtag_get_pin_remap(pin_mask);
	
	if (ERROR_OK !=  versaloon_send_command(2, &inlen))
	{
		LOG_INFO(_GETTEXT("fail to set jtag daisy chain\n"));
		return ERROR_FAIL;
	}
	if (inlen != 1)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATE_DEVICE), "auxio pins");
		return ERROR_FAIL;
	}
	sleep_ms(10);

	*value = vsllink_jtag_get_pin_map(port_value);
	return ERROR_OK;
}





RESULT vsllink_jtagll_commit(void)
{
	uint16 inlen = 0, i, *pinlen;
	uint8 command;
	
	if (0 == vsllink_buffer_index)
	{
		return ERROR_OK;
	}
	
	if (versaloon_pending_idx > 0)
	{
		pinlen = &inlen;
	}
	else
	{
		pinlen = NULL;
	}
	
	versaloon_buf[0] = VSLLINK_CMD_HW_JTAGSEQCMD;
	versaloon_buf[1] = (vsllink_buffer_index >> 0) & 0xFF;
	versaloon_buf[2] = (vsllink_buffer_index >> 8) & 0xFF;
	
	if (ERROR_OK != versaloon_send_command(vsllink_buffer_index, pinlen))
	{
		return ERROR_FAIL;
	}
	
	vsllink_buffer_index = 0;
	for (i = 0; i < versaloon_pending_idx; i++)
	{
		if (vsllink_buffer_index + versaloon_pending[i].actual_data_size 
			> inlen)
		{
			LOG_ERROR(_GETTEXT("invalid received data length"));
			return ERROR_FAIL;
		}
		command = versaloon_pending[i].cmd & VSLLINK_CMDJTAGSEQ_CMDMSK;
		switch (command)
		{
		case VSLLINK_CMDJTAGSEQ_TMSBYTE:
			LOG_BUG("0x%02x has return data?\n", command);
			vsllink_buffer_index = 0;
			versaloon_pending_idx = 0;
			return ERROR_FAIL;
			break;
		case VSLLINK_CMDJTAGSEQ_TMSCLOCK:
			// no need to check, return data is only used to notify ready
			vsllink_buffer_index++;
			break;
		case VSLLINK_CMDJTAGSEQ_SCAN:
			if ((versaloon_pending[i].actual_data_size > 0) 
				&& (versaloon_pending[i].data_buffer != NULL))
			{
				memcpy(versaloon_pending[i].data_buffer, 
					   versaloon_buf + vsllink_buffer_index, 
					   versaloon_pending[i].actual_data_size);
			}
			vsllink_buffer_index += versaloon_pending[i].actual_data_size;
			break;
		default:
			LOG_BUG("is this really my command?\n");
			vsllink_buffer_index = 0;
			versaloon_pending_idx = 0;
			return ERROR_FAIL;
			break;
		}
	}
	
	// data received, but not the right size
	if ((versaloon_pending_idx > 0) && (inlen != vsllink_buffer_index))
	{
		LOG_ERROR(_GETTEXT("length of return data invalid\n"));
		return ERROR_FAIL;
	}
	
	vsllink_buffer_index = 0;
	
	if (versaloon_pending_idx > 0)
	{
		versaloon_pending_idx = 0;
	}
	else
	{
		// no receive data, avoid collision
		// it's not efficient to call this function is no data is returned
		sleep_ms(10);
	}
	
	return ERROR_OK;
}

RESULT vsllink_jtagll_disconnect(void)
{
	if (vsllink_buffer_index > 0)
	{
		// there is command in buffer, try to send
		vsllink_jtagll_commit();
	}
	
	return vsllink_disconnect();
}

RESULT vsllink_jtagll_add_pending(uint8 cmd, uint8 *buf, uint16 len)
{
	switch (cmd & VSLLINK_CMDJTAGSEQ_CMDMSK)
	{
	case VSLLINK_CMDJTAGSEQ_TMSBYTE:
		return ERROR_OK;
	case VSLLINK_CMDJTAGSEQ_TMSCLOCK:
		versaloon_add_pending(0, cmd, len, 0, 0, buf, 0, 0);
		return ERROR_OK;
	case VSLLINK_CMDJTAGSEQ_SCAN:
		versaloon_add_pending(0, cmd, len, 0, 0, buf, 0, 0);
		return ERROR_OK;
		break;
	default:
		LOG_BUG(_GETTEXT("Is this really my command?\n"));
		return ERROR_FAIL;
		break;
	}
}

RESULT vsllink_jtagll_add_command(uint8 cmd, uint8 *cmddata, uint16 cmdlen, 
								  uint8 *retdata, uint16 retlen)
{
	// check free space, commit if not enough
	if ((vsllink_buffer_index + cmdlen + 1)>= versaloon_buf_size)
	{
		if (ERROR_OK != vsllink_jtagll_commit())
		{
			return ERROR_FAIL;
		}
	}
	
	if (0 == vsllink_buffer_index)
	{
		vsllink_buffer_index = 3;
	}
	
	versaloon_buf[vsllink_buffer_index++] = cmd;
	if ((cmdlen > 0) && (NULL != cmddata))
	{
		memcpy(versaloon_buf + vsllink_buffer_index, cmddata, cmdlen);
		vsllink_buffer_index += cmdlen;
	}
	
	return vsllink_jtagll_add_pending(cmd, retdata, retlen);
}

RESULT vsllink_jtagll_tms(uint8 *tms, uint8 len)
{
	if ((len - 1) > VSLLINK_CMDJTAGHL_LENMSK)
	{
		LOG_BUG("invalid len, must be < %d\n", VSLLINK_CMDJTAGHL_LENMSK + 1);
		return ERROR_FAIL;
	}
	
	return vsllink_jtagll_add_command(VSLLINK_CMDJTAGSEQ_TMSBYTE | (len - 1), 
									  tms, len, tms, len);
}

RESULT vsllink_jtagll_tms_clocks(uint32 len, uint8 tms)
{
	buf_tmp[0] = (len >> 0) & 0xFF;
	buf_tmp[1] = (len >> 8) & 0xFF;
	buf_tmp[2] = (len >> 16) & 0xFF;
	buf_tmp[3] = (len >> 24) & 0xFF;
	
	return vsllink_jtagll_add_command(VSLLINK_CMDJTAGSEQ_TMSCLOCK | (tms > 0), 
									  buf_tmp, 4, NULL, 1);
}

RESULT vsllink_jtagll_xr(uint8* r, uint16 len, uint8 tms_before_valid, 
						 uint8 tms_before, uint8 tms_after0, uint8 tms_after1)
{
	uint16 byte_len = (len + 7) >> 3;
	
	// shift byte length
	buf_tmp[0] = (byte_len >> 0) & 0xFF;
	buf_tmp[1] = (byte_len >> 8) & 0xFF;
	
	// tms before
	if (tms_before_valid > 0)
	{
		tms_before_valid = 1;
		buf_tmp[2] = tms_before;
	}
	
	// shift data
	memcpy(&buf_tmp[2 + tms_before_valid], r, byte_len);
	
	// tms after0, tms after1
	buf_tmp[2 + tms_before_valid + byte_len + 0] = tms_after0;
	buf_tmp[2 + tms_before_valid + byte_len + 1] = tms_after1;
	
	return vsllink_jtagll_add_command(
								VSLLINK_CMDJTAGSEQ_SCAN | tms_before_valid, 
								buf_tmp, byte_len + tms_before_valid + 4, r, 
								byte_len);
}





jtag_callback_t vsllink_jtaghl_receive_callback = NULL;
jtag_callback_t vsllink_jtaghl_send_callback = NULL;
uint32 vsllink_jtaghl_ir_backup = 0;
RESULT vsllink_jtaghl_disconnect(void)
{
	if (vsllink_buffer_index > 0)
	{
		// there is command in buffer, try to send
		vsllink_jtaghl_commit();
	}
 
	return vsllink_disconnect();
}

RESULT vsllink_jtaghl_set_daisychain(uint8 ub, uint8 ua, uint16 bb, uint16 ba)
{
	if (vsllink_buffer_index > 0)
	{
		LOG_BUG(_GETTEXT("there is jtag_tap command uncommited\n"));
		return ERROR_FAIL;
	}
	
	versaloon_buf[0] = VSLLINK_CMD_JTAGHL_SET_DAISYCHAIN_POS;
	versaloon_buf[1] = ub;
	versaloon_buf[2] = ua;
	versaloon_buf[3] = (bb >> 0) & 0xFF;
	versaloon_buf[4] = (bb >> 8) & 0xFF;
	versaloon_buf[5] = (ba >> 0) & 0xFF;
	versaloon_buf[6] = (ba >> 8) & 0xFF;
	
	if (ERROR_OK !=  versaloon_send_command(7, NULL))
	{
		LOG_INFO(_GETTEXT("fail to set jtag daisy chain\n"));
		return ERROR_FAIL;
	}
	sleep_ms(10);
	
	return ERROR_OK;
}

RESULT vsllink_jtaghl_config(uint16 kHz, uint8 ub, uint8 ua, uint16 bb, 
							 uint16 ba)
{
	if (ERROR_OK != vsllink_jtag_set_freq(kHz))
	{
		LOG_ERROR(_GETTEXT("fail to set jtag speed\n"));
		return ERROR_FAIL;
	}
	sleep_ms(10);
	if (ERROR_OK != vsllink_jtaghl_set_daisychain(ub, ua, bb, ba))
	{
		LOG_ERROR(_GETTEXT("fail to set jtag daisy chain\n"));
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT vsllink_jtaghl_add_pending(uint8 cmd, uint8 *buf, uint16 len)
{
	switch (cmd & VSLLINK_CMDJTAGHL_CMDMSK)
	{
	case VSLLINK_CMDJTAGHL_TMS:
		return ERROR_OK;
	case VSLLINK_CMDJTAGHL_POLL_DLY:
	case VSLLINK_CMDJTAGHL_IR:
	case VSLLINK_CMDJTAGHL_DR:
		versaloon_add_pending(0, cmd, len, 0, 0, buf, 0, 
							  vsllink_jtaghl_ir_backup);
		return ERROR_OK;
		break;
	default:
		LOG_BUG(_GETTEXT("is this really my command?\n"));
		return ERROR_FAIL;
		break;
	}
}

RESULT vsllink_jtaghl_add_command(uint8 cmd, uint8 *cmddata, uint16 cmdlen, 
								  uint8 *retdata, uint16 retlen)
{
	uint16 processed_len = 0;
	
	// check free space, commit if not enough
	if ((vsllink_buffer_index + cmdlen + 1)>= versaloon_buf_size)
	{
		if (ERROR_OK != vsllink_jtaghl_commit())
		{
			return ERROR_FAIL;
		}
	}
	
	if (0 == vsllink_buffer_index)
	{
		vsllink_buffer_index = 3;
	}
	
	versaloon_buf[vsllink_buffer_index++] = cmd;
	
	if (vsllink_jtaghl_send_callback != NULL)
	{
		if ((cmd & VSLLINK_CMDJTAGHL_CMDMSK) == VSLLINK_CMDJTAGHL_DR)
		{
			// first 2 bytes of dr data is bit_len
			versaloon_buf[vsllink_buffer_index++] = cmddata[0];
			versaloon_buf[vsllink_buffer_index++] = cmddata[1];
			cmddata += 2;
			cmdlen -= 2;
			vsllink_jtaghl_send_callback(JTAG_SCANTYPE_DR, 
										 vsllink_jtaghl_ir_backup, 
										 versaloon_buf + vsllink_buffer_index, 
										 cmddata, cmdlen, &processed_len);
			vsllink_buffer_index += processed_len;
		}
		else if((cmd & VSLLINK_CMDJTAGHL_CMDMSK) == VSLLINK_CMDJTAGHL_IR)
		{
			// first 1 byte of ir data is bit_len
			versaloon_buf[vsllink_buffer_index++] = cmddata[0];
			cmddata += 1;
			cmdlen -= 1;
			vsllink_jtaghl_send_callback(JTAG_SCANTYPE_IR, 
										 vsllink_jtaghl_ir_backup, 
										 versaloon_buf + vsllink_buffer_index, 
										 cmddata, cmdlen, &processed_len);
			vsllink_buffer_index += processed_len;
		}
	}
	
	if (!processed_len && (cmdlen > 0) && (NULL != cmddata))
	{
		memcpy(versaloon_buf + vsllink_buffer_index, cmddata, cmdlen);
		vsllink_buffer_index += cmdlen;
	}
	
	return vsllink_jtaghl_add_pending(cmd, retdata, retlen);
}

RESULT vsllink_jtaghl_register_callback(jtag_callback_t send_callback, 
										jtag_callback_t receive_callback)
{
	vsllink_jtaghl_receive_callback = receive_callback;
	vsllink_jtaghl_send_callback = send_callback;
	return ERROR_OK;
}

RESULT vsllink_jtaghl_commit(void)
{
	uint16 inlen = 0, *pinlen, i;
	uint8 command;
	uint16 processed;
	RESULT ret;
	
	if (0 == vsllink_buffer_index)
	{
		return ERROR_OK;
	}
	
	if (versaloon_pending_idx > 0)
	{
		pinlen = &inlen;
	}
	else
	{
		pinlen = NULL;
	}
	
	versaloon_buf[0] = VSLLINK_CMD_HW_JTAGHLCMD;
	versaloon_buf[1] = (vsllink_buffer_index >> 0) & 0xFF;
	versaloon_buf[2] = (vsllink_buffer_index >> 8) & 0xFF;
	
	if (ERROR_OK != versaloon_send_command(vsllink_buffer_index, pinlen))
	{
		return ERROR_FAIL;
	}
	
	vsllink_buffer_index = 0;
	for (i = 0; i < versaloon_pending_idx; i++)
	{
		if (vsllink_buffer_index + versaloon_pending[i].actual_data_size 
			> inlen)
		{
			LOG_ERROR(_GETTEXT("invalid received data length\n"));
			return ERROR_FAIL;
		}
		command = versaloon_pending[i].cmd & VSLLINK_CMDJTAGHL_CMDMSK;
		switch (command)
		{
		case VSLLINK_CMDJTAGHL_TMS:
			LOG_BUG(_GETTEXT("0x%02x has return data?\n"), command);
			vsllink_buffer_index = 0;
			versaloon_pending_idx = 0;
			return ERROR_FAIL;
			break;
		case VSLLINK_CMDJTAGHL_POLL_DLY:
			if (versaloon_buf[vsllink_buffer_index++] > 0)
			{
				LOG_ERROR(_GETTEXT("poll timeout\n"));
				vsllink_buffer_index = 0;
				versaloon_pending_idx = 0;
				return ERROR_FAIL;
			}
			break;
		case VSLLINK_CMDJTAGHL_IR:
		case VSLLINK_CMDJTAGHL_DR:
			processed = 0;
			if (vsllink_jtaghl_receive_callback != NULL)
			{
				if (VSLLINK_CMDJTAGHL_IR == command)
				{
					ret = vsllink_jtaghl_receive_callback(JTAG_SCANTYPE_IR, 
										versaloon_pending[i].id, 
										versaloon_pending[i].data_buffer, 
										versaloon_buf + vsllink_buffer_index, 
										versaloon_pending[i].actual_data_size, 
										&processed);
					if (ERROR_OK != ret)
					{
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								  "call callback");
						return ERROR_FAIL;
					}
				}
				else
				{
					ret = vsllink_jtaghl_receive_callback(JTAG_SCANTYPE_DR, 
										versaloon_pending[i].id, 
										versaloon_pending[i].data_buffer, 
										versaloon_buf + vsllink_buffer_index, 
										versaloon_pending[i].actual_data_size, 
										&processed);
					if (ERROR_OK != ret)
					{
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								  "call callback");
						return ERROR_FAIL;
					}
				}
			}
			if (!processed && (versaloon_pending[i].actual_data_size > 0) 
				&& (versaloon_pending[i].data_buffer != NULL))
			{
				memcpy(versaloon_pending[i].data_buffer, 
					   versaloon_buf + vsllink_buffer_index, 
					   versaloon_pending[i].actual_data_size);
			}
			vsllink_buffer_index += versaloon_pending[i].actual_data_size;
			break;
		default:
			LOG_BUG(_GETTEXT("is this really my command?\n"));
			vsllink_buffer_index = 0;
			versaloon_pending_idx = 0;
			return ERROR_FAIL;
			break;
		}
	}
	
	// data received, but not the right size
	if ((versaloon_pending_idx > 0) && (inlen != vsllink_buffer_index))
	{
		LOG_ERROR(_GETTEXT("length of return data invalid\n"));
		return ERROR_FAIL;
	}
	
	vsllink_buffer_index = 0;
	
	if (versaloon_pending_idx > 0)
	{
		versaloon_pending_idx = 0;
	}
	else
	{
		// no receive data, avoid collision
		sleep_ms(10);
	}
	
	return ERROR_OK;
}

RESULT vsllink_jtaghl_tms(uint8 *tms, uint8 len)
{
	if ((len - 1) > VSLLINK_CMDJTAGHL_LENMSK)
	{
		LOG_BUG(_GETTEXT("invalid len, must be < %d\n"), 
				VSLLINK_CMDJTAGHL_LENMSK + 1);
		return ERROR_FAIL;
	}
	
	return vsllink_jtaghl_add_command(VSLLINK_CMDJTAGHL_TMS | (len - 1), 
									  tms, (len + 7) >> 3, NULL, 0);
}

RESULT vsllink_jtaghl_runtest(uint32 cycles)
{
	uint8 tms[(VSLLINK_CMDJTAGHL_LENMSK + 7) >> 3];
	uint8 cur_cycles;
	
	memset(tms, 0, sizeof(tms));		// tms = 0 in runtest
	while(cycles > 0)
	{
		if (cycles > (VSLLINK_CMDJTAGHL_LENMSK + 1))
		{
			cur_cycles = VSLLINK_CMDJTAGHL_LENMSK + 1;
		}
		else
		{
			cur_cycles = (uint8)cycles;
		}
		
		if (ERROR_OK != vsllink_jtaghl_tms((uint8*)&tms, cur_cycles))
		{
			return ERROR_FAIL;
		}
		
		cycles -= cur_cycles;
	}
	
	return ERROR_OK;
}

RESULT vsllink_jtaghl_ir(uint8 *ir, uint8 len, uint8 idle, uint8 want_ret)
{
	uint16 byte_len = (len + 7) >> 3;
	RESULT ret;
	
	if ((idle - 1) > VSLLINK_CMDJTAGHL_LENMSK)
	{
		LOG_BUG("invalid len, must be < %d\n", VSLLINK_CMDJTAGHL_LENMSK + 1);
		return ERROR_FAIL;
	}
	
	buf_tmp[0] = len;
	memcpy(buf_tmp + 1, ir, byte_len);
	
	if (len < 8)
	{
		vsllink_jtaghl_ir_backup = ir[0];
	}
	else if (len < 16)
	{
		vsllink_jtaghl_ir_backup = ir[0] + (ir[1] << 8);
	}
	else if (len < 24)
	{
		vsllink_jtaghl_ir_backup = ir[0] + (ir[1] << 8) + (ir[2] << 16);
	}
	else if (len < 32)
	{
		vsllink_jtaghl_ir_backup = ir[0] + (ir[1] << 8) + (ir[2] << 16) 
								   + (ir[3] << 24);
	}
	
	if (want_ret)
	{
		ret = vsllink_jtaghl_add_command(VSLLINK_CMDJTAGHL_IR | idle, buf_tmp, 
										 byte_len + 1, ir, byte_len);
	}
	else
	{
		ret = vsllink_jtaghl_add_command(VSLLINK_CMDJTAGHL_IR | idle, buf_tmp, 
										 byte_len + 1, NULL, byte_len);
	}
	
	return ret;
}

RESULT vsllink_jtaghl_dr(uint8 *dr, uint16 len, uint8 idle, uint8 want_ret)
{
	uint16 byte_len = (len + 7) >> 3;
	RESULT ret;
	
	if ((idle - 1) > VSLLINK_CMDJTAGHL_LENMSK)
	{
		LOG_BUG("invalid len, must be < %d\n", VSLLINK_CMDJTAGHL_LENMSK + 1);
		return ERROR_FAIL;
	}
	
	buf_tmp[0] = (len >> 0) & 0xFF;
	buf_tmp[1] = (len >> 8) & 0xFF;
	memcpy(buf_tmp + 2, dr, byte_len);
	
	if (want_ret)
	{
		ret = vsllink_jtaghl_add_command(VSLLINK_CMDJTAGHL_DR | idle, buf_tmp, 
										 byte_len + 2, dr, byte_len);
	}
	else
	{
		ret = vsllink_jtaghl_add_command(VSLLINK_CMDJTAGHL_DR | idle, buf_tmp, 
										 byte_len + 2, NULL, byte_len);
	}
	
	return ret;
}

RESULT vsllink_jtaghl_poll(uint8 *ir0, uint8 ir0idle, uint8 ir0len, 
						   uint8 *dr0, uint8 dr0idle, uint8 dr0len,
						   uint8 *ir1, uint8 ir1idle, uint8 ir1len, 
						   uint8 *dr1, uint8 dr1idle, uint8 dr1len,
						   uint8 *ir2, uint8 ir2idle, uint8 ir2len, 
						   uint8 *dr2, uint8 dr2idle, uint8 dr2len,
						   uint8 pos, uint8 mask, uint8 value, 
						   uint16 poll_count)
{
	uint8 buf_tmp[1024], cmd;
	uint16 idx = 0;
	
	cmd = VSLLINK_CMDJTAGHL_POLL_DLY;
	
	// poll_count
	buf_tmp[0] = (poll_count >> 0) & 0xFF;
	buf_tmp[1] = (poll_count >> 8) & 0xFF;
	idx = 2;
	
	// poll ir0
	if ((ir0 != NULL) && (ir0len > 0))
	{
		cmd |= (1 << 0);
		buf_tmp[idx++] = ir0len - 1;					// ir length in bits
		buf_tmp[idx++] = ir0idle;						// ir idle count
		memcpy(buf_tmp + idx, ir0, (ir0len + 7) >> 3);	// ir data
		idx += (ir0len + 7) >> 3;
	}
	
	// poll dr0
	if ((dr0 != NULL) && (dr0len > 0))
	{
		cmd |= (1 << 3);
		buf_tmp[idx++] = dr0len - 1;					// dr length in bits
		buf_tmp[idx++] = dr0idle;						// dr idle count
		memcpy(buf_tmp + idx, dr0, (dr0len + 7) >> 3);	// dr data
		idx += (dr0len + 7) >> 3;
	}
	
	// poll ir1
	if ((ir1 != NULL) && (ir1len > 0))
	{
		cmd |= (1 << 0);
		buf_tmp[idx++] = ir1len - 1;					// ir length in bits
		buf_tmp[idx++] = ir1idle;						// ir idle count
		memcpy(buf_tmp + idx, ir1, (ir1len + 7) >> 3);	// ir data
		idx += (ir1len + 7) >> 3;
	}
	
	// poll dr1
	if ((dr1 != NULL) && (dr1len > 0))
	{
		cmd |= (1 << 3);
		buf_tmp[idx++] = dr1len - 1;					// dr length in bits
		buf_tmp[idx++] = dr1idle;						// dr idle count
		memcpy(buf_tmp + idx, dr1, (dr1len + 7) >> 3);	// dr data
		idx += (dr1len + 7) >> 3;
	}
	
	// poll ir2
	if ((ir2 != NULL) && (ir2len > 0))
	{
		cmd |= (1 << 0);
		buf_tmp[idx++] = ir2len - 1;					// ir length in bits
		buf_tmp[idx++] = ir2idle;						// ir idle count
		memcpy(buf_tmp + idx, ir2, (ir2len + 7) >> 3);	// ir data
		idx += (ir2len + 7) >> 3;
	}
	
	// poll dr2
	if ((dr2 != NULL) && (dr2len > 0))
	{
		cmd |= (1 << 3);
		buf_tmp[idx++] = dr2len - 1;					// dr length in bits
		buf_tmp[idx++] = dr2idle;						// dr idle count
		memcpy(buf_tmp + idx, dr2, (dr2len + 7) >> 3);	// dr data
		idx += (dr2len + 7) >> 3;
	}
	
	// poll check
	buf_tmp[idx++] = 1;				// check 1 byte
	buf_tmp[idx++] = pos;			// check position of last operation
	buf_tmp[idx++] = mask;			// check mask
	buf_tmp[idx++] = value;			// check value
	
	return vsllink_jtaghl_add_command(cmd, buf_tmp, idx, NULL, 0);
}

RESULT vsllink_jtaghl_delay_us(uint16 us)
{
	if (us > 0x8000)
	{
		LOG_BUG(_GETTEXT("parameter of delay_us is too large, truncted\n"));
		return ERRCODE_INVALID_PARAMETER;
	}
	
	return vsllink_jtaghl_add_command(VSLLINK_CMDJTAGHL_POLL_DLY, (uint8*)&us, 
									  2, NULL, 0);
}

RESULT vsllink_jtaghl_delay_ms(uint16 ms)
{
	if (ms > 0x8000)
	{
		LOG_BUG(_GETTEXT("parameter of delay_ms is too large, truncted\n"));
		return ERRCODE_INVALID_PARAMETER;
	}
	
	ms |= 0x8000;
	return vsllink_jtaghl_add_command(VSLLINK_CMDJTAGHL_POLL_DLY, (uint8*)&ms, 
									  2, NULL, 0);
}






RESULT vsllink_swj_connect(void)
{
	return vsllink_connect(VSLLINK_MODE_SWJ);
}

RESULT vsllink_swj_disconnect(void)
{
	if (vsllink_buffer_index > 0)
	{
		// there is command in buffer, try to send
		vsllink_swj_commit(NULL);
	}
	
	return vsllink_disconnect();
}

RESULT vsllink_swj_commit(uint8 *result)
{
	uint16 inlen = 0, i, *pinlen;
	uint8 command;
	
	if (0 == vsllink_buffer_index)
	{
		return ERROR_OK;
	}
	
	if (versaloon_pending_idx > 0)
	{
		pinlen = &inlen;
	}
	else
	{
		pinlen = NULL;
	}
	
	versaloon_buf[0] = VSLLINK_CMD_HW_SWJCMD;
	versaloon_buf[1] = (vsllink_buffer_index >> 0) & 0xFF;
	versaloon_buf[2] = (vsllink_buffer_index >> 8) & 0xFF;
	
	if (ERROR_OK != versaloon_send_command(vsllink_buffer_index, pinlen))
	{
		return ERROR_FAIL;
	}
	
	vsllink_buffer_index = 0;
	for (i = 0; i < versaloon_pending_idx; i++)
	{
		if (vsllink_buffer_index + versaloon_pending[i].actual_data_size 
			> inlen)
		{
			LOG_ERROR(_GETTEXT("invalid received data length"));
			return ERROR_FAIL;
		}
		command = versaloon_pending[i].cmd & VSLLINK_CMDSWJ_CMDMSK;
		switch (command)
		{
		case VSLLINK_CMDSWJ_SEQOUT:
			if (versaloon_buf[vsllink_buffer_index] != 0)
			{
				LOG_ERROR(_GETTEXT("fail to execute swj_seqout\n"));
				return ERROR_FAIL;
			}
			vsllink_buffer_index += versaloon_pending[i].actual_data_size;
			break;
		case VSLLINK_CMDSWJ_SEQIN:
			memcpy(versaloon_pending[i].data_buffer, 
				   versaloon_buf + vsllink_buffer_index, 
				   versaloon_pending[i].actual_data_size);
			vsllink_buffer_index += versaloon_pending[i].actual_data_size;
			break;
		case VSLLINK_CMDSWJ_TRANS:
			if (result != NULL)
			{
				*result = versaloon_buf[vsllink_buffer_index] & 0x07;
			}
			if ((versaloon_buf[vsllink_buffer_index] & 0xF0) != SWJ_SUCCESS)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ERRCODE16), 
						  "do swj transaction", 
						  versaloon_buf[vsllink_buffer_index]);
				
				vsllink_buffer_index = 0;
				versaloon_pending_idx = 0;
				return ERROR_FAIL;
			}
			else
			{
				memcpy(versaloon_pending[i].data_buffer, 
					   versaloon_buf + vsllink_buffer_index + 1, 
					   4);
			}
			vsllink_buffer_index += 5;
			break;
		default:
			LOG_BUG("is this really my command?\n");
			vsllink_buffer_index = 0;
			versaloon_pending_idx = 0;
			return ERROR_FAIL;
			break;
		}
	}
	
	// data received, but not the right size
	if ((versaloon_pending_idx > 0) && (inlen != vsllink_buffer_index))
	{
		LOG_ERROR(_GETTEXT("length of return data invalid\n"));
		return ERROR_FAIL;
	}
	
	vsllink_buffer_index = 0;
	
	if (versaloon_pending_idx > 0)
	{
		versaloon_pending_idx = 0;
	}
	else
	{
		// no receive data, avoid collision
		// it's not efficient to call this function is no data is returned
		sleep_ms(10);
	}
	
	return ERROR_OK;
}

RESULT vsllink_swj_add_pending(uint8 cmd, uint8 *buf, uint16 len)
{
	switch (cmd & VSLLINK_CMDSWJ_CMDMSK)
	{
	case VSLLINK_CMDSWJ_SEQOUT:
		versaloon_add_pending(0, cmd, len, 0, 0, buf, 0, 0);
		return ERROR_OK;
	case VSLLINK_CMDSWJ_SEQIN:
		versaloon_add_pending(0, cmd, len, 0, 0, buf, 0, 0);
		return ERROR_OK;
	case VSLLINK_CMDSWJ_TRANS:
		versaloon_add_pending(0, cmd, len, 0, 0, buf, 0, 0);
		return ERROR_OK;
		break;
	case VSLLINK_CMDSWJ_PARA:
		return ERROR_OK;
	default:
		LOG_BUG(_GETTEXT("Is this really my command?\n"));
		return ERROR_FAIL;
		break;
	}
}

RESULT vsllink_swj_add_command(uint8 cmd, uint8 *cmddata, uint16 cmdlen, 
							   uint8 *retdata, uint16 retlen)
{
	// check free space, commit if not enough
	if ((vsllink_buffer_index + cmdlen + 1)>= versaloon_buf_size)
	{
		if (ERROR_OK != vsllink_swj_commit(NULL))
		{
			return ERROR_FAIL;
		}
	}
	
	if (0 == vsllink_buffer_index)
	{
		vsllink_buffer_index = 3;
	}
	
	versaloon_buf[vsllink_buffer_index++] = cmd;
	if ((cmdlen > 0) && (NULL != cmddata))
	{
		memcpy(versaloon_buf + vsllink_buffer_index, cmddata, cmdlen);
		vsllink_buffer_index += cmdlen;
	}
	
	return vsllink_swj_add_pending(cmd, retdata, retlen);
}

RESULT vsllink_swj_setpara(uint8 trn, uint16 retry, uint16 dly)
{
	buf_tmp[0] = trn;
	buf_tmp[1] = (retry >> 0) & 0xFF;
	buf_tmp[2] = (retry >> 8) & 0xFF;
	buf_tmp[3] = (dly >> 0) & 0xFF;
	buf_tmp[4] = (dly >> 8) & 0xFF;
	
	return vsllink_swj_add_command(VSLLINK_CMDSWJ_PARA, buf_tmp, 
								   5, NULL, 0);
}

RESULT vsllink_swj_seqout(uint8 *data, uint16 bit_len)
{
	uint16 data_len = (bit_len + 7) / 8;
	
	buf_tmp[0] = (bit_len >> 0) & 0xFF;
	buf_tmp[1] = (bit_len >> 8) & 0xFF;
	memcpy(buf_tmp + 2, data, data_len);
	
	return vsllink_swj_add_command(VSLLINK_CMDSWJ_SEQOUT, buf_tmp, 
								   2 + data_len, data, 1);
}

RESULT vsllink_swj_seqin(uint8 *data, uint16 bit_len)
{
	uint16 data_len = (bit_len + 7) / 8;
	
	buf_tmp[0] = (bit_len >> 0) & 0xFF;
	buf_tmp[1] = (bit_len >> 8) & 0xFF;
	memcpy(buf_tmp + 2, data, data_len);
	
	return vsllink_swj_add_command(VSLLINK_CMDSWJ_SEQIN, buf_tmp, 
								   2 + data_len, data, data_len);
}

RESULT vsllink_swj_transact(uint8 request, uint32 *data)
{
	uint8 parity;
	
	parity = (request >> 1) & 1;
	parity += (request >> 2) & 1;
	parity += (request >> 3) & 1;
	parity += (request >> 4) & 1;
	parity &= 1;
	buf_tmp[0] = (request | 0x81 | (parity << 5)) & ~0x40;
	memcpy(buf_tmp + 1, (uint8*)data, 4);
	
	return vsllink_swj_add_command(VSLLINK_CMDSWJ_TRANS, buf_tmp, 5, 
								   (uint8*)data, 5);
}

