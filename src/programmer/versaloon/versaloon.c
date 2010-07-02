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
#include "prog_interface.h"

#include "usb.h"
#include "usbapi.h"

#include "pgbar.h"
#include "../programmer.h"
#include "versaloon.h"
#include "versaloon_internal.h"
#include "usbtoxxx/usbtoxxx.h"


const char *versaloon_hardwares[] = 
{
	"Versaloon_Full",		// 1
	"Versaloon_Mini",		// 2
	"Versaloon_Nano",		// 3
};

uint8_t *versaloon_buf = NULL;
uint8_t *versaloon_cmd_buf = NULL;
uint16_t versaloon_buf_size = 256;

struct versaloon_pending_t versaloon_pending[VERSALOON_MAX_PENDING_NUMBER];
uint16_t versaloon_pending_idx = 0;

static usb_dev_handle *versaloon_device_handle = NULL;
static uint16_t versaloon_vid = VERSALOON_VID;
static uint16_t versaloon_pid = VERSALOON_PID;
static char *versaloon_serialstring = NULL;
static uint8_t versaloon_epout = VERSALOON_OUTP;
static uint8_t versaloon_epin = VERSALOON_INP;
static uint8_t versaloon_interface = 1;
static uint32_t versaloon_to = VERSALOON_TIMEOUT;

void versaloon_usage(void)
{
	printf("\
Usage of %s:\n\
  -U,  --usb <PID_VID_EPIN_EPOUT>           set usb VID, PID, EPIN, EPOUT\n\n", 
		   VERSALOON_STRING);
}

void versaloon_support(void)
{
	printf("\
%s: see http://www.SimonQian.com/en/Versaloon\n", VERSALOON_STRING);
}

RESULT versaloon_check_argument(char cmd, const char *argu)
{
	char *cur_pointer, *end_pointer;
	
	switch (cmd)
	{
	case 'h':
		versaloon_usage();
		break;
	case 'S':
		versaloon_support();
		break;
	case 'U':
		// --usb
		if (NULL == argu)
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		cur_pointer = (char *)argu;
		// Format: SERIALSTRING
		while (*cur_pointer != 0)
		{
			if ((' ' == *cur_pointer) || ('_' == *cur_pointer) 
				|| ('-' == *cur_pointer))
			{
				break;
			}
			cur_pointer++;
		}
		if ((cur_pointer - argu) == (int32_t)strlen(argu))
		{
			versaloon_serialstring = (char *)argu;
			goto print_usb_device;
		}
		
		cur_pointer = (char *)argu;
		// Format: VID_PID_EPIN_EPOUT_INTERFACE_SERIALSTRING
		versaloon_vid = (uint16_t)strtoul(cur_pointer, &end_pointer, 0);
		if ((end_pointer == cur_pointer) 
			|| ((*end_pointer != '_') && (*end_pointer != ' ') 
				&& (*end_pointer != '-')) 
			|| ('\0' == *(end_pointer + 1)))
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		cur_pointer = end_pointer + 1;
		versaloon_pid = (uint16_t)strtoul(cur_pointer, &end_pointer, 0);
		if ((end_pointer == cur_pointer) 
			|| ((*end_pointer != '_') && (*end_pointer != ' ') 
				&& (*end_pointer != '-')) 
			|| ('\0' == *(end_pointer + 1)))
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		cur_pointer = end_pointer + 1;
		versaloon_epin = (uint8_t)strtoul(cur_pointer, &end_pointer, 0);
		if ((end_pointer == cur_pointer) 
			|| ((*end_pointer != '_') && (*end_pointer != ' ') 
				&& (*end_pointer != '-')) 
			|| ('\0' == *(end_pointer + 1)))
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		cur_pointer = end_pointer + 1;
		versaloon_epout = (uint8_t)strtoul(cur_pointer, &end_pointer, 0);
		if ((end_pointer == cur_pointer) 
			|| ((*end_pointer != '_') && (*end_pointer != ' ') 
				&& (*end_pointer != '-')) 
			|| ('\0' == *(end_pointer + 1)))
		{
			goto print_usb_device;
		}
		cur_pointer = end_pointer + 1;
		versaloon_interface = (uint8_t)strtoul(cur_pointer, &end_pointer, 0);
		if ((end_pointer == cur_pointer) 
			|| ((*end_pointer != '_') && (*end_pointer != ' ') 
				&& (*end_pointer != '-')) 
			|| ('\0' == *(end_pointer + 1)))
		{
			goto print_usb_device;
		}
		cur_pointer = end_pointer + 1;
		versaloon_serialstring = cur_pointer;
		end_pointer = cur_pointer + strlen(versaloon_serialstring);
		if ((end_pointer == cur_pointer) || (*end_pointer != '\0'))
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
print_usb_device:
		if (versaloon_serialstring != NULL)
		{
			LOG_DEBUG(
			   _GETTEXT("Versaloon is on 0x%04X:0x%04X(0x%02x_0x%02X):%s.\n"), 
			   versaloon_vid, versaloon_pid, versaloon_epin, versaloon_epout, 
			   versaloon_serialstring);
		}
		else
		{
			LOG_DEBUG(
				_GETTEXT("Versaloon is on 0x%04X:0x%04X(0x%02x_0x%02X).\n"), 
				versaloon_vid, versaloon_pid, versaloon_epin, versaloon_epout);
		}
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}




// programmer_cmd
static uint32_t versaloon_pending_id = 0;
static versaloon_callback_t versaloon_callback = NULL;
void versaloon_set_pending_id(uint32_t id)
{
	versaloon_pending_id = id;
}
void versaloon_set_callback(versaloon_callback_t callback)
{
	versaloon_callback = callback;
}

RESULT versaloon_add_pending(uint8_t type, uint8_t cmd, uint16_t actual_szie, 
	uint16_t want_pos, uint16_t want_size, uint8_t *buffer, uint8_t collect)
{
#if PARAM_CHECK
	if (versaloon_pending_idx >= VERSALOON_MAX_PENDING_NUMBER)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_INDEX), 
				versaloon_pending_idx, "versaloon pending data");
		return ERROR_FAIL;
	}
#endif
	
	versaloon_pending[versaloon_pending_idx].type = type;
	versaloon_pending[versaloon_pending_idx].cmd = cmd;
	versaloon_pending[versaloon_pending_idx].actual_data_size = actual_szie;
	versaloon_pending[versaloon_pending_idx].want_data_pos = want_pos;
	versaloon_pending[versaloon_pending_idx].want_data_size = want_size;
	versaloon_pending[versaloon_pending_idx].data_buffer = buffer;
	versaloon_pending[versaloon_pending_idx].collect = collect;
	versaloon_pending[versaloon_pending_idx].id = versaloon_pending_id;
	versaloon_pending_id = 0;
	versaloon_pending[versaloon_pending_idx].callback = versaloon_callback;
	versaloon_callback = NULL;
	versaloon_pending_idx++;
	
	return ERROR_OK;
}

RESULT versaloon_send_command(uint16_t out_len, uint16_t *inlen)
{
	int ret;
	
#if PARAM_CHECK
	if (NULL == versaloon_buf)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_BUFFER), TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
	if ((0 == out_len) || (out_len > versaloon_buf_size))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	ret = usb_bulk_write(versaloon_device_handle, versaloon_epout, 
						 (char *)versaloon_buf, out_len, versaloon_to);
	if (ret != out_len)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ERRSTRING), 
				  "send usb data", usb_strerror());
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (inlen != NULL)
	{
		ret = usb_bulk_read(versaloon_device_handle, versaloon_epin, 
					(char *)versaloon_buf, versaloon_buf_size, versaloon_to);
		if (ret > 0)
		{
			*inlen = (uint16_t)ret;
			return ERROR_OK;
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ERRSTRING), 
					  "receive usb data", usb_strerror());
			return ERROR_FAIL;
		}
	}
	else
	{
		return ERROR_OK;
	}
}

#define VERSALOON_RETRY_CNT				10
RESULT versaloon_init(void)
{
	uint16_t ret = 0;
	uint8_t retry;
	uint32_t timeout_tmp;
	
	versaloon_device_handle = find_usb_device(versaloon_vid, versaloon_pid, 
							versaloon_interface, VERSALOON_SERIALSTRING_INDEX, 
							versaloon_serialstring);
	if (NULL == versaloon_device_handle)
	{
		if (versaloon_serialstring != NULL)
		{
			LOG_ERROR(
				_GETTEXT("Not found vid=0x%04x,pid = 0x%04x,serial = %s.\n"), 
				versaloon_vid, versaloon_pid, versaloon_serialstring);
		}
		else
		{
			LOG_ERROR(
				_GETTEXT("Not found vid=0x%04x,pid = 0x%04x.\n"), 
				versaloon_vid, versaloon_pid);
		}
		return ERROR_FAIL;
	}
	
	// malloc temporary buffer
	versaloon_buf = (uint8_t *)malloc(versaloon_buf_size);
	if (NULL == versaloon_buf)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	
	// connect to versaloon
	LOG_PUSH();
	LOG_MUTE();
	timeout_tmp = versaloon_to;
	// not output error message when connectting
	// 500ms delay when connect
	versaloon_to = 100;
	for (retry = 0; retry < VERSALOON_RETRY_CNT; retry++)
	{
		versaloon_buf[0] = VERSALOON_GET_INFO;
		if ((ERROR_OK == versaloon_send_command(1, &ret)) && (ret >= 3))
		{
			break;
		}
	}
	LOG_POP();
	versaloon_to = timeout_tmp;
	if (VERSALOON_RETRY_CNT == retry)
	{
		versaloon_fini();
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	versaloon_buf[ret] = 0;
	versaloon_buf_size = versaloon_buf[0] + (versaloon_buf[1] << 8);
	LOG_INFO("%s\n", versaloon_buf + 2);
	
	// free temporary buffer
	free(versaloon_buf);
	versaloon_buf = NULL;
	
	versaloon_buf = (uint8_t *)malloc(versaloon_buf_size);
	if (NULL == versaloon_buf)
	{
		versaloon_fini();
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	versaloon_cmd_buf = (uint8_t *)malloc(versaloon_buf_size - 3);
	if (NULL == versaloon_cmd_buf)
	{
		versaloon_fini();
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	return versaloon_get_target_voltage(&ret);
}

RESULT versaloon_fini(void)
{
	if (versaloon_device_handle != NULL)
	{
		usb_release_interface(versaloon_device_handle, versaloon_interface);
		usb_close(versaloon_device_handle);
		versaloon_device_handle = NULL;
		
		if (versaloon_buf != NULL)
		{
			free(versaloon_buf);
			versaloon_buf = NULL;
		}
		if (versaloon_cmd_buf != NULL)
		{
			free(versaloon_cmd_buf);
			versaloon_cmd_buf = NULL;
		}
	}
	
	return ERROR_OK;
}

RESULT versaloon_enter_firmware_update_mode(void)
{
#if PARAM_CHECK
	if (NULL == versaloon_buf)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_BUFFER), TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	versaloon_buf[0] = VERSALOON_FW_UPDATE;
	versaloon_buf[1] = 0xAA;
	versaloon_buf[2] = 0x55;
	
	if (ERROR_OK != versaloon_send_command(3, NULL))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	else
	{
		return ERROR_OK;
	}
}

const char* versaloon_get_hardware_name(uint8_t idx)
{
	if (idx < dimof(versaloon_hardwares))
	{
		return versaloon_hardwares[idx - 1];
	}
	else
	{
		return "unknown hardware";
	}
}

RESULT versaloon_get_hardware(uint8_t *hardware)
{
	uint16_t inlen;
	
#if PARAM_CHECK
	if (NULL == versaloon_buf)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_BUFFER), TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
	if (NULL == hardware)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	versaloon_buf[0] = VERSALOON_GET_HARDWARE;
	
	if ((ERROR_OK != versaloon_send_command(1, &inlen)) || (inlen != 1))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	else
	{
		*hardware = versaloon_buf[0];
		LOG_DEBUG(_GETTEXT("versaloon hardware is %s\n"), 
				  versaloon_get_hardware_name(*hardware));
		return ERROR_OK;
	}
}

RESULT versaloon_set_target_voltage(uint16_t voltage)
{
	usbtopwr_init();
	usbtopwr_config(VERSALOON_POWER_PORT);
	usbtopwr_output(VERSALOON_POWER_PORT, voltage);
	usbtopwr_fini();
	
	return usbtoxxx_execute_command();
}

RESULT versaloon_get_target_voltage(uint16_t *voltage)
{
	uint16_t inlen;
	
#if PARAM_CHECK
	if (NULL == versaloon_buf)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_BUFFER), TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
	if (NULL == voltage)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	versaloon_buf[0] = VERSALOON_GET_TVCC;
	
	if ((ERROR_OK != versaloon_send_command(1, &inlen)) || (inlen != 2))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	else
	{
		*voltage = versaloon_buf[0] + (versaloon_buf[1] << 8);
		return ERROR_OK;
	}
}

RESULT versaloon_query_mass_product_data_size(uint32_t *size)
{
	uint16_t inlen;
	
#if PARAM_CHECK
	if (NULL == versaloon_buf)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_BUFFER), TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
	if (NULL == size)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	versaloon_buf[0] = VERSALOON_GET_OFFLINE_SIZE;
	
	if ((ERROR_OK != versaloon_send_command(1, &inlen)) || (inlen != 4))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	else
	{
		*size = versaloon_buf[0] + (versaloon_buf[1] << 8) 
				+ (versaloon_buf[2] << 16) + (versaloon_buf[3] << 24);
		return ERROR_OK;
	}
}

RESULT versaloon_erase_mass_product_data(void)
{
	uint16_t inlen;
	
#if PARAM_CHECK
	if (NULL == versaloon_buf)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_BUFFER), TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	versaloon_buf[0] = VERSALOON_ERASE_OFFLINE_DATA;
	
	if ((ERROR_OK != versaloon_send_command(1, &inlen)) || (inlen != 1))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (versaloon_buf[0] != MP_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "erase mass product data");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

RESULT versaloon_checksum_mass_product_data(uint16_t * checksum, uint16_t len)
{
	uint16_t inlen;
	
#if PARAM_CHECK
	if (NULL == versaloon_buf)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_BUFFER), TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
	if (NULL == checksum)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	versaloon_buf[0] = VERSALOON_GET_OFFLINE_CHECKSUM;
	versaloon_buf[1] = (len >> 0) & 0xFF;
	versaloon_buf[2] = (len >> 8) & 0xFF;
	
	if ((ERROR_OK != versaloon_send_command(3, &inlen)) || (inlen != 2))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	*checksum = versaloon_buf[0] + (versaloon_buf[1] << 8);
	
	return ERROR_OK;
}

RESULT versaloon_download_mass_product_data(const char *name, uint8_t *buffer, 
											uint32_t len)
{
	uint32_t size = 0, i, j;
	uint16_t inlen, checksum = 0;
	
#if PARAM_CHECK
	if ((NULL == buffer) || (0 == len))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (ERROR_OK != versaloon_query_mass_product_data_size(&size))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "get size of mass-product area");
		return ERRCODE_FAILURE_OPERATION;
	}
	if (len >= size)
	{
		LOG_ERROR(_GETTEXT("Not enough mass-product area\n"));
		return ERROR_FAIL;
	}
	if (ERROR_OK != versaloon_erase_mass_product_data())
	{
		return ERROR_FAIL;
	}
	
	// check target index
	if (!strcmp(name, "psoc"))
	{
		// target is psoc
		buffer[0] = MP_ISSP;
	}
	else
	{
		// target is not supported, SHOULD be checked in the main()
		LOG_BUG(_GETTEXT(ERRMSG_NOT_SUPPORT), name);
		return ERRCODE_NOT_SUPPORT;
	}
	
	pgbar_init("writing mp data |", "|", 0, len, PROGRESS_STEP, '=');
	
	for (i = 0; i < len; i += 256)
	{
		// command: 1byte
		versaloon_buf[0] = VERSALOON_WRITE_OFFLINE_DATA;
		// length: 2bytes
		versaloon_buf[1] = 0x00;
		versaloon_buf[2] = 0x01;
		// address: 4bytes
		versaloon_buf[3] = (i >> 0) & 0xFF;
		versaloon_buf[4] = (i >> 8) & 0xFF;
		versaloon_buf[5] = (i >> 16) & 0xFF;
		versaloon_buf[6] = (i >> 24) & 0xFF;
		// data: 256 bytes
		for (j = 0; j < 256; j++)
		{
			if ((i + j) < len)
			{
				versaloon_buf[7 + j] = buffer[i + j];
				checksum += versaloon_buf[7 + j];
			}
			else
			{
				versaloon_buf[7 + j] = 0xFF;
			}
		}
		
		// program
		if ((ERROR_OK != versaloon_send_command(263, &inlen)) || (inlen != 1))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "communicate with versaloon");
			pgbar_fini();
			return ERRCODE_FAILURE_OPERATION;
		}
		if (versaloon_buf[0] != MP_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "write mass product data");
			pgbar_fini();
			return ERRCODE_FAILURE_OPERATION;
		}
		
		pgbar_update(256);
	}
	pgbar_fini();
	
	// verify checksum
	if (ERROR_OK != versaloon_checksum_mass_product_data(&inlen, (uint16_t)len))
	{
		LOG_WARNING(_GETTEXT(ERRMSG_FAILURE_READ), "checksum");
	}
	else
	{
		if (checksum != inlen)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_VERIFY), "checksum");
			return ERRCODE_FAILURE_VERIFY;
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFIED), 
					 "checksum of mass product data");
		}
	}
	
	return ERROR_OK;
}






// Interfaces:

// NULL function
RESULT versaloon_null(void)
{
	return ERROR_OK;
}
// Commit
RESULT versaloon_peripheral_commit(void)
{
	RESULT ret = usbtoxxx_execute_command();
	versaloon_to = VERSALOON_TIMEOUT;
	return ret;
}
// USART
RESULT versaloon_usart_init(void)
{
	return usbtousart_init();
}
RESULT versaloon_usart_fini(void)
{
	return usbtousart_fini();
}
RESULT versaloon_usart_config(uint32_t baudrate, uint8_t datalength, 
								char paritybit, char stopbit, char handshake)
{
	return usbtousart_config(VERSALOON_USART_PORT, baudrate, datalength, 
								paritybit, stopbit, handshake);
}
RESULT versaloon_usart_send(uint8_t *buf, uint16_t len)
{
	return usbtousart_send(VERSALOON_USART_PORT, buf, len);
}
RESULT versaloon_usart_receive(uint8_t *buf, uint16_t len)
{
	return usbtousart_receive(VERSALOON_USART_PORT, buf, len);
}
RESULT versaloon_usart_status(uint32_t buffer_len[2])
{
	return usbtousart_status(VERSALOON_USART_PORT, buffer_len);
}
// SPI
RESULT versaloon_spi_init(void)
{
	return usbtospi_init();
}
RESULT versaloon_spi_fini(void)
{
	return usbtospi_fini();
}
RESULT versaloon_spi_config(uint16_t kHz, uint8_t cpol, uint8_t cpha, 
							uint8_t first_bit)
{
	return usbtospi_config(VERSALOON_SPI_PORT, kHz, cpol, cpha, first_bit);
}
RESULT versaloon_spi_io(uint8_t *out, uint8_t *in, uint16_t len, uint16_t inpos, 
						uint16_t inlen)
{
	return usbtospi_io(VERSALOON_SPI_PORT, out, in, len, inpos, inlen);
}
// GPIO
RESULT versaloon_gpio_init(void)
{
	return usbtogpio_init();
}
RESULT versaloon_gpio_fini(void)
{
	return usbtogpio_fini();
}
RESULT versaloon_gpio_config(uint16_t mask, uint16_t dir_mask, 
							 uint16_t input_pull_mask)
{
	return usbtogpio_config(VERSALOON_GPIO_PORT, mask, dir_mask, input_pull_mask);
}
RESULT versaloon_gpio_in(uint16_t mask, uint16_t *value)
{
	return usbtogpio_in(VERSALOON_GPIO_PORT, mask, value);
}
RESULT versaloon_gpio_out(uint16_t mask, uint16_t value)
{
	return usbtogpio_out(VERSALOON_GPIO_PORT, mask, value);
}
// Delay
RESULT versaloon_delay_ms(uint16_t ms)
{
	return usbtodelay_delay(ms | 0x8000);
}
RESULT versaloon_delay_us(uint16_t us)
{
	return usbtodelay_delay(us & 0x7FFF);
}
// POLL
RESULT versaloon_poll_start(uint16_t retry_cnt, uint16_t interval_us)
{
	versaloon_to = VERSALOON_TIMEOUT_LONG;
	return usbtopoll_start(retry_cnt, interval_us);
}
RESULT versaloon_poll_end(void)
{
	return usbtopoll_end();
}
RESULT versaloon_poll_checkbyte(uint8_t offset, uint8_t mask, uint8_t value)
{
	return usbtopoll_checkbyte(offset, mask, value);
}
RESULT versaloon_poll_checkfail(uint8_t offset, uint8_t mask, uint8_t value)
{
	return usbtopoll_checkfail(offset, mask, value);
}
// ISSP
RESULT versaloon_issp_init(void)
{
	if (ERROR_OK != usbtoissp_init())
	{
		return ERROR_FAIL;
	}
	
	return usbtoissp_config(VERSALOON_ISSP_PORT);
}
RESULT versaloon_issp_fini(void)
{
	return usbtoissp_fini();
}
RESULT versaloon_issp_enter_program_mode(uint8_t mode)
{
	return usbtoissp_enter_program_mode(VERSALOON_ISSP_PORT, mode);
}
RESULT versaloon_issp_leave_program_mode(uint8_t mode)
{
	return usbtoissp_leave_program_mode(VERSALOON_ISSP_PORT, mode);
}
RESULT versaloon_issp_wait_and_poll(void)
{
	return usbtoissp_wait_and_poll(VERSALOON_ISSP_PORT);
}
RESULT versaloon_issp_vector(uint8_t operate, uint8_t addr, 
								uint8_t data, uint8_t *buf)
{
	return usbtoissp_vector(VERSALOON_ISSP_PORT, operate, addr, data, buf);
}
// LPCICP
RESULT versaloon_lpcicp_init(void)
{
	return usbtolpcicp_init();
}
RESULT versaloon_lpcicp_fini(void)
{
	return usbtolpcicp_fini();
}
RESULT versaloon_lpcicp_enter_program_mode(void)
{
	return usbtolpcicp_enter_program_mode(VERSALOON_LPCICP_PORT);
}
RESULT versaloon_lpcicp_in(uint8_t *buff, uint16_t len)
{
	return usbtolpcicp_in(VERSALOON_LPCICP_PORT, buff, len);
}
RESULT versaloon_lpcicp_out(uint8_t *buff, uint16_t len)
{
	return usbtolpcicp_out(VERSALOON_LPCICP_PORT, buff, len);
}
RESULT versaloon_lpcicp_poll_ready(uint8_t data, uint8_t *ret, uint8_t setmask, 
								   uint8_t clearmask, uint16_t pollcnt)
{
	return usbtolpcicp_poll_ready(VERSALOON_LPCICP_PORT, ret, data, setmask, 
								  clearmask, pollcnt);
}
// SWD
RESULT versaloon_swd_get_last_ack(uint8_t *result)
{
	if (result != NULL)
	{
		*result = usbtoswd_get_last_ack();
	}
	return ERROR_OK;
}
RESULT versaloon_swd_init(void)
{
	return usbtoswd_init();
}
RESULT versaloon_swd_fini(void)
{
	return usbtoswd_fini();
}
RESULT versaloon_swd_setpara(uint8_t trn, uint16_t retry, uint16_t dly)
{
	return usbtoswd_config(VERSALOON_SWD_PORT, trn, retry, dly);
}
RESULT versaloon_swd_seqout(uint8_t *data, uint16_t bitlen)
{
	return usbtoswd_seqout(VERSALOON_SWD_PORT, data, bitlen);
}
RESULT versaloon_swd_seqin(uint8_t *data, uint16_t bitlen)
{
	return usbtoswd_seqin(VERSALOON_SWD_PORT, data, bitlen);
}
RESULT versaloon_swd_transact(uint8_t request, uint32_t *data)
{
	return usbtoswd_transact(VERSALOON_SWD_PORT, request, data);
}
// JTAG
RESULT versaloon_jtagll_connect(void)
{
	return usbtojtagll_init();
}
RESULT versaloon_jtagll_disconnect(void)
{
	return usbtojtagll_fini();
}
RESULT versaloon_jtagll_set_frequency(uint16_t kHz)
{
	return usbtojtagll_config(VERSALOON_JTAGLL_PORT, kHz);
}
RESULT versaloon_jtagll_tms(uint8_t *tms, uint8_t bytelen)
{
	return usbtojtagll_tms(VERSALOON_JTAGLL_PORT, tms, bytelen);
}
RESULT versaloon_jtagll_tms_clocks(uint32_t bytelen, uint8_t tms)
{
	return usbtojtagll_tms_clocks(VERSALOON_JTAGLL_PORT, bytelen, tms);
}
RESULT versaloon_jtagll_scan(uint8_t* r, uint16_t bitlen, 
				uint8_t tms_before_valid, uint8_t tms_before, 
				uint8_t tms_after0, uint8_t tms_after1)
{
	return usbtojtagll_scan(VERSALOON_JTAGLL_PORT, r, bitlen, tms_before_valid, 
							tms_before, tms_after0, tms_after1);
}
RESULT versaloon_jtaghl_register_callback(jtag_callback_t send_callback, 
									 jtag_callback_t receive_callback)
{
	return usbtojtaghl_register_callback(send_callback, receive_callback);
}
RESULT versaloon_jtaghl_init(void)
{
	return usbtojtaghl_init();
}
RESULT versaloon_jtaghl_fini(void)
{
	return usbtojtaghl_fini();
}
RESULT versaloon_jtaghl_config(uint16_t kHz, uint8_t ub, uint8_t ua, 
							   uint16_t bb, uint16_t ba)
{
	return usbtojtaghl_config(VERSALOON_JTAGHL_PORT, kHz, ub, ua, bb, ba);
}
RESULT versaloon_jtaghl_tms(uint8_t *tms, uint16_t bitlen)
{
	return usbtojtaghl_tms(VERSALOON_JTAGHL_PORT, tms, bitlen);
}
RESULT versaloon_jtaghl_runtest(uint32_t cycles)
{
	return usbtojtaghl_runtest(VERSALOON_JTAGHL_PORT, cycles);
}
RESULT versaloon_jtaghl_ir(uint8_t *ir, uint8_t len, uint8_t idle, 
							uint8_t want_ret)
{
	return usbtojtaghl_ir(VERSALOON_JTAGHL_PORT, ir, len, idle, want_ret);
}
RESULT versaloon_jtaghl_dr(uint8_t *dr, uint16_t len, uint8_t idle, 
							uint8_t want_ret)
{
	return usbtojtaghl_dr(VERSALOON_JTAGHL_PORT, dr, len, idle, want_ret);
}
// MSP430_JTAG
RESULT versaloon_msp430jtag_init(void)
{
	return usbtomsp430jtag_init();
}
RESULT versaloon_msp430jtag_fini(void)
{
	return usbtomsp430jtag_fini();
}
RESULT versaloon_msp430jtag_config(uint8_t has_test)
{
	return usbtomsp430jtag_config(VERSALOON_MSP430_JTAG_PORT, has_test);
}
RESULT versaloon_msp430jtag_ir(uint8_t *ir, uint8_t want_ret)
{
	return usbtomsp430jtag_ir(VERSALOON_MSP430_JTAG_PORT, ir, want_ret);
}
RESULT versaloon_msp430jtag_dr(uint32_t *dr, uint8_t len, uint8_t want_ret)
{
	return usbtomsp430jtag_dr(VERSALOON_MSP430_JTAG_PORT, dr, len, want_ret);
}
RESULT versaloon_msp430jtag_tclk(uint8_t value)
{
	return usbtomsp430jtag_tclk(VERSALOON_MSP430_JTAG_PORT, value);
}
RESULT versaloon_msp430jtag_reset(void)
{
	return usbtomsp430jtag_reset(VERSALOON_MSP430_JTAG_PORT);
}
RESULT versaloon_msp430jtag_poll(uint32_t dr, uint32_t mask, uint32_t value, 
						uint8_t len, uint16_t poll_cnt, uint8_t toggle_tclk)
{
	return usbtomsp430jtag_poll(VERSALOON_MSP430_JTAG_PORT, dr, mask, value, 
								len, poll_cnt, toggle_tclk);
}
RESULT versaloon_msp430jtag_tclk_strobe(uint16_t cnt)
{
	return usbtomsp430jtag_tclk_strobe(VERSALOON_MSP430_JTAG_PORT, cnt);
}
// MSP430_SBW
RESULT versaloon_msp430sbw_init(void)
{
	return usbtomsp430sbw_init();
}
RESULT versaloon_msp430sbw_fini(void)
{
	return usbtomsp430sbw_fini();
}
RESULT versaloon_msp430sbw_config(uint8_t has_test)
{
	if (has_test)
	{
		return usbtomsp430sbw_config(VERSALOON_MSP430_JTAG_PORT);
	}
	else
	{
		return ERROR_FAIL;
	}
}
RESULT versaloon_msp430sbw_ir(uint8_t *ir, uint8_t want_ret)
{
	return usbtomsp430sbw_ir(VERSALOON_MSP430_JTAG_PORT, ir, want_ret);
}
RESULT versaloon_msp430sbw_dr(uint32_t *dr, uint8_t len, uint8_t want_ret)
{
	return usbtomsp430sbw_dr(VERSALOON_MSP430_JTAG_PORT, dr, len, want_ret);
}
RESULT versaloon_msp430sbw_tclk(uint8_t value)
{
	return usbtomsp430sbw_tclk(VERSALOON_MSP430_JTAG_PORT, value);
}
RESULT versaloon_msp430sbw_reset(void)
{
	return usbtomsp430sbw_reset(VERSALOON_MSP430_JTAG_PORT);
}
RESULT versaloon_msp430sbw_poll(uint32_t dr, uint32_t mask, uint32_t value, 
						uint8_t len, uint16_t poll_cnt, uint8_t toggle_tclk)
{
	return usbtomsp430sbw_poll(VERSALOON_MSP430_JTAG_PORT, dr, mask, value, 
							   len, poll_cnt, toggle_tclk);
}
RESULT versaloon_msp430sbw_tclk_strobe(uint16_t cnt)
{
	return usbtomsp430sbw_tclk_strobe(VERSALOON_MSP430_JTAG_PORT, cnt);
}
// C2
RESULT versaloon_c2_init(void)
{
	if (ERROR_OK != usbtoc2_init())
	{
		return ERROR_FAIL;
	}
	
	return usbtoc2_config(VERSALOON_C2_PORT);
}
RESULT versaloon_c2_fini(void)
{
	return usbtoc2_fini();
}
RESULT versaloon_c2_addr_write(uint8_t addr)
{
	return usbtoc2_writeaddr(VERSALOON_C2_PORT, addr);
}
RESULT versaloon_c2_addr_read(uint8_t *data)
{
	return usbtoc2_readaddr(VERSALOON_C2_PORT, data);
}
RESULT versaloon_c2_data_write(uint8_t *data, uint8_t len)
{
	return usbtoc2_data(VERSALOON_C2_PORT, 0, len, data);
}
RESULT versaloon_c2_data_read(uint8_t *data, uint8_t len)
{
	return usbtoc2_data(VERSALOON_C2_PORT, 1, len, data);
}
// I2C
RESULT versaloon_i2c_init(void)
{
	return usbtoi2c_init();
}
RESULT versaloon_i2c_fini(void)
{
	return usbtoi2c_fini();
}
RESULT versaloon_i2c_set_speed(uint16_t kHz, uint16_t dead_cnt, 
								uint16_t byte_interval)
{
	return usbtoi2c_set_speed(VERSALOON_I2C_PORT, kHz, dead_cnt, byte_interval);
}
RESULT versaloon_i2c_read(uint16_t chip_addr, uint8_t *data, 
							uint16_t data_len, uint8_t stop)
{
	return usbtoi2c_read(VERSALOON_I2C_PORT, chip_addr, data, data_len, stop);
}
RESULT versaloon_i2c_write(uint16_t chip_addr, uint8_t *data, 
							uint16_t data_len, uint8_t stop)
{
	return usbtoi2c_write(VERSALOON_I2C_PORT, chip_addr, data, data_len, stop);
}
// SWIM
RESULT versaloon_swim_init(void)
{
	return usbtoswim_init();
}
RESULT versaloon_swim_fini(void)
{
	return usbtoswim_fini();
}
RESULT versaloon_swim_set_param(uint8_t mHz, uint8_t cnt0, uint8_t cnt1)
{
	return usbtoswim_set_param(VERSALOON_SWIM_PORT, mHz, cnt0, cnt1);
}
RESULT versaloon_swim_srst(void)
{
	return usbtoswim_srst(VERSALOON_SWIM_PORT);
}
RESULT versaloon_swim_wotf(uint8_t *data, uint16_t bytelen, uint32_t addr)
{
	return usbtoswim_wotf(VERSALOON_SWIM_PORT, data, bytelen, addr);
}
RESULT versaloon_swim_rotf(uint8_t *data, uint16_t bytelen, uint32_t addr)
{
	return usbtoswim_rotf(VERSALOON_SWIM_PORT, data, bytelen, addr);
}
RESULT versaloon_swim_sync(uint8_t mHz)
{
	return usbtoswim_sync(VERSALOON_SWIM_PORT, mHz);
}
RESULT versaloon_swim_enable(void)
{
	return usbtoswim_enable(VERSALOON_SWIM_PORT);
}



RESULT versaloon_init_capability(void *p)
{
	struct programmer_info_t *t = (struct programmer_info_t *)p;
	struct interfaces_info_t *i = &(t->interfaces);
	
	t->init = versaloon_init;
	t->fini = versaloon_fini;
	
	t->interfaces_mask = (SPI | GPIO | ISSP | JTAG_LL | JTAG_HL | SWIM 
						| C2 | MSP430_JTAG | MSP430_SBW | LPC_ICP | SWD);
	
	// USART
	i->usart.usart_init = versaloon_usart_init;
	i->usart.usart_fini = versaloon_usart_fini;
	i->usart.usart_config = versaloon_usart_config;
	i->usart.usart_send = versaloon_usart_send;
	i->usart.usart_receive = versaloon_usart_receive;
	i->usart.usart_status = versaloon_usart_status;
	
	// SPI
	i->spi.spi_init = versaloon_spi_init;
	i->spi.spi_fini = versaloon_spi_fini;
	i->spi.spi_config = versaloon_spi_config;
	i->spi.spi_io = versaloon_spi_io;
	
	// GPIO
	i->gpio.gpio_init = versaloon_gpio_init;
	i->gpio.gpio_fini = versaloon_gpio_fini;
	i->gpio.gpio_config = versaloon_gpio_config;
	i->gpio.gpio_in = versaloon_gpio_in;
	i->gpio.gpio_out = versaloon_gpio_out;
	
	// Delay
	i->delay.delayms = versaloon_delay_ms;
	i->delay.delayus = versaloon_delay_us;
	
	// ISSP
	i->issp.issp_init = versaloon_issp_init;
	i->issp.issp_fini = versaloon_issp_fini;
	i->issp.issp_enter_program_mode = versaloon_issp_enter_program_mode;
	i->issp.issp_leave_program_mode = versaloon_issp_leave_program_mode;
	i->issp.issp_wait_and_poll = versaloon_issp_wait_and_poll;
	i->issp.issp_vector = versaloon_issp_vector;
	
	// LPCICP
	i->lpcicp.lpcicp_init = versaloon_lpcicp_init;
	i->lpcicp.lpcicp_fini = versaloon_lpcicp_fini;
	i->lpcicp.lpcicp_enter_program_mode = versaloon_lpcicp_enter_program_mode;
	i->lpcicp.lpcicp_in = versaloon_lpcicp_in;
	i->lpcicp.lpcicp_out = versaloon_lpcicp_out;
	i->lpcicp.lpcicp_poll_ready = versaloon_lpcicp_poll_ready;
	
	// Target voltage
	i->target_voltage.get_target_voltage = versaloon_get_target_voltage;
	i->target_voltage.set_target_voltage = versaloon_set_target_voltage;
	
	// JTAG_HL & SWD
	i->swd.swd_init = versaloon_swd_init;
	i->swd.swd_fini = versaloon_swd_fini;
	i->swd.swd_seqout = versaloon_swd_seqout;
	i->swd.swd_seqin = versaloon_swd_seqin;
	i->swd.swd_transact = versaloon_swd_transact;
	i->swd.swd_setpara = versaloon_swd_setpara;
	i->swd.swd_get_last_ack = versaloon_swd_get_last_ack;

	i->jtag_hl.jtag_hl_init = versaloon_jtaghl_init;
	i->jtag_hl.jtag_hl_fini = versaloon_jtaghl_fini;
	i->jtag_hl.jtag_hl_config= versaloon_jtaghl_config;
	i->jtag_hl.jtag_hl_tms = versaloon_jtaghl_tms;
	i->jtag_hl.jtag_hl_runtest = versaloon_jtaghl_runtest;
	i->jtag_hl.jtag_hl_ir = versaloon_jtaghl_ir;
	i->jtag_hl.jtag_hl_dr = versaloon_jtaghl_dr;
	i->jtag_hl.jtag_hl_register_callback = versaloon_jtaghl_register_callback;
	
	// JTAG_LL
	i->jtag_ll.jtag_ll_init = versaloon_jtagll_connect;
	i->jtag_ll.jtag_ll_fini = versaloon_jtagll_disconnect;
	i->jtag_ll.jtag_ll_set_frequency = versaloon_jtagll_set_frequency;
	i->jtag_ll.jtag_ll_tms = versaloon_jtagll_tms;
	i->jtag_ll.jtag_ll_tms_clocks = versaloon_jtagll_tms_clocks;
	i->jtag_ll.jtag_ll_scan = versaloon_jtagll_scan;
	
	// MSP430_JTAG
	i->msp430jtag.msp430jtag_init = versaloon_msp430jtag_init;
	i->msp430jtag.msp430jtag_fini = versaloon_msp430jtag_fini;
	i->msp430jtag.msp430jtag_config = versaloon_msp430jtag_config;
	i->msp430jtag.msp430jtag_ir = versaloon_msp430jtag_ir;
	i->msp430jtag.msp430jtag_dr = versaloon_msp430jtag_dr;
	i->msp430jtag.msp430jtag_tclk = versaloon_msp430jtag_tclk;
	i->msp430jtag.msp430jtag_tclk_strobe = versaloon_msp430jtag_tclk_strobe;
	i->msp430jtag.msp430jtag_reset = versaloon_msp430jtag_reset;
	i->msp430jtag.msp430jtag_poll = versaloon_msp430jtag_poll;
	
	// MSP430_SBW
	i->msp430sbw.msp430sbw_init = versaloon_msp430sbw_init;
	i->msp430sbw.msp430sbw_fini = versaloon_msp430sbw_fini;
	i->msp430sbw.msp430sbw_config = versaloon_msp430sbw_config;
	i->msp430sbw.msp430sbw_ir = versaloon_msp430sbw_ir;
	i->msp430sbw.msp430sbw_dr = versaloon_msp430sbw_dr;
	i->msp430sbw.msp430sbw_tclk = versaloon_msp430sbw_tclk;
	i->msp430sbw.msp430sbw_tclk_strobe = versaloon_msp430sbw_tclk_strobe;
	i->msp430sbw.msp430sbw_reset = versaloon_msp430sbw_reset;
	i->msp430sbw.msp430sbw_poll = versaloon_msp430sbw_poll;
	
	// C2
	i->c2.c2_init = versaloon_c2_init;
	i->c2.c2_fini = versaloon_c2_fini;
	i->c2.c2_addr_write = versaloon_c2_addr_write;
	i->c2.c2_addr_read = versaloon_c2_addr_read;
	i->c2.c2_data_write = versaloon_c2_data_write;
	i->c2.c2_data_read = versaloon_c2_data_read;
	
	// I2C
	i->i2c.i2c_init = versaloon_i2c_init;
	i->i2c.i2c_fini = versaloon_i2c_fini;
	i->i2c.i2c_set_speed = versaloon_i2c_set_speed;
	i->i2c.i2c_read = versaloon_i2c_read;
	i->i2c.i2c_write = versaloon_i2c_write;
	
	// SWIM
	i->swim.swim_init = versaloon_swim_init;
	i->swim.swim_fini = versaloon_swim_fini;
	i->swim.swim_set_param = versaloon_swim_set_param;
	i->swim.swim_srst = versaloon_swim_srst;
	i->swim.swim_wotf = versaloon_swim_wotf;
	i->swim.swim_rotf = versaloon_swim_rotf;
	i->swim.swim_sync = versaloon_swim_sync;
	i->swim.swim_enable = versaloon_swim_enable;
	
	// POLL
	i->poll.poll_start = versaloon_poll_start;
	i->poll.poll_end = versaloon_poll_end;
	i->poll.poll_checkbyte = versaloon_poll_checkbyte;
	i->poll.poll_checkfail = versaloon_poll_checkfail;
	
	i->peripheral_commit= versaloon_peripheral_commit;
	
	// Mass-product
	t->download_mass_product_data = versaloon_download_mass_product_data;
	t->query_mass_product_data_size = versaloon_query_mass_product_data_size;
	
	// firmware update
	t->enter_firmware_update_mode = versaloon_enter_firmware_update_mode;
	
	return ERROR_OK;
}

uint32_t versaloon_display_programmer(void)
{
	printf(_GETTEXT("Supported Programmer by Versaloon driver:\n"));
	return print_usb_devices(versaloon_vid, versaloon_pid, 
		VERSALOON_SERIALSTRING_INDEX, versaloon_serialstring, VERSALOON_STRING);
}

