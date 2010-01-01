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

RESULT versaloon_cmd_get_voltage(uint8_t argc, const char *argv[]);
RESULT versaloon_cmd_power_out(uint8_t argc, const char *argv[]);
struct misc_cmd_t versaloon_cmd[] = 
{
	// voltage
	{
		"get target voltage, format: voltage", 
		"voltage", 
		versaloon_cmd_get_voltage
	}, 
	// powerout
	{
		"output power, format: powerout VOLTAGE_IN_MV", 
		"powerout", 
		versaloon_cmd_power_out
	}, 
	// null
	{
		NULL, 
		NULL, 
		NULL
	}
};

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



// versaloon_cmd
RESULT versaloon_cmd_get_voltage(uint8_t argc, const char *argv[])
{
	uint16_t voltage;
	const uint8_t cmd_index = 0;
	argv = argv;
	
	if (argc != 0)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CMD), 
					versaloon_cmd[cmd_index].cmd_name);
		LOG_INFO("%s\n", versaloon_cmd[cmd_index].help_str);
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != versaloon_get_target_voltage(&voltage))
	{
		return ERROR_FAIL;
	}
	LOG_INFO(_GETTEXT(INFOMSG_TARGET_VOLTAGE), voltage / 1000.0);
	
	return ERROR_OK;
}
RESULT versaloon_cmd_power_out(uint8_t argc, const char *argv[])
{
	uint16_t voltage = 0;
	const uint8_t cmd_index = 1;
	
	if (argc != 1)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CMD), 
					versaloon_cmd[cmd_index].cmd_name);
		LOG_INFO("%s\n", versaloon_cmd[cmd_index].help_str);
		return ERROR_FAIL;
	}
	
	voltage = (uint16_t)strtoul(argv[0], NULL, 0);
	
	usbtopwr_init();
	usbtopwr_config(VERSALOON_POWER_PORT);
	usbtopwr_output(VERSALOON_POWER_PORT, voltage);
	usbtopwr_fini();
	return usbtoxxx_execute_command();
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
		LOG_WARNING(_GETTEXT(ERRMSG_FAILURE_OPERATION_MESSAGE), 
				  "get checksum of mass product data", "verify omitted!!");
	}
	else
	{
		if (checksum != inlen)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_04X), 
						  "checksum", inlen, checksum);
			return ERRCODE_FAILURE_VERIFY_TARGET;
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
RESULT versaloon_poll_start(uint16_t retry, uint16_t interval_us)
{
	return usbtopoll_start(retry, interval_us);
}
RESULT versaloon_poll_end(void)
{
	return usbtopoll_end();
}
RESULT versaloon_poll_checkbyte(uint8_t offset, uint8_t mask, uint8_t value)
{
	return usbtopoll_checkbyte(offset, mask, value);
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
// SWJ
RESULT versaloon_swj_commit(uint8_t *result)
{
	RESULT ret;
	
	ret = usbtoxxx_execute_command();
	if (result != NULL)
	{
		*result = usbtoswj_get_last_ack();
	}
	
	return ret;
}
RESULT versaloon_swj_init(void)
{
	return usbtoswj_init();
}
RESULT versaloon_swj_fini(void)
{
	return usbtoswj_fini();
}
RESULT versaloon_swj_setpara(uint8_t trn, uint16_t retry, uint16_t dly)
{
	return usbtoswj_config(VERSALOON_SWJ_PORT, trn, retry, dly);
}
RESULT versaloon_swj_seqout(uint8_t *data, uint16_t bitlen)
{
	return usbtoswj_seqout(VERSALOON_SWJ_PORT, data, bitlen);
}
RESULT versaloon_swj_seqin(uint8_t *data, uint16_t bitlen)
{
	return usbtoswj_seqin(VERSALOON_SWJ_PORT, data, bitlen);
}
RESULT versaloon_swj_transact(uint8_t request, uint32_t *data)
{
	return usbtoswj_transact(VERSALOON_SWJ_PORT, request, data);
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
RESULT versaloon_i2c_set_speed(uint16_t kHz)
{
	return usbtoi2c_set_speed(VERSALOON_I2C_PORT, kHz);
}
RESULT versaloon_i2c_read(uint16_t chip_addr, uint8_t chip_addr_len, 
							uint8_t *data, uint16_t data_len, uint8_t stop)
{
	return usbtoi2c_read(VERSALOON_I2C_PORT, chip_addr, chip_addr_len, data, 
						 data_len, stop);
}
RESULT versaloon_i2c_write(uint16_t chip_addr, uint8_t chip_addr_len, 
							uint8_t *data, uint16_t data_len, uint8_t stop)
{
	return usbtoi2c_write(VERSALOON_I2C_PORT, chip_addr, chip_addr_len, data, 
						  data_len, stop);
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
RESULT versaloon_swim_out(uint8_t data, uint8_t bitlen)
{
	return usbtoswim_out(VERSALOON_SWIM_PORT, data, bitlen);
}
RESULT versaloon_swim_in(uint8_t *data, uint8_t bytelen)
{
	return usbtoswim_in(VERSALOON_SWIM_PORT, data, bytelen);
}
// Commit
RESULT versaloon_peripheral_commit(void)
{
	return usbtoxxx_execute_command();
}



RESULT versaloon_init_capability(void *p)
{
	struct programmer_info_t *t = (struct programmer_info_t *)p;
	
	t->init = versaloon_init;
	t->fini = versaloon_fini;
	
	t->interfaces = (SPI | GPIO | ISSP | JTAG_LL | JTAG_HL | SWIM 
						| C2 | MSP430_JTAG | MSP430_SBW | LPC_ICP | SWJ);
	
	// SPI
	t->spi_init = versaloon_spi_init;
	t->spi_fini = versaloon_spi_fini;
	t->spi_config = versaloon_spi_config;
	t->spi_io = versaloon_spi_io;
	
	// GPIO
	t->gpio_init = versaloon_gpio_init;
	t->gpio_fini = versaloon_gpio_fini;
	t->gpio_config = versaloon_gpio_config;
	t->gpio_in = versaloon_gpio_in;
	t->gpio_out = versaloon_gpio_out;
	
	// Delay
	t->delayms = versaloon_delay_ms;
	t->delayus = versaloon_delay_us;
	
	t->peripheral_commit= versaloon_peripheral_commit;
	
	// ISSP
	t->issp_init = versaloon_issp_init;
	t->issp_fini = versaloon_issp_fini;
	t->issp_enter_program_mode = versaloon_issp_enter_program_mode;
	t->issp_leave_program_mode = versaloon_issp_leave_program_mode;
	t->issp_wait_and_poll = versaloon_issp_wait_and_poll;
	t->issp_vector = versaloon_issp_vector;
	t->issp_commit = versaloon_peripheral_commit;
	
	// LPCICP
	t->lpcicp_init = versaloon_lpcicp_init;
	t->lpcicp_fini = versaloon_lpcicp_fini;
	t->lpcicp_enter_program_mode = versaloon_lpcicp_enter_program_mode;
	t->lpcicp_in = versaloon_lpcicp_in;
	t->lpcicp_out = versaloon_lpcicp_out;
	t->lpcicp_poll_ready = versaloon_lpcicp_poll_ready;
	t->lpcicp_commit = versaloon_peripheral_commit;
	
	// Target voltage
	t->get_target_voltage = versaloon_get_target_voltage;
	
	// JTAG_HL & SWJ
	t->swj_init = versaloon_swj_init;
	t->swj_fini = versaloon_swj_fini;
	t->swj_seqout = versaloon_swj_seqout;
	t->swj_seqin = versaloon_swj_seqin;
	t->swj_transact = versaloon_swj_transact;
	t->swj_setpara = versaloon_swj_setpara;
	t->swj_commit = versaloon_swj_commit;

	t->jtag_hl_init = versaloon_jtaghl_init;
	t->jtag_hl_fini = versaloon_jtaghl_fini;
	t->jtag_hl_config= versaloon_jtaghl_config;
	t->jtag_hl_tms = versaloon_jtaghl_tms;
	t->jtag_hl_runtest = versaloon_jtaghl_runtest;
	t->jtag_hl_ir = versaloon_jtaghl_ir;
	t->jtag_hl_dr = versaloon_jtaghl_dr;
	t->jtag_hl_commit = versaloon_peripheral_commit;
	t->jtag_hl_register_callback = versaloon_jtaghl_register_callback;
	
	// JTAG_LL
	t->jtag_ll_init = versaloon_jtagll_connect;
	t->jtag_ll_fini = versaloon_jtagll_disconnect;
	t->jtag_ll_set_frequency = versaloon_jtagll_set_frequency;
	t->jtag_ll_tms = versaloon_jtagll_tms;
	t->jtag_ll_tms_clocks = versaloon_jtagll_tms_clocks;
	t->jtag_ll_scan = versaloon_jtagll_scan;
	t->jtag_ll_commit = versaloon_peripheral_commit;
	
	// MSP430_JTAG
	t->msp430jtag_init = versaloon_msp430jtag_init;
	t->msp430jtag_fini = versaloon_msp430jtag_fini;
	t->msp430jtag_config = versaloon_msp430jtag_config;
	t->msp430jtag_ir = versaloon_msp430jtag_ir;
	t->msp430jtag_dr = versaloon_msp430jtag_dr;
	t->msp430jtag_tclk = versaloon_msp430jtag_tclk;
	t->msp430jtag_tclk_strobe = versaloon_msp430jtag_tclk_strobe;
	t->msp430jtag_reset = versaloon_msp430jtag_reset;
	t->msp430jtag_poll = versaloon_msp430jtag_poll;
	
	// MSP430_SBW
	t->msp430sbw_init = versaloon_msp430sbw_init;
	t->msp430sbw_fini = versaloon_msp430sbw_fini;
	t->msp430sbw_config = versaloon_msp430sbw_config;
	t->msp430sbw_ir = versaloon_msp430sbw_ir;
	t->msp430sbw_dr = versaloon_msp430sbw_dr;
	t->msp430sbw_tclk = versaloon_msp430sbw_tclk;
	t->msp430sbw_tclk_strobe = versaloon_msp430sbw_tclk_strobe;
	t->msp430sbw_reset = versaloon_msp430sbw_reset;
	t->msp430sbw_poll = versaloon_msp430sbw_poll;
	
	// C2
	t->c2_init = versaloon_c2_init;
	t->c2_fini = versaloon_c2_fini;
	t->c2_addr_write = versaloon_c2_addr_write;
	t->c2_addr_read = versaloon_c2_addr_read;
	t->c2_data_write = versaloon_c2_data_write;
	t->c2_data_read = versaloon_c2_data_read;
	t->c2_commit = versaloon_peripheral_commit;
	
	// I2C
	t->i2c_init = versaloon_i2c_init;
	t->i2c_fini = versaloon_i2c_fini;
	t->i2c_set_speed = versaloon_i2c_set_speed;
	t->i2c_read = versaloon_i2c_read;
	t->i2c_write = versaloon_i2c_write;
	
	// SWIM
	t->swim_init = versaloon_swim_init;
	t->swim_fini = versaloon_swim_fini;
	t->swim_set_param = versaloon_swim_set_param;
	t->swim_out = versaloon_swim_out;
	t->swim_in = versaloon_swim_in;
	
	// POLL
	t->poll_start = versaloon_poll_start;
	t->poll_end = versaloon_poll_end;
	t->poll_checkbyte = versaloon_poll_checkbyte;
	
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

