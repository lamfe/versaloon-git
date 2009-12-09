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
#include "vsllink/vsllink.h"


const char *versaloon_hardwares[] = 
{
	"Versaloon_Full",		// 1
	"Versaloon_Mini",		// 2
	"Versaloon_Nano",		// 3
};

uint8_t *versaloon_buf = NULL;
uint16_t versaloon_buf_size = 256;

versaloon_pending_t versaloon_pending[VERSALOON_MAX_PENDING_NUMBER];
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
misc_cmd_t versaloon_cmd[] = 
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
							 uint16_t want_pos, uint16_t want_size, 
							 uint8_t *buffer, uint8_t collect)
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
							(char *)versaloon_buf, versaloon_buf_size, 
							versaloon_to);
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
	int verbosity_tmp;
	uint32_t timeout_tmp;
	
	versaloon_device_handle = find_usb_device(versaloon_vid, versaloon_pid, 
											  versaloon_interface, 
											  VERSALOON_SERIALSTRING_INDEX, 
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
	verbosity_tmp = verbosity;
	timeout_tmp = versaloon_to;
	// not output error message when connectting
	verbosity = -1;
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
	verbosity = verbosity_tmp;
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
	
	return ERROR_OK;
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
RESULT versaloon_issp_vector(uint8_t operate, uint8_t addr, uint8_t data, uint8_t *buf)
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
// JTAG
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
RESULT versaloon_jtaghl_ir(uint8_t *ir, uint8_t len, uint8_t idle, uint8_t want_ret)
{
	return usbtojtaghl_ir(VERSALOON_JTAGHL_PORT, ir, len, idle, want_ret);
}
RESULT versaloon_jtaghl_dr(uint8_t *dr, uint16_t len, uint8_t idle, uint8_t want_ret)
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
	return usbtoc2_addr(VERSALOON_C2_PORT, addr);
}
RESULT versaloon_c2_data_write(uint8_t *data, uint8_t len)
{
	return usbtoc2_data(VERSALOON_C2_PORT, 0, len, data);
}
RESULT versaloon_c2_data_read(uint8_t *data, uint8_t len)
{
	return usbtoc2_data(VERSALOON_C2_PORT, 1, len, data);
}
RESULT versaloon_c2_addr_poll(uint8_t mask, uint8_t value, uint16_t poll_cnt)
{
	return usbtoc2_addr_poll(VERSALOON_C2_PORT, mask, value, poll_cnt);
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
RESULT versaloon_i2c_read(uint16_t chip_addr, uint8_t chip_addr_len, uint8_t *data, 
						  uint16_t data_len, uint8_t stop)
{
	return usbtoi2c_read(VERSALOON_I2C_PORT, chip_addr, chip_addr_len, data, 
						 data_len, stop);
}
RESULT versaloon_i2c_write(uint16_t chip_addr, uint8_t chip_addr_len, uint8_t *data, 
						   uint16_t data_len, uint8_t stop)
{
	return usbtoi2c_write(VERSALOON_I2C_PORT, chip_addr, chip_addr_len, data, 
						  data_len, stop);
}
// Commit
RESULT versaloon_peripheral_commit(void)
{
	return usbtoxxx_execute_command();
}



RESULT versaloon_init_capability(void *p)
{
	((programmer_info_t *)p)->init = versaloon_init;
	((programmer_info_t *)p)->fini = versaloon_fini;
	
	((programmer_info_t *)p)->interfaces = (SPI | GPIO | ISSP | JTAG_LL 
											 | JTAG_HL | C2 | MSP430_JTAG 
											 | MSP430_SBW | LPC_ICP | SWJ);
	
	// SPI
	((programmer_info_t *)p)->spi_init = versaloon_spi_init;
	((programmer_info_t *)p)->spi_fini = versaloon_spi_fini;
	((programmer_info_t *)p)->spi_config = versaloon_spi_config;
	((programmer_info_t *)p)->spi_io = versaloon_spi_io;
	
	// GPIO
	((programmer_info_t *)p)->gpio_init = versaloon_gpio_init;
	((programmer_info_t *)p)->gpio_fini = versaloon_gpio_fini;
	((programmer_info_t *)p)->gpio_config = versaloon_gpio_config;
	((programmer_info_t *)p)->gpio_in = versaloon_gpio_in;
	((programmer_info_t *)p)->gpio_out = versaloon_gpio_out;
	
	// Delay
	((programmer_info_t *)p)->delayms = versaloon_delay_ms;
	((programmer_info_t *)p)->delayus = versaloon_delay_us;
	
	((programmer_info_t *)p)->peripheral_commit= versaloon_peripheral_commit;
	
	// ISSP
	((programmer_info_t *)p)->issp_init = versaloon_issp_init;
	((programmer_info_t *)p)->issp_fini = versaloon_issp_fini;
	((programmer_info_t *)p)->issp_enter_program_mode = 
											versaloon_issp_enter_program_mode;
	((programmer_info_t *)p)->issp_leave_program_mode = 
											versaloon_issp_leave_program_mode;
	((programmer_info_t *)p)->issp_wait_and_poll = 
											versaloon_issp_wait_and_poll;
	((programmer_info_t *)p)->issp_vector = versaloon_issp_vector;
	((programmer_info_t *)p)->issp_commit = versaloon_peripheral_commit;
	
	// LPCICP
	((programmer_info_t *)p)->lpcicp_init = versaloon_lpcicp_init;
	((programmer_info_t *)p)->lpcicp_fini = versaloon_lpcicp_fini;
	((programmer_info_t *)p)->lpcicp_enter_program_mode = versaloon_lpcicp_enter_program_mode;
	((programmer_info_t *)p)->lpcicp_in = versaloon_lpcicp_in;
	((programmer_info_t *)p)->lpcicp_out = versaloon_lpcicp_out;
	((programmer_info_t *)p)->lpcicp_poll_ready = versaloon_lpcicp_poll_ready;
	((programmer_info_t *)p)->lpcicp_commit = versaloon_peripheral_commit;
	
	// Target voltage
	((programmer_info_t *)p)->get_target_voltage = 
											versaloon_get_target_voltage;
	
	// JTAG_HL & SWJ
#if 1
	((programmer_info_t *)p)->swj_init = vsllink_swj_connect;
	((programmer_info_t *)p)->swj_fini = vsllink_swj_disconnect;
	((programmer_info_t *)p)->swj_seqout = vsllink_swj_seqout;
	((programmer_info_t *)p)->swj_seqin = vsllink_swj_seqin;
	((programmer_info_t *)p)->swj_transact = vsllink_swj_transact;
	((programmer_info_t *)p)->swj_setpara = vsllink_swj_setpara;
	((programmer_info_t *)p)->swj_commit = vsllink_swj_commit;

	((programmer_info_t *)p)->jtag_hl_init = versaloon_jtaghl_init;
	((programmer_info_t *)p)->jtag_hl_fini = versaloon_jtaghl_fini;
	((programmer_info_t *)p)->jtag_hl_config= versaloon_jtaghl_config;
	((programmer_info_t *)p)->jtag_hl_tms = versaloon_jtaghl_tms;
	((programmer_info_t *)p)->jtag_hl_runtest = versaloon_jtaghl_runtest;
	((programmer_info_t *)p)->jtag_hl_ir = versaloon_jtaghl_ir;
	((programmer_info_t *)p)->jtag_hl_dr = versaloon_jtaghl_dr;
	((programmer_info_t *)p)->jtag_hl_commit = versaloon_peripheral_commit;
	((programmer_info_t *)p)->jtag_hl_register_callback = 
											versaloon_jtaghl_register_callback;
#else
	((programmer_info_t *)p)->swj_init = vsllink_swj_connect;
	((programmer_info_t *)p)->swj_fini = vsllink_swj_disconnect;
	((programmer_info_t *)p)->swj_seqout = vsllink_swj_seqout;
	((programmer_info_t *)p)->swj_seqin = vsllink_swj_seqin;
	((programmer_info_t *)p)->swj_transact = vsllink_swj_transact;
	((programmer_info_t *)p)->swj_setpara = vsllink_swj_setpara;
	((programmer_info_t *)p)->swj_commit = vsllink_swj_commit;

	((programmer_info_t *)p)->jtag_hl_init = vsllink_jtag_connect;
	((programmer_info_t *)p)->jtag_hl_fini = vsllink_jtaghl_disconnect;
	((programmer_info_t *)p)->jtag_hl_config= vsllink_jtaghl_config;
	((programmer_info_t *)p)->jtag_hl_runtest = vsllink_jtaghl_runtest;
	((programmer_info_t *)p)->jtag_hl_tms = vsllink_jtaghl_tms;
	((programmer_info_t *)p)->jtag_hl_ir = vsllink_jtaghl_ir;
	((programmer_info_t *)p)->jtag_hl_dr = vsllink_jtaghl_dr;
	((programmer_info_t *)p)->jtag_hl_register_callback = 
											vsllink_jtaghl_register_callback;
	((programmer_info_t *)p)->jtag_hl_commit = vsllink_jtaghl_commit;
#endif
	
	// JTAG_LL
	((programmer_info_t *)p)->jtag_ll_init = vsllink_jtag_connect;
	((programmer_info_t *)p)->jtag_ll_fini = vsllink_jtagll_disconnect;
	((programmer_info_t *)p)->jtag_ll_set_frequency = vsllink_jtag_set_freq;
	((programmer_info_t *)p)->jtag_ll_tms = vsllink_jtagll_tms;
	((programmer_info_t *)p)->jtag_ll_tms_clocks = vsllink_jtagll_tms_clocks;
	((programmer_info_t *)p)->jtag_ll_xr = vsllink_jtagll_xr;
	((programmer_info_t *)p)->jtag_ll_aux_io_init = versaloon_null;
	((programmer_info_t *)p)->jtag_ll_aux_io_fini = versaloon_null;
	((programmer_info_t *)p)->jtag_ll_aux_io_config = 
											vsllink_jtag_auxio_config;
	((programmer_info_t *)p)->jtag_ll_aux_io_out = vsllink_jtag_auxio_out;
	((programmer_info_t *)p)->jtag_ll_aux_io_in = vsllink_jtag_auxio_in;
	((programmer_info_t *)p)->jtag_ll_commit = vsllink_jtagll_commit;
	
	// MSP430_JTAG
	((programmer_info_t *)p)->msp430jtag_init = versaloon_msp430jtag_init;
	((programmer_info_t *)p)->msp430jtag_fini = versaloon_msp430jtag_fini;
	((programmer_info_t *)p)->msp430jtag_config = versaloon_msp430jtag_config;
	((programmer_info_t *)p)->msp430jtag_ir = versaloon_msp430jtag_ir;
	((programmer_info_t *)p)->msp430jtag_dr = versaloon_msp430jtag_dr;
	((programmer_info_t *)p)->msp430jtag_tclk = versaloon_msp430jtag_tclk;
	((programmer_info_t *)p)->msp430jtag_tclk_strobe = 
											versaloon_msp430jtag_tclk_strobe;
	((programmer_info_t *)p)->msp430jtag_reset = versaloon_msp430jtag_reset;
	((programmer_info_t *)p)->msp430jtag_poll = versaloon_msp430jtag_poll;
	
	// MSP430_SBW
	((programmer_info_t *)p)->msp430sbw_init = versaloon_msp430sbw_init;
	((programmer_info_t *)p)->msp430sbw_fini = versaloon_msp430sbw_fini;
	((programmer_info_t *)p)->msp430sbw_config = versaloon_msp430sbw_config;
	((programmer_info_t *)p)->msp430sbw_ir = versaloon_msp430sbw_ir;
	((programmer_info_t *)p)->msp430sbw_dr = versaloon_msp430sbw_dr;
	((programmer_info_t *)p)->msp430sbw_tclk = versaloon_msp430sbw_tclk;
	((programmer_info_t *)p)->msp430sbw_tclk_strobe = 
											versaloon_msp430sbw_tclk_strobe;
	((programmer_info_t *)p)->msp430sbw_reset = versaloon_msp430sbw_reset;
	((programmer_info_t *)p)->msp430sbw_poll = versaloon_msp430sbw_poll;
	
	// C2
	((programmer_info_t *)p)->c2_init = versaloon_c2_init;
	((programmer_info_t *)p)->c2_fini = versaloon_c2_fini;
	((programmer_info_t *)p)->c2_addr_write = versaloon_c2_addr_write;
	((programmer_info_t *)p)->c2_data_write = versaloon_c2_data_write;
	((programmer_info_t *)p)->c2_data_read = versaloon_c2_data_read;
	((programmer_info_t *)p)->c2_addr_poll = versaloon_c2_addr_poll;
	((programmer_info_t *)p)->c2_commit = versaloon_peripheral_commit;
	
	// I2C
	((programmer_info_t *)p)->i2c_init = versaloon_i2c_init;
	((programmer_info_t *)p)->i2c_fini = versaloon_i2c_fini;
	((programmer_info_t *)p)->i2c_set_speed = versaloon_i2c_set_speed;
	((programmer_info_t *)p)->i2c_read = versaloon_i2c_read;
	((programmer_info_t *)p)->i2c_write = versaloon_i2c_write;
	
	// POLL
	((programmer_info_t *)p)->poll_start = versaloon_poll_start;
	((programmer_info_t *)p)->poll_end = versaloon_poll_end;
	((programmer_info_t *)p)->poll_checkbyte = versaloon_poll_checkbyte;
	
	// Mass-product
	((programmer_info_t *)p)->download_mass_product_data = 
										versaloon_download_mass_product_data;
	((programmer_info_t *)p)->query_mass_product_data_size = 
										versaloon_query_mass_product_data_size;
	
	// firmware update
	((programmer_info_t *)p)->enter_firmware_update_mode = 
										versaloon_enter_firmware_update_mode;
	
	return ERROR_OK;
}

uint32_t versaloon_display_programmer(void)
{
	LOG_INFO(_GETTEXT("\nSupported Programmer by Versaloon driver:\n"));
	return print_usb_devices(versaloon_vid, versaloon_pid, 
							 VERSALOON_SERIALSTRING_INDEX, 
							 versaloon_serialstring);
}

