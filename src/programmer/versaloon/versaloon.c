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

uint8 *versaloon_buf = NULL;
uint16 versaloon_buf_size = 256;

versaloon_pending_t versaloon_pending[VERSALOON_MAX_PENDING_NUMBER];
uint16 versaloon_pending_idx = 0;

static usb_dev_handle *versaloon_device_handle = NULL;
static uint16 versaloon_vid = VERSALOON_VID;
static uint16 versaloon_pid = VERSALOON_PID;
static uint8 versaloon_epout = VERSALOON_OUTP;
static uint8 versaloon_epin = VERSALOON_INP;
static uint32 versaloon_to = VERSALOON_TIMEOUT;

static uint8 use_usbtojtaghl = 1;


void versaloon_usage(void)
{
	printf("\
Usage of %s:\n\
  -U,  --usb <PID_VID_EPIN_EPOUT>   set usb VID, PID, EPIN, EPOUT\n\n", 
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
	case 'J':
		use_usbtojtaghl = 0;
		break;
	case 'U':
		if (NULL == argu)
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		cur_pointer = (char *)argu;
		
		// Format: VID_PID_EPIN_EPOUT
		versaloon_vid = (uint16)strtoul(cur_pointer, &end_pointer, 0);
		if ((end_pointer == cur_pointer) 
			|| ((*end_pointer != '_') && (*end_pointer != ' ') 
				&& (*end_pointer != '-')) 
			|| ('\0' == *(end_pointer + 1)))
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		cur_pointer = end_pointer + 1;
		versaloon_pid = (uint16)strtoul(cur_pointer, &end_pointer, 0);
		if ((end_pointer == cur_pointer) 
			|| ((*end_pointer != '_') && (*end_pointer != ' ') 
				&& (*end_pointer != '-')) 
			|| ('\0' == *(end_pointer + 1)))
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		cur_pointer = end_pointer + 1;
		versaloon_epin = (uint8)strtoul(cur_pointer, &end_pointer, 0);
		if ((end_pointer == cur_pointer) 
			|| ((*end_pointer != '_') && (*end_pointer != ' ') 
				&& (*end_pointer != '-')) 
			|| ('\0' == *(end_pointer + 1)))
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		cur_pointer = end_pointer + 1;
		versaloon_epout = (uint8)strtoul(cur_pointer, &end_pointer, 0);
		if ((end_pointer == cur_pointer) || (*end_pointer != '\0'))
		{
			LOG_BUG(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		LOG_DEBUG(_GETTEXT("Versaloon is on 0x%04X:0x%04X(0x%02x_0x%02X).\n"), 
				  versaloon_vid, versaloon_pid, 
				  versaloon_epin, versaloon_epout);
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}




RESULT versaloon_add_pending(uint8 type, uint8 cmd, uint16 actual_szie, 
							 uint16 want_pos, uint16 want_size, 
							 uint8 *buffer, uint8 collect)
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
	versaloon_pending_idx++;
	
	return ERROR_OK;
}

RESULT versaloon_send_command(uint16 out_len, uint16 *inlen)
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
			*inlen = (uint16)ret;
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
	uint16 ret = 0;
	uint8 retry;
	int verbosity_tmp;
	uint32 timeout_tmp;
	
	versaloon_device_handle = find_usb_device(versaloon_vid, versaloon_pid);
	if (NULL == versaloon_device_handle)
	{
		LOG_ERROR(_GETTEXT("No usb device with vid=0x%04x, pid = 0x%04x.\n"), 
				  versaloon_vid, versaloon_pid);
		return ERROR_FAIL;
	}
	
	// malloc temporary buffer
	versaloon_buf = (uint8 *)malloc(versaloon_buf_size);
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
	
	versaloon_buf = (uint8 *)malloc(versaloon_buf_size);
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

const char* versaloon_get_hardware_name(uint8 idx)
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

RESULT versaloon_get_hardware(uint8 *hardware)
{
	uint16 inlen;
	
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

RESULT versaloon_get_target_voltage(uint16 *voltage)
{
	uint16 inlen;
	
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

RESULT versaloon_query_mass_product_data_size(uint32 *size)
{
	uint16 inlen;
	
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
	uint16 inlen;
	
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

RESULT versaloon_checksum_mass_product_data(uint16 * checksum, uint16 len)
{
	uint16 inlen;
	
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

RESULT versaloon_download_mass_product_data(const char *name, uint8 *buffer, 
											uint32 len)
{
	uint32 size = 0, i, j;
	uint16 inlen, checksum = 0;
	
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
	if (ERROR_OK != versaloon_checksum_mass_product_data(&inlen, (uint16)len))
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

// capability
static uint16 versaloon_get_pin_remap(uint16 mask)
{
	return mask;
}
static uint16 versaloon_get_pin_map(uint16 mask)
{
	return mask;
}
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
RESULT versaloon_spi_config(uint16 kHz, uint8 cpol, uint8 cpha, 
							uint8 first_bit)
{
	return usbtospi_config(VERSALOON_SPI_PORT, kHz, cpol, cpha, first_bit);
}
RESULT versaloon_spi_io(uint8 *out, uint8 *in, uint16 len, uint16 inpos, 
						uint16 inlen)
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
RESULT versaloon_gpio_config(uint16 mask, uint16 direction)
{
	return usbtogpio_config(VERSALOON_GPIO_PORT, 
							versaloon_get_pin_remap(mask), direction > 0);
}
RESULT versaloon_gpio_config_8bit(uint8 mask, uint8 direction)
{
	return usbtogpio_config(VERSALOON_GPIO_PORT, 
							versaloon_get_pin_remap(mask), direction > 0);
}
RESULT versaloon_gpio_in(uint16 mask, uint16 *value)
{
	uint16 value_tmp;
	RESULT ret;
	
	ret = usbtogpio_in(VERSALOON_GPIO_PORT, versaloon_get_pin_remap(mask), 
					   &value_tmp);
	*value = versaloon_get_pin_map(value_tmp);
	
	return ret;
}
RESULT versaloon_gpio_in_8bit(uint8 mask, uint8 *value)
{
	uint16 value_tmp;
	RESULT ret;
	
	ret = usbtogpio_in(VERSALOON_GPIO_PORT, versaloon_get_pin_remap(mask), 
					   &value_tmp);
	*value = (uint8)versaloon_get_pin_map(value_tmp);
	
	return ret;
}
RESULT versaloon_gpio_out(uint16 mask, uint16 value)
{
	return usbtogpio_out(VERSALOON_GPIO_PORT, versaloon_get_pin_remap(mask), 
						 versaloon_get_pin_remap(value));
}
RESULT versaloon_gpio_out_8bit(uint8 mask, uint8 value)
{
	return usbtogpio_out(VERSALOON_GPIO_PORT, versaloon_get_pin_remap(mask), 
						 versaloon_get_pin_remap(value));
}
// Delay
RESULT versaloon_delay_ms(uint16 ms)
{
	return usbtodelay_delay(ms | 0x8000);
}
RESULT versaloon_delay_us(uint16 us)
{
	return usbtodelay_delay(us & 0x7FFF);
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
RESULT versaloon_issp_enter_program_mode(uint8 mode)
{
	return usbtoissp_enter_program_mode(VERSALOON_ISSP_PORT, mode);
}
RESULT versaloon_issp_leave_program_mode(uint8 mode)
{
	return usbtoissp_leave_program_mode(VERSALOON_ISSP_PORT, mode);
}
RESULT versaloon_issp_wait_and_poll(void)
{
	return usbtoissp_wait_and_poll(VERSALOON_ISSP_PORT);
}
RESULT versaloon_issp_vector(uint8 operate, uint8 addr, uint8 data, uint8 *buf)
{
	return usbtoissp_vector(VERSALOON_ISSP_PORT, operate, addr, data, buf);
}
// JTAG
RESULT versaloon_jtaghl_init(void)
{
	return usbtojtaghl_init();
}
RESULT versaloon_jtaghl_fini(void)
{
	return usbtojtaghl_fini();
}
RESULT versaloon_jtaghl_config(uint16 kHz, uint8 ub, uint8 ua, uint16 bb, 
							   uint16 ba)
{
	return usbtojtaghl_config(VERSALOON_JTAGHL_PORT, kHz, ub, ua, bb, ba);
}
RESULT versaloon_jtaghl_tms(uint8 *tms, uint8 len)
{
	return usbtojtaghl_tmsbyte(VERSALOON_JTAGHL_PORT, tms, len);
}
RESULT versaloon_jtaghl_ir(uint8 *ir, uint8 len, uint8 idle, uint8 want_ret)
{
	return usbtojtaghl_ir(VERSALOON_JTAGHL_PORT, ir, len, idle, want_ret);
}
RESULT versaloon_jtaghl_dr(uint8 *dr, uint16 len, uint8 idle, uint8 want_ret)
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
RESULT versaloon_msp430jtag_config(uint8 has_test)
{
	return usbtomsp430jtag_config(VERSALOON_MSP430_JTAG_PORT, has_test);
}
RESULT versaloon_msp430jtag_ir(uint8 *ir, uint8 want_ret)
{
	return usbtomsp430jtag_ir(VERSALOON_MSP430_JTAG_PORT, ir, want_ret);
}
RESULT versaloon_msp430jtag_dr(uint32 *dr, uint8 len, uint8 want_ret)
{
	return usbtomsp430jtag_dr(VERSALOON_MSP430_JTAG_PORT, dr, len, want_ret);
}
RESULT versaloon_msp430jtag_tclk(uint8 value)
{
	return usbtomsp430jtag_tclk(VERSALOON_MSP430_JTAG_PORT, value);
}
RESULT versaloon_msp430jtag_reset(void)
{
	return usbtomsp430jtag_reset(VERSALOON_MSP430_JTAG_PORT);
}
RESULT versaloon_msp430jtag_poll(uint32 dr, uint32 mask, uint32 value, 
								 uint8 len, uint16 poll_cnt, uint8 toggle_tclk)
{
	return usbtomsp430jtag_poll(VERSALOON_MSP430_JTAG_PORT, dr, mask, value, 
								len, poll_cnt, toggle_tclk);
}
RESULT versaloon_msp430jtag_tclk_strobe(uint16 cnt)
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
RESULT versaloon_msp430sbw_config(uint8 has_test)
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
RESULT versaloon_msp430sbw_ir(uint8 *ir, uint8 want_ret)
{
	return usbtomsp430sbw_ir(VERSALOON_MSP430_JTAG_PORT, ir, want_ret);
}
RESULT versaloon_msp430sbw_dr(uint32 *dr, uint8 len, uint8 want_ret)
{
	return usbtomsp430sbw_dr(VERSALOON_MSP430_JTAG_PORT, dr, len, want_ret);
}
RESULT versaloon_msp430sbw_tclk(uint8 value)
{
	return usbtomsp430sbw_tclk(VERSALOON_MSP430_JTAG_PORT, value);
}
RESULT versaloon_msp430sbw_reset(void)
{
	return usbtomsp430sbw_reset(VERSALOON_MSP430_JTAG_PORT);
}
RESULT versaloon_msp430sbw_poll(uint32 dr, uint32 mask, uint32 value, 
								uint8 len, uint16 poll_cnt, uint8 toggle_tclk)
{
	return usbtomsp430sbw_poll(VERSALOON_MSP430_JTAG_PORT, dr, mask, value, 
							   len, poll_cnt, toggle_tclk);
}
RESULT versaloon_msp430sbw_tclk_strobe(uint16 cnt)
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
RESULT versaloon_c2_addr_write(uint8 addr)
{
	return usbtoc2_addr(VERSALOON_C2_PORT, addr);
}
RESULT versaloon_c2_data_write(uint8 *data, uint8 len)
{
	return usbtoc2_data(VERSALOON_C2_PORT, 0, len, data);
}
RESULT versaloon_c2_data_read(uint8 *data, uint8 len)
{
	return usbtoc2_data(VERSALOON_C2_PORT, 1, len, data);
}
RESULT versaloon_c2_addr_poll(uint8 mask, uint8 value, uint16 poll_cnt)
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
RESULT versaloon_i2c_set_speed(uint16 kHz)
{
	return usbtoi2c_set_speed(VERSALOON_I2C_PORT, kHz);
}
RESULT versaloon_i2c_read(uint16 chip_addr, uint8 chip_addr_len, uint8 *data, 
						  uint16 data_len, uint8 stop)
{
	return usbtoi2c_read(VERSALOON_I2C_PORT, chip_addr, chip_addr_len, data, 
						 data_len, stop);
}
RESULT versaloon_i2c_write(uint16 chip_addr, uint8 chip_addr_len, uint8 *data, 
						   uint16 data_len, uint8 stop)
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
											 | MSP430_SBW);
	
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
	
	// Target voltage
	((programmer_info_t *)p)->get_target_voltage = 
											versaloon_get_target_voltage;
	
	// JTAG_HL
	if (use_usbtojtaghl)
	{
		((programmer_info_t *)p)->jtag_hl_init = versaloon_jtaghl_init;
		((programmer_info_t *)p)->jtag_hl_fini = versaloon_jtaghl_fini;
		((programmer_info_t *)p)->jtag_hl_config= versaloon_jtaghl_config;
		((programmer_info_t *)p)->jtag_hl_tms = versaloon_jtaghl_tms;
		((programmer_info_t *)p)->jtag_hl_ir = versaloon_jtaghl_ir;
		((programmer_info_t *)p)->jtag_hl_dr = versaloon_jtaghl_dr;
		((programmer_info_t *)p)->jtag_hl_aux_io_init = versaloon_gpio_init;
		((programmer_info_t *)p)->jtag_hl_aux_io_fini = versaloon_gpio_fini;
		((programmer_info_t *)p)->jtag_hl_aux_io_config = 
											versaloon_gpio_config_8bit;
		((programmer_info_t *)p)->jtag_hl_aux_io_out = versaloon_gpio_out_8bit;
		((programmer_info_t *)p)->jtag_hl_aux_io_in = versaloon_gpio_in_8bit;
		((programmer_info_t *)p)->jtag_hl_delay_us = versaloon_delay_us;
		((programmer_info_t *)p)->jtag_hl_delay_ms = versaloon_delay_ms;
		((programmer_info_t *)p)->jtag_hl_commit = versaloon_peripheral_commit;
	}
	else
	{
		((programmer_info_t *)p)->jtag_hl_init = vsllink_jtag_connect;
		((programmer_info_t *)p)->jtag_hl_fini = vsllink_jtaghl_disconnect;
		((programmer_info_t *)p)->jtag_hl_config= vsllink_jtaghl_config;
		((programmer_info_t *)p)->jtag_hl_tms = vsllink_jtaghl_tms;
		((programmer_info_t *)p)->jtag_hl_ir = vsllink_jtaghl_ir;
		((programmer_info_t *)p)->jtag_hl_dr = vsllink_jtaghl_dr;
		((programmer_info_t *)p)->jtag_hl_aux_io_init = versaloon_null;
		((programmer_info_t *)p)->jtag_hl_aux_io_fini = versaloon_null;
		((programmer_info_t *)p)->jtag_hl_aux_io_config = 
											vsllink_jtag_auxio_config;
		((programmer_info_t *)p)->jtag_hl_aux_io_out = vsllink_jtag_auxio_out;
		((programmer_info_t *)p)->jtag_hl_aux_io_in = vsllink_jtag_auxio_in;
		((programmer_info_t *)p)->jtag_hl_delay_us = vsllink_jtaghl_delay_us;
		((programmer_info_t *)p)->jtag_hl_delay_ms = vsllink_jtaghl_delay_ms;
		((programmer_info_t *)p)->jtag_hl_commit = vsllink_jtaghl_commit;
	}
	
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

