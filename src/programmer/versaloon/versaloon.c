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

#include <stdio.h>
#include <string.h>

#include "versaloon_include.h"
#include "versaloon.h"
#include "versaloon_internal.h"
#include "usbtoxxx/usbtoxxx.h"

const char *versaloon_hardwares[] = 
{
	"Versaloon_Full",		// 1
	"Versaloon_Mini",		// 2
	"Versaloon_Nano",		// 3
};

MISC_HANDLER(versaloon_support);
MISC_HANDLER(versaloon_help);
const struct misc_cmd_t versaloon_notifier[] = 
{
	MISC_CMD(	"support",
				"print support information, format: support/S",
				versaloon_support),
	MISC_CMD(	"S",
				"print support information, format: support/S",
				versaloon_support),
	MISC_CMD(	"help",
				"print help information, format: help/h",
				versaloon_help),
	MISC_CMD(	"h",
				"print help information, format: help/h",
				versaloon_help),
	MISC_CMD_END
};

uint8_t *versaloon_buf = NULL;
uint8_t *versaloon_cmd_buf = NULL;
uint16_t versaloon_buf_size = 256;

struct versaloon_pending_t versaloon_pending[VERSALOON_MAX_PENDING_NUMBER];
uint16_t versaloon_pending_idx = 0;

static usb_dev_handle *versaloon_device_handle = NULL;
static uint32_t versaloon_to = VERSALOON_TIMEOUT;

MISC_HANDLER(versaloon_help)
{
	MISC_CHECK_ARGC(1);
	printf("\
Usage of %s:\n\
  -U,  --usb <PID_VID_EPIN_EPOUT>           set usb VID, PID, EPIN, EPOUT\n\n", 
		   VERSALOON_STRING);
	return ERROR_OK;
}

MISC_HANDLER(versaloon_support)
{
	MISC_CHECK_ARGC(1);
	printf("\
%s: see http://www.SimonQian.com/en/Versaloon\n", VERSALOON_STRING);
	return ERROR_OK;
}





// programmer_cmd
static uint32_t versaloon_pending_id = 0;
static versaloon_callback_t versaloon_callback = NULL;
static void *versaloon_extra_data = NULL;
void versaloon_set_pending_id(uint32_t id)
{
	versaloon_pending_id = id;
}
void versaloon_set_callback(versaloon_callback_t callback)
{
	versaloon_callback = callback;
}
void versaloon_set_extra_data(void * p)
{
	versaloon_extra_data = p;
}

RESULT versaloon_add_pending(uint8_t type, uint8_t cmd, uint16_t actual_szie, 
	uint16_t want_pos, uint16_t want_size, uint8_t *buffer, uint8_t collect)
{
#if PARAM_CHECK
	if (versaloon_pending_idx >= VERSALOON_MAX_PENDING_NUMBER)
	{
		LOG_BUG(ERRMSG_INVALID_INDEX, versaloon_pending_idx, 
					"versaloon pending data");
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
	versaloon_pending[versaloon_pending_idx].extra_data = versaloon_extra_data;
	versaloon_extra_data = NULL;
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
		LOG_BUG(ERRMSG_INVALID_BUFFER, TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
	if ((0 == out_len) || (out_len > versaloon_buf_size))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	ret = usb_bulk_write(versaloon_device_handle, usb_param_epout(), 
						 (char *)versaloon_buf, out_len, versaloon_to);
	if (ret != out_len)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRSTRING, "send usb data", 
					usb_strerror());
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (inlen != NULL)
	{
		ret = usb_bulk_read(versaloon_device_handle, usb_param_epin(), 
					(char *)versaloon_buf, versaloon_buf_size, versaloon_to);
		if (ret > 0)
		{
			*inlen = (uint16_t)ret;
			return ERROR_OK;
		}
		else
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRSTRING, "receive usb data", 
						usb_strerror());
			return ERROR_FAIL;
		}
	}
	else
	{
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
		LOG_BUG(ERRMSG_INVALID_BUFFER, TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
	if (NULL == voltage)
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	versaloon_buf[0] = VERSALOON_GET_TVCC;
	
	if ((ERROR_OK != versaloon_send_command(1, &inlen)) || (inlen != 2))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	else
	{
		*voltage = versaloon_buf[0] + (versaloon_buf[1] << 8);
		return ERROR_OK;
	}
}

RESULT versaloon_fini(void)
{
	if (versaloon_device_handle != NULL)
	{
		usb_release_interface(versaloon_device_handle, usb_param_interface());
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

#define VERSALOON_RETRY_CNT				10
RESULT versaloon_init(void)
{
	uint16_t ret = 0;
	uint8_t retry;
	uint32_t timeout_tmp;
	
	versaloon_device_handle = find_usb_device(usb_param_vid(), 
		usb_param_pid(), usb_param_interface(), VERSALOON_SERIALSTRING_INDEX, 
		usb_param_serial(), VERSALOON_PRODUCTSTRING_INDEX, 
						VERSALOON_PRODUCTSTRING);
	if (NULL == versaloon_device_handle)
	{
		if (usb_param_serial() != NULL)
		{
			LOG_ERROR("Not found vid=0x%04x,pid = 0x%04x,serial = %s.", 
						usb_param_vid(), usb_param_pid(), usb_param_serial());
		}
		else
		{
			LOG_ERROR("Not found vid=0x%04x,pid = 0x%04x.", usb_param_vid(), 
						usb_param_pid());
		}
		return ERROR_FAIL;
	}
	
	// malloc temporary buffer
	versaloon_buf = (uint8_t *)malloc(versaloon_buf_size);
	if (NULL == versaloon_buf)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	
	// disable cdc device first
	if (0 > usb_control_msg(versaloon_device_handle, 
			USB_TYPE_VENDOR | USB_RECIP_INTERFACE, 0, 0, 0, NULL, 0, 
			versaloon_to))
	{
		versaloon_fini();
		LOG_ERROR(ERRMSG_FAILURE_OPERATION_MESSAGE, 
					"disable cdc device in versaloon", usb_strerror());
		return ERRCODE_FAILURE_OPERATION;
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
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	versaloon_buf[ret] = 0;
	versaloon_buf_size = versaloon_buf[0] + (versaloon_buf[1] << 8);
	LOG_INFO("%s", versaloon_buf + 2);
	
	// free temporary buffer
	free(versaloon_buf);
	versaloon_buf = NULL;
	
	versaloon_buf = (uint8_t *)malloc(versaloon_buf_size);
	if (NULL == versaloon_buf)
	{
		versaloon_fini();
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	versaloon_cmd_buf = (uint8_t *)malloc(versaloon_buf_size - 3);
	if (NULL == versaloon_cmd_buf)
	{
		versaloon_fini();
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	return versaloon_get_target_voltage(&ret);
}

RESULT versaloon_enter_firmware_update_mode(void)
{
#if PARAM_CHECK
	if (NULL == versaloon_buf)
	{
		LOG_BUG(ERRMSG_INVALID_BUFFER, TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	versaloon_buf[0] = VERSALOON_FW_UPDATE;
	versaloon_buf[1] = (uint8_t)(VERSALOON_FW_UPDATE_KEY);
	versaloon_buf[2] = (uint8_t)(~VERSALOON_FW_UPDATE_KEY);
	
	if (ERROR_OK != versaloon_send_command(3, NULL))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "communicate with versaloon");
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
		LOG_BUG(ERRMSG_INVALID_BUFFER, TO_STR(versaloon_buf));
		return ERRCODE_INVALID_BUFFER;
	}
	if (NULL == hardware)
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	versaloon_buf[0] = VERSALOON_GET_HARDWARE;
	
	if ((ERROR_OK != versaloon_send_command(1, &inlen)) || (inlen != 1))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "communicate with versaloon");
		return ERRCODE_FAILURE_OPERATION;
	}
	else
	{
		*hardware = versaloon_buf[0];
		LOG_DEBUG("versaloon hardware is %s", 
					versaloon_get_hardware_name(*hardware));
		return ERROR_OK;
	}
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
RESULT versaloon_poll_checkok(enum poll_check_type_t type, uint16_t offset, 
								uint8_t size, uint32_t mask, uint32_t value)
{
	uint8_t equ = 0;
	
	if (POLL_CHECK_EQU == type)
	{
		equ = 1;
	}
	return usbtopoll_checkok(equ, offset, size, mask, value);
}
RESULT versaloon_poll_checkfail(enum poll_check_type_t type, uint16_t offset, 
								uint8_t size, uint32_t mask, uint32_t value)
{
	uint8_t equ = 0;
	
	if (POLL_CHECK_EQU == type)
	{
		equ = 1;
	}
	return usbtopoll_checkfail(equ, offset, size, mask, value);
}
RESULT versaloon_poll_verifybuff(uint16_t offset, uint16_t size, uint8_t *buff)
{
	return usbtopoll_verifybuff(offset, size, buff);
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
RESULT versaloon_swd_init(void)
{
	return usbtoswd_init();
}
RESULT versaloon_swd_fini(void)
{
	return usbtoswd_fini();
}
RESULT versaloon_swd_config(uint8_t trn, uint16_t retry, uint16_t dly)
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
RESULT versaloon_swd_transact(uint8_t request, uint32_t *data, uint8_t *ack)
{
	return usbtoswd_transact(VERSALOON_SWD_PORT, request, data, ack);
}
// JTAG
RESULT versaloon_jtagraw_init(void)
{
	return usbtojtagraw_init();
}
RESULT versaloon_jtagraw_fini(void)
{
	return usbtojtagraw_fini();
}
RESULT versaloon_jtagraw_config(uint16_t kHz)
{
	return usbtojtagraw_config(VERSALOON_JTAGRAW_PORT, kHz);
}
RESULT versaloon_jtagraw_execute(uint8_t *tdi, uint8_t *tms, uint8_t *tdo, 
									uint16_t bytelen)
{
	return usbtojtagraw_execute(VERSALOON_JTAGRAW_PORT, tdi, tms, tdo, bytelen);
}
RESULT versaloon_jtagll_init(void)
{
	return usbtojtagll_init();
}
RESULT versaloon_jtagll_fini(void)
{
	return usbtojtagll_fini();
}
RESULT versaloon_jtagll_config(uint16_t kHz)
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
RESULT versaloon_i2c_config(uint16_t kHz, uint16_t byte_interval, 
							uint16_t max_dly)
{
	return usbtoi2c_config(VERSALOON_I2C_PORT, kHz, byte_interval, max_dly);
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
RESULT versaloon_swim_config(uint8_t mHz, uint8_t cnt0, uint8_t cnt1)
{
	return usbtoswim_config(VERSALOON_SWIM_PORT, mHz, cnt0, cnt1);
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
	
	t->interfaces_mask = (USART | SPI | I2C | GPIO | POWER | ISSP | JTAG_LL 
		| JTAG_HL | SWIM | JTAG_RAW | C2 | MSP430_JTAG | LPC_ICP | SWD);
	
	// USART
	i->usart.init = versaloon_usart_init;
	i->usart.fini = versaloon_usart_fini;
	i->usart.config = versaloon_usart_config;
	i->usart.send = versaloon_usart_send;
	i->usart.receive = versaloon_usart_receive;
	i->usart.status = versaloon_usart_status;
	
	// SPI
	i->spi.init = versaloon_spi_init;
	i->spi.fini = versaloon_spi_fini;
	i->spi.config = versaloon_spi_config;
	i->spi.io = versaloon_spi_io;
	
	// GPIO
	i->gpio.init = versaloon_gpio_init;
	i->gpio.fini = versaloon_gpio_fini;
	i->gpio.config = versaloon_gpio_config;
	i->gpio.in = versaloon_gpio_in;
	i->gpio.out = versaloon_gpio_out;
	
	// Delay
	i->delay.delayms = versaloon_delay_ms;
	i->delay.delayus = versaloon_delay_us;
	
	// ISSP
	i->issp.init = versaloon_issp_init;
	i->issp.fini = versaloon_issp_fini;
	i->issp.enter_program_mode = versaloon_issp_enter_program_mode;
	i->issp.leave_program_mode = versaloon_issp_leave_program_mode;
	i->issp.wait_and_poll = versaloon_issp_wait_and_poll;
	i->issp.vector = versaloon_issp_vector;
	
	// LPCICP
	i->lpcicp.init = versaloon_lpcicp_init;
	i->lpcicp.fini = versaloon_lpcicp_fini;
	i->lpcicp.enter_program_mode = versaloon_lpcicp_enter_program_mode;
	i->lpcicp.in = versaloon_lpcicp_in;
	i->lpcicp.out = versaloon_lpcicp_out;
	i->lpcicp.poll_ready = versaloon_lpcicp_poll_ready;
	
	// Target voltage
	i->target_voltage.get = versaloon_get_target_voltage;
	i->target_voltage.set = versaloon_set_target_voltage;
	
	// JTAG_HL & SWD
	i->swd.init = versaloon_swd_init;
	i->swd.fini = versaloon_swd_fini;
	i->swd.seqout = versaloon_swd_seqout;
	i->swd.seqin = versaloon_swd_seqin;
	i->swd.transact = versaloon_swd_transact;
	i->swd.config = versaloon_swd_config;

	i->jtag_hl.init = versaloon_jtaghl_init;
	i->jtag_hl.fini = versaloon_jtaghl_fini;
	i->jtag_hl.config= versaloon_jtaghl_config;
	i->jtag_hl.tms = versaloon_jtaghl_tms;
	i->jtag_hl.runtest = versaloon_jtaghl_runtest;
	i->jtag_hl.ir = versaloon_jtaghl_ir;
	i->jtag_hl.dr = versaloon_jtaghl_dr;
	i->jtag_hl.register_callback = versaloon_jtaghl_register_callback;
	
	// JTAG_LL
	i->jtag_ll.init = versaloon_jtagll_init;
	i->jtag_ll.fini = versaloon_jtagll_fini;
	i->jtag_ll.config = versaloon_jtagll_config;
	i->jtag_ll.tms = versaloon_jtagll_tms;
	i->jtag_ll.tms_clocks = versaloon_jtagll_tms_clocks;
	i->jtag_ll.scan = versaloon_jtagll_scan;
	
	// JTAG_RAW
	i->jtag_raw.init = versaloon_jtagraw_init;
	i->jtag_raw.fini = versaloon_jtagraw_fini;
	i->jtag_raw.config = versaloon_jtagraw_config;
	i->jtag_raw.execute = versaloon_jtagraw_execute;
	
	// MSP430_JTAG
	i->msp430jtag.init = versaloon_msp430jtag_init;
	i->msp430jtag.fini = versaloon_msp430jtag_fini;
	i->msp430jtag.config = versaloon_msp430jtag_config;
	i->msp430jtag.ir = versaloon_msp430jtag_ir;
	i->msp430jtag.dr = versaloon_msp430jtag_dr;
	i->msp430jtag.tclk = versaloon_msp430jtag_tclk;
	i->msp430jtag.tclk_strobe = versaloon_msp430jtag_tclk_strobe;
	i->msp430jtag.reset = versaloon_msp430jtag_reset;
	i->msp430jtag.poll = versaloon_msp430jtag_poll;
	
	// MSP430_SBW
	i->msp430sbw.init = versaloon_msp430sbw_init;
	i->msp430sbw.fini = versaloon_msp430sbw_fini;
	i->msp430sbw.config = versaloon_msp430sbw_config;
	i->msp430sbw.ir = versaloon_msp430sbw_ir;
	i->msp430sbw.dr = versaloon_msp430sbw_dr;
	i->msp430sbw.tclk = versaloon_msp430sbw_tclk;
	i->msp430sbw.tclk_strobe = versaloon_msp430sbw_tclk_strobe;
	i->msp430sbw.reset = versaloon_msp430sbw_reset;
	i->msp430sbw.poll = versaloon_msp430sbw_poll;
	
	// C2
	i->c2.init = versaloon_c2_init;
	i->c2.fini = versaloon_c2_fini;
	i->c2.addr_write = versaloon_c2_addr_write;
	i->c2.addr_read = versaloon_c2_addr_read;
	i->c2.data_write = versaloon_c2_data_write;
	i->c2.data_read = versaloon_c2_data_read;
	
	// I2C
	i->i2c.init = versaloon_i2c_init;
	i->i2c.fini = versaloon_i2c_fini;
	i->i2c.config = versaloon_i2c_config;
	i->i2c.read = versaloon_i2c_read;
	i->i2c.write = versaloon_i2c_write;
	
	// SWIM
	i->swim.init = versaloon_swim_init;
	i->swim.fini = versaloon_swim_fini;
	i->swim.config = versaloon_swim_config;
	i->swim.srst = versaloon_swim_srst;
	i->swim.wotf = versaloon_swim_wotf;
	i->swim.rotf = versaloon_swim_rotf;
	i->swim.sync = versaloon_swim_sync;
	i->swim.enable = versaloon_swim_enable;
	
	// POLL
	i->poll.start = versaloon_poll_start;
	i->poll.end = versaloon_poll_end;
	i->poll.checkok = versaloon_poll_checkok;
	i->poll.checkfail = versaloon_poll_checkfail;
	i->poll.verifybuff = versaloon_poll_verifybuff;
	
	i->peripheral_commit = versaloon_peripheral_commit;
	
	// firmware update
	t->enter_firmware_update_mode = versaloon_enter_firmware_update_mode;
	
	// usb parameter
	if (!usb_param_valid())
	{
		usb_set_param(VERSALOON_VID, VERSALOON_PID, VERSALOON_INP, 
						VERSALOON_OUTP, 1);
	}
	
	return ERROR_OK;
}

uint32_t versaloon_display_programmer(void)
{
	// usb parameter
	if (!usb_param_valid())
	{
		usb_set_param(VERSALOON_VID, VERSALOON_PID, VERSALOON_INP, 
						VERSALOON_OUTP, 1);
	}
	
	printf(_GETTEXT("Supported Programmer by Versaloon driver:\n"));
	return print_usb_devices(usb_param_vid(), usb_param_pid(), 
					VERSALOON_SERIALSTRING_INDEX, usb_param_serial(), 
					VERSALOON_PRODUCTSTRING_INDEX, VERSALOON_PRODUCTSTRING);
}

