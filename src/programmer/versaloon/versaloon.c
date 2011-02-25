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
#include "usbtoxxx/usbtoxxx_internal.h"

const char *versaloon_hardwares[] = 
{
	"Versaloon_Full",		// 1
	"Versaloon_Mini",		// 2
	"Versaloon_Nano",		// 3
};

VSS_HANDLER(versaloon_support);
VSS_HANDLER(versaloon_help);
const struct vss_cmd_t versaloon_notifier[] = 
{
	VSS_CMD(	"support",
				"print support information, format: support/S",
				versaloon_support),
	VSS_CMD(	"S",
				"print support information, format: support/S",
				versaloon_support),
	VSS_CMD(	"help",
				"print help information, format: help/h",
				versaloon_help),
	VSS_CMD(	"h",
				"print help information, format: help/h",
				versaloon_help),
	VSS_CMD_END
};

uint8_t *versaloon_buf = NULL;
uint8_t *versaloon_cmd_buf = NULL;
uint16_t versaloon_buf_size = 256;

struct versaloon_pending_t versaloon_pending[VERSALOON_MAX_PENDING_NUMBER];
uint16_t versaloon_pending_idx = 0;

static usb_dev_handle *versaloon_device_handle = NULL;
static uint32_t versaloon_to = VERSALOON_TIMEOUT;

VSS_HANDLER(versaloon_help)
{
	VSS_CHECK_ARGC(1);
	printf("\
Usage of %s:\n\
  -U,  --usb <PID_VID_EPIN_EPOUT>           set usb VID, PID, EPIN, EPOUT\n\n", 
		   VERSALOON_STRING);
	return ERROR_OK;
}

VSS_HANDLER(versaloon_support)
{
	VSS_CHECK_ARGC(1);
	printf("\
%s: see http://www.SimonQian.com/en/Versaloon\n", VERSALOON_STRING);
	return ERROR_OK;
}





// programmer_cmd
static uint32_t versaloon_pending_id = 0;
static versaloon_callback_t versaloon_callback = NULL;
static void *versaloon_extra_data = NULL;
static struct versaloon_want_pos_t *versaloon_want_pos = NULL;
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

void versaloon_free_want_pos(void)
{
	uint16_t i;
	struct versaloon_want_pos_t *tmp, *free_tmp;
	
	tmp = versaloon_want_pos;
	while (tmp != NULL)
	{
		free_tmp = tmp;
		tmp = tmp->next;
		free(free_tmp);
	}
	versaloon_want_pos = NULL;
	
	for (i = 0; i < dimof(versaloon_pending); i++)
	{
		tmp = versaloon_pending[i].pos;
		while (tmp != NULL)
		{
			free_tmp = tmp;
			tmp = tmp->next;
			free(free_tmp);
		}
		versaloon_pending[i].pos = NULL;
	}
}

RESULT versaloon_add_want_pos(uint16_t offset, uint16_t size, uint8_t *buff)
{
	struct versaloon_want_pos_t *new_pos = NULL;
	
	new_pos = (struct versaloon_want_pos_t *)malloc(sizeof(*new_pos));
	if (NULL == new_pos)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	new_pos->offset = offset;
	new_pos->size = size;
	new_pos->buff = buff;
	new_pos->next = NULL;
	
	if (NULL == versaloon_want_pos)
	{
		versaloon_want_pos = new_pos;
	}
	else
	{
		struct versaloon_want_pos_t *tmp = versaloon_want_pos;
		
		while (tmp->next != NULL)
		{
			tmp = tmp->next;
		}
		tmp->next = new_pos;
	}
	
	return ERROR_OK;
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
	versaloon_pending[versaloon_pending_idx].pos = versaloon_want_pos;
	versaloon_want_pos = NULL;
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

static RESULT versaloon_peripheral_commit(void)
{
	RESULT ret = usbtoxxx_execute_command();
	versaloon_to = VERSALOON_TIMEOUT;
	return ret;
}

RESULT versaloon_set_target_voltage(uint8_t index, uint16_t voltage)
{
	REFERENCE_PARAMETER(index);
	
	usbtopwr_init(0);
	usbtopwr_config(0);
	usbtopwr_output(0, voltage);
	usbtopwr_fini(0);
	
	return usbtoxxx_execute_command();
}

RESULT versaloon_get_target_voltage(uint8_t index, uint16_t *voltage)
{
	uint16_t inlen;
	
	REFERENCE_PARAMETER(index);
	
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
		*voltage = GET_LE_U16(&versaloon_buf[0]);
		return ERROR_OK;
	}
}

static RESULT versaloon_fini(void)
{
	if (versaloon_device_handle != NULL)
	{
		usbtoxxx_fini();
		versaloon_free_want_pos();
		
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
static RESULT versaloon_init(void *p)
{
	struct programmer_info_t *t = (struct programmer_info_t *)p;
	uint16_t ret = 0;
	uint8_t retry;
	uint32_t timeout_tmp;
	
	memset(versaloon_pending, 0, sizeof(versaloon_pending));
	
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
	sleep_ms(100);
	
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
	versaloon_buf_size = GET_LE_U16(&versaloon_buf[0]);
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
	
	if (ERROR_OK != usbtoxxx_init())
	{
		return ERROR_FAIL;
	}
	// fixes programmer abilities
	if ((t->interfaces.support_mask & POLL) && 
		!usbtoxxx_interface_supported(USB_TO_POLL))
	{
		t->interfaces.support_mask &= ~POLL;
	}
	if ((t->interfaces.support_mask & USART) && 
		!usbtoxxx_interface_supported(USB_TO_USART))
	{
		t->interfaces.support_mask &= ~USART;
	}
	if ((t->interfaces.support_mask & SPI) && 
		!usbtoxxx_interface_supported(USB_TO_SPI))
	{
		t->interfaces.support_mask &= ~SPI;
	}
	if ((t->interfaces.support_mask & I2C) && 
		!usbtoxxx_interface_supported(USB_TO_I2C))
	{
		t->interfaces.support_mask &= ~I2C;
	}
	if ((t->interfaces.support_mask & GPIO) && 
		!usbtoxxx_interface_supported(USB_TO_GPIO))
	{
		t->interfaces.support_mask &= ~GPIO;
	}
	if ((t->interfaces.support_mask & CAN) && 
		!usbtoxxx_interface_supported(USB_TO_CAN))
	{
		t->interfaces.support_mask &= ~CAN;
	}
	if ((t->interfaces.support_mask & ADC) && 
		!usbtoxxx_interface_supported(USB_TO_ADC))
	{
		t->interfaces.support_mask &= ~ADC;
	}
	if ((t->interfaces.support_mask & DAC) && 
		!usbtoxxx_interface_supported(USB_TO_DAC))
	{
		t->interfaces.support_mask &= ~DAC;
	}
	if ((t->interfaces.support_mask & POWER) && 
		!usbtoxxx_interface_supported(USB_TO_POWER))
	{
		t->interfaces.support_mask &= ~POWER;
	}
	if ((t->interfaces.support_mask & ISSP) && 
		!usbtoxxx_interface_supported(USB_TO_ISSP))
	{
		t->interfaces.support_mask &= ~ISSP;
	}
	if ((t->interfaces.support_mask & JTAG_HL) && 
		!usbtoxxx_interface_supported(USB_TO_JTAG_HL))
	{
		t->interfaces.support_mask &= ~JTAG_HL;
	}
	if ((t->interfaces.support_mask & JTAG_LL) && 
		!usbtoxxx_interface_supported(USB_TO_JTAG_LL))
	{
		t->interfaces.support_mask &= ~JTAG_LL;
	}
	if ((t->interfaces.support_mask & MSP430_JTAG) && 
		!usbtoxxx_interface_supported(USB_TO_MSP430_JTAG))
	{
		t->interfaces.support_mask &= ~MSP430_JTAG;
	}
	if ((t->interfaces.support_mask & C2) && 
		!usbtoxxx_interface_supported(USB_TO_C2))
	{
		t->interfaces.support_mask &= ~C2;
	}
	if ((t->interfaces.support_mask & USART) && 
		!usbtoxxx_interface_supported(USB_TO_USART))
	{
		t->interfaces.support_mask &= ~USART;
	}
	if ((t->interfaces.support_mask & LPC_ICP) && 
		!usbtoxxx_interface_supported(USB_TO_LPCICP))
	{
		t->interfaces.support_mask &= ~LPC_ICP;
	}
	if ((t->interfaces.support_mask & SWD) && 
		!usbtoxxx_interface_supported(USB_TO_SWD))
	{
		t->interfaces.support_mask &= ~SWD;
	}
	if ((t->interfaces.support_mask & SWIM) && 
		!usbtoxxx_interface_supported(USB_TO_SWIM))
	{
		t->interfaces.support_mask &= ~SWIM;
	}
	if ((t->interfaces.support_mask & JTAG_RAW) && 
		!usbtoxxx_interface_supported(USB_TO_JTAG_RAW))
	{
		t->interfaces.support_mask &= ~JTAG_RAW;
	}
	if ((t->interfaces.support_mask & BDM) && 
		!usbtoxxx_interface_supported(USB_TO_BDM))
	{
		t->interfaces.support_mask &= ~BDM;
	}
	if ((t->interfaces.support_mask & DUSI) && 
		!usbtoxxx_interface_supported(USB_TO_DUSI))
	{
		t->interfaces.support_mask &= ~DUSI;
	}
	if ((t->interfaces.support_mask & MICROWIRE) && 
		!usbtoxxx_interface_supported(USB_TO_MICROWIRE))
	{
		t->interfaces.support_mask &= ~MICROWIRE;
	}
	
	return ERROR_OK;
}

static RESULT versaloon_enter_firmware_update_mode(void)
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
// Delay
static RESULT versaloon_delay_ms(uint16_t ms)
{
	return usbtodelay_delay(ms | 0x8000);
}
static RESULT versaloon_delay_us(uint16_t us)
{
	return usbtodelay_delay(us & 0x7FFF);
}
// POLL
static RESULT versaloon_poll_start(uint16_t retry_cnt, uint16_t interval_us)
{
	versaloon_to = VERSALOON_TIMEOUT_LONG;
	return usbtopoll_start(retry_cnt, interval_us);
}
static RESULT versaloon_poll_end(void)
{
	return usbtopoll_end();
}
static RESULT versaloon_poll_checkok(enum poll_check_type_t type, uint16_t offset, 
								uint8_t size, uint32_t mask, uint32_t value)
{
	uint8_t equ = 0;
	
	if (POLL_CHECK_EQU == type)
	{
		equ = 1;
	}
	return usbtopoll_checkok(equ, offset, size, mask, value);
}
static RESULT versaloon_poll_checkfail(enum poll_check_type_t type, uint16_t offset, 
								uint8_t size, uint32_t mask, uint32_t value)
{
	uint8_t equ = 0;
	
	if (POLL_CHECK_EQU == type)
	{
		equ = 1;
	}
	return usbtopoll_checkfail(equ, offset, size, mask, value);
}
static RESULT versaloon_poll_verifybuff(uint16_t offset, uint16_t size, uint8_t *buff)
{
	return usbtopoll_verifybuff(offset, size, buff);
}




// MSP430_SBW, not implemented for chips with TEST pin
RESULT versaloon_msp430sbw_config(uint8_t index, uint8_t has_test)
{
	if (has_test)
	{
		return usbtomsp430sbw_config(index);
	}
	else
	{
		// not support yet
		return ERROR_FAIL;
	}
}



RESULT versaloon_init_capability(void *p)
{
	struct programmer_info_t *t = (struct programmer_info_t *)p;
	struct interfaces_info_t *i = &(t->interfaces);
	
	t->init = versaloon_init;
	t->fini = versaloon_fini;
	
	i->support_mask = (USART | SPI | I2C | GPIO | POWER | ISSP | JTAG_LL | POLL 
		| JTAG_HL | SWIM | JTAG_RAW | C2 | MSP430_JTAG | LPC_ICP | SWD | BDM 
		| DUSI | MICROWIRE);
	
	// USART
	i->usart.init = usbtousart_init;
	i->usart.fini = usbtousart_fini;
	i->usart.config = usbtousart_config;
	i->usart.send = usbtousart_send;
	i->usart.receive = usbtousart_receive;
	i->usart.status = usbtousart_status;
	
	// SPI
	i->spi.init = usbtospi_init;
	i->spi.fini = usbtospi_fini;
	i->spi.config = usbtospi_config;
	i->spi.io = usbtospi_io;
	
	// GPIO
	i->gpio.init = usbtogpio_init;
	i->gpio.fini = usbtogpio_fini;
	i->gpio.config = usbtogpio_config;
	i->gpio.in = usbtogpio_in;
	i->gpio.out = usbtogpio_out;
	
	// Delay
	i->delay.delayms = versaloon_delay_ms;
	i->delay.delayus = versaloon_delay_us;
	
	// ISSP
	i->issp.init = usbtoissp_init;
	i->issp.fini = usbtoissp_fini;
	i->issp.enter_program_mode = usbtoissp_enter_program_mode;
	i->issp.leave_program_mode = usbtoissp_leave_program_mode;
	i->issp.wait_and_poll = usbtoissp_wait_and_poll;
	i->issp.vector = usbtoissp_vector;
	
	// LPCICP
	i->lpcicp.init = usbtolpcicp_init;
	i->lpcicp.fini = usbtolpcicp_fini;
	i->lpcicp.enter_program_mode = usbtolpcicp_enter_program_mode;
	i->lpcicp.in = usbtolpcicp_in;
	i->lpcicp.out = usbtolpcicp_out;
	i->lpcicp.poll_ready = usbtolpcicp_poll_ready;
	
	// Target voltage
	i->target_voltage.get = versaloon_get_target_voltage;
	i->target_voltage.set = versaloon_set_target_voltage;
	
	// JTAG_HL & SWD
	i->swd.init = usbtoswd_init;
	i->swd.fini = usbtoswd_fini;
	i->swd.seqout = usbtoswd_seqout;
	i->swd.seqin = usbtoswd_seqin;
	i->swd.transact = usbtoswd_transact;
	i->swd.config = usbtoswd_config;

	i->jtag_hl.init = usbtojtaghl_init;
	i->jtag_hl.fini = usbtojtaghl_fini;
	i->jtag_hl.config= usbtojtaghl_config;
	i->jtag_hl.tms = usbtojtaghl_tms;
	i->jtag_hl.runtest = usbtojtaghl_runtest;
	i->jtag_hl.ir = usbtojtaghl_ir;
	i->jtag_hl.dr = usbtojtaghl_dr;
	i->jtag_hl.register_callback = usbtojtaghl_register_callback;
	
	// JTAG_LL
	i->jtag_ll.init = usbtojtagll_init;
	i->jtag_ll.fini = usbtojtagll_fini;
	i->jtag_ll.config = usbtojtagll_config;
	i->jtag_ll.tms = usbtojtagll_tms;
	i->jtag_ll.tms_clocks = usbtojtagll_tms_clocks;
	i->jtag_ll.scan = usbtojtagll_scan;
	
	// JTAG_RAW
	i->jtag_raw.init = usbtojtagraw_init;
	i->jtag_raw.fini = usbtojtagraw_fini;
	i->jtag_raw.config = usbtojtagraw_config;
	i->jtag_raw.execute = usbtojtagraw_execute;
	
	// MSP430_JTAG
	i->msp430jtag.init = usbtomsp430jtag_init;
	i->msp430jtag.fini = usbtomsp430jtag_fini;
	i->msp430jtag.config = usbtomsp430jtag_config;
	i->msp430jtag.ir = usbtomsp430jtag_ir;
	i->msp430jtag.dr = usbtomsp430jtag_dr;
	i->msp430jtag.tclk = usbtomsp430jtag_tclk;
	i->msp430jtag.tclk_strobe = usbtomsp430jtag_tclk_strobe;
	i->msp430jtag.reset = usbtomsp430jtag_reset;
	i->msp430jtag.poll = usbtomsp430jtag_poll;
	
	// MSP430_SBW
	i->msp430sbw.init = usbtomsp430sbw_init;
	i->msp430sbw.fini = usbtomsp430sbw_fini;
	i->msp430sbw.config = versaloon_msp430sbw_config;
	i->msp430sbw.ir = usbtomsp430sbw_ir;
	i->msp430sbw.dr = usbtomsp430sbw_dr;
	i->msp430sbw.tclk = usbtomsp430sbw_tclk;
	i->msp430sbw.tclk_strobe = usbtomsp430sbw_tclk_strobe;
	i->msp430sbw.reset = usbtomsp430sbw_reset;
	i->msp430sbw.poll = usbtomsp430sbw_poll;
	
	// C2
	i->c2.init = usbtoc2_init;
	i->c2.fini = usbtoc2_fini;
	i->c2.addr_write = usbtoc2_writeaddr;
	i->c2.addr_read = usbtoc2_readaddr;
	i->c2.data_write = usbtoc2_writedata;
	i->c2.data_read = usbtoc2_readdata;
	
	// I2C
	i->i2c.init = usbtoi2c_init;
	i->i2c.fini = usbtoi2c_fini;
	i->i2c.config = usbtoi2c_config;
	i->i2c.read = usbtoi2c_read;
	i->i2c.write = usbtoi2c_write;
	
	// SWIM
	i->swim.init = usbtoswim_init;
	i->swim.fini = usbtoswim_fini;
	i->swim.config = usbtoswim_config;
	i->swim.srst = usbtoswim_srst;
	i->swim.wotf = usbtoswim_wotf;
	i->swim.rotf = usbtoswim_rotf;
	i->swim.sync = usbtoswim_sync;
	i->swim.enable = usbtoswim_enable;
	
	// BDM
	i->bdm.init = usbtobdm_init;
	i->bdm.fini = usbtobdm_fini;
	i->bdm.sync = usbtobdm_sync;
	i->bdm.transact = usbtobdm_transact;
	
	// DUSI
	i->dusi.init = usbtodusi_init;
	i->dusi.fini = usbtodusi_fini;
	i->dusi.config = usbtodusi_config;
	i->dusi.io = usbtodusi_io;
	
	// MICROWIRE
	i->microwire.init = usbtomicrowire_init;
	i->microwire.fini = usbtomicrowire_fini;
	i->microwire.config = usbtomicrowire_config;
	i->microwire.transport = usbtomicrowire_transport;
	i->microwire.poll = usbtomicrowire_poll;
	
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

