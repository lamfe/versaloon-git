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

#include "compiler.h"

#include "versaloon_include.h"
#include "versaloon.h"
#include "versaloon_internal.h"
#include "usbtoxxx/usbtoxxx.h"
#include "usbtoxxx/usbtoxxx_internal.h"

#define VERSALOON_STRING					"VSFCore_USBTOXXX"

uint8_t *versaloon_buf = NULL;
uint8_t *versaloon_cmd_buf = NULL;
uint16_t versaloon_buf_size = 256;

struct versaloon_pending_t versaloon_pending[VERSALOON_MAX_PENDING_NUMBER];
uint16_t versaloon_pending_idx = 0;

static usb_dev_handle *versaloon_device_handle = NULL;
static uint32_t versaloon_to = VERSALOON_TIMEOUT;





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

vsf_err_t versaloon_add_want_pos(uint16_t offset, uint16_t size, uint8_t *buff)
{
	struct versaloon_want_pos_t *new_pos = NULL;
	
	new_pos = (struct versaloon_want_pos_t *)malloc(sizeof(*new_pos));
	if (NULL == new_pos)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
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
	
	return VSFERR_NONE;
}

vsf_err_t versaloon_send_command(uint16_t out_len, uint16_t *inlen)
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
		return VSFERR_INVALID_PARAMETER;
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
			return VSFERR_NONE;
		}
		else
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRSTRING, "receive usb data",
						usb_strerror());
			return VSFERR_FAIL;
		}
	}
	else
	{
		return VSFERR_NONE;
	}
}

static vsf_err_t versaloon_peripheral_commit(void)
{
	vsf_err_t err = usbtoxxx_execute_command();
	versaloon_to = VERSALOON_TIMEOUT;
	return err;
}

vsf_err_t versaloon_add_pending(uint8_t type, uint8_t cmd, uint16_t actual_szie,
	uint16_t want_pos, uint16_t want_size, uint8_t *buffer, uint8_t collect)
{
#if PARAM_CHECK
	if (versaloon_pending_idx >= VERSALOON_MAX_PENDING_NUMBER)
	{
		LOG_BUG(ERRMSG_INVALID_INDEX, versaloon_pending_idx,
					"versaloon pending data");
		return VSFERR_FAIL;
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
	
#ifdef IFS_CFG_NO_BUFFER
	return versaloon_peripheral_commit();
#else
	return VSFERR_NONE;
#endif
}

static vsf_err_t versaloon_reset(void)
{
	return VSFERR_NONE;
}

static vsf_err_t versaloon_fini(void)
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
	
	return VSFERR_NONE;
}

#define VERSALOON_RETRY_CNT				10
static vsf_err_t versaloon_init(void *p)
{
	uint16_t ret = 0;
	uint8_t retry;
	uint32_t timeout_tmp;
	
	REFERENCE_PARAMETER(p);
	
	memset(versaloon_pending, 0, sizeof(versaloon_pending));
	
	if (!usb_param_valid())
	{
		usb_set_param(VERSALOON_VID, VERSALOON_PID, VERSALOON_INP,
						VERSALOON_OUTP, VERSALOON_IFACE);
	}
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
		return VSFERR_FAIL;
	}
	
	// malloc temporary buffer
	versaloon_buf = (uint8_t *)malloc(versaloon_buf_size);
	if (NULL == versaloon_buf)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
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
		if (!versaloon_send_command(1, &ret) && (ret >= 3))
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
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	versaloon_cmd_buf = (uint8_t *)malloc(versaloon_buf_size - 3);
	if (NULL == versaloon_cmd_buf)
	{
		versaloon_fini();
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_NOT_ENOUGH_RESOURCES;
	}
	
	return usbtoxxx_init();
}


struct interfaces_info_t versaloon_interfaces =
{
	{	// core
		versaloon_init,
		versaloon_fini,
		versaloon_reset
	},
#if IFS_FLASH_EN
	{
	},
#endif
#if IFS_GPIO_EN
	{	// gpio
		usbtogpio_init,
		usbtogpio_fini,
		usbtogpio_config_pin,
		usbtogpio_config,
		usbtogpio_set,
		usbtogpio_clear,
		usbtogpio_out,
		usbtogpio_in
	},
#endif
#if IFS_TIMER_EN
	{	// timer
	},
#endif
#if IFS_EINT_EN
	{	// eint
	},
#endif
#if IFS_USART_EN
	{	// usart
		usbtousart_init,
		usbtousart_fini,
		usbtousart_config,
		usbtousart_send,
		usbtousart_receive,
		usbtousart_status
	},
#endif
#if IFS_SPI_EN
	{	// spi
		usbtospi_init,
		usbtospi_fini,
		usbtospi_get_ability,
		usbtospi_enable,
		usbtospi_disable,
		usbtospi_config,
		usbtospi_select,
		usbtospi_deselect,
		NULL, NULL, NULL, NULL,
		usbtospi_io,
		NULL, NULL, NULL
	},
#endif
#if IFS_ADC_EN
	{
		// adc
		usbtoadc_init,
		usbtoadc_fini,
		usbtoadc_config,
		usbtoadc_config_channel,
		usbtoadc_calibrate,
		NULL, NULL, NULL,
		usbtoadc_sample
	},
#endif
#if IFS_IIC_EN
	{	// i2c
		usbtoi2c_init,
		usbtoi2c_fini,
		usbtoi2c_config,
		usbtoi2c_read,
		usbtoi2c_write
	},
#endif
#if IFS_USBD_EN
	{	// usbd
	},
#endif
#if IFS_PWM_EN
	{	// pwm
		usbtopwm_init,
		usbtopwm_fini,
		usbtopwm_config_mode,
		usbtopwm_config_freq,
		usbtopwm_out,
		usbtopwm_in
	},
#endif
#if IFS_MICROWIRE_EN
	{	// microwire
		usbtomicrowire_init,
		usbtomicrowire_fini,
		usbtomicrowire_config,
		usbtomicrowire_transport,
		usbtomicrowire_poll
	},
#endif
#if IFS_EBI_EN
	{
		// ebi
		usbtoebi_init,
		usbtoebi_fini,
		usbtoebi_config,
		NULL, NULL, NULL, NULL, NULL, NULL, NULL,
		NULL,
		usbtoebi_isready,
		usbtoebi_read,
		usbtoebi_write,
		NULL, NULL, NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	},
#endif
#if IFS_SDIO_EN
	{	// sdio
	},
#endif
	{	// tickclk
		NULL, NULL, NULL, NULL, NULL
	},
	{	// delay
		usbtodelay_init,
		usbtodelay_delayms,
		usbtodelay_delayus
	},
	versaloon_peripheral_commit
};

