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

#include <time.h>
#include "compiler.h"

#include "versaloon_include.h"
#include "versaloon.h"
#include "versaloon_internal.h"
#include "usbtoxxx/usbtoxxx.h"
#include "usbtoxxx/usbtoxxx_internal.h"

static usb_dev_handle *versaloon_device_handle = NULL;
static uint32_t versaloon_to = VERSALOON_TIMEOUT;

// usbtoxxx transact structure
static vsf_err_t versaloon_transact(uint16_t out_len, uint16_t *inlen);
static struct usbtoxxx_info_t versaloon_usbtoxxx_info =
{
	NULL, NULL, 0,
	versaloon_transact
};

static vsf_err_t versaloon_transact(uint16_t out_len, uint16_t *inlen)
{
	int ret;
	
#if PARAM_CHECK
	if (NULL == versaloon_usbtoxxx_info.buff)
	{
		LOG_BUG(ERRMSG_INVALID_BUFFER, TO_STR(versaloon_usbtoxxx_info.buff));
		return ERRCODE_INVALID_BUFFER;
	}
	if ((0 == out_len) || (out_len > versaloon_usbtoxxx_info.buff_len))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	ret = usb_bulk_write(versaloon_device_handle, usb_param_epout(),
				(char *)versaloon_usbtoxxx_info.buff, out_len, versaloon_to);
	if (ret != out_len)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION_ERRSTRING, "send usb data",
					usb_strerror());
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (inlen != NULL)
	{
		ret = usb_bulk_read(versaloon_device_handle, usb_param_epin(),
							(char *)versaloon_usbtoxxx_info.buff,
							versaloon_usbtoxxx_info.buff_len, versaloon_to);
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






// Interfaces:
// Core
static vsf_err_t versaloon_fini(void)
{
	if (versaloon_device_handle != NULL)
	{
		usbtoxxx_fini();
		usbtoxxx_info = NULL;
		
		usb_release_interface(versaloon_device_handle, usb_param_interface());
		usb_close(versaloon_device_handle);
		versaloon_device_handle = NULL;
	}
	
	return VSFERR_NONE;
}

#define VERSALOON_RETRY_CNT				10
static vsf_err_t versaloon_init(void *p)
{
	uint16_t ret = 0;
	uint8_t retry;
	uint32_t timeout_tmp;
	vsf_err_t err = VSFERR_NONE;
	
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
	if (!versaloon_usbtoxxx_info.buff_len)
	{
		versaloon_usbtoxxx_info.buff_len = 256;
	}
	versaloon_usbtoxxx_info.buff =
						(uint8_t *)malloc(versaloon_usbtoxxx_info.buff_len);
	if (NULL == versaloon_usbtoxxx_info.buff)
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
		versaloon_usbtoxxx_info.buff[0] = VERSALOON_GET_INFO;
		if (!versaloon_transact(1, &ret) && (ret >= 3))
		{
			break;
		}
	}
	LOG_POP();
	versaloon_to = timeout_tmp;
	if (VERSALOON_RETRY_CNT == retry)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "communicate with versaloon");
		err = ERRCODE_FAILURE_OPERATION;
		goto versaloon_init_fail;
	}
	
	versaloon_usbtoxxx_info.buff[ret] = 0;
	versaloon_usbtoxxx_info.buff_len =
								GET_LE_U16(&versaloon_usbtoxxx_info.buff[0]);
	LOG_INFO("%s", &versaloon_usbtoxxx_info.buff[2]);
	
	// free temporary buffer
	free(versaloon_usbtoxxx_info.buff);
	versaloon_usbtoxxx_info.buff = NULL;
	
	usbtoxxx_info = &versaloon_usbtoxxx_info;
	if (usbtoxxx_init())
	{
		err = VSFERR_FAIL;
		goto versaloon_init_fail;
	}
	
	return VSFERR_NONE;
versaloon_init_fail:
	versaloon_fini();
	return err;
}

static vsf_err_t versaloon_reset(void)
{
	return VSFERR_NONE;
}

// Commit
static vsf_err_t versaloon_peripheral_commit(void)
{
	vsf_err_t err = usbtoxxx_execute_command();
	versaloon_to = VERSALOON_TIMEOUT;
	return err;
}

// tick clock
vsf_err_t versaloon_tickclk_init(void)
{
	return VSFERR_NONE;
}

vsf_err_t versaloon_tickclk_fini(void)
{
	return VSFERR_NONE;
}

vsf_err_t versaloon_tickclk_start(void)
{
	return VSFERR_NONE;
}

vsf_err_t versaloon_tickclk_stop(void)
{
	// not supported
	return VSFERR_FAIL;
}

uint32_t versaloon_tickclk_get_count(void)
{
	return (uint32_t)(clock() / (CLOCKS_PER_SEC / 1000));
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
		versaloon_tickclk_init,
		versaloon_tickclk_fini,
		versaloon_tickclk_start,
		versaloon_tickclk_stop,
		versaloon_tickclk_get_count,
	},
	{	// delay
		usbtodelay_init,
		usbtodelay_delayms,
		usbtodelay_delayus
	},
	versaloon_peripheral_commit
};

