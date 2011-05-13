/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       interfaces.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    interfaces implementation file                            *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-12-05:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#include "interfaces.h"

#if INTERFACE_USART_EN
#	include "USART/USART.h"
#endif
#if POWER_OUT_EN
#	include "PowerExt/PowerExt.h"
#endif
#if INTERFACE_SPI_EN
#	include "SPI/SPI.h"
#endif
#if INTERFACE_GPIO_EN
#	include "GPIO/GPIO.h"
#endif
#if INTERFACE_ISSP_EN
#	include "ISSP/ISSP.h"
#endif
#if INTERFACE_SWD_EN
#	include "SWD/SWD.h"
#endif
#if INTERFACE_JTAG_EN
#	include "JTAG/JTAG_TAP.h"
#endif
#if INTERFACE_MSP430_JTAG_EN
#	include "MSP430_JTAG/MSP430_JTAG.h"
#endif
#if INTERFACE_C2_EN
#	include "C2/C2.h"
#endif
#if INTERFACE_IIC_EN
#	include "IIC/IIC.h"
#endif
#if INTERFACE_LPC_ICP_EN
#	include "LPC_ICP/LPC_ICP.h"
#endif
#if INTERFACE_SWIM_EN
#	include "SWIM/SWIM.h"
#endif
#if INTERFACE_BDM_EN
#	include "BDM/BDM.h"
#endif
#if INTERFACE_DUSI_EN
#	include "DUSI/DUSI.h"
#endif
#if INTERFACE_MICROWIRE_EN
#	include "MicroWire/MicroWire.h"
#endif
#if INTERFACE_PWM_EN
#	include "PWM/PWM.h"
#endif

// work around, because RESULT is comflict with struct in usb_core.h
RESULT target_voltage_set(uint8_t index, uint16_t voltage);
RESULT target_voltage_get(uint8_t index, uint16_t *voltage);

// delay
static RESULT delay_delayms(uint16_t ms)
{
	DelayMS(ms);
	return ERROR_OK;
}
static RESULT delay_delayus(uint16_t us)
{
	DelayUS(us);
	return ERROR_OK;
}

static RESULT app_interface_init(void *p)
{
	return ERROR_OK;
}

static RESULT app_interface_fini(void)
{
	return ERROR_OK;
}

static RESULT app_peripheral_commit(void)
{
	return ERROR_OK;
}

static const struct interfaces_info_t app_interfaces = 
{
	app_interface_init,
	app_interface_fini,
	app_peripheral_commit,
	
	0
#if INTERFACE_USART_EN
	| IFS_USART 
#endif
#if INTERFACE_SPI_EN
	| IFS_SPI 
#endif
#if INTERFACE_IIC_EN
	| IFS_I2C 
#endif
#if INTERFACE_GPIO_EN
	| IFS_GPIO 
#endif
#if POWER_OUT_EN
	| IFS_POWER 
#endif
#if INTERFACE_ISSP_EN
	| IFS_ISSP 
#endif
#if INTERFACE_JTAG_EN
	| IFS_JTAG_LL | IFS_JTAG_HL | IFS_JTAG_RAW 
#endif
#if INTERFACE_SWIM_EN
	| IFS_SWIM 
#endif
#if INTERFACE_C2_EN
	| IFS_C2 
#endif
#if INTERFACE_MSP430_JTAG_EN
	| IFS_MSP430_JTAG 
#endif
#if INTERFACE_LPC_ICP_EN
	| IFS_LPC_ICP 
#endif
#if INTERFACE_SWD_EN
	| IFS_SWD 
#endif
#if INTERFACE_BDM_EN
	| IFS_BDM 
#endif
#if INTERFACE_DUSI_EN
	| IFS_DUSI 
#endif
#if INTERFACE_MICROWIRE_EN
	| IFS_MICROWIRE 
#endif
#if INTERFACE_PWM_EN
	| IFS_PWM
#endif
	
#if INTERFACE_GPIO_EN
	,{
		// gpio
		gpio_init,
		gpio_fini,
		gpio_config,
		gpio_out,
		gpio_in
	}
#endif
#if INTERFACE_USART_EN
	,{
		// usart
		usart_init,
		usart_fini,
		usart_config,
		usart_send,
		usart_receive,
		usart_status
	}
#endif
#if INTERFACE_SPI_EN
	,{
		// spi
		spi_init,
		spi_fini,
		spi_config,
		spi_io
	}
#endif
#if INTERFACE_IIC_EN
	,{
		// i2c
		iic_init,
		iic_fini,
		iic_config,
		iic_read,
		iic_write
	}
#endif
#if INTERFACE_PWM_EN
	,{
		// pwm
		pwm_init,
		pwm_fini,
		pwm_config,
		pwm_out,
		pwm_in
	}
#endif
// Allways included
	,{
		// delay
		delay_delayms,
		delay_delayus
	}
//
#if POWER_OUT_EN
	,{
		// target_voltage
		target_voltage_get,
		target_voltage_set
	}
#endif
#if INTERFACE_ISSP_EN
	,{
		// issp
		issp_init,
		issp_fini,
		issp_enter_program_mode,
		issp_leave_program_mode,
		issp_wait_and_poll,
		issp_vector
	}
#endif
#if INTERFACE_SWD_EN
	,{
		// swd
		swd_init,
		swd_fini,
		swd_config,
		swd_seqout,
		swd_seqin,
		swd_transact
	}
#endif
#if INTERFACE_JTAG_EN
	,{
		// jtag_hl
		jtaghl_init,
		jtaghl_fini,
		jtaghl_config_speed,
		jtaghl_config_daisychain,
		jtaghl_config,
		jtaghl_tms,
		jtaghl_runtest,
		jtaghl_ir,
		jtaghl_dr,
		jtaghl_register_callback
	}
#endif
#if INTERFACE_JTAG_EN
	,{
		// jtag_ll
		jtagll_init,
		jtagll_fini,
		jtagll_config,
		jtagll_tms,
		jtagll_tms_clocks,
		jtagll_scan
	}
#endif
#if INTERFACE_JTAG_EN
	,{
		// jtag_raw
		jtagraw_init,
		jtagraw_fini,
		jtagraw_config,
		jtagraw_execute
	}
#endif
#if INTERFACE_MSP430_JTAG_EN
	,{
		// msp430jtag
		msp430jtag_init,
		msp430jtag_fini,
		msp430jtag_config,
		msp430jtag_ir,
		msp430jtag_dr,
		msp430jtag_tclk,
		msp430jtag_tclk_strobe,
		msp430jtag_reset,
		msp430jtag_poll
	}
#endif
#if INTERFACE_MSP430_SBW_EN
	,{
		// msp430sbw
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	}
#endif
#if INTERFACE_C2_EN
	,{
		// c2
		c2_init,
		c2_fini,
		c2_addr_write,
		c2_addr_read,
		c2_data_write,
		c2_data_read
	}
#endif
#if INTERFACE_LPC_ICP_EN
	,{
		// lpcicp
		lpcicp_init,
		lpcicp_fini,
		lpcicp_enter_program_mode,
		lpcicp_in,
		lpcicp_out,
		lpcicp_poll_ready
	}
#endif
#if INTERFACE_SWIM_EN
	,{
		// swim
		swim_init,
		swim_fini,
		swim_config,
		swim_srst,
		swim_wotf,
		swim_rotf,
		swim_sync,
		swim_enable
	}
#endif
#if INTERFACE_BDM_EN
	,{
		// bdm
		bdm_init,
		bdm_fini,
		bdm_sync,
		bdm_transact
	}
#endif
#if INTERFACE_DUSI_EN
	,{
		// dusi
		dusi_init,
		dusi_fini,
		dusi_config,
		dusi_io
	}
#endif
#if INTERFACE_MICROWIRE_EN
	,{
		// microwire
		microwire_init,
		microwire_fini,
		microwire_config,
		microwire_transport,
		microwire_poll
	}
#endif
};

const struct interfaces_info_t *interfaces = &app_interfaces;





#include "GPIO/STM32_GPIO.h"
#include "USBD/STM32_USBD.h"

static RESULT core_interface_init(void *p)
{
	BeforeInit();
	Sys_Init();
	AfterInit();
	return ERROR_OK;
}

static RESULT core_interface_fini(void)
{
	return ERROR_OK;
}

const struct core_interfaces_info_t core_interfaces = 
{
	core_interface_init,
	core_interface_fini,
	
	{
		// gpio
		CORE_GPIO_INIT(__TARGET_CHIP__),
		CORE_GPIO_FINI(__TARGET_CHIP__),
		CORE_GPIO_CONFIG(__TARGET_CHIP__),
		CORE_GPIO_OUT(__TARGET_CHIP__),
		CORE_GPIO_IN(__TARGET_CHIP__)
	},
	{
		// usart
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL
	},
	{
		// spi
		NULL,
		NULL,
		NULL,
		NULL
	},
	{
		// i2c
		NULL,
		NULL,
		NULL,
		NULL,
		NULL
	},
	{
		// usbd
		CORE_USBD_INIT(__TARGET_CHIP__),
		CORE_USBD_FINI(__TARGET_CHIP__),
		CORE_USBD_RESET(__TARGET_CHIP__),
		CORE_USBD_POLL(__TARGET_CHIP__),
		CORE_USBD_CONNECT(__TARGET_CHIP__),
		CORE_USBD_DISCONNECT(__TARGET_CHIP__),
		CORE_USBD_SET_ADDRESS(__TARGET_CHIP__),
		CORE_USBD_GET_ADDRESS(__TARGET_CHIP__),
		CORE_USBD_SUSPEND(__TARGET_CHIP__),
		CORE_USBD_RESUME(__TARGET_CHIP__),
		CORE_USBD_LOWPOWER(__TARGET_CHIP__),
		CORE_USBD_GET_FRAME_NUM(__TARGET_CHIP__),
		// ep
		{
			CORE_USBD_EP_NUM(__TARGET_CHIP__),
			CORE_USBD_EP_RESET(__TARGET_CHIP__),
			CORE_USBD_EP_SET_TYPE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_TYPE(__TARGET_CHIP__),
			
			CORE_USBD_EP_SET_IN_HANDLER(__TARGET_CHIP__),
			CORE_USBD_EP_SET_IN_DBUFFER(__TARGET_CHIP__),
			CORE_USBD_EP_SET_IN_EPSIZE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_IN_EPSIZE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_IN_STATE(__TARGET_CHIP__),
			CORE_USBD_EP_SET_IN_STATE(__TARGET_CHIP__),
			CORE_USBD_EP_SET_IN_COUNT(__TARGET_CHIP__),
			CORE_USBD_EP_WRITE_IN_BUFFER(__TARGET_CHIP__),
			
			CORE_USBD_EP_SET_OUT_HANDLER(__TARGET_CHIP__),
			CORE_USBD_EP_SET_OUT_DBUFFER(__TARGET_CHIP__),
			CORE_USBD_EP_SET_OUT_EPSIZE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_OUT_EPSIZE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_OUT_STATE(__TARGET_CHIP__),
			CORE_USBD_EP_SET_OUT_STATE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_OUT_COUNT(__TARGET_CHIP__),
			CORE_USBD_EP_READ_OUT_BUFFER(__TARGET_CHIP__),
		},
	},
	{
		// pwm
		NULL,
		NULL,
		NULL,
		NULL,
		NULL
	},
	{
		// delay
		delay_delayms,
		delay_delayus
	}
};
