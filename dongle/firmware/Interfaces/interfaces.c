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

#include "USART.h"
#include "PowerExt.h"
#include "SPI.h"
#include "GPIO.h"
#include "ISSP.h"
#include "SWD.h"
#include "JTAG_TAP.h"
#include "MSP430_JTAG.h"
#include "C2.h"
#include "IIC.h"
#include "LPC_ICP.h"
#include "SWIM.h"
#include "BDM.h"

// work around, because RESULT is comflict with struct in usb_core.h
RESULT target_voltage_set(uint8_t index, uint16_t voltage);
RESULT target_voltage_get(uint8_t index, uint16_t *voltage);

// delay
RESULT delay_delayms(uint16_t ms)
{
	DelayMS(ms);
	return ERROR_OK;
}
RESULT delay_delayus(uint16_t us)
{
	DelayUS(us);
	return ERROR_OK;
}

RESULT peripheral_commit(void)
{
	return ERROR_OK;
}

const struct interfaces_info_t internal_interfaces = 
{
	{
		// target_voltage
		target_voltage_get,
		target_voltage_set
	},
	{
		// usart
		usart_init,
		usart_fini,
		usart_config,
		usart_send,
		usart_receive,
		usart_status
	},
	{
		// spi
		spi_init,
		spi_fini,
		spi_config,
		spi_io
	},
	{
		// gpio
		gpio_init,
		gpio_fini,
		gpio_config,
		gpio_out,
		gpio_in
	},
	{
		// delay
		delay_delayms,
		delay_delayus
	},
	{
		// issp
		issp_init,
		issp_fini,
		issp_enter_program_mode,
		issp_leave_program_mode,
		issp_wait_and_poll,
		issp_vector
	},
	{
		// swd
		swd_init,
		swd_fini,
		swd_config,
		swd_seqout,
		swd_seqin,
		swd_transact
	},
	{
		// jtag_hl
		jtaghl_init,
		jtaghl_fini,
		jtaghl_config,
		jtaghl_tms,
		jtaghl_runtest,
		jtaghl_ir,
		jtaghl_dr,
		jtaghl_register_callback
	},
	{
		// jtag_ll
		jtagll_init,
		jtagll_fini,
		jtagll_config,
		jtagll_tms,
		jtagll_tms_clocks,
		jtagll_scan
	},
	{
		// jtag_raw
		jtagraw_init,
		jtagraw_fini,
		jtagraw_config,
		jtagraw_execute
	},
	{
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
	},
	{
		// msp430sbw
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{
		// c2
		c2_init,
		c2_fini,
		c2_addr_write,
		c2_addr_read,
		c2_data_write,
		c2_data_read
	},
	{
		// i2c
		iic_init,
		iic_fini,
		iic_config,
		iic_read,
		iic_write
	},
	{
		// lpcicp
		lpcicp_init,
		lpcicp_fini,
		lpcicp_enter_program_mode,
		lpcicp_in,
		lpcicp_out,
		lpcicp_poll_ready
	},
	{
		// swim
		swim_init,
		swim_fini,
		swim_config,
		swim_srst,
		swim_wotf,
		swim_rotf,
		swim_sync,
		swim_enable
	},
	{
		// bdm
		bdm_init,
		bdm_fini,
		bdm_sync,
		bdm_transact
	},
	peripheral_commit
};

const struct interfaces_info_t *interfaces = &internal_interfaces;
