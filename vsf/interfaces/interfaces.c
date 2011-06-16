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

RESULT peripheral_commit(void)
{
	return ERROR_OK;
}

const struct interfaces_info_t core_interfaces = 
{
	(void *)&CORE_INFO(__TARGET_CHIP__),
	
	CORE_INIT(__TARGET_CHIP__),
	CORE_FINI(__TARGET_CHIP__),
	
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
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{
		// spi
		NULL, NULL, NULL, NULL
	},
	{
		// i2c
		NULL, NULL, NULL, NULL, NULL
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
			&CORE_USBD_EP_NUM(__TARGET_CHIP__),
			CORE_USBD_EP_RESET(__TARGET_CHIP__),
			CORE_USBD_EP_SET_TYPE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_TYPE(__TARGET_CHIP__),
			
			CORE_USBD_EP_SET_IN_HANDLER(__TARGET_CHIP__),
			CORE_USBD_EP_SET_IN_DBUFFER(__TARGET_CHIP__),
			CORE_USBD_EP_IS_IN_DBUFFER(__TARGET_CHIP__),
			CORE_USBD_EP_SWITCH_IN_BUFFER(__TARGET_CHIP__),
			CORE_USBD_EP_SET_IN_EPSIZE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_IN_EPSIZE(__TARGET_CHIP__),
			CORE_USBD_EP_RESET_IN_TOGGLE(__TARGET_CHIP__),
			CORE_USBD_EP_TOGGLE_IN_TOGGLE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_IN_STATE(__TARGET_CHIP__),
			CORE_USBD_EP_SET_IN_STATE(__TARGET_CHIP__),
			CORE_USBD_EP_SET_IN_COUNT(__TARGET_CHIP__),
			CORE_USBD_EP_WRITE_IN_BUFFER(__TARGET_CHIP__),
			
			CORE_USBD_EP_SET_OUT_HANDLER(__TARGET_CHIP__),
			CORE_USBD_EP_SET_OUT_DBUFFER(__TARGET_CHIP__),
			CORE_USBD_EP_IS_OUT_DBUFFER(__TARGET_CHIP__),
			CORE_USBD_EP_SWITCH_OUT_BUFFER(__TARGET_CHIP__),
			CORE_USBD_EP_SET_OUT_EPSIZE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_OUT_EPSIZE(__TARGET_CHIP__),
			CORE_USBD_EP_RESET_OUT_TOGGLE(__TARGET_CHIP__),
			CORE_USBD_EP_TOGGLE_OUT_TOGGLE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_OUT_STATE(__TARGET_CHIP__),
			CORE_USBD_EP_SET_OUT_STATE(__TARGET_CHIP__),
			CORE_USBD_EP_GET_OUT_COUNT(__TARGET_CHIP__),
			CORE_USBD_EP_READ_OUT_BUFFER(__TARGET_CHIP__),
		},
	},
	{
		// pwm
		NULL, NULL, NULL, NULL, NULL
	},
	{
		// microwre
		NULL, NULL, NULL, NULL, NULL
	},
	{
		// delay
		NULL, NULL
	},
	peripheral_commit
};

struct interfaces_info_t *interfaces = 
								(struct interfaces_info_t *)&core_interfaces;
