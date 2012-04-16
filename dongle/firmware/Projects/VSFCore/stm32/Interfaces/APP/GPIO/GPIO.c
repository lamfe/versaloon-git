/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       GPIO.h                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    GPIO interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_GPIO_EN

#include "app_interfaces.h"
#include "GPIO.h"

// GPIO0..5 are connected to PORTC.0..5
#define GPIO_PORTC_MASK				0x0000003F
// GPIO6.7.8.12.13.14.15 are connected to PORTG6.7.8.12.13.14.15
#define GPIO_PORTG_MASK				0x0000F1C
// GPIO9..11 are connected to PORTD9..11
#define GPIO_PORTF_MASK				0x00000E00

vsf_err_t gpio_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		core_interfaces.gpio.init(2);
		core_interfaces.gpio.init(5);
		core_interfaces.gpio.init(6);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t gpio_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t gpio_config(uint8_t index, uint32_t pin_mask, uint32_t io, 
					uint32_t pull_en_mask, uint32_t input_pull_mask)
{
	switch (index)
	{
	case 0:
		core_interfaces.gpio.config(2, pin_mask & GPIO_PORTC_MASK,
										io & GPIO_PORTC_MASK,
										pull_en_mask & GPIO_PORTC_MASK,
										input_pull_mask & GPIO_PORTC_MASK);
		core_interfaces.gpio.config(5, pin_mask & GPIO_PORTF_MASK,
										io & GPIO_PORTF_MASK,
										pull_en_mask & GPIO_PORTF_MASK,
										input_pull_mask & GPIO_PORTF_MASK);
		core_interfaces.gpio.config(6, pin_mask & GPIO_PORTG_MASK,
										io & GPIO_PORTG_MASK,
										pull_en_mask & GPIO_PORTG_MASK,
										input_pull_mask & GPIO_PORTG_MASK);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t gpio_clear(uint8_t index, uint32_t pin_mask)
{
	switch (index)
	{
	case 0:
		core_interfaces.gpio.clear(2, pin_mask & GPIO_PORTC_MASK);
		core_interfaces.gpio.clear(5, pin_mask & GPIO_PORTF_MASK);
		core_interfaces.gpio.clear(6, pin_mask & GPIO_PORTG_MASK);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t gpio_set(uint8_t index, uint32_t pin_mask)
{
	switch (index)
	{
	case 0:
		core_interfaces.gpio.set(2, pin_mask & GPIO_PORTC_MASK);
		core_interfaces.gpio.set(5, pin_mask & GPIO_PORTF_MASK);
		core_interfaces.gpio.set(6, pin_mask & GPIO_PORTG_MASK);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t gpio_out(uint8_t index, uint32_t pin_mask, uint32_t value)
{
	switch (index)
	{
	case 0:
		core_interfaces.gpio.out(2, pin_mask & GPIO_PORTC_MASK, value);
		core_interfaces.gpio.out(5, pin_mask & GPIO_PORTF_MASK, value);
		core_interfaces.gpio.out(6, pin_mask & GPIO_PORTG_MASK, value);
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

vsf_err_t gpio_in(uint8_t index, uint32_t pin_mask, uint32_t *value)
{
	uint32_t port_data;

	switch (index)
	{
	case 0:
		core_interfaces.gpio.in(2, pin_mask & GPIO_PORTC_MASK, &port_data);
		core_interfaces.gpio.in(5, pin_mask & GPIO_PORTF_MASK, &port_data);
		core_interfaces.gpio.in(6, pin_mask & GPIO_PORTG_MASK, &port_data);
		*value = port_data;
		return VSFERR_NONE;
	default:
		return VSFERR_NOT_SUPPORT;
	}
}

#endif
