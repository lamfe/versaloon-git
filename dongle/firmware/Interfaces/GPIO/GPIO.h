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

#define GPIO_DIR_MSK			0x01
#define GPIO_SRST				(1 << 0)
#define GPIO_TRST				(1 << 1)
#if JTAG_HAS_USER_PIN
#	define GPIO_USR1			(1 << 2)
#	define GPIO_USR2			(1 << 3)
#else
#	define GPIO_USR1			0
#	define GPIO_USR2			0
#endif
#define GPIO_TCK				(1 << 4)
#define GPIO_TDO				(1 << 5)
#define GPIO_TDI				(1 << 6)
#define GPIO_RTCK				(1 << 7)
#define GPIO_TMS				(1 << 8)
#define GPIO_OUT_MSK			(GPIO_SRST | GPIO_TRST | GPIO_USR1 | GPIO_USR2 | GPIO_TCK | GPIO_TDI | GPIO_TMS)
#define GPIO_IN_MSK				(GPIO_SRST | GPIO_TRST | GPIO_USR1 | GPIO_USR2 | GPIO_TDO | GPIO_RTCK | GPIO_TMS)
#define GPIO_MSK				(GPIO_OUT_MSK | GPIO_IN_MSK)

RESULT gpio_init(uint8_t index);
RESULT gpio_fini(uint8_t index);
RESULT gpio_config(uint8_t index, uint16_t pin_mask, uint16_t io, 
					 uint16_t input_pull_mask);
RESULT gpio_out(uint8_t index, uint16_t pin_mask, uint16_t value);
RESULT gpio_in(uint8_t index, uint16_t pin_mask, uint16_t *value);
