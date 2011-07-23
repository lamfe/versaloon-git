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

RESULT stm32_gpio_init(uint8_t index);
RESULT stm32_gpio_fini(uint8_t index);
RESULT stm32_gpio_config_pin(uint8_t index, uint8_t pin_idx, uint8_t mode);
RESULT stm32_gpio_config(uint8_t index, uint32_t pin_mask, uint32_t io, 
							uint32_t pull_en_mask, uint32_t input_pull_mask);
RESULT stm32_gpio_set(uint8_t index, uint32_t pin_mask);
RESULT stm32_gpio_clear(uint8_t index, uint32_t pin_mask);
RESULT stm32_gpio_out(uint8_t index, uint32_t pin_mask, uint32_t value);
RESULT stm32_gpio_in(uint8_t index, uint32_t pin_mask, uint32_t *value);
uint32_t stm32_gpio_get(uint8_t index, uint32_t pin_mask);
