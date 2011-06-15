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
#include "interfaces/interfaces.h"
#include "stm32f10x_conf.h"
#include "HW.h"

#include "STM32_GPIO.h"

#define STM32_GPIO_NUM					7

RESULT stm32_gpio_init(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_GPIO_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	RCC->APB2RSTR |= RCC_APB2Periph_GPIOA << index;
	return ERROR_OK;
}

RESULT stm32_gpio_fini(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_GPIO_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	RCC->APB2RSTR &= ~(RCC_APB2Periph_GPIOA << index);
	return ERROR_OK;
}

RESULT stm32_gpio_config(uint8_t index, uint32_t pin_mask, uint32_t io, 
							uint32_t pull_en_mask, uint32_t input_pull_mask)
{
	GPIO_TypeDef *gpio;
	uint32_t mask, tmpregl, tmpregh;
	uint8_t i;
	
#if __VSF_DEBUG__
	if (index >= STM32_GPIO_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + 0x400 * index);
	tmpregl = gpio->CRL;
	tmpregh = gpio->CRH;
	for (i = 0; i < 8; i++)
	{
		mask = (1 << i);
		if (pin_mask & mask)
		{
			tmpregl &= 0x0F << (4 * i);
			if (io & mask)
			{
				tmpregl |= 0x03 << (4 * i);
				if (pull_en_mask & input_pull_mask & mask)
				{
					tmpregl |= 0x01 << (4 * i + 2);
				}
			}
			else
			{
				if (pull_en_mask & mask)
				{
					tmpregl |= 0x10 << (4 * i + 2);
					if (input_pull_mask & mask)
					{
						gpio->BSRR = mask;
					}
					else
					{
						gpio->BRR = mask;
					}
				}
				else
				{
					tmpregl |= 0x01 << (4 * i + 2);
				}
			}
		}
		mask = (1 << (i + 8));
		if (pin_mask & mask)
		{
			tmpregh &= 0x0F << (4 * i);
			if (io & mask)
			{
				tmpregh |= 0x03 << (4 * i);
				if (pull_en_mask & input_pull_mask & mask)
				{
					tmpregh |= 0x01 << (4 * i + 2);
				}
			}
			else
			{
				if (pull_en_mask & mask)
				{
					tmpregh |= 0x10 << (4 * i + 2);
					if (input_pull_mask & mask)
					{
						gpio->BSRR = mask;
					}
					else
					{
						gpio->BRR = mask;
					}
				}
				else
				{
					tmpregh |= 0x01 << (4 * i + 2);
				}
			}
		}
	}
	gpio->CRL = tmpregl;
	gpio->CRH = tmpregh;
	return ERROR_OK;
}

RESULT stm32_gpio_out(uint8_t index, uint32_t pin_mask, uint32_t value)
{
	GPIO_TypeDef *gpio;
	
#if __VSF_DEBUG__
	if (index >= STM32_GPIO_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + 0x400 * index);
	gpio->BSRR = pin_mask & value;
	gpio->BRR = pin_mask & ~value;
	return ERROR_OK;
}

RESULT stm32_gpio_in(uint8_t index, uint32_t pin_mask, uint32_t *value)
{
	GPIO_TypeDef *gpio;
	
#if __VSF_DEBUG__
	if (index >= STM32_GPIO_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	gpio = (GPIO_TypeDef *)(GPIOA_BASE + 0x400 * index);
	*value = gpio->IDR & pin_mask;
	return ERROR_OK;
}
