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
#include "interfaces.h"
#include "stm32f10x_conf.h"
#include "HW.h"

#include "STM32_EINT.h"

#define STM32_EINT_NUM					20

RESULT stm32_eint_init(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_EINT_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	return ERROR_OK;
}

RESULT stm32_eint_fini(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_EINT_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	EXTI->IMR = 0x00000000;
	EXTI->EMR = 0x00000000;
	EXTI->RTSR = 0x00000000; 
	EXTI->FTSR = 0x00000000; 
	EXTI->PR = 0x000FFFFF;
	return ERROR_OK;
}

RESULT stm32_eint_config(uint8_t index, bool on_fall, bool on_rise, 
							void (*callback)(void))
{
	uint32_t mask =  (1 << index);
	
#if __VSF_DEBUG__
	if ((index >= STM32_EINT_NUM) || (!on_fall && !on_rise))
	{
		return ERROR_FAIL;
	}
#endif
	
	if (on_fall)
	{
		EXTI->FTSR |= mask;
	}
	else
	{
		EXTI->FTSR &= ~mask;
	}
	if (on_rise)
	{
		EXTI->RTSR |= mask;
	}
	else
	{
		EXTI->RTSR &= ~mask;
	}
	return ERROR_OK;
}

RESULT stm32_eint_enable(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_EINT_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	EXTI->IMR |= (1 << index);
	return ERROR_OK;
}

RESULT stm32_eint_disable(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_EINT_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	EXTI->IMR &= ~(1 << index);
	return ERROR_OK;
}

RESULT stm32_eint_trigger(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_EINT_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	EXTI->SWIER |= (1 << index);
	return ERROR_OK;
}
