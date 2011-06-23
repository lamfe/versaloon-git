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

#include "STM32_TIM.h"

#define STM32_TIM_NUM					17
#define STM32_TIM_CHANNEL_NUM			4

static TIM_TypeDef * stm32_timer_index(uint8_t index)
{
	switch (index)
	{
	case 0:		return TIM1;
	case 1:		return TIM2;
	case 2:		return TIM3;
	case 3:		return TIM4;
	case 4:		return TIM5;
	case 5:		return TIM6;
	case 6:		return TIM7;
	case 7:		return TIM8;
	case 8:		return TIM9;
	case 9:		return TIM10;
	case 10:	return TIM11;
	case 11:	return TIM12;
	case 12:	return TIM13;
	case 13:	return TIM14;
	case 14:	return TIM15;
	case 15:	return TIM16;
	case 16:	return TIM17;
	default:	return NULL;
	}
}

RESULT stm32_timer_init(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_TIM_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	switch (index)
	{
	case 0:
		RCC->APB2RSTR |= RCC_APB2Periph_TIM1;
		break;
	case 1: case 2: case 3: case 4: case 5: case 6:
		RCC->APB1RSTR |= RCC_APB1Periph_TIM2 << (index - 1);
		break;
	case 7:
		RCC->APB2RSTR |= RCC_APB2Periph_TIM8;
		break;
	case 8: case 9: case 10:
		RCC->APB2RSTR |= RCC_APB2Periph_TIM9 << (index - 8);
		break;
	case 11: case 12: case 13:
		RCC->APB1RSTR |= RCC_APB1Periph_TIM12 << (index - 11);
		break;
	case 14: case 15: case 16:
		RCC->APB2RSTR |= RCC_APB2Periph_TIM15 << (index - 14);
		break;
	}
	return ERROR_OK;
}

RESULT stm32_timer_fini(uint8_t index)
{
	TIM_TypeDef * timer = stm32_timer_index(index);
	
#if __VSF_DEBUG__
	if (NULL == timer)
	{
		return ERROR_FAIL;
	}
#endif
	
	TIM_DeInit(timer);
	return ERROR_OK;
}

RESULT stm32_timer_config(uint8_t index, uint32_t kHz, uint32_t mode, 
							void (*overflow)(void))
{
	TIM_TypeDef * timer = stm32_timer_index(index);
	
#if __VSF_DEBUG__
	if (NULL == timer)
	{
		return ERROR_FAIL;
	}
#endif
	
	return ERROR_OK;
}

RESULT stm32_timer_start(uint8_t index)
{
	TIM_TypeDef * timer = stm32_timer_index(index);
	
#if __VSF_DEBUG__
	if (NULL == timer)
	{
		return ERROR_FAIL;
	}
#endif
	
	timer->CR1 |= TIM_CR1_CEN;
	return ERROR_OK;
}

RESULT stm32_timer_stop(uint8_t index)
{
	TIM_TypeDef * timer = stm32_timer_index(index);
	
#if __VSF_DEBUG__
	if (NULL == timer)
	{
		return ERROR_FAIL;
	}
#endif
	
	timer->CR1 &= ~TIM_CR1_CEN;
	return ERROR_OK;
}

RESULT stm32_timer_get_count(uint8_t index, uint32_t *count)
{
	TIM_TypeDef * timer = stm32_timer_index(index);
	
#if __VSF_DEBUG__
	if (NULL == timer)
	{
		return ERROR_FAIL;
	}
#endif
	
	*count = (uint32_t)timer->CNT;
	return ERROR_OK;
}

RESULT stm32_timer_set_count(uint8_t index, uint32_t count)
{
	TIM_TypeDef * timer = stm32_timer_index(index);
	
#if __VSF_DEBUG__
	if (NULL == timer)
	{
		return ERROR_FAIL;
	}
#endif
	
	timer->CNT = (uint16_t)count;
	return ERROR_OK;
}

RESULT stm32_timer_config_channel(uint8_t index, uint8_t channel, 
									uint32_t mode, void (*callback)(void))
{
	TIM_TypeDef * timer = stm32_timer_index(index);
	
#if __VSF_DEBUG__
	if (NULL == timer)
	{
		return ERROR_FAIL;
	}
#endif
	
	return ERROR_OK;
}

RESULT stm32_timer_get_channel(uint8_t index, uint8_t channel, uint32_t *count)
{
	TIM_TypeDef * timer = stm32_timer_index(index);
	
#if __VSF_DEBUG__
	if ((NULL == timer) || (channel >= STM32_TIM_CHANNEL_NUM))
	{
		return ERROR_FAIL;
	}
#endif
	
	*count = (uint32_t)*(uint16_t *)(&timer->CCR1 + channel * 4);
	return ERROR_OK;
}

RESULT stm32_timer_set_channel(uint8_t index, uint8_t channel, uint32_t count)
{
	TIM_TypeDef * timer = stm32_timer_index(index);
	
#if __VSF_DEBUG__
	if ((NULL == timer) || (channel >= STM32_TIM_CHANNEL_NUM))
	{
		return ERROR_FAIL;
	}
#endif
	
	*(uint16_t *)(&timer->CCR1 + channel * 4) = (uint16_t)count;
	return ERROR_OK;
}

