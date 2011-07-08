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

#include "STM32_SPI.h"

#define STM32_SPI_NUM					3

static const SPI_TypeDef *stm32_spis[STM32_SPI_NUM] = {SPI1, SPI2, SPI3};

static uint8_t stm32_spi_get_sck_div(uint32_t module_khz, uint32_t khz)
{
	// Set Speed
	if(khz >= module_khz / 4)
	{
		khz = 0;
	}
	else if(khz >= module_khz / 8)
	{
		khz = 1;
	}
	else if(khz >= module_khz / 16)
	{
		khz = 2;
	}
	else if(khz >= module_khz / 32)
	{
		khz = 3;
	}
	else if(khz >= module_khz / 64)
	{
		khz = 4;
	}
	else if(khz > module_khz / 128)
	{
		khz = 5;
	}
	else if(khz > module_khz / 256)
	{
		khz = 6;
	}
	else
	{
		khz = 7;
	}

	return (uint8_t)(khz << 3);
}

RESULT stm32_spi_init(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_SPI_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	switch (index)
	{
	case 0:
		RCC->APB2ENR |= RCC_APB2Periph_SPI1;
		break;
	case 1:
		RCC->APB1ENR |= RCC_APB1Periph_SPI2;
		SPI_AllSPIHW();
		break;
	case 2:
		RCC->APB1ENR |= RCC_APB1Periph_SPI3;
		break;
	}
	return ERROR_OK;
}

RESULT stm32_spi_fini(uint8_t index)
{
#if __VSF_DEBUG__
	if (index >= STM32_SPI_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	switch (index)
	{
	case 0:
		RCC->APB2RSTR &= ~RCC_APB2Periph_SPI1;
		break;
	case 1:
		RCC->APB1RSTR &= ~RCC_APB1Periph_SPI2;
		break;
	case 2:
		RCC->APB1RSTR &= ~RCC_APB1Periph_SPI2;
		break;
	}
	SPI_I2S_DeInit((SPI_TypeDef *)stm32_spis[index]);
	return ERROR_OK;
}

RESULT stm32_spi_config(uint8_t index, uint32_t kHz, uint8_t mode)
{
	uint32_t module_khz;
	uint32_t cpol, cpha, first_bit;
	
#if __VSF_DEBUG__
	if (index >= STM32_SPI_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	switch (index)
	{
	case 0:
		module_khz = _SYS_FREQUENCY * 1000;
		break;
	case 1:
	case 2:
		module_khz = _SYS_FREQUENCY * 500;
		break;
	}
	switch (mode & 0x03)
	{
	case 0:
		cpol = SPI_CPOL_Low;
		cpha = SPI_CPHA_1Edge;
		break;
	case 1:
		cpol = SPI_CPOL_Low;
		cpha = SPI_CPHA_2Edge;
		break;
	case 2:
		cpol = SPI_CPOL_High;
		cpha = SPI_CPHA_1Edge;
		break;
	case 3:
		cpol = SPI_CPOL_High;
		cpha = SPI_CPHA_2Edge;
		break;
	}
	if(mode & SPI_MSB_FIRST)
	{
		first_bit = SPI_FirstBit_MSB;
	}
	else
	{
		first_bit = SPI_FirstBit_LSB;
	}
	
	SPI_Configuration((SPI_TypeDef *)stm32_spis[index], SPI_Mode_Master, 
				stm32_spi_get_sck_div(module_khz, kHz), first_bit, cpol, cpha);
	return ERROR_OK;
}

RESULT stm32_spi_io(uint8_t index, uint8_t *out, uint8_t *in, uint16_t len)
{
	SPI_TypeDef *spi = (SPI_TypeDef *)stm32_spis[index];
	uint16_t i;
	uint8_t in8, out8;
	
#if __VSF_DEBUG__
	if (index >= STM32_SPI_NUM)
	{
		return ERROR_FAIL;
	}
#endif
	
	for(i = 0; i < len; i++)
	{
		if (NULL == out)
		{
			out8 = 0xFF;
		}
		else
		{
			out8 = out[i];
		}
		spi->DR = out8;
		
		while(!(spi->SR & SPI_I2S_FLAG_RXNE));
		in8 = spi->DR;
		
		if (in != NULL)
		{
			in[i] = in8;
		}
	}
	return ERROR_OK;
}
