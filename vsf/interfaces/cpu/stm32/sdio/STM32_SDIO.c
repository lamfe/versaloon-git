/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       SDIO.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    GPIO interface header file                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2012-01-23:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_type.h"
#include "interfaces.h"

#if IFS_SDIO_EN

#include "STM32_SDIO.h"

#define STM32_RCC_AHBENR_DMA2			((uint32_t)1 << 1)
#define STM32_RCC_AHBENR_SDIO			((uint32_t)1 << 10)

#define STM32_SDIO_CLKCR_BYPASS			((uint32_t)1 << 10)
#define STM32_SDIO_CLKCR_1B				((uint32_t)0)
#define STM32_SDIO_CLKCR_4B				((uint32_t)1 << 11)
#define STM32_SDIO_CLKCR_8B				((uint32_t)1 << 12)
#define STM32_SDIO_CLKCR_CLKEN			((uint32_t)1 << 8)

#define STM32_SDIO_POWER_PWROFF			0
#define STM32_SDIO_POWER_PWRON			0x03

#define STM32_SDIO_CMD_CPSMEN			((uint32_t)1 << 10)
#define STM32_SDIO_CMD_RESP_NO			((uint32_t)0 << 6)
#define STM32_SDIO_CMD_RESP_SHORT		((uint32_t)1 << 6)
#define STM32_SDIO_CMD_RESP_LONG		((uint32_t)3 << 6)

#define STM32_SDIO_STA_CMDSENT			((uint32_t)1 << 7)
#define STM32_SDIO_STA_CMDREND			((uint32_t)1 << 6)
#define STM32_SDIO_STA_DTIMEOUT			((uint32_t)1 << 3)
#define STM32_SDIO_STA_CTIMEOUT			((uint32_t)1 << 2)
#define STM32_SDIO_STA_DCRCFAIL			((uint32_t)1 << 1)
#define STM32_SDIO_STA_CCRCFAIL			((uint32_t)1 << 0)

#define STM32_SDIO_NUM					1

vsf_err_t stm32_sdio_fini(uint8_t index)
{
#if __VSF_DEBUG__
	if (spi_idx >= STM32_SDIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	SDIO->POWER = 0x00000000;
	SDIO->CLKCR = 0x00000000;
	SDIO->ARG = 0x00000000;
	SDIO->CMD = 0x00000000;
	SDIO->DTIMER = 0x00000000;
	SDIO->DLEN = 0x00000000;
	SDIO->DCTRL = 0x00000000;
	SDIO->ICR = 0x00C007FF;
	SDIO->MASK = 0x00000000;
	return VSFERR_NONE;
}

vsf_err_t stm32_sdio_init(uint8_t index)
{
#if __VSF_DEBUG__
	if (spi_idx >= STM32_SDIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	RCC->APB2ENR |= RCC_APB2Periph_GPIOC | RCC_APB2Periph_GPIOD;
	// PC8, PC9, PC10, PC11, PC12 as AFPP
	GPIOC->CRH = (GPIOC->CRH & ~(0xFFFFF << ((8 - 8) * 4))) | 
					(uint32_t)stm32_GPIO_AFPP << ((8 - 8) * 4) |
					(uint32_t)stm32_GPIO_AFPP << ((9 - 8) * 4) |
					(uint32_t)stm32_GPIO_AFPP << ((10 - 8) * 4) |
					(uint32_t)stm32_GPIO_AFPP << ((11 - 8) * 4) |
					(uint32_t)stm32_GPIO_AFPP << ((12 - 8) * 4);
	// PD2 as AFPP
	GPIOD->CRL = (GPIOD->CRL & ~(0x0F << (2 * 4))) | 
					(uint32_t)stm32_GPIO_AFPP << (2 * 4);
	RCC->AHBENR |= STM32_RCC_AHBENR_SDIO | STM32_RCC_AHBENR_DMA2;
	
	return stm32_sdio_fini(index);
}

vsf_err_t stm32_sdio_config(uint8_t index, uint16_t kHz, uint8_t buswidth)
{
	struct stm32_info_t *info;
	uint32_t src_kHz;
	uint32_t temp_reg;
	uint32_t clk_div;
	
#if __VSF_DEBUG__
	if ((spi_idx >= STM32_SDIO_NUM) ||
		((buswidth != 1) && (buswidth != 4) && (buswidth != 8)))
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	if (stm32_interface_get_info(&info))
	{
		return VSFERR_FAIL;
	}
	src_kHz = info->ahb_freq_hz / 1000;
	
	temp_reg = (1 == buswidth) ? STM32_SDIO_CLKCR_1B :
					(4 == buswidth) ? STM32_SDIO_CLKCR_4B : STM32_SDIO_CLKCR_8B;
	clk_div = src_kHz / kHz;
	if (!clk_div)
	{
		temp_reg |= STM32_SDIO_CLKCR_BYPASS;
	}
	else
	{
		if (clk_div < 2)
		{
			clk_div = 2;
		}
		if (clk_div > 0xFF)
		{
			clk_div = 0xFF;
		}
		temp_reg |= (clk_div - 2);
	}
	SDIO->CLKCR = temp_reg;
	
	return VSFERR_NONE;
}

vsf_err_t stm32_sdio_enable(uint8_t index)
{
#if __VSF_DEBUG__
	if (spi_idx >= STM32_SDIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	SDIO->POWER = STM32_SDIO_POWER_PWRON;
	SDIO->CLKCR |= STM32_SDIO_CLKCR_CLKEN;
	return VSFERR_NONE;
}

vsf_err_t stm32_sdio_disable(uint8_t index)
{
#if __VSF_DEBUG__
	if (spi_idx >= STM32_SDIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	SDIO->CLKCR &= ~STM32_SDIO_CLKCR_CLKEN;
	SDIO->POWER = STM32_SDIO_POWER_PWROFF;
	return VSFERR_NONE;
}

static uint32_t stm32_sdio_cmd_to[STM32_SDIO_NUM];
vsf_err_t stm32_sdio_send_cmd(uint8_t index, uint8_t cmd, uint32_t arg,
								uint8_t resp)
{
#if __VSF_DEBUG__
	if (spi_idx >= STM32_SDIO_NUM)
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	cmd &= 0x3F;
	
	SDIO->ARG = arg;
	SDIO->CMD = cmd | STM32_SDIO_CMD_CPSMEN | resp;
	stm32_sdio_cmd_to[index] = 0;
	return VSFERR_NONE;
}

vsf_err_t stm32_sdio_send_cmd_isready(uint8_t index, uint8_t resp)
{
	uint32_t status = SDIO->STA;
	
	if (status & STM32_SDIO_STA_CTIMEOUT)
	{
		SDIO->ICR = STM32_SDIO_STA_CTIMEOUT;
		return VSFERR_FAIL;
	}
	
	// crc is not processed in cmd phase
	if (resp != stm32_SDIO_RESP_NONE)
	{
		if (status & (STM32_SDIO_STA_CMDREND | STM32_SDIO_STA_CCRCFAIL))
		{
			SDIO->ICR = STM32_SDIO_STA_CMDREND | STM32_SDIO_STA_CCRCFAIL;
			return VSFERR_NONE;
		}
	}
	else
	{
		if (status & STM32_SDIO_STA_CCRCFAIL)
		{
			SDIO->ICR = STM32_SDIO_STA_CCRCFAIL;
		}
		if (status & STM32_SDIO_STA_CMDSENT)
		{
			SDIO->ICR = STM32_SDIO_STA_CMDSENT;
			return VSFERR_NONE;
		}
	}
	
	return VSFERR_NOT_READY;
}

vsf_err_t stm32_sdio_get_resp(uint8_t index, uint8_t *cresp, uint32_t *resp,
								uint8_t resp_num)
{
	uint32_t i;
	
#if __VSF_DEBUG__
	if ((spi_idx >= STM32_SDIO_NUM) || (resp_num > 4))
	{
		return VSFERR_NOT_SUPPORT;
	}
#endif
	
	if (cresp != NULL)
	{
		*cresp = SDIO->RESPCMD;
	}
	if (resp != NULL)
	{
		for (i = 0; i < resp_num; i++)
		{
			resp[i] = ((uint32_t *)&SDIO->RESP1)[i];
		}
	}
	return VSFERR_NONE;
}

vsf_err_t stm32_sdio_data_tx(uint8_t index, uint32_t timeout, uint32_t size,
								uint32_t block_size, uint8_t *buffer)
{
}

vsf_err_t stm32_sdio_data_tx_isready(uint8_t index, uint32_t timeout,
							uint32_t size, uint32_t block_size, uint8_t *buffer)
{
}

vsf_err_t stm32_sdio_data_rx(uint8_t index, uint32_t timeout, uint32_t size,
								uint32_t block_size, uint8_t *buff)
{
}

vsf_err_t stm32_sdio_data_rx_isready(uint8_t index, uint32_t timeout,
							uint32_t size, uint32_t block_size, uint8_t *buffer)
{
}

#endif
