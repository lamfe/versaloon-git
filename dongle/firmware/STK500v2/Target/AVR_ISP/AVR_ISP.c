/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       AVR_ISP.c                                                 *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation for AVR_ISP support                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "interfaces.h"
#include "GPIO.h"
#include "AVR_ISP.h"

/// ISP Communicate
/// @param[in]		data	data to send
/// @param[out]		ret		data received
void AVRISP_CommInt(uint8_t *data, uint8_t *ret, uint32_t len)
{
	interfaces->spi.io(0, data, ret, len);
}

/// ISP Wait Ready
/// @return		Success or not
uint8_t AVRISP_RDY_Wait()
{
	uint8_t dly = 255;
	uint8_t cmd[4];

poll:
	cmd[0] = 0xF0;
	cmd[1] = 0x00;
	cmd[2] = 0x00;
	cmd[3] = 0x00;
	AVRISP_Comm((uint8_t*)cmd, (uint8_t*)cmd);

	if((cmd[3] & 1) && --dly)
	{
		interfaces->delay.delayus(80);
		goto poll;
	}

	if(dly)
	{
		return AVRISP_Success;
	}
	else
	{
		return AVRISP_Error;
	}
}

/// ISP Initialization
/// @return
void AVRISP_Init(uint32_t freq)
{
	interfaces->target_voltage.set(0, 3300);
	interfaces->delay.delayms(1);
	interfaces->gpio.init(0);
	interfaces->gpio.config(0, GPIO_SRST, GPIO_SRST, GPIO_SRST, GPIO_SRST);
	interfaces->spi.init(0);
	interfaces->spi.config(0, freq / 1000, SPI_CPOL_LOW, SPI_CPHA_1EDGE, SPI_MSB_FIRST);
}

/// ISP Finilization
/// @return
void AVRISP_Fini(void)
{
	interfaces->spi.fini(0);
	interfaces->gpio.config(0, GPIO_SRST, 0, GPIO_SRST, GPIO_SRST);
	interfaces->target_voltage.set(0, 0);
}
