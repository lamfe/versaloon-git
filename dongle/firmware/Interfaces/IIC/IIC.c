/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       IIC.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    IIC interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_IIC_EN

#include "IIC_MOD_Common.h"
#include "EMIIC_MOD.h"
#include "IIC.h"

DEFINE_EMIIC_MOD(USBTOXXX, IIC_SCL_CLR, IIC_SCL_SET, IIC_SCL_GET, IIC_SDA_CLR, IIC_SDA_SET, IIC_SDA_GET, DelayUS, uint16)

uint16 IIC_Delay;

uint8 IIC_SetParameter(uint16 kHz, uint16 ByteInterval, uint16 max_dly)
{
	uint16 clock_cycle = 1000 / kHz;
	EMIIC_USBTOXXX_SetParameter(clock_cycle, max_dly, 1, ByteInterval);
	return 0;
}

uint8 IIC_Init(uint16 kHz, uint16 ByteInterval, uint16 max_dly)
{
	IIC_PULL_INIT();
	EMIIC_USBTOXXX_Init();
	return IIC_SetParameter(kHz, ByteInterval, max_dly);
}

void IIC_Fini(void)
{
	EMIIC_USBTOXXX_DeInit();
}

uint8 IIC_Write(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop, uint16 *actual_len)
{
	IIC_STOP_t iic_stop;

	if (stop)
	{
		iic_stop = IIC_FORCESTOP;
	}
	else
	{
		iic_stop = IIC_NOSTOP;
	}
	return (uint8)EMIIC_USBTOXXX_Send(chip_addr, data, data_len, iic_stop, actual_len);
}

uint8 IIC_Read(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop, uint16 *actual_len)
{
	IIC_STOP_t iic_stop;

	if (stop)
	{
		iic_stop = IIC_FORCESTOP;
	}
	else
	{
		iic_stop = IIC_NOSTOP;
	}
	return (uint8)EMIIC_USBTOXXX_Receive(chip_addr, data, data_len, iic_stop, actual_len);
}

#endif
