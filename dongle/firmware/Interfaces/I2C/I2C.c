/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       I2C.c                                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    I2C interface implementation file                         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_I2C_EN

#include "IIC_MOD_Common.h"
#include "EMIIC_MOD.h"
#include "I2C.h"

uint16_t dead_cnt = 0;

#define I2C_SCL_SET_HOOK()			do{\
										if (dead_cnt)\
										{\
											dead_cnt--;\
										}\
										if (!dead_cnt)\
										{\
											return IIC_MOD_NACK;\
										}\
										I2C_SCL_SET();\
									}while(0)
#define I2C_SCL_CLR_HOOK()			do{\
										if (dead_cnt)\
										{\
											dead_cnt--;\
										}\
										if (!dead_cnt)\
										{\
											return IIC_MOD_NACK;\
										}\
										I2C_SCL_CLR();\
									}while(0)

DEFINE_EMIIC_MOD(USBTOXXX, I2C_SCL_CLR_HOOK, I2C_SCL_SET_HOOK, I2C_SCL_GET, I2C_SDA_CLR, I2C_SDA_SET, I2C_SDA_GET, DelayUS, uint16)

uint16 I2C_Delay;

uint8 I2C_Init(uint16 kHz)
{
	I2C_PULL_INIT();
	EMIIC_USBTOXXX_Init();
	return I2C_SetParameter(kHz);
}

void I2C_Fini(void)
{
	EMIIC_USBTOXXX_DeInit();
}

uint8 I2C_SetParameter(uint16 kHz)
{
	uint16 dly = 500 / kHz;
	EMIIC_USBTOXXX_SetParameter(dly, dly, 2048, dly, 0);
	return 0;
}

uint16 I2C_Write(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop)
{
	uint16 actual_len = 0;

	if (stop)
	{
		EMIIC_USBTOXXX_Send(chip_addr, data, data_len, IIC_FORCESTOP, &actual_len);
	}
	else
	{
		EMIIC_USBTOXXX_Send(chip_addr, data, data_len, IIC_NOSTOP, &actual_len);
	}
	return actual_len;
}

uint16 I2C_Read(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop)
{
	uint16 actual_len = 0;

	if (stop)
	{
		EMIIC_USBTOXXX_Receive(chip_addr, data, data_len, IIC_FORCESTOP, &actual_len);
	}
	else
	{
		EMIIC_USBTOXXX_Receive(chip_addr, data, data_len, IIC_NOSTOP, &actual_len);
	}
	return actual_len;
}

#endif
