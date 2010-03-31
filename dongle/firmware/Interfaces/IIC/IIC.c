/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
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

uint16_t dead_cnt = 0;

#define IIC_SCL_SET_HOOK()			do{\
										if (dead_cnt)\
										{\
											dead_cnt--;\
										}\
										if (!dead_cnt)\
										{\
											return IIC_MOD_NACK;\
										}\
										IIC_SCL_SET();\
									}while(0)
#define IIC_SCL_CLR_HOOK()			do{\
										if (dead_cnt)\
										{\
											dead_cnt--;\
										}\
										if (!dead_cnt)\
										{\
											return IIC_MOD_NACK;\
										}\
										IIC_SCL_CLR();\
									}while(0)

DEFINE_EMIIC_MOD(USBTOXXX, IIC_SCL_CLR_HOOK, IIC_SCL_SET_HOOK, IIC_SCL_GET, IIC_SDA_CLR, IIC_SDA_SET, IIC_SDA_GET, DelayUS, uint16)

uint16 IIC_Delay;

uint8 IIC_Init(uint16 kHz)
{
	IIC_PULL_INIT();
	EMIIC_USBTOXXX_Init();
	return IIC_SetParameter(kHz);
}

void IIC_Fini(void)
{
	EMIIC_USBTOXXX_DeInit();
}

uint8 IIC_SetParameter(uint16 kHz)
{
	uint16 dly = 500 / kHz;
	EMIIC_USBTOXXX_SetParameter(dly, dly, 2048, dly, 0);
	return 0;
}

uint16 IIC_Write(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop)
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

uint16 IIC_Read(uint8 chip_addr, uint8 *data, uint16 data_len, uint8 stop)
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
