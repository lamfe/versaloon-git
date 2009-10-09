/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       USB_TO_SPI.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation file for USB_TO_SPI                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if USB_TO_SPI_EN

#include "USB_TO_XXX.h"
#include "SPI.h"
#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

#define USB_TO_SPI_GetSPI(index)			SPI_Interface
#define USB_TO_SPI_HW_Enable(index)			SPI_AllSPIHW()
#define USB_TO_SPI_GPIO_Enable(index)		SPI_AllSPIIO()

void USB_TO_SPI_ProcessCmd(uint8* dat, uint16 len)
{
	uint16 index, device_num, length;
	uint8 command;

	uint8 cpol, cpha, firstbit, attr;
	uint16 frequency, i;

	index = 0;
	while(index < len)
	{
		command = dat[index] & USB_TO_XXX_CMDMASK;
		device_num = dat[index] & USB_TO_XXX_IDXMASK;
		if(device_num >= USB_TO_SPI_NUM)
		{
			buffer_out[rep_len++] = USB_TO_XXX_INVALID_INDEX;
			return;
		}
		length = dat[index + 1] + (dat[index + 2] << 8);
		index += 3;

		switch(command)
		{
		case USB_TO_XXX_INIT:
			buffer_out[rep_len++] = USB_TO_XXX_OK;
			buffer_out[rep_len++] = USB_TO_SPI_NUM;

			GLOBAL_OUTPUT_Acquire();
			PWREXT_Acquire();
			DelayMS(1);

			break;
		case USB_TO_XXX_CONFIG:
			attr = dat[index];
			frequency = dat[index + 1] + (dat[index + 2] << 8);
			index += 3;

			if(attr & USB_TO_SPI_CPOL_HIGH)
			{
				cpol = SPI_CPOL_High;
			}
			else
			{
				cpol = SPI_CPOL_Low;
			}
			if(attr & USB_TO_SPI_CPHA_2EDGE)
			{
				// 2 edge
				cpha = SPI_CPHA_2Edge;
			}
			else
			{
				// 1 edge
				cpha = SPI_CPHA_1Edge;
			}
			if(attr & USB_TO_SPI_MSB_FIRST)
			{
				// msb first
				firstbit = SPI_FirstBit_MSB;
			}
			else
			{
				// lsb first
				firstbit = SPI_FirstBit_LSB;
			}

			if(frequency >= SPI_MIN_SPEED)
			{
				// SPI Mode
				USB_TO_SPI_HW_Enable(index);
				SPI_Configuration(USB_TO_SPI_GetSPI(index), SPI_Mode_Master, SPI_GetSCKDiv(frequency), firstbit, cpol, cpha);
			}
			else
			{
				// GPIO Mode
				USB_TO_SPI_GPIO_Enable();
				SPI_SetClk(frequency * 1000);
			}

			buffer_out[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_FINI:
			SPI_I2S_DeInit(USB_TO_SPI_GetSPI(i));
			SPI_AllInput();

			PWREXT_Release();
			GLOBAL_OUTPUT_Release();

			buffer_out[rep_len++] = USB_TO_XXX_OK;

			break;
		case USB_TO_XXX_IN_OUT:
			buffer_out[rep_len++] = USB_TO_XXX_OK;

			for(i = 0; i < length; i++)
			{
				buffer_out[rep_len++] = SPI_RW(dat[index + i]);
			}

			break;
		default:
			buffer_out[rep_len++] = USB_TO_XXX_CMD_NOT_SUPPORT;

			break;
		}
		index += length;
	}
}

#endif
