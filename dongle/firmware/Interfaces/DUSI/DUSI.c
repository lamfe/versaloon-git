/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       DUSI.c                                                    *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    DUSI interface implementation file                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_DUSI_EN

#include "interfaces.h"
#include "../SPI/SPI.h"
#include "DUSI.h"

static uint8_t DUSI_GetDivFromFreq(uint16_t kHz)
{
	return SPI_GetSCKDiv(kHz);
}

static void DUSI_Fini(void)
{
	JTAG_TAP_HS_DMA_FINI();

	JTAG_TAP_HS_PortFini();

	SPI_I2S_DeInit(JTAG_TAP_HS_SPI_M);
	SPI_I2S_DeInit(JTAG_TAP_HS_SPI_S);
}

static void DUSI_Init(uint16_t kHz, uint8_t cpol, uint8_t cpha, 
						uint8_t first_bit)
{
	DUSI_Fini();
	DUSI_Config(DUSI_GetDivFromFreq(kHz), first_bit, cpol, cpha);
}

RESULT dusi_init(uint8_t index)
{
	switch (index)
	{
	case 0:
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT dusi_fini(uint8_t index)
{
	switch (index)
	{
	case 0:
		DUSI_Fini();
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT dusi_config(uint8_t index, uint16_t kHz, uint8_t cpol, uint8_t cpha, 
					 uint8_t first_bit)
{
	switch (index)
	{
	case 0:
		if(cpol & SPI_CPOL_MASK)
		{
			cpol = SPI_CPOL_High;
		}
		else
		{
			cpol = SPI_CPOL_Low;
		}
		if(cpha & SPI_CPHA_MASK)
		{
			// 2 edge
			cpha = SPI_CPHA_2Edge;
		}
		else
		{
			// 1 edge
			cpha = SPI_CPHA_1Edge;
		}
		if(first_bit & SPI_FIRSTBIT_MASK)
		{
			// msb first
			first_bit = SPI_FirstBit_MSB;
		}
		else
		{
			// lsb first
			first_bit = SPI_FirstBit_LSB;
		}
		DUSI_Init(kHz, first_bit, cpol, cpha);
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

RESULT dusi_io(uint8_t index, uint8_t *mo, uint8_t *mi, uint8_t *so, uint8_t *si, 
			   uint32_t bitlen)
{
	uint32_t i;
	uint8_t tmp;
	
	switch (index)
	{
	case 0:
		// currently support byte mode ONLY
		bitlen /= 8;
		for(i = 0; i < bitlen; i++)
		{
			if (so != NULL)
			{
				DUSI_SlaveOutBytePtr(so);
				so++;
			}
			if (mo != NULL)
			{
				tmp = *mo;
				mo++;
			}
			else
			{
				tmp = 0;
			}
			DUSI_MasterOutByte(tmp);
			
			JTAG_TAP_HS_WaitReady();
			
			if (si != NULL)
			{
				*si = DUSI_SlaveInByte();
				si++;
			}
			if (mi != NULL)
			{
				*mi = DUSI_MasterInByte();
				mi++;
			}
		}
		return ERROR_OK;
	default:
		return ERROR_FAIL;
	}
}

#endif
