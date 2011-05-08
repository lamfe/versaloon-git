/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK500v2_AVR32_JTAG.h                                     *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation for STK500v2 protocol for AVR32 JTAG       *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/
#include "app_cfg.h"

#include "STK500v2_Const.h"
#include "STK500v2.h"

#include "interfaces.h"

#include "Target/AVR32_JTAG/AVR32_JTAG.h"

uint8_t STK500V2_AVR32_JTAG_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint32_t i, length;
	uint8_t sab_addr[5], sab_data[5], tmp8;

	switch (dat[0])
	{
	case CMND_JTAG_INSTR:
//dat[idx]	para(value)				Description
//dat[1]	IR Code					5 bits
		dat[1] = AVR32_JTAG_Instr(dat[1]);

		STK500V2_RSP_SCAN_CHAIN_READ(1);
		break;
	case CMND_JTAG_DATA:
//dat[idx]	para(value)				Description
//dat[1]	DR Length				in bits
//dat[2].	DR Codes
		length = dat[1];

		AVR32_JTAG_DataPtr(dat + 2, dat + 1, length);

		length = (length + 7) >> 3;
		for(i = 1; i <= (length >> 1); i++)
		{
			tmp8 = dat[i];
			dat[i] = dat[length - i + 1];
			dat[length - i + 1] = tmp8;
		}

		STK500V2_RSP_SCAN_CHAIN_READ(length);
		break;
	case CMND_JTAG_SAB_WRITE:
//dat[idx]	para(value)				Description
//dat[1]	Address					5 bytes
//dat[6]	Data					4 bytes
		for(i = 0; i < 5; i++)
		{
			sab_addr[i] = dat[5 - i];
		}
		for(i = 0; i < 4; i++)
		{
			sab_data[i] = dat[9 - i];
		}

		tmp8 = AVR32_JTAG_SAB_WordAccess(sab_addr, sab_data, AVR32_JTAG_WRITE);
		if(tmp8 > 0)
		{
			// Error
			STK500V2_RSP_FAILED();
			return 1;
		}

		STK500V2_RSP_OK();
		break;
	case CMND_JTAG_SAB_READ:
//dat[idx]	para(value)				Description
//dat[1]	Address					5 bytes
		for(i = 0; i < 5; i++)
		{
			sab_addr[i] = dat[5 - i];
		}

		tmp8 = AVR32_JTAG_SAB_WordAccess(sab_addr, sab_data, AVR32_JTAG_READ);
		if(tmp8 > 0)
		{
			// Error
			STK500V2_RSP_FAILED();
			return 1;
		}

		for(i = 0; i < 4; i++)
		{
			dat[1 + i] = sab_data[3 - i];
		}
		STK500V2_RSP_SCAN_CHAIN_READ(4);
		break;
	case CMND_JTAG_BLOCK_WRITE:
//dat[idx]	para(value)				Description
//dat[1]	Length					in words(4 bytes)
//dat[5]	Address					5 bytes
//data[10].	Data
		length = dat[1];
		for(i = 0; i < 5; i++)
		{
			sab_addr[i] = dat[9 - i];
		}

		for(i = 0; i < length; i++)
		{
			tmp8 = dat[10+i*4];
			dat[10+i*4] = dat[13+i*4];
			dat[13+i*4] = tmp8;

			tmp8 = dat[11+i*4];
			dat[11+i*4] = dat[12+i*4];
			dat[12+i*4] = tmp8;
		}

		tmp8 = AVR32_JTAG_SAB_WordBlockAccess(sab_addr, dat + 10, AVR32_JTAG_WRITE, length);
		if(tmp8 > 0)
		{
			// Error
			STK500V2_RSP_FAILED();
			return 1;
		}

		STK500V2_RSP_OK();
		break;
	case CMND_JTAG_BLOCK_READ:
//dat[idx]	para(value)				Description
//dat[1]	Length					in words(4 bytes)
//dat[2]	Address					5 bytes
		length = dat[1];
		for(i = 0; i < 5; i++)
		{
			sab_addr[i] = dat[6 - i];
		}

		tmp8 = AVR32_JTAG_SAB_WordBlockAccess(sab_addr, dat + 1, AVR32_JTAG_READ, length);
		if(tmp8 > 0)
		{
			// Error
			STK500V2_RSP_FAILED();
			return 1;
		}

		for(i = 0; i < length; i++)
		{
			tmp8 = dat[1+i*4];
			dat[1+i*4] = dat[4+i*4];
			dat[4+i*4] = tmp8;

			tmp8 = dat[2+i*4];
			dat[2+i*4] = dat[3+i*4];
			dat[3+i*4] = tmp8;
		}
		STK500V2_RSP_SCAN_CHAIN_READ(length * 4);
		break;
	default:
		STK500V2_RSP_ILLEGAL_COMMAND();
		break;
	}
	return 0;
}