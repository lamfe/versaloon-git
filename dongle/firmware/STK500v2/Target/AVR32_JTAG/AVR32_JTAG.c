/**************************************************************************
 *  Copyright (C) 2008 -2010 by Simon Qian                                *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       AVR32_JTAG.c                                              *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation for AVR32_JTAG support                     *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "interfaces.h"
#include "AVR32_JTAG.h"

#define AVR32_JTAG_SAB_Delay()		

#define AVR32_JTAG_MaxRetry				0//0xFFFF

uint32_t AVR32_JTAG_Instr(uint32_t instr)
{
	interfaces->jtag_hl.ir(0, (uint8_t *)&instr, AVR32_JTAG_INS_Len, AVR32_JTAG_RTI_CYCLE, 1);
	return instr;
}
uint32_t AVR32_JTAG_Data(uint32_t tdi, uint16_t bitlen)
{
	interfaces->jtag_hl.dr(0, (uint8_t *)&tdi, bitlen, AVR32_JTAG_RTI_CYCLE, 1);
	return tdi;
}
void AVR32_JTAG_DataInPtr(uint8_t *ptdo, uint16_t bitlen)
{
	interfaces->jtag_hl.dr(0, ptdo, bitlen, AVR32_JTAG_RTI_CYCLE, 1);
}
void AVR32_JTAG_DataOutPtr(uint8_t *ptdi, uint16_t bitlen)
{
	interfaces->jtag_hl.dr(0, ptdi, bitlen, AVR32_JTAG_RTI_CYCLE, 0);
}
void AVR32_JTAG_DataPtr(uint8_t *ptdi, uint8_t *ptdo, uint16_t bitlen)
{
	memcpy(ptdo, ptdi, (bitlen + 7) >> 3);
	interfaces->jtag_hl.dr(0, ptdo, bitlen, AVR32_JTAG_RTI_CYCLE, 1);
}

uint8_t AVR32_JTAG_CancelAccess(void)
{
#if AVR32_JTAG_MaxRetry > 0
	uint32_t cnt;
#endif
	uint8_t instr_ret;

#if AVR32_JTAG_MaxRetry > 0
	cnt = 0;
#endif
	do{
		instr_ret = (uint8_t)AVR32_JTAG_Instr(AVR32_JTAG_INS_CANCEL_ACCESS);
#if AVR32_JTAG_MaxRetry > 0
		if(++cnt > AVR32_JTAG_MaxRetry)
		{
			return 1;
		}
#endif
	}while(instr_ret & 0x0C);

	return 0;
}

uint8_t AVR32_JTAG_NexusAccess(uint8_t a, uint8_t *data, uint8_t r)
{
	uint8_t ret[5];
#if AVR32_JTAG_MaxRetry > 0
	uint32_t cnt;
#endif

	// Phase 1: Write AVR32_JTAG_INS_NEXUS_ACCESS to IR
#if AVR32_JTAG_MaxRetry > 0
	cnt = 0;
#endif
	do{
		ret[0] = AVR32_JTAG_Instr(AVR32_JTAG_INS_NEXUS_ACCESS);
		if(
#if AVR32_JTAG_MaxRetry > 0
			(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
			(ret[0] & 0x10))
		{
			return 1;
		}
	}while(ret[0] & 0x04);

	// Phase 2: Write Address and Read bit
	AVR32_JTAG_Data((a << 1) | (r > 0), 8);

	// Phase 3: Read/Write Data
#if AVR32_JTAG_MaxRetry > 0
	cnt = 0;
#endif
	if(r)
	{
		do{
			AVR32_JTAG_DataInPtr(data, 34);
			if(
#if AVR32_JTAG_MaxRetry > 0
				(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
				(ret[4] & 0x02))
			{
				return 2;
			}
		}while(data[4] & 0x01);
	}
	else
	{
		do{
			AVR32_JTAG_DataPtr(data, ret, 32);
/*
			No error will occur here
			if(
#if AVR32_JTAG_MaxRetry > 0
				(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
				(ret[0] & 0x02))
			{
				return 3;
			}
*/
		}while(ret[0] & 0x01);

		// Phase 4: Wait Ready
#if AVR32_JTAG_MaxRetry > 0
		cnt = 0;
#endif
		do{
			ret[0] = AVR32_JTAG_Instr(AVR32_JTAG_INS_MEMORY_WORD_ACCESS);
			if(
#if AVR32_JTAG_MaxRetry > 0
				(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
				(ret[0] & 0x10))
			{
				return 4;
			}
		}while(ret[0] & 0x04);
	}

	return 0;
}

uint8_t AVR32_JTAG_SAB_WordAccess(uint8_t *addr, uint8_t *data, uint8_t r)
{
	uint8_t cmd[5],ret[5];
#if AVR32_JTAG_MaxRetry > 0
	uint32_t cnt;
#endif

	// Phase 1: Write AVR32_JTAG_INS_MEMORY_WORD_ACCESS to IR
	AVR32_JTAG_Instr(AVR32_JTAG_INS_MEMORY_WORD_ACCESS);
#if AVR32_JTAG_MaxRetry > 0
	cnt = 0;
#endif
	do{
		ret[0] = AVR32_JTAG_Instr(AVR32_JTAG_INS_MEMORY_WORD_ACCESS);
		if(
#if AVR32_JTAG_MaxRetry > 0
			(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
			(ret[0] & 0x10))
		{
			return 1;
		}
	}while(ret[0] & 0x04);

	// Phase 2: Write Address
	cmd[0] = (r > 0) | (addr[0] >> 1) | (addr[1] << 7);
	cmd[1] = (addr[1] >> 1) | (addr[2] << 7);
	cmd[2] = (addr[2] >> 1) | (addr[3] << 7);
	cmd[3] = (addr[3] >> 1) | (addr[4] << 7);
	cmd[4] = (addr[4] >> 1);
	AVR32_JTAG_DataPtr(cmd, ret, 35);

	// Delay
	AVR32_JTAG_SAB_Delay();

	// Phase 3: Read/Write Data
#if AVR32_JTAG_MaxRetry > 0
	cnt = 0;
#endif
	if(r)
	{
		do{
			AVR32_JTAG_DataInPtr(data, 35);
			if(
#if AVR32_JTAG_MaxRetry > 0
			(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
			(data[4] & 0x02))
			{
				return 2;
			}
		}while(data[4] & 0x01);
	}
	else
	{
		do{
			AVR32_JTAG_DataPtr(data, ret, 32);
/*
			No error will occur here
			if(
#if AVR32_JTAG_MaxRetry > 0
				(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
				(ret[0] & 0x02))
			{
				return 3;
			}
*/
		}while(ret[0] & 0x01);

		// Phase 4: Wait Ready
#if AVR32_JTAG_MaxRetry > 0
		cnt = 0;
#endif
		do{
			ret[0] = AVR32_JTAG_Instr(AVR32_JTAG_INS_MEMORY_WORD_ACCESS);
			if(
#if AVR32_JTAG_MaxRetry > 0
				(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
				(ret[0] & 0x10))
			{
				return 4;
			}
		}while(ret[0] & 0x04);
	}

	return 0;
}

uint8_t AVR32_JTAG_SAB_WordBlockAccess(uint8_t *addr, uint8_t *data, uint8_t r, uint8_t len)
{
	uint8_t i;
	uint8_t ret[5];
#if AVR32_JTAG_MaxRetry > 0
	uint32_t cnt;
#endif

	// Phase 1: Read/Write the first Word
	if(AVR32_JTAG_SAB_WordAccess(addr, data, r))
	{
		return 1;
	}

	// Phase 2: Write AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS to IR
	AVR32_JTAG_Instr(AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS);

	// Phase 3: DR Read Loop
	for(i = 1; i < len; i++)
	{
#if AVR32_JTAG_MaxRetry > 0
		cnt = 0;
#endif
		if(r)
		{
			do{
				AVR32_JTAG_DataInPtr(data + 4 * i, 34);
				if(
#if AVR32_JTAG_MaxRetry > 0
					(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
					(data[4*i+4] & 0x02))
				{
					return 2;
				}
			}while(data[4*i+4] & 0x01);
		}
		else
		{
			do{
				AVR32_JTAG_DataPtr(data + 4 * i, ret, 32);
				if(
#if AVR32_JTAG_MaxRetry > 0
				(++cnt > AVR32_JTAG_MaxRetry) || 
#endif
				(ret[0] & 0x02))
				{
					return 3;
				}
			}while(ret[0] & 0x01);
		}
	}

	return 0;
}
