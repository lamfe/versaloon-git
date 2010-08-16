/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    Versaloon                                                 *
 *  File:       JTAG_TAP.c                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    JTAG interface implementation file                        *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"
#if INTERFACE_JTAG_EN

#include "JTAG_TAP.h"
#include "SPI.h"

#define JTAG_TAP_DR_IR_PREPARE_DLY()	

static uint32 JTAG_TAP_UnitsBefore, JTAG_TAP_UnitsAfter, JTAG_TAP_BitsBefore, JTAG_TAP_BitsAfter;
void (*JTAG_TAP_Operate_RAW)(uint32 bit_len, uint8 *tdi, uint8 *tms, uint8 *tdo);
uint16 (*JTAG_TAP_Operate_Asyn)(uint16 tdi, uint16 tms);

static int16 JTAG_kHz = 0xFFFF;

static void JTAG_TAP_RTCK_Wait(uint8 signal)
{
	uint32 retry = 1000;	// 1000us = 1ms, min clock is 1K

	while (retry--)
	{
		if ((signal && JTAG_TAP_RTCK_GET()) 
			|| (!signal && !JTAG_TAP_RTCK_GET()))
		{
			break;
		}
		DelayUS(1);
	}
}

static uint16 JTAG_TAP_GPIO_Operate_Asyn(uint16 tdi, uint16 tms)
{
	static uint16 tdo = 0;
	uint16 result = tdo;
	uint8 i;

	tdo = 0;
	for (i = 0; i < 8; i++)
	{
		tdo >>= 1;
		JTAG_TAP_TCK_CLR();
		if (!JTAG_kHz)
		{
			JTAG_TAP_RTCK_Wait(0);
		}

		if (tdi & 1)
		{
			JTAG_TAP_TDI_SET();
		}
		else
		{
			JTAG_TAP_TDI_CLR();
		}
		if (tms & 1)
		{
			JTAG_TAP_TMS_SET();
		}
		else
		{
			JTAG_TAP_TMS_CLR();
		}

		if (JTAG_kHz)
		{
			DelayUS(500 / JTAG_kHz);
		}
		JTAG_TAP_TCK_SET();
		if (JTAG_TAP_TDO_GET())
		{
			tdo |= 0x80;
		}
		if (JTAG_kHz)
		{
			DelayUS(500 / JTAG_kHz);
		}
		else
		{
			JTAG_TAP_RTCK_Wait(1);
		}

		tdi >>= 1;
		tms >>= 1;
	}

	return result;
}

static void JTAG_TAP_GPIO_Operate_RAW(uint32 bit_len, uint8 *tdi, uint8 *tms, uint8 *tdo)
{
	uint32 offset;
	uint8 mask;
	uint32 i;

	for (i = 0; i < bit_len; i++)
	{
		offset = i >> 3;
		mask = 1 << (i & 7);
		if (1 == mask)
		{
			tdo[offset] = 0;
		}

		JTAG_TAP_TCK_CLR();
		if (!JTAG_kHz)
		{
			JTAG_TAP_RTCK_Wait(0);
		}

		if (tdi[offset] & mask)
		{
			JTAG_TAP_TDI_SET();
		}
		else
		{
			JTAG_TAP_TDI_CLR();
		}
		if (tms[offset] & mask)
		{
			JTAG_TAP_TMS_SET();
		}
		else
		{
			JTAG_TAP_TMS_CLR();
		}

		if (JTAG_kHz)
		{
			DelayUS(500 / JTAG_kHz);
		}
		JTAG_TAP_TCK_SET();
		if (JTAG_TAP_TDO_GET())
		{
			tdo[offset] |= mask;
		}
		if (JTAG_kHz)
		{
			DelayUS(500 / JTAG_kHz);
		}
		else
		{
			JTAG_TAP_RTCK_Wait(1);
		}
	}
}

static void JTAG_TAP_HS_Operate_RAW_DMA(uint32 bit_len, uint8 *tdi, uint8 *tms, uint8 *tdo)
{
	uint16 i, byte_len = bit_len >> 3;

	if (byte_len)
	{
		JTAG_TAP_HS_SPI_M_RX_DMA_LEN(byte_len);
		JTAG_TAP_HS_SPI_M_RX_DMA_ADDR((uint32)tdo);
		JTAG_TAP_HS_SPI_M_RX_DMA_EN();
		JTAG_TAP_HS_SPI_S_TX_DMA_LEN(byte_len);
		JTAG_TAP_HS_SPI_S_TX_DMA_ADDR((uint32)tms);
		JTAG_TAP_HS_SPI_S_TX_DMA_EN();

		for(i = 0; i < byte_len; i++)
		{
			JTAG_TAP_HS_WaitTxReady();
			JTAG_TAP_HS_TDI_Out(tdi[i]);
		}

		JTAG_TAP_HS_SPI_M_RX_DMA_WAIT();
		JTAG_TAP_HS_SPI_S_TX_DMA_WAIT();

		JTAG_TAP_HS_SPI_M_RX_DMA_DIS();
		JTAG_TAP_HS_SPI_S_TX_DMA_DIS();
	}
	tdi += byte_len;
	tms += byte_len;
	tdo += byte_len;
	if (bit_len & 7)
	{
		JTAG_TAP_HS_SPIS_Disable();
		JTAG_TAP_TCK_SET();
		JTAG_TAP_TCK_SETOUTPUT();
		JTAG_TAP_TDO_SETINPUT();
		JTAG_TAP_TDI_SETOUTPUT();
		JTAG_TAP_TMS_SETOUTPUT();
		
		JTAG_TAP_GPIO_Operate_RAW(bit_len & 7, tdi, tms, tdo);

		JTAG_TAP_HS_SPIS_Enable();
		SPI_Cmd(JTAG_TAP_HS_SPI_S, ENABLE);
		JTAG_TAP_HS_PortIOInit();
	}
}

static uint16 JTAG_TAP_HS_Operate_Asyn(uint16 tdi, uint16 tms)
{
	uint16 tdo;

	JTAG_TAP_HS_WaitRxReady();
	tdo = JTAG_TAP_HS_In();
	JTAG_TAP_HS_Out(tms, tdi);

	return tdo;
}





void JTAG_TAP_RW(uint8 *tdo, uint8 *tdi, uint8 tms_before, uint8 tms_after0, uint8 tms_after1, uint16 dat_byte_len)
{
	uint8 tdo_tmp;
	uint16 ret_len = 0, cur_pos = 0;

	if(dat_byte_len & 0x8000)
	{
		JTAG_TAP_Operate_Asyn(tdi[cur_pos++], tms_before);
		ret_len++;
		dat_byte_len &= 0x7FFF;
		dat_byte_len--;
	}

	JTAG_TAP_DR_IR_PREPARE_DLY();
	dat_byte_len--;
	while(dat_byte_len-- > 0)
	{
		tdo_tmp = JTAG_TAP_Operate_Asyn(tdi[cur_pos++], 0);
		if(ret_len > 0)
		{
			tdo[ret_len - 1] = tdo_tmp;
		}
		ret_len++;
	}
	tdo_tmp = JTAG_TAP_Operate_Asyn(tdi[cur_pos], tms_after0);
	if(ret_len > 0)
	{
		tdo[ret_len - 1] = tdo_tmp;
	}
	tdo_tmp = JTAG_TAP_Operate_Asyn(0, tms_after1);
	tdo[ret_len] = tdo_tmp;
}

void JTAG_TAP_R(uint8 *tdo, uint8 tms_before, uint8 tms_after0, uint8 tms_after1, uint16 dat_byte_len)
{
	uint8 tdo_tmp;
	uint16 ret_len = 0;

	if(dat_byte_len & 0x8000)
	{
		JTAG_TAP_Operate_Asyn(0, tms_before);
		ret_len++;
		dat_byte_len &= 0x7FFF;
		dat_byte_len--;
	}

	JTAG_TAP_DR_IR_PREPARE_DLY();
	dat_byte_len--;
	while(dat_byte_len-- > 0)
	{
		tdo_tmp = JTAG_TAP_Operate_Asyn(0, 0);
		if(ret_len > 0)
		{
			tdo[ret_len - 1] = tdo_tmp;
		}
		ret_len++;
	}
	tdo_tmp = JTAG_TAP_Operate_Asyn(0, tms_after0);
	if(ret_len > 0)
	{
		tdo[ret_len - 1] = tdo_tmp;
	}
	tdo_tmp = JTAG_TAP_Operate_Asyn(0, tms_after1);
	tdo[ret_len] = tdo_tmp;
}

void JTAG_TAP_W(uint8 *tdi, uint8 tms_before, uint8 tms_after0, uint8 tms_after1, uint16 dat_byte_len)
{
	uint16 cur_pos = 0;

	if(dat_byte_len & 0x8000)
	{
		JTAG_TAP_Operate_Asyn(tdi[cur_pos++], tms_before);
		dat_byte_len &= 0x7FFF;
		dat_byte_len--;
	}

	JTAG_TAP_DR_IR_PREPARE_DLY();
	dat_byte_len--;
	while(dat_byte_len-- > 0)
	{
		JTAG_TAP_Operate_Asyn(tdi[cur_pos++], 0);
	}
	JTAG_TAP_Operate_Asyn(tdi[cur_pos], tms_after0);
	JTAG_TAP_Operate_Asyn(0, tms_after1);
}












uint8 JTAG_TAP_1s[] = {0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, 0xFF};
uint8 JTAG_TAP_TMS_scrap = 0;
uint8 JTAG_TAP_TMS_scraplen = 0;
void JTAG_TAP_TMS_Bit(uint8* tms, uint8 bit_len)
{
	while (bit_len >= 8)
	{
		JTAG_TAP_WriteTMSByte_ASYN(*tms);
		tms++;
		bit_len -= 8;
	}
	if (bit_len)
	{
		JTAG_TAP_TMS_scraplen = bit_len;
		JTAG_TAP_TMS_scrap = *tms;
	}
}

static void JTAG_TAP_ProcessDataRW(uint8 *tdo, uint8 *tdi, uint8 tms_before, uint8 tms_len_before, uint16 bit_len, uint8 len_of_1s_before, uint8 len_of_1s_after, uint8 idle)
{
	uint8 tdi_tmp, tdo_tmp, tms_tmp, len_tmp;
	uint8 offset, Rec_offset;
	uint16 iSend = 0, iReceive = 0, iTmp = 0, bit_len_remain, receiveFromByte;

	bit_len_remain = bit_len;
	receiveFromByte = len_of_1s_before + tms_len_before;
	Rec_offset = receiveFromByte & 0x07;
	receiveFromByte = (receiveFromByte + 7) >> 3;

	// process TMS scrap
	if (JTAG_TAP_TMS_scraplen)
	{
		if ((tms_len_before + JTAG_TAP_TMS_scraplen) > 8)
		{
			JTAG_TAP_TMS_scrap |= tms_before << JTAG_TAP_TMS_scraplen;
			JTAG_TAP_WriteTMSByte_ASYN(JTAG_TAP_TMS_scrap);

			tms_before >>= 8 - JTAG_TAP_TMS_scraplen;
			tms_len_before -= 8 - JTAG_TAP_TMS_scraplen;
			JTAG_TAP_TMS_scraplen = 0;
		}
		else
		{
			tms_before |= JTAG_TAP_TMS_scrap << tms_len_before;
			tms_len_before += JTAG_TAP_TMS_scraplen;
			JTAG_TAP_TMS_scraplen = 0;
		}
	}

	tms_tmp = tms_before;
	tdi_tmp = 0;
	len_tmp = tms_len_before;
	offset = tms_len_before;

	while(len_of_1s_before + bit_len + len_of_1s_after > 0)
	{
		if(len_of_1s_before > 0)
		{
			if(len_tmp > 0)
			{
				if(len_of_1s_before >= 8 - len_tmp)
				{
					tdi_tmp = JTAG_TAP_1s[8 - len_tmp] << offset;
				}
				else
				{
					tdi_tmp = JTAG_TAP_1s[len_of_1s_before] << offset;
				}
			}
			else
			{
				if(len_of_1s_before >= 8)
				{
					tdi_tmp = 0xFF;
				}
				else
				{
					tdi_tmp = JTAG_TAP_1s[len_of_1s_before];
				}
			}
			if((len_of_1s_before + len_tmp) <= 8)
			{
				len_tmp += len_of_1s_before;
				len_of_1s_before = 0;
				offset = len_tmp;
				if(offset == 8)
				{
					offset = 0;
				}
			}
			else
			{
				len_of_1s_before -= 8 - len_tmp;
				len_tmp = 8;
			}
		}
		if((bit_len > 0) && (len_tmp < 8))
		{
			if(tdi != (uint8*)0)
			{
				if(iSend > 0)
				{
					tdi_tmp = tdi[iSend - 1] >> (8 - offset);
					if(bit_len > offset)
					{
						tdi_tmp |= tdi[iSend] << offset;
					}
				}
				else
				{
					tdi_tmp |= tdi[iSend] << offset;
				}
				iSend++;
			}
			if((bit_len + len_tmp) <= 8)
			{
				len_tmp += bit_len;
				bit_len = 0;
				offset = len_tmp;
				if(offset == 8)
				{
					offset = 0;
				}
			}
			else
			{
				bit_len -= 8 - len_tmp;
				len_tmp = 8;
			}
		}
		if((len_of_1s_after > 0) && (len_tmp < 8))
		{
			if(len_tmp > 0)
			{
				if(len_of_1s_after >= 8 - len_tmp)
				{
					tdi_tmp |= JTAG_TAP_1s[8 - len_tmp] << offset;
				}
				else
				{
					tdi_tmp |= JTAG_TAP_1s[len_of_1s_after] << offset;
				}
			}
			else
			{
				if(len_of_1s_after >= 8)
				{
					tdi_tmp = 0xFF;
				}
				else
				{
					tdi_tmp = JTAG_TAP_1s[len_of_1s_after];
				}
			}
			if((len_of_1s_after + len_tmp) <= 8)
			{
				len_tmp += len_of_1s_after;
				len_of_1s_after = 0;
			}
			else
			{
				len_of_1s_after -= 8 - len_tmp;
				len_tmp = 8;
			}
		}

		if((len_of_1s_before + bit_len + len_of_1s_after) == 0)
		{
			tms_tmp |= 1 << (len_tmp - 1);
		}
		tdo_tmp = JTAG_TAP_Operate_Asyn(tdi_tmp, tms_tmp);
		len_tmp = 0;
		tms_tmp = 0;

		if(tdo != (uint8*)0)
		{
			if(iReceive > 0)
			{
				if((bit_len_remain > 0) && (iReceive >= receiveFromByte))
				{
					iTmp = iReceive - receiveFromByte;
					if(iTmp > 0)
					{
						tdo[iTmp - 1] |= tdo_tmp << (8 - Rec_offset);
						bit_len_remain -= Rec_offset;
					}
					if((iTmp == 0) || ((iTmp > 0) && (bit_len_remain > Rec_offset)))
					{
						tdo[iTmp] = tdo_tmp >> Rec_offset;
						bit_len_remain -= 8 - Rec_offset;
					}
				}
			}
			iReceive++;
		}
	}

	len_tmp = idle & 0x07;
	if(len_tmp < 6)
	{
		tdo_tmp = JTAG_TAP_Operate_Asyn(0, JTAG_TAP_TMS_E12UPDATE >> idle);
	}
	else
	{
		tdo_tmp = JTAG_TAP_Operate_Asyn(0, JTAG_TAP_TMS_E12UPDATE << (8 - idle));
		JTAG_TAP_Operate_Asyn(0, JTAG_TAP_TMS_E12UPDATE >> idle);
	}
	if((tdo != (uint8*)0) && (bit_len_remain > 0))
	{
		iTmp = iReceive - receiveFromByte;
		if(iTmp > 0)
		{
			tdo[iTmp - 1] |= tdo_tmp << (8 - Rec_offset);
			if(bit_len_remain > Rec_offset)
			{
				tdo[iTmp] = tdo_tmp >> Rec_offset;
			}
		}
		else
		{
			tdo[iTmp] = tdo_tmp >> Rec_offset;
		}
	}
	len_tmp = idle >> 3;
	while(len_tmp--)
	{
		JTAG_TAP_Operate_Asyn(0, 0);
	}
}

uint32 JTAG_TAP_Instr(uint32 instr, uint8 bit_len, uint8 idle)
{
	uint32 ret;

	JTAG_TAP_ProcessDataRW((uint8*)&ret,
						   (uint8*)&instr,
						   JTAG_TAP_TMS_UPDATERTI2SI,
						   JTAG_TAP_TMS_UPDATERTI2SI_LEN,
						   bit_len,
						   JTAG_TAP_BitsBefore,
						   JTAG_TAP_BitsAfter,
						   idle);

	return ret;
}

void JTAG_TAP_InstrPtr(uint8 *instr, uint8 *tdo, uint16 bit_len, uint8 idle)
{
	JTAG_TAP_ProcessDataRW(tdo,
						   instr,
						   JTAG_TAP_TMS_UPDATERTI2SI,
						   JTAG_TAP_TMS_UPDATERTI2SI_LEN,
						   bit_len,
						   JTAG_TAP_BitsBefore,
						   JTAG_TAP_BitsAfter,
						   idle);
}

void JTAG_TAP_InstrOutPtr(uint8 *instr, uint16 bit_len, uint8 idle)
{
	JTAG_TAP_ProcessDataRW((uint8*)0,
						   instr,
						   JTAG_TAP_TMS_UPDATERTI2SI,
						   JTAG_TAP_TMS_UPDATERTI2SI_LEN,
						   bit_len,
						   JTAG_TAP_BitsBefore,
						   JTAG_TAP_BitsAfter,
						   idle);
}

void JTAG_TAP_DataOutPtr(uint8 *tdi, uint16 bit_len, uint8 idle)
{
	JTAG_TAP_ProcessDataRW((uint8*)0,
						   tdi,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);
}

void JTAG_TAP_DataInPtr(uint8 *tdo, uint16 bit_len, uint8 idle)
{
	JTAG_TAP_ProcessDataRW(tdo,
						   (uint8*)0,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);
}

void JTAG_TAP_DataPtr(uint8 *tdi, uint8 *tdo, uint16 bit_len, uint8 idle)
{
	JTAG_TAP_ProcessDataRW(tdo,
						   tdi,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);
}

uint32 JTAG_TAP_Data(uint32 tdi, uint16 bit_len, uint8 idle)
{
	uint32 tdo;

	JTAG_TAP_ProcessDataRW((uint8*)&tdo,
						   (uint8*)&tdi,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);

	return tdo;
}

void JTAG_TAP_DataOut(uint32 tdi, uint16 bit_len, uint8 idle)
{
	JTAG_TAP_ProcessDataRW((uint8*)0,
						   (uint8*)&tdi,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);
}

uint32 JTAG_TAP_DataIn(uint16 bit_len, uint8 idle)
{
	uint32 tdo;

	JTAG_TAP_ProcessDataRW((uint8*)&tdo,
						   (uint8*)0,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsBefore,
						   JTAG_TAP_UnitsAfter,
						   idle);

	return tdo;
}



void JTAG_TAP_SetDaisyChainPos(uint32 ub, uint32 ua, uint32 bb, uint32 ba)
{
	JTAG_TAP_UnitsBefore	= ub;
	JTAG_TAP_UnitsAfter		= ua;
	JTAG_TAP_BitsBefore		= bb;
	JTAG_TAP_BitsAfter		= ba;
}

static uint8 JTAG_TAP_HS_GetDivFromFreq(uint16 kHz)
{
	return SPI_GetSCKDiv(kHz);
}

void JTAG_TAP_SetTCKFreq(uint16 kHz)
{
	// Set Speed
	JTAG_kHz = kHz;
	if (JTAG_kHz >= JTAG_TAP_HS_MIN_KHZ)
	{
		JTAG_TAP_HS_SetSpeed(JTAG_TAP_HS_GetDivFromFreq(kHz));
	}
}

void JTAG_TAP_Init(uint16 kHz, uint8 mode)
{
	JTAG_TAP_Fini();

	if (kHz >= JTAG_TAP_HS_MIN_KHZ)
	{
		JTAG_TAP_HS_PortInit();
	}
	else
	{
		JTAG_TAP_TCK_SET();
		JTAG_TAP_TCK_SETOUTPUT();
		JTAG_TAP_TDO_SETINPUT();
		JTAG_TAP_TDI_SETOUTPUT();
		JTAG_TAP_TMS_SETOUTPUT();
		JTAG_TAP_RTCK_SETINPUT();
	}
	JTAG_TAP_SetTCKFreq(kHz);
	if (!kHz)
	{
		// Wait RTCK
		JTAG_TAP_RTCK_Wait(1);
	}

	if(mode == JTAG_TAP_ASYN)
	{
		JTAG_TAP_HS_Out(JTAG_TAP_TMS_2RTI, 0);
		if (kHz >= JTAG_TAP_HS_MIN_KHZ)
		{
			JTAG_TAP_Operate_Asyn = JTAG_TAP_HS_Operate_Asyn;
		}
		else
		{
			JTAG_TAP_Operate_Asyn = JTAG_TAP_GPIO_Operate_Asyn;
		}
	}
	else if(mode == JTAG_TAP_RAW)
	{
		if (kHz >= JTAG_TAP_HS_MIN_KHZ)
		{
			// DMA Init
			JTAG_TAP_HS_DMA_Init();

			JTAG_TAP_Operate_RAW = JTAG_TAP_HS_Operate_RAW_DMA;
		}
		else
		{
			JTAG_TAP_Operate_RAW = JTAG_TAP_GPIO_Operate_RAW;
		}
	}
	else
	{
		JTAG_TAP_Operate_RAW = JTAG_TAP_GPIO_Operate_RAW;
		JTAG_TAP_Operate_Asyn = JTAG_TAP_GPIO_Operate_Asyn;
	}
}

void JTAG_TAP_Fini(void)
{
	JTAG_TAP_HS_DMA_Fini();

	JTAG_kHz = 0xFFFF;
	JTAG_TAP_HS_PortFini();

	SPI_I2S_DeInit(JTAG_TAP_HS_SPI_M);
	SPI_I2S_DeInit(JTAG_TAP_HS_SPI_S);
}

#endif
