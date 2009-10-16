/**************************************************************************
 *  Copyright (C) 2008 by Simon Qian                                      *
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

#define JTAG_TAP_DR_IR_PREPARE_DLY()	

static uint32 JTAG_TAP_UnitsBefore, JTAG_TAP_UnitsAfter, JTAG_TAP_BitsBefore, JTAG_TAP_BitsAfter;

/// JTAG Speed
int16 JTAG_Freq = 0xFFFF;

RAMFUNC void JTAG_TAP_HS_Operate_DMA(uint16 byte_len, uint8 *tdi, uint8 *tms, uint8 *tdo)
{
	uint16 i;

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

RAMFUNC uint16 JTAG_TAP_HS_Operate_Asyn(uint16 tdi, uint16 tms)
{
	uint16 tdo;

	JTAG_TAP_HS_WaitRxReady();
	tdo = JTAG_TAP_HS_In();

	JTAG_TAP_HS_Out(tms, tdi);

	return tdo;
}

RAMFUNC void JTAG_TAP_HS_OperateOut_Asyn(uint16 tdi, uint16 tms)
{
	JTAG_TAP_HS_Operate_Asyn(tdi, tms);
}


RAMFUNC void JTAG_TAP_HS_RW(uint8 *tdo, uint8 *tdi, uint8 tms_before, uint8 tms_after0, uint8 tms_after1, uint16 dat_byte_len)
{
	uint8 tdo_tmp;
	uint16 ret_len = 0, cur_pos = 0;

	if(dat_byte_len & 0x8000)
	{
		JTAG_TAP_HS_Operate_Asyn(tdi[cur_pos++], tms_before);
		ret_len++;
		dat_byte_len &= 0x7FFF;
		dat_byte_len--;
	}

	JTAG_TAP_DR_IR_PREPARE_DLY();
	dat_byte_len--;
	while(dat_byte_len-- > 0)
	{
		tdo_tmp = JTAG_TAP_HS_Operate_Asyn(tdi[cur_pos++], 0);
		if(ret_len > 0)
		{
			tdo[ret_len - 1] = tdo_tmp;
		}
		ret_len++;
	}
	tdo_tmp = JTAG_TAP_HS_Operate_Asyn(tdi[cur_pos], tms_after0);
	if(ret_len > 0)
	{
		tdo[ret_len - 1] = tdo_tmp;
	}
	tdo_tmp = JTAG_TAP_HS_Operate_Asyn(0, tms_after1);
	tdo[ret_len] = tdo_tmp;
}

RAMFUNC void JTAG_TAP_HS_R(uint8 *tdo, uint8 tms_before, uint8 tms_after0, uint8 tms_after1, uint16 dat_byte_len)
{
	uint8 tdo_tmp;
	uint16 ret_len = 0;

	if(dat_byte_len & 0x8000)
	{
		JTAG_TAP_HS_Operate_Asyn(0, tms_before);
		ret_len++;
		dat_byte_len &= 0x7FFF;
		dat_byte_len--;
	}

	JTAG_TAP_DR_IR_PREPARE_DLY();
	dat_byte_len--;
	while(dat_byte_len-- > 0)
	{
		tdo_tmp = JTAG_TAP_HS_Operate_Asyn(0, 0);
		if(ret_len > 0)
		{
			tdo[ret_len - 1] = tdo_tmp;
		}
		ret_len++;
	}
	tdo_tmp = JTAG_TAP_HS_Operate_Asyn(0, tms_after0);
	if(ret_len > 0)
	{
		tdo[ret_len - 1] = tdo_tmp;
	}
	tdo_tmp = JTAG_TAP_HS_Operate_Asyn(0, tms_after1);
	tdo[ret_len] = tdo_tmp;
}

RAMFUNC void JTAG_TAP_HS_W(uint8 *tdi, uint8 tms_before, uint8 tms_after0, uint8 tms_after1, uint16 dat_byte_len)
{
	uint16 cur_pos = 0;

	if(dat_byte_len & 0x8000)
	{
		JTAG_TAP_HS_OperateOut_Asyn(tdi[cur_pos++], tms_before);
		dat_byte_len &= 0x7FFF;
		dat_byte_len--;
	}

	JTAG_TAP_DR_IR_PREPARE_DLY();
	dat_byte_len--;
	while(dat_byte_len-- > 0)
	{
		JTAG_TAP_HS_OperateOut_Asyn(tdi[cur_pos++], 0);
	}
	JTAG_TAP_HS_OperateOut_Asyn(tdi[cur_pos], tms_after0);
	JTAG_TAP_HS_OperateOut_Asyn(0, tms_after1);
}












uint8 JTAG_TAP_1s[] = {0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, 0xFF};

RAMFUNC static void JTAG_TAP_ProcessDataRW(uint8 *tdo, uint8 *tdi, uint8 tms_before, uint8 len_before, uint16 bit_len, uint8 len_of_1s_before, uint8 len_of_1s_after, uint8 idle)
{
	uint8 tdi_tmp, tdo_tmp, tms_tmp, len_tmp;
	uint8 offset, Rec_offset;
	uint16 iSend = 0, iReceive = 0, iTmp = 0, bit_len_remain, receiveFromByte;

	bit_len_remain = bit_len;
	receiveFromByte = len_of_1s_after + len_before;
	Rec_offset = receiveFromByte & 0x07;
	receiveFromByte = (receiveFromByte + 7) >> 3;

	tms_tmp = tms_before;
	tdi_tmp = 0;
	len_tmp = len_before;
	offset = len_before;

	while(len_of_1s_before + bit_len + len_of_1s_after > 0)
	{
		if(len_of_1s_before > 0)
		{
			if(len_tmp > 0)
			{
				if(len_of_1s_before >= 8 - len_tmp)
				{
					tdi_tmp |= JTAG_TAP_1s[8 - len_tmp] << offset;
				}
				else
				{
					tdi_tmp |= JTAG_TAP_1s[len_of_1s_before] << offset;
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
		tdo_tmp = JTAG_TAP_HS_Operate_Asyn(tdi_tmp, tms_tmp);
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
		tdo_tmp = JTAG_TAP_HS_Operate_Asyn(0, JTAG_TAP_HS_TMS_E12UPDATE >> idle);
	}
	else
	{
		tdo_tmp = JTAG_TAP_HS_Operate_Asyn(0, JTAG_TAP_HS_TMS_E12UPDATE << (8 - idle));
		JTAG_TAP_HS_Operate_Asyn(0, JTAG_TAP_HS_TMS_E12UPDATE >> idle);
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
		JTAG_TAP_HS_Operate_Asyn(0, 0);
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
						   JTAG_TAP_UnitsAfter,
						   JTAG_TAP_UnitsBefore,
						   idle);
}

void JTAG_TAP_DataInPtr(uint8 *tdo, uint16 bit_len, uint8 idle)
{
	JTAG_TAP_ProcessDataRW(tdo,
						   (uint8*)0,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsAfter,
						   JTAG_TAP_UnitsBefore,
						   idle);
}

void JTAG_TAP_DataPtr(uint8 *tdi, uint8 *tdo, uint16 bit_len, uint8 idle)
{
	JTAG_TAP_ProcessDataRW(tdo,
						   tdi,
						   JTAG_TAP_TMS_UPDATERTI2SD,
						   JTAG_TAP_TMS_UPDATERTI2SD_LEN,
						   bit_len,
						   JTAG_TAP_UnitsAfter,
						   JTAG_TAP_UnitsBefore,
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
						   JTAG_TAP_UnitsAfter,
						   JTAG_TAP_UnitsBefore,
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
						   JTAG_TAP_UnitsAfter,
						   JTAG_TAP_UnitsBefore,
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
						   JTAG_TAP_UnitsAfter,
						   JTAG_TAP_UnitsBefore,
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

static uint8 JTAG_TAP_HS_GetDivFromFreq(uint16 freq)
{
	if(freq >= _SYS_FREQUENCY * 500 / 2)
	{
		freq = 0;
	}
	else if(freq >= _SYS_FREQUENCY * 500 / 4)
	{
		freq = 1;
	}
	else if(freq >= _SYS_FREQUENCY * 500 / 8)
	{
		freq = 2;
	}
	else if(freq >= _SYS_FREQUENCY * 500 / 16)
	{
		freq = 3;
	}
	else if(freq >= _SYS_FREQUENCY * 500 / 32)
	{
		freq = 4;
	}
	else if(freq > _SYS_FREQUENCY * 500 / 64)
	{
		freq = 5;
	}
	else if(freq > _SYS_FREQUENCY * 500 / 128)
	{
		freq = 6;
	}
	else
	{
		freq = 7;
	}

	return (uint8)(freq << 3);
}

// freq is in KHz
void JTAG_TAP_HS_SetTCKFreq(uint16 freq)
{
	if(freq == 0)
	{
		// Detect Speed
		JTAG_Freq = 0xFFFF;
	}
	else
	{
		// Set Speed
		JTAG_Freq = freq;
		JTAG_TAP_HS_SetSpeed(JTAG_TAP_HS_GetDivFromFreq(freq));
	}
}

static uint8 __jtag_inited = 0;
void JTAG_TAP_HS_Init(uint16 freq, uint8 mode)
{
	if(__jtag_inited)
		return;
	__jtag_inited = 1;

	JTAG_TAP_HS_PortInit();

	SPI_Configuration(JTAG_TAP_HS_SPI_M,SPI_Mode_Master,JTAG_TAP_HS_GetDivFromFreq(freq),SPI_FirstBit_LSB,SPI_CPOL_High,SPI_CPHA_2Edge);
	SPI_Configuration(JTAG_TAP_HS_SPI_S,SPI_Mode_Slave,SPI_BaudRatePrescaler_2,SPI_FirstBit_LSB,SPI_CPOL_High,SPI_CPHA_2Edge);

	if(mode == JTAG_TAP_ASYN)
	{
		JTAG_TAP_HS_Out(JTAG_TAP_HS_TMS_2RTI, 0);
	}
	else if(mode == JTAG_TAP_DMA)
	{
		// DMA Init
		JTAG_DMA_Init();
	}
}

void JTAG_TAP_HS_Fini(void)
{
	__jtag_inited = 0;

	JTAG_DMA_Fini();

	JTAG_Freq = 0xFFFF;
	JTAG_TAP_HS_PortFini();

	SPI_I2S_DeInit(JTAG_TAP_HS_SPI_M);
	SPI_I2S_DeInit(JTAG_TAP_HS_SPI_S);
}

#endif
