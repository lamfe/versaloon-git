/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       MEGA_JTAG_PRG.c                                           *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation for AVR MEGA programming thru JTAG         *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2008-11-07:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "STK500v2.h"

#include "interfaces.h"
#include "MEGA_JTAG.h"
#include "MEGA_JTAG_PRG.h"

uint32_t AVR_JTAG_SendIns(uint32_t ins)
{
	interfaces->jtag_hl.ir(0, (uint8_t *)&ins, AVR_JTAG_INS_Len, AVR_JTAG_RTI_CYCLE, 1);
	return ins;
}
uint32_t AVR_JTAG_SendDat(uint32_t dat, uint16_t bitlen)
{
	interfaces->jtag_hl.dr(0, (uint8_t *)&dat, bitlen, AVR_JTAG_RTI_CYCLE, 1);
	return dat;
}

/// Read JTAG ID
void AVR_JTAGPRG_ReadJTAGID(uint8_t *id)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_IDCODE);
	interfaces->jtag_hl.dr(0, id, AVR_JTAG_REG_JTAGID_Len, AVR_JTAG_RTI_CYCLE, 1);
}

/// Enter programming mode
void AVR_JTAGPRG_EnterProgMode(void)
{
	AVR_JTAG_Reset(1);

	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_ENABLE);
	AVR_JTAG_SendDat(0xA370, AVR_JTAG_REG_ProgrammingEnable_Len);
}

/// Leave programming mode
void AVR_JTAGPRG_LeaveProgMode(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_LoadNoOperationCommand();

	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_ENABLE);
	AVR_JTAG_SendDat(0, AVR_JTAG_REG_ProgrammingEnable_Len);

	AVR_JTAG_Reset(0);
}

/// Chip erase
void AVR_JTAGPRG_ChipErase(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_ChipErase();
	while(!AVR_JTAG_PROG_ChipEraseComplete());
}

// Read OSCCAL byte
uint8_t AVR_JTAGPRG_ReadOSCCALByte(uint8_t addr)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterCaliByteRead();

	AVR_JTAG_PROG_LoadAddrByte(addr);
	return AVR_JTAG_PROG_ReadCaliByte();
}

/// Read Signature
void AVR_JTAGPRG_ReadSignature(uint8_t *sig)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterSignByteRead();

	AVR_JTAG_PROG_LoadAddrByte(0);
	sig[0] = AVR_JTAG_PROG_ReadSignByte();
	AVR_JTAG_PROG_LoadAddrByte(1);
	sig[1] = AVR_JTAG_PROG_ReadSignByte();
	AVR_JTAG_PROG_LoadAddrByte(2);
	sig[2] = AVR_JTAG_PROG_ReadSignByte();
}

/// Read Lockbits
uint8_t AVR_JTAGPRG_ReadLockbits(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseLockbitRead();

	return AVR_JTAG_PROG_ReadLockbit();
}

/// Read Fuse low byte
uint8_t AVR_JTAGPRG_ReadFuseLowByte(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseLockbitRead();

	return AVR_JTAG_PROG_ReadFuseLowByte();
}

/// Read Fuse high byte
uint8_t AVR_JTAGPRG_ReadFuseHighByte(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseLockbitRead();

	return AVR_JTAG_PROG_ReadFuseHighByte();
}

/// Read Fuse extend byte
uint8_t AVR_JTAGPRG_ReadFuseExtByte(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseLockbitRead();

	return AVR_JTAG_PROG_ReadExtFuseByte();
}

/// Read EEPROM page
void AVR_JTAGPRG_ReadEEPROMPage(uint8_t *eeprom, uint32_t addr, uint32_t len)
{
	uint32_t i;

	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterEEPROMRead();

	for(i = 0; i < len; i++)
	{
		AVR_JTAG_PROG_LoadAddrHighByte(addr >> 8);
		AVR_JTAG_PROG_LoadAddrLowByte(addr + i);
		AVR_JTAG_PROG_INS(0x3300 | (addr + i));
		AVR_JTAG_PROG_INS(0x3200);
		eeprom[i] = AVR_JTAG_PROG_INS(0x3300) & 0xFF;
	}
}

/// Read Flash page
void AVR_JTAGPRG_ReadFlashPage(uint8_t *flash, uint32_t addr, uint32_t len)
{
	uint32_t i, j, page_num = len / ddf->uiFlashPageSize;

	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFlashRead();
	if(addr >> 24)
	{
		AVR_JTAG_PROG_LoadAddrExtendedHighByte(1);
	}

	AVR_JTAG_PROG_LoadAddrHighByte(addr >> 9);

	for(j = 0; j < page_num; j++)
	{
		AVR_JTAG_PROG_LoadAddrLowByte((addr >> 1) + ((j * ddf->uiFlashPageSize) >> 1));
		AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_PAGEREAD);

		if(ddf->ucAllowFullPageBitstream)
		{
			uint8_t tmp0 = 0;
			interfaces->jtag_hl.dr(0, &tmp0, 8, AVR_JTAG_RTI_CYCLE, 0);
			interfaces->jtag_hl.dr(0, &flash[j * ddf->uiFlashPageSize], ddf->uiFlashPageSize * 8, AVR_JTAG_RTI_CYCLE, 1);
		}
		else
		{
			for(i = 0; i < len; i++)
			{
				interfaces->jtag_hl.dr(0, &flash[j * ddf->uiFlashPageSize + i], 8, AVR_JTAG_RTI_CYCLE, 1);
			}
		}
	}
}

/// Write lockbits
void AVR_JTAGPRG_WriteLockbits(uint8_t lockbits)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterLockbitWrite();

	AVR_JTAG_PROG_LoadDataByte(lockbits);
	AVR_JTAG_PROG_WriteLockbit();
	while(!AVR_JTAG_PROG_WriteLockbitComplete());
}

/// Write Fuse low byte
void AVR_JTAGPRG_WriteFuseLowByte(uint8_t fuselowbyte)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseWrite();

	AVR_JTAG_PROG_LoadDataLowByte(fuselowbyte);
	AVR_JTAG_PROG_WriteFuseLowByte();
	while(!AVR_JTAG_PROG_WriteFuseLowByteComplete());
}

/// Write Fuse high byte
void AVR_JTAGPRG_WriteFuseHighByte(uint8_t fusehighbyte)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseWrite();

	AVR_JTAG_PROG_LoadDataLowByte(fusehighbyte);
	AVR_JTAG_PROG_WriteFuseHighByte();
	while(!AVR_JTAG_PROG_WriteFuseHighByteComplete());
}

/// Write Fuse extend byte
void AVR_JTAGPRG_WriteFuseExtByte(uint8_t fuseextbyte)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseWrite();

	AVR_JTAG_PROG_LoadDataLowByte(fuseextbyte);
	AVR_JTAG_PROG_WriteFuseExtByte();
	while(!AVR_JTAG_PROG_WriteFuseExtByteComplete());
}

/// Write EEPROM page
void AVR_JTAGPRG_WriteEEPROMPage(uint8_t *eeprom, uint32_t addr, uint32_t len)
{
	uint32_t i;

	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterEEPROMWrite();

	AVR_JTAG_PROG_LoadAddrHighByte(addr >> 8);
	for(i = 0; i < len; i++)
	{
		AVR_JTAG_PROG_LoadAddrLowByte(addr + i);
		AVR_JTAG_PROG_LoadDataByte(eeprom[i]);
		AVR_JTAG_PROG_LatchData();
	}

	AVR_JTAG_PROG_WriteEEPROMPage();
	while(!AVR_JTAG_PROG_WriteEEPROMPageComplete());
}

/// Write Flash page
void AVR_JTAGPRG_WriteFlashPage(uint8_t *flash, uint32_t addr, uint32_t len)
{
	uint32_t i;

	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFlashWrite();
	if(addr >> 24)
	{
		AVR_JTAG_PROG_LoadAddrExtendedHighByte(1);
	}
	AVR_JTAG_PROG_LoadAddrHighByte(addr >> 9);

	AVR_JTAG_PROG_LoadAddrLowByte(addr >> 1);
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_PAGELOAD);

	if(ddf->ucAllowFullPageBitstream)
	{
		interfaces->jtag_hl.dr(0, flash, len * 8, AVR_JTAG_RTI_CYCLE, 0);
	}
	else
	{
		for(i = 0; i < len; i++)
		{
			interfaces->jtag_hl.dr(0, &flash[i], 8, AVR_JTAG_RTI_CYCLE, 0);
		}
	}

	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);

	AVR_JTAG_PROG_WriteFlashPage();
	while(!AVR_JTAG_PROG_WriteFlashPageComplete());
}
