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

#include "JTAG_TAP.h"
#include "MEGA_JTAG.h"
#include "MEGA_JTAG_PRG.h"

/// Read JTAG ID
void AVR_JTAGPRG_ReadJTAGID(uint8 *id)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_IDCODE);
	JTAG_TAP_DataInPtr(id, AVR_JTAG_REG_JTAGID_Len, AVR_JTAG_RTI_CYCLE);
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
uint8 AVR_JTAGPRG_ReadOSCCALByte(uint8 addr)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterCaliByteRead();

	AVR_JTAG_PROG_LoadAddrByte(addr);
	return AVR_JTAG_PROG_ReadCaliByte();
}

/// Read Signature
void AVR_JTAGPRG_ReadSignature(uint8* sig)
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
uint8 AVR_JTAGPRG_ReadLockbits(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseLockbitRead();

	return AVR_JTAG_PROG_ReadLockbit();
}

/// Read Fuse low byte
uint8 AVR_JTAGPRG_ReadFuseLowByte(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseLockbitRead();

	return AVR_JTAG_PROG_ReadFuseLowByte();
}

/// Read Fuse high byte
uint8 AVR_JTAGPRG_ReadFuseHighByte(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseLockbitRead();

	return AVR_JTAG_PROG_ReadFuseHighByte();
}

/// Read Fuse extend byte
uint8 AVR_JTAGPRG_ReadFuseExtByte(void)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseLockbitRead();

	return AVR_JTAG_PROG_ReadExtFuseByte();
}

/// Read EEPROM page
void AVR_JTAGPRG_ReadEEPROMPage(uint8 *eeprom, uint32 addr, uint32 len)
{
	uint32 i;

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
void AVR_JTAGPRG_ReadFlashPage(uint8 *flash, uint32 addr, uint32 len)
{
	uint32 i, j, page_num = len / ddf->uiFlashPageSize;

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
			JTAG_TAP_Data(0, 8, AVR_JTAG_RTI_CYCLE);
			JTAG_TAP_DataInPtr(flash + j * ddf->uiFlashPageSize, ddf->uiFlashPageSize * 8, AVR_JTAG_RTI_CYCLE);
		}
		else
		{
			for(i = 0; i < len; i++)
			{
				flash[j * ddf->uiFlashPageSize + i] = JTAG_TAP_Data(0, 8, AVR_JTAG_RTI_CYCLE);
			}
		}
	}
}

/// Write lockbits
void AVR_JTAGPRG_WriteLockbits(uint8 lockbits)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterLockbitWrite();

	AVR_JTAG_PROG_LoadDataByte(lockbits);
	AVR_JTAG_PROG_WriteLockbit();
	while(!AVR_JTAG_PROG_WriteLockbitComplete());
}

/// Write Fuse low byte
void AVR_JTAGPRG_WriteFuseLowByte(uint8 fuselowbyte)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseWrite();

	AVR_JTAG_PROG_LoadDataLowByte(fuselowbyte);
	AVR_JTAG_PROG_WriteFuseLowByte();
	while(!AVR_JTAG_PROG_WriteFuseLowByteComplete());
}

/// Write Fuse high byte
void AVR_JTAGPRG_WriteFuseHighByte(uint8 fusehighbyte)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseWrite();

	AVR_JTAG_PROG_LoadDataLowByte(fusehighbyte);
	AVR_JTAG_PROG_WriteFuseHighByte();
	while(!AVR_JTAG_PROG_WriteFuseHighByteComplete());
}

/// Write Fuse extend byte
void AVR_JTAGPRG_WriteFuseExtByte(uint8 fuseextbyte)
{
	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);
	AVR_JTAG_PROG_EnterFuseWrite();

	AVR_JTAG_PROG_LoadDataLowByte(fuseextbyte);
	AVR_JTAG_PROG_WriteFuseExtByte();
	while(!AVR_JTAG_PROG_WriteFuseExtByteComplete());
}

/// Write EEPROM page
void AVR_JTAGPRG_WriteEEPROMPage(uint8 *eeprom, uint32 addr, uint32 len)
{
	uint32 i;

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
void AVR_JTAGPRG_WriteFlashPage(uint8 *flash, uint32 addr, uint32 len)
{
	uint32 i;

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
		JTAG_TAP_DataOutPtr(flash, len * 8, AVR_JTAG_RTI_CYCLE);
	}
	else
	{
		for(i = 0; i < len; i++)
		{
			JTAG_TAP_Data(flash[i], 8, AVR_JTAG_RTI_CYCLE);
		}
	}

	AVR_JTAG_SendIns(AVR_JTAG_INS_PROG_COMMANDS);

	AVR_JTAG_PROG_WriteFlashPage();
	while(!AVR_JTAG_PROG_WriteFlashPageComplete());
}
