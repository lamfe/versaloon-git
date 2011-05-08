/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK500v2_MEGA_JTAG.h                                      *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation for STK500v2 protocol for AVR MEGA JTAG    *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/
#include "app_cfg.h"

#include "STK500v2_Const.h"
#include "STK500v2.h"

#include "STK_Param.h"

#include "interfaces.h"

#include "Target/MEGA_JTAG/MEGA_JTAG.h"
#include "Target/MEGA_JTAG/MEGA_JTAG_PRG.h"

bool STK500V2_OnWriteEntryPoint(const struct STKPARAM *param, uint8_t attr);
bool STK500V2_OnReadJTAGID(const struct STKPARAM *param, uint8_t attr);

static const struct STKPARAM STK500V2_MEGA_JTAG_Param[] = 
{
	STKPARAM_DECLEAR(PARAM_BOOT_ADDRESS, STKPARAM_ATTR_RW, 4, &STK500V2_NULL, 0),
	STKPARAM_DECLEAR(PARAM_PROGRAM_ENTRY_POINT, STKPARAM_ATTR_W, 4, &STK500V2_NULL, 0),
	STKPARAM_DECLEAR(PARAM_JTAGID, STKPARAM_ATTR_R, 4, &STK500V2_NULL, STK500V2_OnReadJTAGID),
	STKPARAM_NULL
};

bool STK500V2_OnReadJTAGID(const struct STKPARAM *param, uint8_t attr)
{
	if (STKPARAM_ATTR_R == attr)
	{
		AVR_JTAGPRG_ReadJTAGID((uint8_t*)&STK500V2_NULL);
	}
	return true;
}

bool STK500V2_OnWriteEntryPoint(const struct STKPARAM *param, uint8_t attr)
{
	if (STKPARAM_ATTR_W == attr)
	{
		// Enable PSB0 for ProgramEntryPoint Break
//		AVR_JTAGOCD_WriteOCD(AVR_JTAG_OCDREG_PSB0, STK500V2_NULL);
//		bcr = AVR_JTAGOCD_ReadOCD(AVR_JTAG_OCDREG_BCR);
//		AVR_JTAGOCD_WriteOCD(AVR_JTAG_OCDREG_BCR, AVR_JTAG_BCR_EN_PSB0);
	}
	return true;
}

/// JTAG Programming and Debugging for AVR under STK500v2 Protocol
/// @param[in]	dat		Command Array
/// @param[in]	len		Command Length
/// @return		Command Result
uint8_t STK500V2_MEGA_JTAG_ProcessCmd(uint8_t *dat, uint16_t len)
{
	uint32_t addr, length, data;
	uint8_t tmp8;

	switch(dat[0])
	{
	case CMND_SET_PARAMETER:
		length = 0;
		if (STKPARAM_GetSize(STK500V2_MEGA_JTAG_Param, dat[1], (uint8_t*)&length))
		{
			STKPARAM_SetValue(STK500V2_MEGA_JTAG_Param, dat[1], &dat[2]);
			STK500V2_RSP_OK();
		}
		break;
	case CMND_GET_PARAMETER:
		length = 0;
		if (STKPARAM_GetSize(STK500V2_MEGA_JTAG_Param, dat[1], (uint8_t*)&length))
		{
			STKPARAM_GetValue(STK500V2_MEGA_JTAG_Param, dat[1], &dat[1]);
			STK500V2_RSP_PARAMETER(length);
		}
		break;
	case CMND_WRITE_MEMORY:
		length = GET_LE_U32(&dat[2]);
		addr = GET_LE_U32(&dat[6]);

		switch(dat[1])
		{
		case MEMTYPE_SRAM:
//			AVR_JTAGOCD_WriteSRAM(dat+10, addr, length);
			break;
		case MEMTYPE_EEPROM:
//			AVR_JTAGOCD_WriteEEPROM(dat+10, addr, length);
			break;
		case MEMTYPE_EVENT + 1:
			break;
		case MEMTYPE_SPM:
//			AVR_JTAGOCD_WriteFlash(dat+10, addr, length);
			break;
		case MEMTYPE_FLASH_PAGE:
			AVR_JTAGPRG_WriteFlashPage(dat + 10, addr, length);
			break;
		case MEMTYPE_EEPROM_PAGE:
			AVR_JTAGPRG_WriteEEPROMPage(dat + 10, addr, length);
			break;
		case MEMTYPE_FUSE_BITS:
			for(tmp8 = 0; tmp8 < length; tmp8++)
			{
				if((addr + tmp8) == 0)
				{
					AVR_JTAGPRG_WriteFuseLowByte(dat[10 + tmp8]);
				}
				else if((addr + tmp8) == 1)
				{
					AVR_JTAGPRG_WriteFuseHighByte(dat[10 + tmp8]);
				}
				else if((addr + tmp8) == 2)
				{
					AVR_JTAGPRG_WriteFuseExtByte(dat[10 + tmp8]);
				}
				else
				{
					STK500V2_RSP_ILLEGAL_MEMORY_RANGE();
					return 0;
				}
			}
			break;
		case MEMTYPE_LOCK_BITS:
			AVR_JTAGPRG_WriteLockbits(dat[10]);
			break;
		default:
			STK500V2_RSP_ILLEGAL_MEMORY_TYPE();
			return 0;
		}
		STK500V2_RSP_OK();
		break;
	case CMND_READ_MEMORY:
		length = GET_LE_U32(&dat[2]);
		addr = GET_LE_U32(&dat[6]);

		switch(dat[1])
		{
		case MEMTYPE_SRAM:
//			AVR_JTAGOCD_ReadSRAM(dat+1, addr, length);
			break;
		case MEMTYPE_EEPROM:
//			AVR_JTAGOCD_ReadEEPROM(dat+1, addr, length);
			break;
		case MEMTYPE_SPM:
//			AVR_JTAGOCD_ReadFlash(dat+1, addr, length);
			break;
		case MEMTYPE_FLASH_PAGE:
			if(STK500V2_MCUState != MCUSTATE_PROGRAMMING)
			{
				STK500V2_RSP_ILLEGAL_MCU_STATE();
				return 0;
			}
			AVR_JTAGPRG_ReadFlashPage(dat + 1, addr, length);
			break;
		case MEMTYPE_EEPROM_PAGE:
			if(STK500V2_MCUState != MCUSTATE_PROGRAMMING)
			{
				STK500V2_RSP_ILLEGAL_MCU_STATE();
				return 0;
			}
			AVR_JTAGPRG_ReadEEPROMPage(dat + 1, addr, length);
			break;
		case MEMTYPE_FUSE_BITS:
			if(STK500V2_MCUState != MCUSTATE_PROGRAMMING)
			{
				STK500V2_RSP_ILLEGAL_MCU_STATE();
				return 0;
			}
			for(tmp8 = 0; tmp8 < length; tmp8++)
			{
				if((addr + tmp8) == 0)
				{
					dat[1 + tmp8] = AVR_JTAGPRG_ReadFuseLowByte();
				}
				else if((addr + tmp8) == 1)
				{
					dat[1 + tmp8] = AVR_JTAGPRG_ReadFuseHighByte();
				}
				else if((addr + tmp8) == 2)
				{
					dat[1 + tmp8] = AVR_JTAGPRG_ReadFuseExtByte();
				}
				else
				{
					STK500V2_RSP_ILLEGAL_MEMORY_RANGE();
					return 0;
				}
			}
			break;
		case MEMTYPE_LOCK_BITS:
			if(STK500V2_MCUState != MCUSTATE_PROGRAMMING)
			{
				STK500V2_RSP_ILLEGAL_MCU_STATE();
				return 0;
			}
			dat[1] = AVR_JTAGPRG_ReadLockbits();
			break;
		case MEMTYPE_SIGN_JTAG:
			if(STK500V2_MCUState != MCUSTATE_PROGRAMMING)
			{
				STK500V2_RSP_ILLEGAL_MCU_STATE();
				return 0;
			}
			AVR_JTAGPRG_ReadSignature((uint8_t*)&data);

			for(tmp8 = 0; tmp8 < length; tmp8++)
			{
				dat[1 + tmp8] = ((uint8_t*)&data)[tmp8 + addr];
			}
			break;
		case MEMTYPE_OSCCAL_BYTE:
			if(STK500V2_MCUState != MCUSTATE_PROGRAMMING)
			{
				STK500V2_RSP_ILLEGAL_MCU_STATE();
				return 0;
			}
			dat[1] = AVR_JTAGPRG_ReadOSCCALByte(addr);

			break;
		default:
			STK500V2_RSP_ILLEGAL_MEMORY_TYPE();
			return 0;
		}
		STK500V2_RSP_MEMORY(length);
		break;
	case CMND_WRITE_PC:
		addr = GET_LE_U16(&dat[1]);

		if(STK500V2_MCUState != MCUSTATE_STOPPED)
		{
			STK500V2_RSP_ILLEGAL_MCU_STATE();
			return 0;
		}

//		AVR_Context.PC = addr;

		STK500V2_RSP_OK();
		break;
	case CMND_READ_PC:
		if(STK500V2_MCUState != MCUSTATE_STOPPED)
		{
			STK500V2_RSP_ILLEGAL_MCU_STATE();
			return 0;
		}

//		SET_LE_U16(&dat[1], AVR_Context.PC);
		dat[3] = 0;
		dat[4] = 0;
		STK500V2_RSP_PC();
		break;
	case CMND_GO:
		if(STK500V2_MCUState & MCUSTATE_PROGRAMMING)
		{
			STK500V2_RSP_ILLEGAL_MCU_STATE();
			return 0;
		}

//		AVR_JTAGOCD_RestoreContext();
//		AVR_JTAG_SendIns(AVR_JTAG_INS_DBG_RUN);
		STK500V2_MCUState = MCUSTATE_RUNNING;

		STK500V2_RSP_OK();
		break;
	case CMND_SINGLE_STEP:
		if(STK500V2_MCUState & MCUSTATE_PROGRAMMING)
		{
			STK500V2_RSP_ILLEGAL_MCU_STATE();
			return 0;
		}

//		AVR_JTAGOCD_RestoreContext();
//		AVR_JTAGOCD_WriteOCD(AVR_JTAG_OCDREG_BCR, AVR_JTAG_BCR_BRK_STEP);

//		AVR_JTAG_SendIns(AVR_JTAG_INS_DBG_RUN);
		STK500V2_MCUState = MCUSTATE_RUNNING;

		STK500V2_RSP_OK();
		break;
	case CMND_FORCED_STOP:
		AVR_JTAG_SendIns(AVR_JTAG_INS_DBG_FORCE_BRK);

		STK500V2_RSP_OK();
		break;
	case CMND_RESET:
		AVR_JTAG_Reset(1);
//		AVR_JTAG_SendIns(AVR_JTAG_INS_DBG_FORCE_BRK);
		interfaces->delay.delayms(1);
		AVR_JTAG_Reset(0);
//		AVR_JTAGOCD_SaveContext();
//		STK500V2_MCUState = MCUSTATE_RUNNING;

		STK500V2_RSP_OK();
		break;
	case CMND_ERASEPAGE_SPM:
		break;
	case CMND_SELFTEST:
		STK500V2_RSP_SELFTEST();
		break;
	case CMND_SET_BREAK:
		addr = GET_LE_U16(&dat[3]);
		switch(dat[1])
		{
		case 1:
			// Program memory breakpoint
			switch(dat[2])
			{
			case 1:
//				AVR_JTAGOCD_SetPSB0(addr);
				break;
			case 2:
//				AVR_JTAGOCD_SetPSB1(addr);
				break;
			case 3:
//				AVR_JTAGOCD_SetPDMSB(addr,BM_PROGRAM);
				break;
			case 4:
//				AVR_JTAGOCD_SetPDSB(addr,BM_PROGRAM);
				break;
			default:
				STK500V2_RSP_ILLEGAL_BREAKPOINT();
				return 0;
			}
			break;
		case 2:
			// Data breakpoint
			switch(dat[2])
			{
			case 3:
//				AVR_JTAGOCD_SetPDMSB(addr,dat[7]);
				break;
			case 4:
//				AVR_JTAGOCD_SetPDSB(addr,dat[7]);
				break;
			default:
				STK500V2_RSP_ILLEGAL_BREAKPOINT();
				return 0;
			}
			break;
		case 3:
			// Mask for data breakpoint
//			AVR_JTAGOCD_SetPDMSB(addr,BM_MASK);
			break;
		default:
			STK500V2_RSP_ILLEGAL_BREAKPOINT();
			return 0;
		}

		STK500V2_RSP_OK();
		break;
	case CMND_GET_BREAK:
		break;
	case CMND_CHIP_ERASE:
		AVR_JTAGPRG_ChipErase();

		STK500V2_RSP_OK();
		break;
	case CMND_ENTER_PROGMODE:
		STK500V2_MCUState = MCUSTATE_PROGRAMMING;
		AVR_JTAGPRG_EnterProgMode();

		STK500V2_RSP_OK();
		break;
	case CMND_LEAVE_PROGMODE:
		AVR_JTAGPRG_LeaveProgMode();

		STK500V2_MCUState = MCUSTATE_RUNNING;

		STK500V2_RSP_OK();
		break;
	case CMND_CLR_BREAK:
		switch(dat[1])
		{
		case 1:
//			AVR_JTAGOCD_ClrPSB0();
			break;
		case 2:
//			AVR_JTAGOCD_ClrPSB1();
			break;
		case 3:
//			AVR_JTAGOCD_ClrPDMSB();
			break;
		case 4:
//			AVR_JTAGOCD_ClrPDSB();
			break;
		default:
			STK500V2_RSP_ILLEGAL_BREAKPOINT();
			return 0;
		}

		STK500V2_RSP_OK();
		break;
	case CMND_RUN_TO_ADDR:
		break;
	case CMND_SPI_CMD:
		break;
	case CMND_CLEAR_EVENTS:
		STK500V2_RSP_OK();
		break;
	case CMND_RESTORE_TARGET:
		AVR_JTAG_Reset(1);
		tmp8 = 0x7F;
		interfaces->jtag_hl.tms(0, &tmp8, 8);
		AVR_JTAG_Reset(0);

		STK500V2_RSP_OK();
		break;
	default:
		STK500V2_RSP_ILLEGAL_COMMAND();
		break;
	}

	return 0;
}
