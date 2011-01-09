/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       MEGA_JTAG.h                                               *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header for AVR_MEGA_JTAG support                          *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

// Instructions:
#define AVR_JTAG_INS_Len						4
// Public Instructions:
#define AVR_JTAG_INS_EXTEST						0x00
#define AVR_JTAG_INS_IDCODE						0x01
#define AVR_JTAG_INS_SAMPLE_PRELOAD				0x02
#define AVR_JTAG_INS_BYPASS						0x0F
// AVR Specified Public Instructions:
#define AVR_JTAG_INS_AVR_RESET					0x0C
#define AVR_JTAG_INS_PROG_ENABLE				0x04
#define AVR_JTAG_INS_PROG_COMMANDS				0x05
#define AVR_JTAG_INS_PROG_PAGELOAD				0x06
#define AVR_JTAG_INS_PROG_PAGEREAD				0x07
// AVR specified Private Instructions:
#define AVR_JTAG_INS_DBG_FORCE_BRK				0x08
#define AVR_JTAG_INS_DBG_RUN					0x09
// IR 4	,0x0A
// DR 16,instr_1
// ......
// DR 16,instr_n
#define AVR_JTAG_INS_DBG_INSTR					0x0A
// Read/Write internal Addres Space
// IR 4	,0x0B
// DR 5	,0 + ins_addr	// if write, omit
// DR 16,addr
// DR 5,RW + ins_addr
// 
// Read OCDR
// IR 4	,0x0B
// DR 5	,0x0C
// DR 16,0
#define AVR_JTAG_INS_DBG_OCD					0x0B

// AVR Debug Registers
#define AVR_JTAG_OCDREG_PSB0					0
#define AVR_JTAG_OCDREG_PSB1					1
#define AVR_JTAG_OCDREG_PDMSB					2
#define AVR_JTAG_OCDREG_PDSB					3
#define AVR_JTAG_OCDREG_BCR						8  /* break control register  */
#define AVR_JTAG_OCDREG_BSR						9  /* break control and status register */
#define AVR_JTAG_OCDREG_COMM_DATA				12 /* communication data OCDR register */
#define AVR_JTAG_OCDREG_COMM_CTL				13 /* control and status of OCDR register */

// BCR bits
#define AVR_JTAG_BCR_PDSB_MODE0					0x0008
#define AVR_JTAG_BCR_PDSB_MODE1					0x0010
#define AVR_JTAG_BCR_PDMSB_MODE0				0x0020
#define AVR_JTAG_BCR_PDMSB_MODE1				0x0040
#define AVR_JTAG_BCR_EN_PDSB					0x0080
#define AVR_JTAG_BCR_EN_PDMSB					0x0100
#define AVR_JTAG_BCR_MASK_BREAK					0x0200
#define AVR_JTAG_BCR_EN_PSB1					0X0400
#define AVR_JTAG_BCR_EN_PSB0					0X0800
#define AVR_JTAG_BCR_BRK_ON_FLOW				0x1000
#define AVR_JTAG_BCR_BRK_STEP					0x2000
#define AVR_JTAG_BCR_PC_MOD						0X4000
#define AVR_JTAG_BCR_TMR_RUN_ON_BRK				0x8000

// SPMCR bits
#define AVR_JTAG_SPMCR_SPMEN					0x01
#define AVR_JTAG_SPMCR_PGERS					0x02
#define AVR_JTAG_SPMCR_PGWRT					0x04
#define AVR_JTAG_SPMCR_BLBSET					0x08
#define AVR_JTAG_SPMCR_RWWSRE					0x10
#define AVR_JTAG_SPMCR_RWWSB					0x40

// BSR bits
#define AVR_JTAG_BSR_BRK_INSTR					0x01
#define AVR_JTAG_BSR_FORCED						0x02
#define AVR_JTAG_BSR_STAT						0x04
#define AVR_JTAG_BSR_PDSB						0x08
#define AVR_JTAG_BSR_PDMSB						0x10
#define AVR_JTAG_BSR_PSB1						0x20
#define AVR_JTAG_BSR_PSB2						0x40
#define AVR_JTAG_BSR_FLOW						0x80

// Control and status bits
#define AVR_JTAG_CTL_SET_OCDR					0x0004
#define AVR_JTAG_CTL_RES_1						0x0008
#define AVR_JTAG_CTL_EN_OCDR					0x8000

#define AVR_JTAG_WR_OCDR						0x10

// SPMCR bits
#define AVR_SPMCR_SPMEN							0x01
#define AVR_SPMCR_PGERS							0x02
#define AVR_SPMCR_PGWRT							0x04
#define AVR_SPMCR_BLBSET						0x08
#define AVR_SPMCR_RWWSRE						0x10
#define AVR_SPMCR_RWWSB							0x40



// Data Registers:
#define AVR_JTAG_REG_Bypass_Len					1
#define AVR_JTAG_REG_DeviceID_Len				32

#define AVR_JTAG_REG_Reset_Len					1
#define AVR_JTAG_REG_JTAGID_Len					32
#define AVR_JTAG_REG_ProgrammingEnable_Len		16
#define AVR_JTAG_REG_ProgrammingCommand_Len		15
#define AVR_JTAG_REG_FlashDataByte_Len			16



#define AVR_JTAG_RTI_CYCLE						1

#define AVR_JTAG_Reset(r)						do{AVR_JTAG_SendIns(AVR_JTAG_INS_AVR_RESET);AVR_JTAG_SendDat((r),AVR_JTAG_REG_Reset_Len);}while(0)


// JTAG Programming Instructions:
#define AVR_JTAG_PROG_OPERATIONCOMPLETE			0x0200
#define AVR_JTAG_PROG_INS(d)					AVR_JTAG_SendDat((d), AVR_JTAG_REG_ProgrammingCommand_Len)
#define AVR_JTAG_PROG_LoadAddrExtendedHighByte(c)	AVR_JTAG_PROG_INS(0xB00 | ((c) & 0xFF))
#define AVR_JTAG_PROG_LoadAddrHighByte(a)		AVR_JTAG_PROG_INS(0x0700 | ((a) & 0xFF))
#define AVR_JTAG_PROG_LoadAddrLowByte(b)		AVR_JTAG_PROG_INS(0x0300 | ((b) & 0xFF))
#define AVR_JTAG_PROG_LoadAddrByte(b)			AVR_JTAG_PROG_LoadAddrLowByte(b)
#define AVR_JTAG_PROG_LoadDataLowByte(i)		AVR_JTAG_PROG_INS(0x1300 | ((i) & 0xFF))
#define AVR_JTAG_PROG_LoadDataHighByte(i)		AVR_JTAG_PROG_INS(0x1700 | ((i) & 0xFF))
#define AVR_JTAG_PROG_LoadDataByte(i)			AVR_JTAG_PROG_LoadDataLowByte(i)
#define AVR_JTAG_PROG_LatchData()				(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x7700), AVR_JTAG_PROG_INS(0x3700))
// Chip Erase
#define AVR_JTAG_PROG_ChipErase()				(AVR_JTAG_PROG_INS(0x2380), AVR_JTAG_PROG_INS(0x3180), AVR_JTAG_PROG_INS(0x3380), AVR_JTAG_PROG_INS(0x3380))
#define AVR_JTAG_PROG_ChipEraseComplete()		(AVR_JTAG_PROG_INS(0x3380) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Write Flash
#define AVR_JTAG_PROG_EnterFlashWrite()			AVR_JTAG_PROG_INS(0x2310)
#define AVR_JTAG_PROG_WriteFlashPage()			(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3500), AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3700))
#define AVR_JTAG_PROG_WriteFlashPageComplete()	(AVR_JTAG_PROG_INS(0x3700) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Read Flash
#define AVR_JTAG_PROG_EnterFlashRead()			AVR_JTAG_PROG_INS(0x2302)

// Write EEPROM
#define AVR_JTAG_PROG_EnterEEPROMWrite()		AVR_JTAG_PROG_INS(0x2311)
#define AVR_JTAG_PROG_WriteEEPROMPage()			(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteEEPROMPageComplete()	(AVR_JTAG_PROG_INS(0x3300) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Read EEPROM
#define AVR_JTAG_PROG_EnterEEPROMRead()			AVR_JTAG_PROG_INS(0x2303)

// Write Fuses
#define AVR_JTAG_PROG_EnterFuseWrite()			AVR_JTAG_PROG_INS(0x2340)
#define AVR_JTAG_PROG_WriteFuseExtByte()		(AVR_JTAG_PROG_INS(0x3B00), AVR_JTAG_PROG_INS(0x3900), AVR_JTAG_PROG_INS(0x3B00), AVR_JTAG_PROG_INS(0x3B00))
#define AVR_JTAG_PROG_WriteFuseExtByteComplete()	(AVR_JTAG_PROG_INS(0x3700) & AVR_JTAG_PROG_OPERATIONCOMPLETE)
#define AVR_JTAG_PROG_WriteFuseHighByte()		(AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3500), AVR_JTAG_PROG_INS(0x3700), AVR_JTAG_PROG_INS(0x3700))
#define AVR_JTAG_PROG_WriteFuseHighByteComplete()	(AVR_JTAG_PROG_INS(0x3700) & AVR_JTAG_PROG_OPERATIONCOMPLETE)
#define AVR_JTAG_PROG_WriteFuseLowByte()		(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteFuseLowByteComplete()	(AVR_JTAG_PROG_INS(0x3300) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Write Lockbits
#define AVR_JTAG_PROG_EnterLockbitWrite()		AVR_JTAG_PROG_INS(0x2320)
#define AVR_JTAG_PROG_WriteLockbit()			(AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3100), AVR_JTAG_PROG_INS(0x3300), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_WriteLockbitComplete()	(AVR_JTAG_PROG_INS(0x3300) & AVR_JTAG_PROG_OPERATIONCOMPLETE)

// Read Fuses/Lockbits
#define AVR_JTAG_PROG_EnterFuseLockbitRead()	AVR_JTAG_PROG_INS(0x2304)
#define AVR_JTAG_PROG_ReadExtFuseByte()			(AVR_JTAG_PROG_INS(0x3A00), AVR_JTAG_PROG_INS(0x3B00))
#define AVR_JTAG_PROG_ReadFuseHighByte()		(AVR_JTAG_PROG_INS(0x3E00), AVR_JTAG_PROG_INS(0x3F00))
#define AVR_JTAG_PROG_ReadFuseLowByte()			(AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_INS(0x3300))
#define AVR_JTAG_PROG_ReadLockbit()				(AVR_JTAG_PROG_INS(0x3600), AVR_JTAG_PROG_INS(0x3700))

// Read Signature
#define AVR_JTAG_PROG_EnterSignByteRead()		AVR_JTAG_PROG_INS(0x2308)
#define AVR_JTAG_PROG_ReadSignByte()			(AVR_JTAG_PROG_INS(0x3200), AVR_JTAG_PROG_INS(0x3300))

// Read Calibration Byte
#define AVR_JTAG_PROG_EnterCaliByteRead()		AVR_JTAG_PROG_INS(0x2308)
#define AVR_JTAG_PROG_ReadCaliByte()			(AVR_JTAG_PROG_INS(0x3600), AVR_JTAG_PROG_INS(0x3700))

// No Operation Command
#define AVR_JTAG_PROG_LoadNoOperationCommand()	(AVR_JTAG_PROG_INS(0x2300), AVR_JTAG_PROG_INS(0x3300))

uint32_t AVR_JTAG_SendIns(uint32_t ins);
uint32_t AVR_JTAG_SendDat(uint32_t dat, uint16_t bitlen);

void AVR_JTAG_Init(void);
