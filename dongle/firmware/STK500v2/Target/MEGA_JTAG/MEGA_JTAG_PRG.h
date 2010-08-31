/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       MEGA_JTAG_PRG.h                                           *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header for AVR MEGA programming thru JTAG                 *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-30:     created(by SimonQian)                             *
 **************************************************************************/

void AVR_JTAGPRG_ReadJTAGID(uint8 *id);
void AVR_JTAGPRG_EnterProgMode(void);
void AVR_JTAGPRG_LeaveProgMode(void);
void AVR_JTAGPRG_ChipErase(void);
uint8 AVR_JTAGPRG_ReadOSCCALByte(uint8 addr);
void AVR_JTAGPRG_ReadSignature(uint8* sig);
uint8 AVR_JTAGPRG_ReadLockbits(void);
uint8 AVR_JTAGPRG_ReadFuseLowByte(void);
uint8 AVR_JTAGPRG_ReadFuseHighByte(void);
uint8 AVR_JTAGPRG_ReadFuseExtByte(void);
void AVR_JTAGPRG_ReadEEPROMPage(uint8 *eeprom, uint32 addr, uint32 len);
void AVR_JTAGPRG_ReadFlashPage(uint8 *flash, uint32 addr, uint32 len);
void AVR_JTAGPRG_WriteLockbits(uint8 lockbits);
void AVR_JTAGPRG_WriteFuseLowByte(uint8 fuselowbyte);
void AVR_JTAGPRG_WriteFuseHighByte(uint8 fusehighbyte);
void AVR_JTAGPRG_WriteFuseExtByte(uint8 fuseextbyte);
void AVR_JTAGPRG_WriteEEPROMPage(uint8 *eeprom, uint32 addr, uint32 len);
void AVR_JTAGPRG_WriteFlashPage(uint8 *flash, uint32 addr, uint32 len);
