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

void AVR_JTAGPRG_ReadJTAGID(uint8_t *id);
void AVR_JTAGPRG_EnterProgMode(void);
void AVR_JTAGPRG_LeaveProgMode(void);
void AVR_JTAGPRG_ChipErase(void);
uint8_t AVR_JTAGPRG_ReadOSCCALByte(uint8_t addr);
void AVR_JTAGPRG_ReadSignature(uint8_t *sig);
uint8_t AVR_JTAGPRG_ReadLockbits(void);
uint8_t AVR_JTAGPRG_ReadFuseLowByte(void);
uint8_t AVR_JTAGPRG_ReadFuseHighByte(void);
uint8_t AVR_JTAGPRG_ReadFuseExtByte(void);
void AVR_JTAGPRG_ReadEEPROMPage(uint8_t *eeprom, uint32_t addr, uint32_t len);
void AVR_JTAGPRG_ReadFlashPage(uint8_t *flash, uint32_t addr, uint32_t len);
void AVR_JTAGPRG_WriteLockbits(uint8_t lockbits);
void AVR_JTAGPRG_WriteFuseLowByte(uint8_t fuselowbyte);
void AVR_JTAGPRG_WriteFuseHighByte(uint8_t fusehighbyte);
void AVR_JTAGPRG_WriteFuseExtByte(uint8_t fuseextbyte);
void AVR_JTAGPRG_WriteEEPROMPage(uint8_t *eeprom, uint32_t addr, uint32_t len);
void AVR_JTAGPRG_WriteFlashPage(uint8_t *flash, uint32_t addr, uint32_t len);
