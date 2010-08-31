/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK500v2.h                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    header for STK500v2 protocol                              *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/

extern enum STK500V2_MCUSTATE STK500V2_MCUState;
extern uint32 STK500V2_PARAM_DaisyChain;
extern uint32 STK500V2_NULL;

/// Device Descriptor Fields
typedef struct __tag_DeviceDescriptorFields
{
	uint8 ucReadIO[8];
	uint8 ucReadIOShadow[8];
	uint8 ucWriteIO[8];
	uint8 ucWriteIOShadow[8];
	uint8 ucReadExtIO[52];
	uint8 ucReadIOExtShadow[52];
	uint8 ucWriteExtIO[52];
	uint8 ucWriteIOExtShadow[52];
	uint8 ucIDRAddress;
	uint8 ucSPMCRAddress;
	uint8 ucRAMPZAddress;
	uint16 uiFlashPageSize;
	uint8 ucEepromPageSize;
	uint32 ulBootAddress;
	uint16 uiUpperExtIOLoc;
	uint32 ulFlashSize;
	uint8 ucEepromInst[20];
	uint8 ucFlashInst[3];
	uint8 ucSPHAddr;
	uint8 ucSPLAddr;
	uint16 uiFlashPages;
	uint8 ucDWDRAddress;
	uint8 ucDWBasePC;
	uint8 ucAllowFullPageBitstream;
	uint16 uiDtartSmallestBootloaderSection;
	uint8 ucEnablePageProgramming;
	uint8 ucCacheType;
	uint16 uiSramStartAddr;
	uint8 ucResetType;
	uint8 ucPCMaskExtended;
	uint8 ucPCMaskHigh;
	uint8 ucEindAddress;
	uint16 uiEECRAddr;
}DeviceDescriptorFields;
#define DeviceInfo						asyn_rx_buf
#define ddf								((DeviceDescriptorFields*)DeviceInfo)

void STK500V2_Poll(void);
void STK500V2_Process(uint8* dat, uint16 len);

uint8 STK500V2_MEGA_JTAG_ProcessCmd(uint8* dat, uint16 len);
uint8 STK500V2_AVR32_JTAG_ProcessCmd(uint8 *dat, uint16 len);
