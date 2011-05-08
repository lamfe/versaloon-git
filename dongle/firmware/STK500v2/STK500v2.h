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
extern uint32_t STK500V2_PARAM_DaisyChain;
extern uint32_t STK500V2_NULL;

/// Device Descriptor Fields
typedef struct __tag_DeviceDescriptorFields
{
	uint8_t ucReadIO[8];
	uint8_t ucReadIOShadow[8];
	uint8_t ucWriteIO[8];
	uint8_t ucWriteIOShadow[8];
	uint8_t ucReadExtIO[52];
	uint8_t ucReadIOExtShadow[52];
	uint8_t ucWriteExtIO[52];
	uint8_t ucWriteIOExtShadow[52];
	uint8_t ucIDRAddress;
	uint8_t ucSPMCRAddress;
	uint8_t ucRAMPZAddress;
	uint16_t uiFlashPageSize;
	uint8_t ucEepromPageSize;
	uint32_t ulBootAddress;
	uint16_t uiUpperExtIOLoc;
	uint32_t ulFlashSize;
	uint8_t ucEepromInst[20];
	uint8_t ucFlashInst[3];
	uint8_t ucSPHAddr;
	uint8_t ucSPLAddr;
	uint16_t uiFlashPages;
	uint8_t ucDWDRAddress;
	uint8_t ucDWBasePC;
	uint8_t ucAllowFullPageBitstream;
	uint16_t uiDtartSmallestBootloaderSection;
	uint8_t ucEnablePageProgramming;
	uint8_t ucCacheType;
	uint16_t uiSramStartAddr;
	uint8_t ucResetType;
	uint8_t ucPCMaskExtended;
	uint8_t ucPCMaskHigh;
	uint8_t ucEindAddress;
	uint16_t uiEECRAddr;
}DeviceDescriptorFields;
#define DeviceInfo						asyn_rx_buf
#define ddf								((DeviceDescriptorFields*)DeviceInfo)

void STK500V2_Poll(void);
void STK500V2_Process(uint8_t* dat, uint16_t len);

uint8_t STK500V2_MEGA_JTAG_ProcessCmd(uint8_t* dat, uint16_t len);
uint8_t STK500V2_AVR32_JTAG_ProcessCmd(uint8_t *dat, uint16_t len);
