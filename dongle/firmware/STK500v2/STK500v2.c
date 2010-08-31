/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK500v2.c                                                *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    implementation for STK500v2 protocol                      *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

#include "STK500v2_Const.h"
#include "STK500v2.h"
#include "STK500.h"

#include "STK_Param.h"

#include "JTAG_TAP.h"

#if POWER_OUT_EN
#	include "PowerExt.h"
#endif

static uint8 STK500V2_DeviceName[] = STK500V2_Device_Name " on Versaloon";

enum STK500V2_MCUSTATE STK500V2_MCUState = MCUSTATE_STOPPED;
enum STK500V2_EMUMODE STK500V2_EmuMode = EMUMODE_NONE;
uint32 STK500V2_PARAM_DaisyChain = 0;
uint32 STK500V2_NULL = 0;
uint8 STK500V2_PARAM_RunAfterProgramming = 0;

#define STK500V2_MakeVersion(MMajor, MMinor, SMajor, SMinor)	\
								(((uint32)(MMajor) << 24) \
									| ((uint32)(MMinor) << 16) \
									| ((uint32)(SMajor) << 8) \
									| (SMinor))
#define STK500V2_FW_VER			STK500V2_MakeVersion(STK500V2_MFW_MAJOR_VER, \
														STK500V2_MFW_MINOR_VER, \
														STK500V2_SFW_MAJOR_VER, \
														STK500V2_SFW_MINOR_VER)
uint8 STK500V2_comm_id			= STK500V2_COMM_ID;
uint8 STK500V2_m_mcu_bldr		= STK500V2_MBootload_VER;
uint8 STK500V2_s_mcu_bldr		= STK500V2_SBootload_VER;
uint8 STK500V2_serial_number[6]	= STK500V2_SerialNumber;
uint16 STK500V2_hw_version 		= STK500V2_HW_VER;
uint32 STK500V2_fw_version		= STK500V2_FW_VER;

bool STK500V2_OnWriteEmuMode(const struct STKPARAM *param, uint8 attr);
bool STK500V2_OnWriteDaisyChain(const struct STKPARAM *param, uint8 attr);

static const struct STKPARAM STK500V2_Param[] = 
{
	STKPARAM_DECLEAR(PARAM_HW_VERSION, STKPARAM_ATTR_R, 2, &STK500V2_hw_version, 0),
	STKPARAM_DECLEAR(PARAM_FW_VERSION, STKPARAM_ATTR_R, 4, &STK500V2_fw_version, 0),
	STKPARAM_DECLEAR(PARAM_EMULATOR_MODE, STKPARAM_ATTR_RW, 1, &STK500V2_EmuMode, STK500V2_OnWriteEmuMode),
	STKPARAM_DECLEAR(PARAM_OCD_VTARGET, STKPARAM_ATTR_R, 2, &Vtarget, 0),
	STKPARAM_DECLEAR(PARAM_OCD_JTAG_CLOCK, STKPARAM_ATTR_RW, 1, &STK500V2_NULL, 0),
	STKPARAM_DECLEAR(PARAM_DAISY_CHAIN_INFO, STKPARAM_ATTR_RW, 4, &STK500V2_PARAM_DaisyChain, STK500V2_OnWriteDaisyChain),
	STKPARAM_DECLEAR(PARAM_RUN_AFTER_PROGRAMMING, STKPARAM_ATTR_W, 1, &STK500V2_PARAM_RunAfterProgramming, 0),
	STKPARAM_NULL
};

bool STK500V2_OnWriteDaisyChain(const struct STKPARAM *param, uint8 attr)
{
	if (STKPARAM_ATTR_W == attr)
	{
		if((STK500V2_EmuMode == EMUMODE_MEGA_JTAG) || (STK500V2_EmuMode == EMUMODE_AVR32_JTAG))
		{
			JTAG_TAP_SetDaisyChainPos((STK500V2_PARAM_DaisyChain >> 0) & 0xFF, 
										(STK500V2_PARAM_DaisyChain >> 8) & 0xFF, 
										(STK500V2_PARAM_DaisyChain >> 16) & 0xFF, 
										(STK500V2_PARAM_DaisyChain >> 24) & 0xFF);
		}
	}
	return true;
}

bool STK500V2_OnWriteEmuMode(const struct STKPARAM *param, uint8 attr)
{
	if (STKPARAM_ATTR_W == attr)
	{
		if((STK500V2_EmuMode == EMUMODE_MEGA_JTAG) || (STK500V2_EmuMode == EMUMODE_AVR32_JTAG))
		{
			JTAG_TAP_Init(AVR_JTAG_DEFAULT_SPEED, JTAG_TAP_ASYN);
		}
		else
		{
			JTAG_TAP_Fini();
		}
		return (STK500V2_EmuMode == EMUMODE_MEGA_JTAG) 
				|| (STK500V2_EmuMode == EMUMODE_NONE) 
				|| (STK500V2_EmuMode == EMUMODE_SPI) 
				|| (STK500V2_EmuMode == EMUMODE_AVR32_JTAG) 
				|| (STK500V2_EmuMode == EMUMODE_XMEGA_JTAG) 
				|| (STK500V2_EmuMode == EMUMODE_XMEGA_PDI);
	}
	return true;
}

/***************************************** CRC Calc. *****************************************/
#define STK500V2_CRC_INIT				0xFFFF

static const uint16 STK500V2_crc_tbl[256] = {
 0x0000, 0x1189, 0x2312, 0x329b, 0x4624, 0x57ad, 0x6536, 0x74bf,
 0x8c48, 0x9dc1, 0xaf5a, 0xbed3, 0xca6c, 0xdbe5, 0xe97e, 0xf8f7,
 0x1081, 0x0108, 0x3393, 0x221a, 0x56a5, 0x472c, 0x75b7, 0x643e,
 0x9cc9, 0x8d40, 0xbfdb, 0xae52, 0xdaed, 0xcb64, 0xf9ff, 0xe876,
 0x2102, 0x308b, 0x0210, 0x1399, 0x6726, 0x76af, 0x4434, 0x55bd,
 0xad4a, 0xbcc3, 0x8e58, 0x9fd1, 0xeb6e, 0xfae7, 0xc87c, 0xd9f5,
 0x3183, 0x200a, 0x1291, 0x0318, 0x77a7, 0x662e, 0x54b5, 0x453c,
 0xbdcb, 0xac42, 0x9ed9, 0x8f50, 0xfbef, 0xea66, 0xd8fd, 0xc974,
 0x4204, 0x538d, 0x6116, 0x709f, 0x0420, 0x15a9, 0x2732, 0x36bb,
 0xce4c, 0xdfc5, 0xed5e, 0xfcd7, 0x8868, 0x99e1, 0xab7a, 0xbaf3,
 0x5285, 0x430c, 0x7197, 0x601e, 0x14a1, 0x0528, 0x37b3, 0x263a,
 0xdecd, 0xcf44, 0xfddf, 0xec56, 0x98e9, 0x8960, 0xbbfb, 0xaa72,
 0x6306, 0x728f, 0x4014, 0x519d, 0x2522, 0x34ab, 0x0630, 0x17b9,
 0xef4e, 0xfec7, 0xcc5c, 0xddd5, 0xa96a, 0xb8e3, 0x8a78, 0x9bf1,
 0x7387, 0x620e, 0x5095, 0x411c, 0x35a3, 0x242a, 0x16b1, 0x0738,
 0xffcf, 0xee46, 0xdcdd, 0xcd54, 0xb9eb, 0xa862, 0x9af9, 0x8b70,
 0x8408, 0x9581, 0xa71a, 0xb693, 0xc22c, 0xd3a5, 0xe13e, 0xf0b7,
 0x0840, 0x19c9, 0x2b52, 0x3adb, 0x4e64, 0x5fed, 0x6d76, 0x7cff,
 0x9489, 0x8500, 0xb79b, 0xa612, 0xd2ad, 0xc324, 0xf1bf, 0xe036,
 0x18c1, 0x0948, 0x3bd3, 0x2a5a, 0x5ee5, 0x4f6c, 0x7df7, 0x6c7e,
 0xa50a, 0xb483, 0x8618, 0x9791, 0xe32e, 0xf2a7, 0xc03c, 0xd1b5,
 0x2942, 0x38cb, 0x0a50, 0x1bd9, 0x6f66, 0x7eef, 0x4c74, 0x5dfd,
 0xb58b, 0xa402, 0x9699, 0x8710, 0xf3af, 0xe226, 0xd0bd, 0xc134,
 0x39c3, 0x284a, 0x1ad1, 0x0b58, 0x7fe7, 0x6e6e, 0x5cf5, 0x4d7c,
 0xc60c, 0xd785, 0xe51e, 0xf497, 0x8028, 0x91a1, 0xa33a, 0xb2b3,
 0x4a44, 0x5bcd, 0x6956, 0x78df, 0x0c60, 0x1de9, 0x2f72, 0x3efb,
 0xd68d, 0xc704, 0xf59f, 0xe416, 0x90a9, 0x8120, 0xb3bb, 0xa232,
 0x5ac5, 0x4b4c, 0x79d7, 0x685e, 0x1ce1, 0x0d68, 0x3ff3, 0x2e7a,
 0xe70e, 0xf687, 0xc41c, 0xd595, 0xa12a, 0xb0a3, 0x8238, 0x93b1,
 0x6b46, 0x7acf, 0x4854, 0x59dd, 0x2d62, 0x3ceb, 0x0e70, 0x1ff9,
 0xf78f, 0xe606, 0xd49d, 0xc514, 0xb1ab, 0xa022, 0x92b9, 0x8330,
 0x7bc7, 0x6a4e, 0x58d5, 0x495c, 0x3de3, 0x2c6a, 0x1ef1, 0x0f78
};

static uint16 STK500V2_CRC_Val;

/// Calc 16-bit CRC for STK500v2 Protocol
/// @param[in]		c current char
static void STK500V2_CRC16(uint8 c)
{
	uint16 tbl_val;
	uint8 idx = (STK500V2_CRC_Val ^ c);

	tbl_val = STK500V2_crc_tbl[idx];

	STK500V2_CRC_Val = (STK500V2_CRC_Val >> 8) ^ tbl_val;
}
/************************************* End CRC Calc. *****************************************/


/***************************************** Responses *****************************************/
/// Prepare Response Package
/// @param[in]	ID		Response ID
/// @param[in]	len		Response Length
void STK500V2_PrepareRSP(uint8 ID, uint32 len)
{
	rep_len = len;
	buffer_out[8] = ID;
}

/// Prepare Event Package
/// @param[in]	ID		Response ID
/// @param[in]	len		Response Length
void STK500V2_PrepareEvent(uint8 ID, uint32 len)
{
	rep_len = len;
	buffer_out[1] = 0xFF;
	buffer_out[2] = 0xFF;
	buffer_out[8] = ID;
}

/// STK500v2 Reply to SignOn
/// @return
static void STK500V2_RSP_SIGN_ON(void)
{
	STK500V2_PrepareRSP(RSP_SIGN_ON, 16 + sizeof(STK500V2_DeviceName));

	buffer_out[9]	= STK500V2_comm_id;
	buffer_out[10]	= STK500V2_m_mcu_bldr;
	buffer_out[11]	= STK500V2_fw_version >> 16;
	buffer_out[12]	= STK500V2_fw_version >> 24;
	buffer_out[13]	= STK500V2_hw_version >> 8;
	buffer_out[14]	= STK500V2_s_mcu_bldr;
	buffer_out[15]	= STK500V2_fw_version;
	buffer_out[16]	= STK500V2_fw_version >> 8;
	buffer_out[17]	= STK500V2_hw_version & 0xFF;
	memcpy(&buffer_out[18], STK500V2_serial_number, 6);
	memcpy(&buffer_out[18 + sizeof(STK500V2_serial_number)], STK500V2_DeviceName, sizeof(STK500V2_DeviceName) - 1);
}
/************************************* End Responses *****************************************/

/***************************************** STK500v2 ******************************************/
/// STK500v2 Process Common Command
/// @param[in]	dat		Command Array
/// @param[in]	len		Command Length
static void STK500V2_ProcessCommonCmd(uint8* dat, uint16 len)
{
	uint16 length;

	switch(dat[0])
	{
	case CMND_SIGN_OFF:
		GLOBAL_OUTPUT_Release();
		PWREXT_Release();

		JTAG_TAP_Fini();
		STK500V2_MCUState = MCUSTATE_STOPPED;
		STK500V2_RSP_OK();
		break;
	case CMND_SIGN_ON:
		GLOBAL_OUTPUT_Acquire();
		PWREXT_Acquire();

		STK500V2_MCUState = MCUSTATE_STOPPED;

		STK500V2_RSP_SIGN_ON();
		break;
	case CMND_SET_PARAMETER:
		length = 0;
		if (STKPARAM_GetSize(STK500V2_Param, dat[1], (uint8*)&length))
		{
			STKPARAM_SetValue(STK500V2_Param, dat[1], &dat[2]);
			STK500V2_RSP_OK();
		}
		break;
	case CMND_GET_PARAMETER:
		length = 0;
		if (STKPARAM_GetSize(STK500V2_Param, dat[1], (uint8*)&length))
		{
			STKPARAM_GetValue(STK500V2_Param, dat[1], &dat[1]);
			STK500V2_RSP_PARAMETER(length);
		}
		break;
	case CMND_SET_DEVICE_DESCRIPTOR:
#define DDF_COPY(e)		do{memcpy(&ddf->e, dat+length, sizeof(ddf->e));length += sizeof(ddf->e);}while(0)
		length = 1;
		DDF_COPY(ucReadIO);
		DDF_COPY(ucReadIOShadow);
		DDF_COPY(ucWriteIO);
		DDF_COPY(ucWriteIOShadow);
		DDF_COPY(ucReadExtIO);
		DDF_COPY(ucReadIOExtShadow);
		DDF_COPY(ucWriteExtIO);
		DDF_COPY(ucWriteIOExtShadow);
		DDF_COPY(ucIDRAddress);
		DDF_COPY(ucSPMCRAddress);
		DDF_COPY(ucRAMPZAddress);
		DDF_COPY(uiFlashPageSize);
		DDF_COPY(ucEepromPageSize);
		DDF_COPY(ulBootAddress);
		DDF_COPY(uiUpperExtIOLoc);
		DDF_COPY(ulFlashSize);
		DDF_COPY(ucEepromInst);
		DDF_COPY(ucFlashInst);
		DDF_COPY(ucSPHAddr);
		DDF_COPY(ucSPLAddr);
		DDF_COPY(uiFlashPages);
		DDF_COPY(ucDWDRAddress);
		DDF_COPY(ucDWBasePC);
		DDF_COPY(ucAllowFullPageBitstream);
		DDF_COPY(uiDtartSmallestBootloaderSection);
		DDF_COPY(ucEnablePageProgramming);
		DDF_COPY(ucCacheType);
		DDF_COPY(uiSramStartAddr);
		DDF_COPY(ucResetType);
		DDF_COPY(ucPCMaskExtended);
		DDF_COPY(ucPCMaskHigh);
		DDF_COPY(ucEindAddress);
		DDF_COPY(uiEECRAddr);

		STK500V2_RSP_OK();
		break;
	case CMND_GET_SYNC:
		STK500V2_RSP_OK();
		break;
	}
}

/// STK500v2 Process Command
/// @param[in]	dat		Command Array
/// @param[in]	len		Command Length
static void STK500V2_ProcessCmd(uint8* dat, uint16 len)
{
	if(dat[0] == CMND_ISP_PACKET)
	{
		// This package is STK500 package encapsulated according to STK500v2 protocol
		// call STK500 command processor
		STK500_ProcessCmd(dat + 3, len - 3);
		// encapsulate return data according to STK500v2 protocol
		memcpy(dat + 1, dat + 3, rep_len);

		STK500V2_RSP_SPI_DATA();
	}
	else
	{
		// process common command first, if processed, rep_len will be > 0
		STK500V2_ProcessCommonCmd(dat, len);

		if(!rep_len)
		{
			// call different processor according to different mode
			switch(STK500V2_EmuMode)
			{
			case EMUMODE_MEGA_JTAG:
				STK500V2_MEGA_JTAG_ProcessCmd(dat, len);
				break;
			case EMUMODE_AVR32_JTAG:
				STK500V2_AVR32_JTAG_ProcessCmd(dat, len);
				break;
			case EMUMODE_NONE:
			case EMUMODE_SPI:
				STK500V2_RSP_OK();
				break;
			default:
				STK500V2_RSP_ILLEGAL_EMULATOR_MODE();
				break;
			}
		}

		if (!rep_len)
		{
			STK500V2_RSP_FAILED();
		}
	}
}

void STK500V2_Process(uint8* dat, uint16 len)
{
	if (dat[0] != STK500V2_COMMAND_CHAR)
	{
		return;
	}

	STK500V2_ProcessCmd(dat + 8, len - 10);

	// set reply length in the 8-byte protocol area
	dat[3] = (uint8)(rep_len & 0xFFFF);
	dat[4] = (uint8)((rep_len & 0xFFFF) >> 8);

	// Calc CRC16
	STK500V2_CRC_Val = STK500V2_CRC_INIT;
	for(len = 0; len < (rep_len & 0xFFFF) + 8; len++)
	{
		STK500V2_CRC16(dat[len]);
	}
	// Append CRC16
	memcpy(dat + (rep_len & 0xFFFF) + 8, (u8*)&STK500V2_CRC_Val, 2);

	rep_len += 10;		// 8 bytes protocol area and 2 bytes CRC16
}

/// Poll target status when debugging
void STK500V2_Poll(void)
{
}

/************************************* End STK500v2 ******************************************/
