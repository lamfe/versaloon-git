/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK500v2_Const.h                                          *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    consts for STK500v2 protocol                              *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/

#include "app_cfg.h"

//#define RSP_MAX_PKGSIZE						278
//#define CMD_MAX_PKGSIZE						317

#define STK500V2_VID							0xEB,0x03
#define STK500V2_USB_SIZ_STRING_VENDOR			12
#define STK500V2_USB_STRING_VENDOR				'A', 0, 't', 0, 'm', 0, 'e', 0, 'l', 0
#define STK500V2_USB_SIZ_STRING_SERIAL			28
#define STK500V2_USB_STRING_SERIAL				'0', 0, '0', 0, 'A', 0, '2', 0, '0', 0, '0', 0, '0', 0, '0', 0, '0', 0, '1', 0, '1', 0, 'E', 0, 0, 0

#if USB_PROTOCOL == USB_AT_DRAGON
// AVR Dragon
#define STK500V2_PID							0x07,0x21
#define STK500V2_USB_SIZ_STRING_PRODUCT			22
#define STK500V2_USB_STRING_PRODUCT				'A', 0, 'V', 0, 'R', 0, ' ', 0, 'D', 0, 'r', 0, 'a', 0, 'g', 0, 'o', 0, 'n', 0

#define STK500V2_Device_Name					"AVR Dragon"
#define STK500V2_HW_VER							0x0102
#define STK500V2_COMM_ID						0x01
#define STK500V2_SerialNumber					{0x00,0xA2,0x00,0x00,0x01,0x1E}
// Master Version
#define STK500V2_MBootload_VER					0xFF
#define STK500V2_MFW_MAJOR_VER					0x01	// Firmware Major Version for Master MCU
#define STK500V2_MFW_MINOR_VER					0xFF	// Firmware Minor Version for Master MCU
// Slave Version
#define STK500V2_SBootload_VER					0xFF
#define STK500V2_SFW_MAJOR_VER					0x02	// Firmware Major Version for Slave MCU
#define STK500V2_SFW_MINOR_VER					0xFF	// Firmware Minor Version for Slave MCU
#elif USB_PROTOCOL == USB_AT_JTAGICE_MKII
// JTAGICE mkII
#define STK500V2_PID							0x03,0x21
#define STK500V2_USB_SIZ_STRING_PRODUCT			26
#define STK500V2_USB_STRING_PRODUCT				'J', 0, 'T', 0, 'A', 0, 'G', 0, 'I', 0, 'C', 0, 'E', 0, ' ', 0, 'm', 0, 'k', 0, 'I', 0, 'I', 0

#define STK500V2_Device_Name					"JTAGICE mkII"
#define STK500V2_HW_VER							0x0000
#define STK500V2_COMM_ID						0x01
#define STK500V2_SerialNumber					{0x00,0xA2,0x00,0x00,0x01,0x1E}
// Master Version
#define STK500V2_MBootload_VER					0xFF
#define STK500V2_MFW_MAJOR_VER					0x06	// Firmware Major Version for Master MCU
#define STK500V2_MFW_MINOR_VER					0xFF	// Firmware Minor Version for Master MCU
// Slave Version
#define STK500V2_SBootload_VER					0xFF
#define STK500V2_SFW_MAJOR_VER					0x06	// Firmware Major Version for Slave MCU
#define STK500V2_SFW_MINOR_VER					0xFF	// Firmware Minor Version for Slave MCU
#endif

#define __CONNECT(a, b)							a ## b

/***************************************** Common Responses *****************************************/
void STK500V2_PrepareRSP(uint8_t, uint32_t);
#define DECLEAR_RSP(RSP, size)					STK500V2_PrepareRSP(__CONNECT(RSP_, RSP), size)
#define STK500V2_RSP_OK()						DECLEAR_RSP(OK, 1)
#define STK500V2_RSP_FAILED()					DECLEAR_RSP(FAILED, 1)
#define STK500V2_RSP_ILLEGAL_PARAMETER()		DECLEAR_RSP(ILLEGAL_PARAMETER, 1)
#define STK500V2_RSP_ILLEGAL_MEMORY_TYPE()		DECLEAR_RSP(ILLEGAL_MEMORY_TYPE, 1)
#define STK500V2_RSP_ILLEGAL_MEMORY_RANGE()		DECLEAR_RSP(ILLEGAL_MEMORY_RANGE, 1)
#define STK500V2_RSP_ILLEGAL_COMMAND()			DECLEAR_RSP(ILLEGAL_COMMAND, 1)
#define STK500V2_RSP_ILLEGAL_VALUE()			DECLEAR_RSP(ILLEGAL_VALUE, 1)
#define STK500V2_RSP_ILLEGAL_EMULATOR_MODE()	DECLEAR_RSP(ILLEGAL_EMULATOR_MODE, 1)
#define STK500V2_RSP_ILLEGAL_MCU_STATE()		DECLEAR_RSP(ILLEGAL_MCU_STATE, 1)
#define STK500V2_RSP_DEBUGWIRE_SYNC_FAILED()	DECLEAR_RSP(DEBUGWIRE_SYNC_FAILED, 1)
#define STK500V2_RSP_ILLEGAL_BREAKPOINT()		DECLEAR_RSP(ILLEGAL_BREAKPOINT, 1)
#define STK500V2_RSP_ILLEGAL_JTAG_ID()			DECLEAR_RSP(ILLEGAL_JTAG_ID, 1)
#define STK500V2_RSP_NO_TARGET_POWER()			DECLEAR_RSP(NO_TARGET_POWER, 1)
#define STK500V2_RSP_ILLEGAL_POWER_STATE()		DECLEAR_RSP(ILLEGAL_POWER_STATE, 1)
#define STK500V2_RSP_MEMORY(len)				DECLEAR_RSP(MEMORY, 1 + (len))
#define STK500V2_RSP_PC()						DECLEAR_RSP(PC, 5)
#define STK500V2_RSP_SPI_DATA()					DECLEAR_RSP(SPI_DATA, rep_len + 1)
#define STK500V2_RSP_SELFTEST()					DECLEAR_RSP(SELFTEST, 9)
#define STK500V2_RSP_PARAMETER(len)				DECLEAR_RSP(PARAMETER, len + 1)
#define STK500V2_RSP_SCAN_CHAIN_READ(len)		DECLEAR_RSP(SCAN_CHAIN_READ, 1 + (len))


/***************************************** Common Events *****************************************/
void STK500V2_PrepareEvent(uint8_t ID, uint32_t len);
#define DECLEAR_EVT(EVT, size)					STK500V2_PrepareEvent(__CONNECT(STK500V2_EVTVAL_, EVT), size)
#define STK500V2_EVT_BREAK(pc, bc)				do{\
													buffer_out[9] = (pc) & 0xFF;\
													buffer_out[10] = pc >> 8;\
													buffer_out[11] = 0;\
													buffer_out[12] = 0;\
													buffer_out[13] = (bc);\
													DECLEAR_EVT(BREAK, 6);\
												}while(0)
#define STK500V2_EVT_TARGET_POWER_ON()			DECLEAR_EVT(TARGET_POWER_ON, 1)
#define STK500V2_EVT_TARGET_POWER_OFF()			DECLEAR_EVT(TARGET_POWER_OFF, 1)
#define STK500V2_EVT_EXT_RESET()				DECLEAR_EVT(EXT_RESET, 1)
#define STK500V2_EVT_TARGET_SLEEP()				DECLEAR_EVT(TARGET_SLEEP, 1)
#define STK500V2_EVT_TARGET_WAKEUP()			DECLEAR_EVT(TARGET_WAKEUP, 1)
#define STK500V2_EVT_ICE_POWER_ERROR_STATE()	DECLEAR_EVT(ICE_POWER_ERROR_STATE, 1)
#define STK500V2_EVT_ICE_POWER_OK()				DECLEAR_EVT(ICE_POWER_OK, 1)
#define STK500V2_EVT_IDR_DIRTY()				DECLEAR_EVT(IDR_DIRTY, 2)


/***************************************** Const Definations *****************************************/
// Command
enum STK500V2_CMND
{
	CMND_SIGN_OFF								= 0x00,
	CMND_SIGN_ON								= 0x01,
	CMND_SET_PARAMETER							= 0x02,
	CMND_GET_PARAMETER							= 0x03,
	CMND_WRITE_MEMORY							= 0x04,
	CMND_READ_MEMORY							= 0x05,
	CMND_WRITE_PC								= 0x06,
	CMND_READ_PC								= 0x07,
	CMND_GO										= 0x08,
	CMND_SINGLE_STEP							= 0x09,
	CMND_FORCED_STOP							= 0x0A,
	CMND_RESET									= 0x0B,
	CMND_SET_DEVICE_DESCRIPTOR					= 0x0C,
	CMND_ERASEPAGE_SPM							= 0x0D,
	CMND_GET_SYNC								= 0x0F,
	CMND_SELFTEST								= 0x10,
	CMND_SET_BREAK								= 0x11,
	CMND_GET_BREAK								= 0x12,
	CMND_CHIP_ERASE								= 0x13,
	CMND_ENTER_PROGMODE							= 0x14,
	CMND_LEAVE_PROGMODE							= 0x15,
	CMND_CLR_BREAK								= 0x1A,
	CMND_RUN_TO_ADDR							= 0x1C,
	CMND_SPI_CMD								= 0x1D,
	CMND_CLEAR_EVENTS							= 0x22,
	CMND_RESTORE_TARGET							= 0x23,
	CMND_ISP_PACKET								= 0x2F,
	CMND_JTAG_INSTR								= 0x24,
	CMND_JTAG_DATA								= 0x25,
	CMND_JTAG_SAB_WRITE							= 0x28,
	CMND_JTAG_SAB_READ							= 0x29,
	CMND_JTAG_BLOCK_READ						= 0x2C,
	CMND_JTAG_BLOCK_WRITE						= 0x2D,
	CMND_XMEGA_ERASE							= 0x34,
};

// State Machine
enum STK500V2_STATEMACHINE
{
	STATEMACHINE_MESSAGE_START					= 0x01,
	STATEMACHINE_SEQUENCE_NUMBER				= 0x02,
	STATEMACHINE_MESSAGE_SIZE					= 0x03,
	STATEMACHINE_TOKEN							= 0x04,
	STATEMACHINE_MESSAGE_BODY					= 0x05,
	STATEMACHINE_CRC							= 0x06,
};

// STK500v2 Package Char
#define STK500V2_COMMAND_CHAR					0x1B
#define STK500V2_TOKEN_CHAR						0x0E

// Response
enum STK500V2_RSP
{
	RSP_OK										= 0x80,
	RSP_PARAMETER								= 0x81,
	RSP_MEMORY									= 0x82,
	RSP_GET_BREAK								= 0x83,
	RSP_PC										= 0x84,
	RSP_SELFTEST								= 0x85,
	RSP_SIGN_ON									= 0x86,
	RSP_SCAN_CHAIN_READ							= 0x87,
	RSP_SPI_DATA								= 0x88,
	RSP_FAILED									= 0xA0,
	RSP_ILLEGAL_PARAMETER						= 0xA1,
	RSP_ILLEGAL_MEMORY_TYPE						= 0xA2,
	RSP_ILLEGAL_MEMORY_RANGE					= 0xA3,
	RSP_ILLEGAL_EMULATOR_MODE					= 0xA4,
	RSP_ILLEGAL_MCU_STATE						= 0xA5,
	RSP_ILLEGAL_VALUE							= 0xA6,
	RSP_ILLEGAL_BREAKPOINT						= 0xA8,
	RSP_ILLEGAL_JTAG_ID							= 0xA9,
	RSP_ILLEGAL_COMMAND							= 0xAA,
	RSP_NO_TARGET_POWER							= 0xAB,
	RSP_DEBUGWIRE_SYNC_FAILED					= 0xAC,
	RSP_ILLEGAL_POWER_STATE						= 0xAD,
};

// Event
enum STK500V2_EVT
{
	EVT_BREAK									= 0xE0,
	EVT_TARGET_POWER_ON							= 0xE4,
	EVT_TARGET_POWER_OFF						= 0xE5,
	EVT_EXT_RESET								= 0xE7,
	EVT_TARGET_SLEEP							= 0xE8,
	EVT_TARGET_WAKEUP							= 0xE9,
	EVT_ICE_POWER_ERROR_STATE					= 0xEA,
	EVT_ICE_POWER_OK							= 0xEB,
	EVT_IDR_DIRTY								= 0xEC,
	EVT_ERROR_PHY_FORCE_BREAK_TIMEOUT			= 0xE2,
	EVT_ERROR_PHY_RELEASE_BREAK_TIMEOUT			= 0xE3,
	EVT_ERROR_PHY_MAX_BIT_LENGTH_DIFF			= 0xED,
	EVT_ERROR_PHY_SYNC_TIMEOUT					= 0xF0,
	EVT_ERROR_PHY_SYNC_TIMEOUT_BAUD				= 0xF4,
	EVT_ERROR_PHY_SYNC_OUT_OF_RANGE				= 0xF5,
	EVT_ERROR_PHY_SYNC_WAIT_TIMEOUT				= 0xF6,
	EVT_ERROR_PHY_RECEIVE_TIMEOUT				= 0xF7,
	EVT_ERROR_PHY_RECEIVED_BREAK				= 0xF8,
	EVT_ERROR_PHY_OPT_RECEIVE_TIMEOUT			= 0xF9,
	EVT_ERROR_PHY_OPT_RECEIVED_BREAK			= 0xFA,
	EVT_ERROR_PHY_NO_ACTIVITY					= 0xFB,
};

// Memory Type
enum STK500V2_MEMTYPE
{
	MEMTYPE_IO_SHADOW							= 0x30,
	MEMTYPE_SRAM								= 0x20,
	MEMTYPE_EEPROM								= 0x22,
	MEMTYPE_EVENT								= 0x60,
	MEMTYPE_SPM									= 0xA0,
	MEMTYPE_FLASH_PAGE							= 0xB0,
	MEMTYPE_EEPROM_PAGE							= 0xB1,
	MEMTYPE_FUSE_BITS							= 0xB2,
	MEMTYPE_LOCK_BITS							= 0xB3,
	MEMTYPE_SIGN_JTAG							= 0xB4,
	MEMTYPE_OSCCAL_BYTE							= 0xB5,
	MEMTYPE_CAN									= 0xB6,
	MEMTYPE_XMEGA_APP							= 0xC0,
	MEMTYPE_XMEGA_BOOT							= 0xC1,
	MEMTYPE_XMEGA_USER_SIGN						= 0xC5,
	MEMTYPE_XMEGA_CALI_SIGN						= 0xC6,
};

// Parameter
enum STK500V2_PARAM
{
	PARAM_HW_VERSION							= 0x01,	// R 2 BYTES
	PARAM_FW_VERSION							= 0x02,	// R 4 BYTES
	PARAM_EMULATOR_MODE							= 0x03,	// RW
	PARAM_BAUD_RATE								= 0x05,	// RW
	PARAM_OCD_VTARGET							= 0x06,	// R 2 BYTES
	PARAM_OCD_JTAG_CLOCK						= 0x07,	// RW
	PARAM_OCD_BREAK_CAUSE						= 0x08,	// R
	PARAM_TIMERS_RUNNING_IN_STOPPED_MODE		= 0x09,	// RW
	PARAM_JTAGID								= 0x0E,	// R 4 BYTES
	PARAM_EXTERNAL_RESET						= 0x13,	// RW
	PARAM_FLASH_PAGE_SIZE						= 0x14,	// RW 2 BYTES
	PARAM_EEPROM_PAGE_SIZE						= 0x15,	// RW
	PARAM_MCU_STATE								= 0x1A,	// R
	PARAM_DAISY_CHAIN_INFO						= 0x1B,	// RW 4 BYTES, ub ua bb ba
	PARAM_BOOT_ADDRESS							= 0x1C,	// RW 4 BYTE
	PARAM_TARGET_SIGNATURE						= 0x1D,	// R 2 BYTES
	PARAM_PROGRAM_ENTRY_POINT					= 0x1F,	// W 4 BYTES
	PARAM_READ_CAN_MAILBOX						= 0x22,	// RW
	PARAM_ENABLE_IDR_EVENTS						= 0x23,	// W
	PARAM_ALLOW_PAGEPROGRAMMING_IN_SCANCHAIN	= 0x24,	// W
	PARAM_SOFT_RESET							= 0x2D,	// W
	PARAM_PDI_NVM_OFFSET						= 0x31,	// W 4 BYTES
	PARAM_PDI_APP_OFFSET						= 0x32,	// W 4 BYTES
	PARAM_PDI_BOOT_OFFSET						= 0x33,	// W 4 BYTES
	PARAM_AVR32_JTAG_ENABLE_SEQ					= 0x37,	// W
	PARAM_RUN_AFTER_PROGRAMMING					= 0x38,	// W
	PARAM_PACKET_PARSING_ERRORS					= 0x40,	// R 4 BYTES
	PARAM_VALID_PACKETS_RECEIVED				= 0x41,	// R 4 BYTES
	PARAM_INTERCOMMUNICATION_TX_FAILURES		= 0x42,	// R 4 BYTES
	PARAM_INTERCOMMUNICATION_RX_FAILURES		= 0x43,	// R 4 BYTES
	PARAM_CRC_ERRORS							= 0x44,	// R 4 BYTES
	PARAM_POWER_SOURCE							= 0x45,	// R
};

// Emulator Mode
enum STK500V2_EMUMODE
{
	EMUMODE_DEBUGWIRE							= 0x00,
	EMUMODE_MEGA_JTAG							= 0x01,
	EMUMODE_NONE								= 0x02,
	EMUMODE_SPI									= 0x03,
	EMUMODE_AVR32_JTAG							= 0x04,
	EMUMODE_XMEGA_JTAG							= 0x05,
	EMUMODE_XMEGA_PDI							= 0x06,
};

// MCU State
enum STK500V2_MCUSTATE
{
	MCUSTATE_STOPPED							= 0x00,
	MCUSTATE_RUNNING							= 0x01,
	MCUSTATE_PROGRAMMING						= 0x02,
};

// Power Source
enum STK500V2_POWER_SOURCE
{
	POWER_EXTERNAL								= 0x00,
	POWER_USB									= 0x01,
};
