/**************************************************************************
 *  Copyright (C) 2008 - 2010 by Simon Qian                               *
 *  SimonQian@SimonQian.com                                               *
 *                                                                        *
 *  Project:    STK500V2                                                  *
 *  File:       STK500_Const.h 	                                          *
 *  Author:     SimonQian                                                 *
 *  Versaion:   See changelog                                             *
 *  Purpose:    consts for STK500 protocol                                *
 *  License:    See license                                               *
 *------------------------------------------------------------------------*
 *  Change Log:                                                           *
 *      YYYY-MM-DD:     What(by Who)                                      *
 *      2010-08-31:     created(by SimonQian)                             *
 **************************************************************************/

#define STK500_CMDTYPE_MASK						0xF0
enum STK500_CMDTYPE
{
	CMDTYPE_GENERAL								= 0x00,
	CMDTYPE_ISP									= 0x10,
	CMDTYPE_HVPP								= 0x20,
	CMDTYPE_HVSP								= 0x30,
};

// General Command
enum STK500_GENCMD
{
	// General Command
	CMD_SIGN_ON									= 0x01,
	CMD_SET_PARAMETER							= 0x02,
	CMD_GET_PARAMETER							= 0x03,
	CMD_OSCCAL									= 0x05,
	CMD_LOAD_ADDRESS							= 0x06,
	CMD_FIRMWARE_UPGRADE						= 0x07,
	CMD_RESET_PROTECTION						= 0x0A,
};

// Programming Command
enum STK500_PROGCMD
{
	// ISP Command
	CMD_ENTER_PROGMODE							= 0x00,
	CMD_LEAVE_PROGMODE							= 0x01,
	CMD_CHIP_ERASE								= 0x02,
	CMD_PROGRAM_FLASH							= 0x03,
	CMD_READ_FLASH								= 0x04,
	CMD_PROGRAM_EEPROM							= 0x05,
	CMD_READ_EEPROM								= 0x06,
	CMD_PROGRAM_FUSE							= 0x07,
	CMD_READ_FUSE								= 0x08,
	CMD_PROGRAM_LOCK							= 0x09,
	CMD_READ_LOCK								= 0x0A,
	CMD_READ_SIGNATURE							= 0x0B,
	CMD_READ_OSCCAL								= 0x0C,
	CMD_SPI_MULTI								= 0x0D,
};

// Status
enum STK500_STATUS
{
	STATUS_CMD_OK								= 0x00,
	STATUS_CMD_TOUT								= 0x80,
	STATUS_RDY_BSY_TOUT							= 0x81,
	STATUS_SET_PARAM_MISSING					= 0x82,
	STATUS_CMD_FAILED							= 0xC0,
	STATUS_CMD_UNKNOWN							= 0xC9,
};

// Parameter
enum STK500_PARAM
{
	PARAM_BUILD_NUMBER_LOW						= 0x80,
	PARAM_BUILD_NUMBER_HIGH						= 0x81,
	PARAM_HW_VER								= 0x90,
	PARAM_SW_MAJOR								= 0x91,
	PARAM_SW_MINOR								= 0x92,
	PARAM_VTARGET								= 0x94,
	PARAM_SCK_DURATION							= 0x98,
	PARAM_RESET_POLARITY						= 0x9E,
	PARAM_STATUS_TGT_CONN						= 0xA1,
	PARAM_DISCHARGEDELAY						= 0xA4,
};
