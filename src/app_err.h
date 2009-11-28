/***************************************************************************
 *   Copyright (C) 2009 by Simon Qian <SimonQian@SimonQian.com>            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef __APP_ERR_H_INCLUDED__
#define __APP_ERR_H_INCLUDED__

// Common error messages
#define ERRMSG_TRY_HELP						"Try add '--help' or '-h' for more information.\n"
#define ERRMSG_TRY_SUPPORT					"Try add '--support' or '-S' for more information.\n"
#define ERRMSG_MUTIPLE_DEFINED				"Mutiple %s defined.\n"
#define ERRMSG_NOT_DEFINED					"%s is not defined, please define first.\n"

#define ERRMSG_NOT_ENOUGH_MEMORY			"Lack of memory.\n"
#define ERRCODE_NOT_ENOUGH_MEMORY			ERROR_FAIL

#define ERRMSG_INVALID						"%s is invalide for %s.\n"
#define ERRMSG_INVALID_CHARACTER			"%c is invalide for %s.\n"
#define ERRMSG_INVALID_CHARACTER_MESSAGE	"%c is invalide for %s, %s\n"
#define ERRMSG_INVALID_VALUE				"%d is invalid for %s.\n"
#define ERRMSG_INVALID_VALUE_MESSAGE		"%d is invalid for %s, %s\n"
#define ERRMSG_INVALID_HEX					"0x%X is invalid for %s.\n"
#define ERRMSG_INVALID_HEX_MESSAGE			"0x%X is invalid for %s, %s\n"
#define ERRMSG_INVALID_ADDRESS				"Address 0x%X is invalid for %s.\n"
#define ERRMSG_INVALID_INDEX				"Index %d is invalid for %s.\n"
#define ERRMSG_INVALID_RANGE				"Invalid range for %s.\n"
#define ERRCODE_INVALID						ERROR_FAIL

#define ERRMSG_INVALID_OPTION				"Invalid option: %c.\n"
#define ERRMSG_INVALID_CMD					"Invalid cmd: %s.\n"
#define ERRCODE_INVALID_OPTION				ERROR_FAIL

#define ERRMSG_INVALID_PARAMETER			"Invalid parameter of %s.\n"
#define ERRCODE_INVALID_PARAMETER			ERROR_FAIL

#define ERRMSG_NOT_SUPPORT					"%s is not supported.\n"
#define ERRMSG_NOT_SUPPORT_BY				"%s is not supported by %s.\n"
#define ERRMSG_NOT_SUPPORT_AS				"%s is not supported as %s.\n"
#define ERRCODE_NOT_SUPPORT					ERROR_FAIL

#define ERRMSG_INVALID_BUFFER				"Buffer %s is not valid.\n"
#define ERRCODE_INVALID_BUFFER				ERROR_FAIL

#define ERRMSG_INVALID_HANDLER				"%s is not valid handler for %s.\n"
#define ERRCODE_INVALID_HANDLER				ERROR_FAIL

#define ERRMSG_FAILURE_OPERATION			"Fail to %s.\n"
#define ERRMSG_FAILURE_OPERATION_ERRCODE	"Fail to %s, error code is %d.\n"
#define ERRMSG_FAILURE_OPERATION_ERRCODE16	"Fail to %s, error code is 0x%X.\n"
#define ERRMSG_FAILURE_OPERATION_ERRSTRING	"Fail to %s, error string is %s.\n"
#define ERRMSG_FAILURE_OPERATION_MESSAGE	"Fail to %s, %s\n"
#define ERRMSG_FAILURE_OPERATE_DEVICE		"Fail to operate %s.\n"
#define ERRMSG_FAILURE_HANDLE_DEVICE		"Fail to %s %s.\n"
#define ERRCODE_FAILURE_OPERATION			ERROR_FAIL

#define ERRMSG_FAILURE_OPEN					"Fail to open %s.\n"
#define ERRCODE_FAILURE_OPEN				ERROR_FAIL





// User defined error messages
#define ERRMSG_AUTODETECT_FAIL				"%s auto-detect failed.\n"
#define ERRCODE_AUTODETECT_FAIL				ERROR_FAIL

#define ERRMSG_INVALID_PROG_MODE			"Program mode %d is invalid for %s.\n"
#define ERRCODE_INVALID_PROG_MODE			ERROR_FAIL

#define ERRMSG_FAILURE_ENTER_PROG_MODE		"Fail to enter into program mode, try slower frequency?\n"
#define ERRCODE_FAILURE_ENTER_PROG_MODE		ERROR_FAIL

#define ERRMSG_INVALID_CHIP_ID				"Chip-id unmatch, read=0x%X, want=0x%X\n"
#define ERRCODE_INVALID_CHIP_ID				ERROR_FAIL

#define ERRMSG_FAILURE_VERIFY_TARGET_02X	"%s verify failed, read=0x%02X, want=0x%02X.\n"
#define ERRMSG_FAILURE_VERIFY_TARGET_AT_02X	"%s verify failed at 0x%X, read=0x%02X, want=0x%02X.\n"
#define ERRMSG_FAILURE_VERIFY_TARGET_04X	"%s verify failed, read=0x%04X, want=0x%04X.\n"
#define ERRMSG_FAILURE_VERIFY_TARGET_AT_04X	"%s verify failed at 0x%X, read=0x%04X, want=0x%04X.\n"
#define ERRMSG_FAILURE_VERIFY_TARGET_06X	"%s verify failed, read=0x%06X, want=0x%06X.\n"
#define ERRMSG_FAILURE_VERIFY_TARGET_D		"%s verify failed, read=%d, want=%d.\n"
#define ERRMSG_FAILURE_VERIFY_TARGET_AT_D	"%s verify failed at 0x%X, read=%d, want=%d.\n"
#define ERRCODE_FAILURE_VERIFY_TARGET		ERROR_FAIL

#define ERRMSG_FAILURE_OPERATION_ADDR		"Fail to %s at 0x%X.\n"
#define ERRCODE_FAILURE_OPERATION_ADDR		ERROR_FAIL

// User defined information messages
#define INFOMSG_TRY_AUTODETECT				"Chip name undefined, try auto-detecting.\n"
#define INFOMSG_AUTODETECT_SIGNATURE		"Auto-detect signature is 0x%X\n"
#define INFOMSG_CHIP_FOUND					"%s found\n"
#define INFOMSG_TARGET_VOLTAGE				"Target runs at %.2fV\n"
#define INFOMSG_TARGET_LOW_POWER			"No power is detected on target.\n"
#define INFOMSG_USE_DEFAULT					"%s not defined, use %s as default.\n"
#define INFOMSG_TARGET_CHIP_ID				"Chip-id read is 0x%X.\n"
#define INFOMSG_CHECKSUM					"Checksum is 0x%X.\n"
#define INFOMSG_CHECKSUM_BANK				"Checksum at band%d is 0x%X.\n"

#define INFOMSG_CHECKING					"checking %s\n"
#define INFOMSG_CHECKED						"%s checked\n"
#define INFOMSG_ERASING						"erasing %s\n"
#define INFOMSG_ERASED						"%s erased\n"
#define INFOMSG_PROGRAMMING					"programming %s\n"
#define INFOMSG_PROGRAMMED_SIZE				"%s programmed for %dbytes\n"
#define INFOMSG_PROGRAMMED					"%s programmed\n"
#define INFOMSG_VERIFYING					"verifying %s\n"
#define INFOMSG_VERIFIED_SIZE				"%s verified for %dbytes\n"
#define INFOMSG_VERIFIED					"%s verified\n"
#define INFOMSG_READING						"reading %s\n"
#define INFOMSG_READ_SIZE					"%s read for %dbytes\n"
#define INFOMSG_READ						"%s read\n"
#define INFOMSG_READ_VALUE					"%s read is %d\n"
#define INFOMSG_READ_VALUE_02X				"%s read is 0x%02X\n"
#define INFOMSG_READ_VALUE_04X				"%s read is 0x%04X\n"
#define INFOMSG_READ_VALUE_06X				"%s read is 0x%06X\n"
#define INFOMSG_READ_VALUE_08X				"%s read is 0x%08X\n"

#endif /* __APP_ERR_H_INCLUDED__ */

