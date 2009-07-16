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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "memlist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "avr8.h"
#include "avr8_internal.h"

#define CUR_TARGET_STRING			AVR8_STRING
#define cur_chip_param				avr8_chip_param
#define cur_chips_param				avr8_chips_param
#define cur_flash_offset			avr8_flash_offset
#define cur_eeprom_offset			avr8_eeprom_offset
#define cur_prog_mode				avr8_prog_mode
#define cur_frequency				avr8_isp_frequency

const program_area_map_t avr8_program_area_map[] = 
{
	{APPLICATION, 'f', 1},
//	{EEPROM, 'e', 1},
//	{FUSE, 'u', 0},
//	{LOCK, 'l', 0},
	{0, 0}
};

const avr8_param_t avr8_chips_param[] = {
//	chip_name,		signature,		prog_mode,														flash_page_size,	flash_page_num,	ee_page_size,		ee_page_num,
	{"attiny11",	0x001E9004,		AVR8_HVSP,														1024,				1,				64,					1},
	{"attiny12",	0x001E9005,		AVR8_ISP | AVR8_HVSP,											1024,				1,				64,					1},
	{"attiny13",	0x001E9007,		AVR8_ISP | AVR8_HVSP,											32,					32,				4,					16},
	{"attiny15",	0x001E9006,		AVR8_ISP | AVR8_HVSP,											1024,				1,				64,					1},
	{"attiny24",	0x001E910B,		AVR8_ISP | AVR8_HVSP,											32,					64,				4,					32},
	{"attiny44",	0x001E9207,		AVR8_ISP | AVR8_HVSP,											64,					64,				4,					64},
	{"attiny84",	0x001E930C,		AVR8_ISP | AVR8_HVSP,											64,					128,			4,					128},
	{"attiny25",	0x001E9108,		AVR8_ISP | AVR8_HVSP,											32,					64,				4,					32},
	{"attiny45",	0x001E9206,		AVR8_ISP | AVR8_HVSP,											64,					64,				4,					64},
	{"attiny85",	0x001E930B,		AVR8_ISP | AVR8_HVSP,											64,					128,			4,					128},
	{"attiny26",	0x001E9109,		AVR8_ISP | AVR8_HVPP,											32,					64,				4,					32},
	{"attiny28",	0x001E9107,		AVR8_HVPP,														2048,				1,				0,					0},
	{"attiny48",	0x001E9209,		AVR8_ISP | AVR8_HVPP,											64,					64,				4,					16},
	{"attiny88",	0x001E9311,		AVR8_ISP | AVR8_HVPP,											64,					128,			4,					16},
	{"attiny261",	0x001E910C,		AVR8_ISP | AVR8_HVPP,											32,					64,				4,					64},
	{"attiny461",	0x001E9208,		AVR8_ISP | AVR8_HVPP,											64,					64,				4,					64},
	{"attiny861",	0x001E930D,		AVR8_ISP | AVR8_HVPP,											64,					128,			4,					128},
	{"attiny2313",	0x001E910A,		AVR8_ISP | AVR8_HVPP,											32,					64,				4,					32},
	{"at90can32",	0x001E9581,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				128,			8,					128},
	{"at90can64",	0x001E9681,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				256,			8,					256},
	{"at90can128",	0x001E9781,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				512,			8,					512},
	{"at90pwm2/3",	0x001E9381,		AVR8_ISP | AVR8_HVPP,											64,					128,			4,					128},
	{"at90pwm2/3B",	0x001E9383,		AVR8_ISP | AVR8_HVPP,											64,					128,			4,					128},
	{"atmega8",		0x001E9307,		AVR8_ISP | AVR8_HVPP,											64,					128,			4,					128},
	{"atmega16",	0x001E9403,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP | AVR8_JTAG_FULL_BITSTREAM,	128,				128,			4,					128},
	{"atmega32",	0x001E9502,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP | AVR8_JTAG_FULL_BITSTREAM,	128,				256,			4,					256},
	{"atmega48",	0x001E9205,		AVR8_ISP | AVR8_HVPP,											64,					64,				4,					64},
	{"atmega88",	0x001E930a,		AVR8_ISP | AVR8_HVPP,											64,					128,			4,					128},
	{"atmega168",	0x001E9406,		AVR8_ISP | AVR8_HVPP,											128,				128,			4,					128},
	{"atmega162",	0x001E9404,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP | AVR8_JTAG_FULL_BITSTREAM,	128,				128,			4,					128},
	{"atmega164p",	0x001E940A,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								128,				128,			4,					128},
	{"atmega324p",	0x001E9508,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								128,				256,			4,					256},
	{"atmega644p",	0x001E960A,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				256,			8,					256},
	{"atmega165",	0x001E9407,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								128,				128,			4,					128},
	{"atmega169",	0x001E9405,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								128,				128,			4,					128},
	{"atmega325",	0x001E9505,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								128,				256,			4,					256},
	{"atmega3250",	0x001E9506,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								128,				256,			4,					256},
	{"atmega64",	0x001E9602,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				256,			8,					256},
	{"atmega645",	0x001E9605,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				256,			8,					256},
	{"atmega6450",	0x001E9606,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				256,			8,					256},
	{"atmega329",	0x001E9503,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								128,				256,			4,					256},
	{"atmega3290",	0x001E9504,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								128,				256,			4,					256},
	{"atmega649",	0x001E9603,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				256,			8,					256},
	{"atmega6490",	0x001E9604,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				256,			8,					256},
	{"atmega640",	0x001E9608,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				256,			8,					512},
	{"atmega1280",	0x001E9703,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				512,			8,					512},
	{"atmega1281",	0x001E9704,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				512,			8,					512},
	{"atmega2560",	0x001E9801,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				1024,			8,					512},
	{"atmega2561",	0x001E9802,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP,								256,				1024,			8,					512},
	{"atmega8515",	0x001E9306,		AVR8_ISP | AVR8_HVPP,											64,					128,			4,					128},
	{"atmega8535",	0x001E9308,		AVR8_ISP | AVR8_HVPP,											64,					128,			4,					128},
	{"atmega128",	0x001E9702,		AVR8_ISP | AVR8_JTAG | AVR8_HVPP | AVR8_JTAG_FULL_BITSTREAM,	256,				512,			8,					512},
};
avr8_param_t avr8_chip_param;

static uint32 avr8_flash_offset = 0;
static uint32 avr8_eeprom_offset = 0;

static uint8 avr8_lock = AVR8_LOCK_CHAR;
static uint32 avr8_fuse = AVR8_FUSE_CHAR;

uint16 avr8_isp_frequency = 560;
static uint8 avr8_prog_mode = 0;


static void avr8_usage(void)
{
	printf("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>      set ISP frequency, in KHz\n\
  -l,  --lock <LOCK>                set lock\n\
  -f,  --fuse <FUSE>                set fuse\n\
  -m,  --mode <MODE>                set mode<b|p>\n\n", CUR_TARGET_STRING);
}

static void avr8_support(void)
{
	uint32 i;
	
	printf("Support list of %s:\n", CUR_TARGET_STRING);
	for (i = 0; i < dimof(cur_chips_param); i++)
	{
		printf("%s: signature = 0x%06x, prog_mode = 0x%02x\n", 
				cur_chips_param[i].chip_name, 
				cur_chips_param[i].signature, 
				cur_chips_param[i].prog_mode);
	}
	printf("\n");
}

RESULT avr8_parse_argument(char cmd, const char *argu)
{
	switch (cmd)
	{
	case 'h':
		avr8_usage();
		break;
	case 'S':
		avr8_support();
		break;
	case 'F':
		// set Frequency
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		cur_frequency = (uint16)strtoul(argu, NULL, 0);
		
		break;
	case 'l':
		// define Lock
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		avr8_lock = (uint8)strtoul(argu, NULL, 0);
		
		break;
	case 'f':
		// define Fuse
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		avr8_fuse = (uint32)strtoul(argu, NULL, 0);
		
		break;
	case 'm':
		// program Mode
		if ((NULL == argu) || (strlen(argu) != 1))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		switch (argu[0])
		{
		case 'i':
			// ISP mode
			cur_prog_mode = AVR8_ISP;
			break;
		case 'j':
			// JTAG mode
			cur_prog_mode = AVR8_JTAG;
			break;
		case 'p':
			cur_prog_mode = AVR8_HVPP;
			break;
		case 's':
			cur_prog_mode = AVR8_HVSP;
			break;
		default:
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHARACTER_MESSAGE), cmd, 
					  "avr8 program mode", "MUST be 'i', 'j', 'p' or 's'!!");
			return ERRCODE_INVALID;
			break;
		}
		
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT avr8_probe_chip(char *chip_name)
{
	uint32 i;

	for (i = 0; i < dimof(cur_chips_param); i++)
	{
		if (!strcmp(cur_chips_param[i].chip_name, chip_name))
		{
			return ERROR_OK;
		}
	}
	
	return ERROR_FAIL;
}

RESULT avr8_prepare_buffer(program_info_t *pi)
{
	if (pi->app != NULL)
	{
		memset(pi->app, AVR8_FLASH_CHAR, pi->app_size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	if (pi->eeprom != NULL)
	{
		memset(pi->eeprom, AVR8_EEPROM_CHAR, pi->eeprom_size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	pi->lock_value = avr8_lock;
	pi->fuse_value = avr8_fuse;
	
	return ERROR_OK;
}

RESULT avr8_write_buffer_from_file_callback(uint32 address, uint32 seg_addr, 
											uint8* data, uint32 length, 
											void* buffer)
{
	program_info_t *pi = (program_info_t *)buffer;
	uint32 mem_addr = address & 0x0000FFFF, page_size;
	RESULT ret;

#ifdef PARAM_CHECK
	if ((length > 0) && (NULL == data))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	switch (seg_addr)
	{
	case 0x0010:
		mem_addr += 0x00010000;
	case 0x0000:
		// Flash
		if (NULL == pi->app)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "pi->app");
			return ERRCODE_INVALID_BUFFER;
		}
		
		if ((0 == cur_chip_param.flash_page_num) 
			|| (0 == cur_chip_param.flash_page_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID), "Flash", 
					  cur_chip_param.chip_name);
			return ERRCODE_INVALID;
		}
		
		mem_addr += cur_flash_offset;
		if ((mem_addr >= cur_chip_param.flash_size) 
			|| (length > cur_chip_param.flash_size) 
			|| ((mem_addr + length) > cur_chip_param.flash_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "flash memory");
			return ERRCODE_INVALID;
		}
		memcpy(pi->app + mem_addr, data, length);
		pi->app_size_valid += (uint16)length;
		
		if (cur_chip_param.flash_page_num > 1)
		{
			page_size = cur_chip_param.flash_page_size;
		}
		else
		{
			page_size = 256;
		}
		
		ret = MEMLIST_Add(&pi->app_memlist, mem_addr, 
						  length, page_size);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	case 0x0020:
		// EEPROM
		if (NULL == pi->eeprom)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), 
					  "eeprom");
			return ERRCODE_INVALID_BUFFER;
		}
		
		if ((0 == cur_chip_param.eeprom_page_num) 
			|| (0 == cur_chip_param.eeprom_page_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID), "Eeprom", 
					  cur_chip_param.chip_name);
			return ERRCODE_INVALID;
		}
		
		mem_addr += cur_eeprom_offset;
		if ((mem_addr >= cur_chip_param.eeprom_size) || 
			(length > cur_chip_param.eeprom_size) || 
			((mem_addr + length) > cur_chip_param.eeprom_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "eeprom memory");
			return ERRCODE_INVALID;
		}
		memcpy(pi->eeprom + mem_addr, data, length);
		pi->eeprom_size_valid += (uint16)length;
		
		if (cur_chip_param.eeprom_page_num > 1)
		{
			page_size = cur_chip_param.eeprom_page_size;
		}
		else
		{
			page_size = 256;
		}
		
		ret = MEMLIST_Add(&pi->eeprom_memlist, mem_addr, 
						  length, page_size);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_ADDRESS), address, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID;
		break;
	}
	
	return ERROR_OK;
}

RESULT avr8_fini(program_info_t *pi, programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	return ERROR_OK;
}

RESULT avr8_init(program_info_t *pi, const char *dir, programmer_info_t *prog)
{
	uint8 i;
	operation_t opt_tmp = {0};
	
	dir = dir;
	
	if (strcmp(pi->chip_type, CUR_TARGET_STRING))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_HANDLER), CUR_TARGET_STRING, 
				pi->chip_type);
		return ERRCODE_INVALID_HANDLER;
	}
	
	if (NULL == pi->chip_name)
	{
		// auto detect
		LOG_INFO(_GETTEXT(INFOMSG_TRY_AUTODETECT));
		opt_tmp.read_operations = CHIP_ID;
		cur_chip_param.prog_mode = AVR8_PROG_MODE_MASK;
		
		if (ERROR_OK != avr8_program(opt_tmp, pi, prog))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
			return ERRCODE_AUTODETECT_FAIL;
		}
		
		LOG_INFO(_GETTEXT(INFOMSG_AUTODETECT_SIGNATURE), pi->chip_id);
		for (i = 0; i < dimof(cur_chips_param); i++)
		{
			if (pi->chip_id == cur_chips_param[i].signature)
			{
				memcpy(&cur_chip_param, cur_chips_param + i, 
					   sizeof(cur_chip_param));
				cur_chip_param.flash_size = cur_chip_param.flash_page_num 
											* cur_chip_param.flash_page_size;
				cur_chip_param.eeprom_size = cur_chip_param.eeprom_page_num 
											 * cur_chip_param.eeprom_page_size;
				pi->app_size = cur_chip_param.flash_size;
				pi->app_size_valid = 0;
				pi->eeprom_size = cur_chip_param.eeprom_size;
				pi->eeprom_size_valid = 0;
				
				LOG_INFO(_GETTEXT(INFOMSG_CHIP_FOUND), 
						 cur_chip_param.chip_name);
				pi->chip_name = (char *)cur_chip_param.chip_name;
				
				return ERROR_OK;
			}
		}
		
		LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
		return ERRCODE_AUTODETECT_FAIL;
	}
	else
	{
		for (i = 0; i < dimof(cur_chips_param); i++)
		{
			if (!strcmp(cur_chips_param[i].chip_name, pi->chip_name))
			{
				memcpy(&cur_chip_param, cur_chips_param + i, 
					   sizeof(cur_chip_param));
				cur_chip_param.flash_size = cur_chip_param.flash_page_num 
											* cur_chip_param.flash_page_size;
				cur_chip_param.eeprom_size = cur_chip_param.eeprom_page_num 
											 * cur_chip_param.eeprom_page_size;
				
				pi->app_size = cur_chip_param.flash_size;
				pi->app_size_valid = 0;
				pi->eeprom_size = cur_chip_param.eeprom_size;
				pi->eeprom_size_valid = 0;
				
				return ERROR_OK;
			}
		}
		
		return ERROR_FAIL;
	}
}

uint32 avr8_interface_needed(void)
{
	switch (cur_prog_mode)
	{
	case 0:		// default is ISP
	case AVR8_ISP:
		return AVR8_ISP_INTERFACE_NEEDED;
	case AVR8_JTAG:
		return AVR8_JTAG_INTERFACE_NEEDED;
	case AVR8_HVPP:
		return AVR8_HVPP_INTERFACE_NEEDED;
	case AVR8_HVSP:
		return AVR8_HVSP_INTERFACE_NEEDED;
	default:
		// invalid mode
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), cur_prog_mode, 
				  CUR_TARGET_STRING);
		return INVALID_INTERFACE;
	}
}





#define get_target_voltage(v)					prog->get_target_voltage(v)
RESULT avr8_program(operation_t operations, program_info_t *pi, 
					programmer_info_t *prog)
{
	uint16 voltage;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if ((   (operations.read_operations & APPLICATION) 
			&& (NULL == pi->app)) 
		|| ((   (operations.write_operations & APPLICATION) 
				|| (operations.verify_operations & APPLICATION)) 
			&& ((NULL == pi->app) 
				|| (0 == pi->app_size_valid))))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for flash");
		return ERRCODE_INVALID_BUFFER;
	}
	if ((   (operations.read_operations & EEPROM) 
			&& (NULL == pi->eeprom)) 
		|| ((   (operations.write_operations & EEPROM) 
				|| (operations.verify_operations & EEPROM)) 
			&& ((NULL == pi->eeprom) 
				|| (0 == pi->eeprom_size_valid))))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for eeprom");
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	// get target voltage
	if (ERROR_OK != get_target_voltage(&voltage))
	{
		return ERROR_FAIL;
	}
	LOG_DEBUG(_GETTEXT(INFOMSG_TARGET_VOLTAGE), voltage / 1000, 
			  voltage % 1000);
	if (voltage < 2700)
	{
		LOG_WARNING(_GETTEXT(INFOMSG_TARGET_LOW_POWER));
	}
	
	switch (cur_prog_mode)
	{
	case 0:		// default is ISP
		LOG_WARNING(_GETTEXT(INFOMSG_USE_DEFAULT), "Program interface", "ISP");
		cur_prog_mode = AVR8_ISP;
	case AVR8_ISP:
		if (cur_chip_param.prog_mode & AVR8_ISP)
		{
			return avr8_isp_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "ISP", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	case AVR8_JTAG:
		if (cur_chip_param.prog_mode & AVR8_JTAG)
		{
			return avr8_jtag_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "JTAG", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	case AVR8_HVPP:
		if (cur_chip_param.prog_mode & AVR8_HVPP)
		{
			return avr8_hvpp_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "HVPP", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	case AVR8_HVSP:
		if (cur_chip_param.prog_mode & AVR8_HVSP)
		{
			return avr8_hvsp_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "HVSP", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	default:
		// invalid mode
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), cur_prog_mode, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID_PROG_MODE;
	}
}

