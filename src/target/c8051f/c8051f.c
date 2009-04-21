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

#include "c8051f.h"
#include "c8051f_internal.h"

#define CUR_TARGET_STRING			C8051F_STRING
#define cur_chip_param				c8051f_chip_param
#define cur_chips_param				c8051f_chips_param
#define cur_flash_offset			c8051f_flash_offset
#define cur_prog_mode				c8051f_prog_mode

const program_area_map_t c8051f_program_area_map[] = 
{
	{APPLICATION, 'f', 1},
	{0, 0}
};

const c8051f_param_t c8051f_chips_param[] = {
//	chip_name,				chipid,		flash_page_size	flash_page_num,	flash_size,	interface,	jtag_param{erase_addr},	c2_param{fpctl_addr,fpdat_addr}
	{"c8051f30x",			0x00000004,	0,				0,				8 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	{"c8051f31x",			0x00000008,	0,				0,				16 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	{"c8051f32xL",			0x00000009,	0,				0,				16 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	{"c8051f32xH",			0x0000000D,	0,				0,				16 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	{"c8051f33xL",			0x0000000A,	0,				0,				8 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	{"c8051f33xH",			0x00000014,	0,				0,				16 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	{"c8051f34x",			0x0000000F,	0,				0,				64 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xAD}},
	{"c8051f35x",			0x0000000B,	0,				0,				8 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	{"c8051f36x",			0x00000012,	0,				0,				32 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xBF}},
	{"c8051f41x",			0x0000000C,	0,				0,				32 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xBF}},
	{"c8051f50x",			0x0000001C,	0,				0,				64 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	{"c8051f52/3x",			0x00000011,	0,				0,				8 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	{"c8051f92/3x",			0x00000016,	0,				0,				64 * 1024,	C8051F_C2,	{0x0000},				{0x02,		0xB4}},
	//	chip_name,			chipid,		flash_page_size	flash_page_num,	flash_size,	interface,	erase_addr
	{"c8051f00x/2xx",		0x00000243,	512,			128,			64 * 1024,	C8051F_JTAG,{0xfdff},				{0x00,		0x00}},
	{"c8051f01x",			0x00002243,	512,			128,			64 * 1024,	C8051F_JTAG,{0xfdff},				{0x00,		0x00}},
	{"c8051f02x/12x/13x",	0x00003243,	512,			256,			128 * 1024,	C8051F_JTAG,{0xfdff},				{0x00,		0x00}},
	{"c8051f04x",			0x00005243,	512,			128,			64 * 1024,	C8051F_JTAG,{0xfdff},				{0x00,		0x00}},
	{"c8051f06x",			0x00006243,	512,			128,			64 * 1024,	C8051F_JTAG,{0xfdff},				{0x00,		0x00}},
};

static uint8 c8051f_prog_mode = 0;

c8051f_param_t c8051f_chip_param;
static uint32 c8051f_flash_offset = 0;

static void c8051f_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                set mode<j|c>\n\n", CUR_TARGET_STRING);
}

static void c8051f_support(void)
{
	uint32 i;
	
	printf("Support list of %s:\n", CUR_TARGET_STRING);
	for (i = 0; i < dimof(cur_chips_param); i++)
	{
		printf("%s: id = 0x%02x, prog_mode = 0x%02x\n", 
				cur_chips_param[i].chip_name, 
				cur_chips_param[i].chip_id,
				cur_chips_param[i].interface);
	}
	printf("\n");
}

RESULT c8051f_parse_argument(char cmd, const char *argu)
{
	switch (cmd)
	{
	case 'h':
		c8051f_usage();
		break;
	case 'S':
		c8051f_support();
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
		case 'c':
			// C2 mode
			cur_prog_mode = C8051F_C2;
			break;
		case 'j':
			// JTAG mode
			cur_prog_mode = C8051F_JTAG;
			break;
		default:
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHARACTER_MESSAGE), cmd, 
					  "c8051f program mode", "MUST be 'c' or 'j'!!");
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

RESULT c8051f_probe_chip(char *chip_name)
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

RESULT c8051f_prepare_buffer(program_info_t *pi)
{
	if (pi->app != NULL)
	{
		memset(pi->app, C8051F_FLASH_CHAR, pi->app_size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT c8051f_write_buffer_from_file_callback(uint32 address, uint32 seg_addr, 
											  uint8* data, uint32 length, 
											  void* buffer)
{
	program_info_t *pi = (program_info_t *)buffer;
	uint32 mem_addr = address & 0x0000FFFF;
	RESULT ret;
	
#ifdef PARAM_CHECK
	if ((length > 0) && (NULL == data))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (seg_addr != 0)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), 
				  "segment address", CUR_TARGET_STRING);
		return ERRCODE_NOT_SUPPORT;
	}
	
	// flash from 0x00000000
	switch (address >> 16)
	{
	case 0x0000:
		if (NULL == pi->app)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "pi->app");
			return ERRCODE_INVALID_BUFFER;
		}
		
/*		if ((0 == cur_chip_param.flash_page_num) 
			|| (0 == cur_chip_param.flash_page_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID), "Flash", 
					  cur_chip_param.chip_name);
			return ERRCODE_INVALID;
		}
*/		
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
		
		ret = MEMLIST_Add(&pi->app_memlist, mem_addr, length, C8051F_BLOCK_SIZE);
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

RESULT c8051f_fini(program_info_t *pi, programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	return ERROR_OK;
}

RESULT c8051f_init(program_info_t *pi, const char *dir, 
				   programmer_info_t *prog)
{
	uint32 i;
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
		
		cur_chip_param.interface = cur_prog_mode;
		if (ERROR_OK != c8051f_program(opt_tmp, pi, prog))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
			return ERRCODE_AUTODETECT_FAIL;
		}
		
		LOG_INFO(_GETTEXT(INFOMSG_AUTODETECT_SIGNATURE), pi->chip_id);
		for (i = 0; i < dimof(cur_chips_param); i++)
		{
			if (pi->chip_id == cur_chips_param[i].chip_id)
			{
				memcpy(&cur_chip_param, cur_chips_param + i, 
					   sizeof(cur_chip_param));
				pi->app_size = cur_chip_param.flash_size;
				
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
				pi->app_size = cur_chip_param.flash_size;
				
				return ERROR_OK;
			}
		}
		
		return ERROR_FAIL;
	}
}

uint32 c8051f_interface_needed(void)
{
	switch (cur_prog_mode)
	{
	case 0:		// default is C2
	case C8051F_JTAG:
		return C8051F_JTAG_INTERFACE_NEEDED;
	case C8051F_C2:
		return C8051F_C2_INTERFACE_NEEDED;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), cur_prog_mode, 
				  CUR_TARGET_STRING);
		return INVALID_INTERFACE;
	}
}





#define get_target_voltage(v)					prog->get_target_voltage(v)
RESULT c8051f_program(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog)
{
	uint16 voltage;

#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if ((   (operations.write_operations & APPLICATION) 
			|| (operations.verify_operations & APPLICATION)) 
		&& ((NULL == pi->app) 
			|| (0 == pi->app_size_valid)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for flash");
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
		LOG_WARNING(_GETTEXT(INFOMSG_USE_DEFAULT), "Promgram interface", "C2");
		cur_prog_mode = C8051F_C2;
	case C8051F_C2:
		if (cur_chip_param.interface & C8051F_C2)
		{
			return c8051f_c2_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "C2", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
	case C8051F_JTAG:
		if (cur_chip_param.interface & C8051F_JTAG)
		{
			return c8051f_jtag_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "JTAG", 
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

