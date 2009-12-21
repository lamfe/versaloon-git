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
#define cur_chip_param				target_chip_param
#define cur_chips_param				target_chips.chips_param
#define cur_chips_num				target_chips.num_of_chips
#define cur_flash_offset			avr8_flash_offset
#define cur_eeprom_offset			avr8_eeprom_offset
#define cur_prog_mode				program_mode
#define cur_target_defined			target_defined
#define cur_program_area_map		avr8_program_area_map

const struct program_area_map_t avr8_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, 0},
	{EEPROM_CHAR, 1, 0, 0, 2, 0},
	{FUSE_CHAR, 0, 0, 0, 0, 0},
	{LOCK_CHAR, 0, 0, 0, 0, 0},
	{CALIBRATION_CHAR, 0, 0, 0, 0, 0},
	{0, 0, 0, 0, 0, 0}
};

static uint32_t avr8_flash_offset = 0;
static uint32_t avr8_eeprom_offset = 0;

static uint8_t avr8_lock = AVR8_LOCK_CHAR;
static uint32_t avr8_fuse = AVR8_FUSE_CHAR;


static void avr8_usage(void)
{
	printf("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>              set ISP frequency, in KHz\n\
  -l,  --lock <LOCK>                        set lock\n\
  -f,  --fuse <FUSE>                        set fuse\n\
  -m,  --mode <MODE>                        set mode<b|p>\n\n", 
			CUR_TARGET_STRING);
}

RESULT avr8_parse_argument(char cmd, const char *argu)
{
	switch (cmd)
	{
	case 'h':
		avr8_usage();
		break;
	case 'l':
		// define Lock
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		avr8_lock = (uint8_t)strtoul(argu, NULL, 0);
		cur_target_defined |= LOCK;
		
		break;
	case 'f':
		// define Fuse
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		avr8_fuse = (uint32_t)strtoul(argu, NULL, 0);
		cur_target_defined |= FUSE;
		
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT avr8_prepare_buffer(struct program_info_t *pi)
{
	if (pi->program_areas[APPLICATION_IDX].buff != NULL)
	{
		memset(pi->program_areas[APPLICATION_IDX].buff, AVR8_FLASH_CHAR, 
				pi->program_areas[APPLICATION_IDX].size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	if (pi->program_areas[EEPROM_IDX].buff != NULL)
	{
		memset(pi->program_areas[EEPROM_IDX].buff, AVR8_EEPROM_CHAR, 
				pi->program_areas[EEPROM_IDX].size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	pi->program_areas[LOCK_IDX].value = avr8_lock;
	pi->program_areas[FUSE_IDX].value = avr8_fuse;
	
	return ERROR_OK;
}

RESULT avr8_write_buffer_from_file_callback(uint32_t address, 
			uint32_t seg_addr, uint8_t* data, uint32_t length, void* buffer)
{
	struct program_info_t *pi = (struct program_info_t *)buffer;
	uint32_t mem_addr = address & 0x0001FFFF, page_size;
	RESULT ret;
	uint8_t *tbuff;
	struct chip_area_info_t *areas;

#ifdef PARAM_CHECK
	if ((length > 0) && (NULL == data))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	areas = cur_chip_param.chip_areas;
	switch (seg_addr)
	{
	case 0x0010:
		mem_addr += 0x00010000;
	case 0x0000:
		// Flash
		tbuff = pi->program_areas[APPLICATION_IDX].buff;
		if (NULL == tbuff)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "pi->app");
			return ERRCODE_INVALID_BUFFER;
		}
		
		if ((0 == areas[APPLICATION_IDX].page_num) 
			|| (0 == areas[APPLICATION_IDX].page_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID), "Flash", 
					  cur_chip_param.chip_name);
			return ERRCODE_INVALID;
		}
		
		mem_addr += cur_flash_offset;
		if ((mem_addr >= areas[APPLICATION_IDX].size) 
			|| (length > areas[APPLICATION_IDX].size) 
			|| ((mem_addr + length) > areas[APPLICATION_IDX].size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "flash memory");
			return ERRCODE_INVALID;
		}
		cur_target_defined |= APPLICATION;
		
		memcpy(tbuff + mem_addr, data, length);
		
		if (areas[APPLICATION_IDX].page_num > 1)
		{
			page_size = areas[APPLICATION_IDX].page_size;
		}
		else
		{
			page_size = 256;
		}
		
		ret = MEMLIST_Add(&pi->program_areas[APPLICATION_IDX].memlist, 
								mem_addr, length, page_size);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	case 0x0002:
		// EEPROM
		tbuff = pi->program_areas[EEPROM_IDX].buff;
		if (NULL == tbuff)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), 
					  "eeprom");
			return ERRCODE_INVALID_BUFFER;
		}
		
		if ((0 == areas[EEPROM_IDX].page_num) 
			|| (0 == areas[EEPROM_IDX].page_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID), "Eeprom", 
					  cur_chip_param.chip_name);
			return ERRCODE_INVALID;
		}
		
		mem_addr += cur_eeprom_offset;
		if ((mem_addr >= areas[EEPROM_IDX].size) || 
			(length > areas[EEPROM_IDX].size) || 
			((mem_addr + length) > areas[EEPROM_IDX].size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "eeprom memory");
			return ERRCODE_INVALID;
		}
		cur_target_defined |= EEPROM;
		
		memcpy(tbuff + mem_addr, data, length);
		
		if (areas[EEPROM_IDX].page_num > 1)
		{
			page_size = areas[EEPROM_IDX].page_size;
		}
		else
		{
			page_size = 256;
		}
		
		ret = MEMLIST_Add(&pi->program_areas[EEPROM_IDX].memlist, mem_addr, 
							length, page_size);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_ADDRESS), seg_addr, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID;
		break;
	}
	
	return ERROR_OK;
}

RESULT avr8_fini(struct program_info_t *pi, struct programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	return ERROR_OK;
}

RESULT avr8_init(struct program_info_t *pi, struct programmer_info_t *prog)
{
	uint8_t i;
	struct operation_t opt_tmp;
	
	memset(&opt_tmp, 0, sizeof(opt_tmp));
	
	if (strcmp(pi->chip_type, CUR_TARGET_STRING))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_HANDLER), CUR_TARGET_STRING, 
				pi->chip_type);
		return ERRCODE_INVALID_HANDLER;
	}
	// define read only targets
	cur_target_defined |= CALIBRATION;
	
	if (NULL == pi->chip_name)
	{
		// auto detect
		LOG_INFO(_GETTEXT(INFOMSG_TRY_AUTODETECT));
		opt_tmp.read_operations = CHIPID;
		cur_chip_param.program_mode = AVR8_PROG_MODE_MASK;
		
		if (ERROR_OK != avr8_program(opt_tmp, pi, prog))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
			return ERRCODE_AUTODETECT_FAIL;
		}
		
		LOG_INFO(_GETTEXT(INFOMSG_AUTODETECT_SIGNATURE), pi->chip_id);
		for (i = 0; i < cur_chips_num; i++)
		{
			if (pi->chip_id == cur_chips_param[i].chip_id)
			{
				pi->chip_name = (char *)cur_chips_param[i].chip_name;
				LOG_INFO(_GETTEXT(INFOMSG_CHIP_FOUND), pi->chip_name);
				
				goto Post_Init;
			}
		}
		
		LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
		return ERRCODE_AUTODETECT_FAIL;
	}
	else
	{
		for (i = 0; i < cur_chips_num; i++)
		{
			if (!strcmp(cur_chips_param[i].chip_name, pi->chip_name))
			{
				goto Post_Init;
			}
		}
		
		return ERROR_FAIL;
	}
Post_Init:
	memcpy(&cur_chip_param, cur_chips_param + i, 
		   sizeof(cur_chip_param));
	
	pi->program_areas[APPLICATION_IDX].size = 
				cur_chip_param.chip_areas[APPLICATION_IDX].size;
	pi->program_areas[EEPROM_IDX].size = 
				cur_chip_param.chip_areas[EEPROM_IDX].size;
	
	return ERROR_OK;
}

uint32_t avr8_interface_needed(void)
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
RESULT avr8_program(struct operation_t operations, struct program_info_t *pi, 
					struct programmer_info_t *prog)
{
	uint16_t voltage;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if ((   (operations.read_operations & APPLICATION) 
			&& (NULL == pi->program_areas[APPLICATION_IDX].buff)) 
		|| ((   (operations.write_operations & APPLICATION) 
				|| (operations.verify_operations & APPLICATION)) 
			&& (NULL == pi->program_areas[APPLICATION_IDX].buff)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for flash");
		return ERRCODE_INVALID_BUFFER;
	}
	if ((   (operations.read_operations & EEPROM) 
			&& (NULL == pi->program_areas[EEPROM_IDX].buff)) 
		|| ((   (operations.write_operations & EEPROM) 
				|| (operations.verify_operations & EEPROM)) 
			&& (NULL == pi->program_areas[EEPROM_IDX].buff)))
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
	LOG_DEBUG(_GETTEXT(INFOMSG_TARGET_VOLTAGE), voltage / 1000.0);
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
		if (cur_chip_param.program_mode & AVR8_ISP)
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
		if (cur_chip_param.program_mode & AVR8_JTAG)
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
		if (cur_chip_param.program_mode & AVR8_HVPP)
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
		if (cur_chip_param.program_mode & AVR8_HVSP)
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

