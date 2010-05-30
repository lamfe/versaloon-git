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
#include <ctype.h>
#include <inttypes.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "memlist.h"
#include "pgbar.h"
#include <libxml/parser.h>

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "target.h"
#include "at89s5x/at89s5x.h"
#include "psoc1/psoc1.h"
#include "lpc900/lpc900.h"
#include "msp430/msp430.h"
#include "c8051f/c8051f.h"
#include "avr8/avr8.h"
#include "comisp/comisp.h"
#include "svf_player/svf_player.h"
#include "cortex-m3/cm3.h"
#include "stm32/stm32.h"
#include "lpc1000/lpc1000.h"
#include "stm8/stm8.h"
#include "at91sam3/at91sam3.h"
#include "avr32/avr32.h"

const struct target_area_name_t target_area_name[NUM_OF_TARGET_AREA] = 
{
	{CHIPID_CHAR,				CHIPID,				"chipid"},
	{CHIPID_CHKSUM_CHAR,		CHIPID_CHKSUM,		"chipid_checksum"},
	{BOOTLOADER_CHAR,			BOOTLOADER,			"bootloader"},
	{BOOTLOADER_CHKSUM_CHAR,	BOOTLOADER_CHKSUM,	"bootloader_checksum"},
	{APPLICATION_CHAR,			APPLICATION,		"flash"},
	{APPLICATION_CHKSUM_CHAR,	APPLICATION_CHKSUM,	"flash_checksum"},
	{EEPROM_CHAR,				EEPROM,				"eeprom"},
	{EEPROM_CHKSUM_CHAR,		EEPROM_CHKSUM,		"eeprom_checksum"},
	{OTPROM_CHAR,				OTPROM,				"otprom"},
	{OTPROM_CHKSUM_CHAR,		OTPROM_CHKSUM,		"otprom_checksum"},
	{FUSE_CHAR,					FUSE,				"fuse"},
	{FUSE_CHKSUM_CHAR,			FUSE_CHKSUM,		"fuse_checksum"},
	{LOCK_CHAR,					LOCK,				"lock"},
	{LOCK_CHKSUM_CHAR,			LOCK_CHKSUM,		"lock_checksum"},
	{USRSIG_CHAR,				USRSIG,				"usrsig"},
	{USRSIG_CHKSUM_CHAR,		USRSIG_CHKSUM,		"usrsig_checksum"},
	{CALIBRATION_CHAR,			CALIBRATION,		"calibration"},
	{CALIBRATION_CHKSUM_CHAR,	CALIBRATION_CHKSUM	,"calibration_checksum"}
};

struct chip_series_t target_chips = {0, NULL};
struct chip_param_t target_chip_param;

struct target_info_t targets_info[] = 
{
	// stm32
	{
		STM32_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		stm32_program_area_map,				// program_area_map
		stm32_program_mode,					// program_mode
		&stm32_program_functions,			// program_functions
		stm32_parse_argument,				// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// lpc1000
	{
		LPC1000_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		lpc1000_program_area_map,			// program_area_map
		lpc1000_program_mode,				// program_mode
		&lpc1000_program_functions,			// program_functions
		lpc1000_parse_argument,				// parse_argument
		lpc1000_adjust_setting,				// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// at91sam3
	{
		AT91SAM3_STRING,					// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		at91sam3_program_area_map,			// program_area_map
		at91sam3_program_mode,				// program_mode
		&at91sam3_program_functions,		// program_functions
		at91sam3_parse_argument,			// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// avr32
	{
		AVR32_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		avr32_program_area_map,				// program_area_map
		avr32_program_mode,					// program_mode
		&avr32_program_functions,			// program_functions
		avr32_parse_argument,				// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// stm8
	{
		STM8_STRING,						// name
		"",									// feature
		stm8_program_area_map,				// program_area_map
		stm8_program_mode,					// program_mode
		&stm8_program_functions,			// program_functions
		stm8_parse_argument,				// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// at89s5x
	{
		S5X_STRING,							// name
		AUTO_DETECT,						// feature
		s5x_program_area_map,				// program_area_map
		s5x_program_mode,					// program_mode
		&s5x_program_functions,				// program_functions
		s5x_parse_argument,					// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// psoc
	{
		PSOC1_STRING,						// name
		AUTO_DETECT,						// feature
		psoc1_program_area_map,				// program_area_map
		psoc1_program_mode,					// program_mode
		&psoc1_program_functions,			// program_functions
		psoc1_parse_argument,				// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// msp430
	{
		MSP430_STRING,						// name
		AUTO_DETECT,						// feature
		msp430_program_area_map,			// program_area_map
		msp430_program_mode,				// program_mode
		&msp430_program_functions,			// program_functions
		msp430_parse_argument,				// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// c8051f
	{
		C8051F_STRING,						// name
		AUTO_DETECT,						// feature
		c8051f_program_area_map,			// program_area_map
		c8051f_program_mode,				// program_mode
		&c8051f_program_functions,			// program_functions
		c8051f_parse_argument,				// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// avr8
	{
		AVR8_STRING,						// name
		AUTO_DETECT,						// feature
		avr8_program_area_map,				// program_area_map
		avr8_program_mode,					// program_mode
		&avr8_program_functions,			// program_functions
		avr8_parse_argument,				// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// svf_player
	{
		SVFP_STRING,						// name
		NO_TARGET,							// feature
		svfp_program_area_map,				// program_area_map
		svfp_program_mode,					// program_mode
		&svfp_program_functions,			// program_functions
		svfp_parse_argument,				// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// lpc900
	{
		LPC900_STRING,						// name
		AUTO_DETECT,						// feature
		lpc900_program_area_map,			// program_area_map
		lpc900_program_mode,				// program_mode
		&lpc900_program_functions,			// program_functions
		lpc900_parse_argument,				// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	{
		NULL,								// name
		0,									// areas
		NULL,								// program_area_map
		NULL,								// program_mode
		NULL,								// program_functions
		NULL,								// parse_argument
		NULL,								// adjust_setting
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	}
};
struct target_info_t *cur_target = NULL;
struct program_info_t program_info;

RESULT target_alloc_data_buffer(void)
{
	uint8_t i;
	
	for (i = 0; i < dimof(program_info.program_areas); i++)
	{
		if ((NULL == program_info.program_areas[i].buff) 
			&& (program_info.program_areas[i].size > 0))
		{
			program_info.program_areas[i].buff = 
				(uint8_t *)malloc(program_info.program_areas[i].size);
			if (NULL == program_info.program_areas[i].buff)
			{
				return ERRCODE_NOT_ENOUGH_MEMORY;
			}
			if (strlen(target_chip_param.chip_name) > 0)
			{
				memset(program_info.program_areas[i].buff, 
						(uint8_t)target_chip_param.chip_areas[i].default_value, 
						program_info.program_areas[i].size);
			}
		}
		if ((NULL == program_info.program_areas[i].checksum_buff) 
			&& (program_info.program_areas[i].checksum_size > 0))
		{
			program_info.program_areas[i].checksum_buff = 
				(uint8_t *)malloc(program_info.program_areas[i].checksum_size);
			if (NULL == program_info.program_areas[i].checksum_buff)
			{
				return ERRCODE_NOT_ENOUGH_MEMORY;
			}
		}
	}
	
	return ERROR_OK;
}

void target_free_data_buffer(void)
{
	uint8_t i;
	struct program_area_t *area;
	
	target_release_chip_series(&target_chips);
	
	for (i = 0; i < dimof(program_info.program_areas); i++)
	{
		area = &program_info.program_areas[i];
		if (area->buff != NULL)
		{
			free(area->buff);
			area->buff = NULL;
			area->size = 0;
			area->value = 0;
		}
		if (area->memlist != NULL)
		{
			MEMLIST_Free(&area->memlist);
		}
		if (area->checksum_buff != NULL)
		{
			free(area->checksum_buff);
			area->checksum_buff = NULL;
			area->checksum_size = 0;
			area->checksum_value = 0;
		}
		if (area->checksum_memlist != NULL)
		{
			MEMLIST_Free(&area->checksum_memlist);
		}
	}
}

void target_get_target_area(char area, uint8_t **buff, uint32_t *size)
{
	int8_t i;
	
	i = target_area_idx(area);
	if (i >= 0)
	{
		*buff = program_info.program_areas[i].buff;
		*size = program_info.program_areas[i].size;
	}
	else
	{
		*buff = NULL;
		*size = 0;
	}
}

char target_area_char_by_fullname(char *fullname)
{
	uint32_t i;
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		if (!strcmp(target_area_name[i].full_name, fullname))
		{
			return target_area_name[i].name;
		}
	}
	return '\0';
}

int8_t target_area_idx_by_mask(uint32_t mask)
{
	int8_t i;
	
	for (i = 0; i < (int8_t)dimof(target_area_name); i++)
	{
		if (target_area_name[i].mask == mask)
		{
			return i;
		}
	}
	return -1;
}

char* target_area_fullname_by_mask(uint32_t mask)
{
	uint32_t i;
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		if (target_area_name[i].mask == mask)
		{
			return (char *)target_area_name[i].full_name;
		}
	}
	return NULL;
}

int8_t target_area_idx(char area_name)
{
	int8_t i;
	
	for (i = 0; i < (int8_t)dimof(target_area_name); i++)
	{
		if (target_area_name[i].name == area_name)
		{
			return i;
		}
	}
	return -1;
}

char* target_area_fullname(char area_name)
{
	int8_t i;
	
	i = target_area_idx(area_name);
	if (i < 0)
	{
		return NULL;
	}
	else
	{
		return (char *)target_area_name[i].full_name;
	}
}

uint32_t target_area_mask(char area_name)
{
	int8_t i;
	
	i = target_area_idx(area_name);
	if (i < 0)
	{
		return 0;
	}
	else
	{
		return target_area_name[i].mask;
	}
}

static RESULT target_check_single_defined(uint32_t opt)
{
	uint8_t i;
	
	opt = (program_info.areas_defined ^ opt) & opt;
	
	for (i = 0; i < 32; i++)
	{
		if (opt & (1 << i))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), 
						target_area_fullname_by_mask(1 << i));
			return ERROR_FAIL;
		}
	}
	return ERROR_OK;
}

int8_t target_mode_get_idx(const struct program_mode_t *mode, char mode_name)
{
	int8_t i;
	
	if (NULL == mode)
	{
		return -1;
	}
	
	i = 0;
	while (mode[i].name != 0)
	{
		if (mode[i].name == mode_name)
		{
			return i;
		}
		i++;
	}
	return -1;
}

RESULT target_check_defined(struct operation_t operations)
{
	if (ERROR_OK != target_check_single_defined(operations.verify_operations) 
		|| (ERROR_OK 
			!= target_check_single_defined(operations.write_operations)))
	{
		return ERROR_FAIL;
	}
	else
	{
		return ERROR_OK;
	}
}

RESULT target_write_buffer_from_file_callback(uint32_t address, 
			uint32_t seg_addr, uint8_t* data, uint32_t length, void* buffer)
{
	uint32_t i, j;
	int8_t area_idx;
	char area_name;
	uint8_t *area_buff;
	struct memlist **area_memlist;
	uint32_t area_seg, area_addr, area_size, area_page_size;
	uint64_t *area_value;
	struct program_info_t *pi = (struct program_info_t *)buffer;
	uint32_t mem_addr;
	RESULT ret;
	
	if ((NULL == cur_target) || (0 == strlen(target_chip_param.chip_name)))
	{
		LOG_BUG("target should be initialized first.\n");
		return ERROR_FAIL;
	}
	
	// find a right target to fill the memory
	i = 0;
	while (cur_target->program_area_map[i].name != 0)
	{
		area_name = cur_target->program_area_map[i].name;
		area_idx = target_area_idx(area_name);
		if (area_idx < 0)
		{
			i++;
			continue;
		}
		area_seg = target_chip_param.chip_areas[area_idx].seg 
						+ cur_target->program_area_map[i].fseg_addr;
		area_addr = target_chip_param.chip_areas[area_idx].addr 
						+ cur_target->program_area_map[i].fstart_addr;
		area_size = target_chip_param.chip_areas[area_idx].size;
		area_page_size = target_chip_param.chip_areas[area_idx].page_size;
		if (islower(area_name))
		{
			area_buff = pi->program_areas[area_idx].buff;
			area_value = &(pi->program_areas[area_idx].value);
			area_memlist = &(pi->program_areas[area_idx].memlist);
		}
		else
		{
			area_buff = pi->program_areas[area_idx].checksum_buff;
			area_value = &(pi->program_areas[area_idx].checksum_value);
			area_memlist = &(pi->program_areas[area_idx].checksum_memlist);
		}
		
		if ((area_seg != seg_addr) || (area_addr > address) 
			|| ((area_addr + area_size) < (address + length)))
		{
			// not this area
			i++;
			continue;
		}
		
		// found
		if (0 == area_page_size)
		{
			// default page size is 256 bytes
			area_page_size = 256;
		}
		pi->areas_defined |= target_area_mask(area_name);
		mem_addr = address - area_addr;
		if (area_buff != NULL)
		{
			// put in area_buff
			memcpy(area_buff + mem_addr, data, length);
			ret = MEMLIST_Add(area_memlist, address, length, area_page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							"add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		else if (area_size <= 8)
		{
			// put in area_value
			*area_value = 0;
			for (j = 0; j < length; j++)
			{
				*area_value += data[j] << (j * 8);
			}
		}
		
		return ERROR_OK;
	}
	
	// not found
	return ERROR_FAIL;
}

static const uint64_t byte_mask[9] = {
	0x0000000000000000,
	0x00000000000000FF,
	0x000000000000FFFF,
	0x0000000000FFFFFF,
	0x00000000FFFFFFFF,
	0x000000FFFFFFFFFF,
	0x0000FFFFFFFFFFFF,
	0x00FFFFFFFFFFFFFF,
	0xFFFFFFFFFFFFFFFF,
};
RESULT target_program(struct program_context_t *context)
{
	const struct program_functions_t *pf = cur_target->program_functions;
	const struct program_area_map_t *p_map = cur_target->program_area_map;
	struct program_info_t *pi = context->pi;
	struct operation_t *op = context->op;
	struct chip_param_t *param = context->param;
	
	struct chip_area_info_t *area_info;
	struct program_area_t *prog_area;
	RESULT ret = ERROR_OK;
	uint32_t i, j;
	int8_t area_idx;
	char area_char;
	uint32_t area_mask;
	enum area_attr_t area_attr;
	uint32_t target_size, page_size, start_addr;
	uint8_t *tbuff;
	char *fullname, str_tmp[256];
	struct memlist **ml, *ml_tmp;
	uint64_t value;
	uint32_t time_in_ms = 1000;
	
	// check mode
	if ((target_chips.num_of_chips > 0) 
		&& (target_chips.chips_param[0].program_mode != 0) 
		&& !(param->program_mode & (1 << pi->mode)))
	{
		LOG_ERROR(_GETTEXT("current mode is not supported by %s\n"), 
					pi->chip_name);
		return ERROR_FAIL;
	}
	
	if ((pf->enter_program_mode != NULL) 
		&&(pf->enter_program_mode(context) != ERROR_OK))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "enter program mode");
		ret = ERRCODE_FAILURE_ENTER_PROG_MODE;
		goto leave_program_mode;
	}
	
	if (pf->execute != NULL)
	{
		ret = pf->execute(context);
		goto leave_program_mode;
	}
	
	// read chip id
	pi->chip_id = 0;
	if (ERROR_OK != pf->read_target(context, CHIPID_CHAR, 0, 
									(uint8_t *)&pi->chip_id, 0))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read chip id");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	LOG_INFO(_GETTEXT(INFOMSG_TARGET_CHIP_ID), pi->chip_id);
	if (!(op->read_operations & CHIPID))
	{
		if (pi->chip_id != param->chip_id)
		{
			LOG_WARNING(_GETTEXT(ERRMSG_INVALID_CHIP_ID), pi->chip_id, 
						param->chip_id);
		}
	}
	else
	{
		goto leave_program_mode;
	}
	
	// chip erase
	if (op->erase_operations && param->chip_erase)
	{
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "chip");
		pgbar_init("erasing chip |", "|", 0, 1, PROGRESS_STEP, 
						PROGRESS_CHAR);
		
		if (ERROR_OK != pf->erase_target(context, '*', 0, 0))
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "chip");
	}
	
	// erase, program, verify/read cycle
	i = 0;
	while (p_map[i].name != 0)
	{
		area_char = p_map[i].name;
		area_attr = p_map[i].attr;
		area_idx = target_area_idx(area_char);
		area_mask = target_area_mask(area_char);
		fullname = target_area_fullname(area_char);
		if (area_idx < 0)
		{
			// invalid area
			continue;
		}
		
		area_info = &(param->chip_areas[area_idx]);
		page_size = area_info->page_size;
		start_addr = area_info->addr;
		if ((p_map[i].fpage_size > page_size) 
			&& ((p_map[i].fpage_size % page_size) == 0))
		{
			page_size = p_map[i].fpage_size;
		}
		
		prog_area = &(pi->program_areas[area_idx]);
		if (p_map[i].data_pos)
		{
			ml = &(prog_area->memlist);
			target_size = MEMLIST_CalcAllSize(*ml);
			tbuff = prog_area->buff;
		}
		else
		{
			ml = NULL;
			tbuff = NULL;
			target_size = area_info->size;
		}
		
		// not chip_erase, required to be erased, erasable 
		if (!param->chip_erase && (op->erase_operations & area_mask) 
			&& (area_attr & AREA_ATTR_E))
		{
			// target erase
			LOG_INFO(_GETTEXT(INFOMSG_ERASING), fullname);
			strcpy(str_tmp, "erasing ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, area_info->page_num, PROGRESS_STEP, 
							PROGRESS_CHAR);
			
			if (area_attr & AREA_ATTR_EP)
			{
				// erase every page
				for (j = 0; j < area_info->page_num; j++)
				{
					if (ERROR_OK != pf->erase_target(context, area_char, 
							start_addr + j * page_size, page_size))
					{
						pgbar_fini();
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_ERASE), fullname);
						ret = ERRCODE_FAILURE_OPERATION;
						goto leave_program_mode;
					}
					pgbar_update(1);
				}
			}
			else
			{
				// erase all in one run
				if (ERROR_OK != pf->erase_target(context, area_char, 
													start_addr, 0))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_ERASE), fullname);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				pgbar_update(area_info->page_num);
			}
			
			pgbar_fini();
			LOG_INFO(_GETTEXT(INFOMSG_ERASED), fullname);
			
			// Reset After Erase
			if ((area_attr & AREA_ATTR_RAE) 
				&& ((op->checksum_operations != 0) 
					|| (op->read_operations != 0) 
					|| (op->verify_operations != 0) 
					|| (op->write_operations != 0)))
			{
				if ((pf->leave_program_mode != NULL) 
					&& (ERROR_OK != pf->leave_program_mode(context, 0)))
				{
					return ERRCODE_FAILURE_OPERATION;
				}
				sleep_ms(100);
				if ((pf->enter_program_mode != NULL) 
					&& (ERROR_OK != pf->enter_program_mode(context)))
				{
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
			}
		}
		
		// required to program, writable
		if ((op->write_operations & area_mask) && (area_attr & AREA_ATTR_W))
		{
			LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), fullname);
			strcpy(str_tmp, "writing ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, target_size, PROGRESS_STEP, '=');
			
			if ((tbuff != NULL) && (ml != NULL))
			{
				ml_tmp = *ml;
				while(ml_tmp != NULL)
				{
					if (area_attr & AREA_ATTR_WNP)
					{
						int32_t tmp_addr = ml_tmp->addr;
						uint8_t *tmp_buf = &(tbuff[tmp_addr - start_addr]);
						
						if (ERROR_OK != pf->write_target(context, area_char, 
								tmp_addr, tmp_buf, target_size))
						{
							pgbar_fini();
							LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_PROGRAM), 
										fullname);
							ret = ERRCODE_FAILURE_OPERATION;
							goto leave_program_mode;
						}
								
					}
					else
					{
						for (j = 0; j < ml_tmp->len; j += page_size)
						{
							uint32_t tmp_addr = ml_tmp->addr + j;
							uint8_t *tmp_buf = &(tbuff[tmp_addr - start_addr]);
							
							if (ERROR_OK != pf->write_target(context, 
									area_char, tmp_addr, tmp_buf, page_size))
							{
								pgbar_fini();
								LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_PROGRAM), 
											fullname);
								ret = ERRCODE_FAILURE_OPERATION;
								goto leave_program_mode;
							}
							pgbar_update(page_size);
						}
					}
					ml_tmp = MEMLIST_GetNext(ml_tmp);
				}
			}
			else
			{
				if (ERROR_OK != pf->write_target(context, area_char, 
											start_addr, NULL, target_size))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_PROGRAM), fullname);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				pgbar_update(target_size);
			}
			
			time_in_ms = pgbar_fini();
			LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), fullname, target_size, 
						(target_size / 1024.0) / (time_in_ms / 1000.0));
		}
		
		if ((op->verify_operations & area_mask) 
			&& (area_attr & AREA_ATTR_V))
		{
			// specific verify defined by target
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), fullname);
			strcpy(str_tmp, "verifying ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, target_size, PROGRESS_STEP, '=');
			
			if ((page_size == 0) || (area_attr & AREA_ATTR_RNP))
			{
				// verify whole target area
				if (ERROR_OK != pf->read_target(context, area_char, 
										start_addr, tbuff, target_size))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_VERIFY), fullname);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				pgbar_update(target_size);
			}
			else if ((tbuff != NULL) && (ml != NULL))
			{
				// verify target area page by page
				ml_tmp = *ml;
				while(ml_tmp != NULL)
				{
					for (j = 0; j < ml_tmp->len; j += page_size)
					{
						uint32_t tmp_addr = ml_tmp->addr + j;
						uint8_t *tmp_buf = &(tbuff[tmp_addr - start_addr]);
						
						if (ERROR_OK != pf->read_target(context, area_char, 
								tmp_addr, tmp_buf, page_size))
						{
							pgbar_fini();
							LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_READ), fullname);
							ret = ERRCODE_FAILURE_OPERATION;
							goto leave_program_mode;
						}
						pgbar_update(page_size);
					}
					ml_tmp = MEMLIST_GetNext(ml_tmp);
				}
			}
			
			pgbar_fini();
		}
		else if (((op->read_operations & area_mask) 
					|| (op->verify_operations & area_mask))
				&& (area_attr && AREA_ATTR_R))
		{
			if ((p_map[i].data_pos) && (op->read_operations & area_mask))
			{
				if (ERROR_OK != MEMLIST_Add(ml, area_info->addr, 
											area_info->size, page_size))
				{
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								"add memory list");
					return ERRCODE_FAILURE_OPERATION;
				}
				target_size = MEMLIST_CalcAllSize(*ml);
			}
			
			if (op->verify_operations & area_mask)
			{
				LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), fullname);
			}
			else
			{
				LOG_INFO(_GETTEXT(INFOMSG_READING), fullname);
			}
			strcpy(str_tmp, "reading ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, target_size, PROGRESS_STEP, '=');
			
			if ((tbuff != NULL) && (ml != NULL))
			{
				ml_tmp = *ml;
				while(ml_tmp != NULL)
				{
					uint32_t read_offset = ml_tmp->addr - start_addr;
					uint8_t *read_buf = (uint8_t*)malloc(ml_tmp->len);
					if (NULL == read_buf)
					{
						LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
						ret = ERRCODE_NOT_ENOUGH_MEMORY;
						goto leave_program_mode;
					}
					
					if (area_attr & AREA_ATTR_RNP)
					{
						if (ERROR_OK != pf->read_target(context, area_char, 
								ml_tmp->addr, read_buf, ml_tmp->len))
						{
							free(read_buf);
							read_buf = NULL;
							pgbar_fini();
							LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_READ), fullname);
							ret = ERRCODE_FAILURE_OPERATION;
							goto leave_program_mode;
						}
					}
					else
					{
						for (j = 0; j < ml_tmp->len; j += page_size)
						{
							if (ERROR_OK != pf->read_target(context, 
									area_char, ml_tmp->addr + j, 
									read_buf + j, page_size))
							{
								free(read_buf);
								read_buf = NULL;
								pgbar_fini();
								LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_READ), 
											fullname);
								ret = ERRCODE_FAILURE_OPERATION;
								goto leave_program_mode;
							}
							
							pgbar_update(page_size);
						}
					}
					
					if (op->verify_operations & area_mask)
					{
						for (j = 0; j < ml_tmp->len; j++)
						{
							if (tbuff[read_offset + j] != read_buf[j])
							{
								pgbar_fini();
								LOG_ERROR(
									_GETTEXT(ERRMSG_FAILURE_VERIFY_AT_02X), 
									fullname, ml_tmp->addr + j, 
									(uint64_t)read_buf[j], 
									(uint64_t)tbuff[read_offset + j]);
								free(read_buf);
								read_buf = NULL;
								ret = ERRCODE_FAILURE_VERIFY;
								goto leave_program_mode;
							}
						}
					}
					else
					{
						memcpy(&tbuff[read_offset], read_buf, ml_tmp->len);
					}
					free(read_buf);
					read_buf = NULL;
					
					ml_tmp = MEMLIST_GetNext(ml_tmp);
				}
				time_in_ms = pgbar_fini();
			}
			else
			{
				if (ERROR_OK != pf->read_target(context, area_char, 
							start_addr, (uint8_t *)&value, target_size))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_READ), fullname);
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				pgbar_update(target_size);
				time_in_ms = pgbar_fini();
				
				value &= byte_mask[target_size];
				
				if (op->verify_operations & area_mask)
				{
					if (value == prog_area->value)
					{
						LOG_INFO(_GETTEXT(INFOMSG_VERIFIED), fullname);
					}
					else
					{
						sprintf(str_tmp, 
							"%%s verify failed, read=0x%%0%d"PRIX64", "
							"want=0x%%0%d"PRIX64".\n",
							target_size * 2, target_size * 2);
						LOG_ERROR(str_tmp, fullname, value, prog_area->value);
					}
				}
				else
				{
					sprintf(str_tmp, "%%s read is 0x%%0%d"PRIX64"\n", 
							target_size * 2);
					LOG_INFO(str_tmp, fullname, value);
				}
			}
			
			if (op->verify_operations & area_mask)
			{
				LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), fullname, 
							target_size, 
							(target_size / 1024.0) / (time_in_ms / 1000.0));
			}
			else
			{
				LOG_INFO(_GETTEXT(INFOMSG_READ_SIZE), fullname, 
							target_size, 
							(target_size / 1024.0) / (time_in_ms / 1000.0));
			}
		}
		i++;
	}
	
leave_program_mode:
	if (pf->leave_program_mode != NULL)
	{
		if (ret != ERROR_OK)
		{
			pf->leave_program_mode(context, 0);
		}
		else if (ERROR_OK != pf->leave_program_mode(context, 1))
		{
			ret = ERROR_FAIL;
		}
	}
	return ret;
}

RESULT target_init(struct program_info_t *pi, struct programmer_info_t *prog)
{
	uint16_t i;
	uint8_t area_idx;
	char mode_buff[4];
	
	LOG_PUSH();
	LOG_MUTE();
	sprintf(mode_buff, "%d", pi->mode);
	cur_target->parse_argument('m', mode_buff);
	LOG_POP();
	
	if (NULL == pi->chip_name)
	{
		if (strchr(cur_target->feature, NO_TARGET[0]) != NULL)
		{
			return ERROR_OK;
		}
		else if (NULL == strchr(cur_target->feature, AUTO_DETECT[0]))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "Auto-detect", 
						cur_target->name);
			return ERRCODE_NOT_SUPPORT;
		}
		// auto detect
		memcpy(&target_chip_param, &target_chips.chips_param[0], 
					sizeof(target_chip_param));
		LOG_INFO(_GETTEXT(INFOMSG_TRY_AUTODETECT));
		
		if (target_chips.num_of_chips > 1)
		{
			struct program_context_t context;
			struct operation_t opt_tmp;
			
			memset(&opt_tmp, 0, sizeof(opt_tmp));
			opt_tmp.read_operations = CHIPID;
			context.op = &opt_tmp;
			context.param = &target_chip_param;
			context.pi = pi;
			context.prog = prog;
			if (ERROR_OK != target_program(&context))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
				return ERRCODE_AUTODETECT_FAIL;
			}
			
			LOG_INFO(_GETTEXT(INFOMSG_AUTODETECT_SIGNATURE), pi->chip_id);
			for (i = 0; i < target_chips.num_of_chips; i++)
			{
				if (pi->chip_id == target_chips.chips_param[i].chip_id)
				{
					pi->chip_name = target_chips.chips_param[i].chip_name;
					LOG_INFO(_GETTEXT(INFOMSG_CHIP_FOUND), pi->chip_name);
					
					goto Post_Init;
				}
			}
		}
		else
		{
			i = 0;
			goto Post_Init;
		}
		
		LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
		return ERRCODE_AUTODETECT_FAIL;
	}
	else
	{
		for (i = 0; i < target_chips.num_of_chips; i++)
		{
			if (!strcmp(target_chips.chips_param[i].chip_name, pi->chip_name))
			{
				goto Post_Init;
			}
		}
		
		return ERROR_FAIL;
	}
Post_Init:
	if ((cur_target->adjust_setting != NULL) 
		&&(ERROR_OK != cur_target->adjust_setting(&program_info, 
							&target_chips.chips_param[i], program_info.mode)))
	{
		return ERROR_FAIL;
	}
	memcpy(&target_chip_param, &target_chips.chips_param[i], 
			sizeof(target_chip_param));
	
	i = 0;
	while (cur_target->program_area_map[i].name != 0)
	{
		area_idx = target_area_idx(cur_target->program_area_map[i].name);
		pi->program_areas[area_idx].size = 
								target_chip_param.chip_areas[area_idx].size;
		i++;
	}
	
	return ERROR_OK;
}

static uint32_t target_prepare_operation(uint32_t *operation)
{
	uint32_t i;
	uint32_t ret;
	struct program_area_map_t *a;
	
	a = (struct program_area_map_t *)cur_target->program_area_map;
	if (*operation & ALL)
	{
		i = 0;
		while (a[i].name != 0)
		{
			*operation |= target_area_mask(a[i].name);
			i++;
		}
	}
	
	ret = 0;
	i = 0;
	while (a[i].name != 0)
	{
		if (*operation & target_area_mask(a[i].name))
		{
			ret += a[i].data_pos;
		}
		i++;
	}
	return ret;
}

RESULT target_prepare_operations(struct operation_t *operations, 
									uint32_t *readfile, uint32_t *writefile)
{
	if ((NULL == cur_target) || (NULL == cur_target->program_area_map) 
		|| (NULL == operations))
	{
		return ERROR_FAIL;
	}
	
	*readfile = *writefile = 0;
	target_prepare_operation(&operations->erase_operations);
	*readfile += target_prepare_operation(&operations->write_operations);
	*readfile += target_prepare_operation(&operations->verify_operations);
	*writefile += target_prepare_operation(&operations->read_operations);
	
	return ERROR_OK;
}

static void target_print_single_memory(char type)
{
	uint32_t mapidx;
	int8_t paramidx;
	char *full_type = target_area_fullname(type);
	struct program_area_map_t *p_map;
	
	p_map = (struct program_area_map_t *)cur_target->program_area_map;
	mapidx = 0;
	while ((p_map[mapidx].name != 0) && (p_map[mapidx].name != type))
	{
		mapidx++;
	}
	if (0 == p_map[mapidx].name)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), full_type, 
					program_info.chip_name);
		return;
	}
	
	paramidx = target_area_idx(type);
	if (paramidx < 0 )
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), full_type, 
					program_info.chip_name);
		return;
	}
	
	printf("%s of %s:\n", full_type, program_info.chip_name);
	if (p_map[mapidx].data_pos)
	{
		printf("%c_seg = 0x%08X, ", type, 
				target_chip_param.chip_areas[paramidx].seg);
		printf("%c_addr = 0x%08X, ", type, 
				target_chip_param.chip_areas[paramidx].addr);
	}
	printf("%c_default = 0x%"PRIX64", ", type, 
				target_chip_param.chip_areas[paramidx].default_value);
	printf("%c_bytelen = %d\n", type, 
				target_chip_param.chip_areas[paramidx].size);
}

void target_print_memory(char type)
{
	uint8_t i;
	
	if (NULL == cur_target)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "Target");
		return;
	}
	
	if (type > 0)
	{
		target_print_single_memory(type);
	}
	else
	{
		i = 0;
		while (cur_target->program_area_map[i].name != 0)
		{
			target_print_single_memory(cur_target->program_area_map[i].name);
			i++;
		}
	}
}

void target_print_setting(char type)
{
	struct chip_fl_t fl;
	uint32_t i, j;
	char *full_type = target_area_fullname(type);
	struct program_area_map_t *p_map;
	
	if (NULL == full_type)
	{
		LOG_BUG(_GETTEXT("%c passed to %s\n"), type, __FUNCTION__);
		return;
	}
	
	p_map = (struct program_area_map_t *)cur_target->program_area_map;
	i = 0;
	while ((p_map[i].name != 0) && (p_map[i].name != type))
	{
		i++;
	}
	if (0 == p_map[i].name)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), full_type, 
					program_info.chip_name);
		return;
	}
	
	memset(&fl, 0, sizeof(struct chip_fl_t));
	target_build_chip_fl(program_info.chip_type, program_info.chip_name, 
						 full_type, &fl);
	
	// print fl
	printf("%s of %s:\n", full_type, program_info.chip_name);
	printf("init = 0x%"PRIX64", ", fl.init_value);
	printf("num_of_warnings = %d, ", fl.num_of_fl_warnings);
	printf("num_of_settings = %d\n", fl.num_of_fl_settings);
	for (i = 0; i < fl.num_of_fl_warnings; i++)
	{
		printf("warning: mask = 0x%"PRIX64", ", fl.warnings[i].mask);
		printf("value = 0x%"PRIX64", ", fl.warnings[i].value);
		printf("msg = %s\n", fl.warnings[i].msg);
	}
	for (i = 0; i < fl.num_of_fl_settings; i++)
	{
		printf("setting: name = %s, ", fl.settings[i].name);
		printf("mask = 0x%"PRIX64", ", fl.settings[i].mask);
		printf("num_of_choices = %d", fl.settings[i].num_of_choices);
		if (fl.settings[i].ban != NULL)
		{
			printf(", ban = %s", fl.settings[i].ban);
		}
		if (fl.settings[i].info != NULL)
		{
			printf(", info = %s", fl.settings[i].info);
		}
		if (fl.settings[i].use_checkbox)
		{
			printf(", checked = 0x%"PRIX64, fl.settings[i].checked);
			printf(", unchecked = 0x%"PRIX64, fl.settings[i].unchecked);
		}
		else if (fl.settings[i].use_edit)
		{
			printf(", radix = %d", fl.settings[i].radix);
			printf(", shift = %d", fl.settings[i].shift);
			printf(", bytelen = %d", fl.settings[i].bytelen);
		}
		printf("\n");
		for (j = 0; j < fl.settings[i].num_of_choices; j++)
		{
			printf("choice: value = 0x%"PRIX64", ", fl.settings[i].choices[j].value);
			printf("text = %s\n", fl.settings[i].choices[j].text);
		}
	}
	
	target_release_chip_fl(&fl);
}

void target_print_target(uint32_t index)
{
	uint32_t i, j;
	struct chip_param_t *p_param;
	struct program_area_map_t *p_map;
	char area[3];
	
	target_build_chip_series(targets_info[index].name, 
						targets_info[index].program_mode, &target_chips);
	
	if (0 == target_chips.num_of_chips)
	{
		return;
	}
	
	if (strlen(targets_info[index].feature) > 0)
	{
		printf("Support list of %s(%s):", targets_info[index].name, 
				targets_info[index].feature);
	}
	else
	{
		printf("Support list of %s:", targets_info[index].name);
	}
	// fake
	p_map = (struct program_area_map_t *)targets_info[index].program_area_map;
	i = 0;
	while (p_map[i].name != 0)
	{
		if (p_map[i].fseg_addr)
		{
			printf(" %c_fseg = 0x%X,", p_map[i].name, p_map[i].fseg_addr);
		}
		if (p_map[i].fstart_addr)
		{
			printf(" %c_faddr = 0x%X,", p_map[i].name, p_map[i].fstart_addr);
		}
		i++;
	}
	// extra info from target
	targets_info[index].parse_argument('E', NULL);
	printf("\n");
	
	// Targets based on ComPort outputs there special COM settings
	if (strchr(targets_info[index].feature, 'C') != NULL)
	{
		targets_info[index].parse_argument('S', NULL);
	}
	else
	{
		for (i = 0; i < target_chips.num_of_chips; i++)
		{
			p_param = &target_chips.chips_param[i];
			
			// name
			printf("%s:", p_param->chip_name);
			// id
			printf(" id = 0x%X,", p_param->chip_id);
			// mode
			if (p_param->program_mode_str != NULL)
			{
				printf(" mode = %s,", p_param->program_mode_str);
			}
			// area
			printf(" area = ");
			area[2] = 0;
			j = 0;
			while (p_map[j].name != 0)
			{
				area[0] = p_map[j].name;
				area[1] = p_map[j].data_pos + '0';
				printf("%s", area);
				j++;
			}
			printf("\n");
		}
		printf("\n");
	}
	
	target_release_chip_series(&target_chips);
}

void target_print_list(void)
{
	uint32_t i;
	
	printf(_GETTEXT("Supported targets:\n"));
	for (i = 0; targets_info[i].name != NULL; i++)
	{
		target_print_target(i);
	}
}

void target_print_help(void)
{
	uint32_t i;
	
	for (i = 0; targets_info[i].name != NULL; i++)
	{
		targets_info[i].parse_argument('h', NULL);
	}
}

uint32_t target_get_number(void)
{
	return sizeof(targets_info) / sizeof(targets_info[0]) - 1;
}

static RESULT target_probe_chip(char *chip_name)
{
	uint32_t i;
	
	if (NULL == chip_name)
	{
		return ERROR_FAIL;
	}
	
	for (i = 0; i < target_chips.num_of_chips; i++)
	{
		if (NULL == target_chips.chips_param[i].chip_name)
		{
			continue;
		}
		
		if (!strcmp(target_chips.chips_param[i].chip_name, chip_name))
		{
			return ERROR_OK;
		}
	}
	
	return ERROR_FAIL;
}

RESULT target_info_init(struct program_info_t *pi)
{
	uint32_t i;
	RESULT (*probe_chip)(char *chip_name);
	
#if PARAM_CHECK
	if ((NULL == pi) || ((NULL == pi->chip_name) && (NULL == pi->chip_type)))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	target_release_chip_series(&target_chips);
	
	if (NULL == pi->chip_type)
	{
		// find which series of target contain current chip_name
		for (i = 0; targets_info[i].name != NULL; i++)
		{
			if (ERROR_OK == target_build_chip_series(targets_info[i].name, 
								targets_info[i].program_mode, &target_chips))
			{
				// configuration file exists, use default probe function
				probe_chip = target_probe_chip;
			}
			else
			{
				// use probe function defined by target chip
				continue;
			}
			
			if (probe_chip(pi->chip_name) == ERROR_OK)
			{
				cur_target = &targets_info[i];
				pi->chip_type = (char *)targets_info[i].name;
				LOG_DEBUG(_GETTEXT("%s initialized for %s.\n"), 
						 cur_target->name, pi->chip_name);
				
				return ERROR_OK;
			}
			target_release_chip_series(&target_chips);
		}
		
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), pi->chip_name);
	}
	else
	{
		// find current series of chip_type
		for (i = 0; targets_info[i].name != NULL; i++)
		{
			if (!strcmp(targets_info[i].name, pi->chip_type))
			{
				if (ERROR_OK == target_build_chip_series(targets_info[i].name, 
								targets_info[i].program_mode, &target_chips))
				{
					// configuration file exists, use default probe function
					probe_chip = target_probe_chip;
				}
				else if (strchr(targets_info[i].feature, NO_TARGET[0]) != NULL)
				{
					cur_target = &targets_info[i];
					return ERROR_OK;
				}
				else
				{
					LOG_BUG(_GETTEXT("probe_chip not defined by %s\n"), 
							targets_info[i].name);
					return ERROR_FAIL;
				}
				
				if ((pi->chip_name != NULL) 
				   && (ERROR_OK != probe_chip(pi->chip_name)))
				{
					LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), 
							  pi->chip_name, targets_info[i].name);
					target_release_chip_series(&target_chips);
					cur_target = NULL;
					return ERRCODE_NOT_SUPPORT;
				}
				else
				{
					cur_target = &targets_info[i];
					LOG_DEBUG(_GETTEXT("%s initialized.\n"), cur_target->name);
					return ERROR_OK;
				}
			}
		}
		
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), pi->chip_type);
	}
	
	cur_target = NULL;
	return ERRCODE_NOT_SUPPORT;
}

static uint32_t target_xml_get_child_number(xmlNodePtr parentNode, 
												char * child_name)
{
	uint32_t result = 0;
	
	if ((parentNode != NULL) && (parentNode->children != NULL))
	{
		parentNode = parentNode->children->next;
		while (parentNode != NULL)
		{
			if (!xmlStrcmp(parentNode->name, BAD_CAST child_name))
			{
				result++;
			}
			parentNode = parentNode->next->next;
		}
	}
	
	return result;
}

RESULT target_build_chip_fl(const char *chip_series, const char *chip_module, 
							char *type, struct chip_fl_t *fl)
{
	xmlDocPtr doc = NULL;
	xmlNodePtr curNode = NULL;
	xmlNodePtr paramNode, settingNode;
	char *filename = NULL;
	uint32_t i, j, num_of_chips;
	RESULT ret = ERROR_OK;
	FILE *fp;
	uint32_t str_len;
	char *m;
	
#if PARAM_CHECK
	if ((NULL == chip_series) || (NULL == chip_module) || (NULL == fl))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (NULL == config_dir)
	{
		return ERROR_FAIL;
	}
	
	// release first if necessary
	target_release_chip_fl(fl);
	
	filename = (char *)malloc(strlen(config_dir)
					+ strlen(chip_series) + strlen(TARGET_CONF_FILE_EXT) + 1);
	if (NULL == filename)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	strcpy(filename, config_dir);
	strcat(filename, chip_series);
	strcat(filename, TARGET_CONF_FILE_EXT);
	fp = fopen(filename, "r");
	if (NULL == fp)
	{
		// no error message, just return error
		ret = ERROR_FAIL;
		goto free_and_exit;
	}
	else
	{
		fclose(fp);
		fp = NULL;
	}
	
	doc = xmlReadFile(filename, "", XML_PARSE_RECOVER);
	if (NULL == doc)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPEN), filename);
		ret = ERRCODE_FAILURE_OPEN;
		goto free_and_exit;
	}
	curNode = xmlDocGetRootElement(doc);
	if (NULL == curNode)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	// valid check
	if (xmlStrcmp(curNode->name, BAD_CAST "series") 
		|| !xmlHasProp(curNode, BAD_CAST "name") 
		|| xmlStrcmp(xmlGetProp(curNode, BAD_CAST "name"), 
					 (const xmlChar *)chip_series))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	num_of_chips = target_xml_get_child_number(curNode, "chip");
	if (0 == num_of_chips)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), chip_series);
		ret = ERRCODE_NOT_SUPPORT;
		goto free_and_exit;
	}
	
	// read data
	curNode = curNode->children->next;
	for (i = 0; i < num_of_chips; i++)
	{
		// check
		if ((NULL == curNode) 
			|| xmlStrcmp(curNode->name, BAD_CAST "chip")
			|| !xmlHasProp(curNode, BAD_CAST "name"))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read config file");
			ret = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		if (strcmp((const char *)chip_module, 
				   (const char *)xmlGetProp(curNode, BAD_CAST "name")))
		{
			// not the chip I want
			curNode = curNode->next->next;
			continue;
		}
		else
		{
			break;
		}
	}
	if (i >= num_of_chips)
	{
		// not found
		goto free_and_exit;
	}
	
	paramNode = curNode->children->next;
	// read parameters
	while((paramNode != NULL) && xmlStrcmp(paramNode->name, BAD_CAST type))
	{
		paramNode = paramNode->next->next;
	}
	if (NULL == paramNode)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), type, chip_module);
		ret = ERRCODE_NOT_SUPPORT;
		goto free_and_exit;
	}
	
	// we found the parameter
	// valid check
	if (!xmlHasProp(paramNode, BAD_CAST "init"))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	// read fl number
	fl->num_of_fl_settings = 
		(uint16_t)target_xml_get_child_number(paramNode, "setting");
	if (0 == fl->num_of_fl_settings)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), type, chip_module);
		ret = ERRCODE_NOT_SUPPORT;
		goto free_and_exit;
	}
	
	// read fl init value
	fl->init_value = 
		strtoull((const char *)xmlGetProp(paramNode, BAD_CAST "init"), NULL, 0);
	
	// alloc memory for settings
	fl->settings = (struct chip_fl_setting_t*)malloc(
		fl->num_of_fl_settings * sizeof(struct chip_fl_setting_t));
	if (NULL == fl->settings)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		ret = ERRCODE_NOT_ENOUGH_MEMORY;
		goto free_and_exit;
	}
	memset(fl->settings, 0, 
		   fl->num_of_fl_settings * sizeof(struct chip_fl_setting_t));
	
	settingNode = paramNode->children->next;
	// has warning?
	if ((settingNode != NULL) && 
		!strcmp((const char *)settingNode->name, "warning"))
	{
		xmlNodePtr warningNode = settingNode;
		xmlNodePtr wNode;
		
		settingNode = settingNode->next->next;
		// parse warning
		fl->num_of_fl_warnings = 
			(uint16_t)target_xml_get_child_number(warningNode, "w");
		if (fl->num_of_fl_warnings != 0)
		{
			fl->warnings = (struct chip_fl_warning_t*)malloc(
				fl->num_of_fl_warnings * sizeof(struct chip_fl_warning_t));
			if (NULL == fl->warnings)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
			memset(fl->warnings, 0, 
				   fl->num_of_fl_warnings * sizeof(struct chip_fl_warning_t));
			
			wNode = warningNode->children->next;
			for (i = 0; i < fl->num_of_fl_warnings; i++)
			{
				// check
				if (strcmp((const char *)wNode->name, "w"))
				{
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							  "read config file");
					ret = ERRCODE_FAILURE_OPERATION;
					goto free_and_exit;
				}
				
				fl->warnings[i].mask = strtoull(
					(const char *)xmlGetProp(wNode, BAD_CAST "mask"), 
					NULL, 0);
				fl->warnings[i].value = strtoull(
					(const char *)xmlGetProp(wNode, BAD_CAST "value"), 
					NULL, 0);
				m = (char *)xmlGetProp(wNode, BAD_CAST "msg");
				str_len = strlen(m);
				fl->warnings[i].msg = (char *)malloc(str_len + 1);
				if (NULL == fl->warnings[i].msg)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
					ret = ERRCODE_NOT_ENOUGH_MEMORY;
					goto free_and_exit;
				}
				strcpy(fl->warnings[i].msg, (const char *)m);
				
				wNode = wNode->next->next;
			}
		}
	}
	
	// parse settings
	for (i = 0; i < fl->num_of_fl_settings; i++)
	{
		xmlNodePtr choiceNode;
		uint8_t checked_exists;
		
		fl->settings[i].num_of_choices = 
			(uint16_t)target_xml_get_child_number(settingNode, "choice");
		// check
		if (strcmp((const char *)settingNode->name, "setting") 
			|| (!xmlHasProp(settingNode, BAD_CAST "name"))
			|| (!xmlHasProp(settingNode, BAD_CAST "mask")))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "read config file");
			ret = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		m = (char *)xmlGetProp(settingNode, BAD_CAST "name");
		str_len = strlen(m);
		fl->settings[i].name = (char *)malloc(str_len + 1);
		if (NULL == fl->settings[i].name)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
			ret = ERRCODE_NOT_ENOUGH_MEMORY;
			goto free_and_exit;
		}
		strcpy(fl->settings[i].name, m);
		fl->settings[i].mask = strtoull(
			(const char *)xmlGetProp(settingNode, BAD_CAST "mask"), NULL, 0);
		if (xmlHasProp(settingNode, BAD_CAST "ban"))
		{
			m = (char *)xmlGetProp(settingNode, BAD_CAST "ban");
			str_len = strlen(m);
			fl->settings[i].ban = (char *)malloc(str_len + 1);
			if (NULL == fl->settings[i].ban)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
			strcpy(fl->settings[i].ban, m);
		}
		
		// parse info if exists
		if (xmlHasProp(settingNode, BAD_CAST "info"))
		{
			m = (char *)xmlGetProp(settingNode, BAD_CAST "info");
			str_len = strlen(m);
			fl->settings[i].info = (char *)malloc(str_len + 1);
			if (NULL == fl->settings[i].info)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
			strcpy(fl->settings[i].info, m);
		}
		
		// parse bytelen if exists
		if (xmlHasProp(settingNode, BAD_CAST "bytelen"))
		{
			fl->settings[i].bytelen = (uint8_t)strtoul(
				(const char *)xmlGetProp(settingNode, BAD_CAST "bytelen"), 
				NULL, 0);
		}
		
		// parse radix if exists
		if (xmlHasProp(settingNode, BAD_CAST "radix"))
		{
			fl->settings[i].radix = (uint8_t)strtoul(
				(const char *)xmlGetProp(settingNode, BAD_CAST "radix"), 
				NULL, 0);
		}
		
		// parse shift if exists
		if (xmlHasProp(settingNode, BAD_CAST "shift"))
		{
			fl->settings[i].shift = (uint8_t)strtoul(
				(const char *)xmlGetProp(settingNode, BAD_CAST "shift"), 
				NULL, 0);
		}
		
		// parse checked or unchecked
		checked_exists = 0;
		if (xmlHasProp(settingNode, BAD_CAST "checked"))
		{
			checked_exists = 1;
			fl->settings[i].checked = strtoull(
				(const char *)xmlGetProp(settingNode, BAD_CAST "checked"), 
				NULL, 0);
			fl->settings[i].unchecked = 
				fl->settings[i].checked ^ fl->settings[i].mask;
			fl->settings[i].use_checkbox = 1;
		}
		if (xmlHasProp(settingNode, BAD_CAST "unchecked"))
		{
			fl->settings[i].unchecked = strtoull(
				(const char *)xmlGetProp(settingNode, BAD_CAST "unchecked"), 
				NULL, 0);
			if (!checked_exists)
			{
				fl->settings[i].checked = 
						fl->settings[i].unchecked ^ fl->settings[i].mask;
			}
			fl->settings[i].use_checkbox = 1;
		}
		
		if (!fl->settings[i].use_checkbox 
			&& (0 == fl->settings[i].num_of_choices))
		{
			fl->settings[i].use_edit = 1;
		}
		else
		{
			fl->settings[i].use_edit = 0;
		}
		
		// parse choices
		if (0 == fl->settings[i].num_of_choices)
		{
			// no choice
			settingNode = settingNode->next->next;
			continue;
		}
		
		// malloc memory for choices
		fl->settings[i].choices = (struct chip_fl_choice_t*)malloc(
			fl->settings[i].num_of_choices * sizeof(struct chip_fl_choice_t));
		if (NULL == fl->settings[i].choices)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
			ret = ERRCODE_NOT_ENOUGH_MEMORY;
			goto free_and_exit;
		}
		memset(fl->settings[i].choices, 0, 
			fl->settings[i].num_of_choices * sizeof(struct chip_fl_choice_t));
		
		choiceNode = settingNode->children->next;
		// parse choices
		for (j = 0; j < fl->settings[i].num_of_choices; j++)
		{
			// check
			if (strcmp((const char *)choiceNode->name, "choice") 
				|| !xmlHasProp(choiceNode, BAD_CAST "value") 
				|| !xmlHasProp(choiceNode, BAD_CAST "text"))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "read config file");
				ret = ERRCODE_FAILURE_OPERATION;
				goto free_and_exit;
			}
			
			// parse
			fl->settings[i].choices[j].value = strtoull(
				(const char *)xmlGetProp(choiceNode, BAD_CAST "value"), 
				NULL, 0);
			m = (char *)xmlGetProp(choiceNode, BAD_CAST "text");
			str_len = strlen(m);
			fl->settings[i].choices[j].text = (char *)malloc(str_len + 1);
			if (NULL == fl->settings[i].choices[j].text)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
				ret = ERRCODE_NOT_ENOUGH_MEMORY;
				goto free_and_exit;
			}
			strcpy(fl->settings[i].choices[j].text, m);
			
			choiceNode = choiceNode->next->next;
		}
		settingNode = settingNode->next->next;
	}
	
free_and_exit:
	if (filename != NULL)
	{
		free(filename);
		filename = NULL;
	}
	if (doc != NULL)
	{
		xmlFreeDoc(doc);
		doc = NULL;
	}
	
	return ret;
}

RESULT target_release_chip_fl(struct chip_fl_t *fl)
{
	uint32_t i, j;
	
	if (NULL == fl)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	
	// free warnings
	if (fl->warnings != NULL)
	{
		for (i = 0; i < fl->num_of_fl_warnings; i++)
		{
			if (fl->warnings[i].msg != NULL)
			{
				free(fl->warnings[i].msg);
				fl->warnings[i].msg = NULL;
			}
		}
		free(fl->warnings);
		fl->warnings = NULL;
	}
	
	// free settings
	if (fl->settings != NULL)
	{
		for (i = 0; i < fl->num_of_fl_settings; i++)
		{
			if (fl->settings[i].name != NULL)
			{
				free(fl->settings[i].name);
				fl->settings[i].name = NULL;
			}
			if (fl->settings[i].ban != NULL)
			{
				free(fl->settings[i].ban);
				fl->settings[i].ban = NULL;
			}
			if (fl->settings[i].choices != NULL)
			{
				for (j = 0; j < fl->settings[i].num_of_choices; j++)
				{
					if (fl->settings[i].choices[j].text != NULL)
					{
						free(fl->settings[i].choices[j].text);
						fl->settings[i].choices[j].text = NULL;
					}
				}
				free(fl->settings[i].choices);
				fl->settings[i].choices = NULL;
			}
		}
		free(fl->settings);
		fl->settings = NULL;
	}
	memset(fl, 0, sizeof(struct chip_fl_t));
	
	return ERROR_OK;
}

RESULT target_build_chip_series(const char *chip_series, 
		const struct program_mode_t *program_mode, struct chip_series_t *s)
{
	xmlDocPtr doc = NULL;
	xmlNodePtr curNode = NULL;
	char *filename = NULL;
	struct chip_param_t *p_param;
	uint32_t i, j, target_para_size_defined;
	RESULT ret = ERROR_OK;
	FILE *fp;
	
#if PARAM_CHECK
	if ((NULL == chip_series) || (NULL == s))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (NULL == config_dir)
	{
		return ERROR_FAIL;
	}
	
	// release first if necessary
	target_release_chip_series(s);
	
	filename = (char *)malloc(strlen(config_dir)+ strlen(chip_series) 
								+ strlen(TARGET_CONF_FILE_EXT) + 1);
	if (NULL == filename)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	strcpy(filename, config_dir);
	strcat(filename, chip_series);
	strcat(filename, TARGET_CONF_FILE_EXT);
	fp = fopen(filename, "r");
	if (NULL == fp)
	{
		// no error message, just return error
		ret = ERROR_FAIL;
		goto free_and_exit;
	}
	else
	{
		fclose(fp);
		fp = NULL;
	}
	
	doc = xmlReadFile(filename, "", XML_PARSE_RECOVER);
	if (NULL == doc)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPEN), filename);
		ret = ERRCODE_FAILURE_OPEN;
		goto free_and_exit;
	}
	curNode = xmlDocGetRootElement(doc);
	if (NULL == curNode)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	// valid check
	if (xmlStrcmp(curNode->name, BAD_CAST "series") 
		|| !xmlHasProp(curNode, BAD_CAST "name") 
		|| xmlStrcmp(xmlGetProp(curNode, BAD_CAST "name"), 
					 (const xmlChar *)chip_series))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read config file");
		ret = ERRCODE_FAILURE_OPERATION;
		goto free_and_exit;
	}
	
	s->num_of_chips = target_xml_get_child_number(curNode, "chip");
	if (0 == s->num_of_chips)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), chip_series);
		ret = ERRCODE_NOT_SUPPORT;
		goto free_and_exit;
	}
	s->chips_param = (struct chip_param_t *)malloc(sizeof(struct chip_param_t) 
											* s->num_of_chips);
	if (NULL == s->chips_param)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		ret = ERRCODE_NOT_ENOUGH_MEMORY;
		goto free_and_exit;
	}
	memset(s->chips_param, 0, sizeof(struct chip_param_t) * s->num_of_chips);
	
	// read data
	curNode = curNode->children->next;
	for (i = 0; i < s->num_of_chips; i++)
	{
		xmlNodePtr paramNode;
		
		p_param = &(s->chips_param[i]);
		
		// check
		if ((NULL == curNode) 
			|| xmlStrcmp(curNode->name, BAD_CAST "chip")
			|| !xmlHasProp(curNode, BAD_CAST "name"))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read config file");
			ret = ERRCODE_FAILURE_OPERATION;
			goto free_and_exit;
		}
		
		// read name
		strncpy(p_param->chip_name, 
				(const char *)xmlGetProp(curNode, BAD_CAST "name"), 
				sizeof(p_param->chip_name));
		
		// read parameters
		target_para_size_defined = 0;
		paramNode = curNode->children->next;
		while(paramNode != NULL)
		{
			if (!xmlStrcmp(paramNode->name, BAD_CAST "chip_id"))
			{
				p_param->chip_id = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "chip_erase"))
			{
				p_param->chip_erase = (uint8_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "program_mode"))
			{
				char *mode_tmp = (char *)xmlNodeGetContent(paramNode);
				int8_t mode_idx;
				
				p_param->program_mode_str = 
										(char *)malloc(strlen(mode_tmp) + 1);
				if (NULL == p_param->program_mode_str)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
					ret = ERRCODE_NOT_ENOUGH_MEMORY;
					goto free_and_exit;
				}
				strcpy(p_param->program_mode_str, mode_tmp);
				p_param->program_mode = 0;
				if ((0 != i) 
					|| (strcmp(chip_series, p_param->chip_name)))
				{
					for (j = 0; j < strlen(mode_tmp); j++)
					{
						mode_idx = 
								target_mode_get_idx(program_mode, mode_tmp[j]);
						if (mode_idx >= 0)
						{
							p_param->program_mode |= 1 << mode_idx;
						}
						else
						{
							LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), 
								mode_tmp, "program_mode of current target");
							ret = ERRCODE_NOT_SUPPORT;
							goto free_and_exit;
						}
					}
				}
				else
				{
					j = 0;
					while (program_mode[j].name != 0)
					{
						p_param->program_mode |= 1 << j;
						j++;
					}
				}
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_addr"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_seg"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_page_size"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].page_size = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_page_num"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].page_num = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_default"))
			{
				p_param->chip_areas[BOOTLOADER_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_page_size"))
			{
				p_param->chip_areas[SRAM_IDX].page_size = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_page_num"))
			{
				p_param->chip_areas[SRAM_IDX].page_num = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_addr"))
			{
				p_param->chip_areas[APPLICATION_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_seg"))
			{
				p_param->chip_areas[APPLICATION_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_page_size"))
			{
				p_param->chip_areas[APPLICATION_IDX].page_size = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_page_num"))
			{
				p_param->chip_areas[APPLICATION_IDX].page_num = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_default"))
			{
				p_param->chip_areas[APPLICATION_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_addr"))
			{
				p_param->chip_areas[EEPROM_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_seg"))
			{
				p_param->chip_areas[EEPROM_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_page_size"))
			{
				p_param->chip_areas[EEPROM_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_page_num"))
			{
				p_param->chip_areas[EEPROM_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_default"))
			{
				p_param->chip_areas[EEPROM_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_addr"))
			{
				p_param->chip_areas[OTPROM_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_seg"))
			{
				p_param->chip_areas[OTPROM_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_page_size"))
			{
				p_param->chip_areas[OTPROM_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_page_num"))
			{
				p_param->chip_areas[OTPROM_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_default"))
			{
				p_param->chip_areas[OTPROM_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_addr"))
			{
				p_param->chip_areas[USRSIG_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_seg"))
			{
				p_param->chip_areas[USRSIG_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_page_size"))
			{
				p_param->chip_areas[USRSIG_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_page_num"))
			{
				p_param->chip_areas[USRSIG_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_default"))
			{
				p_param->chip_areas[USRSIG_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_addr"))
			{
				p_param->chip_areas[FUSE_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_seg"))
			{
				p_param->chip_areas[FUSE_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_page_num"))
			{
				p_param->chip_areas[FUSE_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_page_size"))
			{
				p_param->chip_areas[FUSE_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_default"))
			{
				p_param->chip_areas[FUSE_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_addr"))
			{
				p_param->chip_areas[LOCK_IDX].addr = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_seg"))
			{
				p_param->chip_areas[LOCK_IDX].seg = 
					(uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_page_num"))
			{
				p_param->chip_areas[LOCK_IDX].page_num = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_page_size"))
			{
				p_param->chip_areas[LOCK_IDX].page_size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_default"))
			{
				p_param->chip_areas[LOCK_IDX].default_value = 
					(uint32_t)strtoull(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse_size"))
			{
				target_para_size_defined |= FUSE;
				p_param->chip_areas[FUSE_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock_size"))
			{
				target_para_size_defined |= LOCK;
				p_param->chip_areas[LOCK_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "boot_size"))
			{
				target_para_size_defined |= BOOTLOADER;
				p_param->chip_areas[BOOTLOADER_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "sram_size"))
			{
				target_para_size_defined |= SRAM;
				p_param->chip_areas[SRAM_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "app_size"))
			{
				target_para_size_defined |= APPLICATION;
				p_param->chip_areas[APPLICATION_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "ee_size"))
			{
				target_para_size_defined |= EEPROM;
				p_param->chip_areas[EEPROM_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "otprom_size"))
			{
				target_para_size_defined |= OTPROM;
				p_param->chip_areas[OTPROM_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "usrsig_size"))
			{
				target_para_size_defined |= USRSIG;
				p_param->chip_areas[USRSIG_IDX].size = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "fuse"))
			{
				target_para_size_defined |= FUSE;
				p_param->chip_areas[FUSE_IDX].size = (uint32_t)strtoul(
					(const char *)xmlGetProp(paramNode, BAD_CAST "bytesize"), 
					NULL, 0);
				p_param->chip_areas[FUSE_IDX].default_value = strtoull(
					(const char *)xmlGetProp(paramNode, BAD_CAST "init"), 
					NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "lock"))
			{
				target_para_size_defined |= LOCK;
				p_param->chip_areas[LOCK_IDX].size = (uint32_t)strtoul(
					(const char *)xmlGetProp(paramNode, BAD_CAST "bytesize"), 
					NULL, 0);
				p_param->chip_areas[LOCK_IDX].default_value = strtoull(
					(const char *)xmlGetProp(paramNode, BAD_CAST "init"), 
					NULL, 0);
			}
			else if (!xmlStrcmp(paramNode->name, BAD_CAST "calibration"))
			{
				p_param->chip_areas[CALIBRATION_IDX].size = (uint32_t)strtoul(
					(const char *)xmlGetProp(paramNode,BAD_CAST "bytesize"),
					NULL, 0);
				p_param->chip_areas[CALIBRATION_IDX].default_value = strtoull(
						(const char *)xmlGetProp(paramNode, BAD_CAST "init"), 
						NULL, 0);
			}
			else
			{
				char *str_tmp = (char *)paramNode->name;
				
				if ((strlen(str_tmp) >= 6) 
					&& ('p' == str_tmp[0]) 
					&& ('a' == str_tmp[1])
					&& ('r' == str_tmp[2])
					&& ('a' == str_tmp[3])
					&& ('m' == str_tmp[4]))
				{
					// parameters
					j = strtoul(&str_tmp[5], NULL, 0);
					p_param->param[j] = (uint32_t)strtoul(
						(const char *)xmlNodeGetContent(paramNode), NULL, 0);
				}
				else
				{
					// wrong parameter
					LOG_ERROR(_GETTEXT(ERRMSG_INVALID), 
								(const char *)xmlNodeGetContent(paramNode), 
								chip_series);
					ret = ERRCODE_INVALID;
					goto free_and_exit;
				}
			}
			
			paramNode = paramNode->next->next;
		}
		
		if (!(target_para_size_defined & SRAM))
		{
			p_param->chip_areas[SRAM_IDX].size = 
				p_param->chip_areas[SRAM_IDX].page_size 
				* p_param->chip_areas[SRAM_IDX].page_num;
		}
		if (!(target_para_size_defined & APPLICATION))
		{
			p_param->chip_areas[APPLICATION_IDX].size = 
				p_param->chip_areas[APPLICATION_IDX].page_size 
				* p_param->chip_areas[APPLICATION_IDX].page_num;
		}
		if (!(target_para_size_defined & EEPROM))
		{
			p_param->chip_areas[EEPROM_IDX].size = 
				p_param->chip_areas[EEPROM_IDX].page_size 
				* p_param->chip_areas[EEPROM_IDX].page_num;
		}
		if (!(target_para_size_defined & BOOTLOADER))
		{
			p_param->chip_areas[BOOTLOADER_IDX].size = 
				p_param->chip_areas[BOOTLOADER_IDX].page_size 
				* p_param->chip_areas[BOOTLOADER_IDX].page_num;
		}
		if (!(target_para_size_defined & FUSE))
		{
			p_param->chip_areas[FUSE_IDX].size = 
				p_param->chip_areas[FUSE_IDX].page_size 
				* p_param->chip_areas[FUSE_IDX].page_num;
		}
		if (!(target_para_size_defined & LOCK))
		{
			p_param->chip_areas[LOCK_IDX].size = 
				p_param->chip_areas[LOCK_IDX].page_size 
				* p_param->chip_areas[LOCK_IDX].page_num;
		}
		if (!(target_para_size_defined & OTPROM))
		{
			p_param->chip_areas[OTPROM_IDX].size = 
				p_param->chip_areas[OTPROM_IDX].page_size 
				* p_param->chip_areas[OTPROM_IDX].page_num;
		}
		if (!(target_para_size_defined & USRSIG))
		{
			p_param->chip_areas[USRSIG_IDX].size = 
				p_param->chip_areas[USRSIG_IDX].page_size 
				* p_param->chip_areas[USRSIG_IDX].page_num;
		}
		
		if (0 == i)
		{
			// first chip is used to setting every chip
			for (j = 1; j < s->num_of_chips; j++)
			{
				memcpy(&s->chips_param[j], &s->chips_param[0], 
							sizeof(struct chip_param_t));
			}
		}
		
		curNode = curNode->next->next;
	}
	
free_and_exit:
	if (filename != NULL)
	{
		free(filename);
		filename = NULL;
	}
	if (doc != NULL)
	{
		xmlFreeDoc(doc);
		doc = NULL;
	}
	
	return ret;
}

RESULT target_release_chip_series(struct chip_series_t *s)
{
	uint32_t i;
	
	if ((s != NULL) && ((s->num_of_chips > 0) || (s->chips_param != NULL)))
	{
		for (i = 0; i < s->num_of_chips; i++)
		{
			if (s->chips_param[i].program_mode_str != NULL)
			{
				free(s->chips_param[i].program_mode_str);
				s->chips_param[i].program_mode_str = NULL;
			}
		}
		free(s->chips_param);
		s->chips_param = NULL;
		s->num_of_chips = 0;
	}
	memset(s, 0, sizeof(struct chip_series_t));
	
	return ERROR_OK;
}

