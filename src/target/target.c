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

#include "target.h"
#include "at89s5x/at89s5x.h"
#include "psoc/psoc.h"
#include "msp430/msp430.h"
#include "c8051f/c8051f.h"
#include "avr8/avr8.h"
#include "comisp/comisp.h"
#include "svf_player/svf_player.h"

target_info_t targets_info[] = 
{
	// at89s5x
	{
		S5X_STRING,							// name
		APPLICATION | LOCK,					// areas
		s5x_program_area_map,				// program_area_map
		s5x_parse_argument,					// parse_argument
		s5x_probe_chip,						// probe_chip
		s5x_init,							// init
		s5x_fini,							// fini
		s5x_interface_needed,				// interfaces_needed
		s5x_prepare_buffer,					// prepare_buffer
		s5x_write_buffer_from_file_callback,
											// write_buffer_from_file_callback
		NULL,								// write_file_from_buffer_callback
		s5x_program,						// program
		
//		NULL,								// get_mass_product_data_size
//		NULL,								// prepare_mass_product_data
	},
	// psoc
	{
		PSOC_STRING,						// name
		APPLICATION | LOCK | CHECKSUM,		// areas
		psoc_program_area_map,				// program_area_map
		psoc_parse_argument,				// parse_argument
		psoc_probe_chip,					// probe_chip
		psoc_init,							// init
		psoc_fini,							// fini
		psoc_interface_needed,				// interfaces_needed
		psoc_prepare_buffer,				// prepare_buffer
		psoc_write_buffer_from_file_callback,
											// write_buffer_from_file_callback
		NULL,								// write_file_from_buffer_callback
		psoc_program,						// program
		
		psoc_get_mass_product_data_size,	// get_mass_product_data_size
		psoc_prepare_mass_product_data,		// prepare_mass_product_data
	},
	// msp430
	{
		MSP430_STRING,						// name
		APPLICATION,						// areas
		msp430_program_area_map,			// program_area_map
		msp430_parse_argument,				// parse_argument
		msp430_probe_chip,					// probe_chip
		msp430_init,						// init
		msp430_fini,						// fini
		msp430_interface_needed,			// interfaces_needed
		msp430_prepare_buffer,				// prepare_buffer
		msp430_write_buffer_from_file_callback,
											// write_buffer_from_file_callback
		NULL,								// write_file_from_buffer_callback
		msp430_program,						// program
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// c8051f
	{
		C8051F_STRING,						// name
		APPLICATION,						// areas
		c8051f_program_area_map,			// program_area_map
		c8051f_parse_argument,				// parse_argument
		c8051f_probe_chip,					// probe_chip
		c8051f_init,						// init
		c8051f_fini,						// fini
		c8051f_interface_needed,			// interfaces_needed
		c8051f_prepare_buffer,				// prepare_buffer
		c8051f_write_buffer_from_file_callback,
											// write_buffer_from_file_callback
		NULL,								// write_file_from_buffer_callback
		c8051f_program,						// program
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// avr8
	{
		AVR8_STRING,						// name
		APPLICATION | EEPROM | LOCK | FUSE,	// areas
		avr8_program_area_map,				// program_area_map
		avr8_parse_argument,				// parse_argument
		avr8_probe_chip,					// probe_chip
		avr8_init,							// init
		avr8_fini,							// fini
		avr8_interface_needed,				// interfaces_needed
		avr8_prepare_buffer,				// prepare_buffer
		avr8_write_buffer_from_file_callback,
											// write_buffer_from_file_callback
		NULL,								// write_file_from_buffer_callback
		avr8_program,						// program
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// comisp
	{
		COMISP_STRING,						// name
		APPLICATION | EEPROM | LOCK | FUSE,	// areas
		comisp_program_area_map,			// program_area_map
		comisp_parse_argument,				// parse_argument
		comisp_probe_chip,					// probe_chip
		comisp_init,						// init
		comisp_fini,						// fini
		comisp_interface_needed,			// interfaces_needed
		comisp_prepare_buffer,				// prepare_buffer
		comisp_write_buffer_from_file_callback,
											// write_buffer_from_file_callback
		NULL,								// write_file_from_buffer_callback
		comisp_program,						// program
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	// svf_player
	{
		SVFP_STRING,						// name
		0,									// areas
		svfp_program_area_map,				// program_area_map
		svfp_parse_argument,				// parse_argument
		svfp_probe_chip,					// probe_chip
		svfp_init,							// init
		svfp_fini,							// fini
		svfp_interface_needed,				// interfaces_needed
		svfp_prepare_buffer,				// prepare_buffer
		svfp_write_buffer_from_file_callback,
											// write_buffer_from_file_callback
		NULL,								// write_file_from_buffer_callback
		svfp_program,						// program
		
		NULL,								// get_mass_product_data_size
		NULL,								// prepare_mass_product_data
	},
	{ NULL }
};
target_info_t *cur_target = NULL;
program_info_t program_info = {0};

RESULT target_alloc_data_buffer(void)
{
	if ((NULL == program_info.boot) && (program_info.boot_size > 0))
	{
		program_info.boot = malloc(program_info.boot_size);
		if (NULL == program_info.boot)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.boot_checksum) 
	   && (program_info.boot_checksum_size > 0))
	{
		program_info.boot_checksum = malloc(program_info.boot_checksum_size);
		if (NULL == program_info.boot_checksum)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.app) && (program_info.app_size > 0))
	{
		program_info.app = malloc(program_info.app_size);
		if (NULL == program_info.app)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.app_checksum) 
	   && (program_info.app_checksum_size > 0))
	{
		program_info.app_checksum = malloc(program_info.app_checksum_size);
		if (NULL == program_info.app_checksum)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.eeprom) && (program_info.eeprom_size > 0))
	{
		program_info.eeprom = malloc(program_info.eeprom_size);
		if (NULL == program_info.eeprom)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.eeprom_checksum) 
	   && (program_info.eeprom_checksum_size > 0))
	{
		program_info.eeprom_checksum = \
									malloc(program_info.eeprom_checksum_size);
		if (NULL == program_info.eeprom_checksum)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.otp_rom) && (program_info.otp_rom_size > 0))
	{
		program_info.otp_rom = malloc(program_info.otp_rom_size);
		if (NULL == program_info.otp_rom)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.otp_rom_checksum) 
	   && (program_info.otp_rom_checksum_size > 0))
	{
		program_info.otp_rom_checksum = \
									malloc(program_info.otp_rom_checksum_size);
		if (NULL == program_info.otp_rom_checksum)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.fuse) && (program_info.fuse_size > 0))
	{
		program_info.fuse = malloc(program_info.fuse_size);
		if (NULL == program_info.fuse)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.fuse_checksum) 
	   && (program_info.fuse_checksum_size > 0))
	{
		program_info.fuse_checksum = malloc(program_info.fuse_checksum_size);
		if (NULL == program_info.fuse_checksum)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.lock) && (program_info.lock_size > 0))
	{
		program_info.lock = malloc(program_info.lock_size);
		if (NULL == program_info.lock)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.lock_checksum) 
	   && (program_info.lock_checksum_size > 0))
	{
		program_info.lock_checksum = malloc(program_info.lock_checksum_size);
		if (NULL == program_info.lock_checksum)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.user_area) && (program_info.user_area_size > 0))
	{
		program_info.user_area = malloc(program_info.user_area_size);
		if (NULL == program_info.user_area)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	if ((NULL == program_info.user_area_checksum) 
	   && (program_info.user_area_checksum_size > 0))
	{
		program_info.user_area_checksum = \
								malloc(program_info.user_area_checksum_size);
		if (NULL == program_info.user_area_checksum)
		{
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
	}
	
	return ERROR_OK;
}

void target_free_data_buffer(void)
{
	if (program_info.boot != NULL)
	{
		free(program_info.boot);
		program_info.boot = NULL;
		program_info.boot_size = 0;
	}
	if (program_info.boot_checksum != NULL)
	{
		free(program_info.boot_checksum);
		program_info.boot_checksum = NULL;
		program_info.boot_checksum_size = 0;
	}
	if (program_info.boot_memlist != NULL)
	{
		MEMLIST_Free(&program_info.boot_memlist);
	}
	
	if (program_info.app != NULL)
	{
		free(program_info.app);
		program_info.app = NULL;
		program_info.app_size = 0;
	}
	if (program_info.app_checksum != NULL)
	{
		free(program_info.app_checksum);
		program_info.app_checksum = NULL;
		program_info.app_checksum_size = 0;
	}
	if (program_info.app_memlist != NULL)
	{
		MEMLIST_Free(&program_info.app_memlist);
	}
	
	if (program_info.otp_rom != NULL)
	{
		free(program_info.otp_rom);
		program_info.otp_rom = NULL;
		program_info.otp_rom_size = 0;
	}
	if (program_info.otp_rom_checksum != NULL)
	{
		free(program_info.otp_rom_checksum);
		program_info.otp_rom_checksum = NULL;
		program_info.otp_rom_checksum_size = 0;
	}
	if (program_info.opt_rom_memlist != NULL)
	{
		MEMLIST_Free(&program_info.opt_rom_memlist);
	}
	
	if (program_info.eeprom != NULL)
	{
		free(program_info.eeprom);
		program_info.eeprom = NULL;
		program_info.eeprom_size = 0;
	}
	if (program_info.eeprom_checksum != NULL)
	{
		free(program_info.eeprom_checksum);
		program_info.eeprom_checksum = NULL;
		program_info.eeprom_checksum_size = 0;
	}
	if (program_info.eeprom_memlist != NULL)
	{
		MEMLIST_Free(&program_info.eeprom_memlist);
	}
	
	if (program_info.fuse != NULL)
	{
		free(program_info.fuse);
		program_info.fuse = NULL;
		program_info.fuse_size = 0;
	}
	if (program_info.fuse_checksum != NULL)
	{
		free(program_info.fuse_checksum);
		program_info.fuse_checksum = NULL;
		program_info.fuse_checksum_size = 0;
	}
	if (program_info.fuse_memlist != NULL)
	{
		MEMLIST_Free(&program_info.fuse_memlist);
	}
	
	if (program_info.lock != NULL)
	{
		free(program_info.lock);
		program_info.lock = NULL;
		program_info.lock_size = 0;
	}
	if (program_info.lock_checksum != NULL)
	{
		free(program_info.lock_checksum);
		program_info.lock_checksum = NULL;
		program_info.lock_checksum_size = 0;
	}
	if (program_info.lock_memlist != NULL)
	{
		MEMLIST_Free(&program_info.lock_memlist);
	}
	
	if (program_info.user_area != NULL)
	{
		free(program_info.user_area);
		program_info.user_area = NULL;
		program_info.user_area_size = 0;
	}
	if (program_info.user_area_checksum != NULL)
	{
		free(program_info.user_area_checksum);
		program_info.user_area_checksum = NULL;
		program_info.user_area_checksum_size = 0;
	}
	if (program_info.user_area_memlist != NULL)
	{
		MEMLIST_Free(&program_info.user_area_memlist);
	}
}

void target_print_list(void)
{
	uint32 i;
	
	printf(_GETTEXT("Supported targets:\n"));
	for (i = 0; targets_info[i].name != NULL; i++)
	{
		targets_info[i].parse_argument('S', NULL);
	}
}

void target_print_help(void)
{
	uint32 i;
	
	for (i = 0; targets_info[i].name != NULL; i++)
	{
		targets_info[i].parse_argument('h', NULL);
	}
}

uint32 target_get_number(void)
{
	return sizeof(targets_info) / sizeof(targets_info[0]) - 1;
}

RESULT target_init(program_info_t *pi)
{
	uint32 i;
	
#if PARAM_CHECK
	if ((NULL == pi) || ((NULL == pi->chip_name) && (NULL == pi->chip_type)))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (NULL == pi->chip_type)
	{
		// find which series of target contain current chip_name
		for (i = 0; targets_info[i].name != NULL; i++)
		{
			if (targets_info[i].probe_chip(pi->chip_name) == ERROR_OK)
			{
				cur_target = &targets_info[i];
				pi->chip_type = (char *)targets_info[i].name;
				LOG_DEBUG(_GETTEXT("%s initialized for %s.\n"), 
						 cur_target->name, pi->chip_name);
				
				return ERROR_OK;
			}
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
				if ((pi->chip_name != NULL) 
				   && (ERROR_OK != targets_info[i].probe_chip(pi->chip_name)))
				{
					LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), 
							  pi->chip_name, targets_info[i].name);
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

