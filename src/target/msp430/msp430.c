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

#include "msp430.h"
#include "msp430_internal.h"

#include "JTAGfunc.h"

#define CUR_TARGET_STRING			MSP430_STRING
#define cur_chip_param				target_chip_param
#define cur_chips_param				target_chips.chips_param
#define cur_chips_num				target_chips.num_of_chips
#define cur_prog_mode				program_mode
#define cur_target_defined			target_defined

const program_area_map_t msp430_program_area_map[] = 
{
	{APPLICATION, APPLICATION_CHAR, 1},
	{0, 0, 0}
};

void msp430_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                set mode<j|s|b>\n\n", CUR_TARGET_STRING);
}

void msp430_support(void)
{
	uint32_t i;
	
	printf("Support list of %s:\n", CUR_TARGET_STRING);
	for (i = 0; i < cur_chips_num; i++)
	{
		printf("%s: id = 0x%04x, prog_mode = %s\n", 
			   cur_chips_param[i].chip_name, cur_chips_param[i].chip_id,
			   cur_chips_param[i].program_mode_str);
	}
	printf("\n");
}

RESULT msp430_parse_argument(char cmd, const char *argu)
{
	argu = argu;
	
	switch (cmd)
	{
	case 'h':
		msp430_usage();
		break;
	case 'S':
		msp430_support();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT msp430_prepare_buffer(program_info_t *pi)
{
	if (pi->app != NULL)
	{
		memset(pi->app, MSP430_FLASH_CHAR, pi->app_size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT msp430_write_buffer_from_file_callback(uint32_t address, uint32_t seg_addr, 
											  uint8_t* data, uint32_t length, 
											  void* buffer)
{
	program_info_t *pi = (program_info_t *)buffer;
	uint32_t mem_addr = address & 0x0000FFFF;
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
	
	switch (address >> 16)
	{
	case 0x0000:
		if (mem_addr >= Device_MainStart())
		{
			if (NULL == pi->app)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), 
						  TO_STR(c8051f_flash_buffer));
				return ERRCODE_INVALID_BUFFER;
			}
			
			if ((mem_addr > 
					(uint32_t)(Device_MainStart() + cur_chip_param.app_size)) 
				|| (length > cur_chip_param.app_size) 
				|| ((mem_addr + length) 
					> (uint32_t)(Device_MainStart() + cur_chip_param.app_size)))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "flash memory");
				return ERRCODE_INVALID;
			}
			cur_target_defined |= APPLICATION;
			
			memcpy(pi->app + mem_addr - Device_MainStart(), data, length);
			pi->app_size_valid += (uint16_t)length;
			
			ret = MEMLIST_Add(&pi->app_memlist, mem_addr, length, 
							  cur_chip_param.app_page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_ADDRESS), address, 
				  CUR_TARGET_STRING);
			return ERRCODE_INVALID;
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

RESULT msp430_fini(program_info_t *pi, programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	return ERROR_OK;
}

RESULT msp430_init(program_info_t *pi, const char *dir, 
				   programmer_info_t *prog)
{
	uint8_t i;
	operation_t opt_tmp;
	
	dir = dir;
	memset(&opt_tmp, 0, sizeof(opt_tmp));
	
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
		cur_chip_param.program_mode = MSP430_PROG_MODE_MASK;

		if (ERROR_OK != msp430_program(opt_tmp, pi, prog))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
			return ERRCODE_AUTODETECT_FAIL;
		}
		
		LOG_INFO(_GETTEXT(INFOMSG_AUTODETECT_SIGNATURE), pi->chip_id);
		for (i = 0; i < cur_chips_num; i++)
		{
			if (pi->chip_id == cur_chips_param[i].chip_id)
			{
				memcpy(&cur_chip_param, cur_chips_param + i, 
					   sizeof(cur_chip_param));
				
				pi->app_size = cur_chip_param.app_size;
				pi->app_size_valid = 0;
				
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
		for (i = 0; i < cur_chips_num; i++)
		{
			if (!strcmp(cur_chips_param[i].chip_name, pi->chip_name))
			{
				memcpy(&cur_chip_param, cur_chips_param + i, 
					   sizeof(cur_chip_param));
				
				pi->app_size = cur_chip_param.app_size;
				pi->app_size_valid = 0;
				
				return ERROR_OK;
			}
		}
		
		return ERROR_FAIL;
	}
}

uint32_t msp430_interface_needed(void)
{
	switch(cur_prog_mode & MSP430_PROG_MODE_MASK)
	{
	case 0:			// default is JTAG
	case MSP430_MODE_JTAG:
		return MSP430_JTAG;
	case MSP430_MODE_SBW:
		return MSP430_SBW;
	case MSP430_MODE_BSL:
		return USART | GPIO;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), cur_prog_mode, 
				  CUR_TARGET_STRING);
		return INVALID_INTERFACE;
	}
}

RESULT (*msp430jtagsbw_init)(void);
RESULT (*msp430jtagsbw_fini)(void);
RESULT (*msp430jtagsbw_config)(uint8_t has_test);
RESULT (*msp430jtagsbw_ir)(uint8_t *ir, uint8_t want_ret);
RESULT (*msp430jtagsbw_dr)(uint32_t *dr, uint8_t len, uint8_t want_ret);
RESULT (*msp430jtagsbw_tclk)(uint8_t value);
RESULT (*msp430jtagsbw_tclk_strobe)(uint16_t cnt);
RESULT (*msp430jtagsbw_reset)(void);
RESULT (*msp430jtagsbw_poll)(uint32_t dr, uint32_t mask, uint32_t value, uint8_t len, 
							 uint16_t poll_cnt, uint8_t toggle_tclk);


#define get_target_voltage(v)					prog->get_target_voltage(v)
RESULT msp430_program(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog)
{
	uint16_t voltage;
	
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
#endif
	
	if ((operations.read_operations & APPLICATION) 
		&& !(operations.verify_operations & APPLICATION))
	{
		pi->app_size_valid = cur_chip_param.app_size;
	}

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
	
	switch(cur_prog_mode & MSP430_PROG_MODE_MASK)
	{
	case 0:
		LOG_WARNING(_GETTEXT(INFOMSG_USE_DEFAULT), "Program interface", "JTAG");
		cur_prog_mode = MSP430_MODE_JTAG;
	case MSP430_MODE_JTAG:
		if (cur_chip_param.program_mode & MSP430_MODE_JTAG)
		{
			msp430jtagsbw_init = prog->msp430jtag_init;
			msp430jtagsbw_fini = prog->msp430jtag_fini;
			msp430jtagsbw_config = prog->msp430jtag_config;
			msp430jtagsbw_ir = prog->msp430jtag_ir;
			msp430jtagsbw_dr = prog->msp430jtag_dr;
			msp430jtagsbw_tclk = prog->msp430jtag_tclk;
			msp430jtagsbw_tclk_strobe = prog->msp430jtag_tclk_strobe;
			msp430jtagsbw_reset = prog->msp430jtag_reset;
			msp430jtagsbw_poll = prog->msp430jtag_poll;
			
			return msp430_jtag_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "JTAG", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		break;
	case MSP430_MODE_SBW:
		if (cur_chip_param.program_mode & MSP430_MODE_SBW)
		{
			msp430jtagsbw_init = prog->msp430sbw_init;
			msp430jtagsbw_fini = prog->msp430sbw_fini;
			msp430jtagsbw_config = prog->msp430sbw_config;
			msp430jtagsbw_ir = prog->msp430sbw_ir;
			msp430jtagsbw_dr = prog->msp430sbw_dr;
			msp430jtagsbw_tclk = prog->msp430sbw_tclk;
			msp430jtagsbw_tclk_strobe = prog->msp430sbw_tclk_strobe;
			msp430jtagsbw_reset = prog->msp430sbw_reset;
			msp430jtagsbw_poll = prog->msp430sbw_poll;
			
			return msp430_jtag_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "SBW", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		
		break;
	case MSP430_MODE_BSL:
		if (cur_chip_param.program_mode & MSP430_MODE_BSL)
		{
			return msp430_bsl_program(operations, pi, prog);
		}
		else
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "BSL", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		
		break;
	default:
		// invalid mode
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), cur_prog_mode, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID_PROG_MODE;
		break;
	}
}

