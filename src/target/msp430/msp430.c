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

#define CUR_TARGET_STRING			MSP430_STRING
#define cur_chip_param				msp430_chip_param
#define cur_chips_param				msp430_chips_param
#define cur_prog_mode				msp430_prog_mode

const program_area_map_t msp430_program_area_map[] = 
{
	{APPLICATION, 'f', 1},
	{0, 0}
};

/*const*/ msp430_param_t msp430_chips_param[] = {
//	chip_name,		chip_id,	program_mode,																													flash_page_size,	flash_page_num,	info_page_size,	info_page_num,	ram_start,	ram_end,	main_start,
	{"msp430f112",	0xF112,		MSP430_MODE_JTAG | MSP430_MODE_TEST | MSP430_MODE_DATAQUICK,																	256,				239,			128,			2,				0x0200,		0x02FF,		0xF000},
	{"msp430f1132",	0x1132,		MSP430_MODE_JTAG | MSP430_MODE_TEST | MSP430_MODE_DATAQUICK,																	256,				239,			128,			2,				0x0200,		0x02FF,		0xE000},
	{"msp430f123",	0xF123,		MSP430_MODE_JTAG | MSP430_MODE_TEST,																							256,				239,			128,			2,				0x0200,		0x02FF,		0xE000},
	{"msp430f1232",	0x1232,		MSP430_MODE_JTAG | MSP430_MODE_TEST | MSP430_MODE_DATAQUICK,																	256,				239,			128,			2,				0x0200,		0x02FF,		0xE000},
	{"msp430f149",	0xF149,		MSP430_MODE_JTAG | MSP430_MODE_DATAQUICK,																						256,				239,			128,			2,				0x0200,		0x09FF,		0x1100},
	{"msp430f169",	0xF169,		MSP430_MODE_JTAG | MSP430_MODE_DATAQUICK,																						256,				239,			128,			2,				0x0200,		0x09FF,		0x1100},
	{"msp430f1610",	0xF16C,		MSP430_MODE_JTAG | MSP430_MODE_DATAQUICK,																						256,				239,			128,			2,				0x1100,		0x24FF,		0x8000},
	{"msp430f2013",	0xF201,		MSP430_MODE_JTAG | MSP430_MODE_SBW | MSP430_MODE_TEST | MSP430_MODE_DATAQUICK | MSP430_MODE_FASTFLASH,							256,				239,			128,			2,				0x0200,		0x027F,		0xF800},
	{"msp430f2131",	0xF213,		MSP430_MODE_JTAG | MSP430_MODE_TEST | MSP430_MODE_DATAQUICK | MSP430_MODE_FASTFLASH,											256,				239,			128,			2,				0x0200,		0x02FF,		0xE000},
	{"msp430f2274",	0xF227,		MSP430_MODE_JTAG | MSP430_MODE_SBW | MSP430_MODE_TEST | MSP430_MODE_DATAQUICK | MSP430_MODE_FASTFLASH | MSP430_MODE_ENHVERIFY,	256,				239,			128,			2,				0x0200,		0x05FF,		0x8000},
	{"msp430f2370",	0xF237,		MSP430_MODE_JTAG | MSP430_MODE_TEST | MSP430_MODE_DATAQUICK | MSP430_MODE_FASTFLASH | MSP430_MODE_ENHVERIFY,					256,				239,			128,			2,				0x0200,		0x09FF,		0x8000},
	{"msp430f249",	0xF249,		MSP430_MODE_JTAG | MSP430_MODE_DATAQUICK | MSP430_MODE_FASTFLASH | MSP430_MODE_ENHVERIFY,										256,				239,			128,			2,				0x0200,		0x09FF,		0x1100},
	{"msp430f2619",	0xF26F,		MSP430_MODE_JTAG | MSP430_MODE_CPUX | MSP430_MODE_DATAQUICK | MSP430_MODE_FASTFLASH | MSP430_MODE_ENHVERIFY,					256,				239,			128,			2,				0x1100,		0x20FF,		0x2100},
	{"msp430f413",	0xF413,		MSP430_MODE_JTAG,																												256,				239,			128,			2,				0x0200,		0x02FF,		0xE000},
	{"msp430fw427",	0xF427,		MSP430_MODE_JTAG | MSP430_MODE_DATAQUICK,																						256,				239,			128,			2,				0x0200,		0x02FF,		0xE000},
	{"msp430f437",	0xF437,		MSP430_MODE_JTAG | MSP430_MODE_DATAQUICK,																						256,				239,			128,			2,				0x0200,		0x05FF,		0xA000},
	{"msp430fg439",	0xF439,		MSP430_MODE_JTAG | MSP430_MODE_DATAQUICK,																						256,				239,			128,			2,				0x0200,		0x09FF,		0x1100},
	{"msp430f449",	0xF449,		MSP430_MODE_JTAG | MSP430_MODE_DATAQUICK,																						256,				239,			128,			2,				0x0200,		0x09FF,		0x1100},
	{"msp430fg4619",0xF46F,		MSP430_MODE_JTAG | MSP430_MODE_CPUX | MSP430_MODE_DATAQUICK | MSP430_MODE_FASTFLASH | MSP430_MODE_ENHVERIFY,					256,				239,			128,			2,				0x1100,		0x20FF,		0x2100},
};
msp430_param_t msp430_chip_param;
uint8 msp430_prog_mode = 0;

void msp430_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                set mode<j|s|b>\n\n", CUR_TARGET_STRING);
}

void msp430_support(void)
{
	uint32 i;
	
	printf("Support list of %s:\n", CUR_TARGET_STRING);
	for (i = 0; i < dimof(msp430_chips_param); i++)
	{
		printf("%s: id = 0x%04x, prog_mode = 0x%02x\n", 
			   msp430_chips_param[i].chip_name, msp430_chips_param[i].chip_id,
			   msp430_chips_param[i].program_mode | MSP430_MODE_BSL);
	}
	printf("\n");
}

RESULT msp430_parse_argument(char cmd, const char *argu)
{
	switch (cmd)
	{
	case 'h':
		msp430_usage();
		break;
	case 'S':
		msp430_support();
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
		case 'j':
			// Jtag
			msp430_prog_mode = MSP430_MODE_JTAG;
			break;
		case 's':
			// Sbw
			msp430_prog_mode = MSP430_MODE_SBW;
			break;
		case 'b':
			// Bsl
			msp430_prog_mode = MSP430_MODE_BSL;
			break;
		default:
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHARACTER_MESSAGE), cmd, 
					  "msp430 program mode", "MUST be 'j', 's' or 'b'!!");
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

RESULT msp430_probe_chip(char *chip_name)
{
	uint32 i;
	
	for (i = 0; i < dimof(msp430_chips_param); i++)
	{
		if (!strcmp(msp430_chips_param[i].chip_name, chip_name))
		{
			return ERROR_OK;
		}
	}
	
	return ERROR_FAIL;
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

RESULT msp430_write_buffer_from_file_callback(uint32 address, uint32 seg_addr, 
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
	
	switch (address >> 16)
	{
	case 0x0000:
		if (mem_addr >= msp430_chip_param.main_start)
		{
			if (NULL == pi->app)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), 
						  TO_STR(c8051f_flash_buffer));
				return ERRCODE_INVALID_BUFFER;
			}
			
			if ((mem_addr > (uint32)(msp430_chip_param.main_start 
									 + msp430_chip_param.flash_size)) 
				|| (length > msp430_chip_param.flash_size) 
				|| ((mem_addr + length) 
					> (uint32)(msp430_chip_param.main_start 
							   + msp430_chip_param.flash_size)))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "flash memory");
			return ERRCODE_INVALID;
			}
			memcpy(pi->app + mem_addr 
				   - msp430_chip_param.main_start, data, length);
			pi->app_size_valid += (uint16)length;
			
			ret = MEMLIST_Add(&pi->app_memlist, mem_addr, length, 
							  msp430_chip_param.flash_page_size);
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
		msp430_chip_param.program_mode = MSP430_PROG_MODE_MASK;

		if (ERROR_OK != msp430_program(opt_tmp, pi, prog))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
			return ERRCODE_AUTODETECT_FAIL;
		}
		
		LOG_INFO(_GETTEXT(INFOMSG_AUTODETECT_SIGNATURE), pi->chip_id);
		for (i = 0; i < dimof(msp430_chips_param); i++)
		{
			if (pi->chip_id == msp430_chips_param[i].chip_id)
			{
				memcpy(&msp430_chip_param, msp430_chips_param + i, 
					   sizeof(msp430_chip_param));
				msp430_chip_param.flash_size = 
										msp430_chip_param.flash_page_num 
										* msp430_chip_param.flash_page_size;
				
				pi->app_size = msp430_chip_param.flash_size;
				pi->app_size_valid = 0;
				
				LOG_INFO(_GETTEXT(INFOMSG_CHIP_FOUND), 
						 cur_chip_param.chip_name);
				pi->chip_name = (char *)msp430_chip_param.chip_name;

				return ERROR_OK;
			}
		}
		
		LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
		return ERRCODE_AUTODETECT_FAIL;
	}
	else
	{
		for (i = 0; i < dimof(msp430_chips_param); i++)
		{
			if (!strcmp(msp430_chips_param[i].chip_name, pi->chip_name))
			{
				memcpy(&msp430_chip_param, msp430_chips_param + i, 
					   sizeof(msp430_chip_param));
				msp430_chip_param.flash_size = 
										msp430_chip_param.flash_page_num 
										* msp430_chip_param.flash_page_size;
				
				pi->app_size = msp430_chip_param.flash_size;
				pi->app_size_valid = 0;
				
				return ERROR_OK;
			}
		}
		
		return ERROR_FAIL;
	}
}

uint32 msp430_interface_needed(void)
{
	switch(msp430_prog_mode & MSP430_PROG_MODE_MASK)
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
RESULT (*msp430jtagsbw_config)(uint8 has_test);
RESULT (*msp430jtagsbw_ir)(uint8 *ir, uint8 want_ret);
RESULT (*msp430jtagsbw_dr)(uint32 *dr, uint8 len, uint8 want_ret);
RESULT (*msp430jtagsbw_tclk)(uint8 value);
RESULT (*msp430jtagsbw_tclk_strobe)(uint16 cnt);
RESULT (*msp430jtagsbw_reset)(void);
RESULT (*msp430jtagsbw_poll)(uint32 dr, uint32 mask, uint32 value, uint8 len, 
							 uint16 poll_cnt, uint8 toggle_tclk);


#define get_target_voltage(v)					prog->get_target_voltage(v)
RESULT msp430_program(operation_t operations, program_info_t *pi, 
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
#endif
	
	if ((operations.read_operations & APPLICATION) 
		&& !(operations.verify_operations & APPLICATION))
	{
		pi->app_size_valid = msp430_chip_param.flash_size;
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
	
	switch(msp430_prog_mode & MSP430_PROG_MODE_MASK)
	{
	case 0:
		LOG_WARNING(_GETTEXT(INFOMSG_USE_DEFAULT), "Program interface", "JTAG");
		msp430_prog_mode = MSP430_MODE_JTAG;
	case MSP430_MODE_JTAG:
		if (msp430_chip_param.program_mode & MSP430_MODE_JTAG)
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
		if (msp430_chip_param.program_mode & MSP430_MODE_SBW)
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
		if (msp430_chip_param.program_mode & MSP430_MODE_BSL)
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

