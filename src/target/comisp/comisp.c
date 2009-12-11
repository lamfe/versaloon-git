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

#include "port.h"
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

#include "comisp.h"
#include "stm32isp.h"
#include "lpcarmisp.h"

#include "comisp_internal.h"

#include "comport.h"

#define CUR_TARGET_STRING			COMISP_STRING
#define cur_chip_param				comisp_chip_param
#define cur_chips_param				comisp_chips_param
#define cur_flash_offset			comisp_flash_offset
#define cur_target_defined			target_defined

#define COMISP_MAX_BUFSIZE			(1024 * 1024)

const program_area_map_t comisp_program_area_map[] = 
{
	{APPLICATION, APPLICATION_CHAR, 1, 0, 0},
//	{LOCK, LOCK_CHAR, 0, 0, 0},
	{0, 0, 0, 0, 0}
};

#define COMISP_STM32		0
#define COMISP_LPCARM		1
#define COMISP_TEST			2
const comisp_param_t comisp_chips_param[] = {
//	chip_name,			com_mode,																												default_char,		flash_start_addr
//						{comport,	baudrate,	datalength,	paritybit,			stopbit,		handshake,				aux_pin}	
	{"comisp_stm32",	{"",		-1,			8,			COMM_PARITYBIT_EVEN,COMM_STOPBIT_1,	COMM_PARAMETER_UNSURE,	COMM_PARAMETER_UNSURE},	STM32_FLASH_CHAR,	0x08000000},
	{"comisp_lpcarm",	{"",		-1,			8,			COMM_PARITYBIT_NONE,COMM_STOPBIT_1,	COMM_PARAMETER_UNSURE,	COMM_PARAMETER_UNSURE},	LPCARM_FLASH_CHAR,	0x00000000},
	{"comisp_test",		{"",		-1,			8,			COMM_PARITYBIT_NONE,COMM_STOPBIT_1,	COMM_PARAMETER_UNSURE,	COMM_PARAMETER_UNSURE},	0x00,				0x00000000},
};
static uint8_t comisp_chip_index = 0;
comisp_param_t comisp_chip_param;

uint8_t comisp_execute_flag = 0;
uint32_t comisp_execute_addr = 0;

com_mode_t com_mode = 
{"", 115200, 8, COMM_PARITYBIT_NONE, COMM_STOPBIT_1, 
COMM_HANDSHAKE_NONE, COMM_AUXPIN_DISABLE};

static uint32_t comisp_flash_offset = 0;
static uint32_t comisp_test_buffsize = 0;

static void comisp_usage(void)
{
	printf("\
Usage of %s:\n\
  -C,  --comport <COMM_ATTRIBUTE>           set com port\n\
  -x,  --execute <ADDRESS>                  execute program\n\n", 
		   CUR_TARGET_STRING);
}

static void comisp_support(void)
{
	uint32_t i;

	printf("Support list of %s:\n", CUR_TARGET_STRING);
	for (i = 0; i < dimof(cur_chips_param); i++)
	{
		printf("\
%s: baudrate = %d, datalength = %d, paritybit = %c, stopbit = %c, \
handshake = %c, auxpin = %c\n",
				cur_chips_param[i].chip_name, 
				cur_chips_param[i].com_mode.baudrate,
				cur_chips_param[i].com_mode.datalength,
				cur_chips_param[i].com_mode.paritybit,
				cur_chips_param[i].com_mode.stopbit,
				cur_chips_param[i].com_mode.handshake,
				cur_chips_param[i].com_mode.auxpin);
	}
	printf("\n");
}

RESULT comisp_parse_argument(char cmd, const char *argu)
{
	char *end_pointer, *cur_pointer;
	
	switch (cmd)
	{
	case 'h':
		comisp_usage();
		break;
	case 'S':
		comisp_support();
		break;
	case 'b':
		if ((NULL == argu) || (strlen(argu) == 0))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		comisp_test_buffsize = (uint32_t)strtoul(argu, NULL, 0);
		break;
	case 'C':
		// COM Mode
		if ((NULL == argu) || (strlen(argu) == 0))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		// Format: port:baudrate_attribute_extra
		// Eg: COM1:115200_8N1_HA
		// parse "port:*"
		cur_pointer = strrchr(argu, ':');
		if (NULL == cur_pointer)
		{
			strncpy(com_mode.comport, argu, sizeof(com_mode.comport));
			return ERROR_OK;
		}
		else
		{
			*cur_pointer = '\0';
			strncpy(com_mode.comport, argu, sizeof(com_mode.comport));
		}
		
		cur_pointer++;
		if (*cur_pointer != '\0')
		{
			// parse "baudrate*"
			com_mode.baudrate = (uint32_t)strtoul(cur_pointer, &end_pointer, 0);
			
			if (cur_pointer == end_pointer)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
				return ERRCODE_INVALID_OPTION;
			}
			
			cur_pointer = end_pointer;
			if ((*cur_pointer != '\0'))
			{
				// parse "_attribute*"
				if ((strlen(cur_pointer) < 4) 
					|| ((cur_pointer[0] != ' ') && (cur_pointer[0] != '-') 
						&& (cur_pointer[0] != '_')) 
					|| (cur_pointer[1] < '5') || (cur_pointer[1] > '9') 
					|| ((toupper(cur_pointer[2]) != 'N') 
						&& (toupper(cur_pointer[2]) != 'E') 
						&& (toupper(cur_pointer[2]) != 'O')) 
					|| ((cur_pointer[3] != '1') 
						&& (cur_pointer[3] != '0')))
				{
					LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
					return ERRCODE_INVALID_OPTION;
				}
				
				com_mode.datalength = cur_pointer[1] - '0';
				com_mode.paritybit = cur_pointer[2];
				com_mode.stopbit = cur_pointer[3] - '0';
				
				cur_pointer += 4;
				if (*cur_pointer != '\0')
				{
					// parse "_extra"
					if ((strlen(cur_pointer) != 3) 
						|| ((cur_pointer[0] != ' ') && (cur_pointer[0] != '-') 
							&&(cur_pointer[0] != '_')) 
						|| ((toupper(cur_pointer[1]) != 'H') 
							&& (toupper(cur_pointer[1]) != 'N')) 
						|| ((toupper(cur_pointer[2]) != 'A') 
							&& (toupper(cur_pointer[2]) != 'N')))
					{
						LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
						return ERRCODE_INVALID_OPTION;
					}
					
					com_mode.handshake = cur_pointer[1];
					com_mode.auxpin = cur_pointer[2];
				}
			}
		}
		break;
	case 'x':
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		comisp_execute_addr = (uint32_t)strtoul(argu, NULL, 0);
		comisp_execute_flag = 1;
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT comisp_probe_chip(char *chip_name)
{
	uint32_t i;
	
	for (i = 0; i < dimof(cur_chips_param); i++)
	{
		if (!strcmp(cur_chips_param[i].chip_name, chip_name))
		{
			return ERROR_OK;
		}
	}
	
	return ERROR_FAIL;
}

RESULT comisp_prepare_buffer(program_info_t *pi)
{
	if (pi->app != NULL)
	{
		memset(pi->app, cur_chip_param.default_char, pi->app_size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT comisp_write_buffer_from_file_callback(uint32_t address, uint32_t seg_addr, 
											  uint8_t* data, uint32_t length, 
											  void* buffer)
{
	program_info_t *pi = (program_info_t *)buffer;
	uint32_t mem_addr = address, page_size;
	RESULT ret;
	
	seg_addr = seg_addr;
	
#ifdef PARAM_CHECK
	if ((length > 0) && (NULL == data))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if (NULL == pi->app)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), 
				  "pi->app");
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	mem_addr += cur_flash_offset;
	if (((mem_addr - cur_chip_param.flash_start_addr) >= COMISP_MAX_BUFSIZE) 
		|| (length > COMISP_MAX_BUFSIZE) 
		|| ((mem_addr - cur_chip_param.flash_start_addr + length) 
			> COMISP_MAX_BUFSIZE))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "flash memory");
		return ERRCODE_INVALID;
	}
	cur_target_defined |= APPLICATION;
	
	memcpy(pi->app + mem_addr - cur_chip_param.flash_start_addr, 
		   data, length);
	pi->app_size_valid += (uint32_t)length;
	
	switch (comisp_chip_index)
	{
	case COMISP_STM32:
		page_size = STM32ISP_PAGE_SIZE;
		break;
	case COMISP_LPCARM:
		page_size = LPCARMISP_PAGE_SIZE;
		break;
	default:
		// invalid target
		LOG_BUG(_GETTEXT(ERRMSG_INVALID), TO_STR(comisp_chip_index), 
				CUR_TARGET_STRING);
		return ERRCODE_INVALID;
	}
	
	ret = MEMLIST_Add(&pi->app_memlist, mem_addr, length, page_size);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	return ERROR_OK;
}

RESULT comisp_fini(program_info_t *pi, programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	if (strlen(com_mode.comport) > 0)
	{
		strcpy(com_mode.comport, "");
	}
	
	return ERROR_OK;
}

RESULT comisp_init(program_info_t *pi, programmer_info_t *prog)
{
	uint8_t i;
	
	prog = prog;
	
	if (strcmp(pi->chip_type, CUR_TARGET_STRING))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_HANDLER), CUR_TARGET_STRING, 
				pi->chip_type);
		return ERRCODE_INVALID_HANDLER;
	}
	
	if (NULL == pi->chip_name)
	{
		// auto detect
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "Auto-detect", 
				  CUR_TARGET_STRING);
		return ERRCODE_NOT_SUPPORT;
	}
	else
	{
		for (i = 0; i < dimof(cur_chips_param); i++)
		{
			if (!strcmp(cur_chips_param[i].chip_name, pi->chip_name))
			{
				comisp_chip_index = i;
				memcpy(&cur_chip_param, cur_chips_param + comisp_chip_index, 
					   sizeof(cur_chip_param));
				
				pi->app_size = COMISP_MAX_BUFSIZE;
				pi->app_size_valid = 0;
				
				return ERROR_OK;
			}
		}
		
		return ERROR_FAIL;
	}
}

uint32_t comisp_interface_needed(void)
{
	// comisp uses COM ports only
	return 0;
}

RESULT comisp_program(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog)
{
	RESULT ret = ERROR_OK;
	uint32_t retry;
	uint8_t *buff_w = NULL, *buff_r = NULL;
	int32_t comm_ret;
	
	pi = pi;
	prog = prog;
	
	if (!strlen(com_mode.comport))
	{
		strncpy(com_mode.comport, DEFAULT_COMPORT, sizeof(com_mode.comport));
		LOG_WARNING(_GETTEXT(INFOMSG_USE_DEFAULT), "Com port", 
					DEFAULT_COMPORT);
	}
	
	switch(comisp_chip_index)
	{
	case COMISP_TEST:
		if (0 == comisp_test_buffsize)
		{
			LOG_INFO(_GETTEXT("buffsize not defined, use 64 for default.\n"));
			comisp_test_buffsize = 64;
		}
		buff_w = (uint8_t*)malloc(comisp_test_buffsize);
		buff_r = (uint8_t*)malloc(comisp_test_buffsize);
		if ((NULL == buff_r) || (NULL == buff_w))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
			ret = ERRCODE_NOT_ENOUGH_MEMORY;
			goto comtest_end;
		}
		
		ret = comm_open(com_mode.comport, com_mode.baudrate, 8, 
					COMM_PARITYBIT_EVEN, COMM_STOPBIT_1, COMM_HANDSHAKE_NONE);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPEN), com_mode.comport);
			ret = ERROR_FAIL;
			goto comtest_end;
		}
		
		while(1)
		{
			comm_ret = comm_read(buff_r, 1);
			if (comm_ret < 0)
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_HANDLE_DEVICE), 
						  "read", com_mode.comport);
				ret = ERRCODE_FAILURE_OPERATION;
				goto comtest_end;
			}
			else if (comm_ret == 0)
			{
				break;
			}
		}
		
		LOG_INFO(_GETTEXT("start to run com test for buffsize = %d.\n"), 
				 comisp_test_buffsize);
		retry = 0;
		while(1)
		{
			uint32_t i;
			
			for (i = 0; i < comisp_test_buffsize; i++)
			{
				buff_w[i] = (uint8_t)(i/* ^ retry*/);
			}
			
			// send
			comm_ret = comm_write(buff_w, comisp_test_buffsize);
			if (comm_ret < 0)
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_HANDLE_DEVICE), 
						  "write", com_mode.comport);
				ret = ERRCODE_FAILURE_OPERATION;
				goto comtest_end;
			}
			if (comm_ret != (int32_t)comisp_test_buffsize)
			{
				LOG_ERROR("Fail to send %d bytes, %d bytes sent.\n", 
						  comisp_test_buffsize, 
						  comm_ret);
				ret = ERROR_FAIL;
				goto comtest_end;
			}
			// read
			memset(buff_r, 0, comisp_test_buffsize);
			comm_ret = comm_read(buff_r, comisp_test_buffsize);
			if (comm_ret < 0)
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_HANDLE_DEVICE), 
						  "read", com_mode.comport);
				ret = ERRCODE_FAILURE_OPERATION;
				goto comtest_end;
			}
			if (comm_ret != (int32_t)comisp_test_buffsize)
			{
				LOG_ERROR("Fail to receive %d bytes, %d bytes received.\n", 
						  comisp_test_buffsize, 
						  comm_ret);
				ret = ERROR_FAIL;
			}
			// check
			for (i = 0; i < comisp_test_buffsize; i++)
			{
				if (buff_w[i] != buff_r[i])
				{
					LOG_ERROR("Data error at %d.\n", i);
					goto comtest_end;
//					break;
				}
			}
			retry++;
			
			comm_ret = comm_read(buff_r, 1);
			if (comm_ret < 0)
			{
				LOG_DEBUG(_GETTEXT(ERRMSG_FAILURE_HANDLE_DEVICE), 
						  "write", com_mode.comport);
				ret = ERRCODE_FAILURE_OPERATION;
				goto comtest_end;
			}
			else if (comm_ret > 0)
			{
				LOG_ERROR("Too many data received.\n");
				goto comtest_end;
			}
			
			LOG_INFO(_GETTEXT("round %d OK.\n"), retry);
		}
		
comtest_end:
		comm_close();
		comisp_test_buffsize = 0;
		if (buff_w != NULL)
		{
			free(buff_w);
			buff_w = NULL;
		}
		if (buff_r != NULL)
		{
			free(buff_r);
			buff_r = NULL;
		}
		return ret;
	case COMISP_STM32:
		return stm32isp_program(operations, pi);
	case COMISP_LPCARM:
		return lpcarmisp_program(operations, pi);
	default:
		// invalid target
		LOG_BUG(_GETTEXT(ERRMSG_INVALID), TO_STR(comisp_chip_index), 
				CUR_TARGET_STRING);
		return ERRCODE_INVALID;
	}
}

