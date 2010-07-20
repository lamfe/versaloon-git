/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
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
#include "strparser.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "comisp.h"
#include "stm32isp.h"
#include "lpcarmisp.h"

#include "comisp_internal.h"

#include "comport.h"

#define CUR_TARGET_STRING			COMISP_STRING

ENTER_PROGRAM_MODE_HANDLER(comisp);
struct program_functions_t comisp_program_functions;

const struct comisp_param_t comisp_chips_param[] = {
//	chip_name,			com_mode,																											
//						{comport,	baudrate,	datalength,	paritybit,			stopbit,		handshake,				aux_pin},				program_functions
	{"comisp_stm32",	{"",		-1,			8,			COMM_PARITYBIT_EVEN,COMM_STOPBIT_1,	COMM_PARAMETER_UNSURE,	COMM_PARAMETER_UNSURE}, &stm32isp_program_functions},
	{"comisp_lpcarm",	{"",		-1,			8,			COMM_PARITYBIT_NONE,COMM_STOPBIT_1,	COMM_PARAMETER_UNSURE,	COMM_PARAMETER_UNSURE}, NULL},
};
static uint8_t comisp_chip_index = 0;

uint8_t comisp_mode_offset = 0;
uint8_t comisp_execute_flag = 0;
uint32_t comisp_execute_addr = 0;

struct com_mode_t com_mode = 
{"", 115200, 8, COMM_PARITYBIT_NONE, COMM_STOPBIT_1, 
COMM_HANDSHAKE_NONE, COMM_AUXPIN_DISABLE};


void comisp_print_comm_info(uint8_t i)
{
	printf("baudrate = %d, ", comisp_chips_param[i].com_mode.baudrate);
	printf("datalength = %d, ", comisp_chips_param[i].com_mode.datalength);
	printf("paritybit = %c, ", comisp_chips_param[i].com_mode.paritybit);
	printf("stopbit = %c, ", comisp_chips_param[i].com_mode.stopbit);
	printf("handshake = %c, ", comisp_chips_param[i].com_mode.handshake);
	printf("auxpin = %c",comisp_chips_param[i].com_mode.auxpin);
}

PARSE_ARGUMENT_HANDLER(comisp)
{
	uint8_t i;
	
	switch (cmd)
	{
	case 'c':
		if (NULL == argu)
		{
			LOG_ERROR(ERRMSG_INVALID_OPTION, cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		for (i = 0; i < dimof(comisp_chips_param); i++)
		{
			if (!strcmp(comisp_chips_param[i].chip_name, argu))
			{
				comisp_chip_index = i;
				break;
			}
		}
		memcpy(&comisp_program_functions, 
				comisp_chips_param[i].program_functions, 
				sizeof(comisp_program_functions));
		comisp_program_functions.enter_program_mode = \
				ENTER_PROGRAM_MODE_FUNCNAME(comisp);
		break;
	case 'E':
		if (argu != NULL)
		{
			i = (uint8_t)strtoul(argu, NULL, 0);
			comisp_print_comm_info(i);
		}
		break;
	case 'C':
		// COM Mode
		if ((NULL == argu) || (strlen(argu) == 0))
		{
			LOG_ERROR(ERRMSG_INVALID_OPTION, cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		{
			// port: 256 bytes
			// baudrate: 4 bytes
			// datalength, paritybit, stopbit: 3 bytes
			// handle, extra: 2 bytes
			uint8_t comm_setting[256 + 4 + 3 + 2], *ptr;
			RESULT success;
			uint8_t i;
			char* formats[] = 
			{
				// port(s):baudrate(4d):datalength(1d):parity(c):stop(1d)
				// :handshake(1d):extra(1d)
				// Eg: COM1:115200_8N1_HA
				"%s%4d%1d%c%1d%c%c",
				// port(s):baudrate(4d):datalength(1d):parity(c):stop(1d)
				// Eg: COM1:115200_8N1
				"%s%4d%1d%c%1d",
				// port(s):baudrate(4d)
				// Eg: COM1:115200
				"%s%4d",
				// port(s)
				// Eg: COM1
				"%s"
			};
			
			success = ERROR_FAIL;
			for (i = 0; i < dimof(formats); i++)
			{
				memset(comm_setting, 0, sizeof(comm_setting));
				success = strparser_parse((char*)argu, formats[i], 
										comm_setting, sizeof(comm_setting));
				if (ERROR_OK == success)
				{
					break;
				}
			}
			
			if (success != ERROR_OK)
			{
				LOG_ERROR(ERRMSG_INVALID_OPTION, cmd);
				return ERRCODE_INVALID_OPTION;
			}
			
			ptr = comm_setting;
			strncpy(com_mode.comport, (char*)ptr, sizeof(com_mode.comport));
			ptr += strlen(com_mode.comport) + 1;
			if (i < 3)
			{
				com_mode.baudrate = *(uint32_t*)ptr;
				ptr += 4;
				if (i < 2)
				{
					com_mode.datalength = ptr[0];
					com_mode.paritybit = ptr[1];
					com_mode.stopbit = ptr[2];
					ptr += 3;
					if (i < 1)
					{
						com_mode.handshake = ptr[0];
						com_mode.auxpin = ptr[1];
						ptr += 2;
					}
				}
			}
		}
		break;
	case 'x':
		if (NULL == argu)
		{
			LOG_ERROR(ERRMSG_INVALID_OPTION, cmd);
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

ENTER_PROGRAM_MODE_HANDLER(comisp)
{
	struct program_functions_t *pf = 
					comisp_chips_param[comisp_chip_index].program_functions;
	
	context->pi->mode -= comisp_mode_offset;
	if (pf->enter_program_mode != NULL)
	{
		return pf->enter_program_mode(context);
	}
	return ERROR_OK;
}

