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

RESULT comisp_enter_program_mode(struct program_context_t *context);
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
	printf("\
baudrate = %d, datalength = %d, paritybit = %c, stopbit = %c, \
handshake = %c, auxpin = %c",
			comisp_chips_param[i].com_mode.baudrate,
			comisp_chips_param[i].com_mode.datalength,
			comisp_chips_param[i].com_mode.paritybit,
			comisp_chips_param[i].com_mode.stopbit,
			comisp_chips_param[i].com_mode.handshake,
			comisp_chips_param[i].com_mode.auxpin);
}

RESULT comisp_parse_argument(char cmd, const char *argu)
{
	uint8_t i;
	char *end_pointer, *cur_pointer;
	
	switch (cmd)
	{
	case 'c':
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
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
		comisp_program_functions.enter_program_mode = comisp_enter_program_mode;
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

RESULT comisp_enter_program_mode(struct program_context_t *context)
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

