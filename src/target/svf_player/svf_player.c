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

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "memlist.h"
#include "filelist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"


#include "byte_tap.h"
#include "svf.h"
#include "svf_player.h"
#include "svf_parser.h"

#define CUR_TARGET_STRING			SVFP_STRING
#define cur_chips_param				svfp_chips_param

typedef struct
{
	char *chip_name;
	char *info;
}svfp_param_t;

svfp_param_t svfp_chips_param[] = 
{
//	name			info
	{"epm240",		"CPLD from Altera"},
};

const program_area_map_t svfp_program_area_map[] = 
{
	{0, 0, 0}
};

#define SVF_SET_FREQ_CMD			"FREQUENCY %.02f HZ"
static char *first_command = NULL;

static void svfp_usage(void)
{
	printf("\
Usage of %s:\n\
  -I,  --inputfile <FILENAME>       set svf file\n\
  -F,  --frequency <FREQUENCY>      set JTAG frequency, in KHz\n\n", 
		   CUR_TARGET_STRING);
}

static void svfp_support(void)
{
	uint32_t i;
	
	printf("Support list of %s:\n", CUR_TARGET_STRING);
	for (i = 0; i < dimof(cur_chips_param); i++)
	{
		printf("%s: %s\n", 
				cur_chips_param[i].chip_name, 
				cur_chips_param[i].info);
	}
	printf("\n");
}

RESULT svfp_parse_argument(char cmd, const char *argu)
{
	uint16_t jtag_initial_speed;
	
	switch (cmd)
	{
	case 'h':
		svfp_usage();
		break;
	case 'S':
		svfp_support();
		break;
	case 'F':
		// set JTAG initial frequency
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		jtag_initial_speed = (uint16_t)strtoul(argu, NULL, 0);
		first_command = (char*)malloc(strlen(SVF_SET_FREQ_CMD) + 20);
		sprintf(first_command, SVF_SET_FREQ_CMD, 
				(float)jtag_initial_speed * 1000);
		
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT svfp_probe_chip(char *chip_name)
{
	chip_name = chip_name;
	return ERROR_FAIL;
}

RESULT svfp_prepare_buffer(program_info_t *pi)
{
	pi = pi;
	return ERROR_OK;
}

RESULT svfp_write_buffer_from_file_callback(uint32_t address, uint32_t seg_addr, 
											uint8_t* data, uint32_t length, 
											void* buffer)
{
	address = address;
	seg_addr = seg_addr;
	data = data;
	length = length;
	buffer = buffer;
	
	LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "Hex file", CUR_TARGET_STRING);
	return ERRCODE_NOT_SUPPORT;
}

RESULT svfp_fini(program_info_t *pi, programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	if (first_command != NULL)
	{
		free(first_command);
		first_command = NULL;
	}
	
	return ERROR_OK;
}

RESULT svfp_init(program_info_t *pi, programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	return ERROR_OK;
}

uint32_t svfp_interface_needed(void)
{
	return JTAG_LL;
}

programmer_info_t *p = NULL;
extern filelist *fl_in;
RESULT svfp_program(operation_t operations, program_info_t *pi, 
					programmer_info_t *prog)
{
	FILE *svf_file = NULL;
	uint32_t svf_file_size = 0, command_num = 0;
	char *svfp_command_buffer = NULL;
	uint32_t svfp_command_buffer_len = 0;
	RESULT ret = ERROR_OK;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	operations = operations;
	pi = pi;
	p = prog;
	
	if ((NULL == fl_in) || (NULL == fl_in->path) || (NULL == fl_in->file) 
		|| (strlen(fl_in->path) <= 4) 
		|| (toupper(fl_in->path[strlen(fl_in->path) - 4]) != '.') 
		|| (toupper(fl_in->path[strlen(fl_in->path) - 3]) != 'S') 
		|| (toupper(fl_in->path[strlen(fl_in->path) - 2]) != 'V') 
		|| (toupper(fl_in->path[strlen(fl_in->path) - 1]) != 'F'))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "svf file");
		return ERROR_FAIL;
	}
	svf_file = fl_in->file;
	
	fseek(svf_file, 0L, SEEK_END);
	svf_file_size = ftell(svf_file);
	rewind(svf_file);
	
	svf_parser_init();
	
	if (ERROR_OK != tap_init())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "open jtag");
		svf_parser_fini();
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (first_command != NULL)
	{
		if (ERROR_OK != svf_parser_run_command(first_command))
		{
			LOG_ERROR(_GETTEXT("Fail to execute first command: %s\n"), 
					  first_command);
			ret = ERROR_FAIL;
			goto leave_program_mode;
		}
	}
	
	if (verbosity < DEBUG_LEVEL)
	{
		pgbar_init("executing svf |", "|", 0, svf_file_size, 
				   PROGRESS_STEP, '=');
	}
	
	// parse commands and run
	while (ERROR_OK == svf_parser_get_command(svf_file, &svfp_command_buffer, 
											  &svfp_command_buffer_len))
	{
		if (ERROR_OK != svf_parser_run_command(svfp_command_buffer))
		{
			if (verbosity < DEBUG_LEVEL)
			{
				pgbar_fini();
			}
			LOG_ERROR(_GETTEXT("Command execute failed at line %d\n"), 
					  svf_line_number);
			ret = ERROR_FAIL;
			goto leave_program_mode;
		}
		if (verbosity < DEBUG_LEVEL)
		{
			if (svf_file_index * 100 / svf_file_size > 3)
			{
				pgbar_update(svf_file_index);
				svf_file_index = 0;
			}
		}
		command_num++;
	}
	
	// commit last commands
	if (ERROR_OK != tap_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "do jtag");
		return ERRCODE_FAILURE_OPERATION;
	}
	else if (ERROR_OK != svf_parser_check_tdo())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "check tdo data");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	if (verbosity < DEBUG_LEVEL)
	{
		pgbar_update(svf_file_index);
		pgbar_fini();
	}
	LOG_INFO(_GETTEXT("%d commands execute finised OK\n"), command_num);
	
leave_program_mode:
	// free all
	svf_parser_fini();
	if (svfp_command_buffer)
	{
		free(svfp_command_buffer);
		svfp_command_buffer = NULL;
	}
	
	return ret;
}

