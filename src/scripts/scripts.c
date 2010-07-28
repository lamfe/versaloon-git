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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "scripts.h"
#include "programmer.h"

#define PARAM_EXIT_ON_FAIL					0
#define PARAM_NO_COMMIT						1
struct misc_param_t misc_param[] =
{
	{
		"exit_on_fail",
		"whether to exit when execute fail",
		0
	},
	{
		"no_commit",
		"no commit on command except commit command",
		0
	},
	{
		NULL,
		NULL,
		0
	}
};

RESULT misc_set_parameters(uint16_t argc, const char *argv[]);
RESULT misc_help(uint16_t argc, const char *argv[]);
RESULT misc_shell(uint16_t argc, const char *argv[]);
RESULT misc_run(uint16_t argc, const char *argv[]);
RESULT misc_loop(uint16_t argc, const char *argv[]);
RESULT misc_exit(uint16_t argc, const char *argv[]);
RESULT misc_close(uint16_t argc, const char *argv[]);
struct misc_cmd_t misc_generic_cmd[] = 
{
	// param
	{
		"param",
		"set parameters, format: param NAME VALUE",
		misc_set_parameters
	},
	// help
	{
		"help",
		"print help message, format: help <OBJECT>",
		misc_help
	},
	// shell
	{
		"shell",
		"enter shell mode, format: shell",
		misc_shell
	},
	// run
	{
		"run",
		"run script file, format: run FILE_NAME [quiet]",
		misc_run
	},
	// loop
	{
		"loop",
		"loop next command, format: loop COUNT",
		misc_loop
	},
	// exit
	{
		"exit",
		"exit current session, format: exit",
		misc_exit
	},
	// close
	{
		"close",
		"close program, format: close",
		misc_close
	},
	{
		NULL,
		NULL,
		NULL
	}
};

extern struct misc_param_t programmer_param[];
extern struct misc_cmd_t programmer_cmd[];

struct misc_param_t *misc_params_list[] = 
{
	misc_param,
	programmer_param
};

struct misc_cmd_t *misc_cmds_list[] = 
{
	misc_generic_cmd,
	programmer_cmd
};

static int8_t misc_exit_mark = 0;
static uint32_t misc_loop_cnt = 0;

static struct misc_param_t* mic_search_param(const char *name)
{
	int i, j;
	struct misc_param_t *param = NULL;
	
	if (NULL == name)
	{
		return NULL;
	}
	
	for (i = 0; i < (int)dimof(misc_params_list); i++)
	{
		param = misc_params_list[i];
		
		j= 0;
		while ((param != NULL) && (param[j].param_name != NULL))
		{
			if (!strcmp(param[j].param_name, name))
			{
				return &param[j];
			}
			
			j++;
		}
	}
	
	return NULL;
}

static struct misc_cmd_t* misc_search_cmd(const char *name)
{
	int i, j;
	struct misc_cmd_t *cmd = NULL;
	
	if (NULL == name)
	{
		return NULL;
	}
	
	for (i = 0; i < (int)dimof(misc_cmds_list); i++)
	{
		cmd = misc_cmds_list[i];
		
		j= 0;
		while ((cmd != NULL) && (cmd[j].cmd_name != NULL))
		{
			if (!strcmp(cmd[j].cmd_name, name))
			{
				return &cmd[j];
			}
			
			j++;
		}
	}
	
	return NULL;
}

RESULT misc_print_help(const char *name)
{
	struct misc_cmd_t *cmd = misc_search_cmd(name);
	
	if (NULL == cmd)
	{
		return ERROR_FAIL;
	}
	
	LOG_INFO("%s", cmd->help_str);
	
	return ERROR_OK;
}

static RESULT misc_parse_cmd_line(char *cmd, uint16_t *argc, char **argv)
{
	uint32_t i, cmd_len;
	uint16_t argu_num = 0;
	
	while (('"' == cmd[0]) || ('\'' == cmd[0]))
	{
		char ch = cmd[0];
		
		if (cmd[strlen(cmd) - 1] != ch)
		{
			return ERROR_FAIL;
		}
		cmd[strlen(cmd) - 1] = '\0';
		strcpy(cmd, cmd + 1);
	}
	
	// parse arg
	memset(argv, 0, *argc);
	argu_num = 0;
	i = 0;
	cmd_len = strlen(cmd);
	while (i < cmd_len)
	{
		while (isspace(cmd[i]))
		{
			i++;
		}
		if ('\0' == cmd[i])
		{
			break;
		}
		
		argv[argu_num++] = &cmd[i];
		while (!isspace(cmd[i]) && (cmd[i] != '\0'))
		{
			i++;
		}
		cmd[i++] = '\0';
		if (argu_num >= *argc)
		{
			break;
		}
	}
	*argc = argu_num;
	
	return ERROR_OK;
}

static RESULT misc_run_cmd(uint16_t argc, const char *argv[])
{
	struct misc_cmd_t *cmd = misc_search_cmd(argv[0]);
	
	if (NULL == cmd)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[0]);
		return ERROR_FAIL;
	}
	
	if (NULL == cmd->processor)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		return ERROR_FAIL;
	}
	else if (ERROR_OK != cmd->processor(argc, argv))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "run", argv[0]);
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT misc_run_script(char *cmd)
{
	uint16_t argc;
	char *argv[1024];
	RESULT ret = ERROR_OK;
	uint32_t i, run_times;
	
	argc = (uint16_t)dimof(argv);
	misc_parse_cmd_line(cmd, &argc, (char **)argv);
	if ((0 == argc) || ('#' == argv[0][0]))
	{
		return ERROR_OK;
	}
	
	// run command
	run_times = misc_loop_cnt;
	misc_loop_cnt = 0;
	if (!run_times)
	{
		run_times = 1;
	}
	for (i = 0; i < run_times; i++)
	{
		ret = misc_run_cmd(argc, (const char**)argv);
		if ((ret != ERROR_OK) 
			&& misc_param[PARAM_EXIT_ON_FAIL].value)
		{
			misc_exit_mark = -1;
			return ret;
		}
	}
	
	// commit if required
	if (misc_param[PARAM_NO_COMMIT].value)
	{
		return ret;
	}
	else
	{
		return cur_programmer->interfaces.peripheral_commit();
	}
}

static RESULT misc_run_file(FILE *f, char *head, uint8_t quiet)
{
	char cmd_line[4096];
	
	rewind(f);
	while (1)
	{
		if ((!quiet) && (head != NULL))
		{
			printf("%s>>>", head);
		}
		
		// get a line
		if (NULL == fgets(cmd_line, sizeof(cmd_line), f))
		{
			if (!feof(f))
			{
				return ERROR_FAIL;
			}
			else
			{
				return ERROR_OK;
			}
		}
		
		if ((!quiet) && (f != stdin))
		{
			printf("%s", cmd_line);
		}
		if ((ERROR_OK != misc_run_script(cmd_line))
			&& misc_param[PARAM_EXIT_ON_FAIL].value)
		{
			return ERROR_FAIL;
		}
		if (!quiet)
		{
			printf("\n");
		}
		if (misc_exit_mark != 0)
		{
			if (misc_exit_mark > 0)
			{
				misc_exit_mark = 0;
			}
			break;
		}
	}
	
	return ERROR_OK;
}

// commands
// param
RESULT misc_set_parameters(uint16_t argc, const char *argv[])
{
	struct misc_param_t *param = NULL;
	
	if (3 != argc)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	param = mic_search_param(argv[1]);
	if (NULL == param)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	param->value = strtoul(argv[2], NULL, 0);
	
	return ERROR_OK;
}

// help
RESULT misc_help(uint16_t argc, const char *argv[])
{
	struct misc_cmd_t *cmd = NULL;
	
	if ((argc != 2) && (argc != 1))
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	if (2 == argc)
	{
		cmd = misc_search_cmd(argv[1]);
		
		if (NULL == cmd)
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
		}
		else
		{
			LOG_INFO("command %s:", argv[1]);
			LOG_INFO("\t%s: %s", cmd->cmd_name, cmd->help_str);
		}
	}
	else if (1 == argc)
	{
		int i, j;
		
		LOG_INFO("command list:");
		
		for (i = 0; i < (int)dimof(misc_cmds_list); i++)
		{
			cmd = misc_cmds_list[i];
			
			j = 0;
			while (cmd[j].cmd_name != NULL)
			{
				LOG_INFO("\t%s: %s", cmd[j].cmd_name, cmd[j].help_str);
				j++;
			}
		}
	}
	return ERROR_OK;
}

// shell
RESULT misc_shell(uint16_t argc, const char *argv[])
{
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	LOG_INFO("enter shell mode.");
	
	return misc_run_file(stdin, cur_programmer->name, 0);
}

// run
RESULT misc_run(uint16_t argc, const char *argv[])
{
	FILE *f = NULL;
	uint8_t quiet = 0;
	RESULT ret = ERROR_OK;
	
	if ((argc != 2) && (argc != 3))
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	if ((3 == argc) && (!strcmp(argv[2], "quiet")))
	{
		quiet = 1;
	}
	
	f = fopen(argv[1], "rt");
	if (NULL == f)
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "open script file", argv[1]);
		return ERROR_FAIL;
	}
	else
	{
		ret = misc_run_file(f, (char*)argv[1], quiet);
		fclose(f);
	}
	return ret;
}

// loop
RESULT misc_loop(uint16_t argc, const char *argv[])
{
	if (argc != 2)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	misc_loop_cnt = strtoul(argv[1], NULL, 0);
	return ERROR_OK;
}

// exit
RESULT misc_exit(uint16_t argc, const char *argv[])
{
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	misc_exit_mark = 1;
	
	return ERROR_OK;
}

// close
RESULT misc_close(uint16_t argc, const char *argv[])
{
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	misc_exit_mark = -1;
	return ERROR_OK;
}

