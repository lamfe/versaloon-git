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

#include "vsprog.h"
#include "scripts.h"
#include "programmer.h"

#define PARAM_EXIT_ON_FAIL					0
#define PARAM_NO_COMMIT						1
struct misc_param_t misc_param[] =
{
	MISC_PARAM(	"exit_on_fail",
				"whether to exit when execute fail",
				0),
	MISC_PARAM(	"no_commit",
				"no commit on command except commit command",
				0),
	MISC_PARAM_END
};

MISC_HANDLER(misc_set_parameters);
MISC_HANDLER(misc_help);
MISC_HANDLER(misc_shell);
MISC_HANDLER(misc_run);
MISC_HANDLER(misc_loop);
MISC_HANDLER(misc_exit);
MISC_HANDLER(misc_close);
MISC_HANDLER(misc_run_command);
MISC_HANDLER(misc_log_info);
MISC_HANDLER(misc_getchar);

struct misc_cmd_t misc_generic_cmd[] = 
{
	MISC_CMD(	"param",
				"set parameters, format: param NAME VALUE",
				misc_set_parameters),
	MISC_CMD(	"misc-help",
				"print misc-help message, format: misc-help <OBJECT>",
				misc_help),
	MISC_CMD(	"shell",
				"enter shell mode, format: shell",
				misc_shell),
	MISC_CMD(	"run",
				"run script file, format: run FILE_NAME [quiet]",
				misc_run),
	MISC_CMD(	"loop",
				"loop next command, format: loop COUNT",
				misc_loop),
	MISC_CMD(	"exit",
				"exit current session, format: exit",
				misc_exit),
	MISC_CMD(	"close",
				"close program, format: close",
				misc_close),
	MISC_CMD(	"misc-cmd",
				"run misc command, format: misc-cmd/V COMMAND",
				misc_run_command),
	MISC_CMD(	"V",
				"run misc command, format: misc-cmd/V COMMAND",
				misc_run_command),
	MISC_CMD(	"log_info",
				"display information, format: log_info INFO",
				misc_log_info),
	MISC_CMD(	"getchar",
				"wait keyboard input, format: getchar",
				misc_getchar),
	MISC_CMD_END
};

extern struct misc_cmd_t programmer_cmd[];
extern struct misc_cmd_t pgbar_cmd[];
extern struct misc_cmd_t vsprog_cmd[];
extern struct misc_cmd_t target_cmd[];
extern struct misc_cmd_t filelist_cmd[];
extern struct misc_cmd_t comisp_cmd[];
extern struct misc_cmd_t usbapi_cmd[];

struct misc_param_t *misc_params_list[] = 
{
	misc_param
};

struct misc_cmd_t *misc_cmds_list[] = 
{
	misc_generic_cmd,
	vsprog_cmd,
	target_cmd,
	pgbar_cmd,
	filelist_cmd,
	programmer_cmd,
	comisp_cmd,
	usbapi_cmd
};

static int8_t misc_exit_mark = 0;
static uint32_t misc_loop_cnt = 0;
static uint8_t misc_fatal_error = 0;

void misc_set_fatal_error(void)
{
	misc_fatal_error = 1;
}

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

static struct misc_cmd_t* misc_search_cmd(struct misc_cmd_t ** cmds_list, 
										uint32_t cmd_size, const char *name)
{
	uint32_t i, j;
	struct misc_cmd_t *cmd = NULL;
	
	if (NULL == name)
	{
		return NULL;
	}
	
	for (i = 0; i < cmd_size; i++)
	{
		cmd = cmds_list[i];
		
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
	struct misc_cmd_t *cmd = misc_search_cmd(misc_cmds_list, 
		dimof(misc_cmds_list), name);
	
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
		
		if (('\'' == cmd[i]) || ('"' == cmd[i]))
		{
			// everything between ' or " is one parameter
			// nesting of ' and " is not supported
			uint32_t j;
			char div = cmd[i];
			
			j = i + 1;
			argv[argu_num++] = &cmd[j];
			while (cmd[j] != div)
			{
				if ('\0' == cmd[j])
				{
					// shouldn't end here too
					return ERROR_FAIL;
				}
				j++;
			}
			i = j;
		}
		else
		{
			argv[argu_num++] = &cmd[i];
			while (!isspace(cmd[i]) && (cmd[i] != '\0'))
			{
				i++;
			}
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

RESULT misc_cmd_supported_by_notifier(const struct misc_cmd_t *notifier, 
										char *notify_cmd)
{
	struct misc_cmd_t *cmds_list[1], *cmd;
	
	if ((NULL == notifier) || (NULL == notify_cmd))
	{
		return ERROR_FAIL;
	}
	
	cmd = misc_search_cmd((struct misc_cmd_t **)&notifier, dimof(cmds_list), 
							notify_cmd);
	if (NULL == cmd)
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

RESULT misc_call_notifier(const struct misc_cmd_t *notifier, 
							char *notify_cmd, char *notify_param)
{
	struct misc_cmd_t *cmds_list[1], *cmd;
	char *argv[2];
	uint16_t argc;
	
	if ((NULL == notifier) || (NULL == notify_cmd))
	{
		return ERROR_FAIL;
	}
	
	argv[0] = notify_cmd;
	argv[1] = notify_param;
	cmds_list[0] = (struct misc_cmd_t *)notifier;
	if (notify_param != NULL)
	{
		argc = dimof(argv);
	}
	else
	{
		argc = 1;
	}
	
	cmd = misc_search_cmd(cmds_list, dimof(cmds_list), argv[0]);
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
	else if (ERROR_OK != cmd->processor(argc, (const char **)argv))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "run", argv[0]);
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT misc_cmd_supported(char *name)
{
	struct misc_cmd_t *cmd = misc_search_cmd(misc_cmds_list, 
		dimof(misc_cmds_list), name);
	
	if (NULL == cmd)
	{
		return ERROR_FAIL;
	}
	else
	{
		return ERROR_OK;
	}
}

RESULT misc_run_cmd(uint16_t argc, const char *argv[])
{
	struct misc_cmd_t *cmd = misc_search_cmd(misc_cmds_list, 
		dimof(misc_cmds_list), argv[0]);
	
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
	char *buff_in_memory = NULL, first_char;
	uint16_t argc;
	char *argv[1024];
	RESULT ret = ERROR_OK;
	uint32_t i, run_times;
	
	first_char = 0;
	for (i = 0; i < strlen(cmd); i++)
	{
		first_char = cmd[i];
		if (!isspace(first_char))
		{
			break;
		}
	}
	if ('#' == first_char)
	{
		// comment line
		return ERROR_OK;
	}
	
	buff_in_memory = (char *)malloc(strlen(cmd) + 1);
	if (NULL == buff_in_memory)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERROR_FAIL;
	}
	strcpy(buff_in_memory, cmd);
	
	argc = (uint16_t)dimof(argv);
	if (ERROR_OK != misc_parse_cmd_line(buff_in_memory, &argc, (char **)argv))
	{
		ret = ERROR_FAIL;
		goto end;
	}
	// empty line or comment line
	if ((0 == argc) || ('#' == argv[0][0]))
	{
		goto end;
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
			&& (misc_fatal_error 
				|| misc_param[PARAM_EXIT_ON_FAIL].value))
		{
			misc_exit_mark = -1;
			goto end;
		}
	}
	
	// commit if required
	if ((cur_programmer != NULL) 
		&& (cur_programmer->interfaces.peripheral_commit != NULL))
	{
		if (0 == misc_param[PARAM_NO_COMMIT].value)
		{
			ret = cur_programmer->interfaces.peripheral_commit();
		}
	}
end:
	if (buff_in_memory != NULL)
	{
		free(buff_in_memory);
		buff_in_memory = NULL;
	}
	return ret;
}

static RESULT misc_run_file(FILE *f, char *head, uint8_t quiet)
{
	char cmd_line[4096];
	
	rewind(f);
	while (1)
	{
		if (!quiet)
		{
			if (head != NULL)
			{
				printf("%s", head);
			}
			printf(">>>");
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
			&& (misc_fatal_error 
				|| misc_param[PARAM_EXIT_ON_FAIL].value))
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
MISC_HANDLER(misc_set_parameters)
{
	struct misc_param_t *param = NULL;
	
	MISC_CHECK_ARGC(3);
	
	param = mic_search_param(argv[1]);
	if (NULL == param)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], "parameters");
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	param->value = strtoul(argv[2], NULL, 0);
	
	return ERROR_OK;
}

// help
MISC_HANDLER(misc_help)
{
	struct misc_cmd_t *cmd = NULL;
	
	MISC_CHECK_ARGC_2(1, 2);
	
	if (2 == argc)
	{
		cmd = misc_search_cmd(misc_cmds_list, 
			dimof(misc_cmds_list), argv[1]);
		
		if (NULL == cmd)
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
		}
		else
		{
			LOG_INFO("command %s:", argv[1]);
			LOG_INFO("  %s: %s", cmd->cmd_name, cmd->help_str);
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
				LOG_INFO("  %s: %s", cmd[j].cmd_name, cmd[j].help_str);
				j++;
			}
		}
	}
	return ERROR_OK;
}

// shell
MISC_HANDLER(misc_shell)
{
	MISC_CHECK_ARGC(1);
	
	LOG_INFO("enter shell mode.");
	
	if (cur_programmer != NULL)
	{
		return misc_run_file(stdin, cur_programmer->name, 0);
	}
	else
	{
		return misc_run_file(stdin, NULL, 0);
	}
}

// run
MISC_HANDLER(misc_run)
{
	FILE *f = NULL;
	uint8_t quiet = 0;
	RESULT ret = ERROR_OK;
	
	MISC_CHECK_ARGC_2(2, 3);
	
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
MISC_HANDLER(misc_loop)
{
	MISC_CHECK_ARGC(2);
	
	misc_loop_cnt = strtoul(argv[1], NULL, 0);
	return ERROR_OK;
}

// exit
MISC_HANDLER(misc_exit)
{
	MISC_CHECK_ARGC(1);
	misc_exit_mark = 1;
	return ERROR_OK;
}

// close
MISC_HANDLER(misc_close)
{
	MISC_CHECK_ARGC(1);
	misc_exit_mark = -1;
	return ERROR_OK;
}

MISC_HANDLER(misc_run_command)
{
	vsprog_no_call_operate();
	MISC_CHECK_ARGC(2);
	return misc_run_script((char *)argv[1]);
}

MISC_HANDLER(misc_log_info)
{
	MISC_CHECK_ARGC(2);
	LOG_INFO("%s", argv[1]);
	return ERROR_OK;
}

MISC_HANDLER(misc_getchar)
{
	MISC_CHECK_ARGC(1);
	getchar();
	return ERROR_OK;
}

