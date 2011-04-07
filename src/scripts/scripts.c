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
#include "port.h"

#include "vsprog.h"
#include "scripts.h"
#include "programmer.h"
#include "bufffunc.h"

#define PARAM_EXIT_ON_FAIL					0
#define PARAM_NO_COMMIT						1
struct vss_param_t vss_param[] =
{
	VSS_PARAM(	"exit_on_fail",
				"whether to exit when execute fail",
				0),
	VSS_PARAM(	"no_commit",
				"no commit on command except commit command",
				0),
	VSS_PARAM_END
};

VSS_HANDLER(vss_set_parameters);
VSS_HANDLER(vss_help);
VSS_HANDLER(vss_shell);
VSS_HANDLER(vss_run);
VSS_HANDLER(vss_loop);
VSS_HANDLER(vss_exit);
VSS_HANDLER(vss_close);
VSS_HANDLER(vss_run_command);
VSS_HANDLER(vss_log_info);
VSS_HANDLER(vss_getchar);
VSS_HANDLER(vss_sleep);
VSS_HANDLER(vss_quiet);
VSS_HANDLER(vss_function_register);
VSS_HANDLER(vss_function_end);
VSS_HANDLER(vss_function_call);
VSS_HANDLER(vss_function_free);

struct vss_cmd_t vss_generic_cmd[] = 
{
	VSS_CMD(	"param",
				"set parameters, format: param NAME VALUE",
				vss_set_parameters),
	VSS_CMD(	"vss-help",
				"print vss-help message, format: vss-help <OBJECT>",
				vss_help),
	VSS_CMD(	"shell",
				"enter shell mode, format: shell",
				vss_shell),
	VSS_CMD(	"run",
				"run script file, format: run FILE_NAME [quiet]",
				vss_run),
	VSS_CMD(	"loop",
				"loop next command, format: loop COUNT",
				vss_loop),
	VSS_CMD(	"exit",
				"exit current session, format: exit",
				vss_exit),
	VSS_CMD(	"close",
				"close program, format: close",
				vss_close),
	VSS_CMD(	"vss-cmd",
				"run vss command, format: vss-cmd/V COMMAND",
				vss_run_command),
	VSS_CMD(	"V",
				"run vss command, format: vss-cmd/V COMMAND",
				vss_run_command),
	VSS_CMD(	"log_info",
				"display information, format: log_info INFO",
				vss_log_info),
	VSS_CMD(	"getchar",
				"wait keyboard input, format: getchar",
				vss_getchar),
	VSS_CMD(	"sleep",
				"sleep defined ms, format: sleep MS",
				vss_sleep),
	VSS_CMD(	"quiet",
				"set quiet mode, format: quiet/q [0/1]",
				vss_quiet),
	VSS_CMD(	"q",
				"set quiet mode, format: quiet/q [0/1]",
				vss_quiet),
	VSS_CMD(	"function",
				"define a function, format: function FUNC_NAME",
				vss_function_register),
	VSS_CMD(	"end_function",
				"end a function",
				vss_function_end),
	VSS_CMD(	"function_call",
				"call a function, format: function_call FUNC_NAME",
				vss_function_call),
	VSS_CMD(	"function_free",
				"free function(s), format: function_free [FUNC_NAME]",
				vss_function_free),
	VSS_CMD_END
};

extern struct vss_cmd_t interface_cmd[];
extern struct vss_cmd_t programmer_cmd[];
extern struct vss_cmd_t pgbar_cmd[];
extern struct vss_cmd_t vsprog_cmd[];
extern struct vss_cmd_t target_cmd[];
extern struct vss_cmd_t filelist_cmd[];
extern struct vss_cmd_t comisp_cmd[];
extern struct vss_cmd_t usbapi_cmd[];
extern struct vss_cmd_t dal_cmd[];

struct vss_param_t *vss_params_list[] = 
{
	vss_param
};

struct vss_cmd_t *vss_cmds_list[] = 
{
	vss_generic_cmd,
	vsprog_cmd,
	target_cmd,
	pgbar_cmd,
	filelist_cmd,
	programmer_cmd,
	interface_cmd,
	comisp_cmd,
	usbapi_cmd,
	dal_cmd
};

static int8_t vss_exit_mark = 0;
static uint32_t vss_loop_cnt = 0;
static uint8_t vss_fatal_error = 0;
static uint8_t *vss_quiet_mode_ptr = NULL;

static struct vss_function_t *vss_functions = NULL, *vss_cur_function = NULL;

static struct vss_function_t *vss_function_search(struct vss_function_t *f, char *func_name)
{
	if (func_name != NULL)
	{
		while (f != NULL)
		{
			if (!strcpy(f->func_name, func_name))
			{
				return f;
			}
			f = f->next;
		}
	}
	return NULL;
}

static RESULT vss_function_run(struct vss_function_t *f)
{
	struct vss_function_cmd_t *cmd;
	
	if ((NULL == f) || (NULL == f->cmds))
	{
		return ERROR_OK;
	}
	
	cmd = f->cmds;
	while (cmd->func_cmd != NULL)
	{
		if (ERROR_OK != vss_run_script(cmd->func_cmd))
		{
			return ERROR_FAIL;
		}
		cmd = cmd->next;
	}
	
	return ERROR_OK;
}

static RESULT vss_function_free_node(struct vss_function_t *f)
{
	struct vss_function_cmd_t *cmd, *cmd_tmp;
	
	if (NULL == f)
	{
		return ERROR_OK;
	}
	
	cmd = f->cmds;
	while (cmd != NULL)
	{
		cmd_tmp = cmd;
		cmd = cmd->next;
		if (cmd_tmp->func_cmd != NULL)
		{
			free(cmd_tmp->func_cmd);
			cmd_tmp->func_cmd = NULL;
		}
		free(cmd_tmp);
	}
	if (f->func_name != NULL)
	{
		free(f->func_name);
		f->func_name = NULL;
	}
	free(f);
	f = NULL;
	
	return ERROR_OK;
}

static RESULT vss_append_function_cmd(struct vss_function_t *func, char * str)
{
	struct vss_function_cmd_t *cmd, *tmp;
	
	if (NULL == func)
	{
		return ERROR_FAIL;
	}
	
	cmd = (struct vss_function_cmd_t *)malloc(sizeof(*cmd));
	if (NULL == cmd)
	{
		return ERROR_FAIL;
	}
	memset(cmd, 0, sizeof(*cmd));
	
	if ((str != NULL) && 
		(NULL == bufffunc_malloc_and_copy_str(&cmd->func_cmd, str)))
	{
		free(cmd);
		cmd = NULL;
		return ERROR_FAIL;
	}
	
	tmp = func->cmds;
	if (NULL == tmp)
	{
		func->cmds = cmd;
	}
	else
	{
		while (tmp->next != NULL)
		{
			tmp = tmp->next;
		}
		tmp->next = cmd;
	}
	cmd = tmp = NULL;
	return ERROR_OK;
}

RESULT vss_get_binary_buffer(uint16_t argc, const char *argv[], 
						uint8_t data_size, uint32_t data_num, void **pbuff)
{
	uint32_t i;
	uint64_t value;
	
	if ((NULL == argv) || (argc < data_num) || (NULL == pbuff)
		|| ((data_size != 1) && (data_size != 2) && (data_size != 4) 
			&& (data_size != 8)))
	{
		return ERROR_FAIL;
	}
	
	if (NULL == *pbuff)
	{
		*pbuff = malloc(data_size * data_num);
		if (NULL == *pbuff)
		{
			return ERROR_FAIL;
		}
	}
	
	for (i = 0; i < data_num; i++)
	{
		value = strtoull(argv[i], NULL, 0);
		switch (data_size)
		{
		case 1:
			(*(uint8_t **)pbuff)[i] = (uint8_t)value;
			break;
		case 2:
			(*(uint16_t **)pbuff)[i] = (uint16_t)value;
			break;
		case 4:
			(*(uint32_t **)pbuff)[i] = (uint32_t)value;
			break;
		case 8:
			(*(uint64_t **)pbuff)[i] = (uint64_t)value;
			break;
		default:
			return ERROR_FAIL;
			break;
		}
	}
	return ERROR_OK;
}

void vss_set_fatal_error(void)
{
	vss_fatal_error = 1;
}

static char vss_get_first_non_space_char(char *cmd, uint32_t *idx)
{
	char result = 0;
	uint32_t i;
	
	for (i = 0; i < strlen(cmd); i++)
	{
		if (!isspace(cmd[i]))
		{
			result = cmd[i];
			break;
		}
	}
	if (idx != NULL)
	{
		*idx = i;
	}
	return result;
}

static struct vss_param_t* mic_search_param(const char *name)
{
	int i, j;
	struct vss_param_t *param = NULL;
	
	if (NULL == name)
	{
		return NULL;
	}
	
	for (i = 0; i < (int)dimof(vss_params_list); i++)
	{
		param = vss_params_list[i];
		
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

static struct vss_cmd_t* vss_search_cmd(struct vss_cmd_t ** cmds_list, 
										uint32_t cmd_size, const char *name)
{
	uint32_t i, j;
	struct vss_cmd_t *cmd = NULL;
	
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

RESULT vss_print_help(const char *name)
{
	struct vss_cmd_t *cmd = vss_search_cmd(vss_cmds_list, 
		dimof(vss_cmds_list), name);
	
	if (NULL == cmd)
	{
		return ERROR_FAIL;
	}
	
	LOG_INFO("%s", cmd->help_str);
	
	return ERROR_OK;
}

static RESULT vss_parse_cmd_line(char *cmd, uint16_t *argc, char **argv)
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

RESULT vss_cmd_supported_by_notifier(const struct vss_cmd_t *notifier, 
										char *notify_cmd)
{
	struct vss_cmd_t *cmds_list[1], *cmd;
	
	if ((NULL == notifier) || (NULL == notify_cmd))
	{
		return ERROR_FAIL;
	}
	
	cmd = vss_search_cmd((struct vss_cmd_t **)&notifier, dimof(cmds_list), 
							notify_cmd);
	if (NULL == cmd)
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

RESULT vss_call_notifier(const struct vss_cmd_t *notifier, 
							char *notify_cmd, char *notify_param)
{
	struct vss_cmd_t *cmds_list[1], *cmd;
	char *argv[2];
	uint16_t argc;
	
	if ((NULL == notifier) || (NULL == notify_cmd))
	{
		return ERROR_FAIL;
	}
	
	argv[0] = notify_cmd;
	argv[1] = notify_param;
	cmds_list[0] = (struct vss_cmd_t *)notifier;
	if (notify_param != NULL)
	{
		argc = dimof(argv);
	}
	else
	{
		argc = 1;
	}
	
	cmd = vss_search_cmd(cmds_list, dimof(cmds_list), argv[0]);
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

RESULT vss_cmd_supported(char *name)
{
	struct vss_cmd_t *cmd = vss_search_cmd(vss_cmds_list, 
		dimof(vss_cmds_list), name);
	
	if (NULL == cmd)
	{
		return ERROR_FAIL;
	}
	else
	{
		return ERROR_OK;
	}
}

RESULT vss_run_cmd(uint16_t argc, const char *argv[])
{
	struct vss_cmd_t *cmd = vss_search_cmd(vss_cmds_list, 
		dimof(vss_cmds_list), argv[0]);
	
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
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "run command:", argv[0]);
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT vss_run_script(char *cmd)
{
	char *buff_in_memory = NULL;
	uint16_t argc;
	char *argv[1024];
	RESULT ret = ERROR_OK;
	uint32_t i, run_times;
	
	if (NULL == bufffunc_malloc_and_copy_str(&buff_in_memory, cmd))
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERROR_FAIL;
	}
	
	argc = (uint16_t)dimof(argv);
	if (ERROR_OK != vss_parse_cmd_line(buff_in_memory, &argc, (char **)argv))
	{
		ret = ERROR_FAIL;
		goto end;
	}
	for (i = 0; i < argc; i++)
	{
		if (VSS_COMMENT_CHAR == argv[i][0])
		{
			argc = (uint16_t)i;
			break;
		}
	}
	// empty line or comment line
	if (0 == argc)
	{
		goto end;
	}
	
	// run command
	run_times = vss_loop_cnt;
	vss_loop_cnt = 0;
	if (!run_times)
	{
		run_times = 1;
	}
	for (i = 0; i < run_times; i++)
	{
		ret = vss_run_cmd(argc, (const char**)argv);
		if ((ret != ERROR_OK) 
			&& (vss_fatal_error 
				|| vss_param[PARAM_EXIT_ON_FAIL].value))
		{
			if (run_times > 1)
			{
				LOG_ERROR("fail to run the %dth times", i + 1);
			}
			vss_exit_mark = -1;
			goto end;
		}
	}
	
	// commit if required
	if ((cur_interface != NULL) 
		&& (cur_interface->peripheral_commit != NULL))
	{
		if (0 == vss_param[PARAM_NO_COMMIT].value)
		{
			if (ERROR_OK != cur_interface->peripheral_commit())
			{
				ret = ERROR_FAIL;
			}
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

static RESULT vss_run_file(FILE *f, char *head, uint8_t quiet)
{
	char cmd_line[4096], *cmd_ptr;
	uint8_t cur_cmd_quiet, vss_quiet_mode;
	uint32_t i;
	
	vss_quiet_mode = 0;
	vss_quiet_mode_ptr = &vss_quiet_mode;
	
	rewind(f);
	while (1)
	{
		if ((f == stdin) && !quiet && !vss_quiet_mode)
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
		
		cur_cmd_quiet = 0;
		if (VSS_HIDE_CHAR == vss_get_first_non_space_char(cmd_line, &i))
		{
			i++;
			cur_cmd_quiet = 1;
		}
		cmd_ptr = &cmd_line[i];
		
		if ((f != stdin) && !quiet && !vss_quiet_mode && !cur_cmd_quiet)
		{
			if (head != NULL)
			{
				printf("%s", head);
			}
			printf(">>>");
		}
		
		if ((f != stdin) && !quiet && !vss_quiet_mode && !cur_cmd_quiet)
		{
			// run from non-shell mode, print the command line to run
			// in shell mode, the command line will have been printed in fgets
			printf("%s", cmd_line);
		}
		
		if ((vss_cur_function != NULL) && (cmd_ptr != strstr(cmd_ptr, "end_function")))
		{
			if ((cmd_ptr == strstr(cmd_ptr, "function")) && isspace(cmd_ptr[strlen("function")]))
			{
				LOG_ERROR("function nesting not supported");
				return ERROR_FAIL;
			}
			if (ERROR_OK != vss_append_function_cmd(vss_cur_function, cmd_ptr))
			{
				return ERROR_FAIL;
			}
		}
		else if ((ERROR_OK != vss_run_script(cmd_ptr)) && 
				(vss_fatal_error || vss_param[PARAM_EXIT_ON_FAIL].value))
		{
			return ERROR_FAIL;
		}
		if (!quiet && !vss_quiet_mode && !cur_cmd_quiet)
		{
			printf("\n");
		}
		if (vss_exit_mark != 0)
		{
			if (vss_exit_mark > 0)
			{
				vss_exit_mark = 0;
			}
			break;
		}
	}
	
	return ERROR_OK;
}

// commands
// param
VSS_HANDLER(vss_set_parameters)
{
	struct vss_param_t *param = NULL;
	
	VSS_CHECK_ARGC(3);
	
	param = mic_search_param(argv[1]);
	if (NULL == param)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], "parameters");
		vss_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	param->value = strtoul(argv[2], NULL, 0);
	
	return ERROR_OK;
}

// help
VSS_HANDLER(vss_help)
{
	struct vss_cmd_t *cmd = NULL;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (2 == argc)
	{
		cmd = vss_search_cmd(vss_cmds_list, 
			dimof(vss_cmds_list), argv[1]);
		
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
		
		for (i = 0; i < (int)dimof(vss_cmds_list); i++)
		{
			cmd = vss_cmds_list[i];
			
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
VSS_HANDLER(vss_shell)
{
	VSS_CHECK_ARGC(1);
	
	LOG_INFO("enter shell mode.");
	
	if (cur_interface != NULL)
	{
		return vss_run_file(stdin, cur_interface->name, 0);
	}
	else
	{
		return vss_run_file(stdin, "stdin", 0);
	}
}

// run
VSS_HANDLER(vss_run)
{
	FILE *f = NULL;
	uint8_t quiet = 0;
	RESULT ret = ERROR_OK;
	
	VSS_CHECK_ARGC_2(2, 3);
	
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
		ret = vss_run_file(f, (char*)argv[1], quiet);
		fclose(f);
	}
	return ret;
}

// loop
VSS_HANDLER(vss_loop)
{
	VSS_CHECK_ARGC(2);
	
	vss_loop_cnt = strtoul(argv[1], NULL, 0);
	return ERROR_OK;
}

// exit
VSS_HANDLER(vss_exit)
{
	VSS_CHECK_ARGC(1);
	vss_exit_mark = 1;
	return ERROR_OK;
}

// close
VSS_HANDLER(vss_close)
{
	VSS_CHECK_ARGC(1);
	vss_exit_mark = -1;
	return ERROR_OK;
}

VSS_HANDLER(vss_run_command)
{
	vsprog_no_call_operate();
	VSS_CHECK_ARGC(2);
	return vss_run_script((char *)argv[1]);
}

VSS_HANDLER(vss_log_info)
{
	VSS_CHECK_ARGC(2);
	LOG_INFO("%s", argv[1]);
	return ERROR_OK;
}

VSS_HANDLER(vss_getchar)
{
	VSS_CHECK_ARGC(1);
	getchar();
	return ERROR_OK;
}

VSS_HANDLER(vss_sleep)
{
	uint32_t ms;
	VSS_CHECK_ARGC(2);
	ms = (uint32_t)strtoul(argv[1], NULL, 0);
	sleep_ms(ms);
	return ERROR_OK;
}

VSS_HANDLER(vss_quiet)
{
	VSS_CHECK_ARGC_RANGE(1, 2);
	if (NULL == vss_quiet_mode_ptr)
	{
		return ERROR_OK;
	}
	
	if (1 == argc)
	{
		*vss_quiet_mode_ptr = 1;
	}
	else if (2 == argc)
	{
		*vss_quiet_mode_ptr = (uint8_t)strtoul(argv[1], NULL, 0);
	}
	return ERROR_OK;
}

VSS_HANDLER(vss_function_register)
{
	struct vss_function_t *func;
	
	VSS_CHECK_ARGC(2);
	
	if (vss_cur_function != NULL)
	{
		LOG_ERROR("function nesting");
		return ERROR_FAIL;
	}
	
	if (NULL != vss_function_search(vss_functions, (char *)argv[1]))
	{
		LOG_ERROR("function %s already registered!!", argv[1]);
		return ERROR_FAIL;
	}
	
	func = (struct vss_function_t *)malloc(sizeof(struct vss_function_t));
	if (NULL == func)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERROR_FAIL;
	}
	memset(func, 0, sizeof(*func));
	
	if (NULL == bufffunc_malloc_and_copy_str(&func->func_name, (char *)argv[1]))
	{
		free(func);
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERROR_FAIL;
	}
	func->next = vss_functions;
	vss_cur_function = vss_functions = func;
	return ERROR_OK;
}

VSS_HANDLER(vss_function_end)
{
	RESULT ret;
	VSS_CHECK_ARGC(1);
	
	if (NULL == vss_cur_function)
	{
		return ERROR_FAIL;
	}
	
	ret = vss_append_function_cmd(vss_cur_function, NULL);
	vss_cur_function = NULL;
	return ret;
}

VSS_HANDLER(vss_function_call)
{
	struct vss_function_t *func;
	
	VSS_CHECK_ARGC(2);
	
	func = vss_function_search(vss_functions, (char *)argv[1]);
	if ((NULL == func) || (ERROR_OK != vss_function_run(func)))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

VSS_HANDLER(vss_function_free)
{
	struct vss_function_t *func_tmp, *func;
	RESULT ret = ERROR_FAIL;
	
	VSS_CHECK_ARGC_MAX(2);
	
	func = vss_functions;
	if (2 == argc)
	{
		if (NULL == vss_function_search(vss_functions, (char *)argv[1]))
		{
			LOG_ERROR("function %s not exists!!", argv[1]);
			return ERROR_FAIL;
		}
		
		if (!strcmp(func->func_name, argv[1]))
		{
			vss_functions = func->next;
			ret = vss_function_free_node(func);
			func = NULL;
		}
		else
		{
			func_tmp = func;
			func = func->next;
			while (func != NULL)
			{
				if (!strcmp(func->func_name, argv[1]))
				{
					func_tmp->next = func->next;
					ret = vss_function_free_node(func);
					func = NULL;
					break;
				}
				func_tmp = func;
				func = func->next;
			}
		}
	}
	else
	{
		// free all functions
		while (func != NULL)
		{
			func_tmp = func;
			func = func->next;
			ret = vss_function_free_node(func_tmp);
			func_tmp = NULL;
			if (ERROR_OK != ret)
			{
				break;
			}
		}
	}
	
	return ret;
}

