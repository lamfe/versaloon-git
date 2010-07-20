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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "prog_interface.h"
#include "programmer.h"
#include "versaloon/versaloon.h"

#include "timer.h"

#define PROGRAMMER_DEFAULT				0

struct programmer_info_t programmers_info[] = 
{
	// versaloon
	PROGRAMMER_DEFINE(VERSALOON_STRING, 			// name
					  versaloon_check_argument, 	// check_argument
					  versaloon_init_capability, 	// init_capability
					  versaloon_display_programmer),// display_programmer
	PROGRAMMER_DEFINE(NULL, NULL, NULL, NULL)
};

struct programmer_info_t *cur_programmer = NULL;

#define PROGRAMMER_PARAM_EXIT_ON_FAIL				0
struct misc_param_t programmer_param[] =
{
	{
		"exit_on_fail",
		"whether to exit when execute fail",
		0
	},
	{
		NULL,
		NULL,
		0
	}
};

RESULT programmer_script_set_parameters(uint8_t argc, const char *argv[]);
RESULT programmer_script_print_help(uint8_t argc, const char *argv[]);
RESULT programmer_script_shell(uint8_t argc, const char *argv[]);
RESULT programmer_script_run(uint8_t argc, const char *argv[]);
RESULT programmer_script_exit(uint8_t argc, const char *argv[]);
RESULT programmer_script_close(uint8_t argc, const char *argv[]);
RESULT programmer_get_target_voltage(uint8_t argc, const char *argv[]);
RESULT programmer_set_target_voltage(uint8_t argc, const char *argv[]);
RESULT programmer_iic_init(uint8_t argc, const char *argv[]);
RESULT programmer_iic_fini(uint8_t argc, const char *argv[]);
RESULT programmer_iic_read(uint8_t argc, const char *argv[]);
RESULT programmer_iic_write(uint8_t argc, const char *argv[]);
RESULT programmer_test(uint8_t argc, const char *argv[]);

struct misc_cmd_t programmer_cmd[] = 
{
	// param
	{
		"param",
		"set parameters, format: param NAME VALUE",
		programmer_script_set_parameters
	},
	// help
	{
		"help",
		"print help message, format: help <OBJECT>",
		programmer_script_print_help
	},
	// shell
	{
		"shell",
		"enter shell mode, format: shell",
		programmer_script_shell
	},
	// run
	{
		"run",
		"run script file, format: run FILE_NAME",
		programmer_script_run
	},
	// exit
	{
		"exit",
		"exit current session, format: exit",
		programmer_script_exit
	},
	// close
	{
		"close",
		"close program, format: close",
		programmer_script_close
	},
	// voltage
	{
		"get_tvcc",
		"get target voltage, format: get_tvcc",
		programmer_get_target_voltage
	},
	// powerout
	{
		"set_tvcc",
		"output power to target, format: set_tvcc VOLTAGE_IN_MV",
		programmer_set_target_voltage
	},
	// iic
	{
		"iic_init",
		"initialize iic, format: iic_init",
		programmer_iic_init
	},
	{
		"iic_fini",
		"finalize iic, format: iic_fini",
		programmer_iic_fini
	},
	{
		"iic_read",
		"read data from iic, format: iic_read CLOCK_KHZ SLAVE_ADDR DATA_SIZE",
		programmer_iic_read
	},
	{
		"iic_write",
		"write data to iic, format: iic_write CLOCK_KHZ SLAVE_ADDR DATA_SIZE DATA",
		NULL
	},
	// test
	{
		"test",
		"run user defined code, format: test",
		programmer_test
	},
	{
		NULL,
		NULL,
		NULL
	}
};

RESULT programmer_init(const char *programmer)
{
	uint32_t i;
	
	if (programmer != NULL)
	{
		for (i = 0; programmers_info[i].name != NULL; i++)
		{
			if (!strcmp(programmers_info[i].name, programmer))
			{
				cur_programmer = &programmers_info[i];
				return ERROR_OK;
			}
		}
		return ERROR_FAIL;
	}
	else
	{
		cur_programmer = &programmers_info[PROGRAMMER_DEFAULT];
		return ERROR_OK;
	}
}

void programmer_print_list(void)
{
	uint32_t i;
	
	printf("Supported programmers:\n");
	for (i = 0; programmers_info[i].name != NULL; i++)
	{
		programmers_info[i].parse_argument('S', NULL);
	}
	printf("\n");
}

void programmer_print_help(void)
{
	uint32_t i;
	
	for (i = 0; programmers_info[i].name != NULL; i++)
	{
		programmers_info[i].parse_argument('h', NULL);
	}
}


static int8_t programmer_script_exit_mark = 0;
static uint32_t programmer_script_current_cmd_idx = 0;

static RESULT programmer_parse_cmd_line(char *cmd, uint8_t *argc, char **argv)
{
	uint32_t i, cmd_len;
	uint8_t argu_num = 0;
	
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
		argv[argu_num++] = &cmd[i];
		while (!isspace(cmd[i]) && (cmd[i] != '\0'))
		{
			i++;
		}
		cmd[i++] = '\0';
		if (argu_num >= 255)
		{
			break;
		}
	}
	*argc = argu_num;
	
	return ERROR_OK;
}

static RESULT programmer_run_file(FILE *f, char *head)
{
	char cmd_line[4096];
	
	while (1)
	{
		if (head != NULL)
		{
			printf("%s>>>", head);
		}
		
		// get a line
		rewind(f);
		if (NULL == fgets(cmd_line, sizeof(cmd_line), f))
		{
			return ERROR_FAIL;
		}
		
		programmer_run_script(cmd_line);
		printf("\n");
		if (programmer_script_exit_mark != 0)
		{
			if (programmer_script_exit_mark > 0)
			{
				programmer_script_exit_mark = 0;
			}
			break;
		}
	}
	
	return ERROR_OK;
}

static RESULT programmer_run_cmd(uint8_t argc, const char *argv[])
{
	int i;
	RESULT ret = ERROR_OK;
	
	i = 0;
	while (programmer_cmd[i].cmd_name != NULL)
	{
		if (!strcmp(programmer_cmd[i].cmd_name, argv[0]))
		{
			programmer_script_current_cmd_idx = i;
			if (NULL == programmer_cmd[i].processor)
			{
				LOG_ERROR("%s is not implemented.\n", argv[0]);
				ret = ERROR_FAIL;
			}
			else if (ERROR_OK != programmer_cmd[i].processor(argc, argv))
			{
				LOG_ERROR("fail to run %s\n", argv[0]);
				ret = ERROR_FAIL;
			}
			break;
		}
		
		i++;
	}
	if (NULL == programmer_cmd[i].cmd_name)
	{
		LOG_ERROR("command %s is not exists.\n", argv[0]);
		ret = ERROR_FAIL;
	}
	
	return ret;
}

static RESULT programmer_print_current_help(void)
{
	printf("%s\n", programmer_cmd[programmer_script_current_cmd_idx].help_str);
	return ERROR_OK;
}

RESULT programmer_run_script(char *cmd)
{
	uint8_t argc;
	char *argv[255];
	RESULT ret = ERROR_OK;
	
	argc = (uint8_t)dimof(argv);
	programmer_parse_cmd_line(cmd, &argc, (char **)argv);
	
	// run command
	if ((ERROR_OK != programmer_run_cmd(argc, (const char**)argv)) 
		&& programmer_param[PROGRAMMER_PARAM_EXIT_ON_FAIL].value)
	{
		programmer_script_exit_mark = -1;
	}
	
	return ret;
}

// commands
// param
RESULT programmer_script_set_parameters(uint8_t argc, const char *argv[])
{
	int i;
	
	if (3 != argc)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	i = 0;
	while (programmer_param[i].param_name != NULL)
	{
		if (!strcmp(programmer_param[i].param_name, argv[1]))
		{
			programmer_param[i].value = strtoul(argv[2], NULL, 0);
		}
		i++;
	}
	return ERROR_OK;
}

// help
RESULT programmer_script_print_help(uint8_t argc, const char *argv[])
{
	int i;
	
	if ((argc != 2) && (argc != 1))
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	if (2 == argc)
	{
		printf("command %s:\n", argv[1]);
		i = 0;
		while (programmer_cmd[i].cmd_name != NULL)
		{
			if (!strcmp(programmer_cmd[i].cmd_name, argv[1]))
			{
				printf("\t%s: %s\n", programmer_cmd[i].cmd_name, 
						programmer_cmd[i].help_str);
				break;
			}
			i++;
		}
		if (NULL == programmer_cmd[i].cmd_name)
		{
			printf("command %s not found.\n", argv[1]);
		}
	}
	else if (1 == argc)
	{
		printf("command list:\n");
		i = 0;
		while (programmer_cmd[i].cmd_name != NULL)
		{
			printf("\t%s: %s\n", programmer_cmd[i].cmd_name, 
					programmer_cmd[i].help_str);
			i++;
		}
	}
	return ERROR_OK;
}

// shell
RESULT programmer_script_shell(uint8_t argc, const char *argv[])
{
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	LOG_INFO("enter shell mode.\n\n");
	
	programmer_run_file(stdin, cur_programmer->name);
	
	return ERROR_OK;
}

// run
RESULT programmer_script_run(uint8_t argc, const char *argv[])
{
	FILE *f = NULL;
	
	if (argc != 2)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	f = fopen(argv[1], "t");
	if (NULL == f)
	{
		LOG_ERROR("fail to open %s.\n", argv[1]);
		return ERROR_FAIL;
	}
	else
	{
		programmer_run_file(f, NULL);
		
		fclose(f);
	}
	return ERROR_OK;
}

// exit
RESULT programmer_script_exit(uint8_t argc, const char *argv[])
{
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	programmer_script_exit_mark = 1;
	
	return ERROR_OK;
}

// close
RESULT programmer_script_close(uint8_t argc, const char *argv[])
{
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	programmer_script_exit_mark = -1;
	return ERROR_OK;
}

// tvcc
RESULT programmer_get_target_voltage(uint8_t argc, const char *argv[])
{
	struct interface_target_voltage_t *tv = 
						&(cur_programmer->interfaces.target_voltage);
	uint16_t voltage = 0;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != tv->get(&voltage))
	{
		ret = ERROR_FAIL;
	}
	else
	{
		LOG_INFO(_GETTEXT(INFOMSG_TARGET_VOLTAGE), voltage / 1000.0);
	}
	return ret;
}

RESULT programmer_set_target_voltage(uint8_t argc, const char *argv[])
{
	struct interface_target_voltage_t *tv = 
						&(cur_programmer->interfaces.target_voltage);
	uint16_t voltage = 0;
	
	if (argc != 2)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	voltage = (uint16_t)strtoul(argv[1], NULL, 0);
	return tv->set(voltage);
}

// iic
RESULT programmer_iic_init(uint8_t argc, const char *argv[])
{
	struct interface_i2c_t *i2c = &(cur_programmer->interfaces.i2c);
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	i2c->init();
	return cur_programmer->interfaces.peripheral_commit();
}

RESULT programmer_iic_fini(uint8_t argc, const char *argv[])
{
	struct interface_i2c_t *i2c = &(cur_programmer->interfaces.i2c);
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	i2c->fini();
	return cur_programmer->interfaces.peripheral_commit();
}

RESULT programmer_iic_read(uint8_t argc, const char *argv[])
{
	struct interface_i2c_t *i2c = &(cur_programmer->interfaces.i2c);
	uint16_t speed_khz = 0;
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t *buff = NULL;
	RESULT ret = ERROR_OK;
	
	if (argc != 4)
	{
		programmer_print_current_help();
		return ERROR_FAIL;
	}
	
	speed_khz = (uint16_t)strtoul(argv[1], NULL, 0);
	addr = (uint8_t)strtoul(argv[2], NULL, 0);
	data_size = (uint8_t)strtoul(argv[3], NULL, 0);
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return ERROR_FAIL;
	}
	
	i2c->config(speed_khz, 0xFFFF, 0);
	i2c->read(addr, buff, data_size, 1);
	ret = cur_programmer->interfaces.peripheral_commit();
	if (ERROR_OK == ret)
	{
		LOG_BYTE_BUF(buff, data_size, LOG_INFO, "%02X", 16);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

RESULT programmer_test(uint8_t argc, const char *argv[])
{
	REFERENCE_PARAMETER(argc);
	REFERENCE_PARAMETER(argv);
	
	return ERROR_OK;
}

