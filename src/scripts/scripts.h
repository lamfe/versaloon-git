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

struct vss_cmd_t
{
	const char *cmd_name;
	const char *help_str;
	RESULT (*processor)(uint16_t argc, const char *argv[]);
};
struct vss_param_t
{
	const char *param_name;
	const char *help_str;
	uint32_t value;
};
struct vss_function_cmd_t
{
	char *func_cmd;
	struct vss_function_cmd_t *next;
};
struct vss_function_t
{
	char *func_name;
	struct vss_function_cmd_t *cmds;
	struct vss_function_t *next;
};

#define VSS_HANDLER(name)						\
	RESULT (name)(uint16_t argc, const char *argv[])

#define VSS_CMD(name, helpstr, handler)			\
	{\
		(name),\
		(helpstr),\
		(handler)\
	}
#define VSS_CMD_END								VSS_CMD(NULL, NULL, NULL)
#define VSS_PARAM(name, helpstr, default)		\
	{\
		(name),\
		(helpstr),\
		(default)\
	}
#define VSS_PARAM_END							VSS_PARAM(NULL, NULL, 0)

#define VSS_CHECK_ARGC(n)						\
	if (argc != (n))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return ERROR_FAIL;\
	}
#define VSS_CHECK_ARGC_2(n1, n2)				\
	if ((argc != (n1)) && (argc != (n2)))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return ERROR_FAIL;\
	}
#define VSS_CHECK_ARGC_MIN(n)					\
	if (argc < (n))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return ERROR_FAIL;\
	}
#define VSS_CHECK_ARGC_MAX(n)					\
	if (argc > (n))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return ERROR_FAIL;\
	}
#define VSS_CHECK_ARGC_RANGE(min, max)			\
	if ((argc < (min)) || (argc > (max)))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		vss_print_help(argv[0]);\
		return ERROR_FAIL;\
	}

#define VSS_COMMENT_CHAR						'#'
#define VSS_HIDE_CHAR							'@'

void vss_set_fatal_error(void);
RESULT vss_cmd_supported_by_notifier(const struct vss_cmd_t *notifier, 
										char *notify_cmd);
RESULT vss_call_notifier(const struct vss_cmd_t *notifier, 
							char *notify_cmd, char *notify_param);
RESULT vss_cmd_supported(char *name);
RESULT vss_print_help(const char *name);
RESULT vss_run_script(char *cmd);
RESULT vss_run_cmd(uint16_t argc, const char *argv[]);
RESULT vss_get_binary_buffer(uint16_t argc, const char *argv[], 
						uint8_t data_size, uint32_t data_num, void **pbuff);

