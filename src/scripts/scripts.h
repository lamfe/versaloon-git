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

struct misc_cmd_t
{
	const char *cmd_name;
	const char *help_str;
	RESULT (*processor)(uint16_t argc, const char *argv[]);
};
struct misc_param_t
{
	const char *param_name;
	const char *help_str;
	uint32_t value;
};

#define MISC_HANDLER(name)						\
	static RESULT (name)(uint16_t argc, const char *argv[])

#define MISC_CMD(name, helpstr, handler)		\
	{\
		(name),\
		(helpstr),\
		(handler)\
	}
#define MISC_CMD_END							MISC_CMD(NULL, NULL, NULL)
#define MISC_PARAM(name, helpstr, default)		\
	{\
		(name),\
		(helpstr),\
		(default)\
	}
#define MISC_PARAM_END							MISC_PARAM(NULL, NULL, 0)

#define MISC_CHECK_ARGC(n)						\
	if (argc != (n))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		misc_print_help(argv[0]);\
		return ERROR_FAIL;\
	}
#define MISC_CHECK_ARGC_2(n1, n2)				\
	if ((argc != (n1)) && (argc != (n2)))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		misc_print_help(argv[0]);\
		return ERROR_FAIL;\
	}
#define MISC_CHECK_ARGC_MIN(n)					\
	if (argc < (n))\
	{\
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);\
		misc_print_help(argv[0]);\
		return ERROR_FAIL;\
	}

RESULT misc_cmd_supported(const struct misc_cmd_t *notifier, char *notify_cmd);
RESULT misc_call_notifier(const struct misc_cmd_t *notifier, 
							char *notify_cmd, char *notify_param);
RESULT misc_print_help(const char *name);
RESULT misc_run_script(char *cmd);
RESULT misc_run_cmd(uint16_t argc, const char *argv[]);

