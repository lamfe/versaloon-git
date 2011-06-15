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
#include <string.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "vsprog.h"
#include "scripts.h"
#include "programmer.h"
#include "strparser.h"

#include "versaloon/versaloon.h"

#include "timer.h"

VSS_HANDLER(programmer_list);
VSS_HANDLER(programmer_define);
VSS_HANDLER(virtualprog_define);

struct vss_cmd_t programmer_cmd[] = 
{
	VSS_CMD(	"display-programmer",
				"list programmers connected, format: display-programmer/L",
				programmer_list),
	VSS_CMD(	"L",
				"list programmers connected, format: display-programmer/L",
				programmer_list),
	VSS_CMD(	"l",
				"define virtual programmer, format: virtualprog/l TARGET",
				virtualprog_define),
	VSS_CMD(	"virtualprog",
				"define virtual programmer, format: virtualprog/l TARGET",
				virtualprog_define),
	VSS_CMD(	"programmer",
				"define programmer to use, format: programmer/p PROGRAMMER",
				programmer_define),
	VSS_CMD(	"p",
				"define programmer to use, format: programmer/p PROGRAMMER",
				programmer_define),
	VSS_CMD_END
};





// scripts support
VSS_HANDLER(programmer_list)
{
	uint32_t i, j = 0;
	
	vsprog_no_call_operate();
	VSS_CHECK_ARGC(1);
	
	for (i = 0; interfaces_info[i] != NULL; i++)
	{
		if (interfaces_info[i]->display_programmer != NULL)
		{
			j += interfaces_info[i]->display_programmer();
		}
	}
	if (0 == j)
	{
		LOG_INFO("no programmer found.");
	}
	return ERROR_OK;
}

VSS_HANDLER(virtualprog_define)
{
	char *ifs = NULL, mode = 0;
	int len;
	
	VSS_CHECK_ARGC(2);
	len = strlen(argv[1]);
	ifs = (char *)argv[1];
	if ((len > 2) && (':' == argv[1][len - 2]))
	{
		ifs[len - 2] = '\0';
		mode = argv[1][len - 1];
	}
	if (ERROR_OK != virtual_interface_init(ifs, mode))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "initialize programmer: ", 
					argv[1]);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

VSS_HANDLER(programmer_define)
{
	char *programmer;
	
	VSS_CHECK_ARGC_2(1, 2);

	programmer = (1 == argc) ? NULL : (char *)argv[1];
	if (ERROR_OK != interface_init(programmer))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "initialize programmer: ", 
					argv[1]);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

