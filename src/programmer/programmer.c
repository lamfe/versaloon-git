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
#include <stdio.h>
#include <string.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "programmer.h"
#include "versaloon/versaloon.h"

#define PROGRAMMER_DEFAULT				0

programmer_info_t programmers_info[] = 
{
	// versaloon
	{
		VERSALOON_STRING,			// name
		versaloon_check_argument,	// check_argument
		versaloon_init_capability,	// init_capability
	},
	{ NULL }
};

programmer_info_t *cur_programmer = NULL;

RESULT programmer_init(const char *programmer, const char *app_dir)
{
	uint32 i;
	
	app_dir = app_dir;
	
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
	uint32 i;
	
	printf("Supported programmers:\n");
	for (i = 0; programmers_info[i].name != NULL; i++)
	{
		programmers_info[i].parse_argument('S', NULL);
	}
	printf("\n");
}

void programmer_print_help(void)
{
	uint32 i;
	
	for (i = 0; programmers_info[i].name != NULL; i++)
	{
		programmers_info[i].parse_argument('h', NULL);
	}
}

