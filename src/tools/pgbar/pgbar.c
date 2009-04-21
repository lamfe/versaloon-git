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
#include <stdlib.h>
#include <string.h>

#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "timer.h"

#include "pgbar.h"

static char *end_str = NULL, disp_char = 0;
static int32 min_num = 0, max_num = 0, position = 0;
static uint32 max_num_of_chars = 0;
static uint32 start_time, end_time;
static uint8 gui_mode_flag = 0;

static uint32 pgbar_get_char_num(int32 pos)
{
	return (pos - min_num) * max_num_of_chars / (max_num - min_num);
}

void pgbar_set_gui_mode(uint8 gui_mode)
{
	gui_mode_flag = gui_mode;
}

void pgbar_update(uint32 step)
{
	uint32 noc;
	
	// erase previous characters
	noc = 3;		// 3 is "%xx"
	while (noc-- > 0)
	{
		if (!gui_mode_flag)
		{
			printf("\b");
		}
	}
	noc = pgbar_get_char_num(position);
	
	// adjust new position
	position += step;
	if (position > max_num)
	{
		position = max_num;
	}
	else if (position < min_num)
	{
		position = min_num;
	}
	
	// output new characters
	noc = pgbar_get_char_num(position) - noc;
	while (noc-- > 0)
	{
		printf("%c", disp_char);
	}
	
	// output percentage
	printf("%%%02d", pgbar_get_char_num(position) * 100 / max_num_of_chars);
	
	if (gui_mode_flag)
	{
		printf("\n");
	}
	
	// flush output
	fflush(stdout);
}

RESULT pgbar_init(char *s, char *e, uint32 min, uint32 max, 
				  uint32 max_chars, char c)
{
	// save settings
	if (e != NULL)
	{
		end_str = (char*)malloc(strlen(e) + 1);
		if (NULL == end_str)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
			return ERRCODE_NOT_ENOUGH_MEMORY;
		}
		strcpy(end_str, (const char *)e);
	}
	max_num = max;
	position = min_num = min;
	max_num_of_chars = max_chars;
	disp_char = c;
	
	// print initial string
	if (NULL != s)
	{
		printf("%s", s);
	}
	// print initial percentage
	printf("%%00");
	
	pgbar_update(0);
	
	// get start time
	start_time = get_time_in_ms();
	
	// flush output
	fflush(stdout);
	return ERROR_OK;
}

void pgbar_fini(void)
{
	// print final string
	if (end_str != NULL)
	{
		printf("%s ", end_str);
		// free allocated memory
		free(end_str);
		end_str = NULL;
	}
	
	// get current time and calculate time used
	end_time = get_time_in_ms();
	printf("%02.02fs used\n", (float)(end_time - start_time) / 1000);
	
	// flush output
	fflush(stdout);
}

