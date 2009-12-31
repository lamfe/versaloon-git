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
#include <stdlib.h>
#include <string.h>

#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "timer.h"

#include "pgbar.h"

static char *end_str = NULL, disp_char = 0;
static int32_t min_num = 0, max_num = 0, position = 0;
static uint32_t max_num_of_chars = 0;
static uint32_t start_time, end_time;
static uint8_t gui_mode_flag = 0;

static uint32_t pgbar_get_char_num(int32_t pos)
{
	return (pos - min_num) * max_num_of_chars / (max_num - min_num);
}

void pgbar_set_gui_mode(uint8_t gui_mode)
{
	gui_mode_flag = gui_mode;
}

void pgbar_update(int32_t step)
{
	int32_t noc;
	uint32_t pos_pre;
	
	pos_pre = position;
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
	if (noc != 0)
	{
		uint8_t erase = 0;
		// update
		// erase previous characters
		if (100 == pgbar_get_char_num(pos_pre) * 100 / max_num_of_chars)
		{
			erase = 4;	// "%100"
		}
		else
		{
			erase = 3;	// "%xx"
		}
		while (erase-- > 0)
		{
			if (!gui_mode_flag)
			{
				printf("\b \b");
			}
		}
		
		while (noc != 0)
		{
			if (noc > 0)
			{
				printf("%c", disp_char);
				noc--;
			}
			else
			{
				printf("\b \b");
				noc++;
			}
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
}

RESULT pgbar_init(char *s, char *e, uint32_t min, uint32_t max, 
				  uint32_t max_chars, char c)
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
	if (gui_mode_flag)
	{
		printf("\n");
	}
	
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

