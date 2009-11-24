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
#include <ctype.h>

#include "app_type.h"
#include "app_log.h"
#include "app_err.h"
#include "file_parser.h"

#include "port.h"

#include "hex.h"
//#include "bin.h"

static file_parser_t file_parser[] = 
{
	{"HEX", read_hex_file}
};

static uint8_t check_file_ext(char *file_name, char *ext)
{
	uint8_t i;
	char *file_ext = &file_name[strlen(file_name) - 1];
	
	while ((*file_ext != '.') && (*file_ext != FILE_SEPARATOR))
	{
		file_ext--;
	}
	if ((*file_ext != '.') || ('\0' == *(file_ext + 1)))
	{
		return 0;
	}
	file_ext++;
	
	// check
	for (i = 0; i < strlen(ext); i++)
	{
		if (toupper(file_ext[i]) != ext[i])
		{
			return 0;
		}
	}
	return 1;
}

RESULT parse_file(char *file_name, FILE *file, void *para, 
				  WRITE_MEMORY_CALLBACK callback, 
				  uint32_t seg_offset, uint32_t addr_offset)
{
	uint8_t i;
	
	for (i = 0; i < dimof(file_parser); i++)
	{
		// check file ext
		if (check_file_ext(file_name, file_parser[i].ext))
		{
			break;
		}
	}
	if (i >= dimof(file_parser))
	{
		// file type not supported
		// return SUCCESS, hope target code will process this
		return ERROR_OK;
	}
	
	// call parser
	return file_parser[i].parser(file, callback, para, 
									seg_offset, addr_offset);
}

