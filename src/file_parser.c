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

#include "app_type.h"
#include "app_log.h"
#include "app_err.h"
#include "file_parser.h"

#include "port.h"

#include "hex.h"

RESULT read_bin_file(FILE *bin_file, WRITE_MEMORY_CALLBACK callback, 
					void *buffer, uint32_t seg_offset, uint32_t addr_offset);
RESULT write_bin_file(FILE *bin_file, uint32_t file_addr, uint8_t *buff, 
					uint32_t buff_size, uint32_t seg_addr, uint32_t start_addr);

static struct file_parser_t file_parser[] = 
{
	{"HEX", read_hex_file, write_hex_file, write_hex_file_end}, 
	{"BIN", read_bin_file, write_bin_file, NULL}
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

static RESULT get_file_parser(char *file_name, uint8_t *index)
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
		return ERROR_FAIL;
	}
	
	*index = i;
	return ERROR_OK;
}

RESULT parse_file(char *file_name, FILE *file, void *para, 
				  WRITE_MEMORY_CALLBACK callback, 
				  uint32_t seg_offset, uint32_t addr_offset)
{
	uint8_t i;
	
	if (ERROR_OK != get_file_parser(file_name, &i) 
		|| (NULL == file_parser[i].parse_file))
	{
		// hope target handler will handle this file
		return ERROR_OK;
	}
	
	return file_parser[i].parse_file(file, callback, para, 
										seg_offset, addr_offset);
}

RESULT end_file(struct filelist *fl)
{
	if (NULL == fl)
	{
		return ERROR_FAIL;
	}
	
	do {
		uint8_t i;
		
		if ((fl->file != NULL) && (fl->path != NULL) && fl->access)
		{
			if (ERROR_OK != get_file_parser(fl->path, &i))
			{
				continue;
			}
			
			if ((file_parser[i].end_file != NULL) 
				&& (ERROR_OK != file_parser[i].end_file(fl->file)))
			{
				return ERROR_FAIL;
			}
		}
		
		fl = FILELIST_GetNext(fl);
	} while (fl != NULL);
	
	return ERROR_OK;
}

RESULT save_target_to_file(struct filelist *fl, uint8_t *buff, 
					uint32_t buff_size, uint32_t seg_addr, uint32_t start_addr)
{
	uint8_t i;
	struct filelist *target_file = fl;
	
	if (NULL == fl)
	{
		return ERROR_FAIL;
	}
	
	// find a most suitable file to write
	do {
		if ((seg_addr == fl->seg_offset) && (start_addr >= fl->addr_offset))
		{
			target_file = fl;
			// if start_addr is match, that's it
			// else find for better match
			if (start_addr == fl->addr_offset)
			{
				break;
			}
		}
		fl = FILELIST_GetNext(fl);
	} while ((fl != NULL) && (fl->path != NULL));
	
	// write target to file
	if (NULL == target_file->path)
	{
		return ERROR_FAIL;
	}
	if (NULL == target_file->file)
	{
		target_file->file = fopen(target_file->path, "wb");
		if (NULL == target_file->file)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_HANDLE_DEVICE), "open", 
						target_file->path);
			return ERROR_FAIL;
		}
	}
	
	if (ERROR_OK != get_file_parser(target_file->path, &i) 
		|| (NULL == file_parser[i].save_target_to_file))
	{
		return ERROR_FAIL;
	}
	
	target_file->access = 1;
	return file_parser[i].save_target_to_file(target_file->file, 
							target_file->addr_offset, buff, buff_size, 
							seg_addr - target_file->seg_offset, start_addr);
}

RESULT read_bin_file(FILE *bin_file, WRITE_MEMORY_CALLBACK callback, 
					 void *buffer, uint32_t seg_offset, uint32_t addr_offset)
{
	uint8_t cur_buff[8];
	uint32_t addr = 0, cur_len;
	RESULT ret;
	
	rewind(bin_file);
	while (1)
	{
		cur_len = fread(cur_buff, 1, sizeof(cur_buff), bin_file);
		if (0 == cur_len)
		{
			if (feof(bin_file))
			{
				break;
			}
			else
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							"read input binary file");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		
		ret = callback(addr + addr_offset, seg_offset, 
						cur_buff, cur_len, buffer);
		if (ret != ERROR_OK)
		{
			return ERROR_FAIL;
		}
		addr += cur_len;
	}
	
	return ERROR_OK;
}

RESULT write_bin_file(FILE *bin_file, uint32_t file_addr, 
						uint8_t *buff, uint32_t buff_size, 
						uint32_t seg_addr, uint32_t start_addr)
{
	uint32_t file_size = 0;
	RESULT ret = ERROR_OK;
	
	// seg_addr is not used in binary file
	seg_addr = seg_addr;
	
	// check
	if (start_addr < file_addr)
	{
		return ERROR_FAIL;
	}
	
	// get file size
	fseek(bin_file, 0L, SEEK_END);
	file_size = ftell(bin_file);
	rewind(bin_file);
	
	if (file_size < (start_addr - file_addr))
	{
		uint32_t append_size = start_addr - file_addr - file_size;
		uint8_t *buff_append = (uint8_t *)malloc(append_size);
		if (NULL == buff_append)
		{
			return ERROR_FAIL;
		}
		memset(buff_append, 0, append_size);
		
		if (fwrite(buff_append, 1, append_size, bin_file) != append_size)
		{
			ret = ERROR_FAIL;
		}
		free(buff_append);
		buff_append = NULL;
	}
	if (ERROR_FAIL == ret)
	{
		return ret;
	}
	
	// write data
	if (fwrite(buff, 1, buff_size, bin_file) != buff_size)
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

