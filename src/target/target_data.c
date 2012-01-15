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

#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "interfaces.h"
#include "target.h"

#include "memlist.h"
#include "filelist.h"
#include "fileparser.h"

extern struct filelist *fl_in, *fl_out;

static void target_free_data_buffer(struct program_context_t *context)
{
	struct program_area_t *prog_area = NULL;
	uint8_t i;
	
	if ((NULL == context) || (NULL == context->pi))
	{
		return;
	}
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		// special_string cannot be freed
		if (SPECIAL_STRING_CHAR == target_area_name[i].name)
		{
			continue;
		}
		
		prog_area = target_get_program_area(context->pi, i);
		if (prog_area != NULL)
		{
			if (prog_area->buff != NULL)
			{
				free(prog_area->buff);
				prog_area->buff = NULL;
			}
			prog_area->size = 0;
			if (prog_area->memlist != NULL)
			{
				MEMLIST_Free(&prog_area->memlist);
			}
			if (prog_area->exact_memlist != NULL)
			{
				MEMLIST_Free(&prog_area->exact_memlist);
			}
		}
	}
}

static vsf_err_t target_alloc_data_buffer(struct program_context_t *context)
{
	struct program_area_t *prog_area = NULL;
	struct chip_area_info_t *area_info = NULL;
	uint8_t i;
	
	if ((NULL == context) || (NULL == context->pi) || (NULL == context->param))
	{
		return VSFERR_FAIL;
	}
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		prog_area = target_get_program_area(context->pi, i);
		if ((prog_area != NULL) && (NULL == prog_area->buff) &&
			(prog_area->size > 0))
		{
			prog_area->buff = (uint8_t *)malloc(prog_area->size);
			if (NULL == prog_area->buff)
			{
				return VSFERR_NOT_ENOUGH_RESOURCES;
			}
			
			area_info = target_get_chip_area(context->param, i);
			if ((strlen(context->param->chip_name) > 0) && (area_info != NULL))
			{
				memset(prog_area->buff, (uint8_t)area_info->default_value,
						prog_area->size);
			}
		}
	}
	
	return VSFERR_NONE;
}

static vsf_err_t target_write_buffer_from_file_callback(char * ext,
				uint32_t address, uint32_t seg_addr, uint8_t* data,
				uint32_t length, void* buffer)
{
	uint32_t i;
	int8_t area_idx;
	char area_name;
	uint8_t *area_buff;
	struct memlist **area_memlist, **area_exact_memlist;
	uint32_t area_seg, area_addr, area_size, area_page_size;
	struct program_context_t *context = (struct program_context_t *)buffer;
	struct target_info_t *target = NULL;
	struct chip_area_info_t *area_info = NULL;
	struct program_area_t *prog_area = NULL;
	uint32_t mem_addr;
	
	if ((NULL == context) || (NULL == context->pi) ||
		(NULL == context->target) || !strlen(target_chip_param.chip_name) ||
		(NULL == ext))
	{
		LOG_BUG(ERRMSG_NOT_INITIALIZED, "target", "");
		return VSFERR_FAIL;
	}
	target = context->target;
	
	// remap if adjust_mapping is defined and format is not BIN
	if ((strcmp(ext, "BIN")) &&
		(target->adjust_mapping != NULL) &&
		target->adjust_mapping(&address, TARGET_MAPPING_FROM_FILE))
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATE_ADDRESS, "remap target address",
				address);
		return VSFERR_FAIL;
	}
	
	// find a right target to fill the memory
	i = 0;
	while (target->program_area_map[i].name != 0)
	{
		area_name = target->program_area_map[i].name;
		area_idx = target_area_idx(area_name);
		if (area_idx < 0)
		{
			i++;
			continue;
		}
		area_info = target_get_chip_area(&target_chip_param, (uint32_t)area_idx);
		prog_area = target_get_program_area(context->pi, (uint32_t)area_idx);
		if ((NULL == area_info) || (NULL == prog_area))
		{
			i++;
			continue;
		}
		
		area_seg = area_info->seg + target->program_area_map[i].fseg_addr;
		area_addr = area_info->addr + target->program_area_map[i].fstart_addr;
		area_size = area_info->size;
		area_page_size = area_info->page_size;
		
		area_buff = prog_area->buff;
		area_memlist = &(prog_area->memlist);
		area_exact_memlist = &(prog_area->exact_memlist);
		
		if ((area_seg != seg_addr) || (area_addr > address)
			|| ((area_addr + area_size) < (address + length)))
		{
			// not this area
			i++;
			continue;
		}
		
		// found
		if (0 == area_page_size)
		{
			// default page size is 256 bytes
			area_page_size = 256;
		}
		context->pi->areas_defined |= target_area_mask(area_name);
		mem_addr = address - area_addr;
		if (area_buff != NULL)
		{
			// put in area_buff
			memcpy(area_buff + mem_addr, data, length);
			if (MEMLIST_Add(area_memlist, address, length, area_page_size))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			if (MEMLIST_Add(area_exact_memlist, address, length, 1))
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		else
		{
			LOG_ERROR(ERRMSG_INVALID_BUFFER, "area_buff");
			return VSFERR_FAIL;
		}
		
		return VSFERR_NONE;
	}
	
	// not found
	return VSFERR_FAIL;
}

vsf_err_t target_data_free(struct program_context_t *context)
{
	if ((NULL == context) || (NULL == context->pi))
	{
		return VSFERR_FAIL;
	}
	
	target_free_data_buffer(context);
	return VSFERR_NONE;
}

vsf_err_t target_data_read(struct program_context_t *context)
{
	uint32_t file_for_read = 0, file_for_write = 0;
	
	if ((NULL == context) || (NULL == context->pi) || (NULL == context->op) ||
		target_data_free(context))
	{
		return VSFERR_FAIL;
	}
	
	// check file
	target_prepare_operations(context, &file_for_read, &file_for_write);
	if ((file_for_read > 0) &&
		((NULL == fl_in) || (NULL == fl_in->path) || (NULL == fl_in->file)))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "input file");
		return VSFERR_FAIL;
	}
	if ((file_for_write > 0) &&
		((NULL == fl_out) || (NULL == fl_out->path)))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "output file");
		return VSFERR_FAIL;
	}
	
	// malloc buffer
	if (target_alloc_data_buffer(context))
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return VSFERR_FAIL;
	}
	
	// read file
	if (file_for_read > 0)
	{
		struct filelist *fl = fl_in;
		
		while ((fl != NULL) && (fl->path != NULL) && (fl->file != NULL)
			&& (strlen(fl->path) > 4))
		{
			if (parse_file(fl->path, fl->file, (void *)context,
								&target_write_buffer_from_file_callback,
								fl->seg_offset, fl->addr_offset))
			{
				LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "parse input file",
							fl->path);
				return VSFERR_FAIL;
			}
			
			fl = FILELIST_GetNext(fl);
		}
	}
	
	return VSFERR_NONE;
}

vsf_err_t target_data_save(struct program_context_t *context)
{
	struct program_area_map_t *p_map;
	
	if ((NULL == context) || (NULL == context->op) || (NULL == context->pi) ||
		(NULL == context->param) || (NULL == context->target) ||
		(NULL == context->target->program_area_map))
	{
		return VSFERR_FAIL;
	}
	p_map = (struct program_area_map_t *)context->target->program_area_map;
	
	while (p_map->name != 0)
	{
		if ((p_map->data_pos) &&
			(context->op->read_operations & target_area_mask(p_map->name)))
		{
			uint8_t *buff = NULL;
			uint32_t size = 0;
			struct chip_area_info_t *area;
			int8_t area_idx;
			
			area_idx = target_area_idx(p_map->name);
			if (area_idx < 0)
			{
				p_map++;
				continue;
			}
			area = target_get_chip_area(context->param, (uint32_t)area_idx);
			if (NULL == area)
			{
				LOG_ERROR(ERRMSG_INVALID_TARGET, "area");
				return VSFERR_FAIL;
			}
			target_get_target_area(p_map->name, &buff, &size);
			if ((buff != NULL) && (size > 0) && (fl_out != NULL))
			{
				if (save_target_to_file(fl_out, buff,
						size, area->seg, area->addr, p_map->fseg_addr,
						p_map->fstart_addr,
						context->target->adjust_mapping))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION,
								"write data to file");
					return VSFERR_FAIL;
				}
			}
		}
		
		p_map++;
	}
	end_file(fl_out);
	
	return VSFERR_NONE;
}
