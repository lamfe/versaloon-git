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
#include "scripts.h"

static uint32_t TARGET_DATA_ADDR = TARGET_DATA_BASE;

VSS_HANDLER(target_data_set_base);

static const struct vss_cmd_t target_data_cmd[] =
{
	VSS_CMD(	"set_base",
				"set base of target data, format: set_base ADDR",
				target_data_set_base,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t target_data_cmd_list =
							VSS_CMD_LIST("target_datat", target_data_cmd);

VSS_HANDLER(target_data_set_base)
{
	uint32_t temp_addr;
	
	VSS_CHECK_ARGC(2);
	
	temp_addr = (uint32_t)strtoul(argv[1], NULL, 0);
	if (temp_addr >= TARGET_DATA_SIZE)
	{
		return VSFERR_FAIL;
	}
	
	TARGET_DATA_ADDR = temp_addr + TARGET_DATA_BASE;
	return VSFERR_NONE;
}

vsf_err_t target_data_free(struct program_context_t *context)
{
	struct program_area_map_t *p_map;
	struct program_area_t *prog_area = NULL;
	int8_t area_idx;
	
	if ((NULL == context) || (NULL == context->target) ||
		(NULL == context->pi) || (NULL == context->param) ||
		(NULL == context->target->program_area_map))
	{
		return VSFERR_NONE;
	}
	p_map = (struct program_area_map_t *)context->target->program_area_map;
	
	while (p_map->name != 0)
	{
		area_idx = target_area_idx(p_map->name);
		if (area_idx < 0)
		{
			p_map++;
			continue;
		}
		prog_area = target_get_program_area(context->pi, (uint32_t)area_idx);
		if (NULL == prog_area)
		{
			p_map++;
			continue;
		}
		
		if ((prog_area->buff != NULL) && !p_map->data_pos)
		{
			free(prog_area->buff);
			prog_area->buff = NULL;
		}
		
		p_map++;
	}
	
	return VSFERR_NONE;
}

vsf_err_t target_data_read(struct program_context_t *context)
{
	uint32_t pos;
	struct program_area_map_t *p_map;
	struct program_area_t *prog_area = NULL;
	struct chip_area_info_t *area_info = NULL;
	int8_t area_idx;
	
	if ((NULL == context) || (NULL == context->target) ||
		(NULL == context->pi) || (NULL == context->param) ||
		(NULL == context->target->program_area_map))
	{
		return VSFERR_FAIL;
	}
	p_map = (struct program_area_map_t *)context->target->program_area_map;
	
	// target data is in flash
	target_prepare_operations(context, NULL, NULL);
	
	while (p_map->name != 0)
	{
		area_idx = target_area_idx(p_map->name);
		if (area_idx < 0)
		{
			p_map++;
			continue;
		}
		prog_area = target_get_program_area(context->pi, (uint32_t)area_idx);
		if (NULL == prog_area)
		{
			p_map++;
			continue;
		}
		
		if ((NULL == prog_area->buff) && (prog_area->size > 0))
		{
			if (!p_map->data_pos)
			{
				prog_area->buff = (uint8_t *)malloc(prog_area->size);
				if (NULL == prog_area->buff)
				{
					return VSFERR_NOT_ENOUGH_RESOURCES;
				}
				
				area_info = target_get_chip_area(context->param,
													(uint32_t)area_idx);
				if ((strlen(context->param->chip_name) > 0) &&
					(area_info != NULL))
				{
					memset(prog_area->buff, (uint8_t)area_info->default_value,
							prog_area->size);
				}
			}
			else
			{
				pos = TARGET_DATA_ADDR +
						area_idx * sizeof(struct program_area_t);
				
				prog_area->cli_str = *(char **)pos;
				pos += sizeof(char *);
				prog_area->buff = *(uint8_t **)pos;
				pos += sizeof(uint8_t *);
				prog_area->size = *(uint32_t *)pos;
				pos += sizeof(uint32_t);
				prog_area->memlist = *(struct memlist **)pos;
				pos += sizeof(struct memlsit *);
				prog_area->exact_memlist = *(struct memlist **)pos;
				pos += sizeof(struct memlist *);
				
				context->pi->areas_defined = 1 << area_idx;
			}
		}
		
		p_map++;
	}
	
	return VSFERR_NONE;
}

vsf_err_t target_data_save(struct program_context_t *context)
{
	struct program_area_map_t *p_map;
	
	if ((NULL == context) || (NULL == context->op) ||
		(NULL == context->target) ||
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
			LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "save target data",
						"embedded vsprog");
			return VSFERR_FAIL;
		}
		
		p_map++;
	}
	
	return VSFERR_NONE;
}
