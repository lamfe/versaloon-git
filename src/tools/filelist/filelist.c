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

#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "filelist.h"

static void FILELIST_InsertLast(filelist *fl, filelist *newitem)
{
	if (NULL == fl)
	{
		return;
	}
	
	while (FILELIST_GetNext(fl) != NULL)
	{
		fl = FILELIST_GetNext(fl);
	}
	
	fl->list.next = &newitem->list;
}

RESULT FILELIST_Open(filelist *fl, char *attr)
{
	if (NULL == fl)
	{
		return ERROR_FAIL;
	}
	
	do{
		if ((NULL == fl->file) && (fl->path != NULL))
		{
			fl->file = fopen((const char *)fl->path, (const char *)attr);
			if (NULL == fl->file)
			{
				return ERROR_FAIL;
			}
		}
		fl = FILELIST_GetNext(fl);
	} while(fl != NULL);
	
	return ERROR_OK;
}

RESULT FILELIST_Add(filelist **fl, char *path, uint32_t seg_offset, 
					uint32_t addr_offset)
{
	filelist *newitem = NULL;
	
	if (NULL == fl)
	{
		return ERROR_FAIL;
	}
	
	newitem = (filelist*)malloc(sizeof(filelist));
	if (NULL == newitem)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		return ERROR_FAIL;
	}
	newitem->path = (char *)malloc(strlen(path) + 1);
	if (NULL == newitem->path)
	{
		free(newitem);
		newitem = NULL;
		
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		return ERROR_FAIL;
	}
	strcpy(newitem->path, path);
	newitem->seg_offset = seg_offset;
	newitem->addr_offset = addr_offset;
	sllist_init_node(newitem->list);
	newitem->file = NULL;
	
	if (NULL == *fl)
	{
		*fl = newitem;
	}
	else
	{
		FILELIST_InsertLast(*fl, newitem);
	}
	
	return ERROR_OK;
}

void FILELIST_Free(filelist **fl)
{
	filelist *tmp1, *tmp2;
	
	if (NULL == fl)
	{
		return;
	}
	
	tmp1 = *fl;
	while (tmp1 != NULL)
	{
		tmp2 = tmp1;
		tmp1 = FILELIST_GetNext(tmp1);
		sllist_init_node(tmp2->list);
		if (tmp2->path != NULL)
		{
			free(tmp2->path);
			tmp2->path = NULL;
		}
		if (tmp2->file != NULL)
		{
			fclose(tmp2->file);
			tmp2->file = NULL;
		}
		free(tmp2);
	}
	tmp1 = tmp2 = NULL;
	*fl = NULL;
}

