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
#include <ctype.h>

#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "bufffunc.h"

uint64_t bufffunc_get_u64(uint8_t *buff, uint32_t size)
{
	uint8_t i;
	uint64_t ret;
	
	if (NULL == buff)
	{
		return 0;
	}
	
	if (size > 8)
	{
		size = 8;
	}
	
	ret = 0;
	for (i = 0; i < size; i++)
	{
		ret += (uint64_t)buff[i] << (i * 8);
	}
	return ret;
}

char* bufffunc_malloc_and_copy_str(char** dest, char* src)
{
	if (NULL == src)
	{
		return NULL;
	}
	if (*dest != NULL)
	{
		free(*dest);
		*dest = NULL;
	}
	
	*dest = (char*)malloc(strlen(src) + 1);
	if (NULL == *dest)
	{
		return NULL;
	}
	strcpy(*dest, src);
	return *dest;
}

