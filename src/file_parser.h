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

#ifndef __FILE_PARSER_H_INCLUDED__
#define __FILE_PARSER_H_INCLUDED__

typedef RESULT (*WRITE_MEMORY_CALLBACK)(uint32_t, uint32_t, uint8_t*, 
										uint32_t, void *);

typedef struct
{
	char ext[4];
	RESULT (*parser)(FILE *hex_file, WRITE_MEMORY_CALLBACK callback, 
					 void *buffer, uint32_t seg_offset, uint32_t addr_offset);
} file_parser_t;

RESULT parse_file(char *file_name, FILE *file, void *para, 
				  WRITE_MEMORY_CALLBACK callback, 
				  uint32_t seg_offset, uint32_t addr_offset);

#endif //__FILE_PARSER_H_INCLUDED__

