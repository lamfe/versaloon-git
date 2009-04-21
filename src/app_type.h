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

#ifndef __APP_TYPE_H_INCLUDED__
#define __APP_TYPE_H_INCLUDED__

typedef enum
{
	ERROR_OK = 0,
	ERROR_FAIL = 1
} RESULT;

// typedefines
typedef unsigned char	uint8;
typedef signed char		int8;
typedef unsigned short	uint16;
typedef signed short	int16;
typedef unsigned int	uint32;
typedef signed int		int32;

#define dimof(arr)				(sizeof(arr) / sizeof(arr[0]))
#define TO_STR(name)			#name

#endif /* __APP_TYPE_H_INCLUDED__ */

