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

#ifndef __APP_TYPE_H_INCLUDED__
#define __APP_TYPE_H_INCLUDED__

#include <stdint.h>

typedef enum result_s
{
	ERROR_OK = 0,
	ERROR_FAIL = 1
} RESULT;

#define dimof(arr)				(sizeof(arr) / sizeof((arr)[0]))
#ifndef min
#	define min(a,b)				(((a) < (b)) ? (a) : (b))
#endif
#ifndef max
#	define max(a,b)				(((a) > (b)) ? (a) : (b))
#endif
#define TO_STR(name)			#name
#define REFERENCE_PARAMETER(a)	(a) = (a)

#endif /* __APP_TYPE_H_INCLUDED__ */

