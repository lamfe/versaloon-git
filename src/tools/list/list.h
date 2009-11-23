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

#ifndef __LIST_H_INCLUDED__
#define __LIST_H_INCLUDED__

typedef struct SLLIST
{
	struct SLLIST *next;
} sllist;

#define sllist_init_node(node)		((node).next = NULL)
#define sllist_append(new, last)	((last).next = new)
#define sllist_get_container(node, type, member)	\
	(node ? \
		((type *)((char *)(node)-(unsigned long)&(((type *)0)->member))) \
		: NULL)

#endif // __LIST_H_INCLUDED__

