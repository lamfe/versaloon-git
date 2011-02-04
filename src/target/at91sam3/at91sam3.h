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
#ifndef __AT91SAM3_H_INCLUDED__
#define __AT91SAM3_H_INCLUDED__

#define AT91SAM3_STRING					"at91sam3"

extern struct program_area_map_t at91sam3_program_area_map[];
extern const struct program_mode_t at91sam3_program_mode[];
extern struct program_functions_t at91sam3_program_functions;
extern const struct vss_cmd_t at91sam3_notifier[];

#endif /* __AT91SAM3_H_INCLUDED__ */

