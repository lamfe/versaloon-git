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
#ifndef __PSOC1_H_INCLUDED__
#define __PSOC1_H_INCLUDED__

#define PSOC1_STRING					"psoc1"

extern const struct program_area_map_t psoc1_program_area_map[];
extern const struct program_mode_t psoc1_program_mode[];
extern const struct program_functions_t psoc1_program_functions;

PARSE_ARGUMENT_HANDLER(psoc1);
ADJUST_SETTING_HANDLER(psoc1);

#endif /* __PSOC1_H_INCLUDED__ */

