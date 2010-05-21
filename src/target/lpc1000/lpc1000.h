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
#ifndef __LPC1000_H_INCLUDED__
#define __LPC1000_H_INCLUDED__

#define LPC1000_STRING					"lpc1000"

extern struct program_area_map_t lpc1000_program_area_map[];
extern const struct program_mode_t lpc1000_program_mode[];
extern struct program_functions_t lpc1000_program_functions;

RESULT lpc1000_parse_argument(char cmd, const char *argu);

#endif /* __LPC1000_H_INCLUDED__ */

