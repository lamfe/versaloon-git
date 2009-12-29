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
#ifndef __AVR8_H_INCLUDED__
#define __AVR8_H_INCLUDED__

#define AVR8_STRING						"avr8"

extern const struct program_area_map_t avr8_program_area_map[];
extern const struct program_mode_t avr8_program_mode[];

RESULT avr8_parse_argument(char cmd, const char *argu);
RESULT avr8_program(struct operation_t operations, struct program_info_t *pi, 
					struct programmer_info_t *prog);

RESULT avr8_get_mass_product_data_size(struct operation_t operations, 
									struct program_info_t pi, uint32_t *size);
RESULT avr8_prepare_mass_product_data(struct operation_t operations, 
									struct program_info_t pi, uint8_t *buff);

#endif /* __AVR8_H_INCLUDED__ */

