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
#define AVR8_PROGRAM_MODE_STR			"ijps"

extern const program_area_map_t avr8_program_area_map[];

RESULT avr8_parse_argument(char cmd, const char *argu);
RESULT avr8_prepare_buffer(program_info_t *pi);

RESULT avr8_init(program_info_t *pi, programmer_info_t *prog);
RESULT avr8_fini(program_info_t *pi, programmer_info_t *prog);
uint32_t avr8_interface_needed(void);
RESULT avr8_write_buffer_from_file_callback(uint32_t address, uint32_t seg_addr, 
											uint8_t* data, uint32_t length, 
											void* buffer);

RESULT avr8_program(operation_t operations, program_info_t *pi, 
					programmer_info_t *prog);

RESULT avr8_get_mass_product_data_size(operation_t operations, 
									   program_info_t pi, uint32_t *size);
RESULT avr8_prepare_mass_product_data(operation_t operations, 
									  program_info_t pi, uint8_t *buff);

#endif /* __AVR8_H_INCLUDED__ */

