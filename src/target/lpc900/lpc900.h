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
#ifndef __LPC900_H_INCLUDED__
#define __LPC900_H_INCLUDED__

#define LPC900_STRING					"lpc900"
#define LPC900_PROGRAM_MODE_STR			""

extern const struct program_area_map_t lpc900_program_area_map[];

RESULT lpc900_parse_argument(char cmd, const char *argu);
RESULT lpc900_prepare_buffer(struct program_info_t *pi);

RESULT lpc900_init(struct program_info_t *pi, struct programmer_info_t *prog);
RESULT lpc900_fini(struct program_info_t *pi, struct programmer_info_t *prog);
uint32_t lpc900_interface_needed(void);
RESULT lpc900_write_buffer_from_file_callback(uint32_t address, 
			uint32_t seg_addr, uint8_t* data, uint32_t length, void* buffer);

RESULT lpc900_program(struct operation_t operations, struct program_info_t *pi, 
					  struct programmer_info_t *prog);

#endif /* __LPC900_H_INCLUDED__ */

