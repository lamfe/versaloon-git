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

extern const program_area_map_t lpc900_program_area_map[];

RESULT lpc900_parse_argument(char cmd, const char *argu);
RESULT lpc900_probe_chip(char *name);
RESULT lpc900_prepare_buffer(program_info_t *pi);

RESULT lpc900_init(program_info_t *pi, const char *dir, 
				   programmer_info_t *prog);
RESULT lpc900_fini(program_info_t *pi, programmer_info_t *prog);
uint32 lpc900_interface_needed(void);
RESULT lpc900_write_buffer_from_file_callback(uint32 address, uint32 seg_addr, 
											  uint8* data, uint32 length, 
											  void* buffer);

RESULT lpc900_program(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog);

#endif /* __LPC900_H_INCLUDED__ */

