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
#ifndef __C8051F_H_INCLUDED__
#define __C8051F_H_INCLUDED__

#define C8051F_STRING					"c8051f"
#define C8051F_PROGRAM_MODE_STR			"jc"

extern const program_area_map_t c8051f_program_area_map[];

RESULT c8051f_parse_argument(char cmd, const char *argu);
RESULT c8051f_probe_chip(char *chip_name);
RESULT c8051f_prepare_buffer(program_info_t *pi);
RESULT c8051f_write_buffer_from_file_callback(uint32 address, uint32 seg_addr, 
											  uint8* data, uint32 length, 
											  void* buffer);
RESULT c8051f_init(program_info_t *pi, const char *dir, 
				   programmer_info_t *prog);
RESULT c8051f_fini(program_info_t *pi, programmer_info_t *prog);
uint32 c8051f_interface_needed(void);
RESULT c8051f_program(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog);

#endif /* __C8051F_H_INCLUDED__ */

