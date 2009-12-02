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
#ifndef __SVF_PLAYER_H_INCLUDED__
#define __SVF_PLAYER_H_INCLUDED__

#define SVFP_STRING					"svf_player"

extern const program_area_map_t svfp_program_area_map[];

RESULT svfp_parse_argument(char cmd, const char *argu);
RESULT svfp_probe_chip(char *chip_name);
RESULT svfp_prepare_buffer(program_info_t *pi);
RESULT svfp_write_buffer_from_file_callback(uint32_t address, uint32_t seg_addr, 
											uint8_t* data, uint32_t length, 
											void* buffer);
RESULT svfp_init(program_info_t *pi, programmer_info_t *prog);
RESULT svfp_fini(program_info_t *pi, programmer_info_t *prog);
uint32_t svfp_interface_needed(void);
RESULT svfp_program(operation_t operations, program_info_t *pi, 
					programmer_info_t *prog);

#endif /* __SVF_PLAYER_H_INCLUDED__ */

