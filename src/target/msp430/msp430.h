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
#ifndef __MSP430_H_INCLUDED__
#define __MSP430_H_INCLUDED__

#define MSP430_STRING					"msp430"
#define MSP430_PROGRAM_MODE_STR			"jsb"

extern const program_area_map_t msp430_program_area_map[];

RESULT msp430_parse_argument(char cmd, const char *argu);
RESULT msp430_probe_chip(char *chip_name);
RESULT msp430_prepare_buffer(program_info_t *pi);
RESULT msp430_write_buffer_from_file_callback(uint32_t address, uint32_t seg_addr, 
											  uint8_t* data, uint32_t length, 
											  void* buffer);
RESULT msp430_init(program_info_t *pi, const char *dir, 
				   programmer_info_t *prog);
RESULT msp430_fini(program_info_t *pi, programmer_info_t *prog);
uint32_t msp430_interface_needed(void);
RESULT msp430_program(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog);

#endif /* __MSP430_H_INCLUDED__ */

