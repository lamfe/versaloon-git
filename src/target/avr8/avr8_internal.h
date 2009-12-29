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

#ifndef __AVR8_INTERNAL_H_INCLUDED__
#define __AVR8_INTERNAL_H_INCLUDED__

#define AVR8_MAX_FLASH_SIZE				(256 * 1024)
#define AVR8_MAX_EEPROM_SIZE			(4 * 1024)

#define AVR8_FLASH_CHAR					0xFF
#define AVR8_EEPROM_CHAR				0xFF
#define AVR8_LOCK_CHAR					0xFF
#define AVR8_FUSE_CHAR					0xFF

#define AVR8_ISP						0
#define AVR8_JTAG						1
#define AVR8_HVPP						2
#define AVR8_HVSP						3

#define AVR8_PARAM_JTAG_FULL_BITSTREAM	0
#define AVR8_PARAM_ISP_EERPOM_PAGE_EN	1
#define AVR8_PARAM_ISP_POLL				2

extern uint16_t avr8_isp_frequency;
RESULT avr8_isp_program(struct operation_t operations, 
						struct program_info_t *pi, 
						struct programmer_info_t *prog);
RESULT avr8_jtag_program(struct operation_t operations, 
						struct program_info_t *pi, 
						struct programmer_info_t *prog);
RESULT avr8_hvpp_program(struct operation_t operations, 
						struct program_info_t *pi, 
						struct programmer_info_t *prog);
RESULT avr8_hvsp_program(struct operation_t operations, 
						struct program_info_t *pi, 
						struct programmer_info_t *prog);
#endif /* __AVR8_INTERNAL_H_INCLUDED__ */

