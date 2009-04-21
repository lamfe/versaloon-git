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
#define AVR8_FUSE_CHAR					0xFFFFFFFF

#define AVR8_ISP						(1 << 0)
#define AVR8_JTAG						(1 << 1)
#define AVR8_HVPP						(1 << 2)
#define AVR8_HVSP						(1 << 3)
#define AVR8_PROG_MODE_MASK				(AVR8_ISP | AVR8_JTAG \
										 | AVR8_HVPP | AVR8_HVSP)
#define AVR8_JTAG_FULL_BITSTREAM		(1 << 4)

#define AVR8_ISP_INTERFACE_NEEDED		(SPI | GPIO)
#define AVR8_JTAG_INTERFACE_NEEDED		(JTAG_HL)
#define AVR8_HVPP_INTERFACE_NEEDED		(GPIO | POWER)
#define AVR8_HVSP_INTERFACE_NEEDED		(GPIO | POWER)

typedef struct
{
	const char *chip_name;
	uint32 signature;
	uint8 prog_mode;
	uint16 flash_page_size;
	uint16 flash_page_num;
	uint8 eeprom_page_size;
	uint16 eeprom_page_num;
	uint32 flash_size;
	uint16 eeprom_size;
}avr8_param_t;

extern avr8_param_t avr8_chip_param;
extern uint16 avr8_isp_frequency;

RESULT avr8_isp_program(operation_t operations, program_info_t *pi, 
						programmer_info_t *prog);
RESULT avr8_jtag_program(operation_t operations, program_info_t *pi, 
						 programmer_info_t *prog);
RESULT avr8_hvpp_program(operation_t operations, program_info_t *pi, 
						 programmer_info_t *prog);
RESULT avr8_hvsp_program(operation_t operations, program_info_t *pi, 
						 programmer_info_t *prog);
 
#endif /* __AVR8_INTERNAL_H_INCLUDED__ */

