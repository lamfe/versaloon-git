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
#ifndef __CM3_INTERNAL_H_INCLUDED__
#define __CM3_INTERNAL_H_INCLUDED__

#define CM3_STM32		0
#define CM3_LPC1700		1
#define CM3_LM3S		2
#define CM3_SAM3		3

struct cm3_param_t
{
	const char *chip_name;
	uint8_t default_char;
	uint32_t flash_start_addr;
	uint32_t flash_max_size;
	uint16_t jtag_khz;
	struct jtag_pos_t pos;
	uint8_t swj_trn;
};

extern struct cm3_param_t *cm3_chip_param;
extern uint8_t cm3_execute_flag;
extern uint32_t cm3_execute_addr;
extern uint16_t cm3_buffer_size;
extern const struct cm3_param_t cm3_chips_param[];

#endif /* __CM3_INTERNAL_H_INCLUDED__ */

