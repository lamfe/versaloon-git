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

#ifndef __AT91SAM3_INTERNAL_H_INCLUDED__
#define __AT91SAM3_INTERNAL_H_INCLUDED__

#define AT91SAM3_FLASH_PAGESIZE		256
#define AT91SAM3_FLASH_ADDR			0x00400000
#define AT91SAM3_SRAM_ADDR			0x20000000
#define AT91SAM3_FLASH_DEFAULT		0xFF

#define AT91SAM3_JTAG				0
#define AT91SAM3_SWD				1
#define AT91SAM3_ISP				2

extern uint8_t at91sam3_wait_state;

#endif /* __AT91SAM3_INTERNAL_H_INCLUDED__ */

