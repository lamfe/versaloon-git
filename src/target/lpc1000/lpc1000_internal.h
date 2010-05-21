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

#ifndef __LPC1000_INTERNAL_H_INCLUDED__
#define __LPC1000_INTERNAL_H_INCLUDED__

#define LPC1000_FLASH_PAGESIZE		256
#define LPC1000_FLASH_ADDR			0x00000000
#define LPC1000_SRAM_ADDR			0x10000000
#define LPC1000_FLASH_DEFAULT		0xFF

#define LPC1000_JTAG				0
#define LPC1000_SWD					1
#define LPC1000_ISP					2

#define LPC1000_IAP_ENTRY			0x1FFF1FF1

#endif /* __LPC1000_INTERNAL_H_INCLUDED__ */

