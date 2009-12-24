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

#ifndef __STM32_INTERNAL_H_INCLUDED__
#define __STM32_INTERNAL_H_INCLUDED__

#define STM32_PAGE_SIZE				128
#define STM32_FLASH_ADDR			0x08000000
#define STM32_FLASH_DEFAULT			0xFF

#define STM32_JTAG					(1 << 0)
#define STM32_SWJ					(1 << 1)
#define STM32_ISP					(1 << 2)

#endif /* __STM32_INTERNAL_H_INCLUDED__ */

