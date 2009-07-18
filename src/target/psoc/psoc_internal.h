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
#ifndef __PSOC_INTERNAL_H_INCLUDED__
#define __PSOC_INTERNAL_H_INCLUDED__

#define PSOC_INTERFACE_NEEDED			(ISSP)

#define PSOC_MAX_FLASH_SIZE				(32 * 1024)
#define PSOC_MAX_SECURE_SIZE			(4 * 32)
#define PSOC_MIN_SECURE_SIZE			64
#define PSOC_MAX_CHECKSUM_SIZE			(4 * 2)
#define PSOC_MAX_BANK_NUM				4
#define PSOC_MIN_BLOCK_NUM_IN_BANK		32
#define PSOC_MAX_BLOCK_SIZE				64

#define PSOC_FLASH_CHAR					0x00
#define PSOC_SECURE_CHAR				0x00
#define PSOC_CHECKSUM_CHAR				0x00
#define PSOC_ID_MASK					0xFFFF

#define PSOC_PARAM_BANK_NUM				0

// parameters
#define PSOC_RESET_MODE					(1 << 0)
#define PSOC_POWERON_MODE				(1 << 1)
#define PSOC_MODE_MASK					(PSOC_RESET_MODE | PSOC_POWERON_MODE)

#endif /* __PSOC_INTERNAL_H_INCLUDED__ */

