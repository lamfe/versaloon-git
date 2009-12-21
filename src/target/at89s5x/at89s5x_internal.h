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
#ifndef __AT89S5X_INTERNAL_H_INCLUDED__
#define __AT89S5X_INTERNAL_H_INCLUDED__

#define S5X_PARAM_PE_OUT			0
#define S5X_DEFAULT_FREQ			560

#define S5X_INTERFACE_NEEDED		(SPI | GPIO)

#define S5X_MAX_FLASH_SIZE			(8 * 1024)
#define S5X_MAX_PAGE_SIZE			256
#define S5X_MAX_PAGE_NUM		

#define S5X_FLASH_CHAR				0xFF

// flash mode
#define S5X_PAGE_MODE				(1 << 0)
#define S5X_BYTE_MODE				(1 << 1)
#define S5X_MODE_MASK				(S5X_PAGE_MODE | S5X_BYTE_MODE)

#endif /* __AT89S5X_INTERNAL_H_INCLUDED__ */

