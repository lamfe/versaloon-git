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
#ifndef __STM8_INTERNAL_H_INCLUDED__
#define __STM8_INTERNAL_H_INCLUDED__

#define STM8_FLASH_PAGESIZE			128
#define STM8_FLASH_ADDR				0x008000
#define STM8_FLASH_DEFAULT			0x00

#define STM8_EE_PAGESIZE			128
#define STM8_EE_ADDR				0x004000
#define STM8_EE_DEFAULT				0x00

#define STM8_SWIM_CMD_BITLEN		3
#define STM8_SWIM_CMD_SRST			0x00
#define STM8_SWIM_CMD_ROTF			0x01
#define STM8_SWIM_CMD_WOTF			0x02

#define STM8_REG_SWIM_CSR			0x007F80
#define STM8_SWIM_CSR_SAFT_MASK		0x80
#define STM8_SWIM_CSR_SWIM_DM		0x20
#define STM8_SWIM_CSR_HS			0x10
#define STM8_SWIM_CSR_RST			0x04

#define STM8_REG_FLASH_DUKR			0x005064
#define STM8_REG_FLASH_PUKR			0x005062

#define STM8_REG_FLASH_IAPSR		0x00505F
#define STM8_FLASH_IAPSR_HVOFF		0x40
#define STM8_FLASH_IAPSR_DUL		0x08
#define STM8_FLASH_IAPSR_PUL		0x02
#define STM8_FLASH_IAPSR_EOP		0x04
#define STM8_FLASH_IAPSR_WRPGDIS	0x01

#define STM8_REG_FLASH_CR2			0x00505B
#define	STM8_REG_FLASH_NCR2			0x00505C

#define	STM8_REG_DM_CSR2			0x007F99

#define STM8_FLASH_CR2_ERASE		0x20
#define STM8_FLASH_CR2_FPRG			0x10
#define STM8_FLASH_CR2_PRG			0x01

#define STM8_SWIM					0
#define STM8_ISP					1

#endif /* __STM8_INTERNAL_H_INCLUDED__ */

