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
#ifndef __CM3_STM32_H_INCLUDED__
#define __CM3_STM32_H_INCLUDED__

#include "adi_v5p1.h"

#define STM32_JTAG_KHZ				570

#define STM32_FLASH_CHAR			0xFF
#define STM32_PAGE_SIZE				128
#define STM32_PAGE_SIZE_RW			4096
#define STM32_FLASH_START_ADDRESS	0x08000000
#define STM32_SRAM_START_ADDRESS	0x20000000

#define STM32_REG_MCU_ID			0xE0042000
#define STM32_REV_MSK				0xFFFF0000
#define STM32_REV_A					0x00000000
#define STM32_REV_B					0x20000000
#define STM32_REV_Z					0x20010000
#define STM32_REV_Y					0x20030000
#define STM32_DEN_MSK				0x00000FFF
#define STM32_DEN_LOW				0x00000412
#define STM32_DEN_MEDIUM			0x00000410
#define STM32_DEN_HIGH				0x00000414
#define STM32_DEN_CONNECTIVITY		0x00000418

#define STM32_REG_FLASH_RAM_SIZE	0x1FFFF7E0

/* stm32 flash register locations */
#define STM32_FLASH_ACR				0x40022000
#define STM32_FLASH_KEYR			0x40022004
#define STM32_FLASH_OPTKEYR			0x40022008
#define STM32_FLASH_SR				0x4002200C
#define STM32_FLASH_CR				0x40022010
#define STM32_FLASH_AR				0x40022014
#define STM32_FLASH_OBR				0x4002201C
#define STM32_FLASH_WRPR			0x40022020

/* option byte location */
#define STM32_OB_RDP				0x1FFFF800
#define STM32_OB_USER				0x1FFFF802
#define STM32_OB_DATA0				0x1FFFF804
#define STM32_OB_DATA1				0x1FFFF806
#define STM32_OB_WRP0				0x1FFFF808
#define STM32_OB_WRP1				0x1FFFF80A
#define STM32_OB_WRP2				0x1FFFF80C
#define STM32_OB_WRP3				0x1FFFF80E

/* FLASH_CR register bits */
#define STM32_FLASH_CR_PG			(1 << 0)
#define STM32_FLASH_CR_PER			(1 << 1)
#define STM32_FLASH_CR_MER			(1 << 2)
#define STM32_FLASH_CR_OPTPG		(1 << 4)
#define STM32_FLASH_CR_OPTER		(1 << 5)
#define STM32_FLASH_CR_STRT			(1 << 6)
#define STM32_FLASH_CR_LOCK			(1 << 7)
#define STM32_FLASH_CR_OPTWRE		(1 << 9)

/* FLASH_SR register bits */
#define STM32_FLASH_SR_BSY			(1 << 0)
#define STM32_FLASH_SR_PGERR		(1 << 2)
#define STM32_FLASH_SR_WRPRTERR		(1 << 4)
#define STM32_FLASH_SR_EOP			(1 << 5)

#define STM32_FLASH_UNLOCK_KEY1		0x45670123
#define STM32_FLASH_UNLOCK_KEY2		0xCDEF89AB

RESULT stm32jtagswj_program(struct operation_t operations, 
					struct program_info_t *pi, struct adi_dp_info_t *dp_info);

#endif /* __CM3_STM32_H_INCLUDED__ */

