/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
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
#ifndef __CM3_STM32F1_H_INCLUDED__
#define __CM3_STM32F1_H_INCLUDED__

#define STM32F1_PAGE_SIZE_RW		5120

#define STM32F1_IRC_KHZ				8000

/* stm32f1 flash register locations */
#define STM32F1_FLASH_ACR			0x40022000
#define STM32F1_FLASH_KEYR			0x40022004
#define STM32F1_FLASH_OPTKEYR		0x40022008
#define STM32F1_FLASH_SR			0x4002200C
#define STM32F1_FLASH_CR			0x40022010
#define STM32F1_FLASH_AR			0x40022014
#define STM32F1_FLASH_OBR			0x4002201C
#define STM32F1_FLASH_WRPR			0x40022020
#define STM32F1_FLASH_KEYR2			0x40022044
#define STM32F1_FLASH_SR2			0x4002204C
#define STM32F1_FLASH_CR2			0x40022050
#define STM32F1_FLASH_AR2			0x40022054

#define STM32F1_FLASH_OBR_RDPRT		(1 << 1)

/* FLASH_CR register bits */
#define STM32F1_FLASH_CR_PG			(1 << 0)
#define STM32F1_FLASH_CR_PER		(1 << 1)
#define STM32F1_FLASH_CR_MER		(1 << 2)
#define STM32F1_FLASH_CR_OPTPG		(1 << 4)
#define STM32F1_FLASH_CR_OPTER		(1 << 5)
#define STM32F1_FLASH_CR_STRT		(1 << 6)
#define STM32F1_FLASH_CR_LOCK		(1 << 7)
#define STM32F1_FLASH_CR_OPTWRE		(1 << 9)

/* FLASH_SR register bits */
#define STM32F1_FLASH_SR_BSY		(1 << 0)
#define STM32F1_FLASH_SR_PGERR		(1 << 2)
#define STM32F1_FLASH_SR_WRPRTERR	(1 << 4)
#define STM32F1_FLASH_SR_EOP		(1 << 5)
#define STM32F1_FLASH_SR_ERRMSK		\
	(STM32F1_FLASH_SR_PGERR | STM32F1_FLASH_SR_WRPRTERR)

#define STM32F1_FLASH_UNLOCK_KEY1	0x45670123
#define STM32F1_FLASH_UNLOCK_KEY2	0xCDEF89AB
#define STM32F1_OPT_UNLOCK_KEY1		0x45670123
#define STM32F1_OPT_UNLOCK_KEY2		0xCDEF89AB

extern const struct program_functions_t stm32f1swj_program_functions;

#endif /* __CM3_STM32F1_H_INCLUDED__ */

