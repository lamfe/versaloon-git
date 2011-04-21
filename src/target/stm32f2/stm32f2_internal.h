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

#ifndef __STM32F2_INTERNAL_H_INCLUDED__
#define __STM32F2_INTERNAL_H_INCLUDED__

#define STM32F2_FLASH_ADDR				0x08000000
#define STM32F2_SRAM_ADDR				0x20000000
#define STM32F2_FLASH_DEFAULT			0xFF
#define STM32F2_FLASH_BANK_SIZE			(512 * 1024)
#define STM32F2_FLASH_BANK1_ADDR		STM32F2_FLASH_ADDR
#define STM32F2_FLASH_BANK2_ADDR		(STM32F2_FLASH_ADDR + STM32F2_FLASH_BANK_SIZE)

#define STM32F2_JTAG					0
#define STM32F2_SWD						1
#define STM32F2_ISP						2

#define STM32F2_REV_MSK					0xFFFF0000
#define STM32F2_DEN_MSK					0x00000FFF
#define STM32F2_DEN_XL					0x0411

#define STM32F2_REG_MCU_ID				0xE0042000

#define STM32F2_OB_ADDR					0x1FFFF800
#define STM32F2_OB_SIZE					16

#define STM32F2_UID_ADDR				(0x1FFF7A10)

void stm32f2_print_device(uint32_t mcuid);
uint16_t stm32f2_get_flash_size(uint32_t mcuid, uint32_t flash_sram_reg);

#endif /* __STM32F2_INTERNAL_H_INCLUDED__ */

