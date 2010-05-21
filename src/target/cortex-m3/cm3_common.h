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
#ifndef __CM3_COMMON_H_INCLUDED__
#define __CM3_COMMON_H_INCLUDED__

#define CM3_CPUID							0xE000ED00

#define CM3_COREREG_R0						0
#define CM3_COREREG_R1						1
#define CM3_COREREG_R12						12
#define CM3_COREREG_SP						13
#define CM3_COREREG_LR						14
#define CM3_COREREG_PC						15

/* Debug Control Block */
#define CM3_DCB_DHCSR						0xE000EDF0
#define CM3_DCB_DCRSR						0xE000EDF4
#define CM3_DCB_DCRDR						0xE000EDF8
#define CM3_DCB_DEMCR						0xE000EDFC

#define CM3_DCB_DCRSR_WnR					(1 << 16)

/* DCB_DHCSR bit and field definitions */
#define CM3_DCB_DHCSR_DBGKEY				(0xA05F << 16)
#define CM3_DCB_DHCSR_C_DEBUGEN				(1 << 0)
#define CM3_DCB_DHCSR_C_HALT				(1 << 1)
#define CM3_DCB_DHCSR_C_STEP				(1 << 2)
#define CM3_DCB_DHCSR_C_MASKINTS			(1 << 3)
#define CM3_DCB_DHCSR_S_REGRDY				(1 << 16)
#define CM3_DCB_DHCSR_S_HALT				(1 << 17)
#define CM3_DCB_DHCSR_S_SLEEP				(1 << 18)
#define CM3_DCB_DHCSR_S_LOCKUP				(1 << 19)
#define CM3_DCB_DHCSR_S_RETIRE_ST			(1 << 24)
#define CM3_DCB_DHCSR_S_RESET_ST			(1 << 25)

#define CM3_REG_NVIC_ICTR					0xE000E004
#define CM3_REG_NVIC_ISE0					0xE000E100
#define CM3_REG_NVIC_ICSR					0xE000ED04
#define CM3_REG_NVIC_AIRCR					0xE000ED0C
#define CM3_REG_NVIC_SHCSR					0xE000ED24
#define CM3_REG_NVIC_CFSR					0xE000ED28
#define CM3_REG_NVIC_MMFSRb					0xE000ED28
#define CM3_REG_NVIC_BFSRb					0xE000ED29
#define CM3_REG_NVIC_USFSRh					0xE000ED2A
#define CM3_REG_NVIC_HFSR					0xE000ED2C
#define CM3_REG_NVIC_DFSR					0xE000ED30
#define CM3_REG_NVIC_MMFAR					0xE000ED34
#define CM3_REG_NVIC_BFAR					0xE000ED38

/* NVIC_AIRCR bits */
#define CM3_REG_NVIC_AIRCR_VECTKEY			(0x5FA << 16)
#define CM3_REG_NVIC_AIRCR_SYSRESETREQ		(1 << 2)
#define CM3_REG_NVIC_AIRCR_VECTCLRACTIVE	(1 << 1)
#define CM3_REG_NVIC_AIRCR_VECTRESET		(1 << 0)

RESULT cm3_dp_parameter_init(adi_dp_if_t *dp);
RESULT cm3_dp_fini(void);
RESULT cm3_dp_init(struct program_context_t *context, adi_dp_if_t *interf);

RESULT cm3_dp_halt(void);
RESULT cm3_dp_run(void);
RESULT cm3_reset(void);

RESULT cm3_read_core_register(uint8_t reg_idx, uint32_t *value);
RESULT cm3_write_core_register(uint8_t reg_idx, uint32_t *value);

uint32_t cm3_get_max_block_size(uint32_t address);

#endif	// __CM3_COMMON_H_INCLUDED__

