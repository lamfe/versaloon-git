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
#ifndef __MSP430_INTERNAL_H_INCLUDED__
#define __MSP430_INTERNAL_H_INCLUDED__

#define	MSP430_FLASH_CHAR				0xFF

// program mode
#define MSP430_MODE_JTAG				(1 << 0)
#define MSP430_MODE_SBW					(1 << 1)
#define MSP430_MODE_BSL					(1 << 2)
#define MSP430_PROG_MODE_MASK			(MSP430_MODE_JTAG | MSP430_MODE_SBW \
										 | MSP430_MODE_BSL)

#define MSP430_CUR_PROG_MODE			(msp430_program_mode \
										 & MSP430_PROG_MODE_MASK)

#define MSP430_PARAM_MAINSTART			0
#define MSP430_PARAM_RAMSTART			1
#define MSP430_PARAM_RAMEND				2
#define MSP430_PARAM_TEST				3
#define MSP430_PARAM_CPUX				4
#define MSP430_PARAM_DATAQUICK			5
#define MSP430_PARAM_FASTFLASH			6
#define MSP430_PARAM_ENHVERIFY			7

extern RESULT (*msp430jtagsbw_init)(void);
extern RESULT (*msp430jtagsbw_fini)(void);
extern RESULT (*msp430jtagsbw_config)(uint8_t has_test);
extern RESULT (*msp430jtagsbw_ir)(uint8_t *ir, uint8_t want_ret);
extern RESULT (*msp430jtagsbw_dr)(uint32_t *dr, uint8_t len, uint8_t want_ret);
extern RESULT (*msp430jtagsbw_tclk)(uint8_t value);
extern RESULT (*msp430jtagsbw_tclk_strobe)(uint16_t cnt);
extern RESULT (*msp430jtagsbw_reset)(void);
extern RESULT (*msp430jtagsbw_poll)(uint32_t dr, uint32_t mask, uint32_t value, 
									uint8_t len, uint16_t poll_cnt, 
									uint8_t toggle_tclk);

RESULT msp430_sbw_program(operation_t operations, program_info_t *pi, 
						  programmer_info_t *prog);
RESULT msp430_jtag_program(operation_t operations, program_info_t *pi, 
						   programmer_info_t *prog);
RESULT msp430_bsl_program(operation_t operations, program_info_t *pi, 
						  programmer_info_t *prog);

// JTAG and SBW
#define MSP430_IR_LEN			8

#define F_BYTE					8
#define F_WORD					16
#define V_RESET					0xFFFE

// Constants for flash erasing modes
#define ERASE_GLOB				0xA50E // main & info of ALL      mem arrays
#define ERASE_ALLMAIN			0xA50C // main        of ALL      mem arrays
#define ERASE_MASS				0xA506 // main & info of SELECTED mem arrays
#define ERASE_MAIN				0xA504 // main        of SELECTED mem arrays
#define ERASE_SGMT				0xA502 // SELECTED segment

#define DeviceHas_TestPin()		cur_chip_param.param[MSP430_PARAM_TEST]
#define DeviceHas_CpuX()		cur_chip_param.param[MSP430_PARAM_CPUX]
#define DeviceHas_DataQuick()	cur_chip_param.param[MSP430_PARAM_DATAQUICK]
#define DeviceHas_FastFlash()	cur_chip_param.param[MSP430_PARAM_FASTFLASH]
#define DeviceHas_EnhVerify()	cur_chip_param.param[MSP430_PARAM_ENHVERIFY]
#define DeviceHas_JTAG()		\
							(cur_chip_param.program_mode & MSP430_MODE_JTAG)
#define DeviceHas_SpyBiWire()	\
							(cur_chip_param.program_mode & MSP430_MODE_SPW)
#define Device_RamStart()		\
						(word)(cur_chip_param.param[MSP430_PARAM_RAMSTART])
#define Device_RamEnd()			\
						(word)(cur_chip_param.param[MSP430_PARAM_RAMEND])
#define Device_MainStart()		\
						(word)(cur_chip_param.param[MSP430_PARAM_MAINSTART])

#endif /* __MSP430_INTERNAL_H_INCLUDED__ */

