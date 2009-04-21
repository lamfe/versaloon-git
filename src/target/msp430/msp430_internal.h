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
#define MSP430_MODE_JTAG				0x01
#define MSP430_MODE_SBW					0x02
#define MSP430_MODE_BSL					0x04
#define MSP430_PROG_MODE_MASK			(MSP430_MODE_JTAG | MSP430_MODE_SBW \
										 | MSP430_MODE_BSL)
#define MSP430_MODE_TEST				0x0100
#define MSP430_MODE_CPUX				0x0200
#define MSP430_MODE_DATAQUICK			0x0400
#define MSP430_MODE_FASTFLASH			0x0800
#define MSP430_MODE_ENHVERIFY			0x1000

#define MSP430_CUR_PROG_MODE			(msp430_program_mode \
										 & MSP430_PROG_MODE_MASK)

typedef struct
{
	const char *chip_name;
	uint16 chip_id;
	uint16 program_mode;
	uint16 flash_page_size;
	uint16 flash_page_num;
	uint16 info_page_size;
	uint16 info_page_num;
	uint16 ram_start;
	uint16 ram_end;
	uint16 main_start;
	uint16 vector_start;
	uint16 flash_size;
}msp430_param_t;

extern msp430_param_t msp430_chip_param;

extern RESULT (*msp430jtagsbw_init)(void);
extern RESULT (*msp430jtagsbw_fini)(void);
extern RESULT (*msp430jtagsbw_config)(uint8 has_test);
extern RESULT (*msp430jtagsbw_ir)(uint8 *ir, uint8 want_ret);
extern RESULT (*msp430jtagsbw_dr)(uint32 *dr, uint8 len, uint8 want_ret);
extern RESULT (*msp430jtagsbw_tclk)(uint8 value);
extern RESULT (*msp430jtagsbw_tclk_strobe)(uint16 cnt);
extern RESULT (*msp430jtagsbw_reset)(void);
extern RESULT (*msp430jtagsbw_poll)(uint32 dr, uint32 mask, uint32 value, 
									uint8 len, uint16 poll_cnt, 
									uint8 toggle_tclk);

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

#define DeviceHas_TestPin()		\
					(msp430_chip_param.program_mode & MSP430_MODE_TEST)
#define DeviceHas_CpuX()		\
					(msp430_chip_param.program_mode & MSP430_MODE_CPUX)
#define DeviceHas_DataQuick()	\
					(msp430_chip_param.program_mode & MSP430_MODE_DATAQUICK)
#define DeviceHas_FastFlash()	\
					(msp430_chip_param.program_mode & MSP430_MODE_FASTFLASH)
#define DeviceHas_EnhVerify()	\
					(msp430_chip_param.program_mode & MSP430_MODE_ENHVERIFY)
#define DeviceHas_JTAG()		\
					(msp430_chip_param.program_mode & MSP430_MODE_JTAG)
#define DeviceHas_SpyBiWire()	\
					(msp430_chip_param.program_mode & MSP430_MODE_SPW)
#define Device_RamStart()		(msp430_chip_param.ram_start)
#define Device_RamEnd()			(msp430_chip_param.ram_end)
#define Device_MainStart()		(msp430_chip_param.main_start)

#endif /* __MSP430_INTERNAL_H_INCLUDED__ */

