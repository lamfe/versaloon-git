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
#ifndef __VSPROG_H_INCLUDED__
#define __VSPROG_H_INCLUDED__

// Target Area
#define CHIPID_IDX					0
#define CHIPID_CHKSUM_IDX			(1 + CHIPID_IDX)
#define BOOTLOADER_IDX				2
#define BOOTLOADER_CHKSUM_IDX		(1 + BOOTLOADER_IDX)
#define APPLICATION_IDX				4
#define APPLICATION_CHKSUM_IDX		(1 + APPLICATION_IDX)
#define	EEPROM_IDX					6
#define EEPROM_CHKSUM_IDX			(1 + EEPROM_IDX)
#define	OTPROM_IDX					8
#define OTPROM_CHKSUM_IDX			(1 + OTPROM_IDX)
#define FUSE_IDX					10
#define FUSE_CHKSUM_IDX				(1 + FUSE_IDX)
#define LOCK_IDX					12
#define LOCK_CHKSUM_IDX				(1 + LOCK_IDX)
#define USRSIG_IDX					14
#define USRSIG_CHKSUM_IDX			(1 + USRSIG_IDX)
#define CALIBRATION_IDX				16
#define CALIBRATION_CHKSUM_IDX		(1 + CALIBRATION_IDX)

#define CHIPID						(1 << CHIPID_IDX)
#define CHIPID_CHKSUM				(1 << CHIPID_CHKSUM_IDX)
#define BOOTLOADER					(1 << BOOTLOADER_IDX)
#define BOOTLOADER_CHKSUM			(1 << BOOTLOADER_CHKSUM_IDX)
#define APPLICATION					(1 << APPLICATION_IDX)
#define APPLICATION_CHKSUM			(1 << APPLICATION_CHKSUM_IDX)
#define EEPROM						(1 << EEPROM_IDX)
#define EEPROM_CHKSUM				(1 << EEPROM_CHKSUM_IDX)
#define OTPROM						(1 << OTPROM_IDX)
#define OTPROM_CHKSUM				(1 << OTPROM_CHKSUM_IDX)
#define FUSE						(1 << FUSE_IDX)
#define FUSE_CHKSUM					(1 << FUSE_CHKSUM_IDX)
#define LOCK						(1 << LOCK_IDX)
#define LOCK_CHKSUM					(1 << LOCK_CHKSUM_IDX)
#define USRSIG						(1 << USRSIG_IDX)
#define USRSIG_CHKSUM				(1 << USRSIG_CHKSUM_IDX)
#define CALIBRATION					(1 << CALIBRATION_IDX)
#define CALIBRATION_CHKSUM			(1 << CALIBRATION_CHKSUM_IDX)
#define ALL							0x80000000UL
#define TARGET_AREA_MASK			(BOOTLOADER | APPLICATION | EEPROM \
									 | OTP_ROM | FUSE | LOCK | USER_SIG \
									 | CHECKSUM | ALL)
#define NUM_OF_TARGET_AREA			18

#define CHIPID_CHAR					'i'
#define CHIPID_CHKSUM_CHAR			'I'
#define BOOTLOADER_CHAR				'b'
#define BOOTLOADER_CHKSUM_CHAR		'B'
#define	APPLICATION_CHAR			'f'
#define APPLICATION_CHKSUM_CHAR		'F'
#define EEPROM_CHAR					'e'
#define EEPROM_CHKSUM_CHAR			'E'
#define OTPROM_CHAR					'o'
#define OTPROM_CHKSUM_CHAR			'O'
#define FUSE_CHAR					'u'
#define FUSE_CHKSUM_CHAR			'U'
#define LOCK_CHAR					'l'
#define LOCK_CHKSUM_CHAR			'L'
#define USRSIG_CHAR					's'
#define USRSIG_CHKSUM_CHAR			'S'
#define CALIBRATION_CHAR			'c'
#define CALIBRATION_CHKSUM_CHAR		'C'

struct target_area_name_t
{
	const char name;
	uint32_t mask;
	const char *full_name;
};

extern const struct target_area_name_t target_area_name[NUM_OF_TARGET_AREA];

// Target Feature
#define AUTO_DETECT					"A"
#define USE_COMM					"C"
#define CAN_EXECUTE					"X"
#define SET_FREQUENCY				"F"

struct operation_t
{
	uint32_t erase_operations;
	uint32_t write_operations;
	uint32_t read_operations;
	uint32_t verify_operations;
	uint32_t checksum_operations;
};

// msic_cmd
struct misc_cmd_t
{
	const char *help_str;
	const char *cmd_name;
	RESULT (*processor)(uint8_t argc, const char *argv[]);
};

extern uint16_t program_frequency;
extern uint8_t gui_mode;
extern uint8_t program_mode;
extern char *config_dir;

#endif /* __VSPROG_H_INCLUDED__ */

