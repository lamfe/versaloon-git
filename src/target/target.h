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
#ifndef __TARGET_H_INCLUDED__
#define __TARGET_H_INCLUDED__

// Target Feature
#define AUTO_DETECT					"A"
#define USE_COMM					"C"
#define CAN_EXECUTE					"X"
#define SET_FREQUENCY				"F"
#define NO_TARGET					"N"

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

enum area_attr_t
{
	AREA_ATTR_E		= (1 << 0), // Erasable
	AREA_ATTR_W		= (1 << 1), // Writable
	AREA_ATTR_R		= (1 << 2), // Readable
	AREA_ATTR_EP	= (1 << 3), // Erase in PageMode
	AREA_ATTR_WNP	= (1 << 4), // Non-Paged Mode when write
	AREA_ATTR_RNP	= (1 << 5), // Non-Paged Mode when read
	AREA_ATTR_V		= (1 << 6), // Simple verify
	AREA_ATTR_WR	= AREA_ATTR_R | AREA_ATTR_W, 
	AREA_ATTR_ER	= AREA_ATTR_R | AREA_ATTR_E, 
	AREA_ATTR_EW	= AREA_ATTR_E | AREA_ATTR_W, 
	AREA_ATTR_EWR	= AREA_ATTR_R | AREA_ATTR_W | AREA_ATTR_E, 
	AREA_ATTR_NP	= AREA_ATTR_WNP | AREA_ATTR_RNP
};

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

struct program_area_t
{
	uint8_t *buff;
	uint64_t value;
	uint32_t size;
	struct memlist *memlist;
	uint8_t *checksum_buff;
	uint64_t checksum_value;
	uint32_t checksum_size;
	struct memlist *checksum_memlist;
};

struct program_info_t
{
	char *chip_type;
	char *chip_name;
	uint32_t chip_id;
	
	struct program_area_t program_areas[dimof(target_area_name)];
	uint32_t areas_defined;
	
	uint8_t mode;
	uint16_t frequency;
	struct jtag_pos_t jtag_pos;
	
	uint8_t *mass_product_data;
	uint32_t mass_product_data_size;
};

struct program_area_map_t
{
	char name;
	uint8_t data_pos;
	int32_t fseg_addr;
	int32_t fstart_addr;
	uint16_t fpage_size;
	enum area_attr_t attr;
};

struct program_mode_t
{
	char name;
	const char *feature;
	uint32_t interface_needed;
};

#define TARGET_CONF_FILE_EXT			".xml"
#define TARGET_MAX_CHIP_NAME_LEN		32

struct chip_area_info_t
{
	uint32_t seg;
	uint32_t addr;
	uint32_t page_size;
	uint32_t page_num;
	uint64_t default_value;
	uint32_t size;
};

struct chip_param_t
{
	char chip_name[TARGET_MAX_CHIP_NAME_LEN];
	uint32_t chip_id;
	char *program_mode_str;
	uint32_t program_mode;
	
	struct chip_area_info_t chip_areas[dimof(target_area_name)];
	uint8_t chip_erase;
	uint32_t param[32];
};

struct program_context_t
{
	struct operation_t *op;
	struct program_info_t *pi;
	struct chip_param_t *param;
	struct programmer_info_t *prog;
};

struct program_functions_t
{
	RESULT (*execute)(struct program_context_t *context);
	RESULT (*enter_program_mode)(struct program_context_t *context);
	RESULT (*leave_program_mode)(struct program_context_t *context, 
									uint8_t success);
	// erase one page at addr or erase full target
	RESULT (*erase_target)(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t size);
	// write one page at addr
	RESULT (*write_target)(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t size);
	// read one page at addr
	RESULT (*read_target)(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t size);
};

struct target_info_t
{
	const char *name;
	const char *feature;
	const struct program_area_map_t *program_area_map;
	const struct program_mode_t *program_mode;
	const struct program_functions_t *program_functions;
	RESULT (*parse_argument)(char cmd, const char *argu);
	
	RESULT (*get_mass_product_data_size)(struct operation_t operations, 
						struct program_info_t *pi, uint32_t *size);
	RESULT (*prepare_mass_product_data)(struct operation_t operations, 
						struct program_info_t *pi, uint8_t *buff);
};

struct chip_series_t
{
	uint32_t num_of_chips;
	struct chip_param_t *chips_param;
};

struct chip_fl_warning_t
{
	uint64_t mask;
	uint64_t value;
	char *msg;
};

struct chip_fl_choice_t
{
	uint64_t value;
	char *text;
};

struct chip_fl_setting_t
{
	char *name;
	char *info;
	uint64_t mask;
	char *ban;
	uint8_t use_checkbox;
	uint8_t use_edit;
	uint8_t shift;
	uint8_t radix;
	uint64_t checked;
	uint64_t unchecked;
	uint8_t bytelen;
	uint16_t num_of_choices;
	struct chip_fl_choice_t *choices;
};

struct chip_fl_t
{
	uint64_t init_value;
	uint16_t num_of_fl_warnings;
	struct chip_fl_warning_t *warnings;
	uint16_t num_of_fl_settings;
	struct chip_fl_setting_t *settings;
};

extern struct target_info_t *cur_target;
extern struct target_info_t targets_info[];
extern struct program_info_t program_info;
extern struct chip_param_t target_chip_param;

RESULT target_build_chip_series(const char *chip_name, 
		const struct program_mode_t *program_mode, struct chip_series_t *s);
RESULT target_release_chip_series(struct chip_series_t *s);

RESULT target_build_chip_fl(const char *chip_series, const char *chip_module, 
							char *type, struct chip_fl_t *fl);
RESULT target_release_chip_fl(struct chip_fl_t *fl);

uint32_t target_area_mask(char area_name);
char* target_area_fullname(char area_name);
int8_t target_area_idx(char area_name);
char target_area_char_by_fullname(char *fullname);
char* target_area_fullname_by_mask(uint32_t mask);

int8_t target_mode_get_idx(const struct program_mode_t *mode, char mode_name);

void target_print_memory(char type);
void target_print_setting(char type);
void target_print_target(uint32_t index);
void target_print_list(void);
void target_print_help(void);
uint32_t target_get_number(void);
void target_get_target_area(char area, uint8_t **buff, uint32_t *size);
RESULT target_program(struct program_context_t *context);
RESULT target_init(struct program_info_t *pi, struct programmer_info_t *prog);
RESULT target_info_init(struct program_info_t *pi);
RESULT target_write_buffer_from_file_callback(uint32_t address, 
			uint32_t seg_addr, uint8_t* data, uint32_t length, void* buffer);
RESULT target_alloc_data_buffer(void);
void target_free_data_buffer(void);
RESULT target_check_defined(struct operation_t operations);
RESULT target_prepare_operations(struct operation_t *operations, 
							uint32_t *readfile, uint32_t *writefile);

#endif /* __TARGET_H_INCLUDED__ */

