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

typedef struct
{
	char *chip_type;
	char *chip_name;
	uint32_t chip_id;
	
	uint8_t *boot;
	uint32_t boot_size;
	uint32_t boot_size_valid;
	uint8_t *boot_checksum;
	uint32_t boot_checksum_value;
	uint32_t boot_checksum_size;
	memlist *boot_memlist;
	
	uint8_t *app;
	uint32_t app_size;
	uint32_t app_size_valid;
	memlist *app_memlist;
	uint8_t *app_checksum;
	uint32_t app_checksum_value;
	uint32_t app_checksum_size;
	
	uint8_t *eeprom;
	uint32_t eeprom_size;
	uint32_t eeprom_size_valid;
	memlist *eeprom_memlist;
	uint8_t *eeprom_checksum;
	uint32_t eeprom_checksum_value;
	uint32_t eeprom_checksum_size;
	
	uint8_t *otp_rom;
	uint32_t otp_rom_size;
	uint32_t otp_rom_size_valid;
	memlist *opt_rom_memlist;
	uint8_t *otp_rom_checksum;
	uint32_t otp_rom_checksum_value;
	uint32_t otp_rom_checksum_size;
	
	uint8_t *fuse;
	uint32_t fuse_value;
	uint32_t fuse_size;
	uint32_t fuse_size_valid;
	memlist *fuse_memlist;
	uint8_t *fuse_checksum;
	uint32_t fuse_checksum_value;
	uint32_t fuse_checksum_size;
	
	uint8_t *lock;
	uint32_t lock_value;
	uint32_t lock_size;
	uint32_t lock_size_valid;
	memlist *lock_memlist;
	uint8_t *lock_checksum;
	uint32_t lock_checksum_value;
	uint32_t lock_checksum_size;
	
	uint8_t *user_area;
	uint32_t user_area_value;
	uint32_t user_area_size;
	uint32_t user_area_size_valid;
	memlist *user_area_memlist;
	uint8_t *user_area_checksum;
	uint32_t user_area_checksum_value;
	uint32_t user_area_checksum_size;
	
	uint8_t *mass_product_data;
	uint32_t mass_product_data_size;
}program_info_t;

typedef struct
{
	uint32_t area_mask;
	char area_char;
	uint8_t data_pos;
}program_area_map_t;

typedef struct
{
	const char *name;
	const uint32_t areas;
	const program_area_map_t *program_area_map;
	const char *program_mode_str;
	RESULT (*parse_argument)(char cmd, const char *argu);
	
	RESULT (*probe_chip)(char *chip_name);
	RESULT (*init)(program_info_t *pi, const char *dir, 
				   programmer_info_t *prog);
	RESULT (*fini)(program_info_t *pi, programmer_info_t *prog);
	uint32_t (*interface_needed)(void);
	RESULT (*prepare_buffer)(program_info_t *pi);
	RESULT (*write_buffer_from_file_callback)(
									uint32_t address, uint32_t seg_addr, 
									uint8_t* data, uint32_t length, void* buffer);
	RESULT (*write_file_from_buffer_callback)(void);
	RESULT (*program)(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog);
	
	RESULT (*get_mass_product_data_size)(operation_t operations, 
										 program_info_t *pi, uint32_t *size);
	RESULT (*prepare_mass_product_data)(operation_t operations, 
										program_info_t *pi, uint8_t *buff);
}target_info_t;

#define TARGET_CONF_FILE_PATH			"config/"
#define TARGET_CONF_FILE_EXT			".xml"
#define TARGET_MAX_CHIP_NAME_LEN		32
typedef struct
{
	char chip_name[TARGET_MAX_CHIP_NAME_LEN];
	uint32_t chip_id;
	char *program_mode_str;
	uint32_t program_mode;
	uint32_t boot_page_size;
	uint32_t boot_page_num;
	uint32_t app_page_size;
	uint32_t app_page_num;
	uint32_t ee_page_size;
	uint32_t ee_page_num;
	uint32_t optrom_page_size;
	uint32_t optrom_page_num;
	uint32_t usrsig_page_size;
	uint32_t usrsig_page_num;
	uint32_t lock_page_size;
	uint32_t lock_page_num;
	uint32_t fuse_page_size;
	uint32_t fuse_page_num;
	uint32_t fuse_size;
	uint32_t fuse_default_value;
	uint32_t lock_size;
	uint32_t lock_default_value;
	uint32_t cali_size;
	uint32_t cali_default_value;
	uint32_t boot_size;
	uint32_t app_size;
	uint32_t ee_size;
	uint32_t optrom_size;
	uint32_t usrsig_size;
	uint32_t param[32];
}chip_param_t;

typedef struct
{
	uint32_t num_of_chips;
	chip_param_t *chips_param;
}chip_series_t;

typedef struct
{
	uint32_t mask;
	uint32_t value;
	char *msg;
}chip_fl_warning_t;

typedef struct
{
	uint32_t value;
	char *text;
}chip_fl_choice_t;

typedef struct
{
	char *name;
	char *info;
	uint32_t mask;
	char *ban;
	uint8_t use_checkbox;
	uint8_t use_edit;
	uint8_t shift;
	uint8_t radix;
	uint32_t checked;
	uint32_t unchecked;
	uint8_t bytelen;
	uint16_t num_of_choices;
	chip_fl_choice_t *choices;
}chip_fl_setting_t;

typedef struct
{
	uint32_t init_value;
	uint16_t num_of_fl_warnings;
	chip_fl_warning_t *warnings;
	uint16_t num_of_fl_settings;
	chip_fl_setting_t *settings;
}chip_fl_t;

extern target_info_t *cur_target;
extern target_info_t targets_info[];
extern program_info_t program_info;
extern chip_series_t target_chips;
extern chip_param_t target_chip_param;
extern uint32_t target_defined;

RESULT target_build_chip_series(const char *chip_name, 
								const char *program_mode, 
								chip_series_t *s);
RESULT target_release_chip_series(chip_series_t *s);

RESULT target_build_chip_fl(const char *chip_series, const char *chip_module, 
							char *type, chip_fl_t *fl);
RESULT target_release_chip_fl(chip_fl_t *fl);

void target_print_fl(char *type);
void target_print_target(uint32_t i);
void target_print_list(void);
void target_print_help(void);
uint32_t target_get_number(void);
RESULT target_init(program_info_t *pi);
RESULT target_alloc_data_buffer(void);
void target_free_data_buffer(void);
RESULT target_check_defined(operation_t operations);

#endif /* __TARGET_H_INCLUDED__ */

