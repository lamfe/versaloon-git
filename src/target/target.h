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
	uint32 chip_id;
	
	uint8 *boot;
	uint32 boot_size;
	uint32 boot_size_valid;
	uint8 *boot_checksum;
	uint32 boot_checksum_value;
	uint32 boot_checksum_size;
	memlist *boot_memlist;
	
	uint8 *app;
	uint32 app_size;
	uint32 app_size_valid;
	memlist *app_memlist;
	uint8 *app_checksum;
	uint32 app_checksum_value;
	uint32 app_checksum_size;
	
	uint8 *eeprom;
	uint32 eeprom_size;
	uint32 eeprom_size_valid;
	memlist *eeprom_memlist;
	uint8 *eeprom_checksum;
	uint32 eeprom_checksum_value;
	uint32 eeprom_checksum_size;
	
	uint8 *otp_rom;
	uint32 otp_rom_size;
	uint32 otp_rom_size_valid;
	memlist *opt_rom_memlist;
	uint8 *otp_rom_checksum;
	uint32 otp_rom_checksum_value;
	uint32 otp_rom_checksum_size;
	
	uint8 *fuse;
	uint32 fuse_value;
	uint32 fuse_size;
	uint32 fuse_size_valid;
	memlist *fuse_memlist;
	uint8 *fuse_checksum;
	uint32 fuse_checksum_value;
	uint32 fuse_checksum_size;
	
	uint8 *lock;
	uint32 lock_value;
	uint32 lock_size;
	uint32 lock_size_valid;
	memlist *lock_memlist;
	uint8 *lock_checksum;
	uint32 lock_checksum_value;
	uint32 lock_checksum_size;
	
	uint8 *user_area;
	uint32 user_area_value;
	uint32 user_area_size;
	uint32 user_area_size_valid;
	memlist *user_area_memlist;
	uint8 *user_area_checksum;
	uint32 user_area_checksum_value;
	uint32 user_area_checksum_size;
	
	uint8 *mass_product_data;
	uint32 mass_product_data_size;
}program_info_t;

typedef struct
{
	uint32 area_mask;
	char area_char;
	uint8 data_pos;
}program_area_map_t;

typedef struct
{
	const char *name;
	const uint32 areas;
	const program_area_map_t *program_area_map;
	RESULT (*parse_argument)(char cmd, const char *argu);
	
	RESULT (*probe_chip)(char *chip_name);
	RESULT (*init)(program_info_t *pi, const char *dir, 
				   programmer_info_t *prog);
	RESULT (*fini)(program_info_t *pi, programmer_info_t *prog);
	uint32 (*interface_needed)(void);
	RESULT (*prepare_buffer)(program_info_t *pi);
	RESULT (*write_buffer_from_file_callback)(
									uint32 address, uint32 seg_addr, 
									uint8* data, uint32 length, void* buffer);
	RESULT (*write_file_from_buffer_callback)(void);
	RESULT (*program)(operation_t operations, program_info_t *pi, 
					  programmer_info_t *prog);
	
	RESULT (*get_mass_product_data_size)(operation_t operations, 
										 program_info_t *pi, uint32 *size);
	RESULT (*prepare_mass_product_data)(operation_t operations, 
										program_info_t *pi, uint8 *buff);
}target_info_t;

#define TARGET_MAX_CHIP_NAME_LEN		32
typedef struct
{
	char chip_name[TARGET_MAX_CHIP_NAME_LEN];
	uint32 chip_id;
	uint32 program_mode;
	uint32 boot_page_size;
	uint32 boot_page_num;
	uint32 app_page_size;
	uint32 app_page_num;
	uint32 ee_page_size;
	uint32 ee_page_num;
	uint32 optrom_page_size;
	uint32 optrom_page_num;
	uint32 usrsig_page_size;
	uint32 usrsig_page_num;
	uint32 fuse_size;
	uint32 lock_size;
	uint32 boot_size;
	uint32 app_size;
	uint32 ee_size;
	uint32 optrom_size;
	uint32 usrsig_size;
	uint32 para[32];
}chip_param_t;

typedef struct
{
	uint32 num_of_chips;
	chip_param_t *params;
}chip_series_t;

extern target_info_t *cur_target;
extern target_info_t targets_info[];
extern program_info_t program_info;

void target_print_list(void);
void target_print_help(void);
uint32 target_get_number(void);
RESULT target_init(program_info_t *pi);
RESULT target_alloc_data_buffer(void);
void target_free_data_buffer(void);

#endif /* __TARGET_H_INCLUDED__ */

