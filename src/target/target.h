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

struct program_area_t
{
	uint8_t *buff;
	uint32_t value;
	uint32_t size;
	struct memlist *memlist;
	uint8_t *checksum_buff;
	uint32_t checksum_value;
	uint32_t checksum_size;
	struct memlist *checksum_memlist;
};

struct program_info_t
{
	char *chip_type;
	char *chip_name;
	uint32_t chip_id;
	
	struct program_area_t program_areas[dimof(target_area_name)];
	
	uint8_t *mass_product_data;
	uint32_t mass_product_data_size;
};

struct program_area_map_t
{
	char name;
	uint8_t data_pos;
	uint32_t seg_addr;
	uint32_t start_addr;
	uint32_t fseg_addr;
	uint32_t fstart_addr;
};

struct target_info_t
{
	const char *name;
	struct target_info_t *sub_target;
	const char *feature;
	const struct program_area_map_t *program_area_map;
	const char *program_mode_str;
	RESULT (*parse_argument)(char cmd, const char *argu);
	
	RESULT (*probe_chip)(char *chip_name);
	RESULT (*init)(struct program_info_t *pi, struct programmer_info_t *prog);
	RESULT (*fini)(struct program_info_t *pi, struct programmer_info_t *prog);
	uint32_t (*interface_needed)(void);
	RESULT (*prepare_buffer)(struct program_info_t *pi);
	RESULT (*write_buffer_from_file_callback)(uint32_t address, 
		uint32_t seg_addr, uint8_t* data, uint32_t length, void* buffer);
	RESULT (*write_file_from_buffer_callback)(void);
	RESULT (*program)(struct operation_t operations, 
		struct program_info_t *pi, struct programmer_info_t *prog);
	
	RESULT (*get_mass_product_data_size)(struct operation_t operations, 
						struct program_info_t *pi, uint32_t *size);
	RESULT (*prepare_mass_product_data)(struct operation_t operations, 
						struct program_info_t *pi, uint8_t *buff);
};

#define TARGET_CONF_FILE_EXT			".xml"
#define TARGET_MAX_CHIP_NAME_LEN		32
struct chip_area_info_t
{
	uint32_t seg;
	uint32_t addr;
	uint32_t page_size;
	uint32_t page_num;
	uint32_t default_value;
	uint32_t size;
};

struct chip_param_t
{
	char chip_name[TARGET_MAX_CHIP_NAME_LEN];
	uint32_t chip_id;
	char *program_mode_str;
	uint32_t program_mode;
	
	struct chip_area_info_t chip_areas[dimof(target_area_name)];
	uint32_t param[32];
};

struct chip_series_t
{
	uint32_t num_of_chips;
	struct chip_param_t *chips_param;
};

struct chip_fl_warning_t
{
	uint32_t mask;
	uint32_t value;
	char *msg;
};

struct chip_fl_choice_t
{
	uint32_t value;
	char *text;
};

struct chip_fl_setting_t
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
	struct chip_fl_choice_t *choices;
};

struct chip_fl_t
{
	uint32_t init_value;
	uint16_t num_of_fl_warnings;
	struct chip_fl_warning_t *warnings;
	uint16_t num_of_fl_settings;
	struct chip_fl_setting_t *settings;
};

extern struct target_info_t *cur_target;
extern struct target_info_t targets_info[];
extern struct program_info_t program_info;
extern struct chip_series_t target_chips;
extern struct chip_param_t target_chip_param;
extern uint32_t target_defined;

RESULT target_build_chip_series(const char *chip_name, 
						const char *program_mode, struct chip_series_t *s);
RESULT target_release_chip_series(struct chip_series_t *s);

RESULT target_build_chip_fl(const char *chip_series, const char *chip_module, 
							char *type, struct chip_fl_t *fl);
RESULT target_release_chip_fl(struct chip_fl_t *fl);

uint32_t target_area_mask(char area_name);
char* target_area_fullname(char area_name);
int8_t target_area_idx_by_char(char area_name);
char target_area_char_by_fullname(char *fullname);

void target_print_memory(char type);
void target_print_setting(char type);
void target_print_target(uint32_t index);
void target_print_list(void);
void target_print_help(void);
uint32_t target_get_number(void);
void target_get_target_area(char area, uint8_t **buff, uint32_t *size);
RESULT target_init(struct program_info_t *pi);
RESULT target_alloc_data_buffer(void);
void target_free_data_buffer(void);
RESULT target_check_defined(struct operation_t operations);
RESULT target_prepare_operations(struct operation_t *operations, 
							uint32_t *readfile, uint32_t *writefile);

#endif /* __TARGET_H_INCLUDED__ */

