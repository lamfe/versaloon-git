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
#define VSPROG_VERSION "VSProg " VERSION " (" PKGBLDDATE ") "RELSTR PKGBLDREV

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <unistd.h>

#include <getopt.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_log.h"
#include "app_err.h"
#include "prog_interface.h"

#include "memlist.h"
#include "filelist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "hex.h"

#define OPTSTR			"hvS:P:s:c:Mp:U:Dd:Go:l:f:F:m:x:C:I:O:J:Zb:"
static const struct option long_opts[] =
{
	{"help", no_argument, NULL, 'h'},
	{"version", no_argument, NULL, 'v'},
	{"support", required_argument, NULL, 'S'},
	{"parameter", required_argument, NULL, 'P'},
	{"target-series", required_argument, NULL, 's'},
	{"target-module", required_argument, NULL, 'c'},
	{"mass-product", no_argument, NULL, 'M'},
	{"programmer", required_argument, NULL, 'p'},
	{"usb", required_argument, NULL, 'U'},
	{"display-programmer", no_argument, NULL, 'D'},
	{"debug", required_argument, NULL, 'd'},
	{"gui-mode", no_argument, NULL, 'G'},
	{"operation", required_argument, NULL, 'o'},
	{"lock", required_argument, NULL, 'l'},
	{"fuse", required_argument, NULL, 'f'},
	{"frequency", required_argument, NULL, 'F'},
	{"mode", required_argument, NULL, 'm'},
	{"execute", required_argument, NULL, 'x'},
	{"comport", required_argument, NULL, 'C'},
	{"input-file", required_argument, NULL, 'I'},
	{"output-file", required_argument, NULL, 'O'},
	{"jtag-dc", required_argument, NULL, 'J'},
	{"firmware_update", no_argument, NULL, 'Z'},
	{"buffsize", required_argument, NULL, 'b'},
	{NULL, 0, NULL, 0},
};

int verbosity = LOG_DEFAULT_LEVEL;
operation_t operations;

static char *program_name = NULL;
char *program_dir = NULL;

uint8_t program_mode = 0;

filelist *fl_in = NULL, *fl_out = NULL;

// for JTAT
jtag_pos_t target_jtag_pos;

static void free_all(void)
{
	FILELIST_Free(&fl_in);
	FILELIST_Free(&fl_out);
	
	if (program_name != NULL)
	{
		free(program_name);
		program_name = NULL;
	}
	if (program_dir != NULL)
	{
		free(program_dir);
		program_name = NULL;
	}
	
	if ((cur_target != NULL) && (cur_programmer != NULL) 
		&& (cur_target->fini != NULL))
	{
		cur_target->fini(&program_info, cur_programmer);
		memset(cur_target, 0, sizeof(cur_target));
	}
	
	if ((cur_programmer != NULL) && (cur_programmer->fini != NULL))
	{
		cur_programmer->fini();
		memset(cur_programmer, 0, sizeof(cur_programmer));
	}
	
	// free program buffer
	target_free_data_buffer();
}

static void free_all_and_exit(int exit_code)
{
	free_all();
	exit(exit_code);
}

static RESULT parse_operation(uint32_t *operation, uint32_t *require_input_num, 
							  const char *opt, uint32_t optlen)
{
	uint32_t mask = 0;
	uint32_t i, j;
	
#if PARAM_CHECK
	if ((NULL == operation) || (NULL == opt))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (require_input_num != NULL)
	{
		*require_input_num = 0;
	}
	
	for (i = 0; i < optlen; i++)
	{
		j = 0;
		if (cur_target != NULL)
		{
			while (cur_target->program_area_map[j].area_mask > 0)
			{
				if (cur_target->program_area_map[j].area_char == opt[i])
				{
					mask |= cur_target->program_area_map[j].area_mask;
					if ((require_input_num != NULL) 
						&& (cur_target->program_area_map[j].data_pos))
					{
						(*require_input_num)++;
					}
					break;
				}
				j++;
			}
			if (cur_target->program_area_map[j].area_mask > 0)
			{
				continue;
			}
			else
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHARACTER), 
						  opt[i], "current target");
				return ERRCODE_INVALID;
			}
		}
		else
		{
			switch (opt[i])
			{
			case 'b':
				mask |= BOOTLOADER;
				break;
			case 'a':
				mask |= APPLICATION;
				break;
			case 'e':
				mask |= EEPROM;
				break;
			case 'o':
				mask |= OTP_ROM;
				break;
			case 'f':
				mask |= FUSE;
				break;
			case 'l':
				mask |= LOCK;
				break;
			case 'u':
				mask |= USER_SIG;
				break;
			default:
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHARACTER), 
						  opt[i], "current target");
				return ERRCODE_INVALID;
				break;
			}
		}
	}
	
	*operation = mask;
	return ERROR_OK;
}

static void print_title(void)
{
	printf(_GETTEXT(VSPROG_VERSION "\n\
CopyRight(c) %s-%s by SimonQian <SimonQian@SimonQian.com>\n\n\
URL: http://www.SimonQian.com/en/Versaloon\n\
mail: SimonQian@SimonQian.com\n\n"), "2008", "2009");
}

static void print_help(void)
{
	printf(_GETTEXT("\
Usage: %s [OPTION]...\n\
  -h,  --help                       display this help\n\
  -v,  --version                    display vsprog version\n\
  -S,  --support <target>           display support information\n\
  -d,  --debug <LEVEL>              set debug level <0-2>\n\
  -s,  --target-series <SERIES>     set target series\n\
  -c,  --target-module <MODULE>     set target module\n\
  -p,  --programmer <PROGRAMMER>    set programmer\n\
  -o,  --operation <OPERATIONS>     set programming operation\n\
  -i,  --input-file <HEXFILE>       set input hex file\n\
  -M,  --mass-product               set mass_product mode\n\
  -G,  --gui-mode                   set gui_mode\n\
  -Z,  --firmware_update            enter into firmware update mode\n\n"), 
			program_name);

	programmer_print_help();
	target_print_help();
}

static void print_version(void)
{
	printf(_GETTEXT("\
vsprog %s(%s) svn:%s\n\
CopyRight(c) %s-%s by SimonQian <SimonQian@SimonQian.com>\n\n\
This is free software; see the source for copying conditions.\n\
There is NO warranty; not even for MERCHANTABILITY or FITNESS\n\
FOR A PARTICULAR PURPOSE.\n"), VERSION, PKGBLDDATE, PKGBLDREV, "2008", "2009");
}

int main(int argc, char* argv[])
{
	int optc;
	uint8_t lose_argu = 0, mass_product_flag, firmware_update_flag;
	uint32_t i, j, argu_num;
	uint32_t require_hex_file_for_read = 0;
	uint32_t require_hex_file_for_write = 0;
	char *cur_pointer, *end_pointer;
	RESULT ret;
	
	// get directory of the application
	program_dir = (char *)malloc(strlen(argv[0]) + 1);
	if (NULL == program_dir)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		free_all_and_exit(ERRCODE_NOT_ENOUGH_MEMORY);
	}
	program_name = (char *)malloc(strlen(argv[0]) + 1);
	if (NULL == program_name)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		free_all_and_exit(ERRCODE_NOT_ENOUGH_MEMORY);
	}
	strcpy(program_dir, argv[0]);
	
	// get program_dir and program_name
	{
		char *p = strrchr(program_dir, FILE_SEPARATOR);
		if (strrchr(program_dir, FILE_SEPARATOR) == NULL)
		{
			strcpy(program_name, program_dir);
			program_dir[0] = 0;
		}
		else
		{
			strcpy(program_name, p + 1);
			*(p + 1) = 0;
			p = NULL;
		}
	}
	
	// initialize varibles
	memset(&program_info, 0, sizeof(program_info));
	
	// if no argument, print help
	if (1 == argc)
	{
		// no parameter
		print_help();
		free_all_and_exit(EXIT_SUCCESS);
	}
	
	// set default parameters
	// mass-product is disable by default
	mass_product_flag = 0;
	firmware_update_flag = 0;
	// non gui mode by default
	pgbar_set_gui_mode(0);
	// set to NULL programmer
	programmer_init(NULL, program_dir);
	
	// parse options
	while ((optc = getopt_long(argc, argv, OPTSTR, long_opts, NULL)) != -1)
	{
		switch (optc)
		{
		case ':':
			break;
		case '?':
			LOG_ERROR(_GETTEXT("unkown option %s.\n"), argv[optind]);
			LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
			print_help();
			free_all_and_exit(EXIT_FAILURE);
			break;
		case 'h':
			// --help
			print_help();
			free_all_and_exit(EXIT_SUCCESS);
			break;
		case 'v':
			// --version
			print_version();
			free_all_and_exit(EXIT_SUCCESS);
			break;
		case 'd':
			// --debug level
			if (((strlen(optarg) != 1)) || (optarg[0] > '3') 
				|| (optarg[0] < '1'))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(ERRCODE_INVALID_OPTION);
			}
			
			verbosity = optarg[0] - '0';
			break;
		case 'D':
			// --display-programmer
			j = 0;
			for (i = 0; programmers_info[i].name != NULL; i++)
			{
				j += programmers_info[i].display_programmer();
			}
			if (0 == j)
			{
				LOG_INFO(_GETTEXT("no programmer supported found.\n"));
			}
			free_all_and_exit(EXIT_SUCCESS);
		case 'P':
			// --parameter [fuse/lock]
			if ((NULL == program_info.chip_name) || (NULL == program_info.chip_type))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "Target");
				free_all_and_exit(ERROR_FAIL);
			}
			
			target_print_fl(optarg);
			free_all_and_exit(ERROR_OK);
		case 'S':
			// --support [target/programmer]
			if (!strcmp(optarg, "all"))
			{
				// print all Supported programmers
				programmer_print_list();
				// print all Supported devices
				target_print_list();
				free_all_and_exit(EXIT_SUCCESS);
			}
			else if (!strcmp(optarg, "programmer"))
			{
				// print all Supported programmers
				programmer_print_list();
				free_all_and_exit(EXIT_SUCCESS);
			}
			else if (!strcmp(optarg, "target"))
			{
				// print all Supported devices
				target_print_list();
				free_all_and_exit(EXIT_SUCCESS);
			}
			else
			{
				for (i = 0; programmers_info[i].name != NULL; i++)
				{
					if (!strcmp(programmers_info[i].name, optarg))
					{
						programmers_info[i].parse_argument('S', optarg);
						free_all_and_exit(EXIT_SUCCESS);
					}
				}
				for (i = 0; targets_info[i].name != NULL; i++)
				{
					if (!strcmp(targets_info[i].name, optarg))
					{
						target_print_target(i);
						free_all_and_exit(EXIT_SUCCESS);
					}
				}
				
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), optarg);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_SUPPORT));
				free_all_and_exit(ERRCODE_NOT_SUPPORT);
			}
			free_all_and_exit(ERROR_OK);
		case 's':
			// --target-series
			program_info.chip_type = optarg;
			ret = target_init(&program_info);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "initialize target");
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(ERRCODE_FAILURE_OPERATION);
			}
			break;
		case 'c':
			// --target-module
			program_info.chip_name = optarg;
			ret = target_init(&program_info);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "initialize target");
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(ERRCODE_FAILURE_OPERATION);
			}
			break;
		case 'p':
			// --programmer
			ret = programmer_init(optarg, program_dir);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), optarg);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_SUPPORT));
				free_all_and_exit(ERRCODE_NOT_SUPPORT);
			}
			break;
		case 'o':
			// --operation
			argu_num = (int)strlen(optarg) - 1;
			if (argu_num > NUM_OF_TARGET_AREA)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_SUPPORT));
				free_all_and_exit(ERRCODE_INVALID_OPTION);
			}
			
			switch (optarg[0])
			{
			case 'e':
				// Erase
				if (operations.erase_operations != 0)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_MUTIPLE_DEFINED), 
							  "erase operation");
					free_all_and_exit(EXIT_FAILURE);
				}
				if (0 == argu_num)
				{
					operations.erase_operations |= ALL;
				}
				else
				{
					ret = parse_operation(&operations.erase_operations, NULL, 
										  optarg + 1, argu_num);
					if (ret != ERROR_OK)
					{
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								  "parse erase operation");
						free_all_and_exit(EXIT_FAILURE);
					}
				}
				break;
			case 'r':
				// Read
				if ((operations.read_operations 
						& ~operations.verify_operations) 
					!= 0)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_MUTIPLE_DEFINED), 
							  "read operation");
					free_all_and_exit(EXIT_FAILURE);
				}
				if (0 == argu_num)
				{
					for (j = 0; 
						 cur_target->program_area_map[j].area_mask > 0; j++)
					{
						operations.read_operations |= 
									cur_target->program_area_map[j].area_mask;
						require_hex_file_for_write += 
									cur_target->program_area_map[j].data_pos;
					}
				}
				else
				{
					uint32_t require_input_num;
					uint32_t tmp_value;
					ret = parse_operation(&tmp_value, &require_input_num, 
										  optarg + 1, argu_num);
					if (ret != ERROR_OK)
					{
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								  "parse read operation");
						free_all_and_exit(EXIT_FAILURE);
					}
					operations.read_operations |= tmp_value;
					require_hex_file_for_write += require_input_num;
				}
				break;
			case 'v':
				// Verify
				if (operations.verify_operations != 0)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_MUTIPLE_DEFINED), 
							  "verify operation");
					free_all_and_exit(EXIT_FAILURE);
				}
				if (0 == argu_num)
				{
					for (j = 0; 
						 cur_target->program_area_map[j].area_mask > 0; j++)
					{
						operations.verify_operations |= 
									cur_target->program_area_map[j].area_mask;
						require_hex_file_for_read += 
									cur_target->program_area_map[j].data_pos;
					}
				}
				else
				{
					uint32_t require_input_num;
					
					ret = parse_operation(&operations.verify_operations, 
										  &require_input_num, optarg + 1, 
										  argu_num);
					if (ret != ERROR_OK)
					{
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								  "parse verify operation");
						free_all_and_exit(EXIT_FAILURE);
					}
					require_hex_file_for_read += require_input_num;
				}
				
				operations.read_operations |= operations.verify_operations;
				break;
			case 'w':
				// Write
				if (operations.write_operations != 0)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_MUTIPLE_DEFINED), 
							  "write operation");
					free_all_and_exit(EXIT_FAILURE);
				}
				if (0 == argu_num)
				{
					for (j = 0; 
						 cur_target->program_area_map[j].area_mask > 0; j++)
					{
						operations.write_operations |= 
									cur_target->program_area_map[j].area_mask;
						require_hex_file_for_read += 
									cur_target->program_area_map[j].data_pos;
					}
				}
				else
				{
					uint32_t require_input_num;
					
					ret = parse_operation(&operations.write_operations, 
										  &require_input_num, optarg + 1, 
										  argu_num);
					if (ret != ERROR_OK)
					{
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								  "parse write operation");
						free_all_and_exit(EXIT_FAILURE);
					}
					require_hex_file_for_read += require_input_num;
				}
				break;
			default:
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(ERRCODE_INVALID_OPTION);
				break;
			}
			break;
		case 'I':
			// --input-file
			if ((('"' == optarg[0]) && ('"' == optarg[strlen(optarg) - 1])) 
			|| (('\'' == optarg[0]) && ('\'' == optarg[strlen(optarg) - 1])))
			{
				((char *)optarg)[strlen(optarg) - 1] = '\0';
				strcpy((char *)optarg, optarg + 1);
			}
			
			if (ERROR_OK != FILELIST_Add(&fl_in, optarg, 0, 0, "rt"))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_HANDLE_DEVICE), 
							"add file", optarg);
				free_all_and_exit(ERRCODE_FAILURE_OPERATION);
			}
			break;
		case 'O':
			// --output-file
			if ((('"' == optarg[0]) && ('"' == optarg[strlen(optarg) - 1])) 
			|| (('\'' == optarg[0]) && ('\'' == optarg[strlen(optarg) - 1])))
			{
				((char *)optarg)[strlen(optarg) - 1] = '\0';
				strcpy((char *)optarg, optarg + 1);
			}
			
			if (ERROR_OK != FILELIST_Add(&fl_out, optarg, 0, 0, "wt"))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_HANDLE_DEVICE), 
							"add file", optarg);
				free_all_and_exit(ERRCODE_FAILURE_OPERATION);
			}
			break;
		case 'J':
			// --jtag-dc
			cur_pointer = optarg;
			
			target_jtag_pos.ub = (uint8_t)strtoul(cur_pointer, &end_pointer, 0);
			if ((cur_pointer == end_pointer) || ('\0' == end_pointer[0]) 
				|| ('\0' == end_pointer[1]))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(ERRCODE_INVALID_OPTION);
			}
			
			cur_pointer = end_pointer + 1;
			target_jtag_pos.ua = (uint8_t)strtoul(cur_pointer, &end_pointer, 0);
			if ((cur_pointer == end_pointer) || ('\0' == end_pointer[0]) 
				|| ('\0' == end_pointer[1]))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(ERRCODE_INVALID_OPTION);
			}
			
			cur_pointer = end_pointer + 1;
			target_jtag_pos.bb = (uint16_t)strtoul(cur_pointer, &end_pointer, 0);
			if ((cur_pointer == end_pointer) || ('\0' == end_pointer[0]) 
				|| ('\0' == end_pointer[1]))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(ERRCODE_INVALID_OPTION);
			}
			
			cur_pointer = end_pointer + 1;
			target_jtag_pos.ba = (uint16_t)strtoul(cur_pointer, &end_pointer, 0);
			if (cur_pointer == end_pointer)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(ERRCODE_INVALID_OPTION);
			}
			break;
		case 'm':
			// --mode, accept one character
			if (strlen(optarg) != 1)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(ERRCODE_INVALID_OPTION);
			}
			if ((NULL == cur_target->program_mode_str) 
				|| (NULL == strchr(cur_target->program_mode_str, optarg[0])))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), optarg, 
						  cur_target->name);
				return ERRCODE_NOT_SUPPORT;
			}
			
			program_mode = 
				1 << (strchr(cur_target->program_mode_str, optarg[0]) 
											- cur_target->program_mode_str);
			break;
		case 'M':
			// --mass-product
			mass_product_flag = 1;
			break;
		case 'G':
			// --gui-mode
			pgbar_set_gui_mode(1);
			break;
		case 'Z':
			// --firmware_update
			firmware_update_flag = 1;
			break;
		default:
			if (cur_target != NULL)
			{
				ret = cur_target->parse_argument((char)optc, optarg);
				if (ERROR_OK == ret)
				{
					break;
				}
			}
			if (cur_programmer != NULL)
			{
				ret = cur_programmer->parse_argument((char)optc, optarg);
				if (ERROR_OK == ret)
				{
					break;
				}
			}
			
			lose_argu = 1;
			break;
		}
	}
	if (lose_argu || (optind < (argc - 1)))
	{
		// parameter parse error
		// print error message and exit
		if (optind < argc)
		{
			LOG_ERROR(_GETTEXT("extra operand: %s.\n"), argv[optind]);
			LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
			free_all_and_exit(EXIT_FAILURE);
		}
		
	}
	
	print_title();
	
	// init programmer capabilities
	cur_programmer->init_capability(cur_programmer);
	
	if (NULL == cur_target)
	{
		LOG_ERROR(_GETTEXT("Target chip not defined, use -c and -s.\n"));
		free_all_and_exit(EXIT_FAILURE);
	}
	// init and check programmer's ability
	if ((cur_programmer->interfaces & cur_target->interface_needed()) 
		!= cur_target->interface_needed())
	{
		LOG_ERROR(_GETTEXT("%s can not support %s in the mode defined.\n"), 
				  cur_programmer->name, cur_target->name);
		free_all_and_exit(EXIT_FAILURE);
	}
	
	// check file
	if ((require_hex_file_for_read > 0) 
		&& ((NULL == fl_in) || (NULL == fl_in->path) || (NULL == fl_in->file)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "input file");
		free_all_and_exit(EXIT_FAILURE);
	}
	if ((require_hex_file_for_write > 0) 
		&& ((NULL == fl_out) || (NULL == fl_out->path) 
			|| (NULL == fl_out->file)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "output file");
		free_all_and_exit(EXIT_FAILURE);
	}
	
	// init programmer
	if (firmware_update_flag)
	{
		int verbosity_tmp = verbosity;
		
		// send command first to enter into firmware update mode
		verbosity = -1;
		ret = cur_programmer->init();
		verbosity = verbosity_tmp;
		if (ERROR_OK == ret)
		{
			if (cur_programmer->enter_firmware_update_mode != NULL)
			{
				if (ERROR_OK != cur_programmer->enter_firmware_update_mode())
				{
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							  "enter into firmware update mode");
					free_all_and_exit(ERRCODE_FAILURE_OPERATION);
				}
			}
			
			// close device
			cur_programmer->fini();
			// sleep 3s
			sleep_ms(3000);
		}
	}
	else if (cur_target->interface_needed())
	{
		ret = cur_programmer->init();
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT("Programmer %s not initialized.\n"), 
					  cur_programmer->name);
			free_all_and_exit(EXIT_FAILURE);
		}
	}
	
	// init target
	ret = cur_target->init(&program_info, program_dir, cur_programmer);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "initialize target");
		free_all_and_exit(ERRCODE_FAILURE_OPERATION);
	}
	// exit here is no operation defined
	if (!(operations.checksum_operations || operations.erase_operations 
		|| operations.read_operations || operations.verify_operations 
		|| operations.write_operations))
	{
		free_all_and_exit(EXIT_SUCCESS);
	}
	
	// malloc buffer
	ret = target_alloc_data_buffer();
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		free_all_and_exit(ERRCODE_NOT_ENOUGH_MEMORY);
	}
	cur_target->prepare_buffer(&program_info);
	if (require_hex_file_for_read > 0)
	{
		filelist *fl = fl_in;
		
		if (NULL == cur_target->write_buffer_from_file_callback)
		{
			LOG_BUG(_GETTEXT("Invalid target struct.\n"));
			free_all_and_exit(EXIT_FAILURE);
		}
		
		while ((fl != NULL) && (fl->path != NULL) && (fl->file != NULL) 
			&& (strlen(fl->path) > 4) 
			&& (toupper(fl->path[strlen(fl->path) - 4]) == '.') 
			&& (toupper(fl->path[strlen(fl->path) - 3]) == 'H') 
			&& (toupper(fl->path[strlen(fl->path) - 2]) == 'E') 
			&& (toupper(fl->path[strlen(fl->path) - 1]) == 'X'))
		{
			ret = read_hex_file(fl->file, 
								cur_target->write_buffer_from_file_callback, 
								(void *)&program_info);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read hex file");
				free_all_and_exit(EXIT_FAILURE);
			}
			
			fl = FILELIST_GetNext(fl);
		}
	}
	
	// do programming
	if (mass_product_flag)
	{
		uint8_t *data_buf = NULL;
		uint32_t target_size, programmer_size;
		
		// mass-product
		if ((operations.read_operations > 0) 
			&& (operations.read_operations != operations.verify_operations))
		{
			LOG_ERROR(_GETTEXT("Cannot read data for mass-product.\n"));
			free_all_and_exit(EXIT_FAILURE);
		}
		if ((NULL == cur_target->get_mass_product_data_size) 
			|| (NULL == cur_target->prepare_mass_product_data))
		{
			LOG_ERROR(_GETTEXT("Not support to mass-product %s.\n"), 
					  cur_target->name);
			free_all_and_exit(EXIT_FAILURE);
		}
		if ((NULL == cur_programmer->query_mass_product_data_size) 
			|| (NULL == cur_programmer->download_mass_product_data))
		{
			LOG_ERROR(_GETTEXT("%s cannot mass-product.\n"), 
					  cur_programmer->name);
			free_all_and_exit(EXIT_FAILURE);
		}
		
		ret = cur_target->get_mass_product_data_size(operations, &program_info, 
													 &target_size);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT("Fail to get mass-product data size of %s.\n"), 
					  cur_target->name);
			free_all_and_exit(EXIT_FAILURE);
		}
		ret = cur_programmer->query_mass_product_data_size(&programmer_size);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT("Fail to get mass-product data size of %s.\n"), 
					  cur_programmer->name);
			free_all_and_exit(EXIT_FAILURE);
		}
		
		target_size += 1;		// 1 more byte for target index
		if (programmer_size >= target_size)
		{
			data_buf = (uint8_t*)malloc(target_size);
			if (NULL == data_buf)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
				free_all_and_exit(ERRCODE_NOT_ENOUGH_MEMORY);
			}
			
			// get mass-product data
			ret = cur_target->prepare_mass_product_data(operations, 
														&program_info, 
														data_buf + 1);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "prepare mass-product data");
				free_all_and_exit(EXIT_FAILURE);
			}
			
			// set target index
			data_buf[0] = 
				(uint8_t)abs((int)((uint8_t*)cur_target - (uint8_t*)&targets_info)) 
				/ sizeof(targets_info[0]);
			
			// download mass-product data to programmer
			ret = cur_programmer->download_mass_product_data(cur_target->name, 
															 data_buf, 
															 target_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "download mass-product data");
				free_all_and_exit(EXIT_FAILURE);
			}
		}
		else
		{
			LOG_ERROR(_GETTEXT("Not enough space for mass-product in %s.\n"), 
					  cur_programmer->name);
			free_all_and_exit(EXIT_FAILURE);
		}
	}
	else
	{
		// in system programmer
		if (!(operations.checksum_operations || operations.erase_operations 
			|| operations.read_operations || operations.verify_operations 
			|| operations.write_operations))
		{
			// no operation defined, read chip id
			operations.read_operations = CHIP_ID;
		}
		
		ret = cur_target->program(operations, &program_info, 
								  cur_programmer);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATE_DEVICE), cur_target->name);
			free_all_and_exit(EXIT_FAILURE);
		}
	}
	
	// close handle and free memory
	free_all_and_exit(EXIT_SUCCESS);
	
	return 0;
}

