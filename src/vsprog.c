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
#define VSPROG_VERSION		"VSProg " VERSION " " RELSTR PKGBLDREV
#define VSPROG_COPYRIGHT	\
				"CopyRight(c) 2008-2010 by SimonQian <SimonQian@SimonQian.com>"

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
#include "file_parser.h"

#include "memlist.h"
#include "filelist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "hex.h"

#define OPTSTR			"hvS:P:s:c:Mp:U:D:Ld:Go:F:m:x:C:I:O:J:Zb:V:t:"
static const struct option long_opts[] =
{
	{"help", no_argument, NULL, 'h'},
	{"version", no_argument, NULL, 'v'},
	{"support", required_argument, NULL, 'S'},
	{"parameter", required_argument, NULL, 'P'},
	{"memory-detail", required_argument, NULL, 'D'},
	{"target-series", required_argument, NULL, 's'},
	{"target-module", required_argument, NULL, 'c'},
	{"mass-product", no_argument, NULL, 'M'},
	{"programmer", required_argument, NULL, 'p'},
	{"usb", required_argument, NULL, 'U'},
	{"list-programmer", no_argument, NULL, 'L'},
	{"debug", required_argument, NULL, 'd'},
	{"gui-mode", no_argument, NULL, 'G'},
	{"operation", required_argument, NULL, 'o'},
	{"target", required_argument, NULL, 't'},
	{"frequency", required_argument, NULL, 'F'},
	{"mode", required_argument, NULL, 'm'},
	{"execute", required_argument, NULL, 'x'},
	{"comport", required_argument, NULL, 'C'},
	{"input-file", required_argument, NULL, 'I'},
	{"output-file", required_argument, NULL, 'O'},
	{"jtag-dc", required_argument, NULL, 'J'},
	{"firmware-update", no_argument, NULL, 'Z'},
	{"buffsize", required_argument, NULL, 'b'},
	{"misc-cmd", required_argument, NULL, 'V'},
	{NULL, 0, NULL, 0},
};

int verbosity = LOG_DEFAULT_LEVEL;
struct operation_t operations;

static char *program_name = NULL;
static char *program_dir = NULL;
char *config_dir = NULL;
const char *config_dirs[] = 
{
	"/usr/share/vsprog/config/", 
	"/usr/local/share/vsprog/config/", 
	"/etc/vsprog/config/", 
	"./config/"
};

const struct target_area_name_t target_area_name[NUM_OF_TARGET_AREA] = 
{
	{CHIPID_CHAR,				CHIPID,				"chipid"},
	{CHIPID_CHKSUM_CHAR,		CHIPID_CHKSUM,		"chipid_checksum"},
	{BOOTLOADER_CHAR,			BOOTLOADER,			"bootloader"},
	{BOOTLOADER_CHKSUM_CHAR,	BOOTLOADER_CHKSUM,	"bootloader_checksum"},
	{APPLICATION_CHAR,			APPLICATION,		"flash"},
	{APPLICATION_CHKSUM_CHAR,	APPLICATION_CHKSUM,	"flash_checksum"},
	{EEPROM_CHAR,				EEPROM,				"eeprom"},
	{EEPROM_CHKSUM_CHAR,		EEPROM_CHKSUM,		"eeprom_checksum"},
	{OTPROM_CHAR,				OTPROM,				"otprom"},
	{OTPROM_CHKSUM_CHAR,		OTPROM_CHKSUM,		"otprom_checksum"},
	{FUSE_CHAR,					FUSE,				"fuse"},
	{FUSE_CHKSUM_CHAR,			FUSE_CHKSUM,		"fuse_checksum"},
	{LOCK_CHAR,					LOCK,				"lock"},
	{LOCK_CHKSUM_CHAR,			LOCK_CHKSUM,		"lock_checksum"},
	{USRSIG_CHAR,				USRSIG,				"usrsig"},
	{USRSIG_CHKSUM_CHAR,		USRSIG_CHKSUM,		"usrsig_checksum"},
	{CALIBRATION_CHAR,			CALIBRATION,		"calibration"},
	{CALIBRATION_CHKSUM_CHAR,	CALIBRATION_CHKSUM	,"calibration_checksum"}
};

uint8_t program_mode = 0;
uint16_t program_frequency = 0;

struct filelist *fl_in = NULL, *fl_out = NULL;

// for JTAT
struct jtag_pos_t target_jtag_pos;

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
		program_dir = NULL;
	}
	if (config_dir != NULL)
	{
		free(config_dir);
		config_dir = NULL;
	}
	
	if (cur_target != NULL)
	{
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

static RESULT parse_operation(uint32_t *operation, 
							  const char *opt, uint32_t optlen)
{
	uint32_t mask = 0, tmp;
	uint32_t i;
	
#if PARAM_CHECK
	if ((NULL == operation) || (NULL == opt))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	for (i = 0; i < optlen; i++)
	{
		tmp = target_area_mask(opt[i]);
		if (tmp == 0)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHARACTER), 
						opt[i], "target area");
			return ERROR_FAIL;
		}
		mask |= tmp;
	}
	
	*operation = mask;
	return ERROR_OK;
}

static void print_title(void)
{
	printf(_GETTEXT(VSPROG_VERSION "\n" VSPROG_COPYRIGHT "\n\n\
URL: http://www.SimonQian.com/en/Versaloon\n\
mail: SimonQian@SimonQian.com\n\n"));
}

static void print_help(void)
{
	printf(_GETTEXT("\
Usage: %s [OPTION]...\n\
  -h,  --help                               display this help\n\
  -v,  --version                            display vsprog version\n\
  -S,  --support <TARGET>                   display support information\n\
  -V,  --misc-cmd \"<CMD PARA>\"              run programmer defined command\n\
  -P,  --parameter <AREA>                   display parameter for target area\n\
  -D,  --memory-detail <AREA>               display memory info for target area\n\
  -J,  --jtag-dc <UB UA BB BA>              set JTAG Daisy Chain\n\
  -d,  --debug <LEVEL>                      set debug level <0-2>\n\
  -s,  --target-series <SERIES>             set target series\n\
  -c,  --target-module <MODULE>             set target module\n\
  -p,  --programmer <PROGRAMMER>            set programmer\n\
  -o,  --operation <OPERATIONS>             set programming operation\n\
  -I,  --input-file \"<FILE>[ seg addr]\"     set input file\n\
  -M,  --mass-product                       set mass_product mode\n\
  -G,  --gui-mode                           set gui_mode\n\
  -Z,  --firmware-update                    enter into firmware update mode\n\n"), 
			program_name);

	programmer_print_help();
	target_print_help();
}

static void print_version(void)
{
	printf(_GETTEXT(VSPROG_VERSION "\n" VSPROG_COPYRIGHT "\n\n\
This is free software; see the source for copying conditions.\n\
There is NO warranty; not even for MERCHANTABILITY or FITNESS\n\
FOR A PARTICULAR PURPOSE.\n"));
}

static void print_system_info(void)
{
	printf("System Information:\n");
	printf("config_dir = %s\n", config_dir);
	
	printf("\n");
}

int main(int argc, char* argv[])
{
	int optc;
	uint8_t lose_argu = 0, mass_product_flag, firmware_update_flag;
	uint32_t i, j, argu_num;
	uint32_t require_hex_file_for_read = 0;
	uint32_t require_hex_file_for_write = 0;
	uint32_t seg_offset, addr_offset;
	char *cur_pointer, *end_pointer;
	char *Varg[8], *cmd = NULL;
	uint8_t Varg_num;
	RESULT ret;
	uint32_t *popt_tmp;
	struct filelist **fl_tmp;
	
	// get directory of the application
	program_dir = (char *)malloc(strlen(argv[0]) + 1);
	if (NULL == program_dir)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		free_all_and_exit(EXIT_FAILURE);
	}
	program_name = (char *)malloc(strlen(argv[0]) + 1);
	if (NULL == program_name)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		free_all_and_exit(EXIT_FAILURE);
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
	
	// set config_dir
	if (strlen(program_dir) > 0)
	{
		// find program_dir first
		config_dir = (char *)malloc(strlen(program_dir) + strlen("config") + 2);
		if (NULL == config_dir)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
			free_all_and_exit(EXIT_FAILURE);
		}
		strcpy(config_dir, "");
		strcat(config_dir, program_dir);
		strcat(config_dir, "config");
		config_dir[strlen(config_dir) + 1] = '\0';
		config_dir[strlen(config_dir)] = FILE_SEPARATOR;
		if (access(config_dir, 0) != 0)
		{
			// not found, free config_dir
			free(config_dir);
			config_dir = NULL;
		}
	}
	if (NULL == config_dir)
	{
		for (i = 0; i < dimof(config_dirs); i++)
		{
			if (access(config_dirs[i], 0) == 0)
			{
				config_dir = (char *)malloc(strlen(config_dirs[i]) + 1);
				strcpy(config_dir, "");
				strcat(config_dir, config_dirs[i]);
				break;
			}
		}
	}
	
	// initialize varibles
	memset(&program_info, 0, sizeof(program_info));
	memset(&target_chip_param, 0, sizeof(target_chip_param));
	
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
	programmer_init(NULL);
	
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
				free_all_and_exit(EXIT_FAILURE);
			}
			
			verbosity = optarg[0] - '0';
			break;
		case 'L':
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
		case 'D':
			// --memory-detail
			if (((NULL == program_info.chip_name) || (NULL == cur_target))
				&& (NULL == program_info.chip_type))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "Target");
				free_all_and_exit(EXIT_FAILURE);
			}
			if (((NULL == program_info.chip_name) || (NULL == cur_target))
				&& (NULL != program_info.chip_type))
			{
				program_info.chip_name = program_info.chip_type;
				target_info_init(&program_info);
				if (NULL == cur_target)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "Target");
					free_all_and_exit(EXIT_FAILURE);
				}
			}
			if (strlen(optarg) > 1)
			{
				if (!strcmp(optarg, "all"))
				{
					optarg[0] = 0;
				}
				else
				{
					optarg[0] = target_area_char_by_fullname(optarg);
					if (0 == optarg[0])
					{
						LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
						LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
						free_all_and_exit(EXIT_FAILURE);
					}
				}
			}
			
			ret = target_init(&program_info, cur_programmer);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							"initialize target");
				free_all_and_exit(EXIT_FAILURE);
			}
			
			target_print_memory(optarg[0]);
			free_all_and_exit(EXIT_SUCCESS);
			break;
		case 'P':
			// --parameter [fuse/lock/calibration/flash/eeprom]
			if ((NULL == program_info.chip_name) 
				|| (NULL == program_info.chip_type) 
				|| (NULL == cur_target))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "Target");
				free_all_and_exit(EXIT_FAILURE);
			}
			if (strlen(optarg) > 1)
			{
				optarg[0] = target_area_char_by_fullname(optarg);
				optarg[1] = '\0';
				if (0 == optarg[0])
				{
					LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
					LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
					free_all_and_exit(EXIT_FAILURE);
				}
			}
			
			ret = target_init(&program_info, cur_programmer);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							"initialize target");
				free_all_and_exit(EXIT_FAILURE);
			}
			
			target_print_setting(optarg[0]);
			free_all_and_exit(EXIT_SUCCESS);
		case 'S':
			// --support [target/programmer/system]
			if (!strcmp(optarg, "all"))
			{
				// print system information
				print_system_info();
				// print all Supported programmers
				programmer_print_list();
				// print all Supported devices
				target_print_list();
				free_all_and_exit(EXIT_SUCCESS);
			}
			else if (!strcmp(optarg, "system"))
			{
				print_system_info();
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
				free_all_and_exit(EXIT_FAILURE);
			}
			free_all_and_exit(EXIT_SUCCESS);
		case 's':
			// --target-series
			program_info.chip_type = optarg;
			ret = target_info_init(&program_info);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "initialize target");
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(EXIT_FAILURE);
			}
			break;
		case 'c':
			// --target-module
			program_info.chip_name = optarg;
			ret = target_info_init(&program_info);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "initialize target");
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(EXIT_FAILURE);
			}
			break;
		case 'p':
			// --programmer
			ret = programmer_init(optarg);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT), optarg);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_SUPPORT));
				free_all_and_exit(EXIT_FAILURE);
			}
			break;
		case 'o':
			// --operation
			argu_num = (int)strlen(optarg) - 1;
			if (argu_num > NUM_OF_TARGET_AREA)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_SUPPORT));
				free_all_and_exit(EXIT_FAILURE);
			}
			if (NULL == cur_target)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "target");
				free_all_and_exit(EXIT_FAILURE);
			}
			
			switch (optarg[0])
			{
			case 'e':
				// Erase
				popt_tmp = &operations.erase_operations;
				goto Parse_Operation;
			case 'r':
				// Read
				popt_tmp = &operations.read_operations;
				goto Parse_Operation;
			case 'v':
				// Verify
				popt_tmp = &operations.verify_operations;
				goto Parse_Operation;
			case 'w':
				// Write
				popt_tmp = &operations.write_operations;
Parse_Operation:
				if (*popt_tmp != 0)
				{
					LOG_ERROR(_GETTEXT(ERRMSG_MUTIPLE_DEFINED), "operation");
					free_all_and_exit(EXIT_FAILURE);
				}
				if (0 == argu_num)
				{
					*popt_tmp = ALL;
				}
				else
				{
					ret = parse_operation(popt_tmp, optarg + 1, argu_num);
					if (ret != ERROR_OK)
					{
						LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
								  "parse write operation");
						free_all_and_exit(EXIT_FAILURE);
					}
				}
				break;
			default:
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(EXIT_FAILURE);
				break;
			}
			break;
		case 't':
			// --target
			if (strlen(optarg) < 2)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_SUPPORT));
				free_all_and_exit(EXIT_FAILURE);
			}
			optc = (int)target_area_idx(optarg[0]);
			if (optc < 0)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHARACTER), (char)optc, 
							"target");
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_SUPPORT));
				free_all_and_exit(EXIT_FAILURE);
			}
			program_info.program_areas[optc].value = 
										(uint64_t)strtoul(&optarg[1], NULL, 0);
			target_defined |= target_area_mask(optarg[0]);
			break;
		case 'I':
			// --input-file
			fl_tmp = &fl_in;
			goto Parse_File;
		case 'O':
			// --output-file
			fl_tmp = &fl_out;
Parse_File:
			if ((('"' == optarg[0]) && ('"' == optarg[strlen(optarg) - 1])) 
			|| (('\'' == optarg[0]) && ('\'' == optarg[strlen(optarg) - 1])))
			{
				((char *)optarg)[strlen(optarg) - 1] = '\0';
				strcpy((char *)optarg, optarg + 1);
			}
			
			for (i = strlen(optarg) - 1; i > 0; i--)
			{
				if ('@' == optarg[i])
				{
					break;
				}
			}
			seg_offset = addr_offset = 0;
			if (i > 0)
			{
				optarg[i] = '\0';
				cur_pointer = &optarg[i + 1];
				seg_offset = (uint32_t)strtoul(cur_pointer, &end_pointer, 0);
				if ((cur_pointer == end_pointer) 
					|| ((*end_pointer != '\0') 
						&& ((*end_pointer != ',') 
							|| (*(end_pointer + 1) == '\0'))))
				{
					LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
					LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
					free_all_and_exit(EXIT_FAILURE);
				}
				if (*end_pointer != '\0')
				{
					cur_pointer = end_pointer + 1;
					addr_offset = \
							(uint32_t)strtoul(cur_pointer, &end_pointer, 0);
					if ((cur_pointer == end_pointer) || (*end_pointer != '\0'))
					{
						LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
						LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
						free_all_and_exit(EXIT_FAILURE);
					}
				}
			}
			
			if (ERROR_OK != 
					FILELIST_Add(fl_tmp, optarg, seg_offset, addr_offset))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_HANDLE_DEVICE), 
							"add file", optarg);
				free_all_and_exit(EXIT_FAILURE);
			}
			break;
		case 'F':
			// --frequency
			program_frequency = (uint16_t)strtoul(optarg, NULL, 0);
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
				free_all_and_exit(EXIT_FAILURE);
			}
			
			cur_pointer = end_pointer + 1;
			target_jtag_pos.ua = (uint8_t)strtoul(cur_pointer, &end_pointer, 0);
			if ((cur_pointer == end_pointer) || ('\0' == end_pointer[0]) 
				|| ('\0' == end_pointer[1]))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(EXIT_FAILURE);
			}
			
			cur_pointer = end_pointer + 1;
			target_jtag_pos.bb = (uint16_t)strtoul(cur_pointer, &end_pointer, 0);
			if ((cur_pointer == end_pointer) || ('\0' == end_pointer[0]) 
				|| ('\0' == end_pointer[1]))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(EXIT_FAILURE);
			}
			
			cur_pointer = end_pointer + 1;
			target_jtag_pos.ba = 
							(uint16_t)strtoul(cur_pointer, &end_pointer, 0);
			if (cur_pointer == end_pointer)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(EXIT_FAILURE);
			}
			break;
		case 'm':
			// --mode, accept one character
			if (strlen(optarg) != 1)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), (char)optc);
				LOG_ERROR(_GETTEXT(ERRMSG_TRY_HELP));
				free_all_and_exit(EXIT_FAILURE);
			}
			optc = target_mode_get_idx(cur_target->program_mode, optarg[0]);
			if (optc < 0)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), optarg, 
						  cur_target->name);
				free_all_and_exit(EXIT_FAILURE);
			}
			
			program_mode = (uint8_t)optc;
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
		case 'V':
			// --misc_cmd
			if (NULL == cur_programmer)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "Programmer");
				free_all_and_exit(EXIT_FAILURE);
			}
			
			if ((('"' == optarg[0]) && ('"' == optarg[strlen(optarg) - 1])) 
			|| (('\'' == optarg[0]) && ('\'' == optarg[strlen(optarg) - 1])))
			{
				((char *)optarg)[strlen(optarg) - 1] = '\0';
				strcpy((char *)optarg, optarg + 1);
			}
			
			// process if help cmd
			if (!strcmp((const char *)optarg, "help"))
			{
				printf("misc_cmd list of current programmer:\n");
				i = 0;
				while (cur_programmer->misc_cmd[i].cmd_name != NULL)
				{
					printf("\t%s: %s\n", cur_programmer->misc_cmd[i].cmd_name, 
										cur_programmer->misc_cmd[i].help_str);
					i++;
				}
				free_all_and_exit(EXIT_SUCCESS);
			}
			
			// parse cmd
			while (' ' == optarg[0])
			{
				strcpy((char *)optarg, optarg + 1);
			}
			cmd = optarg;
			
			// parse arg
			memset(Varg, 0, sizeof(Varg));
			Varg_num = 0;
			for (i = 0; i < strlen(optarg); i++)
			{
				if ((' ' == optarg[i]) && (optarg[i + 1] != '\0'))
				{
					optarg[i]= '\0';
					Varg[Varg_num++] = &optarg[i + 1];
				}
			}
			
			// find processor
			i = 0;
			while (cur_programmer->misc_cmd[i].cmd_name != NULL)
			{
				if (!strcmp((const char *)cur_programmer->misc_cmd[i].cmd_name, 
							(const char *)cmd))
				{
					break;
				}
				i++;
			}
			
			if (NULL == cur_programmer->misc_cmd[i].cmd_name)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), cmd, 
							cur_programmer->name);
				free_all_and_exit(EXIT_FAILURE);
			}
			
			// prepare and call processor
			cur_programmer->init_capability(cur_programmer);
			ret = cur_programmer->init();
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT("Programmer %s not initialized.\n"), 
						  cur_programmer->name);
				free_all_and_exit(EXIT_FAILURE);
			}
			cur_programmer->misc_cmd[i].processor(Varg_num, 
											(const char **)&Varg);
			cur_programmer->fini();
			free_all_and_exit(EXIT_SUCCESS);
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
	
	// check filelist and open file
	// error if output file is meanwhile input file
	{
		struct filelist *fl_out_tmp = fl_out;
		
		while (fl_out_tmp != NULL)
		{
			struct filelist *fl_in_tmp = fl_in;
			
			while (fl_in_tmp != NULL)
			{
				if (!strcmp(fl_out_tmp->path, fl_in_tmp->path))
				{
					LOG_ERROR(
						_GETTEXT("%s is meanwhile outputfile and inputfile"), 
						fl_out_tmp->path);
					free_all_and_exit(EXIT_FAILURE);
				}
				
				fl_in_tmp = FILELIST_GetNext(fl_in_tmp);
			}
			
			fl_out_tmp = FILELIST_GetNext(fl_out_tmp);
		}
	}
	if ((fl_in != NULL) && (ERROR_OK != FILELIST_Open(fl_in, "rb")))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "open input file");
		free_all_and_exit(EXIT_FAILURE);
	}
	
	// init programmer capabilities
	cur_programmer->init_capability(cur_programmer);
	
	if (NULL == cur_target)
	{
		LOG_ERROR(_GETTEXT("Target chip not defined, use -c and -s.\n"));
		free_all_and_exit(EXIT_FAILURE);
	}
	// init and check programmer's ability
	i = cur_target->program_mode[program_mode].interface_needed;
	if ((cur_programmer->interfaces & i) != i)
	{
		LOG_ERROR(_GETTEXT("%s can not support %s in the mode defined.\n"), 
				  cur_programmer->name, cur_target->name);
		free_all_and_exit(EXIT_FAILURE);
	}
	
	// check file
	target_prepare_operations(&operations, 
					&require_hex_file_for_read, &require_hex_file_for_write);
	if ((require_hex_file_for_read > 0) 
		&& ((NULL == fl_in) || (NULL == fl_in->path) || (NULL == fl_in->file)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_DEFINED), "input file");
		free_all_and_exit(EXIT_FAILURE);
	}
	if ((require_hex_file_for_write > 0) 
		&& ((NULL == fl_out) || (NULL == fl_out->path)))
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
					free_all_and_exit(EXIT_FAILURE);
				}
			}
			
			// close device
			cur_programmer->fini();
			// sleep 3s
			sleep_ms(3000);
		}
	}
	else if (cur_target->program_mode[program_mode].interface_needed)
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
	ret = target_init(&program_info, cur_programmer);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "initialize target");
		free_all_and_exit(EXIT_FAILURE);
	}
	
	// malloc buffer
	ret = target_alloc_data_buffer();
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_ENOUGH_MEMORY));
		free_all_and_exit(EXIT_FAILURE);
	}
	// read file
	if (require_hex_file_for_read > 0)
	{
		struct filelist *fl = fl_in;
		
		while ((fl != NULL) && (fl->path != NULL) && (fl->file != NULL) 
			&& (strlen(fl->path) > 4))
		{
			ret = parse_file(fl->path, fl->file, (void *)&program_info, 
								&target_write_buffer_from_file_callback, 
								fl->seg_offset, fl->addr_offset);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							"parse input file");
				free_all_and_exit(EXIT_FAILURE);
			}
			
			fl = FILELIST_GetNext(fl);
		}
	}
	if (ERROR_OK != target_check_defined(operations))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					"check target defined content");
		free_all_and_exit(EXIT_FAILURE);
	}
	sleep_ms(100);	// delay 100ms
	
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
				free_all_and_exit(EXIT_FAILURE);
			}
			
			// get mass-product data
			ret = cur_target->prepare_mass_product_data(operations, 
										&program_info, data_buf + 1);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "prepare mass-product data");
				free_all_and_exit(EXIT_FAILURE);
			}
			
			// set target index
			data_buf[0] = (uint8_t)
				(abs((int)((uint8_t*)cur_target - (uint8_t*)&targets_info)) 
					/ sizeof(targets_info[0]));
			
			// download mass-product data to programmer
			ret = cur_programmer->download_mass_product_data(cur_target->name, 
														data_buf, target_size);
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
			operations.read_operations = CHIPID;
		}
		
		ret = cur_target->program(operations, &program_info, cur_programmer);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATE_DEVICE), cur_target->name);
			free_all_and_exit(EXIT_FAILURE);
		}
		
		// save contect to file for read operation
		if (cur_target != NULL)
		{
			struct program_area_map_t *p_map = 
					(struct program_area_map_t *)cur_target->program_area_map;
			
			while (p_map->name != 0)
			{
				if ((p_map->data_pos) 
					&& (operations.read_operations 
						& target_area_mask(p_map->name)))
				{
					uint8_t *buff = NULL;
					uint32_t size = 0;
					struct chip_area_info_t *area;
					int8_t area_idx;
					
					area_idx = target_area_idx(p_map->name);
					if (area_idx < 0)
					{
						p_map++;
						continue;
					}
					area = &target_chip_param.chip_areas[area_idx];
					target_get_target_area(p_map->name, &buff, &size);
					if ((buff != NULL) && (size > 0) && (fl_out != NULL))
					{
						if (ERROR_OK != save_target_to_file(fl_out, buff, 
								size, area->seg, area->addr, p_map->fseg_addr, 
								p_map->fstart_addr))
						{
							LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
										"write data to file");
							free_all_and_exit(EXIT_FAILURE);
						}
					}
				}
				
				p_map++;
			}
			end_file(fl_out);
		}
	}
	
	// close handle and free memory
	free_all_and_exit(EXIT_SUCCESS);
	
	return 0;
}

