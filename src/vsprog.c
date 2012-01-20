/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
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
#include <stdlib.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"
#include "app_scripts.h"

#include "interfaces_script.h"
#include "pgbar.h"
#include "memlist.h"
#include "filelist.h"
#include "strparser.h"
#include "comisp/comisp.h"
#include "usb/usbapi.h"

#define OPTSTR			"hvS:P:s:c:Mp:U:D:Ld:Go:F:m:x:C:I:O:J:b:V:t:K:W:Aqel:i:Q:a:E:Z:"
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
	{"virtualprog", required_argument, NULL, 'l'},
	{"indexes", required_argument, NULL, 'i'},
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
	{"kernel-khz", required_argument, NULL, 'K'},
	{"quartz-khz", required_argument, NULL, 'Q'},
	{"wait-state", required_argument, NULL, 'W'},
	{"auto-adjust", no_argument, NULL, 'A'},
	{"buffsize", required_argument, NULL, 'b'},
	{"vss-cmd", required_argument, NULL, 'V'},
	{"quiet", no_argument, NULL, 'q'},
	{"erase-on-demand", no_argument, NULL, 'e'},
	{"address", required_argument, NULL, 'a'},
	{"embedded-vsprog-config", required_argument, NULL, 'E'},
	{"embedded-vsprog-data", required_argument, NULL, 'Z'},
	{NULL, 0, NULL, 0},
};

VSS_HANDLER(vsprog_help);
VSS_HANDLER(vsprog_version);
VSS_HANDLER(vsprog_debug_level);
VSS_HANDLER(vsprog_support);
VSS_HANDLER(vsprog_operation);
VSS_HANDLER(vsprog_mass);
VSS_HANDLER(vsprog_free_all);
VSS_HANDLER(vsprog_init);
VSS_HANDLER(embedded_vsprog_config);
VSS_HANDLER(embedded_vsprog_data);

static const struct vss_cmd_t vsprog_cmd[] =
{
	VSS_CMD(	"help",
				"show help, format: help/h",
				vsprog_help,
				NULL),
	VSS_CMD(	"h",
				"show help, format: help/h",
				vsprog_help,
				NULL),
	VSS_CMD(	"version",
				"show version, format: version/v",
				vsprog_version,
				NULL),
	VSS_CMD(	"v",
				"show version, format: version/v",
				vsprog_version,
				NULL),
	VSS_CMD(	"debug",
				"set debug level, format: debug/D LEVEL",
				vsprog_debug_level,
				NULL),
	VSS_CMD(	"d",
				"set debug level, format: debug/D LEVEL",
				vsprog_debug_level,
				NULL),
	VSS_CMD(	"support",
				"display support information, format: support/S [TARGET]",
				vsprog_support,
				NULL),
	VSS_CMD(	"S",
				"display support information, format: support/S [TARGET]",
				vsprog_support,
				NULL),
	VSS_CMD(	"operation",
				"define operations, format: operation/o [OPERATIONS]",
				vsprog_operation,
				NULL),
	VSS_CMD(	"o",
				"define operations, format: operation/o [OPERATIONS]",
				vsprog_operation,
				NULL),
	VSS_CMD(	"mass-product",
				"enable mass product mode, format: mass-product/M",
				vsprog_mass,
				NULL),
	VSS_CMD(	"M",
				"enable mass product mode, format: mass-product/M",
				vsprog_mass,
				NULL),
	VSS_CMD(	"free-all",
				"free everything, format: free-all",
				vsprog_free_all,
				NULL),
	VSS_CMD(	"init",
				"vsprog initialization, format: init",
				vsprog_init,
				NULL),
	VSS_CMD(	"embedded-vsprog-config",
				"generate config data for embedded vsprog, "
				"format: embedded-vsprog-config/E FILE",
				embedded_vsprog_config,
				NULL),
	VSS_CMD(	"E",
				"generate config data for embedded vsprog, "
				"format: embedded-vsprog-config/E FILE",
				embedded_vsprog_config,
				NULL),
	VSS_CMD(	"embedded-vsprog-data",
				"generate target data for embedded vsprog, "
				"format: embedded-vsprog-data/Z FILE",
				embedded_vsprog_data,
				NULL),
	VSS_CMD(	"Z",
				"generate target data for embedded vsprog, "
				"format: embedded-vsprog-data/Z FILE",
				embedded_vsprog_data,
				NULL),
	VSS_CMD_END
};
struct vss_cmd_list_t vsprog_cmd_list = VSS_CMD_LIST("vsprog", vsprog_cmd);

int verbosity = LOG_DEFAULT_LEVEL;
int verbosity_stack[1];
struct operation_t operations;
static bool vsprog_query_cmd = true;

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

struct filelist *fl_in = NULL, *fl_out = NULL;

static void free_all(void)
{
	struct program_context_t context;
	struct program_area_t *prog_area = NULL;
	uint32_t i;
	
	// free program buffer
	context.op = &operations;
	context.target = cur_target;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = interfaces;
	target_data_free(&context);
	
	FILELIST_Free(&fl_in);
	FILELIST_Free(&fl_out);
	
	if (cur_target != NULL)
	{
		cur_target = NULL;
	}
	
	if ((cur_interface != NULL) && (cur_interface->fini != NULL))
	{
		cur_interface->fini();
		cur_interface = NULL;
	}
	
	memset(&operations, 0, sizeof(operations));
	if (program_info.chip_name != NULL)
	{
		free(program_info.chip_name);
		program_info.chip_name = NULL;
	}
	if (program_info.chip_type != NULL)
	{
		free(program_info.chip_type);
		program_info.chip_type = NULL;
	}
	if (program_info.ifs_indexes != NULL)
	{
		free(program_info.ifs_indexes);
		program_info.ifs_indexes = NULL;
	}
	for (i = 0; i < dimof(target_area_name); i++)
	{
		prog_area = target_get_program_area(&program_info, i);
		if (prog_area != NULL)
		{
			if (prog_area->cli_str != NULL)
			{
				free(prog_area->cli_str);
				prog_area->cli_str = NULL;
			}
		}
	}
	memset(&program_info, 0, sizeof(program_info));
	if (target_chip_param.chip_areas != NULL)
	{
		target_chip_area_free(target_chip_param.chip_areas);
	}
	memset(&target_chip_param, 0, sizeof(target_chip_param));
	target_release_chip_series(&target_chips);
}

static void free_vsprog_system(void)
{
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
}

static void free_all_and_exit(int exit_code)
{
	vss_run_script("free-all");
	vss_run_script("function_free");
	vss_fini();
	free_vsprog_system();
	exit(exit_code);
}

static vsf_err_t parse_operation(uint32_t *operation, const char *opt,
									uint32_t optlen)
{
	uint32_t mask = 0, tmp;
	uint32_t i;
	
#if PARAM_CHECK
	if ((NULL == operation) || (NULL == opt))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return VSFERR_INVALID_PARAMETER;
	}
#endif
	
	for (i = 0; i < optlen; i++)
	{
		tmp = target_area_mask(opt[i]);
		if (tmp == 0)
		{
			LOG_ERROR(ERRMSG_INVALID_CHARACTER, opt[i], "target area");
			return VSFERR_FAIL;
		}
		mask |= tmp;
	}
	
	*operation = mask;
	return VSFERR_NONE;
}

static void print_title(void)
{
	PRINTF(_GETTEXT(VSPROG_VERSION LOG_LINE_END VSPROG_COPYRIGHT LOG_LINE_END
LOG_LINE_END
"URL: http://www.SimonQian.com/en/Versaloon" LOG_LINE_END
"mail: SimonQian@SimonQian.com" LOG_LINE_END LOG_LINE_END));
}

static void print_system_info(void)
{
	PRINTF("System Information:" LOG_LINE_END);
	PRINTF("config_dir = %s" LOG_LINE_END, config_dir);
	PRINTF(LOG_LINE_END);
}

VSS_HANDLER(vsprog_help)
{
	VSS_CHECK_ARGC(1);
	
	PRINTF(_GETTEXT(
"Usage: %s [OPTION]..."LOG_LINE_END
"  -h,  --help                               display this help"LOG_LINE_END
"  -v,  --version                            display vsprog version"LOG_LINE_END
"  -S,  --support <TARGET>                   display support information"LOG_LINE_END
"  -V,  --vss-cmd \"<CMD PARA>\"              run programmer defined command"LOG_LINE_END
"  -P,  --parameter <AREA>                   display parameter for target area"LOG_LINE_END
"  -D,  --memory-detail <AREA>               display memory info for target area"LOG_LINE_END
"  -J,  --jtag-dc <UB UA BB BA>              set JTAG Daisy Chain"LOG_LINE_END
"  -d,  --debug <LEVEL>                      set debug level <0-2>"LOG_LINE_END
"  -s,  --target-series <SERIES>             set target series"LOG_LINE_END
"  -c,  --target-module <MODULE>             set target module"LOG_LINE_END
"  -p,  --programmer <PROGRAMMER>            set programmer"LOG_LINE_END
"  -l,  --virtualprog <VIRTUAL_PROGRAMMER>   set virtual programmer"LOG_LINE_END
"  -i,  --indexes <INDEX_STR>                configure indexes of virtual programmer"LOG_LINE_END
"  -o,  --operation <OPERATIONS>             set programming operation"LOG_LINE_END
"  -e,  --erase-on-demand                    erase target according to demand"LOG_LINE_END
"  -I,  --input-file \"<FILE>[@SEG,ADDR]\"     set input file"LOG_LINE_END
"  -O,  --output-file \"<FILE>[@SEG,ADDR]\"    set output file"LOG_LINE_END
"  -F,  --frequency <FREQUENCY_KHZ>          set programming frequency"LOG_LINE_END
"  -K,  --kernel-khz <KERNEL_KHZ>            set kernel frequency"LOG_LINE_END
"  -Q,  --quartz-khz <QUARTZ_KHZ>            set quartz frequency"LOG_LINE_END
"  -A,  --auto-adjust                        enable auto-adjust feature"LOG_LINE_END
"  -m,  --mode <MODE>                        set programming mode"LOG_LINE_END
"  -t,  --target <TARGET VALUE>              set target value, eg(fuse): -tu0x02"LOG_LINE_END
"  -L,  --list-programmer                    list programmers available"LOG_LINE_END
"  -M,  --mass-product                       set mass_product mode"LOG_LINE_END
"  -G,  --gui-mode                           set gui_mode"LOG_LINE_END
"  -a,  --address                            set address of target chip"LOG_LINE_END
LOG_LINE_END), program_name);

	interface_print_help();
	target_print_help();
	
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_version)
{
	VSS_CHECK_ARGC(1);
	
	PRINTF(_GETTEXT(VSPROG_VERSION LOG_LINE_END VSPROG_COPYRIGHT LOG_LINE_END LOG_LINE_END"\
This is free software; see the source for copying conditions."LOG_LINE_END"\
There is NO warranty; not even for MERCHANTABILITY or FITNESS"LOG_LINE_END"\
FOR A PARTICULAR PURPOSE." LOG_LINE_END));
	
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_debug_level)
{
	int value;
	
	VSS_CHECK_ARGC(2);
	
	value = (int)strtoul(argv[1], NULL, 0);
	if ((value < 0) || (value > DEBUG_LEVEL))
	{
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	verbosity = value;
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_support)
{
	VSS_CHECK_ARGC(2);
	
	if (!strcmp(argv[1], "all"))
	{
		// print system information
		print_system_info();
		// print all Supported programmers
		interface_print_list();
		// print all Supported devices
		target_print_list();
	}
	else if (!strcmp(argv[1], "system"))
	{
		print_system_info();
	}
	else if (!strcmp(argv[1], "programmer"))
	{
		// print all Supported programmers
		interface_print_list();
	}
	else if (!strcmp(argv[1], "target"))
	{
		// print all Supported devices
		target_print_list();
	}
	else
	{
		uint32_t i;
		
		for (i = 0; interfaces_info[i] != NULL; i++)
		{
			if (!strcmp(interfaces_info[i]->name, argv[1]))
			{
				vss_call_notifier(interfaces_info[i]->notifier,
									"support", NULL);
				return VSFERR_NONE;
			}
		}
		for (i = 0; targets_info[i].name != NULL; i++)
		{
			if (!strcmp(targets_info[i].name, argv[1]))
			{
				target_print_target(i);
				return VSFERR_NONE;
			}
		}
		
		LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
		LOG_ERROR(ERRMSG_TRY_SUPPORT);
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_operation)
{
	uint32_t argu_num;
	uint32_t *popt_tmp = NULL;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (1 == argc)
	{
		memset(&operations, 0, sizeof(operations));
		return VSFERR_NONE;
	}
	
	argu_num = strlen(argv[1]) - 1;
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "target");
		return VSFERR_FAIL;
	}
	
	switch (argv[1][0])
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
		goto Parse_Operation;
	case 'i':
		// Information
Parse_Operation:
		vsprog_query_cmd = false;
		if (popt_tmp != NULL)
		{
			if (*popt_tmp != 0)
			{
				LOG_ERROR(ERRMSG_MUTIPLE_DEFINED, "operation");
				return VSFERR_FAIL;
			}
			if (0 == argu_num)
			{
				*popt_tmp = ALL;
			}
			else
			{
				if (parse_operation(popt_tmp, &argv[1][1], argu_num))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse operation");
					return VSFERR_FAIL;
				}
			}
		}
		break;
	default:
		LOG_ERROR(ERRMSG_INVALID_OPERATION, argv[1][0]);
		return VSFERR_FAIL;
		break;
	}
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_mass)
{
	VSS_CHECK_ARGC(1);
	LOG_ERROR(ERRMSG_NOT_SUPPORT, "mass product mode");
	return VSFERR_FAIL;
}

VSS_HANDLER(vsprog_free_all)
{
	VSS_CHECK_ARGC(1);
	free_all();
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_init)
{
	uint32_t i;
	
	VSS_CHECK_ARGC(2);
	
	free_vsprog_system();
	
	// get directory of the application
	program_dir = (char *)malloc(strlen(argv[1]) + 1);
	program_name = (char *)malloc(strlen(argv[1]) + 1);
	if ((NULL == program_dir) || (NULL == program_name))
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		free_all_and_exit(EXIT_FAILURE);
	}
	strcpy(program_dir, argv[1]);
	
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
			LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
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
	
	vss_register_cmd_list(&target_cmd_list);
	vss_register_cmd_list(&pgbar_cmd_list);
	vss_register_cmd_list(&filelist_cmd_list);
	vss_register_cmd_list(&programmer_cmd_list);
	vss_register_cmd_list(&interface_cmd_list);
	vss_register_cmd_list(&comisp_cmd_list);
	vss_register_cmd_list(&usbapi_cmd_list);
	vss_register_cmd_list(&app_cmd_list);
	return VSFERR_NONE;
}

VSS_HANDLER(embedded_vsprog_data)
{
	vsf_err_t ret;
	struct target_cfg_data_info_t cfg_data_info;
	struct program_context_t context;
	
	VSS_CHECK_ARGC(2);
	
	if (vss_run_script("prepare"))
	{
		return VSFERR_FAIL;
	}
	
	cfg_data_info.addr = program_info.chip_address;
	cfg_data_info.addr_width = 32;
	cfg_data_info.little_endian = true;
	cfg_data_info.align = 4;
	
	context.op = &operations;
	context.target = cur_target;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = interfaces;
	ret = target_generate_data(&cfg_data_info, &context, (char *)argv[1]);
	
	vsprog_query_cmd = true;
	return ret;
}

VSS_HANDLER(embedded_vsprog_config)
{
	struct target_cfg_data_info_t cfg_data_info;
	
	VSS_CHECK_ARGC(2);
	
	cfg_data_info.addr = program_info.chip_address;
	cfg_data_info.addr_width = 32;
	cfg_data_info.little_endian = true;
	cfg_data_info.align = 4;
	return target_generate_cfg_data(&cfg_data_info, (char *)argv[1]);
}

int main(int argc, char* argv[])
{
	int optc;
	uint8_t lose_argu = 0;
	char *vss_argv[2];
	char vss_cmd[2];
	uint16_t vss_argc;
	
	APP_IO_INIT();
	print_title();
	
	vss_init();
	vss_register_cmd_list(&vsprog_cmd_list);
	
	vss_argv[0] = "init";
	vss_argv[1] = argv[0];
	vss_argc = 2;
	if (vss_run_cmd(vss_argc, (const char **)vss_argv))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize");
		free_all_and_exit(EXIT_SUCCESS);
	}
	
	// if no argument, print help
	if (1 == argc)
	{
		// no parameter
		vss_run_script("help");
		free_all_and_exit(EXIT_SUCCESS);
	}
	
	// parse options
	while ((optc = getopt_long(argc, argv, OPTSTR, long_opts, NULL)) != -1)
	{
		// remove " and '
		if (optarg != NULL)
		{
			while (('"' == optarg[0]) || ('\'' == optarg[0]))
			{
				char ch = optarg[0];
				
				if (optarg[strlen(optarg) - 1] != ch)
				{
					LOG_ERROR(ERRMSG_INVALID_OPTION, (char)optc);
					free_all_and_exit(EXIT_FAILURE);
				}
				optarg[strlen(optarg) - 1] = '\0';
				optarg++;
			}
		}
		
		switch (optc)
		{
		case ':':
			break;
		case '?':
			LOG_ERROR(ERRMSG_INVALID_CMD, argv[optind]);
			LOG_ERROR(ERRMSG_TRY_HELP);
			vss_run_script("help");
			free_all_and_exit(EXIT_FAILURE);
			break;
		default:
			vss_cmd[0] = (char)optc;
			vss_cmd[1] = '\0';
			vss_argv[0] = vss_cmd;
			if (optarg != NULL)
			{
				vss_argv[1] = optarg;
				vss_argc = 2;
			}
			else
			{
				vss_argv[1] = NULL;
				vss_argc = 1;
			}
			
			if (vss_cmd_supported(vss_argv[0]))
			{
				lose_argu = 1;
			}
			else if (vss_run_cmd(vss_argc, (const char **)vss_argv))
			{
				free_all_and_exit(EXIT_FAILURE);
			}
			break;
		}
	}
	if (lose_argu)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[optind]);
		LOG_ERROR(ERRMSG_TRY_HELP);
		free_all_and_exit(EXIT_FAILURE);
	}
	
	// "prepare" and then "operate" programming if target and operation are both defined
	if ((cur_target != NULL) && (!vsprog_query_cmd))
	{
		if (vss_run_script("prepare"))
		{
			free_all_and_exit(EXIT_FAILURE);
		}
		if (vss_run_script("enter_program_mode") ||
			vss_run_script("operate"))
		{
			vss_run_script("leave_program_mode 0");
			free_all_and_exit(EXIT_FAILURE);
		}
		if (vss_run_script("leave_program_mode 1"))
		{
			free_all_and_exit(EXIT_FAILURE);
		}
	}
	
	while (optind < argc)
	{
		vss_argv[0] = "run";
		vss_argv[1] = argv[optind++];
		vss_argc = 2;
		if (vss_run_cmd(vss_argc, (const char **)vss_argv))
		{
			LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "run", vss_argv[1]);
			free_all_and_exit(EXIT_SUCCESS);
		}
	}
	
	// close handle and free memory
	free_all_and_exit(EXIT_SUCCESS);
	
	return 0;
}

