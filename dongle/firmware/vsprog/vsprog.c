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

#include "config.h"

#include <stdlib.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "interfaces.h"
#include "vsprog.h"
#include "target.h"
#include "scripts.h"
#include "app_scripts.h"

#include "interfaces_script.h"
#include "pgbar.h"
#include "memlist.h"
#include "strparser.h"

VSS_HANDLER(vsprog_help);
VSS_HANDLER(vsprog_version);
VSS_HANDLER(vsprog_debug_level);
VSS_HANDLER(vsprog_support);
VSS_HANDLER(vsprog_operation);
VSS_HANDLER(vsprog_mass);
VSS_HANDLER(vsprog_free_all);
VSS_HANDLER(vsprog_init);

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
	VSS_CMD_END
};
struct vss_cmd_list_t vsprog_cmd_list = VSS_CMD_LIST("vsprog", vsprog_cmd);

int verbosity = LOG_DEFAULT_LEVEL;
int verbosity_stack[1];
struct operation_t operations;

static void free_all(void)
{
	struct program_context_t context;
	struct program_area_t *prog_area = NULL;
	uint32_t i;
	
	// free program buffer
	context.op = &operations;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = interfaces;
	target_data_free(&context);
	
	if (cur_target != NULL)
	{
		cur_target = NULL;
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

static void free_all_and_exit(int exit_code)
{
	vss_run_script("free-all");
	vss_run_script("function_free");
	vss_run_script("exit");
	vss_fini();
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
	PRINTF(_GETTEXT(VSPROG_VERSION "\r\n" VSPROG_COPYRIGHT "\r\n\r\n\
URL: http://www.SimonQian.com/en/Versaloon\r\n\
mail: SimonQian@SimonQian.com\r\n\r\n"));
}

static void print_system_info(void)
{
	PRINTF("System Information:\r\n");
	PRINTF("\r\n");
}

VSS_HANDLER(vsprog_help)
{
	VSS_CHECK_ARGC(1);
	
	PRINTF(_GETTEXT("\
Usage: %s [OPTION]...\r\n\
  -h,  --help                               display this help\r\n\
  -v,  --version                            display vsprog version\r\n\
  -S,  --support <TARGET>                   display support information\r\n\
  -V,  --vss-cmd \"<CMD PARA>\"              run programmer defined command\r\n\
  -P,  --parameter <AREA>                   display parameter for target area\r\n\
  -D,  --memory-detail <AREA>               display memory info for target area\r\n\
  -J,  --jtag-dc <UB UA BB BA>              set JTAG Daisy Chain\r\n\
  -d,  --debug <LEVEL>                      set debug level <0-2>\r\n\
  -s,  --target-series <SERIES>             set target series\r\n\
  -c,  --target-module <MODULE>             set target module\r\n\
  -p,  --programmer <PROGRAMMER>            set programmer\r\n\
  -l,  --virtualprog <VIRTUAL_PROGRAMMER>   set virtual programmer\r\n\
  -i,  --indexes <INDEX_STR>                configure indexes of virtual programmer\r\n\
  -o,  --operation <OPERATIONS>             set programming operation\r\n\
  -e,  --erase-on-demand                    erase target according to demand\r\n\
  -I,  --input-file \"<FILE>[@SEG,ADDR]\"     set input file\r\n\
  -O,  --output-file \"<FILE>[@SEG,ADDR]\"    set output file\r\n\
  -F,  --frequency <FREQUENCY_KHZ>          set programming frequency\r\n\
  -K,  --kernel-khz <KERNEL_KHZ>            set kernel frequency\r\n\
  -Q,  --quartz-khz <QUARTZ_KHZ>            set quartz frequency\r\n\
  -A,  --auto-adjust                        enable auto-adjust feature\r\n\
  -m,  --mode <MODE>                        set programming mode\r\n\
  -t,  --target <TARGET VALUE>              set target value, eg(fuse): -tu0x02\r\n\
  -L,  --list-programmer                    list programmers available\r\n\
  -M,  --mass-product                       set mass_product mode\r\n\
  -G,  --gui-mode                           set gui_mode\r\n\
  -a,  --address                            set address of target chip\r\n\r\n"),
			"vsprog");

	target_print_help();
	
	return VSFERR_NONE;
}

VSS_HANDLER(vsprog_version)
{
	VSS_CHECK_ARGC(1);
	
	PRINTF(_GETTEXT(VSPROG_VERSION "\r\n" VSPROG_COPYRIGHT "\r\n\r\n\
This is free software; see the source for copying conditions.\r\n\
There is NO warranty; not even for MERCHANTABILITY or FITNESS\r\n\
FOR A PARTICULAR PURPOSE.\r\n"));
	
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
		// print all Supported devices
		target_print_list();
	}
	else if (!strcmp(argv[1], "system"))
	{
		print_system_info();
	}
	else if (!strcmp(argv[1], "target"))
	{
		// print all Supported devices
		target_print_list();
	}
	else
	{
		uint32_t i;
		
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
	VSS_CHECK_ARGC(1);
	
	print_title();
	
	vss_register_cmd_list(&target_cmd_list);
	vss_register_cmd_list(&pgbar_cmd_list);
	vss_register_cmd_list(&interface_cmd_list);
	vss_register_cmd_list(&app_cmd_list);
	return VSFERR_NONE;
}

