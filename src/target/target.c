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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <inttypes.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "vsprog.h"
#include "memlist.h"
#include "pgbar.h"
#include "strparser.h"
#include "bufffunc.h"

#include "programmer.h"
#include "target.h"
#include "scripts.h"

#include "target.h"
#include "at89s5x/at89s5x.h"
#include "psoc1/psoc1.h"
#include "lpc900/lpc900.h"
#include "msp430/msp430.h"
#include "c8051f/c8051f.h"
#include "avr8/avr8.h"
#include "comisp/comisp.h"
#include "svf_player/svf_player.h"
#include "cortex-m3/cm3.h"
#include "stm32/stm32.h"
#include "lpc1000/lpc1000.h"
#include "stm8/stm8.h"
#include "at91sam3/at91sam3.h"
#include "avr32/avr32.h"
#include "avrxmega/avrxmega.h"
#include "lm3s/lm3s.h"
#include "hcs08/hcs08.h"
#include "hcs12/hcs12.h"
#include "ee93cx6/ee93cx6.h"
#include "ee24cxx/ee24cxx.h"
#include "df25xx/df25xx.h"
#include "stm32f2/stm32f2.h"

VSS_HANDLER(target_memory_detail);
VSS_HANDLER(target_parameter_detail);
VSS_HANDLER(target_series);
VSS_HANDLER(target_chip);
VSS_HANDLER(target_value);
VSS_HANDLER(target_interface_frequency);
VSS_HANDLER(target_kernel_khz);
VSS_HANDLER(target_quartz_khz);
VSS_HANDLER(target_erase_on_demand);
VSS_HANDLER(target_wait_state);
VSS_HANDLER(target_auto_adjust);
VSS_HANDLER(target_jtag_dc);
VSS_HANDLER(target_interface_mode);
VSS_HANDLER(target_prepare);
VSS_HANDLER(target_operate);
VSS_HANDLER(target_execute_addr);
VSS_HANDLER(target_enter_program_mode);
VSS_HANDLER(target_leave_program_mode);
VSS_HANDLER(target_erase);
VSS_HANDLER(target_write);
VSS_HANDLER(target_read);
VSS_HANDLER(target_verify);

struct vss_cmd_t target_cmd[] = 
{
	VSS_CMD(	"memory-detail",
				"show memory detail, format: memory-detail/D TARGET",
				target_memory_detail),
	VSS_CMD(	"D",
				"show memory detail, format: memory-detail/D TARGET",
				target_memory_detail),
	VSS_CMD(	"parameter",
				"show parameter detail, format: parameter/P TARGET",
				target_parameter_detail),
	VSS_CMD(	"P",
				"show parameter detail, format: parameter/P TARGET",
				target_parameter_detail),
	VSS_CMD(	"target-series",
				"set target series, format: target-series/s SERIES",
				target_series),
	VSS_CMD(	"s",
				"set target series, format: target-series/s SERIES",
				target_series),
	VSS_CMD(	"target-chip",
				"set target chip, format: target-chip/c CHIP",
				target_chip),
	VSS_CMD(	"c",
				"set target chip, format: target-chip/c CHIP",
				target_chip),
	VSS_CMD(	"target",
				"set target value, format: target/t TARGET VALUE",
				target_value),
	VSS_CMD(	"t",
				"set target value, format: target/t TARGET VALUE",
				target_value),
	VSS_CMD(	"frequency",
				"set frequency of programming interface, "
				"format: frequency/f FREQUENCY",
				target_interface_frequency),
	VSS_CMD(	"F",
				"set frequency of programming interface, "
				"format: frequency/f FREQUENCY",
				target_interface_frequency),
	VSS_CMD(	"kernel-khz",
				"set target kernel frequency in khz, format: kernel-khz/K KHZ",
				target_kernel_khz),
	VSS_CMD(	"K",
				"set target kernel frequency in khz, format: kernel-khz/K KHZ",
				target_kernel_khz),
	VSS_CMD(	"quartz-khz",
				"set target quartz frequency in khz, format: quartz-khz/Q KHZ",
				target_quartz_khz),
	VSS_CMD(	"Q",
				"set target quartz frequency in khz, format: quartz-khz/Q KHZ",
				target_quartz_khz),
	VSS_CMD(	"wait-state",
				"set target wait state, format: wait-state/W WAIT",
				target_wait_state),
	VSS_CMD(	"e",
				"erase on demand feature, format: e/erase-on-demand",
				target_erase_on_demand),
	VSS_CMD(	"erase-on-demand",
				"erase on demand feature, format: e/erase-on-demand",
				target_erase_on_demand),
	VSS_CMD(	"W",
				"set target wait state, format: wait-state/W WAIT",
				target_wait_state),
	VSS_CMD(	"auto-adjust",
				"set target auto adjust, format: auto-adjust/A",
				target_auto_adjust),
	VSS_CMD(	"A",
				"set target auto adjust, format: auto-adjust/A",
				target_auto_adjust),
	VSS_CMD(	"jtag-dc",
				"set JTAG daisy chain, format: jtag-dc/J UB_UA_BB_BA",
				target_jtag_dc),
	VSS_CMD(	"J",
				"set JTAG daisy chain, format: jtag-dc/J UB_UA_BB_BA",
				target_jtag_dc),
	VSS_CMD(	"mode",
				"set interface mode, format: mode/m MODE",
				target_interface_mode),
	VSS_CMD(	"m",
				"set interface mode, format: mode/m MODE",
				target_interface_mode),
	VSS_CMD(	"prepare",
				"prepare target programming, format: prepare",
				target_prepare),
	VSS_CMD(	"operate",
				"operate target programming, format: operate",
				target_operate),
	VSS_CMD(	"execute",
				"execute defined address, format: execute/x ADDR",
				target_execute_addr),
	VSS_CMD(	"x",
				"execute defined address, format: execute/x ADDR",
				target_execute_addr),
	VSS_CMD(	"enter_program_mode",
				"enter program mode, format: enter_program_mode",
				target_enter_program_mode),
	VSS_CMD(	"leave_program_mode",
				"leave program mode, format: leave_program_mode",
				target_leave_program_mode),
	VSS_CMD(	"erase",
				"erase target area, format: erase [AREA_CHAR]",
				target_erase),
	VSS_CMD(	"read",
				"read target area, format: read AREA_CHAR ADDR SIZE",
				target_read),
	VSS_CMD(	"write",
				"write target area, format: write [AREA_CHAR]",
				target_write),
	VSS_CMD(	"verify",
				"verify target area, format: verify [AREA_CHAR]",
				target_verify),
	VSS_CMD_END
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
	{CALIBRATION_CHKSUM_CHAR,	CALIBRATION_CHKSUM,	"calibration_checksum"},
	{SRAM_CHAR,					SRAM,				"sram"},
	{SPECIAL_STRING_CHAR,		SPECIAL_STRING,		"special_str"},
	{UNIQUEID_CHAR,				UNIQUEID,			"uniqueid"}
};

struct chip_series_t target_chips = {0, NULL};
struct chip_param_t target_chip_param;

struct target_info_t targets_info[] = 
{
	// stm32
#if TARGET_STM32_EN
	{
		STM32_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		stm32_program_area_map,				// program_area_map
		stm32_program_mode,					// program_mode
		&stm32_program_functions,			// program_functions
		stm32_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
	// stm32f2
#if TARGET_STM32F2_EN
	{
		STM32F2_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		stm32f2_program_area_map,			// program_area_map
		stm32f2_program_mode,				// program_mode
		&stm32f2_program_functions,			// program_functions
		stm32f2_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_LPC1000_EN
	// lpc1000
	{
		LPC1000_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		lpc1000_program_area_map,			// program_area_map
		lpc1000_program_mode,				// program_mode
		&lpc1000_program_functions,			// program_functions
		lpc1000_notifier,					// notifier
		lpc1000_adjust_setting,				// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_LM3S_EN
	// lm3s
	{
		LM3S_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		lm3s_program_area_map,				// program_area_map
		lm3s_program_mode,					// program_mode
		&lm3s_program_functions,			// program_functions
		lm3s_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AT91SAM3_EN
	// at91sam3
	{
		AT91SAM3_STRING,					// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		at91sam3_program_area_map,			// program_area_map
		at91sam3_program_mode,				// program_mode
		&at91sam3_program_functions,		// program_functions
		at91sam3_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AVR32_EN
	// avr32
	{
		AVR32_STRING,						// name
		AUTO_DETECT CAN_EXECUTE,			// feature
		avr32_program_area_map,				// program_area_map
		avr32_program_mode,					// program_mode
		&avr32_program_functions,			// program_functions
		avr32_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AVRXMEGA_EN
	// avrxmega
	{
		AVRXMEGA_STRING,					// name
		AUTO_DETECT,						// feature
		avrxmega_program_area_map,			// program_area_map
		avrxmega_program_mode,				// program_mode
		&avrxmega_program_functions,		// program_functions
		avrxmega_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_STM8_EN
	// stm8
	{
		STM8_STRING,						// name
		"",									// feature
		stm8_program_area_map,				// program_area_map
		stm8_program_mode,					// program_mode
		&stm8_program_functions,			// program_functions
		stm8_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AT89S5X_EN
	// at89s5x
	{
		S5X_STRING,							// name
		AUTO_DETECT,						// feature
		s5x_program_area_map,				// program_area_map
		s5x_program_mode,					// program_mode
		&s5x_program_functions,				// program_functions
		s5x_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_PSOC1_EN
	// psoc1
	{
		PSOC1_STRING,						// name
		AUTO_DETECT,						// feature
		psoc1_program_area_map,				// program_area_map
		psoc1_program_mode,					// program_mode
		&psoc1_program_functions,			// program_functions
		psoc1_notifier,						// notifier
		psoc1_adjust_setting,				// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_MSP430_EN
	// msp430
	{
		MSP430_STRING,						// name
		AUTO_DETECT,						// feature
		msp430_program_area_map,			// program_area_map
		msp430_program_mode,				// program_mode
		&msp430_program_functions,			// program_functions
		msp430_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_C8051F_EN
	// c8051f
	{
		C8051F_STRING,						// name
		AUTO_DETECT,						// feature
		c8051f_program_area_map,			// program_area_map
		c8051f_program_mode,				// program_mode
		&c8051f_program_functions,			// program_functions
		c8051f_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_AVR8_EN
	// avr8
	{
		AVR8_STRING,						// name
		AUTO_DETECT,						// feature
		avr8_program_area_map,				// program_area_map
		avr8_program_mode,					// program_mode
		&avr8_program_functions,			// program_functions
		avr8_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_SVF_EN
	// svf_player
	{
		SVFP_STRING,						// name
		NO_TARGET,							// feature
		svfp_program_area_map,				// program_area_map
		svfp_program_mode,					// program_mode
		&svfp_program_functions,			// program_functions
		svfp_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_LPC900_EN
	// lpc900
	{
		LPC900_STRING,						// name
		AUTO_DETECT,						// feature
		lpc900_program_area_map,			// program_area_map
		lpc900_program_mode,				// program_mode
		&lpc900_program_functions,			// program_functions
		lpc900_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_HCS08_EN
	// HCS08
	{
		HCS08_STRING,						// name
		AUTO_DETECT,						// feature
		hcs08_program_area_map,				// program_area_map
		hcs08_program_mode,					// program_mode
		&hcs08_program_functions,			// program_functions
		hcs08_notifier,						// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_HCS12_EN
	// HCS12
	{
		HCS12_STRING,						// name
		"",									// feature
		hcs12_program_area_map,				// program_area_map
		hcs12_program_mode,					// program_mode
		&hcs12_program_functions,			// program_functions
		hcs12_notifier,						// notifier
		NULL,								// adjust_setting
		hcs12_adjust_mapping,				// adjust_mapping
	},
#endif
#if TARGET_EE93CX6_EN
	// EE93CX6
	{
		EE93CX6_STRING,						// name
		"",									// feature
		ee93cx6_program_area_map,			// program_area_map
		ee93cx6_program_mode,				// program_mode
		&ee93cx6_program_functions,			// program_functions
		ee93cx6_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_EE24CXX_EN
	// EE24CXX
	{
		EE24CXX_STRING,						// name
		"",									// feature
		ee24cxx_program_area_map,			// program_area_map
		ee24cxx_program_mode,				// program_mode
		&ee24cxx_program_functions,			// program_functions
		ee24cxx_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
#if TARGET_DF25XX_EN
	// DF25XX
	{
		DF25XX_STRING,						// name
		"",									// feature
		df25xx_program_area_map,			// program_area_map
		df25xx_program_mode,				// program_mode
		&df25xx_program_functions,			// program_functions
		df25xx_notifier,					// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	},
#endif
	{
		NULL,								// name
		0,									// areas
		NULL,								// program_area_map
		NULL,								// program_mode
		NULL,								// program_functions
		NULL,								// notifier
		NULL,								// adjust_setting
		NULL,								// adjust_mapping
	}
};
struct target_info_t *cur_target = NULL;
struct program_info_t program_info;

RESULT target_build_chip_series(const char *chip_series, 
		const struct program_mode_t *program_mode, struct chip_series_t *s);
RESULT target_build_chip_fl(const char *chip_series, 
				const char *chip_module, char *type, struct chip_fl_t *fl);

static RESULT target_parse_cli_string(void)
{
	uint8_t i;
	RESULT ret;
	char *format;
	char format_tmp[32];
	
	for (i = 0; i < dimof(program_info.program_areas); i++)
	{
		if (program_info.program_areas[i].cli_str != NULL)
		{
			if (target_chip_param.chip_areas[i].cli_format != NULL)
			{
				format = target_chip_param.chip_areas[i].cli_format;
			}
			else if (target_chip_param.chip_areas[i].size <= 8)
			{
				// cli_format not defined
				// simply use %8x as format, which is simple integer input
				snprintf(format_tmp, sizeof(format_tmp), "%%%dx", 
							target_chip_param.chip_areas[i].size);
				format = format_tmp;
			}
			else
			{
				LOG_ERROR(ERRMSG_NOT_DEFINED, "cli_format");
				return ERROR_FAIL;
			}
			
			ret = strparser_parse(program_info.program_areas[i].cli_str, 
						format, program_info.program_areas[i].buff, 
						program_info.program_areas[i].size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "parse", 
							target_area_name[i].full_name);
				return ERROR_FAIL;
			}
		}
	}
	
	return ERROR_OK;
}

RESULT target_alloc_data_buffer(void)
{
	uint8_t i;
	
	for (i = 0; i < dimof(program_info.program_areas); i++)
	{
		if ((NULL == program_info.program_areas[i].buff) 
			&& (program_info.program_areas[i].size > 0))
		{
			program_info.program_areas[i].buff = 
				(uint8_t *)malloc(program_info.program_areas[i].size);
			if (NULL == program_info.program_areas[i].buff)
			{
				return ERRCODE_NOT_ENOUGH_MEMORY;
			}
			if (strlen(target_chip_param.chip_name) > 0)
			{
				memset(program_info.program_areas[i].buff, 
						(uint8_t)target_chip_param.chip_areas[i].default_value, 
						program_info.program_areas[i].size);
			}
		}
	}
	
	return ERROR_OK;
}

RESULT target_release_chip_fl(struct chip_fl_t *fl)
{
	uint32_t i, j;
	
	if (NULL == fl)
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	
	if (fl->init_value != NULL)
	{
		free(fl->init_value);
		fl->init_value = NULL;
	}
	
	// free warnings
	if (fl->warnings != NULL)
	{
		for (i = 0; i < fl->num_of_fl_warnings; i++)
		{
			if (fl->warnings[i].mask != NULL)
			{
				free(fl->warnings[i].mask);
				fl->warnings[i].mask = NULL;
			}
			if (fl->warnings[i].value != NULL)
			{
				free(fl->warnings[i].value);
				fl->warnings[i].value = NULL;
			}
			if (fl->warnings[i].msg != NULL)
			{
				free(fl->warnings[i].msg);
				fl->warnings[i].msg = NULL;
			}
		}
		free(fl->warnings);
		fl->warnings = NULL;
	}
	
	// free settings
	if (fl->settings != NULL)
	{
		for (i = 0; i < fl->num_of_fl_settings; i++)
		{
			if (fl->settings[i].name != NULL)
			{
				free(fl->settings[i].name);
				fl->settings[i].name = NULL;
			}
			if (fl->settings[i].ban != NULL)
			{
				free(fl->settings[i].ban);
				fl->settings[i].ban = NULL;
			}
			if (fl->settings[i].info != NULL)
			{
				free(fl->settings[i].info);
				fl->settings[i].info = NULL;
			}
			if (fl->settings[i].format != NULL)
			{
				free(fl->settings[i].format);
				fl->settings[i].format = NULL;
			}
			if (fl->settings[i].mask != NULL)
			{
				free(fl->settings[i].mask);
				fl->settings[i].mask = NULL;
			}
			if (fl->settings[i].checked != NULL)
			{
				free(fl->settings[i].checked);
				fl->settings[i].checked = NULL;
			}
			if (fl->settings[i].unchecked != NULL)
			{
				free(fl->settings[i].unchecked);
				fl->settings[i].unchecked = NULL;
			}
			if (fl->settings[i].choices != NULL)
			{
				for (j = 0; j < fl->settings[i].num_of_choices; j++)
				{
					if (fl->settings[i].choices[j].value != NULL)
					{
						free(fl->settings[i].choices[j].value);
						fl->settings[i].choices[j].value = NULL;
					}
					if (fl->settings[i].choices[j].text != NULL)
					{
						free(fl->settings[i].choices[j].text);
						fl->settings[i].choices[j].text = NULL;
					}
				}
				free(fl->settings[i].choices);
				fl->settings[i].choices = NULL;
			}
		}
		free(fl->settings);
		fl->settings = NULL;
	}
	memset(fl, 0, sizeof(struct chip_fl_t));
	
	return ERROR_OK;
}

RESULT target_release_chip_series(struct chip_series_t *s)
{
	uint32_t i, j;
	
	if ((s != NULL) && ((s->num_of_chips > 0) || (s->chips_param != NULL)))
	{
		for (i = 0; i < s->num_of_chips; i++)
		{
			if (s->chips_param[i].program_mode_str != NULL)
			{
				free(s->chips_param[i].program_mode_str);
				s->chips_param[i].program_mode_str = NULL;
			}
			for (j = 0; j < dimof(s->chips_param[i].chip_areas); j++)
			{
				if (s->chips_param[i].chip_areas[j].mask != NULL)
				{
					free(s->chips_param[i].chip_areas[j].mask);
					s->chips_param[i].chip_areas[j].mask = NULL;
				}
				if (s->chips_param[i].chip_areas[j].cli_format != NULL)
				{
					free(s->chips_param[i].chip_areas[j].cli_format);
					s->chips_param[i].chip_areas[j].cli_format = NULL;
				}
			}
		}
		free(s->chips_param);
		s->chips_param = NULL;
		s->num_of_chips = 0;
	}
	memset(s, 0, sizeof(struct chip_series_t));
	
	return ERROR_OK;
}

void target_free_data_buffer(void)
{
	uint8_t i;
	struct program_area_t *area;
	
	target_release_chip_series(&target_chips);
	
	for (i = 0; i < dimof(program_info.program_areas); i++)
	{
		// special_string cannot be freed
		if (SPECIAL_STRING_CHAR == target_area_name[i].name)
		{
			continue;
		}
		area = &program_info.program_areas[i];
		if (area->buff != NULL)
		{
			free(area->buff);
			area->buff = NULL;
		}
		area->size = 0;
		if (area->memlist != NULL)
		{
			MEMLIST_Free(&area->memlist);
		}
		if (area->exact_memlist != NULL)
		{
			MEMLIST_Free(&area->exact_memlist);
		}
	}
}

static void target_get_target_area(char area, uint8_t **buff, uint32_t *size)
{
	int8_t i;
	
	i = target_area_idx(area);
	if (i >= 0)
	{
		*buff = program_info.program_areas[i].buff;
		*size = program_info.program_areas[i].size;
	}
	else
	{
		*buff = NULL;
		*size = 0;
	}
}

int8_t target_area_idx_by_fullname(char *fullname)
{
	int8_t i;
	
	for (i = 0; i < (int8_t)dimof(target_area_name); i++)
	{
		if (!strcmp(target_area_name[i].full_name, fullname))
		{
			return i;
		}
	}
	return -1;
}

char target_area_char_by_fullname(char *fullname)
{
	uint32_t i;
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		if (!strcmp(target_area_name[i].full_name, fullname))
		{
			return target_area_name[i].name;
		}
	}
	return '\0';
}

int8_t target_area_idx_by_mask(uint32_t mask)
{
	int8_t i;
	
	for (i = 0; i < (int8_t)dimof(target_area_name); i++)
	{
		if (target_area_name[i].mask == mask)
		{
			return i;
		}
	}
	return -1;
}

char* target_area_fullname_by_mask(uint32_t mask)
{
	uint32_t i;
	
	for (i = 0; i < dimof(target_area_name); i++)
	{
		if (target_area_name[i].mask == mask)
		{
			return (char *)target_area_name[i].full_name;
		}
	}
	return NULL;
}

int8_t target_area_idx(char area_name)
{
	int8_t i;
	
	for (i = 0; i < (int8_t)dimof(target_area_name); i++)
	{
		if (target_area_name[i].name == area_name)
		{
			return i;
		}
	}
	return -1;
}

char* target_area_fullname(char area_name)
{
	int8_t i;
	
	i = target_area_idx(area_name);
	if (i < 0)
	{
		return NULL;
	}
	else
	{
		return (char *)target_area_name[i].full_name;
	}
}

uint32_t target_area_mask(char area_name)
{
	int8_t i;
	
	i = target_area_idx(area_name);
	if (i < 0)
	{
		return 0;
	}
	else
	{
		return target_area_name[i].mask;
	}
}

int8_t target_area_idx_by_name(char *name)
{
	if (strlen(name) == 1)
	{
		return target_area_idx(name[0]);
	}
	else
	{
		return target_area_idx_by_fullname(name);
	}
}

static RESULT target_check_single_defined(uint32_t opt)
{
	uint8_t i;
	
	opt = (program_info.areas_defined ^ opt) & opt;
	
	for (i = 0; i < 32; i++)
	{
		if (opt & (1 << i))
		{
			LOG_ERROR(ERRMSG_NOT_DEFINED, target_area_fullname_by_mask(1 << i));
			return ERROR_FAIL;
		}
	}
	return ERROR_OK;
}

int8_t target_mode_get_idx(const struct program_mode_t *mode, char mode_name)
{
	int8_t i;
	
	if (NULL == mode)
	{
		return -1;
	}
	
	i = 0;
	while (mode[i].name != 0)
	{
		if (mode[i].name == mode_name)
		{
			return i;
		}
		i++;
	}
	return -1;
}

static RESULT target_check_defined(struct operation_t operations)
{
	if (ERROR_OK != target_check_single_defined(operations.verify_operations) 
		|| (ERROR_OK 
			!= target_check_single_defined(operations.write_operations)))
	{
		return ERROR_FAIL;
	}
	else
	{
		return ERROR_OK;
	}
}

static RESULT target_write_buffer_from_file_callback(char * ext, 
				uint32_t address, uint32_t seg_addr, uint8_t* data, 
				uint32_t length, void* buffer)
{
	uint32_t i;
	int8_t area_idx;
	char area_name;
	uint8_t *area_buff;
	struct memlist **area_memlist, **area_exact_memlist;
	uint32_t area_seg, area_addr, area_size, area_page_size;
	struct program_info_t *pi = (struct program_info_t *)buffer;
	uint32_t mem_addr;
	RESULT ret;
	
	if ((NULL == cur_target) || (0 == strlen(target_chip_param.chip_name)) 
		|| (NULL == ext))
	{
		LOG_BUG(ERRMSG_NOT_INITIALIZED, "target", "");
		return ERROR_FAIL;
	}
	
	// remap if adjust_mapping is defined and format is not BIN
	if ((strcmp(ext, "BIN")) && 
		(cur_target->adjust_mapping != NULL) && 
		(ERROR_OK != cur_target->adjust_mapping(&address, 
												TARGET_MAPPING_FROM_FILE)))
	{
		LOG_BUG(ERRMSG_FAILURE_OPERATE_ADDRESS, "remap target address", 
				address);
		return ERROR_FAIL;
	}
	
	// find a right target to fill the memory
	i = 0;
	while (cur_target->program_area_map[i].name != 0)
	{
		area_name = cur_target->program_area_map[i].name;
		area_idx = target_area_idx(area_name);
		if (area_idx < 0)
		{
			i++;
			continue;
		}
		area_seg = target_chip_param.chip_areas[area_idx].seg 
						+ cur_target->program_area_map[i].fseg_addr;
		area_addr = target_chip_param.chip_areas[area_idx].addr 
						+ cur_target->program_area_map[i].fstart_addr;
		area_size = target_chip_param.chip_areas[area_idx].size;
		area_page_size = target_chip_param.chip_areas[area_idx].page_size;
		
		area_buff = pi->program_areas[area_idx].buff;
		area_memlist = &(pi->program_areas[area_idx].memlist);
		area_exact_memlist = &(pi->program_areas[area_idx].exact_memlist);
		
		if ((area_seg != seg_addr) || (area_addr > address) 
			|| ((area_addr + area_size) < (address + length)))
		{
			// not this area
			i++;
			continue;
		}
		
		// found
		if (0 == area_page_size)
		{
			// default page size is 256 bytes
			area_page_size = 256;
		}
		pi->areas_defined |= target_area_mask(area_name);
		mem_addr = address - area_addr;
		if (area_buff != NULL)
		{
			// put in area_buff
			memcpy(area_buff + mem_addr, data, length);
			ret = MEMLIST_Add(area_memlist, address, length, area_page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			ret = MEMLIST_Add(area_exact_memlist, address, length, 1);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
		}
		else
		{
			LOG_ERROR(ERRMSG_INVALID_BUFFER, "area_buff");
			return ERROR_FAIL;
		}
		
		return ERROR_OK;
	}
	
	// not found
	return ERROR_FAIL;
}

static RESULT MEMLIST_VerifyBuff(struct memlist *ml, uint8_t *buf1, 
				uint8_t *buf2, uint32_t addr, uint32_t len, uint32_t *pos)
{
	uint32_t i, len_tmp, offset, verified_len;
	
	if ((NULL == buf1) || (NULL == buf2))
	{
		return ERROR_FAIL;
	}
	
	verified_len = 0;
	while ((ml != NULL) && (verified_len < len))
	{
		if ((ml->addr >= addr) && (ml->addr < (addr + len)))
		{
			offset = ml->addr - addr;
			if ((ml->addr + ml->len) <= (addr + len))
			{
				len_tmp = ml->len;
			}
			else
			{
				len_tmp = len;
			}
		}
		else if ((addr >= ml->addr) && (addr < (ml->addr + ml->len)))
		{
			offset = 0;
			if ((ml->addr + ml->len) <= (addr + len))
			{
				len_tmp = ml->len - (addr - ml->addr);
			}
			else
			{
				len_tmp = len;
			}
		}
		else
		{
			ml = MEMLIST_GetNext(ml);
			continue;
		}
		
		for (i = 0; i < len_tmp; i++)
		{
			if (buf1[offset + i] != buf2[offset + i])
			{
				*pos = offset + i;
				return ERROR_FAIL;
			}
		}
		verified_len += len_tmp;
		ml = MEMLIST_GetNext(ml);
	}
	return ERROR_OK;
}

static RESULT target_program_check(struct program_context_t *context)
{
	const struct program_functions_t *pf = cur_target->program_functions;
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	uint64_t i;
	
	if ((NULL == pf) 
		|| ((NULL == pf->execute) 
			&& ((NULL == pf->read_target) 
				|| (NULL == pf->write_target) 
				|| (NULL == pf->erase_target))))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "current mode", pi->chip_name);
		return ERROR_FAIL;
	}
	
	// check mode
	if ((target_chips.num_of_chips > 0) 
		&& (target_chips.chips_param[0].program_mode != 0) 
		&& !(param->program_mode & (1 << pi->mode)))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "current mode", pi->chip_name);
		return ERROR_FAIL;
	}
	
	// assert programmer
	i = cur_target->program_mode[program_info.mode].interface_needed;
	if (i)
	{
		if ((ERROR_OK != interface_assert(&context->prog)) 
			|| (NULL == context->prog))
		{
			LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
			return ERROR_FAIL;
		}
		if ((context->prog->support_mask & i) != i)
		{
			LOG_ERROR("interface not supported: %s.", get_interface_name(i));
			return ERROR_FAIL;
		}
	}
	return ERROR_OK;
}

static RESULT target_enter_progmode(struct program_context_t *context)
{
	const struct program_functions_t *pf = cur_target->program_functions;
	
	if (ERROR_OK != target_program_check(context))
	{
		return ERROR_FAIL;
	}
	
	if ((pf->enter_program_mode != NULL) 
		&&(pf->enter_program_mode(context) != ERROR_OK))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "enter program mode");
		return ERRCODE_FAILURE_ENTER_PROG_MODE;
	}
	return ERROR_OK;
}

static RESULT target_leave_progmode(struct program_context_t *context, 
									uint8_t success)
{
	const struct program_functions_t *pf = cur_target->program_functions;
	
	if (ERROR_OK != target_program_check(context))
	{
		return ERROR_FAIL;
	}
	
	// leave with success
	if ((pf->leave_program_mode != NULL) 
		&&(pf->leave_program_mode(context, success) != ERROR_OK))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "leave program mode");
		return ERRCODE_FAILURE_ENTER_PROG_MODE;
	}
	return ERROR_OK;
}

static RESULT target_program(struct program_context_t *context)
{
	const struct program_functions_t *pf = cur_target->program_functions;
	const struct program_area_map_t *p_map = cur_target->program_area_map;
	struct program_info_t *pi = context->pi;
	struct operation_t *op = context->op;
	struct chip_param_t *param = context->param;
	struct interfaces_info_t *prog = context->prog;
	
	struct chip_area_info_t *area_info;
	struct program_area_t *prog_area;
	RESULT ret = ERROR_OK;
	uint32_t i, j;
	int8_t area_idx;
	char area_char;
	uint32_t area_mask;
	enum area_attr_t area_attr;
	uint32_t target_size, page_size, start_addr;
	char *format = NULL;
	char format_tmp[32];
	uint8_t *tbuff;
	char *fullname, str_tmp[256];
	struct memlist **ml, *ml_tmp, *ml_exact = NULL;
	uint32_t time_in_ms = 1000;
	uint8_t special_string[256];
	
	if (ERROR_OK != target_program_check(context))
	{
		return ERROR_FAIL;
	}
	
	if (pf->execute != NULL)
	{
		ret = pf->execute(context);
		goto target_program_exit;
	}
	
	// read chip id
	pi->chip_id = 0;
	if (ERROR_OK != pf->read_target(context, CHIPID_CHAR, 0, 
									(uint8_t *)&pi->chip_id, 0))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read chip id");
		ret = ERRCODE_FAILURE_OPERATION;
		goto target_program_exit;
	}
	LOG_INFO(INFOMSG_TARGET_CHIP_ID, pi->chip_id);
	if (!(op->read_operations & CHIPID))
	{
		if (pi->chip_id != param->chip_id)
		{
			LOG_WARNING(ERRMSG_INVALID_CHIP_ID, pi->chip_id, param->chip_id);
		}
	}
	else
	{
		goto target_program_exit;
	}
	
	// chip erase
	if (op->erase_operations && param->chip_erase)
	{
		LOG_INFO(INFOMSG_ERASING, "chip");
		pgbar_init("erasing chip |", "|", 0, 1, PROGRESS_STEP, 
						PROGRESS_CHAR);
		
		if (ERROR_OK != pf->erase_target(context, ALL_CHAR, 0, 0))
		{
			pgbar_fini();
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "erase chip");
			ret = ERRCODE_FAILURE_OPERATION;
			goto target_program_exit;
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(INFOMSG_ERASED, "chip");
	}
	
	// erase, program, verify/read cycle
	i = 0;
	while (p_map[i].name != 0)
	{
		area_char = p_map[i].name;
		area_attr = p_map[i].attr;
		area_idx = target_area_idx(area_char);
		area_mask = target_area_mask(area_char);
		fullname = target_area_fullname(area_char);
		if (area_idx < 0)
		{
			// invalid area
			i++;
			continue;
		}
		
		area_info = &(param->chip_areas[area_idx]);
		page_size = area_info->page_size;
		start_addr = area_info->addr;
		if ((p_map[i].fpage_size > page_size) 
			&& ((p_map[i].fpage_size % page_size) == 0))
		{
			page_size = p_map[i].fpage_size;
		}
		
		prog_area = &(pi->program_areas[area_idx]);
		if (area_info->size > prog_area->size)
		{
			area_info->size = prog_area->size;
		}
		tbuff = prog_area->buff;
		if (p_map[i].data_pos)
		{
			ml = &(prog_area->memlist);
			ml_exact = prog_area->exact_memlist;
			target_size = MEMLIST_CalcAllSize(*ml);
		}
		else
		{
			ml = NULL;
			target_size = area_info->size;
			format = area_info->cli_format;
			if (NULL == format)
			{
				if (target_size > 8)
				{
					LOG_ERROR(ERRMSG_NOT_DEFINED, "cli_format");
					ret = ERROR_FAIL;
					goto target_program_exit;
				}
				// default type is hex value with 16-bit length
				snprintf(format_tmp, sizeof(format_tmp), "%%%dx", target_size);
				format = format_tmp;
			}
			if ((0 == target_size) && (SPECIAL_STRING_CHAR == area_char))
			{
				target_size = 1;
			}
		}
		if ((area_char != SPECIAL_STRING_CHAR) && !area_info->size)
		{
			i++;
			continue;
		}
		
		// not chip_erase, required to be erased, erasable
		// erase while write feature and write operation defined
		if (!param->chip_erase && (op->erase_operations & area_mask) 
			&& (area_attr & AREA_ATTR_E) 
			&& (!(area_attr & AREA_ATTR_EWW) 
				|| !(op->write_operations & area_mask)))
		{
			uint32_t page_num = 
				area_info->page_num > 0 ? area_info->page_num : 1;
			// target erase
			LOG_INFO(INFOMSG_ERASING, fullname);
			strcpy(str_tmp, "erasing ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, page_num, PROGRESS_STEP, PROGRESS_CHAR);
			
			if (area_attr & AREA_ATTR_EP)
			{
				// erase every page
				for (j = 0; j < area_info->page_num; j++)
				{
					if (ERROR_OK != pf->erase_target(context, area_char, 
							start_addr + j * page_size, page_size))
					{
						pgbar_fini();
						LOG_ERROR(ERRMSG_FAILURE_ERASE, fullname);
						ret = ERRCODE_FAILURE_OPERATION;
						goto target_program_exit;
					}
					pgbar_update(1);
				}
			}
			else
			{
				// erase all in one run
				if (ERROR_OK != pf->erase_target(context, area_char, 
													start_addr, 0))
				{
					pgbar_fini();
					LOG_ERROR(ERRMSG_FAILURE_ERASE, fullname);
					ret = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				pgbar_update(page_num);
			}
			if ((prog != NULL) && (ERROR_OK != prog->peripheral_commit()))
			{
				LOG_ERROR(ERRMSG_FAILURE_ERASE, fullname);
				ret = ERRCODE_FAILURE_OPERATION;
				goto target_program_exit;
			}
			
			pgbar_fini();
			LOG_INFO(INFOMSG_ERASED, fullname);
			
			// Reset After Erase
			if ((area_attr & AREA_ATTR_RAE) 
				&& ((op->checksum_operations != 0) 
					|| (op->read_operations != 0) 
					|| (op->verify_operations != 0) 
					|| (op->write_operations != 0)))
			{
				if ((pf->leave_program_mode != NULL) 
					&& (ERROR_OK != pf->leave_program_mode(context, 0)))
				{
					// no need to goto leave_program_mode here
					// operation failed IS leave_program_mode
					return ERRCODE_FAILURE_OPERATION;
				}
				sleep_ms(100);
				if (ERROR_OK != target_enter_progmode(context))
				{
					ret = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
			}
		}
		
		// required to program, writable
		if ((op->write_operations & area_mask) && (area_attr & AREA_ATTR_W))
		{
			LOG_INFO(INFOMSG_PROGRAMMING, fullname);
			strcpy(str_tmp, "writing ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, target_size, PROGRESS_STEP, '=');
			
			if ((tbuff != NULL) && (ml != NULL))
			{
				ml_tmp = *ml;
				while(ml_tmp != NULL)
				{
					if (area_attr & AREA_ATTR_WNP)
					{
						int32_t tmp_addr = ml_tmp->addr;
						uint8_t *tmp_buf = &(tbuff[tmp_addr - start_addr]);
						
						if (ERROR_OK != pf->write_target(context, area_char, 
								tmp_addr, tmp_buf, ml_tmp->len))
						{
							pgbar_fini();
							LOG_ERROR(ERRMSG_FAILURE_PROGRAM, fullname);
							ret = ERRCODE_FAILURE_OPERATION;
							goto target_program_exit;
						}
					}
					else
					{
						for (j = 0; j < ml_tmp->len; j += page_size)
						{
							uint32_t tmp_addr = ml_tmp->addr + j;
							uint8_t *tmp_buf = &(tbuff[tmp_addr - start_addr]);
							
							if (ERROR_OK != pf->write_target(context, 
									area_char, tmp_addr, tmp_buf, page_size))
							{
								pgbar_fini();
								LOG_ERROR(ERRMSG_FAILURE_PROGRAM, fullname);
								ret = ERRCODE_FAILURE_OPERATION;
								goto target_program_exit;
							}
							pgbar_update(page_size);
						}
					}
					ml_tmp = MEMLIST_GetNext(ml_tmp);
				}
			}
			else
			{
				uint8_t *buff_tmp;
				if (SPECIAL_STRING_CHAR == area_char)
				{
					if (NULL == prog_area->cli_str)
					{
						pgbar_fini();
						LOG_BUG(ERRMSG_INVALID_BUFFER, 
								TO_STR(prog_area->cli_str));
						ret = ERROR_FAIL;
						goto target_program_exit;
					}
					buff_tmp = (uint8_t *)prog_area->cli_str;
				}
				else if (tbuff != NULL)
				{
					buff_tmp = tbuff;
				}
				else
				{
					LOG_ERROR(ERRMSG_INVALID_BUFFER, "tbuff(prog_area->buff)");
					ret = ERROR_FAIL;
					goto target_program_exit;
				}
				if (ERROR_OK != pf->write_target(context, area_char, 
											start_addr, buff_tmp, target_size))
				{
					pgbar_fini();
					LOG_ERROR(ERRMSG_FAILURE_PROGRAM, fullname);
					ret = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				pgbar_update(target_size);
			}
			if ((prog != NULL) && (ERROR_OK != prog->peripheral_commit()))
			{
				LOG_ERROR(ERRMSG_FAILURE_PROGRAM, fullname);
				ret = ERRCODE_FAILURE_OPERATION;
				goto target_program_exit;
			}
			
			time_in_ms = pgbar_fini();
			LOG_INFO(INFOMSG_PROGRAMMED_SIZE, fullname, target_size, 
						(target_size / 1024.0) / (time_in_ms / 1000.0));
			
			// Reset After Write
			if ((area_attr & AREA_ATTR_RAW) 
				&& ((op->checksum_operations != 0) 
					|| (op->read_operations != 0) 
					|| (op->verify_operations != 0)))
			{
				if ((pf->leave_program_mode != NULL) 
					&& (ERROR_OK != pf->leave_program_mode(context, 0)))
				{
					// no need to goto leave_program_mode here
					// operation failed IS leave_program_mode
					return ERRCODE_FAILURE_OPERATION;
				}
				sleep_ms(100);
				if (ERROR_OK != target_enter_progmode(context))
				{
					ret = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
			}
		}
		
		if ((op->verify_operations & area_mask) 
			&& (area_attr & AREA_ATTR_V))
		{
			// specific verify defined by target
			LOG_INFO(INFOMSG_VERIFYING, fullname);
			strcpy(str_tmp, "verifying ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, target_size, PROGRESS_STEP, '=');
			
			if ((page_size == 0) || (area_attr & AREA_ATTR_RNP))
			{
				// verify whole target area
				if (ERROR_OK != pf->read_target(context, area_char, 
										start_addr, tbuff, target_size))
				{
					pgbar_fini();
					LOG_ERROR(ERRMSG_FAILURE_VERIFY, fullname);
					ret = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				
				pgbar_update(target_size);
			}
			else if ((tbuff != NULL) && (ml != NULL))
			{
				// verify target area page by page
				ml_tmp = *ml;
				while(ml_tmp != NULL)
				{
					for (j = 0; j < ml_tmp->len; j += page_size)
					{
						uint32_t tmp_addr = ml_tmp->addr + j;
						uint8_t *tmp_buf = &(tbuff[tmp_addr - start_addr]);
						
						if (ERROR_OK != pf->read_target(context, area_char, 
								tmp_addr, tmp_buf, page_size))
						{
							pgbar_fini();
							LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
							ret = ERRCODE_FAILURE_OPERATION;
							goto target_program_exit;
						}
						pgbar_update(page_size);
					}
					ml_tmp = MEMLIST_GetNext(ml_tmp);
				}
			}
			if ((prog != NULL) && (ERROR_OK != prog->peripheral_commit()))
			{
				LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
				ret = ERRCODE_FAILURE_OPERATION;
				goto target_program_exit;
			}
			
			pgbar_fini();
		}
		else if (((op->read_operations & area_mask) 
					|| (op->verify_operations & area_mask))
				&& (area_attr & AREA_ATTR_R))
		{
			if ((p_map[i].data_pos) && (op->read_operations & area_mask) && 
				(NULL == *ml))
			{
				if (ERROR_OK != MEMLIST_Add(ml, area_info->addr, 
											area_info->size, page_size))
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "add memory list");
					ret = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				target_size = MEMLIST_CalcAllSize(*ml);
			}
			
			if (op->verify_operations & area_mask)
			{
				LOG_INFO(INFOMSG_VERIFYING, fullname);
			}
			else
			{
				LOG_INFO(INFOMSG_READING, fullname);
			}
			strcpy(str_tmp, "reading ");
			strcat(str_tmp, fullname);
			strcat(str_tmp, " |");
			pgbar_init(str_tmp, "|", 0, target_size, PROGRESS_STEP, '=');
			
			if ((tbuff != NULL) && (ml != NULL))
			{
				ml_tmp = *ml;
				while(ml_tmp != NULL)
				{
					uint32_t read_offset = ml_tmp->addr - start_addr;
					uint8_t *read_buf = NULL;
					uint32_t buf_size;
					
					if (!(area_attr & AREA_ATTR_RNP) 
						&& (ml_tmp->len % page_size))
					{
						buf_size = ((ml_tmp->len / page_size) + 1) * page_size;
					}
					else
					{
						buf_size = ml_tmp->len;
					}
					read_buf = (uint8_t*)malloc(buf_size);
					if (NULL == read_buf)
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						ret = ERRCODE_NOT_ENOUGH_MEMORY;
						goto target_program_exit;
					}
					
					if (area_attr & AREA_ATTR_RNP)
					{
						if (ERROR_OK != pf->read_target(context, area_char, 
								ml_tmp->addr, read_buf, ml_tmp->len))
						{
							free(read_buf);
							read_buf = NULL;
							pgbar_fini();
							LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
							ret = ERRCODE_FAILURE_OPERATION;
							goto target_program_exit;
						}
					}
					else
					{
						for (j = 0; j < ml_tmp->len; j += page_size)
						{
							if (ERROR_OK != pf->read_target(context, 
									area_char, ml_tmp->addr + j, 
									read_buf + j, page_size))
							{
								free(read_buf);
								read_buf = NULL;
								pgbar_fini();
								LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
								ret = ERRCODE_FAILURE_OPERATION;
								goto target_program_exit;
							}
							
							pgbar_update(page_size);
						}
					}
					if ((prog != NULL) && 
						(ERROR_OK != prog->peripheral_commit()))
					{
						LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
						ret = ERRCODE_FAILURE_OPERATION;
						goto target_program_exit;
					}
					
					if (op->verify_operations & area_mask)
					{
						// verify according to ml_exact
						if (NULL == ml_exact)
						{
							LOG_BUG(ERRMSG_INVALID_BUFFER, "ml_exact");
							ret = ERROR_FAIL;
							goto target_program_exit;
						}
						j = 0;
						if (ERROR_OK != 
							MEMLIST_VerifyBuff(ml_exact, read_buf, 
												&tbuff[read_offset], 
												ml_tmp->addr, 
												ml_tmp->len, &j))
						{
							pgbar_fini();
							LOG_ERROR(ERRMSG_FAILURE_VERIFY_AT_02X, 
										fullname, ml_tmp->addr + j, 
										read_buf[j], tbuff[read_offset + j]);
							free(read_buf);
							read_buf = NULL;
							ret = ERRCODE_FAILURE_VERIFY;
							goto target_program_exit;
						}
					}
					else
					{
						memcpy(&tbuff[read_offset], read_buf, ml_tmp->len);
					}
					free(read_buf);
					read_buf = NULL;
					
					ml_tmp = MEMLIST_GetNext(ml_tmp);
				}
				time_in_ms = pgbar_fini();
			}
			else
			{
				uint8_t *buff_tmp;
				uint8_t alloced = 0;
				
				if (SPECIAL_STRING_CHAR == area_char)
				{
					buff_tmp = special_string;
				}
				else if (tbuff != NULL)
				{
					buff_tmp = tbuff;
				}
				else
				{
					buff_tmp = (uint8_t*)malloc(target_size);
					if (NULL == buff_tmp)
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						ret = ERRCODE_NOT_ENOUGH_MEMORY;
						goto target_program_exit;
					}
					memset(buff_tmp, 0, target_size);
					alloced = 1;
				}
				if (ERROR_OK != pf->read_target(context, area_char, 
							start_addr, buff_tmp, target_size))
				{
					pgbar_fini();
					LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
					ret = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				if ((prog != NULL) && (ERROR_OK != prog->peripheral_commit()))
				{
					LOG_ERROR(ERRMSG_FAILURE_READ, fullname);
					ret = ERRCODE_FAILURE_OPERATION;
					goto target_program_exit;
				}
				pgbar_update(target_size);
				time_in_ms = pgbar_fini();
				
				if (op->verify_operations & area_mask)
				{
					if (SPECIAL_STRING_CHAR == area_char)
					{
						if (!strcmp((const char*)prog_area->cli_str, 
									(const char*)special_string))
						{
							LOG_INFO(INFOMSG_VERIFIED, fullname);
						}
						else
						{
							LOG_ERROR(ERRMSG_FAILURE_VERIFY_STR, 
								fullname, special_string, prog_area->cli_str);
							ret = ERRCODE_FAILURE_VERIFY;
							goto target_program_exit;
						}
					}
					else
					{
						if (!memcmp(buff_tmp, tbuff, target_size))
						{
							LOG_INFO(INFOMSG_VERIFIED, fullname);
						}
						else
						{
							char *read_str, *want_str;
							if (NULL == format)
							{
								LOG_BUG(ERRMSG_INVALID_BUFFER, "format");
								ret = ERROR_FAIL;
								goto target_program_exit;
							}
							read_str = strparser_solve(format, buff_tmp, 0);
							if (NULL == read_str)
							{
								LOG_ERROR(ERRMSG_FAILURE_OPERATION, 
											"solve value");
								ret = ERRCODE_FAILURE_OPERATION;
								goto target_program_exit;
							}
							want_str = strparser_solve(format, tbuff, 0);
							if (NULL == want_str)
							{
								free(read_str);
								read_str = NULL;
								LOG_ERROR(ERRMSG_FAILURE_OPERATION, 
											"solve value");
								ret = ERRCODE_FAILURE_OPERATION;
								goto target_program_exit;
							}
							LOG_ERROR(ERRMSG_FAILURE_VERIFY_STR, 
								fullname, read_str, want_str);
							free(read_str);
							read_str = NULL;
							free(want_str);
							want_str = NULL;
							ret = ERRCODE_FAILURE_VERIFY;
							goto target_program_exit;
						}
					}
				}
				else
				{
					if (SPECIAL_STRING_CHAR == area_char)
					{
						LOG_INFO(INFOMSG_TARGET_READ, fullname, special_string);
					}
					else
					{
						char *read_str;
						if (NULL == format)
						{
							LOG_BUG(ERRMSG_INVALID_BUFFER, "format");
							ret = ERROR_FAIL;
							goto target_program_exit;
						}
						read_str = strparser_solve(format, buff_tmp, 0);
						if (NULL == read_str)
						{
							LOG_ERROR(ERRMSG_FAILURE_OPERATION, "solve value");
							ret = ERRCODE_FAILURE_OPERATION;
							goto target_program_exit;
						}
						LOG_INFO(INFOMSG_TARGET_READ, fullname, read_str);
						free(read_str);
						read_str = NULL;
					}
				}
				if (alloced && (buff_tmp != NULL))
				{
					free(buff_tmp);
					buff_tmp = NULL;
				}
			}
			
			if (op->verify_operations & area_mask)
			{
				LOG_INFO(INFOMSG_VERIFIED_SIZE, fullname, target_size, 
							(target_size / 1024.0) / (time_in_ms / 1000.0));
			}
			else
			{
				LOG_INFO(INFOMSG_READ_SIZE, fullname, target_size, 
							(target_size / 1024.0) / (time_in_ms / 1000.0));
			}
		}
		i++;
	}
	
target_program_exit:
	return ret;
}

static RESULT target_init(struct program_info_t *pi)
{
	uint16_t i;
	uint8_t area_idx;
	char mode_buff[4];
	
	LOG_PUSH();
	LOG_MUTE();
	sprintf(mode_buff, "%d", pi->mode);
	vss_call_notifier(cur_target->notifier, "mode", mode_buff);
	LOG_POP();
	
	if (NULL == pi->chip_name)
	{
		if (strchr(cur_target->feature, NO_TARGET[0]) != NULL)
		{
			return ERROR_OK;
		}
		else if (NULL == strchr(cur_target->feature, AUTO_DETECT[0]))
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, "Auto-detect", 
						cur_target->name);
			return ERRCODE_NOT_SUPPORT;
		}
		// auto detect
		memcpy(&target_chip_param, &target_chips.chips_param[0], 
					sizeof(target_chip_param));
		LOG_INFO(INFOMSG_TRY_AUTODETECT);
		
		if (target_chips.num_of_chips > 1)
		{
			struct program_context_t context;
			struct operation_t opt_tmp;
			
			memset(&opt_tmp, 0, sizeof(opt_tmp));
			opt_tmp.read_operations = CHIPID;
			context.op = &opt_tmp;
			context.param = &target_chip_param;
			context.pi = pi;
			context.prog = cur_interface;
			if (ERROR_OK != target_enter_progmode(&context))
			{
				LOG_ERROR(ERRMSG_AUTODETECT_FAIL, pi->chip_type);
				return ERRCODE_AUTODETECT_FAIL;
			}
			if (ERROR_OK != target_program(&context))
			{
				target_leave_progmode(&context, 0);
				LOG_ERROR(ERRMSG_AUTODETECT_FAIL, pi->chip_type);
				return ERRCODE_AUTODETECT_FAIL;
			}
			if (ERROR_OK != target_leave_progmode(&context, 1))
			{
				LOG_ERROR(ERRMSG_AUTODETECT_FAIL, pi->chip_type);
				return ERRCODE_AUTODETECT_FAIL;
			}
			
			// insert a dly between 2 operations
			sleep_ms(100);
			
			LOG_INFO(INFOMSG_AUTODETECT_SIGNATURE, pi->chip_id);
			for (i = 0; i < target_chips.num_of_chips; i++)
			{
				if (pi->chip_id == target_chips.chips_param[i].chip_id)
				{
					if (NULL == bufffunc_malloc_and_copy_str(&pi->chip_name, 
									target_chips.chips_param[i].chip_name))
					{
						LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
						return ERRCODE_NOT_ENOUGH_MEMORY;
					}
					LOG_INFO(INFOMSG_CHIP_FOUND, pi->chip_name);
					
					goto Post_Init;
				}
			}
		}
		else
		{
			i = 0;
			goto Post_Init;
		}
		
		LOG_ERROR(ERRMSG_AUTODETECT_FAIL, pi->chip_type);
		return ERRCODE_AUTODETECT_FAIL;
	}
	else
	{
		for (i = 0; i < target_chips.num_of_chips; i++)
		{
			if (!strcmp(target_chips.chips_param[i].chip_name, pi->chip_name))
			{
				goto Post_Init;
			}
		}
		
		return ERROR_FAIL;
	}
Post_Init:
	if ((cur_target->adjust_setting != NULL) 
		&&(ERROR_OK != cur_target->adjust_setting(&program_info, 
							&target_chips.chips_param[i], program_info.mode)))
	{
		return ERROR_FAIL;
	}
	memcpy(&target_chip_param, &target_chips.chips_param[i], 
			sizeof(target_chip_param));
	
	i = 0;
	while (cur_target->program_area_map[i].name != 0)
	{
		area_idx = target_area_idx(cur_target->program_area_map[i].name);
		if (!pi->program_areas[area_idx].size)
		{
			// if size is not detected, copy from record in xml config
			pi->program_areas[area_idx].size = 
								target_chip_param.chip_areas[area_idx].size;
		}
		else
		{
			// if size is detected, overwrite target parameters
			target_chip_param.chip_areas[area_idx].size = 
								pi->program_areas[area_idx].size;
		}
		i++;
	}
	
	return ERROR_OK;
}

static uint32_t target_prepare_operation(uint32_t *operation)
{
	uint32_t i;
	uint32_t ret;
	struct program_area_map_t *a;
	
	a = (struct program_area_map_t *)cur_target->program_area_map;
	if (*operation & ALL)
	{
		i = 0;
		while (a[i].name != 0)
		{
			*operation |= target_area_mask(a[i].name);
			i++;
		}
	}
	
	ret = 0;
	i = 0;
	while (a[i].name != 0)
	{
		if (*operation & target_area_mask(a[i].name))
		{
			ret += a[i].data_pos;
		}
		i++;
	}
	return ret;
}

static RESULT target_prepare_operations(struct operation_t *operations, 
									uint32_t *readfile, uint32_t *writefile)
{
	if ((NULL == cur_target) || (NULL == cur_target->program_area_map) 
		|| (NULL == operations))
	{
		return ERROR_FAIL;
	}
	
	*readfile = *writefile = 0;
	target_prepare_operation(&operations->erase_operations);
	*readfile += target_prepare_operation(&operations->write_operations);
	*readfile += target_prepare_operation(&operations->verify_operations);
	*writefile += target_prepare_operation(&operations->read_operations);
	
	return ERROR_OK;
}

static void target_print_single_memory(char type)
{
	uint32_t mapidx;
	int8_t paramidx;
	char *full_type = target_area_fullname(type);
	struct program_area_map_t *p_map;
	
	p_map = (struct program_area_map_t *)cur_target->program_area_map;
	mapidx = 0;
	while ((p_map[mapidx].name != 0) && (p_map[mapidx].name != type))
	{
		mapidx++;
	}
	if (0 == p_map[mapidx].name)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, full_type, 
					program_info.chip_name);
		return;
	}
	
	paramidx = target_area_idx(type);
	if (paramidx < 0 )
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, full_type, 
					program_info.chip_name);
		return;
	}
	
	PRINTF("%s of %s:\n", full_type, program_info.chip_name);
	if (p_map[mapidx].data_pos)
	{
		PRINTF("%c_seg = 0x%08X, ", type, 
				target_chip_param.chip_areas[paramidx].seg);
		PRINTF("%c_addr = 0x%08X, ", type, 
				target_chip_param.chip_areas[paramidx].addr);
	}
	else if (target_chip_param.chip_areas[paramidx].cli_format != NULL)
	{
		PRINTF("%c_format = %s, ", type, 
				target_chip_param.chip_areas[paramidx].cli_format);
	}
	PRINTF("%c_default = 0x%"PRIX64", ", type, 
				target_chip_param.chip_areas[paramidx].default_value);
	PRINTF("%c_bytelen = %d\n", type, 
				target_chip_param.chip_areas[paramidx].size);
}

void target_print_memory(char type)
{
	uint8_t i;
	
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return;
	}
	
	if (type > 0)
	{
		target_print_single_memory(type);
	}
	else
	{
		i = 0;
		while (cur_target->program_area_map[i].name != 0)
		{
			target_print_single_memory(cur_target->program_area_map[i].name);
			i++;
		}
	}
}

void target_print_setting(char type)
{
	struct chip_fl_t fl;
	uint32_t i, j;
	char *full_type = target_area_fullname(type);
	struct program_area_map_t *p_map;
	
	if (NULL == full_type)
	{
		LOG_BUG(ERRMSG_INVALID_TARGET, "target");
		return;
	}
	
	p_map = (struct program_area_map_t *)cur_target->program_area_map;
	i = 0;
	while ((p_map[i].name != 0) && (p_map[i].name != type))
	{
		i++;
	}
	if (0 == p_map[i].name)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, full_type, program_info.chip_name);
		return;
	}
	
	memset(&fl, 0, sizeof(struct chip_fl_t));
	if (ERROR_OK != target_build_chip_fl(program_info.chip_type, 
									program_info.chip_name, full_type, &fl))
	{
		target_release_chip_fl(&fl);
		LOG_ERROR(ERRMSG_INVALID_XML_SETTING, program_info.chip_name);
		return;
	}
	
	// print fl
	PRINTF("%s of %s:\n", full_type, program_info.chip_name);
	PRINTF("init = %s, ", fl.init_value);
	PRINTF("num_of_warnings = %d, ", fl.num_of_fl_warnings);
	PRINTF("num_of_settings = %d\n", fl.num_of_fl_settings);
	for (i = 0; i < fl.num_of_fl_warnings; i++)
	{
		PRINTF("warning: mask = %s, ", fl.warnings[i].mask);
		PRINTF("value = %s, ", fl.warnings[i].value);
		PRINTF("msg = %s, ", fl.warnings[i].msg);
		PRINTF("ban = %d\n", fl.warnings[i].ban);
	}
	for (i = 0; i < fl.num_of_fl_settings; i++)
	{
		PRINTF("setting: name = %s, ", fl.settings[i].name);
		PRINTF("mask = %s, ", fl.settings[i].mask);
		PRINTF("num_of_choices = %d", fl.settings[i].num_of_choices);
		if (fl.settings[i].ban != NULL)
		{
			PRINTF(", ban = %s", fl.settings[i].ban);
		}
		if (fl.settings[i].info != NULL)
		{
			PRINTF(", info = %s", fl.settings[i].info);
		}
		if (fl.settings[i].format != NULL)
		{
			PRINTF(", format = %s", fl.settings[i].format);
		}
		if (fl.settings[i].use_checkbox)
		{
			PRINTF(", checked = %s", fl.settings[i].checked);
			PRINTF(", unchecked = %s", fl.settings[i].unchecked);
		}
		else if (fl.settings[i].use_edit)
		{
			PRINTF(", radix = %d", fl.settings[i].radix);
			PRINTF(", shift = %d", fl.settings[i].shift);
			PRINTF(", bytelen = %d", fl.settings[i].bytelen);
		}
		PRINTF("\n");
		for (j = 0; j < fl.settings[i].num_of_choices; j++)
		{
			PRINTF("choice: value = %s, ", fl.settings[i].choices[j].value);
			PRINTF("text = %s\n", fl.settings[i].choices[j].text);
		}
	}
	
	target_release_chip_fl(&fl);
}

void target_print_target(uint32_t index)
{
	uint32_t i, j;
	struct chip_param_t *p_param;
	struct program_area_map_t *p_map;
	char area[3];
	
	if (ERROR_OK != target_build_chip_series(targets_info[index].name, 
						targets_info[index].program_mode, &target_chips))
	{
		target_release_chip_series(&target_chips);
		LOG_ERROR(ERRMSG_INVALID_XML_SETTING, targets_info[index].name);
		return;
	}
	
	if (0 == target_chips.num_of_chips)
	{
		return;
	}
	
	if (strlen(targets_info[index].feature) > 0)
	{
		PRINTF("Support list of %s(%s):", targets_info[index].name, 
				targets_info[index].feature);
	}
	else
	{
		PRINTF("Support list of %s:", targets_info[index].name);
	}
	// fake
	p_map = (struct program_area_map_t *)targets_info[index].program_area_map;
	i = 0;
	while (p_map[i].name != 0)
	{
		if (p_map[i].fseg_addr)
		{
			PRINTF(" %c_fseg = 0x%X,", p_map[i].name, p_map[i].fseg_addr);
		}
		if (p_map[i].fstart_addr)
		{
			PRINTF(" %c_faddr = 0x%X,", p_map[i].name, p_map[i].fstart_addr);
		}
		i++;
	}
	// extra info from target
	if (ERROR_OK == 
		vss_cmd_supported_by_notifier(targets_info[index].notifier, "extra"))
	{
		vss_call_notifier(targets_info[index].notifier, "extra", NULL);
	}
	PRINTF("\n");
	
	// Targets based on ComPort outputs there special COM settings
	if (strchr(targets_info[index].feature, 'C') != NULL)
	{
		vss_call_notifier(targets_info[index].notifier, "support", NULL);
	}
	else
	{
		for (i = 0; i < target_chips.num_of_chips; i++)
		{
			p_param = &target_chips.chips_param[i];
			
			// name
			PRINTF("%s:", p_param->chip_name);
			// id
			PRINTF(" id = 0x%X,", p_param->chip_id);
			// mode
			if (p_param->program_mode_str != NULL)
			{
				PRINTF(" mode = %s,", p_param->program_mode_str);
			}
			// area
			PRINTF(" area = ");
			area[2] = 0;
			j = 0;
			while (p_map[j].name != 0)
			{
				area[0] = p_map[j].name;
				area[1] = p_map[j].data_pos + '0';
				PRINTF("%s", area);
				j++;
			}
			PRINTF("\n");
		}
		PRINTF("\n");
	}
	
	target_release_chip_series(&target_chips);
}

void target_print_list(void)
{
	uint32_t i;
	
	PRINTF(_GETTEXT("Supported targets:\n"));
	for (i = 0; targets_info[i].name != NULL; i++)
	{
		target_print_target(i);
	}
}

void target_print_help(void)
{
	uint32_t i;
	
	for (i = 0; targets_info[i].name != NULL; i++)
	{
		vss_call_notifier(targets_info[i].notifier, "help", NULL);
	}
}

static RESULT target_probe_chip(char *chip_name)
{
	uint32_t i;
	
	if (NULL == chip_name)
	{
		return ERROR_FAIL;
	}
	
	for (i = 0; i < target_chips.num_of_chips; i++)
	{
		if (NULL == target_chips.chips_param[i].chip_name)
		{
			continue;
		}
		
		if (!strcmp(target_chips.chips_param[i].chip_name, chip_name))
		{
			return ERROR_OK;
		}
	}
	
	return ERROR_FAIL;
}

static RESULT target_info_init(struct program_info_t *pi)
{
	uint32_t i;
	RESULT (*probe_chip)(char *chip_name);
	
#if PARAM_CHECK
	if ((NULL == pi) || ((NULL == pi->chip_name) && (NULL == pi->chip_type)))
	{
		LOG_BUG(ERRMSG_INVALID_PARAMETER, __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	target_release_chip_series(&target_chips);
	
	if (NULL == pi->chip_type)
	{
		// find which series of target contain current chip_name
		for (i = 0; targets_info[i].name != NULL; i++)
		{
			if (ERROR_OK == target_build_chip_series(targets_info[i].name, 
								targets_info[i].program_mode, &target_chips))
			{
				// configuration file exists, use default probe function
				probe_chip = target_probe_chip;
			}
			else
			{
				// use probe function defined by target chip
				continue;
			}
			
			if (probe_chip(pi->chip_name) == ERROR_OK)
			{
				cur_target = &targets_info[i];
				if (NULL == bufffunc_malloc_and_copy_str(&pi->chip_type, 
									(char *)targets_info[i].name))
				{
					LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
					return ERRCODE_NOT_ENOUGH_MEMORY;
				}
				LOG_DEBUG("%s initialized for %s.", cur_target->name, 
							pi->chip_name);
				
				return ERROR_OK;
			}
			target_release_chip_series(&target_chips);
		}
		
		LOG_ERROR(ERRMSG_NOT_SUPPORT, pi->chip_name);
	}
	else
	{
		// find current series of chip_type
		for (i = 0; targets_info[i].name != NULL; i++)
		{
			if (!strcmp(targets_info[i].name, pi->chip_type))
			{
				if (ERROR_OK == target_build_chip_series(targets_info[i].name, 
								targets_info[i].program_mode, &target_chips))
				{
					// configuration file exists, use default probe function
					probe_chip = target_probe_chip;
				}
				else if (strchr(targets_info[i].feature, NO_TARGET[0]) != NULL)
				{
					cur_target = &targets_info[i];
					return ERROR_OK;
				}
				else
				{
					LOG_BUG(ERRMSG_NOT_SUPPORT_BY, "probe_chip", 
							targets_info[i].name);
					return ERROR_FAIL;
				}
				
				if ((pi->chip_name != NULL) 
				   && (ERROR_OK != probe_chip(pi->chip_name)))
				{
					LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, pi->chip_name, 
								targets_info[i].name);
					target_release_chip_series(&target_chips);
					cur_target = NULL;
					return ERRCODE_NOT_SUPPORT;
				}
				else
				{
					cur_target = &targets_info[i];
					LOG_DEBUG("%s initialized.", cur_target->name);
					return ERROR_OK;
				}
			}
		}
		
		LOG_ERROR(ERRMSG_NOT_SUPPORT, pi->chip_type);
	}
	
	cur_target = NULL;
	return ERRCODE_NOT_SUPPORT;
}

VSS_HANDLER(target_memory_detail)
{
	char target_char;
	
	vsprog_no_call_operate();
	VSS_CHECK_ARGC(2);
	
	if (((NULL == program_info.chip_name) || (NULL == cur_target))
		&& (NULL == program_info.chip_type))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	if (((NULL == program_info.chip_name) || (NULL == cur_target))
		&& (NULL != program_info.chip_type))
	{
		program_info.chip_name = program_info.chip_type;
		target_info_init(&program_info);
		if (NULL == cur_target)
		{
			LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
			return ERROR_FAIL;
		}
	}
	
	if (!strcmp(argv[1], "all"))
	{
		target_char = 0;
	}
	else
	{
		target_char = target_area_char_by_fullname((char *)argv[1]);
		if (0 == target_char)
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
			return ERROR_FAIL;
		}
	}
	
	if (ERROR_OK != target_init(&program_info))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize target");
		return ERROR_FAIL;
	}
	
	target_print_memory(target_char);
	return ERROR_OK;
}

VSS_HANDLER(target_parameter_detail)
{
	char target_char;
	
	vsprog_no_call_operate();
	VSS_CHECK_ARGC(2);
	
	if ((NULL == program_info.chip_name) 
		|| (NULL == program_info.chip_type) 
		|| (NULL == cur_target))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	
	if (strlen(argv[1]) > 1)
	{
		target_char = target_area_char_by_fullname((char *)argv[1]);
		if (0 == target_char)
		{
			LOG_ERROR(ERRMSG_NOT_SUPPORT, argv[1]);
			return ERROR_FAIL;
		}
	}
	else
	{
		target_char = argv[1][0];
	}
	
	if (ERROR_OK != target_init(&program_info))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize target");
		return ERROR_FAIL;
	}
	
	target_print_setting(target_char);
	return ERROR_OK;
}

VSS_HANDLER(target_series)
{
	VSS_CHECK_ARGC(2);
	
	if (program_info.chip_type != NULL)
	{
		free(program_info.chip_type);
		program_info.chip_type = NULL;
	}
	if (NULL == bufffunc_malloc_and_copy_str(&program_info.chip_type, 
												(char *)argv[1]))
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	
	if (ERROR_OK != target_info_init(&program_info))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "initialize target: ", argv[1]);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

VSS_HANDLER(target_chip)
{
	VSS_CHECK_ARGC(2);
	
	if (program_info.chip_name != NULL)
	{
		free(program_info.chip_name);
		program_info.chip_name = NULL;
	}
	if (NULL == bufffunc_malloc_and_copy_str(&program_info.chip_name, 
												(char *)argv[1]))
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERRCODE_NOT_ENOUGH_MEMORY;
	}
	
	if (ERROR_OK != target_info_init(&program_info))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "initialize target: ", argv[1]);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

VSS_HANDLER(target_value)
{
	char *dest;
	int8_t target_idx;
	
	VSS_CHECK_ARGC(2);
	
	if (strlen(argv[1]) < 2)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		vss_print_help(argv[0]);
		return ERROR_FAIL;
	}
	target_idx = (int)target_area_idx(argv[1][0]);
	if (target_idx < 0)
	{
		LOG_ERROR(ERRMSG_INVALID_CHARACTER, argv[1][0], "target");
		return ERROR_FAIL;
	}
	dest = NULL;
	if (NULL == bufffunc_malloc_and_copy_str(&dest, (char *)&argv[1][1]))
	{
		return ERROR_FAIL;
	}
	if (NULL != program_info.program_areas[target_idx].cli_str)
	{
		free(program_info.program_areas[target_idx].cli_str);
		program_info.program_areas[target_idx].cli_str = NULL;
	}
	program_info.program_areas[target_idx].cli_str = dest;
	program_info.areas_defined |= target_area_name[target_idx].mask;
	return ERROR_OK;
}

VSS_HANDLER(target_interface_frequency)
{
	VSS_CHECK_ARGC(2);
	program_info.frequency = (uint16_t)strtoul(argv[1], NULL, 0);
	return ERROR_OK;
}

VSS_HANDLER(target_kernel_khz)
{
	VSS_CHECK_ARGC(2);
	program_info.kernel_khz = (uint32_t)strtoul(argv[1], NULL, 0);
	return ERROR_OK;
}

VSS_HANDLER(target_quartz_khz)
{
	VSS_CHECK_ARGC(2);
	program_info.quartz_khz = (uint32_t)strtoul(argv[1], NULL, 0);
	return ERROR_OK;
}

VSS_HANDLER(target_erase_on_demand)
{
	uint32_t tmp;
	
	VSS_CHECK_ARGC_2(1, 2);
	
	if (1 == argc)
	{
		program_info.erase_on_demand = true;
	}
	else
	{
		tmp = strtoul(argv[1], NULL, 0);
		if (tmp)
		{
			program_info.erase_on_demand = true;
		}
		else
		{
			program_info.erase_on_demand = false;
		}
	}
	return ERROR_OK;
}

VSS_HANDLER(target_wait_state)
{
	VSS_CHECK_ARGC(2);
	program_info.wait_state = (uint8_t)strtoul(argv[1], NULL, 0);
	return ERROR_OK;
}

VSS_HANDLER(target_auto_adjust)
{
	VSS_CHECK_ARGC(1);
	program_info.auto_adjust = 1;
	return ERROR_OK;
}

VSS_HANDLER(target_jtag_dc)
{
	// UB(1) UA(1) BB(2) BA(2)
	uint8_t buff[6];
	char format[] = "%1d%1d%2d%2d";
	
	VSS_CHECK_ARGC(2);
	if (ERROR_OK != 
		strparser_parse((char *)argv[1], format, buff, sizeof(buff)))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_AS, argv[1], format);
		return ERROR_FAIL;
	}
	program_info.jtag_pos.ub = buff[0];
	program_info.jtag_pos.ua = buff[1];
	program_info.jtag_pos.bb = buff[2] + (buff[3] << 8);
	program_info.jtag_pos.ba = buff[4] + (buff[5] << 8);
	return ERROR_OK;
}

VSS_HANDLER(target_interface_mode)
{
	int8_t mode;
	
	VSS_CHECK_ARGC(2);
	
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "target");
		return ERROR_FAIL;
	}
	if (strlen(argv[1]) != 1)
	{
		LOG_ERROR(ERRMSG_INVALID_CMD, argv[0]);
		vss_print_help(argv[0]);
		return ERROR_FAIL;
	}
	mode = target_mode_get_idx(cur_target->program_mode, argv[1][0]);
	if (mode < 0)
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT_BY, argv[1], cur_target->name);
		return ERROR_FAIL;
	}
	
	program_info.mode = (uint8_t)mode;
	return ERROR_OK;
}

VSS_HANDLER(target_execute_addr)
{
	VSS_CHECK_ARGC_2(1, 2);
	
	if (1 == argc)
	{
		program_info.execute_addr = 0;
		program_info.execute_flag = 0;
	}
	else
	{
		program_info.execute_addr = (uint32_t)strtoul(argv[1], NULL, 0);
		program_info.execute_flag = 1;
	}
	
	return ERROR_OK;
}

#include "filelist.h"
#include "fileparser.h"

extern struct operation_t operations;
extern struct filelist *fl_in, *fl_out;
VSS_HANDLER(target_prepare)
{
	RESULT ret = ERROR_OK;
	uint32_t require_hex_file_for_read = 0;
	uint32_t require_hex_file_for_write = 0;
	
	VSS_CHECK_ARGC(1);
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	
	// check file
	target_prepare_operations(&operations, 
					&require_hex_file_for_read, &require_hex_file_for_write);
	if ((require_hex_file_for_read > 0) 
		&& ((NULL == fl_in) || (NULL == fl_in->path) || (NULL == fl_in->file)))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "input file");
		return ERROR_FAIL;
	}
	if ((require_hex_file_for_write > 0) 
		&& ((NULL == fl_out) || (NULL == fl_out->path)))
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "output file");
		return ERROR_FAIL;
	}
	
	// init target
	ret = target_init(&program_info);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "initialize target");
		return ERROR_FAIL;
	}
	
	// malloc buffer
	ret = target_alloc_data_buffer();
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_NOT_ENOUGH_MEMORY);
		return ERROR_FAIL;
	}
	ret = target_parse_cli_string();
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "parse cli_string");
		return ERROR_FAIL;
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
				LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "parse input file", 
							fl->path);
				return ERROR_FAIL;
			}
			
			fl = FILELIST_GetNext(fl);
		}
	}
	if (ERROR_OK != target_check_defined(operations))
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "check target defined content");
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

VSS_HANDLER(target_enter_program_mode)
{
	struct program_context_t context;
	RESULT ret;
	
	VSS_CHECK_ARGC(1);
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	
	context.op = &operations;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = cur_interface;
	ret = target_enter_progmode(&context);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_target->name);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

VSS_HANDLER(target_leave_program_mode)
{
	struct program_context_t context;
	uint8_t success;
	RESULT ret;
	
	VSS_CHECK_ARGC(2);
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	success = (uint8_t)strtoul(argv[1], NULL, 0);
	
	context.op = &operations;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = cur_interface;
	ret = target_leave_progmode(&context, success);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_target->name);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

VSS_HANDLER(target_erase)
{
	struct operation_t operations;
	struct program_context_t context;
	RESULT ret;
	int8_t index;
	
	VSS_CHECK_ARGC_2(1, 2);
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	
	memset(&operations, 0, sizeof(operations));
	if ((1 == argc) || (argv[1][0] == ALL_CHAR))
	{
		operations.erase_operations = 0xFFFFFFFF;
	}
	else
	{
		index = target_area_idx_by_name((char *)argv[1]);
		if (index < 0)
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "target area");
			return ERROR_FAIL;
		}
		memset(&operations, 0, sizeof(operations));
		operations.erase_operations = target_area_name[index].mask;
	}
	
	context.op = &operations;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = cur_interface;
	ret = target_program(&context);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_target->name);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

VSS_HANDLER(target_write)
{
	struct operation_t operations_tmp;
	struct program_context_t context;
	RESULT ret;
	int8_t index;
	
	VSS_CHECK_ARGC_2(1, 2);
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	
	operations_tmp = operations;
	memset(&operations, 0, sizeof(operations));
	if ((1 == argc) || (argv[1][0] == ALL_CHAR))
	{
		operations.write_operations = 0xFFFFFFFF;
	}
	else
	{
		index = target_area_idx_by_name((char *)argv[1]);
		if (index < 0)
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "target area");
			return ERROR_FAIL;
		}
		memset(&operations, 0, sizeof(operations));
		operations.write_operations = target_area_name[index].mask;
	}
	
	context.op = &operations;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = cur_interface;
	ret = target_program(&context);
	operations = operations_tmp;
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_target->name);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

VSS_HANDLER(target_verify)
{
	struct operation_t operations_tmp;
	struct program_context_t context;
	RESULT ret;
	int8_t index;
	
	VSS_CHECK_ARGC_2(1, 2);
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	
	operations_tmp = operations;
	memset(&operations, 0, sizeof(operations));
	if ((1 == argc) || (argv[1][0] == ALL_CHAR))
	{
		operations.verify_operations = 0xFFFFFFFF;
	}
	else
	{
		index = target_area_idx_by_name((char *)argv[1]);
		if (index < 0)
		{
			LOG_ERROR(ERRMSG_INVALID_TARGET, "target area");
			return ERROR_FAIL;
		}
		memset(&operations, 0, sizeof(operations));
		operations.verify_operations = target_area_name[index].mask;
	}
	
	context.op = &operations;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = cur_interface;
	ret = target_program(&context);
	operations = operations_tmp;
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_target->name);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

VSS_HANDLER(target_read)
{
	struct operation_t operations_tmp;
	struct program_context_t context;
	struct memlist *ml_tmp = NULL, *pml_save = NULL;
	RESULT ret;
	uint32_t byteaddr, bytesize;
	int8_t index;
	
	VSS_CHECK_ARGC(4);
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	
	index = target_area_idx_by_name((char *)argv[1]);
	if (index < 0)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "target area");
		return ERROR_FAIL;
	}
	
	operations_tmp = operations;
	memset(&operations, 0, sizeof(operations));
	operations.read_operations = target_area_name[index].mask;
	
	byteaddr = strtoul(argv[2], NULL, 0);
	bytesize = strtoul(argv[3], NULL, 0);
	
	if (ERROR_OK != MEMLIST_Add(&ml_tmp, byteaddr, bytesize, 
			target_chip_param.chip_areas[index].page_size))
	{
		return ERROR_FAIL;
	}
	pml_save = program_info.program_areas[index].memlist;
	program_info.program_areas[index].memlist = ml_tmp;
	
	context.op = &operations;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = cur_interface;
	ret = target_program(&context);
	program_info.program_areas[index].memlist = pml_save;
	MEMLIST_Free(&ml_tmp);
	operations = operations_tmp;
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_target->name);
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

VSS_HANDLER(target_operate)
{
	RESULT ret = ERROR_OK;
	struct program_context_t context;
	
	VSS_CHECK_ARGC(1);
	if (NULL == cur_target)
	{
		LOG_ERROR(ERRMSG_NOT_DEFINED, "Target");
		return ERROR_FAIL;
	}
	
	// in system programmer
	if ((!(operations.checksum_operations || operations.erase_operations 
			|| operations.read_operations || operations.verify_operations 
			|| operations.write_operations)) 
		&& (NULL == strchr(cur_target->feature, NO_TARGET[0])))
	{
		// no operation defined
		// and not no_target operation
		return ERROR_OK;
	}
	
	context.op = &operations;
	context.param = &target_chip_param;
	context.pi = &program_info;
	context.prog = cur_interface;
	ret = target_program(&context);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATE_DEVICE, cur_target->name);
		return ERROR_FAIL;
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
							p_map->fstart_addr, 
							cur_target->adjust_mapping))
					{
						LOG_ERROR(ERRMSG_FAILURE_OPERATION, 
									"write data to file");
						return ERROR_FAIL;
					}
				}
			}
			
			p_map++;
		}
		end_file(fl_out);
	}
	return ERROR_OK;
}

