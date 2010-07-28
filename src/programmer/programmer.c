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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_log.h"
#include "app_err.h"

#include "scripts.h"
#include "prog_interface.h"
#include "programmer.h"
#include "versaloon/versaloon.h"

#include "timer.h"

#define PROGRAMMER_DEFAULT				0

struct programmer_info_t programmers_info[] = 
{
	// versaloon
	PROGRAMMER_DEFINE(VERSALOON_STRING, 			// name
					  versaloon_check_argument, 	// check_argument
					  versaloon_init_capability, 	// init_capability
					  versaloon_display_programmer),// display_programmer
	PROGRAMMER_DEFINE(NULL, NULL, NULL, NULL)
};

struct programmer_info_t *cur_programmer = NULL;

struct misc_param_t programmer_param[] =
{
	{
		NULL,
		NULL,
		0
	}
};

RESULT programmer_get_target_voltage(uint16_t argc, const char *argv[]);
RESULT programmer_set_target_voltage(uint16_t argc, const char *argv[]);

RESULT programmer_iic_init(uint16_t argc, const char *argv[]);
RESULT programmer_iic_fini(uint16_t argc, const char *argv[]);
RESULT programmer_iic_config(uint16_t argc, const char *argv[]);
RESULT programmer_iic_read(uint16_t argc, const char *argv[]);
RESULT programmer_iic_write(uint16_t argc, const char *argv[]);

RESULT programmer_gpio_init(uint16_t argc, const char *argv[]);
RESULT programmer_gpio_fini(uint16_t argc, const char *argv[]);
RESULT programmer_gpio_config(uint16_t argc, const char *argv[]);
RESULT programmer_gpio_out(uint16_t argc, const char *argv[]);
RESULT programmer_gpio_in(uint16_t argc, const char *argv[]);

RESULT programmer_delay_us(uint16_t argc, const char *argv[]);
RESULT programmer_delay_ms(uint16_t argc, const char *argv[]);
RESULT programmer_commit(uint16_t argc, const char *argv[]);

struct misc_cmd_t programmer_cmd[] = 
{
	// voltage
	{
		"get_tvcc",
		"get target voltage, format: get_tvcc",
		programmer_get_target_voltage
	},
	// powerout
	{
		"set_tvcc",
		"output power to target, format: set_tvcc VOLTAGE_IN_MV",
		programmer_set_target_voltage
	},
	// gpio
	{
		"gpio_init",
		"initialize gpio, format: gpio_init [MASK IO PULL]",
		programmer_gpio_init
	},
	{
		"gpio_fini",
		"finalize gpio, format: gpio_fini",
		programmer_gpio_fini
	},
	{
		"gpio_config",
		"initialize gpio, format: gpio_config MASK IO PULL",
		programmer_gpio_config
	},
	{
		"gpio_out",
		"gpio output, format: gpio_out MASK VALUE",
		programmer_gpio_out
	},
	{
		"gpio_in",
		"gpio input, format: gpio_in MASK",
		programmer_gpio_in
	},
	// iic
	{
		"iic_init",
		"initialize iic, format: iic_init [KHZ MAX_DLY_US]",
		programmer_iic_init
	},
	{
		"iic_fini",
		"finalize iic, format: iic_fini",
		programmer_iic_fini
	},
	{
		"iic_config",
		"config iic, format: iic_config KHZ MAX_DLY_US",
		programmer_iic_config
	},
	{
		"iic_read",
		"read data from iic, format: iic_read SLAVE_ADDR DATA_SIZE",
		programmer_iic_read
	},
	{
		"iic_write",
		"write data to iic, format: iic_write SLAVE_ADDR DATA_SIZE DATA0 DATA1...",
		programmer_iic_write
	},
	// delay
	{
		"delayus",
		"delay us, format: delayus US",
		programmer_delay_us
	},
	{
		"delayms",
		"delay ms, format: delayus MS",
		programmer_delay_ms
	},
	// commit
	{
		"commit",
		"commit all commands",
		programmer_commit
	},
	{
		NULL,
		NULL,
		NULL
	}
};

RESULT programmer_init(const char *programmer)
{
	uint32_t i;
	
	if (programmer != NULL)
	{
		for (i = 0; programmers_info[i].name != NULL; i++)
		{
			if (!strcmp(programmers_info[i].name, programmer))
			{
				cur_programmer = &programmers_info[i];
				return ERROR_OK;
			}
		}
		return ERROR_FAIL;
	}
	else
	{
		cur_programmer = &programmers_info[PROGRAMMER_DEFAULT];
		return ERROR_OK;
	}
}

void programmer_print_list(void)
{
	uint32_t i;
	
	printf("Supported programmers:\n");
	for (i = 0; programmers_info[i].name != NULL; i++)
	{
		programmers_info[i].parse_argument('S', NULL);
	}
	printf("\n");
}

void programmer_print_help(void)
{
	uint32_t i;
	
	for (i = 0; programmers_info[i].name != NULL; i++)
	{
		programmers_info[i].parse_argument('h', NULL);
	}
}





// scripts support
// tvcc
RESULT programmer_get_target_voltage(uint16_t argc, const char *argv[])
{
	struct interface_target_voltage_t *tv = 
						&(cur_programmer->interfaces.target_voltage);
	uint16_t voltage = 0;
	RESULT ret = ERROR_OK;
	
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		programmer_print_help();
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != tv->get(&voltage))
	{
		ret = ERROR_FAIL;
	}
	else
	{
		LOG_INFO(INFOMSG_TARGET_VOLTAGE, voltage / 1000.0);
	}
	return ret;
}

RESULT programmer_set_target_voltage(uint16_t argc, const char *argv[])
{
	struct interface_target_voltage_t *tv = 
						&(cur_programmer->interfaces.target_voltage);
	uint16_t voltage = 0;
	
	if (argc != 2)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	voltage = (uint16_t)strtoul(argv[1], NULL, 0);
	return tv->set(voltage);
}

// gpio
RESULT programmer_gpio_init(uint16_t argc, const char *argv[])
{
	if ((argc != 1) && (argc != 4))
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != cur_programmer->interfaces.gpio.init())
	{
		return ERROR_FAIL;
	}
	
	if (4 == argc)
	{
		return programmer_gpio_config(argc, argv);
	}
	return ERROR_OK;
}

RESULT programmer_gpio_fini(uint16_t argc, const char *argv[])
{
	if (argc != 1)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	return cur_programmer->interfaces.gpio.fini();
}

RESULT programmer_gpio_config(uint16_t argc, const char *argv[])
{
	uint16_t mask, io, pull;
	
	if (argc != 4)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	mask = (uint16_t)strtoul(argv[1], NULL, 0);
	io = (uint16_t)strtoul(argv[2], NULL, 0);
	pull = (uint16_t)strtoul(argv[3], NULL, 0);
	
	return cur_programmer->interfaces.gpio.config(mask, io, pull);
}

RESULT programmer_gpio_out(uint16_t argc, const char *argv[])
{
	uint16_t mask, value;
	
	if (argc != 3)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	mask = (uint16_t)strtoul(argv[1], NULL, 0);
	value = (uint16_t)strtoul(argv[2], NULL, 0);
	
	return cur_programmer->interfaces.gpio.out(mask, value);
}

RESULT programmer_gpio_in(uint16_t argc, const char *argv[])
{
	uint16_t mask, value;
	RESULT ret = ERROR_OK;
	
	if (argc != 2)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	mask = (uint16_t)strtoul(argv[1], NULL, 0);
	
	ret = cur_programmer->interfaces.gpio.in(mask, &value);
	if (ERROR_OK == ret)
	{
		ret = cur_programmer->interfaces.peripheral_commit();
		if (ERROR_OK == ret)
		{
			LOG_INFO(INFOMSG_REG_04X, "GPIO", value);
		}
	}
	return ret;
}

// iic
RESULT programmer_iic_init(uint16_t argc, const char *argv[])
{
	struct interface_i2c_t *i2c = &(cur_programmer->interfaces.i2c);
	
	REFERENCE_PARAMETER(argv);
	if ((argc != 1) && (argc != 3))
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != i2c->init())
	{
		return ERROR_FAIL;
	}
	
	if (3 == argc)
	{
		return programmer_iic_config(argc, argv);
	}
	return ERROR_OK;
}

RESULT programmer_iic_fini(uint16_t argc, const char *argv[])
{
	struct interface_i2c_t *i2c = &(cur_programmer->interfaces.i2c);
	
	REFERENCE_PARAMETER(argv);
	if (argc != 1)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	return i2c->fini();
}

RESULT programmer_iic_config(uint16_t argc, const char *argv[])
{
	struct interface_i2c_t *i2c = &(cur_programmer->interfaces.i2c);
	uint16_t speed_khz = 0;
	uint16_t max_dly = 0;
	
	if (argc != 3)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	speed_khz = (uint16_t)strtoul(argv[1], NULL, 0);
	max_dly = (uint16_t)strtoul(argv[2], NULL, 0);
	
	return i2c->config(speed_khz, 0, max_dly);
}

RESULT programmer_iic_read(uint16_t argc, const char *argv[])
{
	struct interface_i2c_t *i2c = &(cur_programmer->interfaces.i2c);
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t *buff = NULL;
	RESULT ret = ERROR_OK;
	
	if (argc != 3)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	data_size = (uint8_t)strtoul(argv[2], NULL, 0);
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return ERROR_FAIL;
	}
	
	ret = i2c->read(addr, buff, data_size, 1);
	if (ERROR_OK == ret)
	{
		ret = cur_programmer->interfaces.peripheral_commit();
		if (ERROR_OK == ret)
		{
			LOG_BYTE_BUF(buff, data_size, LOG_INFO, "%02X", 16);
		}
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

RESULT programmer_iic_write(uint16_t argc, const char *argv[])
{
	struct interface_i2c_t *i2c = &(cur_programmer->interfaces.i2c);
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t *buff = NULL;
	uint8_t i;
	RESULT ret = ERROR_OK;
	
	if (argc < 3)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	data_size = (uint8_t)strtoul(argv[2], NULL, 0);
	if ((0 == data_size) || (argc != 3 + data_size))
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return ERROR_FAIL;
	}
	
	for (i = 0; i < data_size; i++)
	{
		buff[i] = (uint8_t)strtoul(argv[3 + i], NULL, 0);
	}
	
	ret = i2c->write(addr, buff, data_size, 1);
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

// delay
RESULT programmer_delay_us(uint16_t argc, const char *argv[])
{
	uint16_t delay;
	
	if (argc != 2)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return cur_programmer->interfaces.delay.delayus(delay);
}

RESULT programmer_delay_ms(uint16_t argc, const char *argv[])
{
	uint16_t delay;
	
	if (argc != 2)
	{
		misc_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return cur_programmer->interfaces.delay.delayms(delay);
}

// commit
RESULT programmer_commit(uint16_t argc, const char *argv[])
{
	REFERENCE_PARAMETER(argc);
	REFERENCE_PARAMETER(argv);
	
	return cur_programmer->interfaces.peripheral_commit();
}

