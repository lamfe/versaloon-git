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

#include "vsprog.h"
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
					  versaloon_notifier, 			// notifier
					  versaloon_init_capability, 	// init_capability
					  versaloon_display_programmer),// display_programmer
	PROGRAMMER_DEFINE(NULL, NULL, NULL, NULL)
};

struct programmer_info_t *cur_programmer = NULL;

MISC_HANDLER(programmer_get_target_voltage);
MISC_HANDLER(programmer_set_target_voltage);

MISC_HANDLER(programmer_iic_init);
MISC_HANDLER(programmer_iic_fini);
MISC_HANDLER(programmer_iic_config);
MISC_HANDLER(programmer_iic_read);
MISC_HANDLER(programmer_iic_write);

MISC_HANDLER(programmer_gpio_init);
MISC_HANDLER(programmer_gpio_fini);
MISC_HANDLER(programmer_gpio_config);
MISC_HANDLER(programmer_gpio_out);
MISC_HANDLER(programmer_gpio_in);

MISC_HANDLER(programmer_spi_init);
MISC_HANDLER(programmer_spi_fini);
MISC_HANDLER(programmer_spi_config);
MISC_HANDLER(programmer_spi_io);

MISC_HANDLER(programmer_delay_us);
MISC_HANDLER(programmer_delay_ms);
MISC_HANDLER(programmer_commit);

MISC_HANDLER(programmer_list);
MISC_HANDLER(programmer_define);

struct misc_cmd_t programmer_cmd[] = 
{
	MISC_CMD(	"display-programmer",
				"list programmers connected, format: display-programmer/L",
				programmer_list),
	MISC_CMD(	"L",
				"list programmers connected, format: display-programmer/L",
				programmer_list),
	MISC_CMD(	"programmer",
				"define programmer to use, format: programmer/p PROGRAMMER",
				programmer_define),
	MISC_CMD(	"p",
				"define programmer to use, format: programmer/p PROGRAMMER",
				programmer_define),
	MISC_CMD(	"get_tvcc", 
				"get target voltage, format: get_tvcc", 
				programmer_get_target_voltage),
	MISC_CMD(	"set_tvcc", 
				"output power to target, format: set_tvcc VOLTAGE_IN_MV", 
				programmer_set_target_voltage),
	MISC_CMD(	"gpio_init",
				"initialize gpio, format: gpio_init [MASK IO PULL]",
				programmer_gpio_init),
	MISC_CMD(	"gpio_fini",
				"finalize gpio, format: gpio_fini",
				programmer_gpio_fini),
	MISC_CMD(	"gpio_config",
				"config gpio, format: gpio_config MASK IO PULL",
				programmer_gpio_config),
	MISC_CMD(	"gpio_out",
				"gpio output, format: gpio_out MASK VALUE",
				programmer_gpio_out),
	MISC_CMD(	"gpio_in",
				"gpio input, format: gpio_in MASK",
				programmer_gpio_in),
	MISC_CMD(	"spi_init",
				"initialize spi, format: spi_init [KHZ CPOL CPHA FIRSTBIT]",
				programmer_spi_init),
	MISC_CMD(	"spi_fini",
				"finalize spi, format: spi_fini",
				programmer_spi_fini),
	MISC_CMD(	"spi_config",
				"config spi, format: spi_config KHZ CPOL CPHA FIRSTBIT",
				programmer_spi_config),
	MISC_CMD(	"spi_io",
				"spi input and output, format: spi_io DATASIZE DATA...",
				programmer_spi_io),
	MISC_CMD(	"iic_init",
				"initialize iic, format: iic_init [KHZ MAX_DLY_US]",
				programmer_iic_init),
	MISC_CMD(	"iic_fini",
				"finalize iic, format: iic_fini",
				programmer_iic_fini),
	MISC_CMD(	"iic_config",
				"config iic, format: iic_config KHZ MAX_DLY_US",
				programmer_iic_config),
	MISC_CMD(	"iic_read",
				"read data from iic, format: iic_read SLAVE_ADDR DATA_SIZE",
				programmer_iic_read),
	MISC_CMD(	"iic_write",
				"write data to iic, format: "
				"iic_write SLAVE_ADDR DATA_SIZE DATA0...",
				programmer_iic_write),
	MISC_CMD(	"delayus",
				"delay us, format: delayus US",
				programmer_delay_us),
	MISC_CMD(	"delayms",
				"delay ms, format: delayus MS",
				programmer_delay_ms),
	MISC_CMD(	"commit",
				"commit all commands",
				programmer_commit),
	MISC_CMD_END
};

RESULT programmer_init(const char *programmer)
{
	struct programmer_info_t *programmer_tmp;
	uint32_t i;
	
	programmer_tmp = NULL;
	if (programmer != NULL)
	{
		for (i = 0; programmers_info[i].name != NULL; i++)
		{
			if (!strcmp(programmers_info[i].name, programmer))
			{
				programmer_tmp = &programmers_info[i];
				break;
			}
		}
	}
	else
	{
		programmer_tmp = &programmers_info[PROGRAMMER_DEFAULT];
	}
	
	if (programmer_tmp != NULL)
	{
		if (cur_programmer != NULL)
		{
			cur_programmer->fini();
		}
		
		cur_programmer = programmer_tmp;
		cur_programmer->init_capability(cur_programmer);
		return cur_programmer->init();
	}
	else
	{
		return ERROR_FAIL;
	}
}

RESULT programmer_assert(struct programmer_info_t **prog)
{
	if (NULL == cur_programmer)
	{
		if ((ERROR_OK != programmer_init(NULL)) 
			|| (NULL == cur_programmer))
		{
			misc_set_fatal_error();
			return ERROR_FAIL;
		}
	}
	if (prog != NULL)
	{
		*prog = cur_programmer;
	}
	return ERROR_OK;
}

void programmer_print_list(void)
{
	uint32_t i;
	
	printf("Supported programmers:\n");
	for (i = 0; programmers_info[i].name != NULL; i++)
	{
		misc_call_notifier(programmers_info[i].notifier, "support", NULL);
	}
	printf("\n");
}

void programmer_print_help(void)
{
	uint32_t i;
	
	for (i = 0; programmers_info[i].name != NULL; i++)
	{
		misc_call_notifier(programmers_info[i].notifier, "help", NULL);
	}
}





// scripts support
MISC_HANDLER(programmer_list)
{
	uint32_t i, j = 0;
	
	vsprog_no_call_operate();
	MISC_CHECK_ARGC(1);
	
	for (i = 0; programmers_info[i].name != NULL; i++)
	{
		j += programmers_info[i].display_programmer();
	}
	if (0 == j)
	{
		LOG_INFO("no programmer found.");
	}
	return ERROR_OK;
}

MISC_HANDLER(programmer_define)
{
	char *programmer;
	
	MISC_CHECK_ARGC_2(1, 2);
	if (1 == argc)
	{
		programmer = NULL;
	}
	else
	{
		programmer = (char *)argv[1];
	}
	
	if (ERROR_OK != programmer_init(programmer))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "initialize programmer: ", 
					argv[1]);
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

// tvcc
MISC_HANDLER(programmer_get_target_voltage)
{
	uint16_t voltage = 0;
	RESULT ret = ERROR_OK;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(1);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != prog->interfaces.target_voltage.get(&voltage))
	{
		ret = ERROR_FAIL;
	}
	else
	{
		LOG_INFO(INFOMSG_TARGET_VOLTAGE, voltage / 1000.0);
	}
	return ret;
}

MISC_HANDLER(programmer_set_target_voltage)
{
	uint16_t voltage = 0;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(2);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	voltage = (uint16_t)strtoul(argv[1], NULL, 0);
	return prog->interfaces.target_voltage.set(voltage);
}

// gpio
MISC_HANDLER(programmer_gpio_init)
{
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC_2(1, 4);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != prog->interfaces.gpio.init())
	{
		return ERROR_FAIL;
	}
	
	if (4 == argc)
	{
		return programmer_gpio_config(argc, argv);
	}
	return ERROR_OK;
}

MISC_HANDLER(programmer_gpio_fini)
{
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(1);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	return prog->interfaces.gpio.fini();
}

MISC_HANDLER(programmer_gpio_config)
{
	uint16_t mask, io, pull;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(4);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	mask = (uint16_t)strtoul(argv[1], NULL, 0);
	io = (uint16_t)strtoul(argv[2], NULL, 0);
	pull = (uint16_t)strtoul(argv[3], NULL, 0);
	
	return prog->interfaces.gpio.config(mask, io, pull);
}

MISC_HANDLER(programmer_gpio_out)
{
	uint16_t mask, value;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(3);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	mask = (uint16_t)strtoul(argv[1], NULL, 0);
	value = (uint16_t)strtoul(argv[2], NULL, 0);
	
	return prog->interfaces.gpio.out(mask, value);
}

MISC_HANDLER(programmer_gpio_in)
{
	uint16_t mask, value;
	RESULT ret = ERROR_OK;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(2);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	mask = (uint16_t)strtoul(argv[1], NULL, 0);
	
	ret = prog->interfaces.gpio.in(mask, &value);
	if (ERROR_OK == ret)
	{
		ret = prog->interfaces.peripheral_commit();
		if (ERROR_OK == ret)
		{
			LOG_INFO(INFOMSG_REG_04X, "GPIO", value);
		}
	}
	return ret;
}

// spi
MISC_HANDLER(programmer_spi_init)
{
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC_2(1, 5);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != prog->interfaces.spi.init())
	{
		return ERROR_FAIL;
	}
	
	if (5 == argc)
	{
		return programmer_spi_config(argc, argv);
	}
	return ERROR_OK;
}

MISC_HANDLER(programmer_spi_fini)
{
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(1);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	return prog->interfaces.spi.fini();
}

MISC_HANDLER(programmer_spi_config)
{
	uint16_t khz = 0;
	uint8_t cpol, cpha, firstbit;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(5);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	khz = (uint16_t)strtoul(argv[1], NULL, 0);
	cpol = (uint8_t)strtoul(argv[2], NULL, 0);
	cpha = (uint8_t)strtoul(argv[3], NULL, 0);
	firstbit = (uint8_t)strtoul(argv[4], NULL, 0);
	
	return prog->interfaces.spi.config(khz, cpol, cpha, firstbit);
}

MISC_HANDLER(programmer_spi_io)
{
	uint16_t data_size = 0;
	uint8_t *buff = NULL;
	RESULT ret = ERROR_OK;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC_MIN(2);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	data_size = (uint16_t)strtoul(argv[1], NULL, 0);

	MISC_CHECK_ARGC_2(2, 2 + data_size);
	
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return ERROR_FAIL;
	}
	
	if (2 == argc)
	{
		memset(buff, 0, data_size);
	}
	else
	{
		uint16_t i;
		
		for (i = 0; i < data_size; i++)
		{
			buff[i] = (uint8_t)strtoul(argv[2 + i], NULL, 0);
		}
	}
	
	ret = prog->interfaces.spi.io(buff, buff, data_size, 0, 
											data_size);
	if (ERROR_OK == ret)
	{
		ret = prog->interfaces.peripheral_commit();
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

// iic
MISC_HANDLER(programmer_iic_init)
{
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC_2(1, 3);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != prog->interfaces.i2c.init())
	{
		return ERROR_FAIL;
	}
	
	if (3 == argc)
	{
		return programmer_iic_config(argc, argv);
	}
	return ERROR_OK;
}

MISC_HANDLER(programmer_iic_fini)
{
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(1);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	return prog->interfaces.i2c.fini();
}

MISC_HANDLER(programmer_iic_config)
{
	uint16_t khz = 0;
	uint16_t max_dly = 0;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(3);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	khz = (uint16_t)strtoul(argv[1], NULL, 0);
	max_dly = (uint16_t)strtoul(argv[2], NULL, 0);
	
	return prog->interfaces.i2c.config(khz, 0, max_dly);
}

MISC_HANDLER(programmer_iic_read)
{
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t *buff = NULL;
	RESULT ret = ERROR_OK;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(3);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	data_size = (uint8_t)strtoul(argv[2], NULL, 0);
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return ERROR_FAIL;
	}
	
	ret = prog->interfaces.i2c.read(addr, buff, data_size, 1);
	if (ERROR_OK == ret)
	{
		ret = prog->interfaces.peripheral_commit();
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

MISC_HANDLER(programmer_iic_write)
{
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t *buff = NULL;
	uint8_t i;
	RESULT ret = ERROR_OK;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC_MIN(3);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	data_size = (uint8_t)strtoul(argv[2], NULL, 0);
	
	MISC_CHECK_ARGC(3 + data_size);
	if (0 == data_size)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
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
	
	ret = prog->interfaces.i2c.write(addr, buff, data_size, 1);
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

// delay
MISC_HANDLER(programmer_delay_us)
{
	uint16_t delay;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(2);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return prog->interfaces.delay.delayus(delay);
}

MISC_HANDLER(programmer_delay_ms)
{
	uint16_t delay;
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(2);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return prog->interfaces.delay.delayms(delay);
}

// commit
MISC_HANDLER(programmer_commit)
{
	struct programmer_info_t *prog = NULL;
	
	MISC_CHECK_ARGC(1);
	if ((ERROR_OK != programmer_assert(&prog)) || (NULL == prog))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "programmer module");
		return ERROR_FAIL;
	}
	
	return prog->interfaces.peripheral_commit();
}

