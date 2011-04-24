/***************************************************************************
 *   Copyright (C) 2009 - 2010 by Simon Qian <SimonQian@SimonQian.com>     *
 *                                                                         *
 *   This ifsram is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This ifsram is distributed in the hope that it will be useful,        *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this ifsram; if not, write to the                          *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "port.h"
#include "app_cfg.h"
#include "app_type.h"
#include "app_io.h"
#include "app_log.h"
#include "app_err.h"

#include "interfaces_const.h"
#include "target.h"
#include "scripts.h"
#include "interfaces.h"
#include "versaloon/versaloon.h"
#include "virtualinterface/vi_stm32/vi_stm32.h"

struct interfaces_info_t *interfaces_info[] = 
{
	// real interfaces
	// versaloon
	&versaloon_interfaces,
	// virtual interfaces
	&vi_stm32_interfaces,
	NULL
};

struct interfaces_info_t *cur_interface = NULL;
struct interfaces_info_t *cur_real_interface = NULL;

VSS_HANDLER(interface_get_target_voltage);
VSS_HANDLER(interface_set_target_voltage);

VSS_HANDLER(interface_iic_init);
VSS_HANDLER(interface_iic_fini);
VSS_HANDLER(interface_iic_config);
VSS_HANDLER(interface_iic_read);
VSS_HANDLER(interface_iic_write);
VSS_HANDLER(interface_iic_read_buff8);
VSS_HANDLER(interface_iic_write_buff8);

VSS_HANDLER(interface_gpio_init);
VSS_HANDLER(interface_gpio_fini);
VSS_HANDLER(interface_gpio_config);
VSS_HANDLER(interface_gpio_out);
VSS_HANDLER(interface_gpio_in);

VSS_HANDLER(interface_spi_init);
VSS_HANDLER(interface_spi_fini);
VSS_HANDLER(interface_spi_config);
VSS_HANDLER(interface_spi_io);

VSS_HANDLER(interface_pwm_init);
VSS_HANDLER(interface_pwm_fini);
VSS_HANDLER(interface_pwm_config);
VSS_HANDLER(interface_pwm_out);

VSS_HANDLER(interface_delay_us);
VSS_HANDLER(interface_delay_ms);
VSS_HANDLER(interface_commit);

struct vss_cmd_t interface_cmd[] = 
{
	VSS_CMD(	"tvcc.get", 
				"get target voltage, format: tvcc.get", 
				interface_get_target_voltage),
	VSS_CMD(	"tvcc.set", 
				"output power to target, format: tvcc.set VOLTAGE_IN_MV", 
				interface_set_target_voltage),
	VSS_CMD(	"gpio.init",
				"initialize gpio, format: gpio.init [MASK IO PULL]",
				interface_gpio_init),
	VSS_CMD(	"gpio.fini",
				"finalize gpio, format: gpio.fini",
				interface_gpio_fini),
	VSS_CMD(	"gpio.config",
				"config gpio, format: gpio.config MASK IO PULL",
				interface_gpio_config),
	VSS_CMD(	"gpio.out",
				"gpio output, format: gpio.out MASK VALUE",
				interface_gpio_out),
	VSS_CMD(	"gpio.in",
				"gpio input, format: gpio.in MASK",
				interface_gpio_in),
	VSS_CMD(	"spi.init",
				"initialize spi, format: spi.init [KHZ CPOL CPHA FIRSTBIT]",
				interface_spi_init),
	VSS_CMD(	"spi.fini",
				"finalize spi, format: spi.fini",
				interface_spi_fini),
	VSS_CMD(	"spi.config",
				"config spi, format: spi.config KHZ CPOL CPHA FIRSTBIT",
				interface_spi_config),
	VSS_CMD(	"spi.io",
				"spi input and output, format: spi.io DATASIZE DATA...",
				interface_spi_io),
	VSS_CMD(	"iic.init",
				"initialize iic, format: iic.init [KHZ MAX_DLY_US]",
				interface_iic_init),
	VSS_CMD(	"iic.fini",
				"finalize iic, format: iic.fini [KHZ MAX_DLY_US]",
				interface_iic_fini),
	VSS_CMD(	"iic.config",
				"config iic, format: iic.config KHZ MAX_DLY_US",
				interface_iic_config),
	VSS_CMD(	"iic.read",
				"read data from iic, "
				"format: iic.read SLAVE_ADDR STOP NACKLAST DATA_SIZE",
				interface_iic_read),
	VSS_CMD(	"iic.write",
				"write data to iic, format: "
				"iic.write SLAVE_ADDR STOP DATA_SIZE DATA0...",
				interface_iic_write),
	VSS_CMD(	"iic.read_buff8",
				"read data from iic, "
				"format: iic.read_buff8 SLAVE_ADDR NACKLAST DATA_SIZE ADDR",
				interface_iic_read_buff8),
	VSS_CMD(	"iic.write_buff8",
				"write data to iic, format: "
				"iic.write_buff8 SLAVE_ADDR DATA_SIZE ADDR DATA0...",
				interface_iic_write_buff8),
	VSS_CMD(	"pwm.init",
				"initialize pwm module, format: pwm.init [KHZ PUSHPULL POLARITY]",
				interface_pwm_init),
	VSS_CMD(	"pwm.fini",
				"finialize pwm module, format: pwm.fini",
				interface_pwm_fini),
	VSS_CMD(	"pwm.config",
				"config pwm module, format: pwm.config KHZ PUSHPULL POLARITY",
				interface_pwm_config),
	VSS_CMD(	"pwm.out",
				"output pwm sequence, format: pwm.out CNT RATE0 RATE1 ...",
				interface_pwm_out),
	VSS_CMD(	"delay.delayus",
				"delay us, format: delay.delayus US",
				interface_delay_us),
	VSS_CMD(	"delay.delayms",
				"delay ms, format: delay.delayus MS",
				interface_delay_ms),
	VSS_CMD(	"commit",
				"commit all commands",
				interface_commit),
	VSS_CMD_END
};

char* get_interface_name(uint64_t i)
{
#define interface_case(i) case i: return #i
	
	switch (i)
	{
	interface_case(IFS_USART);
	interface_case(IFS_SPI);
	interface_case(IFS_I2C);
	interface_case(IFS_GPIO);
	interface_case(IFS_CAN);
	interface_case(IFS_CLOCK);
	interface_case(IFS_ADC);
	interface_case(IFS_DAC);
	interface_case(IFS_POWER);
	interface_case(IFS_ISSP);
	interface_case(IFS_JTAG_LL);
	interface_case(IFS_JTAG_HL);
	interface_case(IFS_JTAG_RAW);
	interface_case(IFS_C2);
	interface_case(IFS_MSP430_SBW);
	interface_case(IFS_MSP430_JTAG);
	interface_case(IFS_LPC_ICP);
	interface_case(IFS_SWD);
	interface_case(IFS_SWIM);
	interface_case(IFS_HV);
	interface_case(IFS_PDI);
	interface_case(IFS_BDM);
	default:
		return NULL;
	}
}

#define INTERFACE_DEFAULT				0

static struct interfaces_info_t *find_interface_by_name(const char *ifs)
{
	struct interfaces_info_t *interface_tmp;
	uint32_t i;
	
	interface_tmp = NULL;
	if (ifs != NULL)
	{
		for (i = 0; interfaces_info[i] != NULL; i++)
		{
			if (!strcmp(interfaces_info[i]->name, ifs))
			{
				interface_tmp = interfaces_info[i];
				break;
			}
		}
	}
	return interface_tmp;
}

RESULT virtual_interface_init(const char *vifs, const char mode)
{
	struct interfaces_info_t *interface_tmp;
	
	interface_tmp = find_interface_by_name(vifs);
	if ((vifs != NULL) && (NULL == interface_tmp))
	{
		return ERROR_FAIL;
	}
	
	if (NULL == interface_tmp)
	{
		if (cur_real_interface != NULL)
		{
			cur_interface = cur_real_interface;
		}
	}
	else if (interface_tmp->is_virtual)
	{
		uint32_t i = 0;
		
		if (interface_tmp->mode != NULL)
		{
			while (interface_tmp->mode[i].name != 0)
			{
				if (mode == interface_tmp->mode[i].name)
				{
					break;
				}
				i++;
			}
			if (!interface_tmp->mode[i].name)
			{
				return ERROR_FAIL;
			}
		}
		
		if (NULL == cur_interface)
		{
			if (ERROR_OK != interface_init(NULL))
			{
				return ERROR_FAIL;
			}
		}
		
		if (cur_interface->is_virtual)
		{
			cur_interface = interface_tmp;
		}
		else
		{
			cur_real_interface = cur_interface;
			cur_interface = interface_tmp;
		}
		cur_interface->init(&i);
	}
	else
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

RESULT interface_init(const char *ifs)
{
	struct interfaces_info_t *interface_tmp;
	
	interface_tmp = find_interface_by_name(ifs);
	if (NULL == interface_tmp)
	{
		interface_tmp = interfaces_info[INTERFACE_DEFAULT];
	}
	
	if ((interface_tmp != NULL) && (!interface_tmp->is_virtual))
	{
		if ((cur_interface != NULL) && (!cur_interface->is_virtual))
		{
			cur_interface->fini();
		}
		if ((cur_real_interface != NULL) && (!cur_real_interface->is_virtual))
		{
			cur_real_interface->fini();
		}
		cur_interface = NULL;
		cur_real_interface = NULL;
		
		if (ERROR_OK != interface_tmp->init(interface_tmp))
		{
			return ERROR_FAIL;
		}
		cur_interface = interface_tmp;
		if (cur_interface->support_mask & IFS_POWER)
		{
			return vss_run_script("tvcc.get");
		}
		else
		{
			return ERROR_OK;
		}
	}
	else
	{
		return ERROR_FAIL;
	}
}

RESULT interface_assert(struct interfaces_info_t **ifs)
{
	if (NULL == cur_interface)
	{
		if ((ERROR_OK != interface_init(NULL)) 
			|| (NULL == cur_interface))
		{
			vss_set_fatal_error();
			return ERROR_FAIL;
		}
	}
	if (ifs != NULL)
	{
		*ifs = cur_interface;
	}
	return ERROR_OK;
}

void interface_print_list(void)
{
	uint32_t i;
	
	PRINTF("Supported interfaces:\n");
	for (i = 0; interfaces_info[i] != NULL; i++)
	{
		vss_call_notifier(interfaces_info[i]->notifier, "support", NULL);
	}
	PRINTF("\n");
}

void interface_print_help(void)
{
	uint32_t i;
	
	for (i = 0; interfaces_info[i] != NULL; i++)
	{
		vss_call_notifier(interfaces_info[i]->notifier, "help", NULL);
	}
}

// tvcc
VSS_HANDLER(interface_get_target_voltage)
{
	uint16_t voltage = 0;
	RESULT ret = ERROR_OK;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_POWER))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "power interface");
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != ifs->target_voltage.get(0, &voltage))
	{
		ret = ERROR_FAIL;
	}
	else
	{
		LOG_INFO(INFOMSG_TARGET_VOLTAGE, voltage / 1000.0);
	}
	return ret;
}

VSS_HANDLER(interface_set_target_voltage)
{
	uint16_t voltage = 0;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_POWER))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "power interface");
		return ERROR_FAIL;
	}
	
	voltage = (uint16_t)strtoul(argv[1], NULL, 0);
	return ifs->target_voltage.set(0, voltage);
}

// gpio
VSS_HANDLER(interface_gpio_init)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 4);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_GPIO))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "gpio interface");
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != ifs->gpio.init(0))
	{
		return ERROR_FAIL;
	}
	
	if (4 == argc)
	{
		return interface_gpio_config(argc, argv);
	}
	return ERROR_OK;
}

VSS_HANDLER(interface_gpio_fini)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_GPIO))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "gpio interface");
		return ERROR_FAIL;
	}
	
	return ifs->gpio.fini(0);
}

VSS_HANDLER(interface_gpio_config)
{
	uint32_t mask, io, pull, pull_en;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_GPIO))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "gpio interface");
		return ERROR_FAIL;
	}
	
	mask = (uint32_t)strtoul(argv[1], NULL, 0);
	io = (uint32_t)strtoul(argv[2], NULL, 0);
	pull_en = (uint32_t)strtoul(argv[3], NULL, 0);
	pull = (uint32_t)strtoul(argv[4], NULL, 0);
	
	return ifs->gpio.config(0, mask, io, pull_en, pull);
}

VSS_HANDLER(interface_gpio_out)
{
	uint32_t mask, value;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(3);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_GPIO))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "gpio interface");
		return ERROR_FAIL;
	}
	
	mask = (uint32_t)strtoul(argv[1], NULL, 0);
	value = (uint32_t)strtoul(argv[2], NULL, 0);
	
	return ifs->gpio.out(0, mask, value);
}

VSS_HANDLER(interface_gpio_in)
{
	uint32_t mask, value;
	RESULT ret = ERROR_OK;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_GPIO))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "gpio interface");
		return ERROR_FAIL;
	}
	
	mask = (uint32_t)strtoul(argv[1], NULL, 0);
	
	ret = ifs->gpio.in(0, mask, &value);
	if (ERROR_OK == ret)
	{
		ret = ifs->peripheral_commit();
		if (ERROR_OK == ret)
		{
			LOG_INFO(INFOMSG_REG_08X, "GPIO", value);
		}
	}
	return ret;
}

// spi
VSS_HANDLER(interface_spi_init)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 5);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_SPI))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "spi interface");
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != ifs->spi.init(0))
	{
		return ERROR_FAIL;
	}
	
	if (5 == argc)
	{
		return interface_spi_config(argc, argv);
	}
	return ERROR_OK;
}

VSS_HANDLER(interface_spi_fini)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_SPI))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "spi interface");
		return ERROR_FAIL;
	}
	
	return ifs->spi.fini(0);
}

VSS_HANDLER(interface_spi_config)
{
	uint16_t khz = 0;
	uint8_t cpol, cpha, firstbit;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_SPI))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "spi interface");
		return ERROR_FAIL;
	}
	
	khz = (uint16_t)strtoul(argv[1], NULL, 0);
	cpol = (uint8_t)strtoul(argv[2], NULL, 0);
	cpha = (uint8_t)strtoul(argv[3], NULL, 0);
	firstbit = (uint8_t)strtoul(argv[4], NULL, 0);
	
	return ifs->spi.config(0, khz, cpol, cpha, firstbit);
}

VSS_HANDLER(interface_spi_io)
{
	uint16_t data_size = 0;
	uint8_t *buff = NULL;
	RESULT ret = ERROR_OK;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(2);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_SPI))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "spi interface");
		return ERROR_FAIL;
	}
	
	data_size = (uint16_t)strtoul(argv[1], NULL, 0);

	VSS_CHECK_ARGC_2(2, 2 + data_size);
	if (0 == data_size)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	ret = vss_get_binary_buffer(argc - 2, &argv[2], 1, data_size, (void**)&buff);
	if (ERROR_OK == ret)
	{
		ret = ifs->spi.io(0, buff, buff, data_size);
		if (ERROR_OK == ret)
		{
			ret = ifs->peripheral_commit();
			if (ERROR_OK == ret)
			{
				LOG_BYTE_BUF(buff, data_size, LOG_INFO, "%02X", 16);
			}
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
VSS_HANDLER(interface_iic_init)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 3);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_I2C))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "iic interface");
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != ifs->i2c.init(0))
	{
		return ERROR_FAIL;
	}
	
	if (3 == argc)
	{
		return interface_iic_config(argc, argv);
	}
	return ERROR_OK;
}

VSS_HANDLER(interface_iic_fini)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_I2C))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "iic interface");
		return ERROR_FAIL;
	}
	
	return ifs->i2c.fini(0);
}

VSS_HANDLER(interface_iic_config)
{
	uint16_t khz = 0;
	uint16_t max_dly = 0;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(3);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_I2C))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "iic interface");
		return ERROR_FAIL;
	}
	
	khz = (uint16_t)strtoul(argv[1], NULL, 0);
	max_dly = (uint16_t)strtoul(argv[2], NULL, 0);
	
	return ifs->i2c.config(0, khz, 0, max_dly);
}

VSS_HANDLER(interface_iic_read)
{
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t stop = 0;
	bool nacklast = false;
	uint8_t *buff = NULL;
	RESULT ret = ERROR_OK;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_I2C))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "iic interface");
		return ERROR_FAIL;
	}
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	stop = (uint8_t)strtoul(argv[2], NULL, 0);
	nacklast = strtoul(argv[3], NULL, 0) > 0;
	data_size = (uint8_t)strtoul(argv[4], NULL, 0);
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return ERROR_FAIL;
	}
	
	ret = ifs->i2c.read(0, addr, buff, data_size, stop, nacklast);
	if (ERROR_OK == ret)
	{
		ret = ifs->peripheral_commit();
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

VSS_HANDLER(interface_iic_write)
{
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t stop = 0;
	uint8_t *buff = NULL;
	RESULT ret = ERROR_OK;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(4);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_I2C))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "iic interface");
		return ERROR_FAIL;
	}
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	stop = (uint8_t)strtoul(argv[2], NULL, 0);
	data_size = (uint8_t)strtoul(argv[3], NULL, 0);
	
	VSS_CHECK_ARGC(4 + data_size);
	if (0 == data_size)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	ret = vss_get_binary_buffer(argc - 4, &argv[4], 1, data_size, (void**)&buff);
	if (ERROR_OK == ret)
	{
		ret = ifs->i2c.write(0, addr, buff, data_size, stop);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

VSS_HANDLER(interface_iic_read_buff8)
{
	uint8_t data_size = 0;
	uint8_t slave_addr = 0, addr;
	bool nacklast = false;
	uint8_t *buff = NULL;
	RESULT ret = ERROR_OK;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_I2C))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "iic interface");
		return ERROR_FAIL;
	}
	
	slave_addr = (uint8_t)strtoul(argv[1], NULL, 0);
	nacklast = strtoul(argv[2], NULL, 0) > 0;
	data_size = (uint8_t)strtoul(argv[3], NULL, 0);
	addr = (uint8_t)strtoul(argv[4], NULL, 0);
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return ERROR_FAIL;
	}
	
	ret = ifs->i2c.write(0, slave_addr, &addr, 1, 0);
	if (ERROR_OK == ret)
	{
		ret = ifs->i2c.read(0, slave_addr, buff, data_size, 1, 
										nacklast);
		if (ERROR_OK == ret)
		{
			ret = ifs->peripheral_commit();
			if (ERROR_OK == ret)
			{
				LOG_BYTE_BUF(buff, data_size, LOG_INFO, "%02X", 16);
			}
		}
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

VSS_HANDLER(interface_iic_write_buff8)
{
	uint8_t data_size = 0;
	uint8_t slave_addr = 0;
	uint8_t *buff = NULL;
	RESULT ret = ERROR_OK;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(4);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_I2C))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "iic interface");
		return ERROR_FAIL;
	}
	
	slave_addr = (uint8_t)strtoul(argv[1], NULL, 0);
	data_size = (uint8_t)strtoul(argv[2], NULL, 0);
	
	VSS_CHECK_ARGC(4 + data_size);
	if (0 == data_size)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	ret = vss_get_binary_buffer(argc - 3, &argv[3], 1, data_size + 1, 
								(void**)&buff);
	if (ERROR_OK == ret)
	{
		ret = ifs->i2c.write(0, slave_addr, buff, data_size + 1, 1);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

// pwm
VSS_HANDLER(interface_pwm_init)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 4);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_PWM))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "pwm interface");
		return ERROR_FAIL;
	}
	
	if (ERROR_OK != ifs->pwm.init(0))
	{
		return ERROR_FAIL;
	}
	
	if (4 == argc)
	{
		return interface_pwm_config(argc, argv);
	}
	return ERROR_OK;
}

VSS_HANDLER(interface_pwm_fini)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_PWM))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "pwm interface");
		return ERROR_FAIL;
	}
	
	return ifs->pwm.fini(0);
}

VSS_HANDLER(interface_pwm_config)
{
	struct interfaces_info_t *ifs = NULL;
	uint16_t kHz;
	uint8_t pushpull, polarity;
	
	VSS_CHECK_ARGC(4);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_PWM))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "pwm interface");
		return ERROR_FAIL;
	}
	
	kHz = (uint16_t)strtoul(argv[1], NULL, 0);
	pushpull = (uint8_t)strtoul(argv[2], NULL, 0);
	polarity = (uint8_t)strtoul(argv[3], NULL, 0);
	
	return ifs->pwm.config(0, kHz, pushpull, polarity);
}

VSS_HANDLER(interface_pwm_out)
{
	uint16_t count = 0;
	uint16_t *buff = NULL;
	RESULT ret = ERROR_OK;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(3);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	if (!(cur_interface->support_mask & IFS_PWM))
	{
		LOG_ERROR(ERRMSG_NOT_SUPPORT, "pwm interface");
		return ERROR_FAIL;
	}
	
	count = (uint16_t)strtoul(argv[1], NULL, 0);
	
	VSS_CHECK_ARGC(2 + count);
	if (0 == count)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "count");
		vss_print_help(argv[0]);
		return ERROR_FAIL;
	}
	
	ret = vss_get_binary_buffer(argc - 2, &argv[2], 2, count, (void**)&buff);
	if (ERROR_OK == ret)
	{
		ret = ifs->pwm.out(0, count, buff);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return ret;
}

// delay
VSS_HANDLER(interface_delay_us)
{
	uint16_t delay;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return ifs->delay.delayus(delay);
}

VSS_HANDLER(interface_delay_ms)
{
	uint16_t delay;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return ifs->delay.delayms(delay);
}

// commit
VSS_HANDLER(interface_commit)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	if ((ERROR_OK != interface_assert(&ifs)) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return ERROR_FAIL;
	}
	
	return ifs->peripheral_commit();
}

