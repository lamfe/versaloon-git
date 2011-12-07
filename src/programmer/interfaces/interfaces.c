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

#include "interfaces.h"
#include "scripts.h"
#include "versaloon/versaloon.h"
#include "virtualinterface/vi_stm32/vi_stm32.h"
#include "target.h"

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

VSS_HANDLER(interface_jtag_init);
VSS_HANDLER(interface_jtag_fini);
VSS_HANDLER(interface_jtag_config);
VSS_HANDLER(interface_jtag_reset);
VSS_HANDLER(interface_jtag_runtest);
VSS_HANDLER(interface_jtag_ir);
VSS_HANDLER(interface_jtag_dr);

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
VSS_HANDLER(interface_pwm_config_mode);
VSS_HANDLER(interface_pwm_config_freq);
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
	VSS_CMD(	"jtag.init",
				"initialize jtag, format: jtag.init [KHZ UB UA BB BA]",
				interface_jtag_init),
	VSS_CMD(	"jtag.fini",
				"finalize jtag, format: jtag.fini",
				interface_jtag_fini),
	VSS_CMD(	"jtag.config",
				"configure jtag, format: jtag.config KHZ [UB UA BB BA]",
				interface_jtag_config),
	VSS_CMD(	"jtag.reset",
				"reset jtag, format: jtag.reset",
				interface_jtag_reset),
	VSS_CMD(	"jtag.runtest",
				"jtag runtest, format: jtag.runtest CYCLES",
				interface_jtag_runtest),
	VSS_CMD(	"jtag.ir",
				"jtag ir, format: jtag.ir BITLEN IR",
				interface_jtag_ir),
	VSS_CMD(	"jtag.dr",
				"jtag dr, format: jtag.dr BITLEN DATASIZE DR_DATA...",
				interface_jtag_dr),
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
				"initialize spi, format: spi.init [KHZ MODE FIRSTBIT]",
				interface_spi_init),
	VSS_CMD(	"spi.fini",
				"finalize spi, format: spi.fini",
				interface_spi_fini),
	VSS_CMD(	"spi.config",
				"config spi, format: spi.config KHZ MODE FIRSTBIT",
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
	VSS_CMD(	"pwm.config_mode",
				"config pwm module, format: pwm.config_mode PUSHPULL POLARITY",
				interface_pwm_config_mode),
	VSS_CMD(	"pwm.config_freq",
				"config pwm module, format: pwm.config_freq KHZ",
				interface_pwm_config_freq),
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

vsf_err_t virtual_interface_init(const char *vifs, const char mode)
{
	struct interfaces_info_t *interface_tmp;
	
	interface_tmp = find_interface_by_name(vifs);
	if ((vifs != NULL) && (NULL == interface_tmp))
	{
		return VSFERR_FAIL;
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
				return VSFERR_FAIL;
			}
		}
		
		if (NULL == cur_interface)
		{
			if (interface_init(NULL))
			{
				return VSFERR_FAIL;
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
		return VSFERR_FAIL;
	}
	return VSFERR_NONE;
}

vsf_err_t interface_init(const char *ifs)
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
		
		if (interface_tmp->init(interface_tmp))
		{
			return VSFERR_FAIL;
		}
		cur_interface = interface_tmp;
		if (interface_tmp->support_mask & IFS_POWER)
		{
			if (vss_run_script("tvcc.get"))
			{
				cur_interface = NULL;
				interface_tmp->fini();
				return VSFERR_FAIL;
			}
			return VSFERR_NONE;
		}
		else
		{
			return VSFERR_NONE;
		}
	}
	else
	{
		return VSFERR_FAIL;
	}
}

vsf_err_t interface_assert(struct interfaces_info_t **ifs)
{
	if (NULL == cur_interface)
	{
		if (interface_init(NULL) || (NULL == cur_interface))
		{
			cur_interface = NULL;
			vss_set_fatal_error();
			return VSFERR_FAIL;
		}
	}
	if (ifs != NULL)
	{
		*ifs = cur_interface;
	}
	return VSFERR_NONE;
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



// interfaces scripts
#define INTERFACE_ASSERT(ifs_mask, ifs_name)							\
	do{\
		if (interface_assert(&ifs) || (NULL == ifs))\
		{\
			LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");\
			return VSFERR_FAIL;\
		}\
		if (!(ifs->support_mask & ifs_mask))\
		{\
			LOG_ERROR(ERRMSG_NOT_SUPPORT, ifs_name " interface");\
			return VSFERR_FAIL;\
		}\
	} while (0)

// tvcc
VSS_HANDLER(interface_get_target_voltage)
{
	uint16_t voltage = 0;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_POWER, "power");
	
	if (ifs->target_voltage.get(0, &voltage))
	{
		return VSFERR_FAIL;
	}
	else
	{
		LOG_INFO(INFOMSG_TARGET_VOLTAGE, voltage / 1000.0);
		return VSFERR_NONE;
	}
}

VSS_HANDLER(interface_set_target_voltage)
{
	uint16_t voltage = 0;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_POWER, "power");
	
	voltage = (uint16_t)strtoul(argv[1], NULL, 0);
	if (ifs->target_voltage.set(0, voltage) || ifs->peripheral_commit())
	{
		return VSFERR_FAIL;
	}

	vss_run_script("tvcc.get");
	return VSFERR_NONE;
}

// gpio
VSS_HANDLER(interface_gpio_init)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 4);
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
	if (ifs->gpio.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (4 == argc)
	{
		return interface_gpio_config(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_gpio_fini)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
	return ifs->gpio.fini(0);
}

VSS_HANDLER(interface_gpio_config)
{
	uint32_t mask, io, pull, pull_en;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
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
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
	mask = (uint32_t)strtoul(argv[1], NULL, 0);
	value = (uint32_t)strtoul(argv[2], NULL, 0);
	
	return ifs->gpio.out(0, mask, value);
}

VSS_HANDLER(interface_gpio_in)
{
	uint32_t mask, value;
	vsf_err_t err = VSFERR_NONE;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_GPIO, "gpio");
	
	mask = (uint32_t)strtoul(argv[1], NULL, 0);
	
	err = ifs->gpio.in(0, mask, &value);
	if (!err)
	{
		err = ifs->peripheral_commit();
		if (!err)
		{
			LOG_INFO(INFOMSG_REG_08X, "GPIO", value);
		}
	}
	return err;
}

// jtag
VSS_HANDLER(interface_jtag_init)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_3(1, 2, 6);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	if (ifs->jtag_hl.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (argc > 1)
	{
		return interface_jtag_config(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_jtag_fini)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	return ifs->jtag_hl.fini(0);
}

VSS_HANDLER(interface_jtag_config)
{
	uint32_t khz = 0;
	uint8_t ub, ua;
	uint16_t bb, ba;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_2(2, 6);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	khz = (uint32_t)strtoul(argv[1], NULL, 0);
	ub = ua = 0;
	bb = ba = 0;
	if (argc > 2)
	{
		ub = (uint8_t)strtoul(argv[2], NULL, 0);
		ua = (uint8_t)strtoul(argv[3], NULL, 0);
		bb = (uint16_t)strtoul(argv[4], NULL, 0);
		ba = (uint16_t)strtoul(argv[5], NULL, 0);
	}
	
	return ifs->jtag_hl.config(0, khz, ub, ua, bb, ba);
}

VSS_HANDLER(interface_jtag_reset)
{
	struct interfaces_info_t *ifs = NULL;
	uint8_t tms = 0x7F;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	return ifs->jtag_hl.tms(0, &tms, 8);
}

VSS_HANDLER(interface_jtag_runtest)
{
	uint32_t cycles = 0;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	cycles = (uint32_t)strtoul(argv[1], NULL, 0);
	
	return ifs->jtag_hl.runtest(0, cycles);
}

VSS_HANDLER(interface_jtag_ir)
{
	uint32_t ir;
	uint16_t bitlen;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(3);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	bitlen = (uint16_t)strtoul(argv[1], NULL, 0);
	ir = (uint32_t)strtoul(argv[2], NULL, 0);
	ir = SYS_TO_LE_U32(ir);
	
	LOG_INFO(INFOMSG_REG_08X, "IR_out", ir);
	if (ifs->jtag_hl.ir(0, (uint8_t *)&ir, bitlen, 1, 1) ||
		ifs->peripheral_commit())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "jtag ir");
		return VSFERR_FAIL;
	}
	LOG_INFO(INFOMSG_REG_08X, "IR_in", ir);
	
	return VSFERR_NONE;
}

VSS_HANDLER(interface_jtag_dr)
{
	uint16_t bitlen, bytelen;
	uint8_t *dr = NULL, data_size;
	uint32_t data_num;
	struct interfaces_info_t *ifs = NULL;
	vsf_err_t err;
	
	VSS_CHECK_ARGC_MIN(4);
	INTERFACE_ASSERT(IFS_JTAG_HL, "jtag");
	
	bitlen = (uint16_t)strtoul(argv[1], NULL, 0);
	bytelen = (bitlen + 7) >> 3;
	data_size = (uint8_t)strtoul(argv[2], NULL, 0);
	data_num = (bytelen + data_size - 1) / data_size;
	
	VSS_CHECK_ARGC(3 + data_num);
	if ((data_size != 1) && (data_size != 2) && (data_size != 4))
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	err = vss_get_binary_buffer(argc - 3, &argv[3], data_size, data_num,
								(void**)&dr);
	if (!err)
	{
		err = ifs->jtag_hl.dr(0, dr, bitlen, 1, 1);
		if (!err)
		{
			err = ifs->peripheral_commit();
			if (!err)
			{
				LOG_BUF_STD(data_size, dr, data_num, LOG_INFO);
			}
		}
	}
	
	if (dr != NULL)
	{
		free(dr);
		dr = NULL;
	}
	return VSFERR_NONE;
}

// spi
VSS_HANDLER(interface_spi_init)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 5);
	INTERFACE_ASSERT(IFS_SPI, "spi");
	
	if (ifs->spi.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (4 == argc)
	{
		return interface_spi_config(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_spi_fini)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_SPI, "spi");
	
	return ifs->spi.fini(0);
}

VSS_HANDLER(interface_spi_config)
{
	uint32_t khz = 0;
	uint8_t mode, firstbit;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(4);
	INTERFACE_ASSERT(IFS_SPI, "spi");
	
	khz = (uint32_t)strtoul(argv[1], NULL, 0);
	mode = (uint8_t)strtoul(argv[2], NULL, 0);
	if (mode > SPI_MODE3)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "spi mode");
		return VSFERR_FAIL;
	}
	firstbit = (uint8_t)strtoul(argv[3], NULL, 0);
	firstbit = firstbit ? SPI_MSB_FIRST : SPI_LSB_FIRST;
	
	return ifs->spi.config(0, khz, mode | firstbit);
}

VSS_HANDLER(interface_spi_io)
{
	uint16_t data_size = 0;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(2);
	INTERFACE_ASSERT(IFS_SPI, "spi");
	
	data_size = (uint16_t)strtoul(argv[1], NULL, 0);

	VSS_CHECK_ARGC_2(2, 2 + data_size);
	if (0 == data_size)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	err = vss_get_binary_buffer(argc - 2, &argv[2], 1, data_size, (void**)&buff);
	if (!err)
	{
		err = ifs->spi.io(0, buff, buff, data_size);
		if (!err)
		{
			err = ifs->peripheral_commit();
			if (!err)
			{
				LOG_BUF_STD(1, buff, data_size, LOG_INFO);
			}
		}
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

// iic
VSS_HANDLER(interface_iic_init)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 3);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	if (ifs->i2c.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (3 == argc)
	{
		return interface_iic_config(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_iic_fini)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	return ifs->i2c.fini(0);
}

VSS_HANDLER(interface_iic_config)
{
	uint16_t khz = 0;
	uint16_t max_dly = 0;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(3);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
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
	vsf_err_t err = VSFERR_NONE;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	stop = (uint8_t)strtoul(argv[2], NULL, 0);
	nacklast = strtoul(argv[3], NULL, 0) > 0;
	data_size = (uint8_t)strtoul(argv[4], NULL, 0);
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return VSFERR_FAIL;
	}
	
	err = ifs->i2c.read(0, addr, buff, data_size, stop, nacklast);
	if (!err)
	{
		err = ifs->peripheral_commit();
		if (!err)
		{
			LOG_BUF_STD(1, buff, data_size, LOG_INFO);
		}
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_iic_write)
{
	uint8_t data_size = 0;
	uint8_t addr = 0;
	uint8_t stop = 0;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(4);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	addr = (uint8_t)strtoul(argv[1], NULL, 0);
	stop = (uint8_t)strtoul(argv[2], NULL, 0);
	data_size = (uint8_t)strtoul(argv[3], NULL, 0);
	
	VSS_CHECK_ARGC(4 + data_size);
	if (0 == data_size)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	err = vss_get_binary_buffer(argc - 4, &argv[4], 1, data_size, (void**)&buff);
	if (!err)
	{
		err = ifs->i2c.write(0, addr, buff, data_size, stop);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_iic_read_buff8)
{
	uint8_t data_size = 0;
	uint8_t slave_addr = 0, addr;
	bool nacklast = false;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(5);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	slave_addr = (uint8_t)strtoul(argv[1], NULL, 0);
	nacklast = strtoul(argv[2], NULL, 0) > 0;
	data_size = (uint8_t)strtoul(argv[3], NULL, 0);
	addr = (uint8_t)strtoul(argv[4], NULL, 0);
	buff = (uint8_t*)malloc(data_size);
	if (NULL == buff)
	{
		return VSFERR_FAIL;
	}
	
	err = ifs->i2c.write(0, slave_addr, &addr, 1, 0);
	if (!err)
	{
		err = ifs->i2c.read(0, slave_addr, buff, data_size, 1,
										nacklast);
		if (!err)
		{
			err = ifs->peripheral_commit();
			if (!err)
			{
				LOG_BUF_STD(1, buff, data_size, LOG_INFO);
			}
		}
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

VSS_HANDLER(interface_iic_write_buff8)
{
	uint8_t data_size = 0;
	uint8_t slave_addr = 0;
	uint8_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(4);
	INTERFACE_ASSERT(IFS_I2C, "iic");
	
	slave_addr = (uint8_t)strtoul(argv[1], NULL, 0);
	data_size = (uint8_t)strtoul(argv[2], NULL, 0);
	
	VSS_CHECK_ARGC(4 + data_size);
	if (0 == data_size)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "data_size");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	err = vss_get_binary_buffer(argc - 3, &argv[3], 1, data_size + 1,
								(void**)&buff);
	if (!err)
	{
		err = ifs->i2c.write(0, slave_addr, buff, data_size + 1, 1);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

// pwm
VSS_HANDLER(interface_pwm_init)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_2(1, 3);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	if (ifs->pwm.init(0))
	{
		return VSFERR_FAIL;
	}
	
	if (3 == argc)
	{
		return interface_pwm_config_mode(argc, argv);
	}
	return VSFERR_NONE;
}

VSS_HANDLER(interface_pwm_fini)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	return ifs->pwm.fini(0);
}

VSS_HANDLER(interface_pwm_config_mode)
{
	struct interfaces_info_t *ifs = NULL;
	uint8_t pushpull, polarity, mode;
	
	VSS_CHECK_ARGC(3);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	pushpull = (uint8_t)strtoul(argv[1], NULL, 0);
	polarity = (uint8_t)strtoul(argv[2], NULL, 0);
	
	mode = 0;
	if (pushpull)
	{
		mode |= PWM_OUTPP;
	}
	if (polarity)
	{
		mode |= PWM_OUTPOLARITY;
	}
	return ifs->pwm.config_mode(0, mode);
}

VSS_HANDLER(interface_pwm_config_freq)
{
	struct interfaces_info_t *ifs = NULL;
	uint16_t kHz;
	
	VSS_CHECK_ARGC(2);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	kHz = (uint16_t)strtoul(argv[1], NULL, 0);
	
	return ifs->pwm.config_freq(0, kHz);
}

VSS_HANDLER(interface_pwm_out)
{
	uint16_t count = 0;
	uint16_t *buff = NULL;
	vsf_err_t err = VSFERR_NONE;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC_MIN(3);
	INTERFACE_ASSERT(IFS_PWM, "pwm");
	
	count = (uint16_t)strtoul(argv[1], NULL, 0);
	
	VSS_CHECK_ARGC(2 + count);
	if (0 == count)
	{
		LOG_ERROR(ERRMSG_INVALID_TARGET, "count");
		vss_print_help(argv[0]);
		return VSFERR_FAIL;
	}
	
	err = vss_get_binary_buffer(argc - 2, &argv[2], 2, count, (void**)&buff);
	if (!err)
	{
		err = ifs->pwm.out(0, count, buff);
	}
	
	if (buff != NULL)
	{
		free(buff);
		buff = NULL;
	}
	return err;
}

// delay
VSS_HANDLER(interface_delay_us)
{
	uint16_t delay;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	if (interface_assert(&ifs) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return VSFERR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return ifs->delay.delayus(delay);
}

VSS_HANDLER(interface_delay_ms)
{
	uint16_t delay;
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(2);
	if (interface_assert(&ifs) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return VSFERR_FAIL;
	}
	
	delay = (uint16_t)strtoul(argv[1], NULL, 0);
	return ifs->delay.delayms(delay);
}

// commit
VSS_HANDLER(interface_commit)
{
	struct interfaces_info_t *ifs = NULL;
	
	VSS_CHECK_ARGC(1);
	if (interface_assert(&ifs) || (NULL == ifs))
	{
		LOG_ERROR(ERRMSG_FAILURE_HANDLE_DEVICE, "assert", "interface module");
		return VSFERR_FAIL;
	}
	
	return ifs->peripheral_commit();
}

