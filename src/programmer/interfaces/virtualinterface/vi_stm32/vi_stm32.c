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

#include "app_type.h"
#include "app_io.h"
#include "app_err.h"
#include "app_log.h"

#include "../../interfaces.h"
#include "target.h"
#include "scripts.h"

#include "adi_v5p1.h"
#include "cm3_common.h"
#include "cm3_internal.h"

#define VI_STM32_STRING						"vi_stm32"

VSS_HANDLER(vi_stm32_support);
VSS_HANDLER(vi_stm32_help);
const struct vss_cmd_t vi_stm32_notifier[] = 
{
	VSS_CMD(	"support",
				"print support information, format: support/S",
				vi_stm32_support),
	VSS_CMD(	"S",
				"print support information, format: support/S",
				vi_stm32_support),
	VSS_CMD(	"help",
				"print help information, format: help/h",
				vi_stm32_help),
	VSS_CMD(	"h",
				"print help information, format: help/h",
				vi_stm32_help),
	VSS_CMD_END
};

#define VI_STM32_MODE_JTAG					0
#define VI_STM32_MODE_SWD					1
const struct program_mode_t vi_stm32_mode[] = 
{
	{'j', SET_FREQUENCY, IFS_JTAG_HL},
	{'s', "", IFS_SWD},
	{0, NULL, 0}
};

VSS_HANDLER(vi_stm32_help)
{
	VSS_CHECK_ARGC(1);
		PRINTF("Usage of %s:\n\n", VI_STM32_STRING);
	return ERROR_OK;
}

VSS_HANDLER(vi_stm32_support)
{
	VSS_CHECK_ARGC(1);
	PRINTF("%s: virtual programmer on stm32.\n", VI_STM32_STRING);
	return ERROR_OK;
}



static RESULT vi_stm32_init(void *p)
{
	struct program_info_t *pi = &program_info;
	uint32_t mode = *(uint32_t *)p;
	struct adi_dpif_t dp;
	
	switch (mode)
	{
	case VI_STM32_MODE_JTAG:
		// JTAG;
		dp.type = ADI_DP_JTAG;
		dp.dpif_setting.dpif_jtag_setting.jtag_khz = 
			cm3_chips_param[CM3_PARAM_IDX_STM32].jtag_khz;
		dp.dpif_setting.dpif_jtag_setting.ub = 
			cm3_chips_param[CM3_PARAM_IDX_STM32].jtag_pos.ub + pi->jtag_pos.ub;
		dp.dpif_setting.dpif_jtag_setting.ua = 
			cm3_chips_param[CM3_PARAM_IDX_STM32].jtag_pos.ua + pi->jtag_pos.ua;
		dp.dpif_setting.dpif_jtag_setting.bb = 
			cm3_chips_param[CM3_PARAM_IDX_STM32].jtag_pos.bb + pi->jtag_pos.bb;
		dp.dpif_setting.dpif_jtag_setting.ba = 
			cm3_chips_param[CM3_PARAM_IDX_STM32].jtag_pos.ba + pi->jtag_pos.ba;
		break;
	case VI_STM32_MODE_SWD:
		// SWD
		dp.type = ADI_DP_SWD;
		dp.dpif_setting.dpif_swd_setting.swd_trn = 
				cm3_chips_param[CM3_PARAM_IDX_STM32].swd_trn;
		dp.dpif_setting.dpif_swd_setting.swd_dly = 
				cm3_chips_param[CM3_PARAM_IDX_STM32].swd_delay;
		dp.dpif_setting.dpif_swd_setting.swd_retry = 0;
		break;
	default:
		return ERROR_FAIL;
	}
	if (ERROR_OK != cm3_dp_init(cur_real_interface, &dp))
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

static RESULT vi_stm32_fini(void)
{
	return cur_real_interface->fini();
}

// SPI
RESULT vi_stm32_spi_init(uint8_t interface_index)
{
	return ERROR_OK;
}

RESULT vi_stm32_spi_fini(uint8_t interface_index)
{
	return ERROR_OK;
}

RESULT vi_stm32_spi_config(uint8_t interface_index, uint16_t kHz, uint8_t cpol, 
					   uint8_t cpha, uint8_t firstbit)
{
	return ERROR_OK;
}

RESULT vi_stm32_spi_io(uint8_t interface_index, uint8_t *out, uint8_t *in, 
						uint16_t bytelen)
{
	return ERROR_OK;
}

// GPIO
RESULT vi_stm32_gpio_init(uint8_t interface_index)
{
	return ERROR_OK;
}

RESULT vi_stm32_gpio_fini(uint8_t interface_index)
{
	return ERROR_OK;
}

RESULT vi_stm32_gpio_config(uint8_t interface_index, uint32_t mask, 
							uint32_t dir_mask, uint32_t pull_en_mask, 
							uint32_t input_pull_mask)
{
	return ERROR_OK;
}

RESULT vi_stm32_gpio_in(uint8_t interface_index, uint32_t mask, uint32_t *value)
{
	return ERROR_OK;
}

RESULT vi_stm32_gpio_out(uint8_t interface_index, uint32_t mask, uint32_t value)
{
	return ERROR_OK;
}

// delay
RESULT vi_stm32_delayms(uint16_t ms)
{
	return ERROR_OK;
}

RESULT vi_stm32_delayus(uint16_t us)
{
	return ERROR_OK;
}

static RESULT vi_stm32_peripheral_commit(void)
{
	return ERROR_OK;
}

struct interfaces_info_t vi_stm32_interfaces = 
{
	VI_STM32_STRING,
	vi_stm32_notifier,
	NULL,
	
	true,
	vi_stm32_mode,
	
	vi_stm32_init,
	vi_stm32_fini,
	
	IFS_SPI | IFS_GPIO,
	
	{	// target_voltage
		NULL, NULL
	},
	{	// usart
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// spi
		vi_stm32_spi_init,
		vi_stm32_spi_fini,
		vi_stm32_spi_config,
		vi_stm32_spi_io
	},
	{	// gpio
		vi_stm32_gpio_init,
		vi_stm32_gpio_fini,
		vi_stm32_gpio_config,
		vi_stm32_gpio_out,
		vi_stm32_gpio_in
	},
	{	// delay
		vi_stm32_delayms,
		vi_stm32_delayus
	},
	{	// issp
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// swd
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// jtag_hl
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// jtag_ll
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// jtag_raw
		NULL, NULL, NULL, NULL
	},
	{	// msp430_jtag
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// msp430_sbw
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// c2
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// i2c
		NULL, NULL, NULL, NULL, NULL
	},
	{	// lpcicp
		NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// swim
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
	},
	{	// bdm
		NULL, NULL, NULL, NULL
	},
	{	// dusi
		NULL, NULL, NULL, NULL
	},
	{	// microwire
		NULL, NULL, NULL, NULL, NULL
	},
	{	// pwm
		NULL, NULL, NULL, NULL, NULL
	},
	{	// poll
		NULL, NULL, NULL, NULL, NULL
	},
	vi_stm32_peripheral_commit
};

