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

#include <stdio.h>

#include "app_cfg.h"
#include "app_type.h"

#include "..\dal_internal.h"
#include "mic2826_cfg.h"
#include "mic2826.h"

#define MIC2826_IIC_ADDR					0xB4

#define MIC2826_REG_ENABLE					0x00
#define MIC2826_REG_STATUS					0x01
#define MIC2826_REG_DCDC					0x02
#define MIC2826_REG_LDO1					0x03
#define MIC2826_REG_LDO2					0x04
#define MIC2826_REG_LDO3					0x05

#define MIC2826_REG_ENABLE_POAF				0x20
#define MIC2826_REG_ENABLE_SEQCNT			0x10
#define MIC2826_REG_LDO3_EN					0x08
#define MIC2826_REG_LDO2_EN					0x04
#define MIC2826_REG_LDO1_EN					0x02
#define MIC2826_REG_DCDC_EN					0x01

static uint8_t mic2826_ldo_regmap[] = 
{
	0x00, 0x0B, 0x14, 0x1D, 0x25, 0x2E, 0x37, 0x3E, 
	0x45, 0x4C, 0x52, 0x57, 0x5C, 0x61, 0x65, 0x69, 
	0x6D, 0x72, 0x79, 0x7F, 0x85, 0x8B, 0x91, 0x96, 
	0x9A, 0x9F, 0xA4, 0xA8, 0xAC, 0xB0, 0xB4, 0xB7, 
	0xBA, 0xBD, 0xC1, 0xC4, 0xC7, 0xC9, 0xCC, 0xCE, 
	0xD1, 0xD3, 0xD6, 0xD8, 0xDA, 0xDC, 0xDE, 0xE1, 
	0xE3, 0xE6, 0xE8
};

static RESULT mic2826_dcdc_voltage2reg(uint16_t mV, uint8_t *reg)
{
	uint8_t reg_tmp;
	
	// 800 - 1175: 25 step
	// 1200 - 1800: 50 step
	if ((mV >= 1200) && (mV <= 1800))
	{
		reg_tmp = (uint8_t)((1200 - 800) / 25 + (mV - 1200) / 50);
	}
	else if ((mV >= 800) && (mV < 1200))
	{
		reg_tmp = (uint8_t)((mV - 800) / 25);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	if (NULL != reg)
	{
		*reg = reg_tmp;
	}
	return ERROR_OK;
}

static RESULT mic2826_ldo_voltage2reg(uint16_t mV, uint8_t *reg)
{
	uint8_t reg_tmp;
	
	// 800 - 3300: 25 step
	if ((mV >= 800) && (mV <= 3300))
	{
		reg_tmp = (uint8_t)((mV - 800) / 50);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	if (NULL != reg)
	{
		*reg = mic2826_ldo_regmap[reg_tmp];
	}
	return ERROR_OK;
}

static RESULT mic2826_init(uint16_t kHz)
{
	interfaces->i2c.init(MIC2826_IIC_IDX);
	interfaces->i2c.config(MIC2826_IIC_IDX, kHz, 0, 0xFFFF);
	return interfaces->peripheral_commit();
}

static RESULT mic2826_fini(void)
{
	interfaces->i2c.fini(MIC2826_IIC_IDX);
	return interfaces->peripheral_commit();
}

static RESULT mic2826_config(uint8_t channel, uint16_t mV)
{
	uint8_t reg[2], en_reg;
	
	if (channel > MIC2826_CHANNEL_LDO3)
	{
		return ERROR_FAIL;
	}
	
	reg[0] = MIC2826_REG_ENABLE;
	if (ERROR_OK != 
		interfaces->i2c.write(MIC2826_IIC_IDX, MIC2826_IIC_ADDR, reg, 1, 0))
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != 
		interfaces->i2c.read(MIC2826_IIC_IDX, MIC2826_IIC_ADDR, &en_reg, 1, 1))
	{
		return ERROR_FAIL;
	}
	if (ERROR_OK != interfaces->peripheral_commit())
	{
		return ERROR_FAIL;
	}
	
	if (mV >= 800)
	{
		reg[0] = MIC2826_REG_DCDC + channel;
		if (MIC2826_CHANNEL_DCDC == channel)
		{
			if (ERROR_OK != mic2826_dcdc_voltage2reg(mV, &reg[1]))
			{
				return ERROR_FAIL;
			}
		}
		else
		{
			if (ERROR_OK != mic2826_ldo_voltage2reg(mV, &reg[1]))
			{
				return ERROR_FAIL;
			}
		}
		interfaces->delay.delayus(100);
		if (ERROR_OK != 
			interfaces->i2c.write(MIC2826_IIC_IDX, MIC2826_IIC_ADDR, reg, 2, 1))
		{
			return ERROR_FAIL;
		}
		
		if (!(en_reg & (1 << channel)))
		{
			reg[0] = MIC2826_REG_ENABLE;
			reg[1] = en_reg | (1 << channel);
			if (ERROR_OK != 
				interfaces->i2c.write(MIC2826_IIC_IDX, MIC2826_IIC_ADDR, reg, 
										2, 1))
			{
				return ERROR_FAIL;
			}
		}
		if (ERROR_OK != interfaces->peripheral_commit())
		{
			return ERROR_FAIL;
		}
	}
	else if (0 == mV)
	{
		reg[0] = MIC2826_REG_ENABLE;
		reg[1] = en_reg & ~(1 << channel);
		if (ERROR_OK != 
			interfaces->i2c.write(MIC2826_IIC_IDX, MIC2826_IIC_ADDR, reg, 2, 1))
		{
			return ERROR_FAIL;
		}
		if (ERROR_OK != interfaces->peripheral_commit())
		{
			return ERROR_FAIL;
		}
	}
	else
	{
		return ERROR_FAIL;
	}
	return ERROR_OK;
}

const struct mic2826_t mic2826 = 
{
	mic2826_init,
	mic2826_fini,
	mic2826_config
};

