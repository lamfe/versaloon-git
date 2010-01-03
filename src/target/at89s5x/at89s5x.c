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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "memlist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "at89s5x.h"
#include "at89s5x_internal.h"

#define CUR_TARGET_STRING			S5X_STRING
#define CUR_DEFAULT_FREQ			S5X_DEFAULT_FREQ

const struct program_area_map_t s5x_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR},
	{LOCK_CHAR, 0, 0, 0, 0, AREA_ATTR_WR},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t s5x_program_mode[] = 
{
	{'b', SET_FREQUENCY, SPI | GPIO},
	{'p', SET_FREQUENCY, SPI | GPIO},
	{0, NULL, 0}
};

RESULT s5x_enter_program_mode(struct program_context_t *context);
RESULT s5x_leave_program_mode(struct program_context_t *context, 
								uint8_t success);
RESULT s5x_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size);
RESULT s5x_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size);
RESULT s5x_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size);
const struct program_functions_t s5x_program_functions = 
{
	NULL,			// execute
	s5x_enter_program_mode, 
	s5x_leave_program_mode, 
	s5x_erase_target, 
	s5x_write_target, 
	s5x_read_target
};

static uint16_t s5x_byte_delay_us = 500;

void s5x_usage(void)
{
	printf("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>              set ISP frequency, in KHz\n\
  -m,  --mode <MODE>                        set program mode<b|p>\n\n", 
			CUR_TARGET_STRING);
}

RESULT s5x_parse_argument(char cmd, const char *argu)
{
	switch(cmd)
	{
	case 'h':
		s5x_usage();
		break;
	case 'w':
		// set Wait time
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		s5x_byte_delay_us = (uint16_t)strtoul(argu, NULL, 0);
		if (s5x_byte_delay_us & 0x8000)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		
		break;
	default:
		return ERROR_FAIL;
		break;
	}

	return ERROR_OK;
}

#define spi_init()				prog->spi_init()
#define spi_fini()				prog->spi_fini()
#define spi_conf(speed)			prog->spi_config((speed), SPI_CPOL_LOW, \
												 SPI_CPHA_1EDGE, SPI_MSB_FIRST)
#define spi_io(out, outlen, in, inpos, inlen)	\
								prog->spi_io((out), (in), (outlen), \
											 (inpos), (inlen))

#define reset_init()			prog->gpio_init()
#define reset_fini()			prog->gpio_fini()
#define reset_output()			prog->gpio_config(GPIO_SRST, GPIO_SRST, 0)
#define reset_input()			prog->gpio_config(GPIO_SRST, 0, GPIO_SRST)
#define reset_set()				prog->gpio_out(GPIO_SRST, GPIO_SRST)
#define reset_clr()				reset_input()

#define delay_ms(ms)			prog->delayms((ms) | 0x8000)
#define delay_us(us)			prog->delayus((us) & 0x7FFF)

#define commit()				prog->peripheral_commit()

RESULT s5x_enter_program_mode(struct program_context_t *context)
{
	uint8_t cmd_buf[4];
	uint8_t poll_value;
	
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	struct programmer_info_t *prog = context->prog;
	if (!context->pi->frequency)
	{
		context->pi->frequency = CUR_DEFAULT_FREQ;
	}
	
	// init
	spi_init();
	reset_init();
	
	// enter program mode
	spi_conf(pi->frequency);
	
	// toggle reset
	reset_set();
	reset_output();
	delay_ms(100);
	reset_input();
	delay_ms(30);
	reset_set();
	reset_output();
	delay_ms(10);
	
	// enter into program mode command
	cmd_buf[0] = 0xAC;
	cmd_buf[1] = 0x53;
	cmd_buf[2] = 0x00;
	cmd_buf[3] = 0x00;
	poll_value = 0;
	// ret[3] should be 0x69
	spi_io(cmd_buf, 4, &poll_value, 3, 1);
	if ((ERROR_OK != commit()) 
		|| ((param->param[S5X_PARAM_PE_OUT] != 0xFF) 
			&& (param->param[S5X_PARAM_PE_OUT] != poll_value)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_ENTER_PROG_MODE));
		return ERRCODE_FAILURE_ENTER_PROG_MODE;
	}
	
	return ERROR_OK;
}

RESULT s5x_leave_program_mode(struct program_context_t *context, 
								uint8_t success)
{
	struct programmer_info_t *prog = context->prog;
	
	REFERENCE_PARAMETER(success);
	
	reset_input();
	reset_fini();
	spi_fini();
	if (ERROR_OK != commit())
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	return ERROR_OK;
}

RESULT s5x_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size)
{
	struct programmer_info_t *prog = context->prog;
	uint8_t cmd_buf[4];
	
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(page_size);
	
	cmd_buf[0] = 0xAC;
	cmd_buf[1] = 0x80;
	cmd_buf[2] = 0x00;
	cmd_buf[3] = 0x00;
	spi_io(cmd_buf, 4, NULL, 0, 0);
	delay_ms(500);
	if (ERROR_OK != commit())
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	return ERROR_OK;
}

RESULT s5x_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	struct programmer_info_t *prog = context->prog;
	struct program_info_t *pi = context->pi;
	uint8_t cmd_buf[4];
	uint32_t i;
	RESULT ret = ERROR_OK;
	
	switch (area)
	{
	case APPLICATION_CHAR:
		switch (pi->mode)
		{
		case S5X_PAGE_MODE:
			cmd_buf[0] = 0x50;
			cmd_buf[1] = (addr >> 8) & 0xFF;
			if (page_size == 256)
			{
				spi_io(cmd_buf, 2, NULL, 0, 0);
			}
			else
			{
				cmd_buf[2] = addr & 0xFF;
				spi_io(cmd_buf, 3, NULL, 0, 0);
			}
			
			for (i = 0; i < page_size; i++)
			{
				spi_io(&buff[addr + i], 1, NULL, 0, 0);
				delay_us(s5x_byte_delay_us);
			}
			
			if (ERROR_OK != commit())
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			break;
		case S5X_BYTE_MODE:
			for (i = 0; i < page_size; i++)
			{
				cmd_buf[0] = 0x40;
				cmd_buf[1] = (uint8_t)((addr + i) >> 8);
				cmd_buf[2] = (uint8_t)((addr + i) >> 0);
				cmd_buf[3] = buff[i];
				spi_io(cmd_buf, 4, NULL, 0, 0);
				delay_us(s5x_byte_delay_us);
			}
			
			if (ERROR_OK != commit())
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			break;
		default:
			ret = ERROR_FAIL;
			break;
		}
		break;
	case FUSE_CHAR:
		cmd_buf[0] = 0xAC;
		cmd_buf[1] = 0x10 + (pi->program_areas[FUSE_IDX].value & 0x0F);
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, NULL, 0, 0);
		delay_ms(100);
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	case LOCK_CHAR:
		if ((pi->program_areas[LOCK_IDX].value < 1) 
			|| (pi->program_areas[LOCK_IDX].value > 4))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), 
								(uint32_t)pi->program_areas[LOCK_IDX].value, 
								"lock_value(1..4)");
			ret = ERRCODE_INVALID;
			break;
		}
		
		pi->program_areas[LOCK_IDX].value--;
		if (pi->program_areas[LOCK_IDX].value > 0)
		{
			for (i = 1; i < 4; i++)
			{
				if (pi->program_areas[LOCK_IDX].value >= (uint32_t)i)
				{
					cmd_buf[0] = 0xAC;
					cmd_buf[1] = 0xE0 + (uint8_t)i;
					cmd_buf[2] = 0x00;
					cmd_buf[3] = 0x00;
					spi_io(cmd_buf, 4, NULL, 0, 0);
					delay_ms(100);
				}
			}
			
			if (ERROR_OK != commit())
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
		}
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

RESULT s5x_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	struct programmer_info_t *prog = context->prog;
	struct program_info_t *pi = context->pi;
	uint8_t cmd_buf[4], chip_id[3], tmp8, lock;
	uint32_t i;
	RESULT ret = ERROR_OK;
	
	switch (area)
	{
	case CHIPID_CHAR:
		cmd_buf[0] = 0x28;
		cmd_buf[1] = 0x00;
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, &chip_id[2], 3, 1);
		cmd_buf[0] = 0x28;
		cmd_buf[1] = 0x01;
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, &chip_id[1], 3, 1);
		cmd_buf[0] = 0x28;
		cmd_buf[1] = 0x02;
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, &chip_id[0], 3, 1);
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		pi->chip_id = ((chip_id[2] & 0xFF) << 0) | ((chip_id[1] & 0xFF) << 8) 
					   | ((chip_id[0] & 0xFF) << 16);
		break;
	case APPLICATION_CHAR:
		switch (pi->mode)
		{
		case S5X_PAGE_MODE:
			cmd_buf[0] = 0x30;
			cmd_buf[1] = (addr >> 8) & 0xFF;
			if (page_size == 256)
			{
				spi_io(cmd_buf, 2, NULL, 0, 0);
			}
			else
			{
				cmd_buf[2] = addr & 0xFF;
				spi_io(cmd_buf, 3, NULL, 0, 0);
			}
			
			spi_io(buff, (uint16_t)page_size, buff, 0, 
				   (uint16_t)page_size);
			
			if (ERROR_OK != commit())
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			break;
		case S5X_BYTE_MODE:
			for (i = 0; i < page_size; i++)
			{
				cmd_buf[0] = 0x20;
				cmd_buf[1] = (uint8_t)((addr + i) >> 8);
				cmd_buf[2] = (uint8_t)((addr + i) >> 0);
				cmd_buf[3] = 0;
				spi_io(cmd_buf, 4, buff + i, 3, 1);
			}
			
			if (ERROR_OK != commit())
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			break;
		default:
			break;
		}
		break;
	case FUSE_CHAR:
		cmd_buf[0] = 0x21;
		cmd_buf[1] = 0x00;
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, &tmp8, 3, 1);
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		tmp8 &= 0x0F;
		break;
	case LOCK_CHAR:
		cmd_buf[0] = 0x24;
		cmd_buf[1] = 0x00;
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, &tmp8, 3, 1);
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		lock = 0;
		for (i = 0; i < 3; i++)
		{
			if (tmp8 & (1 << (i + 2)))
			{
				lock += 1;
			}
		}
		pi->program_areas[LOCK_IDX].value = lock;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

