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
#include <string.h>
#include <stdlib.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"

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
	{'p', SET_FREQUENCY, SPI | GPIO},
	{'b', SET_FREQUENCY, SPI | GPIO},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(s5x);
LEAVE_PROGRAM_MODE_HANDLER(s5x);
ERASE_TARGET_HANDLER(s5x);
WRITE_TARGET_HANDLER(s5x);
READ_TARGET_HANDLER(s5x);
const struct program_functions_t s5x_program_functions = 
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(s5x), 
	LEAVE_PROGRAM_MODE_FUNCNAME(s5x), 
	ERASE_TARGET_FUNCNAME(s5x), 
	WRITE_TARGET_FUNCNAME(s5x), 
	READ_TARGET_FUNCNAME(s5x)
};

VSS_HANDLER(s5x_help)
{
	VSS_CHECK_ARGC(1);
	printf("\
Usage of %s:\n\
  -F,  --frequency <FREQUENCY>              set ISP frequency, in KHz\n\
  -m,  --mode <MODE>                        set program mode<b|p>\n\n", 
			CUR_TARGET_STRING);
	return ERROR_OK;
}

const struct vss_cmd_t s5x_notifier[] = 
{
	VSS_CMD(	"help",
				"print help information of current target for internal call",
				s5x_help),
	VSS_CMD_END
};

static uint16_t s5x_byte_delay_us = 500;

#define spi_init()				interfaces->spi.init(0)
#define spi_fini()				interfaces->spi.fini(0)
#define spi_conf(speed)			\
	interfaces->spi.config(0, (speed), SPI_CPOL_LOW, SPI_CPHA_1EDGE, \
							SPI_MSB_FIRST)
#define spi_io(out, bytelen, in)\
	interfaces->spi.io(0, (out), (in), (bytelen))

#define reset_init()			interfaces->gpio.init(0)
#define reset_fini()			interfaces->gpio.fini(0)
#define reset_output()			\
	interfaces->gpio.config(0, GPIO_SRST, GPIO_SRST, 0, GPIO_SRST)
#define reset_input()			\
	interfaces->gpio.config(0, GPIO_SRST, 0, GPIO_SRST, GPIO_SRST)
#define reset_set()				reset_output()
#define reset_clr()				reset_input()

#define delay_ms(ms)			interfaces->delay.delayms((ms) | 0x8000)
#define delay_us(us)			interfaces->delay.delayus((us) & 0x7FFF)

#define poll_start_once()		interfaces->poll.start(0, 0)
#define poll_end()				interfaces->poll.end()
#define poll_fail_unequ(o, m, v)\
	interfaces->poll.checkfail(POLL_CHECK_UNEQU, (o), 1, (m), (v))

#define commit()				interfaces->peripheral_commit()

static struct interfaces_info_t *interfaces = NULL;

ENTER_PROGRAM_MODE_HANDLER(s5x)
{
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	uint8_t cmd_buf[4];
	
	interfaces = context->prog;
	if (!context->pi->frequency)
	{
		context->pi->frequency = CUR_DEFAULT_FREQ;
	}
	s5x_byte_delay_us = 500;
	if (context->pi->wait_state)
	{
		s5x_byte_delay_us = context->pi->wait_state;
	}
	
	// init
	spi_init();
	reset_init();
	
	// enter program mode
	spi_conf(pi->frequency);
	
	// toggle reset
	reset_set();
	delay_ms(100);
	reset_input();
	delay_ms(30);
	reset_set();
	delay_ms(10);
	
	// enter into program mode command
	cmd_buf[0] = 0xAC;
	cmd_buf[1] = 0x53;
	cmd_buf[2] = 0x00;
	cmd_buf[3] = 0x00;
	// ret[3] should be 0x69
	poll_start_once();
	spi_io(cmd_buf, 4, NULL);
	if (param->param[S5X_PARAM_PE_OUT] != 0xFF)
	{
		poll_fail_unequ(0, 0xFF, (uint8_t)param->param[S5X_PARAM_PE_OUT]);
	}
	else
	{
		poll_fail_unequ(0, 0xFF, 0x69);
	}
	poll_end();
	if (ERROR_OK != commit())
	{
		LOG_ERROR(ERRMSG_FAILURE_ENTER_PROG_MODE);
		return ERRCODE_FAILURE_ENTER_PROG_MODE;
	}
	
	return ERROR_OK;
}

LEAVE_PROGRAM_MODE_HANDLER(s5x)
{
	REFERENCE_PARAMETER(context);
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

ERASE_TARGET_HANDLER(s5x)
{
	uint8_t cmd_buf[4];
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(area);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	cmd_buf[0] = 0xAC;
	cmd_buf[1] = 0x80;
	cmd_buf[2] = 0x00;
	cmd_buf[3] = 0x00;
	spi_io(cmd_buf, 4, NULL);
	delay_ms(500);
	if (ERROR_OK != commit())
	{
		return ERRCODE_FAILURE_OPERATION;
	}
	return ERROR_OK;
}

WRITE_TARGET_HANDLER(s5x)
{
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
			if (size == 256)
			{
				spi_io(cmd_buf, 2, NULL);
			}
			else
			{
				cmd_buf[2] = addr & 0xFF;
				spi_io(cmd_buf, 3, NULL);
			}
			
			for (i = 0; i < size; i++)
			{
				spi_io(&buff[i], 1, NULL);
				delay_us(s5x_byte_delay_us);
			}
			
			if (ERROR_OK != commit())
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			break;
		case S5X_BYTE_MODE:
			for (i = 0; i < size; i++)
			{
				cmd_buf[0] = 0x40;
				cmd_buf[1] = (uint8_t)((addr + i) >> 8);
				cmd_buf[2] = (uint8_t)((addr + i) >> 0);
				cmd_buf[3] = buff[i];
				spi_io(cmd_buf, 4, NULL);
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
		cmd_buf[1] = 0x10 + (buff[0] & 0x0F);
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, NULL);
		delay_ms(100);
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		break;
	case LOCK_CHAR:
		if ((buff[0] < 1) || (buff[0] > 4))
		{
			LOG_ERROR(ERRMSG_INVALID_VALUE, buff[0], "lock_value(1..4)");
			ret = ERRCODE_INVALID;
			break;
		}
		
		if (buff[0] > 1)
		{
			for (i = 1; i < buff[0]; i++)
			{
				if (buff[0] >= (uint8_t)i)
				{
					cmd_buf[0] = 0xAC;
					cmd_buf[1] = 0xE0 + (uint8_t)i;
					cmd_buf[2] = 0x00;
					cmd_buf[3] = 0x00;
					spi_io(cmd_buf, 4, NULL);
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

READ_TARGET_HANDLER(s5x)
{
	struct program_info_t *pi = context->pi;
	uint8_t cmd_buf[4], ret_buf[256 * 4], tmp8, lock;
	uint32_t i;
	RESULT ret = ERROR_OK;
	
	switch (area)
	{
	case CHIPID_CHAR:
		cmd_buf[0] = 0x28;
		cmd_buf[1] = 0x00;
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, &ret_buf[0]);
		cmd_buf[0] = 0x28;
		cmd_buf[1] = 0x01;
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, &ret_buf[4]);
		cmd_buf[0] = 0x28;
		cmd_buf[1] = 0x02;
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, &ret_buf[8]);
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		buff[0] = ret_buf[11];
		buff[1] = ret_buf[7];
		buff[2] = ret_buf[3];
		break;
	case APPLICATION_CHAR:
		switch (pi->mode)
		{
		case S5X_PAGE_MODE:
			cmd_buf[0] = 0x30;
			cmd_buf[1] = (addr >> 8) & 0xFF;
			if (size == 256)
			{
				spi_io(cmd_buf, 2, NULL);
			}
			else
			{
				cmd_buf[2] = addr & 0xFF;
				spi_io(cmd_buf, 3, NULL);
			}
			
			spi_io(buff, (uint16_t)size, buff);
			
			if (ERROR_OK != commit())
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			break;
		case S5X_BYTE_MODE:
			if (size > 256)
			{
				return ERROR_FAIL;
			}
			
			for (i = 0; i < size; i++)
			{
				cmd_buf[0] = 0x20;
				cmd_buf[1] = (uint8_t)((addr + i) >> 8);
				cmd_buf[2] = (uint8_t)((addr + i) >> 0);
				cmd_buf[3] = 0;
				spi_io(cmd_buf, 4, &ret_buf[4 * i]);
			}
			
			if (ERROR_OK != commit())
			{
				ret = ERRCODE_FAILURE_OPERATION;
				break;
			}
			for (i = 0; i < size; i++)
			{
				buff[i] = ret_buf[4 * i + 3];
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
		spi_io(cmd_buf, 4, &ret_buf[0]);
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		buff[0] = ret_buf[3] & 0x0F;
		break;
	case LOCK_CHAR:
		cmd_buf[0] = 0x24;
		cmd_buf[1] = 0x00;
		cmd_buf[2] = 0x00;
		cmd_buf[3] = 0x00;
		spi_io(cmd_buf, 4, &ret_buf[0]);
		if (ERROR_OK != commit())
		{
			ret = ERRCODE_FAILURE_OPERATION;
			break;
		}
		
		tmp8 = ret_buf[3];
		lock = 0;
		for (i = 0; i < 3; i++)
		{
			if (tmp8 & (1 << (i + 2)))
			{
				lock += 1;
			}
		}
		buff[0] = lock + 1;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

