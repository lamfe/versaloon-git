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

#include "stm8.h"
#include "stm8_internal.h"

#define CUR_TARGET_STRING			STM8_STRING

const struct program_area_map_t stm8_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0},
	{0, 0, 0, 0}
};

const struct program_mode_t stm8_program_mode[] = 
{
	{'s', "", SWIM},
	{'i', USE_COMM, 0},
	{0, NULL, 0}
};

static void stm8_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<s|i>\n\n",
			CUR_TARGET_STRING);
}

RESULT stm8_parse_argument(char cmd, const char *argu)
{
	argu = argu;
	
	switch (cmd)
	{
	case 'h':
		stm8_usage();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}





#define reset_init()			p->gpio_init()
#define reset_fini()			p->gpio_fini()
#define reset_output()			p->gpio_config(SWIM_RST_PIN, SWIM_RST_PIN, 0)
#define reset_input()			p->gpio_config(SWIM_RST_PIN, 0, SWIM_RST_PIN)
#define reset_set()				reset_input()
#define reset_clr()				do{\
									p->gpio_out(SWIM_RST_PIN, 0);\
									reset_output();\
								}while(0)

#define swim_input()			p->gpio_config(SWIM_PIN, 0, SWIM_PIN)
#define swim_output()			p->gpio_config(SWIM_PIN, SWIM_PIN, 0)
#define swim_set()				swim_input()
#define swim_clr()				do{\
									p->gpio_out(SWIM_PIN, 0);\
									swim_output();\
								}while(0)

#define swim_init()				p->swim_init()
#define swim_fini()				p->swim_fini()
#define swim_set_param(m,c0,c1)	p->swim_set_param((m), (c0), (c1))
#define swim_out(d, l)			p->swim_out((d), (l))
#define swim_in(d, l)			p->swim_in((d), (l))

#define delay_ms(ms)			p->delayms((ms) | 0x8000)
#define delay_us(us)			p->delayus((us) & 0x7FFF)

#define commit()				p->peripheral_commit()

static struct programmer_info_t *p;

static RESULT stm8_swim_srst(void)
{
	return swim_out(STM8_SWIM_CMD_SRST, STM8_SWIM_CMD_BITLEN);
}

static RESULT stm8_swim_rotf(uint32_t addr, uint8_t *buff, uint8_t bytelen)
{
	uint8_t i;
	
	swim_out(STM8_SWIM_CMD_ROTF, STM8_SWIM_CMD_BITLEN);
	swim_out(bytelen, 8);
	swim_out((addr >> 16) & 0xFF, 8);
	swim_out((addr >> 8) & 0xFF, 8);
	swim_out((addr >> 0) & 0xFF, 8);
	
	for (i = 0; i < bytelen; i++)
	{
		swim_in(&buff[i], 8);
	}
	return ERROR_OK;
}

static RESULT stm8_swim_wotf(uint32_t addr, uint8_t *buff, uint8_t bytelen)
{
	uint8_t i;
	
	swim_out(STM8_SWIM_CMD_WOTF, STM8_SWIM_CMD_BITLEN);
	swim_out(bytelen, 8);
	swim_out((addr >> 16) & 0xFF, 8);
	swim_out((addr >> 8) & 0xFF, 8);
	swim_out((addr >> 0) & 0xFF, 8);
	
	for (i = 0; i < bytelen; i++)
	{
		swim_out(buff[i], 8);
	}
	return ERROR_OK;
}

static RESULT stm8_swim_wotf_reg(uint32_t addr, uint8_t value)
{
	return stm8_swim_wotf(addr, &value, 1);
}

static RESULT stm8_swim_program(struct operation_t operations, 
					struct program_info_t *pi, struct programmer_info_t *prog)
{
	uint8_t page_buf[STM8_FLASH_PAGESIZE];
	uint32_t i;
	RESULT ret;
	
	p = prog;
	
	reset_init();
	reset_set();
	swim_set();
	if (ERROR_OK != commit())
	{
		ret = ERROR_FAIL;
		goto leave_program_mode;
	}
	
	reset_clr();
	delay_ms(20);
	reset_set();
	delay_ms(20);
	reset_clr();
	delay_ms(10);
	swim_clr();
	delay_ms(1);
	for (i = 0; i < 4; i++)
	{
		swim_set();
		delay_us(500);
		swim_clr();
		delay_us(500);
	}
	for (i = 0; i < 4; i++)
	{
		swim_set();
		delay_us(250);
		swim_clr();
		delay_us(250);
	}
	swim_set();
	if (ERROR_OK != commit())
	{
		ret = ERROR_FAIL;
		goto leave_program_mode;
	}
	
	// SWIM mode
	swim_init();
	swim_set_param(10, 20, 2);
	delay_ms(15);
	stm8_swim_srst();
	delay_ms(10);
	stm8_swim_wotf_reg(STM8_REG_SWIM_CSR, 
						STM8_SWIM_CSR_SAFT_MASK | STM8_SWIM_CSR_SWIM_DM);
	delay_ms(10);
	reset_set();
	stm8_swim_rotf(0x0067F0, page_buf, 6);
	if (ERROR_OK != commit())
	{
		ret = ERROR_FAIL;
		goto leave_program_mode;
	}
	
leave_program_mode:
	swim_fini();
	reset_input();
	swim_input();
	reset_fini();
	commit();
	return ERROR_OK;
}

RESULT stm8_program(struct operation_t operations, 
					struct program_info_t *pi, struct programmer_info_t *prog)
{
	RESULT ret = ERROR_OK;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	operations = operations;
	pi = pi;
	prog = prog;
	
	switch (program_mode)
	{
	case STM8_SWIM:
		ret = stm8_swim_program(operations, pi, prog);
		break;
	case STM8_ISP:
/*
		pi->chip_name = (char *)comisp_chips_param[COMISP_STM8].chip_name;
		pi->chip_type = "comisp";
		ret = comisp_init(pi, prog);
		if (ERROR_OK != ret)
		{
			goto exit_stm8isp;
		}
		ret = comisp_program(operations, pi, prog);
		
exit_stm8isp:
		comisp_fini(pi, prog);
*/		break;
	}
	
	return ret;
}

