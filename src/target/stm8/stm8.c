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
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{EEPROM_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_EP},
	{FUSE_CHAR, 0, 0, 0, 0, AREA_ATTR_WR},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t stm8_program_mode[] = 
{
	{'s', "", SWIM},
	{'i', USE_COMM, 0},
	{0, NULL, 0}
};

RESULT stm8_enter_program_mode(struct program_context_t *context);
RESULT stm8_leave_program_mode(struct program_context_t *context, 
								uint8_t success);
RESULT stm8_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size);
RESULT stm8_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size);
RESULT stm8_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size);
const struct program_functions_t stm8_program_functions = 
{
	NULL,			// execute
	stm8_enter_program_mode, 
	stm8_leave_program_mode, 
	stm8_erase_target, 
	stm8_write_target, 
	stm8_read_target
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
#define swim_in(d, L)			p->swim_in((d), (L))

#define delay_ms(ms)			p->delayms((ms) | 0x8000)
#define delay_us(us)			p->delayus((us) & 0x7FFF)

#define get_target_voltage(v)	prog->get_target_voltage(v)

#define commit()				p->peripheral_commit()

static struct programmer_info_t *p;

static RESULT stm8_swim_srst(void)
{
	return swim_out(STM8_SWIM_CMD_SRST, STM8_SWIM_CMD_BITLEN);
}

static RESULT stm8_swim_rotf(uint32_t addr, uint8_t *buff, uint8_t bytelen)
{
	swim_out(STM8_SWIM_CMD_ROTF, STM8_SWIM_CMD_BITLEN);
	swim_out(bytelen, 8);
	swim_out((addr >> 16) & 0xFF, 8);
	swim_out((addr >> 8) & 0xFF, 8);
	swim_out((addr >> 0) & 0xFF, 8);
	
	swim_in(buff, bytelen);
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

static void stm8_unlock_eeprom_option(void)
{
	stm8_swim_wotf_reg(STM8_REG_FLASH_DUKR, 0xAE);
	stm8_swim_wotf_reg(STM8_REG_FLASH_DUKR, 0x56);
}

static void stm8_unlock_flash(void)
{
	stm8_swim_wotf_reg(STM8_REG_FLASH_PUKR, 0x56);
	stm8_swim_wotf_reg(STM8_REG_FLASH_PUKR, 0xAE);
}

static RESULT stm8_erase_block(uint32_t block_addr)
{
	uint8_t buff[4];
	
	stm8_swim_wotf_reg(STM8_REG_FLASH_CR2, STM8_FLASH_CR2_ERASE);
	stm8_swim_wotf_reg(STM8_REG_FLASH_NCR2, (uint8_t)~STM8_FLASH_CR2_ERASE);
	memset(buff, 0, 4);
	stm8_swim_wotf(block_addr, buff, 4);
	delay_us(3300);
	return ERROR_OK;
}

static RESULT stm8_program_block(uint32_t block_addr, uint8_t *block_buff, uint8_t block_size)
{
	stm8_swim_wotf_reg(STM8_REG_FLASH_CR2, STM8_FLASH_CR2_FPRG);
	stm8_swim_wotf_reg(STM8_REG_FLASH_NCR2, (uint8_t)~STM8_FLASH_CR2_FPRG);
	stm8_swim_wotf(block_addr, block_buff, block_size);
	delay_us(3300);
	return ERROR_OK;
}

RESULT stm8_enter_program_mode(struct program_context_t *context)
{
	uint8_t i, test_buf0[7], test_buf1[6];
	RESULT ret = ERROR_OK;
	
	p = context->prog;
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		reset_init();
		reset_set();
		swim_set();
		if (ERROR_OK != commit())
		{
			ret = ERROR_FAIL;
			break;
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
			break;
		}
		
		// SWIM mode
		swim_init();
		swim_set_param(10, 20 + 2, 2 + 1);
		delay_ms(10);
		stm8_swim_srst();
		delay_ms(10);
		stm8_swim_wotf_reg(STM8_REG_SWIM_CSR, 
							STM8_SWIM_CSR_SAFT_MASK | STM8_SWIM_CSR_SWIM_DM);
		delay_ms(10);
		reset_set();
		stm8_swim_rotf(0x0067F0, test_buf0, 6);
		// enable high speed
		stm8_swim_wotf_reg(STM8_REG_SWIM_CSR, 
							STM8_SWIM_CSR_SAFT_MASK | STM8_SWIM_CSR_SWIM_DM 
							| STM8_SWIM_CSR_HS | STM8_SWIM_CSR_RST);
		swim_set_param(10, 8, 2 + 1);
		delay_ms(10);
		stm8_swim_rotf(0x0067F0, test_buf1, 6);
		stm8_unlock_eeprom_option();
		stm8_unlock_flash();
		if ((ERROR_OK != commit()) || memcmp(test_buf0, test_buf1, 6))
		{
			ret = ERROR_FAIL;
		}
		else
		{
			test_buf0[6] = '\0';
			LOG_INFO(_GETTEXT("is this chip ID: %s\n"), test_buf0);
		}
		break;
	case STM8_ISP:
		ret = ERRCODE_NOT_SUPPORT;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

RESULT stm8_leave_program_mode(struct program_context_t *context, 
								uint8_t success)
{
	RESULT ret = ERROR_OK;
	REFERENCE_PARAMETER(success);
	p = context->prog;
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		stm8_swim_srst();
		swim_fini();
		reset_input();
		swim_input();
		reset_fini();
		ret = commit();
		break;
	case STM8_ISP:
		ret = ERRCODE_NOT_SUPPORT;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

RESULT stm8_erase_target(struct program_context_t *context, char area, 
							uint32_t addr, uint32_t page_size)
{
	RESULT ret = ERROR_OK;
	REFERENCE_PARAMETER(page_size);
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		switch (area)
		{
		case APPLICATION_CHAR:
		case EEPROM_CHAR:
			ret = stm8_erase_block(addr);
			break;
		default:
			ret = ERROR_FAIL;
			break;
		}
		break;
	case STM8_ISP:
		ret = ERRCODE_NOT_SUPPORT;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}
RESULT stm8_write_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	RESULT ret = ERROR_OK;
	REFERENCE_PARAMETER(page_size);
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		switch (area)
		{
		case APPLICATION_CHAR:
		case EEPROM_CHAR:
			ret = stm8_program_block(addr, buff, (uint8_t)page_size);
			break;
		default:
			ret = ERROR_FAIL;
			break;
		}
		break;
	case STM8_ISP:
		ret = ERRCODE_NOT_SUPPORT;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}
RESULT stm8_read_target(struct program_context_t *context, char area, 
							uint32_t addr, uint8_t *buff, uint32_t page_size)
{
	uint8_t fuse_page[128];
	RESULT ret = ERROR_OK;
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		switch (area)
		{
		case CHIPID_CHAR:
			context->pi->chip_id = 0;
			break;
		case APPLICATION_CHAR:
		case EEPROM_CHAR:
			stm8_swim_rotf(addr, buff, (uint8_t)page_size);
			ret = commit();
			break;
		case FUSE_CHAR:
			stm8_swim_rotf(0x004800, fuse_page, sizeof(fuse_page));
			ret = commit();
			if (ret != ERROR_OK)
			{
				break;
			}
			buff[0] = fuse_page[0];
			buff[1] = fuse_page[1];
			buff[2] = fuse_page[3];
			buff[3] = fuse_page[5];
			buff[4] = fuse_page[7];
			buff[5] = fuse_page[9];
			buff[6] = fuse_page[13];
			buff[7] = fuse_page[126];
			break;
		default:
			ret = ERROR_FAIL;
			break;
		}
		break;
	case STM8_ISP:
		ret = ERRCODE_NOT_SUPPORT;
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

