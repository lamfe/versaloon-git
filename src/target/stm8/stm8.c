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

struct program_area_map_t stm8_program_area_map[] = 
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

ENTER_PROGRAM_MODE_HANDLER(stm8swim);
LEAVE_PROGRAM_MODE_HANDLER(stm8swim);
ERASE_TARGET_HANDLER(stm8swim);
WRITE_TARGET_HANDLER(stm8swim);
READ_TARGET_HANDLER(stm8swim);
const struct program_functions_t stm8_program_functions = 
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(stm8swim), 
	LEAVE_PROGRAM_MODE_FUNCNAME(stm8swim), 
	ERASE_TARGET_FUNCNAME(stm8swim), 
	WRITE_TARGET_FUNCNAME(stm8swim), 
	READ_TARGET_FUNCNAME(stm8swim)
};

static void stm8_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<s|i>\n\n",
			CUR_TARGET_STRING);
}

PARSE_ARGUMENT_HANDLER(stm8)
{
	uint8_t mode;
	
	switch (cmd)
	{
	case 'h':
		stm8_usage();
		break;
	case 'm':
		if (NULL == argu)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_OPTION), cmd);
			return ERRCODE_INVALID_OPTION;
		}
		mode = (uint8_t)strtoul(argu, NULL,0);
		switch (mode)
		{
		case STM8_SWIM:
			stm8_program_area_map[0].attr |= AREA_ATTR_RNP;
			break;
		case STM8_ISP:
			stm8_program_area_map[0].attr &= ~AREA_ATTR_RNP;
			break;
		}
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}





#define reset_init()			interfaces->gpio.gpio_init()
#define reset_fini()			interfaces->gpio.gpio_fini()
#define reset_output()			interfaces->gpio.gpio_config(SWIM_RST_PIN, SWIM_RST_PIN, 0)
#define reset_input()			interfaces->gpio.gpio_config(SWIM_RST_PIN, 0, SWIM_RST_PIN)
#define reset_set()				reset_input()
#define reset_clr()				do{\
									interfaces->gpio.gpio_out(SWIM_RST_PIN, 0);\
									reset_output();\
								}while(0)

#define swim_init()				interfaces->swim.swim_init()
#define swim_fini()				interfaces->swim.swim_fini()
#define swim_set_param(m,c0,c1)	interfaces->swim.swim_set_param((m), (c0), (c1))
#define swim_srst()				interfaces->swim.swim_srst()
#define swim_wotf(a, b, l)		interfaces->swim.swim_wotf((b), (l), (a))
#define swim_rotf(a, b, l)		interfaces->swim.swim_rotf((b), (l), (a))
#define swim_sync(m)			interfaces->swim.swim_sync(m)
#define swim_enable()			interfaces->swim.swim_enable()

#define delay_ms(ms)			interfaces->delay.delayms((ms) | 0x8000)
#define delay_us(us)			interfaces->delay.delayus((us) & 0x7FFF)

#define poll_start()			interfaces->poll.poll_start(50, 100)
#define poll_end()				interfaces->poll.poll_end()
#define poll_check(o, m, v)		interfaces->poll.poll_checkbyte((o), (m), (v))
#define poll_fail(o, m, v)		interfaces->poll.poll_checkfail((o), (m), (v))

#define commit()				interfaces->peripheral_commit()

static struct interfaces_info_t *interfaces = NULL;

static RESULT stm8_swim_wotf_reg(uint32_t addr, uint8_t value)
{
	return swim_wotf(addr, &value, 1);
}

static void stm8_unlock_eeprom_option(uint32_t dukr)
{
	stm8_swim_wotf_reg(dukr, 0xAE);
	stm8_swim_wotf_reg(dukr, 0x56);
}

static void stm8_unlock_flash(uint32_t pukr)
{
	stm8_swim_wotf_reg(pukr, 0x56);
	stm8_swim_wotf_reg(pukr, 0xAE);
}

static RESULT stm8_poll_ready(uint32_t iapsr)
{
	RESULT ret;
	static uint8_t data;
	
	ret = poll_start();
	if (ret != ERROR_FAIL)
	{
		ret = swim_rotf(iapsr, &data, 1);
	}
	if (ret != ERROR_FAIL)
	{
		ret = poll_fail(0, STM8_FLASH_IAPSR_WRPGDIS, STM8_FLASH_IAPSR_WRPGDIS);
	}
	if (ret != ERROR_FAIL)
	{
		ret = poll_check(0, STM8_FLASH_IAPSR_EOP, STM8_FLASH_IAPSR_EOP);
	}
	if (ret != ERROR_FAIL)
	{
		ret = poll_end();
	}
	return ret;
}

static RESULT stm8_erase_block(uint32_t cr2, uint32_t ncr2, 
				uint32_t block_addr)
{
	uint8_t buff[4];
	
	stm8_swim_wotf_reg(cr2, STM8_FLASH_CR2_ERASE);
	if (ncr2)
	{
		stm8_swim_wotf_reg(ncr2, (uint8_t)~STM8_FLASH_CR2_ERASE);
	}
	memset(buff, 0, 4);
	swim_wotf(block_addr, buff, 4);
	return ERROR_OK;
}

static RESULT stm8_program_block(uint32_t cr2, uint32_t ncr2, 
				uint32_t block_addr, uint8_t *block_buff, uint8_t block_size)
{
	stm8_swim_wotf_reg(cr2, STM8_FLASH_CR2_FPRG);
	if (ncr2)
	{
		stm8_swim_wotf_reg(ncr2, (uint8_t)~STM8_FLASH_CR2_FPRG);
	}
	swim_wotf(block_addr, block_buff, block_size);
	return ERROR_OK;
}

static uint8_t stm8_swim_enabled = 0;

ENTER_PROGRAM_MODE_HANDLER(stm8swim)
{
	uint8_t test_buf0[7], test_buf1[6];
	RESULT ret = ERROR_OK;
	struct chip_param_t *param = context->param;
	uint8_t target_mhz = (uint8_t)param->param[STM8_PARAM_IRC];
	
	interfaces = &(context->prog->interfaces);
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		stm8_swim_enabled = 0;
		reset_init();
		reset_set();
		delay_ms(20);
		
		reset_clr();
		delay_ms(20);
		reset_set();
		delay_ms(20);
		reset_clr();
		delay_ms(10);
		swim_enable();
		if (ERROR_OK != commit())
		{
			reset_input();
			reset_fini();
			commit();
			LOG_ERROR(_GETTEXT("No response on SWIM! Is target connected?\n"));
			return ERROR_FAIL;
		}
		
		stm8_swim_enabled = 1;
		// SWIM mode
		swim_init();
		if (param->param[STM8_PARAM_CLK_SWIMCCR] != 0)
		{
			swim_set_param(target_mhz / 2, 20, 2);
		}
		else
		{
			swim_set_param(target_mhz, 20, 2);
		}
		delay_ms(10);
		swim_srst();
		stm8_swim_wotf_reg(STM8_REG_SWIM_CSR, STM8_SWIM_CSR_SAFT_MASK | 
									STM8_SWIM_CSR_SWIM_DM | STM8_SWIM_CSR_RST);
		delay_ms(10);
		reset_set();
		delay_ms(10);
		// enable double speed if supported
		stm8_swim_wotf_reg(param->param[STM8_PARAM_CLK_CKDIVR], 0x00);
		if (param->param[STM8_PARAM_CLK_SWIMCCR] != 0)
		{
			stm8_swim_wotf_reg(param->param[STM8_PARAM_CLK_SWIMCCR], 0x01);
			swim_set_param(target_mhz, 20, 2);
		}
		else
		{
			target_mhz /= 2;
		}
		swim_sync(target_mhz);
		swim_rotf(0x0067F0, test_buf0, 6);
		// enable high speed mode if available
		swim_rotf(STM8_REG_SWIM_CSR, test_buf1, 1);
		if (ERROR_OK != commit())
		{
			return ERROR_FAIL;
		}
		if (test_buf1[0] & STM8_SWIM_CSR_HSIT)
		{
			stm8_swim_wotf_reg(STM8_REG_SWIM_CSR, STM8_SWIM_CSR_SAFT_MASK 
				| STM8_SWIM_CSR_SWIM_DM | STM8_SWIM_CSR_HS | STM8_SWIM_CSR_RST);
			delay_ms(10);
			swim_set_param(target_mhz, 8, 2);
		}
		swim_rotf(0x0067F0, test_buf1, 6);
		stm8_unlock_eeprom_option(param->param[STM8_PARAM_FLASH_DUKR]);
		stm8_unlock_flash(param->param[STM8_PARAM_FLASH_PUKR]);
		if ((ERROR_OK != commit()) || memcmp(test_buf0, test_buf1, 6))
		{
			return ERROR_FAIL;
		}
		else
		{
			test_buf0[6] = '\0';
			LOG_INFO(_GETTEXT("is this chip ID: %s\n"), test_buf0);
		}
		
		// stall core
//		stm8_swim_wotf_reg(STM8_REG_DM_CSR2, STM8_DM_CSR2_STALL | STM8_DM_CSR2_FLUSH);
		// download flashloader
		
		// flush decode and restart core
//		stm8_swim_wotf_reg(STM8_REG_DM_CSR2, STM8_DM_CSR2_FLUSH);
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

LEAVE_PROGRAM_MODE_HANDLER(stm8swim)
{
	RESULT ret = ERROR_OK;
	REFERENCE_PARAMETER(success);
	interfaces = &(context->prog->interfaces);
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		if (stm8_swim_enabled)
		{
			swim_srst();
			swim_fini();
			reset_input();
			reset_fini();
			ret = commit();
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

ERASE_TARGET_HANDLER(stm8swim)
{
	RESULT ret = ERROR_OK;
	struct chip_param_t *param = context->param;
	REFERENCE_PARAMETER(size);
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		switch (area)
		{
		case APPLICATION_CHAR:
		case EEPROM_CHAR:
			ret = stm8_erase_block(param->param[STM8_PARAM_FLASH_CR2], 
									param->param[STM8_PARAM_FLASH_NCR2], addr);
			if (ret != ERROR_OK)
			{
				break;
			}
			ret = stm8_poll_ready(param->param[STM8_PARAM_FLASH_IAPSR]);
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

WRITE_TARGET_HANDLER(stm8swim)
{
	RESULT ret = ERROR_OK;
	struct chip_param_t *param = context->param;
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		switch (area)
		{
		case APPLICATION_CHAR:
		case EEPROM_CHAR:
			ret = stm8_program_block(param->param[STM8_PARAM_FLASH_CR2], 
				param->param[STM8_PARAM_FLASH_NCR2], addr, buff, (uint8_t)size);
			if (ret != ERROR_OK)
			{
				break;
			}
			ret = stm8_poll_ready(param->param[STM8_PARAM_FLASH_IAPSR]);
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

READ_TARGET_HANDLER(stm8swim)
{
	uint8_t fuse_page[128];
	uint16_t cur_size;
	RESULT ret = ERROR_OK;
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		switch (area)
		{
		case CHIPID_CHAR:
			// stm8 has no chip id
			memset(buff, 0, 2);
			break;
		case APPLICATION_CHAR:
		case EEPROM_CHAR:
			cur_size = 0;
			while (size > 0)
			{
				if (size > 0xFF)
				{
					cur_size = 0xFF;
				}
				else
				{
					cur_size = (uint16_t)size;
				}
				
				if (ERROR_OK != swim_rotf(addr, buff, cur_size))
				{
					return ERROR_FAIL;
				}
				
				buff += cur_size;
				addr += cur_size;
				size -= cur_size;
				pgbar_update(cur_size);
			}
			ret = commit();
			break;
		case FUSE_CHAR:
			swim_rotf(0x004800, fuse_page, sizeof(fuse_page));
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

