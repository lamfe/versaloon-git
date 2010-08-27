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

#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"
#include "scripts.h"

#include "s12x.h"
#include "s12x_internal.h"

#define CUR_TARGET_STRING			S12X_STRING

const struct program_area_map_t s12x_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t s12x_program_mode[] = 
{
	{'r', "", ISSP},
	{'p', "", ISSP},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(s12x);
LEAVE_PROGRAM_MODE_HANDLER(s12x);
ERASE_TARGET_HANDLER(s12x);
WRITE_TARGET_HANDLER(s12x);
READ_TARGET_HANDLER(s12x);
const struct program_functions_t s12x_program_functions = 
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(s12x), 
	LEAVE_PROGRAM_MODE_FUNCNAME(s12x), 
	ERASE_TARGET_FUNCNAME(s12x), 
	WRITE_TARGET_FUNCNAME(s12x), 
	READ_TARGET_FUNCNAME(s12x)
};

MISC_HANDLER(s12x_help)
{
	MISC_CHECK_ARGC(1);
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<r|p>\n\n", 
			CUR_TARGET_STRING);
	return ERROR_OK;
}

const struct misc_cmd_t s12x_notifier[] = 
{
	MISC_CMD(	"help",
				"print help information of current target for internal call",
				s12x_help),
	MISC_CMD_END
};



#define reset_init()			interfaces->gpio.init()
#define reset_fini()			interfaces->gpio.fini()
#define reset_output()			\
	interfaces->gpio.config(SWIM_RST_PIN, SWIM_RST_PIN, 0)
#define reset_input()			\
	interfaces->gpio.config(SWIM_RST_PIN, 0, SWIM_RST_PIN)
#define reset_set()				reset_input()
#define reset_clr()				reset_output()
#define bdm_output()			\
	interfaces->gpio.config(BDM_PIN, BDM_PIN, 0)
#define bdm_input()			\
	interfaces->gpio.config(BDM_PIN, 0, BDM_PIN)
#define bdm_set()				bdm_input()
#define bdm_clr()				bdm_output()

#define bdm_init()				interfaces->bdm.init()
#define bdm_fini()				interfaces->bdm.fini()
#define bdm_sync(khz)			interfaces->bdm.sync(khz)
#define bdm_transact(bo, so, bi, si, d, a)		\
	interfaces->bdm.transact((bo), (so), (bi), (si), (d), (a))

#define delay_ms(ms)			interfaces->delay.delayms((ms) | 0x8000)
#define delay_us(us)			interfaces->delay.delayus((us) & 0x7FFF)

#define poll_start()			interfaces->poll.start(5000, 100)
#define poll_end()				interfaces->poll.end()
#define poll_ok(o, m, v)		\
	interfaces->poll.checkok(POLL_CHECK_EQU, (o), 1, (m), (v))
#define poll_fail(o, m, v)		\
	interfaces->poll.checkfail(POLL_CHECK_EQU, (o), 1, (m), (v))

#define commit()				interfaces->peripheral_commit()

static struct interfaces_info_t *interfaces = NULL;
static uint8_t s12x_flash_div = 0;

#define S12X_BDMCMD_ACKENABLE		0xD5	// NI:D5/d
#define S12X_BDMCMD_ACKDIABLE		0xD6	// NI:D6/d
#define	S12X_BDMCMD_BACKGROUND		0x90	// NI:90/d
#define S12X_BDMCMD_READBDBYTE		0xE4	// NI:E4/AAAA/d/RD16
#define S12X_BDMCMD_WRITEBDBYTE		0xC4	// NI:C4/AAAA/WR16/d
#define S12X_BDMCMD_READBDWORD		0xEC	// NI:EC/AAAA/d/RD16
#define S12X_BDMCMD_WRITEBDWORD		0xCC	// NI:CC/AAAA/d/WR16
#define S12X_BDMCMD_READBYTE		0xE0	// NI:E0/AAAA/d/RD16
#define S12X_BDMCMD_WRITEBYTE		0xC0	// NI:C0/AAAA/WD16/d
#define S12X_BDMCMD_READWORD		0xE8	// NI:E8/AAAA/d/RD16
#define S12X_BDMCMD_WRITEWORD		0xC8	// NI:C8/WR16/d

static RESULT s12x_ack_enable(void)
{
	uint8_t outbuff[1];
	
	outbuff[0] = S12X_BDMCMD_ACKENABLE;
	return bdm_transact(outbuff, 1, NULL, 0, 1, 1);
}
/*
static RESULT s12x_background(void)
{
	uint8_t outbuff[1];
	
	outbuff[0] = S12X_BDMCMD_BACKGROUND;
	return bdm_transact(outbuff, 1, NULL, 0, 0, 0);
}
*/
static RESULT s12x_read(uint8_t cmd, uint16_t addr, uint16_t *data)
{
	uint8_t outbuff[3];
	
	outbuff[0] = cmd;
	outbuff[1] = (addr >> 8) & 0xFF;
	outbuff[2] = (addr >> 0) & 0xFF;
	return bdm_transact(outbuff, sizeof(outbuff), (uint8_t *)data, 2, 1, 1);
}

static RESULT s12x_write(uint8_t cmd, uint16_t addr, uint16_t data)
{
	uint8_t outbuff[5];
	
	outbuff[0] = cmd;
	outbuff[1] = (addr >> 8) & 0xFF;
	outbuff[2] = (addr >> 0) & 0xFF;
	outbuff[3] = (data >> 8) & 0xFF;
	outbuff[4] = (data >> 0) & 0xFF;
	return bdm_transact(outbuff, sizeof(outbuff), NULL, 0, 1, 1);
}
/*
static RESULT s12x_read_bd_word(uint16_t addr, uint16_t *data)
{
	return s12x_read(S12X_BDMCMD_READBDWORD, addr, data);
}

static RESULT s12x_write_bd_word(uint16_t addr, uint16_t data)
{
	return s12x_write(S12X_BDMCMD_WRITEBDWORD, addr, data);
}
*/
static RESULT s12x_read_word(uint16_t addr, uint16_t *data)
{
	return s12x_read(S12X_BDMCMD_READWORD, addr, data);
}

static RESULT s12x_write_word(uint16_t addr, uint16_t data)
{
	return s12x_write(S12X_BDMCMD_WRITEWORD, addr, data);
}

static RESULT s12x_write_byte(uint16_t addr, uint8_t data)
{
	uint16_t data16;
	
	data16 = data;
	if (!(addr & 1))
	{
		data16 <<= 8;
	}
	return s12x_write(S12X_BDMCMD_WRITEBYTE, addr, data16);
}

static RESULT s12x_flash_cmd(uint8_t param_num, uint16_t *param)
{
	RESULT ret = ERROR_OK;
	uint8_t i;
	
	for (i = 0; i < param_num; i++)
	{
		ret = s12x_write_byte(S12X_FTMR_CCOBIX_ADDR, i);
		if (ret != ERROR_OK)
		{
			return ret;
		}
		
		// param is in little endian, change to big endian first
		ret = s12x_write_word(S12X_FTMR_FCCOB_ADDR, param[i]);
		if (ret != ERROR_OK)
		{
			return ret;
		}
	}
	// launch the command
	ret = s12x_write_byte(S12X_FTMR_FSTAT_ADDR, S12X_FTMR_FSTAT_FPVIOL 
							| S12X_FTMR_FSTAT_FACCERR | S12X_FTMR_FSTAT_CCIF);
	
	// poll
	poll_start();
	s12x_read_word(S12X_FTMR_FSTAT_ADDR, NULL);
	poll_fail(1, S12X_FTMR_FSTAT_FPVIOL, S12X_FTMR_FSTAT_FPVIOL);
	poll_fail(1, S12X_FTMR_FSTAT_FACCERR, S12X_FTMR_FSTAT_FACCERR);
	poll_ok(1, S12X_FTMR_FSTAT_CCIF, S12X_FTMR_FSTAT_CCIF);
	poll_end();
	
	return ret;
}

ENTER_PROGRAM_MODE_HANDLER(s12x)
{
	uint16_t kernel_khz;
	
	interfaces = &(context->prog->interfaces);
	
	reset_init();
	reset_set();
#if 1
	bdm_clr();
	delay_ms(1);
	reset_clr();
	delay_ms(20);
	reset_set();
	delay_ms(1);
	bdm_set();
#endif
	
	bdm_init();
	bdm_sync(&kernel_khz);
	delay_ms(1);
	s12x_ack_enable();
	
	if (ERROR_OK != commit())
	{
		return ERROR_FAIL;
	}
	LOG_INFO("target running at %dkhz", kernel_khz);
	
	s12x_flash_div = 0;
	if ((kernel_khz % 1000) >= 500)
	{
		s12x_flash_div |= (kernel_khz / 1000);
	}
	else
	{
		s12x_flash_div |= (kernel_khz / 1000) - 1;
	}
	s12x_write_byte(S12X_FTMR_FCLKDIV_ADDR, s12x_flash_div);
	s12x_write_byte(S12X_FTMR_FSTAT_ADDR, 
					S12X_FTMR_FSTAT_FPVIOL | S12X_FTMR_FSTAT_FACCERR);
	s12x_write_byte(S12X_FTMR_FPROT_ADDR, 0xFF);
	if (ERROR_OK != commit())
	{
		return ERROR_FAIL;
	}
	LOG_INFO("flash running at %dkhz", 
				kernel_khz / (1 + (s12x_flash_div & S12X_FTMR_FCDIV_DIVMASK)));
	return ERROR_OK;
}

LEAVE_PROGRAM_MODE_HANDLER(s12x)
{
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	bdm_fini();
	reset_fini();
	
	return commit();
}

ERASE_TARGET_HANDLER(s12x)
{
	RESULT ret = ERROR_OK;
	uint16_t param[8];
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		param[0] = S12X_FTMR_FCMD_EraseAllBlocks << 8;
		ret = s12x_flash_cmd(1, param);
		if (ret != ERROR_OK)
		{
			break;
		}
		
		param[0] = (S12X_FTMR_FCMD_ProgramPFlash << 8) 
			| ((S12X_BDM_BDMGPR_ADDR & 0xFF0000) >> 16);
		param[1] = S12X_BDM_BDMGPR_ADDR & 0x00FFFF;
		param[2] = 0xFFFF;
		param[3] = 0xFFFF;
		param[4] = 0xFFFF;
		param[5] = 0xFFFE;
		ret = s12x_flash_cmd(6, param);
		ret = commit();
		break;
	default:
		ret = ERROR_FAIL;
	}
	return ret;
}

WRITE_TARGET_HANDLER(s12x)
{
	RESULT ret = ERROR_OK;
	uint16_t i;
	uint16_t param[8];
	
	REFERENCE_PARAMETER(size);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(buff);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		if ((addr & 0x07) || (size & 0x07))
		{
			return ERROR_FAIL;
		}
		
		for (i = 0; i < size; i += 8)
		{
			if ((addr + i < S12X_BDM_ROM_START) 
				|| (addr + i > S12X_BDM_ROM_END))
			{
				param[0] = (S12X_FTMR_FCMD_ProgramPFlash << 8) 
					| (((addr + i) & 0xFF0000) >> 16);
				param[1] = (addr + i) & 0x00FFFF;
				param[2] = buff[i + 0] + (buff[i + 1] << 8);
				param[3] = buff[i + 2] + (buff[i + 3] << 8);
				param[4] = buff[i + 4] + (buff[i + 5] << 8);
				param[5] = buff[i + 6] + (buff[i + 7] << 8);
				ret = s12x_flash_cmd(6, param);
				if (ret != ERROR_OK)
				{
					break;
				}
			}
		}
		break;
	default:
		ret = ERROR_FAIL;
	}
	return ret;
}

READ_TARGET_HANDLER(s12x)
{
	RESULT ret = ERROR_OK;
	uint16_t i;
	uint16_t addr16;
	uint8_t ppage;
	
	REFERENCE_PARAMETER(context);
	
	switch (area)
	{
	case CHIPID_CHAR:
		s12x_read_word(0x001A, (uint16_t *)&buff[0]);
		ret = commit();
		buff[0] &= 0xF0;
		buff[1] &= 0xF0;
		break;
	case APPLICATION_CHAR:
		ppage = (uint8_t)(addr >> 14);
		s12x_write_byte(S12X_PPAGE_ADDR, ppage);
		for (i = 0; i < size; i += 2)
		{
			addr16 = (uint16_t)((addr + i) & 0x3FFF) + 0x8000;
			s12x_read_word(addr16, (uint16_t *)&buff[i]);
		}
		ret = commit();
		break;
	default:
		ret = ERROR_FAIL;
		break;
	}
	return ret;
}

