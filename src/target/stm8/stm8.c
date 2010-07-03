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
			stm8_program_area_map[0].attr |= AREA_ATTR_RNP | AREA_ATTR_EWW;
			break;
		case STM8_ISP:
			stm8_program_area_map[0].attr &= ~(AREA_ATTR_RNP | AREA_ATTR_EWW);
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

#define poll_start()			interfaces->poll.poll_start(100, 100)
#define poll_end()				interfaces->poll.poll_end()
#define poll_check(o, m, v)		interfaces->poll.poll_checkbyte((o), (m), (v))
#define poll_fail(o, m, v)		interfaces->poll.poll_checkfail((o), (m), (v))

#define commit()				interfaces->peripheral_commit()

static struct interfaces_info_t *interfaces = NULL;

// 0x0000 ---- 0x007F: flash loader code
// 0x0080 ---- 0x00BF: param_bk + loopcnt
// 0x00C0 ---- 0x00FF: param + sync + err
// 0x0100 ---- 0x017F: buffer0
// 0x0180 ---- 0x01FF: buffer1
//#define STM8_USE_FLASHLOADER
#define STM8_FL_ADDR				0x000000
#define STM8_FL_SYNC_ADDR			(STM8_FL_PARAM_ADDR + STM8_FL_PARAM_SIZE + 0)
#define STM8_FL_ERR_ADDR			(STM8_FL_PARAM_ADDR + STM8_FL_PARAM_SIZE + 1)
#define STM8_FL_PARAM_SIZE			13
#define STM8_FL_LOOPCNT_ADDR		0xBF

#define STM8_FL_IAPSR_OFFSET		0
#define STM8_FL_CR2_OFFSET			2
#define STM8_FL_NCR2_OFFSET			4
#define STM8_FL_BLOCKSIZE_OFFSET	6
#define STM8_FL_DATAADDR_OFFSET		7
#define STM8_FL_TARGETADDR_OFFSET	9
#define STM8_FL_CMD_OFFSET			12

#define STM8_FL_PARAM_ADDR			0xC0
#define STM8_FL_IAPSR_ADDR			(STM8_FL_PARAM_ADDR + STM8_FL_IAPSR_OFFSET)
#define STM8_FL_CR2_ADDR			(STM8_FL_PARAM_ADDR + STM8_FL_CR2_OFFSET)
#define STM8_FL_NCR2_ADDR			(STM8_FL_PARAM_ADDR + STM8_FL_NCR2_OFFSET)
#define STM8_FL_BLOCKSIZE_ADDR		(STM8_FL_PARAM_ADDR + STM8_FL_BLOCKSIZE_OFFSET)
#define STM8_FL_DATA_ADDR			(STM8_FL_PARAM_ADDR + STM8_FL_DATAADDR_OFFSET)
#define STM8_FL_TARGET_ADDR			(STM8_FL_PARAM_ADDR + STM8_FL_TARGETADDR_OFFSET)
#define STM8_FL_CMD_ADDR			(STM8_FL_PARAM_ADDR + STM8_FL_CMD_OFFSET)

#define STM8_FL_PARAM_BKADDR		0x80
#define STM8_FL_IAPSR_BKADDR		(STM8_FL_PARAM_BKADDR + STM8_FL_IAPSR_OFFSET)
#define STM8_FL_CR2_BKADDR			(STM8_FL_PARAM_BKADDR + STM8_FL_CR2_OFFSET)
#define STM8_FL_NCR2_BKADDR			(STM8_FL_PARAM_BKADDR + STM8_FL_NCR2_OFFSET)
#define STM8_FL_BLOCKSIZE_BKADDR	(STM8_FL_PARAM_BKADDR + STM8_FL_BLOCKSIZE_OFFSET)
#define STM8_FL_DATA_BKADDR			(STM8_FL_PARAM_BKADDR + STM8_FL_DATAADDR_OFFSET)
#define STM8_FL_TARGET_BKADDR		(STM8_FL_PARAM_BKADDR + STM8_FL_TARGETADDR_OFFSET)
#define STM8_FL_CMD_BKADDR			(STM8_FL_PARAM_BKADDR + STM8_FL_CMD_OFFSET)

#ifdef STM8_USE_FLASHLOADER
struct stm8_fl_param_t
{
	uint16_t iapsr_addr;
	uint16_t cr2_addr;
	uint16_t ncr2_addr;
	uint8_t block_size;
	uint16_t data_addr;
	uint32_t target_addr;
	uint8_t cmd;
};

static uint8_t stm8_flashloader[] =
{
											// wait_sync:
	0x3D, STM8_FL_SYNC_ADDR,				// TNZ	sync
	0x27, 0xFC,								// JREQ	wait_sync
	0x9D,									// NOP
											// read_operation_param:
	0x35, STM8_FL_PARAM_SIZE, 0x00, STM8_FL_LOOPCNT_ADDR,	// MOV	loopcnt, #param_size
	0xAE, 0x00, STM8_FL_PARAM_ADDR,			// LDW	X, #param_addr
	0x90, 0xAE, 0x00, STM8_FL_PARAM_BKADDR,	// LDW	Y, #param_bkaddr
											// do_read_param:
	0x3D, STM8_FL_LOOPCNT_ADDR,				// TNZ	loopcnt
	0x27, 0x10,								// JREQ	end_read_param
	0x9D,									// NOP
	0xF6,									// LD	A, (X)
	0x90, 0xF7,								// LD	(Y), A
	0x5C,									// INCW	X
	0x90, 0x5C,								// INCW Y
	0xB6, STM8_FL_LOOPCNT_ADDR,				// LD	A, loopcnt
	0xA0, 0x01,								// SUB	A, #1
	0xB7, STM8_FL_LOOPCNT_ADDR,				// LD	loopcnt, A
	0x20, 0xED,								// JRA	do_read_param
	0x9D,									// NOP
											// end_read_param:
											// disable_swim_pri:
	
//	0x3F, STM8_FL_SYNC_ADDR,				// CLR	sync
0X9D, 0X9D,
											// do_operation:
	0xBE, STM8_FL_CR2_BKADDR,				// LDW	X, cr2
	0xB6, STM8_FL_CMD_BKADDR,				// LD	A, cmd
	0xF7,									// LD	(X), A
	0xBE, STM8_FL_NCR2_BKADDR,				// LDW	X, ncr2
	0x5D,									// TNZW	X
	0x27, 0x04,								// JREQ	write_data
	0x9D,									// NOP
	0xA8, 0xFF,								// XOR	A, #0xFF
	0xF7,									// LD	(X), A
											// write_data:
	0xBE, STM8_FL_DATA_BKADDR,				// LDW	X, data
	0x45, STM8_FL_BLOCKSIZE_BKADDR, STM8_FL_LOOPCNT_ADDR,	// MOV	loopcnt, blocksize
											// do_write_data:
	0x3D, STM8_FL_LOOPCNT_ADDR,				// TNZ	loopcnt
	0x27, 0x1A,								// JREQ	end_write_data
	0x9D,									// NOP
	0xF6,									// LD	A, (X)
	0x92, 0xBD, 0x00, STM8_FL_TARGET_BKADDR,// LDF	[target.e], A
	0x5C,									// INCW	X
											// inc_target:
	0x3C, STM8_FL_TARGET_BKADDR + 2,		// INC	target_low
	0x26, 0x06,								// JRNE	end_int_target
	0x3C, STM8_FL_TARGET_BKADDR + 1,		// INC	target_low
	0x26, 0x02,								// JRNE	end_int_target
	0x3C, STM8_FL_TARGET_BKADDR + 0,		// INC	target_low
											// end_inc_target:
	0xB6, STM8_FL_LOOPCNT_ADDR,				// LD	A, loopcnt
	0xA0, 0x01,								// SUB	A, #1
	0xB7, STM8_FL_LOOPCNT_ADDR,				// LD	loopcnt, A
	0x20, 0xE3,								// JRA	do_write_data
	0x9D,									// NOP
											// end_write_data:
											// wait_ready:
	0xBE, STM8_FL_IAPSR_BKADDR,				// LDW	X, iapsr
	0xF6,									// LD	A, (X)
	0xA5, 0x01,								// BCP	A, STM8_FLASH_IAPSR_WRPGDIS
	0x26, 0x09,								// JRNE	return_error
	0x9D,									// NOP
	0xA5, 0x04,								// BCP	A, STM8_FLASH_IAPSR_EOP
	0x26, 0x0A,								// JRNE	return_ok
	0x9D,									// NOP
	0x20, 0xF1,								// JRA	wait_ready
	0x9D,									// NOP
											// return_error:
	0x35, 0x01, 0x00, STM8_FL_ERR_ADDR,		// MOV	err, #1
0x20, 0xFE,								// JRA	$	// issue timeout here
											// return_ok:
											// enable_swim_pri:
	
0x3F, STM8_FL_SYNC_ADDR,				// CLR	sync
	0x20, 0x8F,								// JRA	wait_sync
	0x9D,									// NOP
	0x20, 0xFE								// JRA	$
};
#endif

static RESULT swim_wotf_reg(uint32_t addr, uint32_t value, uint8_t size)
{
	return swim_wotf(addr, (uint8_t*)&value, size);
}

static void stm8_unlock_eeprom_option(uint32_t dukr)
{
	swim_wotf_reg(dukr, 0xAE, 1);
	swim_wotf_reg(dukr, 0x56, 1);
}

static void stm8_unlock_flash(uint32_t pukr)
{
	swim_wotf_reg(pukr, 0x56, 1);
	swim_wotf_reg(pukr, 0xAE, 1);
}

#ifndef STM8_USE_FLASHLOADER
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

static RESULT stm8_program_reg(uint32_t cr2, uint32_t ncr2, uint32_t iapsr, 
	uint8_t cmd, uint32_t reg_addr, uint32_t reg_val, uint8_t reg_size)
{
	RESULT ret = ERROR_OK;
	
	if (reg_size > 4)
	{
		return ERROR_FAIL;
	}
	
	ret = swim_wotf_reg(cr2, cmd, 1);
	if ((ERROR_OK == ret) && (ncr2))
	{
		ret = swim_wotf_reg(ncr2, ~cmd, 1);
	}
	if (ERROR_OK == ret)
	{
		ret = swim_wotf_reg(reg_addr, reg_val, reg_size);
	}
	if (ERROR_OK == ret)
	{
		ret = stm8_poll_ready(iapsr);
	}
	return ret;
}

static RESULT stm8_program_block(uint32_t cr2, uint32_t ncr2, uint32_t iapsr, 
	uint8_t cmd, uint32_t block_addr, uint8_t *block_buff, uint8_t block_size)
{
	RESULT ret = ERROR_OK;
	
	ret = swim_wotf_reg(cr2, cmd, 1);
	if ((ERROR_OK == ret) && (ncr2))
	{
		ret = swim_wotf_reg(ncr2, ~cmd, 1);
	}
	if (ERROR_OK == ret)
	{
		ret = swim_wotf(block_addr, block_buff, block_size);
	}
	if (ERROR_OK == ret)
	{
		ret = stm8_poll_ready(iapsr);
	}
	return ret;
}
#else
static RESULT stm8_fl_print_context(void)
{
	struct
	{
		uint8_t A;
		uint32_t PC;
		uint16_t X;
		uint16_t Y;
		uint16_t SP;
		uint8_t CCR;
	}kernel_reg;
	uint8_t buff[256];
	
	swim_rotf(STM8_REG_DM_CSR2, buff, 1);
	if (ERROR_OK != commit())
	{
		return ERROR_FAIL;
	}
	LOG_INFO(_GETTEXT("DM_CSR2 = 0x%02X\n"), buff[0]);
	if (buff[0] & STM8_DM_CSR2_STALL)
	{
		LOG_INFO("CPU stalled\n");
	}
	else
	{
		LOG_INFO("CPU running\n");
	}
	swim_rotf(0x007F00, buff, 11);
	if (ERROR_OK != commit())
	{
		return ERROR_FAIL;
	}
	kernel_reg.A = buff[0];
	kernel_reg.PC = (buff[1] << 16) + (buff[2] << 8) + (buff[3] << 0);
	kernel_reg.X = (buff[4] << 8) + (buff[5] << 0);
	kernel_reg.Y = (buff[6] << 8) + (buff[7] << 0);
	kernel_reg.SP = (buff[8] << 8) + (buff[9] << 0);
	kernel_reg.CCR = buff[10];
	
	LOG_INFO(_GETTEXT("A = 0x%02X(%d)\n"), kernel_reg.A, kernel_reg.A);
	LOG_INFO(_GETTEXT("PC = 0x%06X\n"), kernel_reg.PC);
	LOG_INFO(_GETTEXT("X = 0x%04X\n"), kernel_reg.X);
	LOG_INFO(_GETTEXT("Y = 0x%04X\n"), kernel_reg.Y);
	LOG_INFO(_GETTEXT("SP = 0x%04X\n"), kernel_reg.SP);
	LOG_INFO(_GETTEXT("CCR = 0x%02X\n"), kernel_reg.CCR);
	
	swim_rotf(0x000000, buff, sizeof(buff));
	if (ERROR_OK != commit())
	{
		return ERROR_FAIL;
	}
	LOG_BYTE_BUF(buff, sizeof(buff), LOG_INFO, "%02X", 16);
	
	return ERROR_OK;
}

static RESULT stm8_fl_run(struct stm8_fl_param_t *param)
{
	RESULT ret = ERROR_OK;
	uint8_t buff[STM8_FL_PARAM_SIZE + 1];
	static uint8_t data[2];
	
	// poll
	if (ret != ERROR_FAIL)
	{
		ret = poll_start();
	}
	if (ret != ERROR_FAIL)
	{
		ret = swim_rotf(STM8_FL_SYNC_ADDR, (uint8_t*)&data, 2);
	}
	if (ret != ERROR_FAIL)
	{
		ret = poll_fail(0, 0xFF, 0x01);
	}
	if (ret != ERROR_FAIL)
	{
		ret = poll_check(1, 0xFF, 0x00);
	}
	if (ret != ERROR_FAIL)
	{
		ret = poll_end();
	}
	
	buff[STM8_FL_IAPSR_OFFSET + 0]		= (param->iapsr_addr >> 8) & 0xFF;
	buff[STM8_FL_IAPSR_OFFSET + 1]		= (param->iapsr_addr >> 0) & 0xFF;
	buff[STM8_FL_CR2_OFFSET + 0]		= (param->cr2_addr >> 8) & 0xFF;
	buff[STM8_FL_CR2_OFFSET + 1]		= (param->cr2_addr >> 0) & 0xFF;
	buff[STM8_FL_NCR2_OFFSET + 0]		= (param->ncr2_addr >> 8) & 0xFF;
	buff[STM8_FL_NCR2_OFFSET + 1]		= (param->ncr2_addr >> 0) & 0xFF;
	buff[STM8_FL_BLOCKSIZE_OFFSET]		= param->block_size;
	buff[STM8_FL_DATAADDR_OFFSET + 0]	= (param->data_addr >> 8) & 0xFF;
	buff[STM8_FL_DATAADDR_OFFSET + 1]	= (param->data_addr >> 0) & 0xFF;
	buff[STM8_FL_TARGETADDR_OFFSET + 0]	= (param->target_addr >> 16) & 0xFF;
	buff[STM8_FL_TARGETADDR_OFFSET + 1]	= (param->target_addr >> 8) & 0xFF;
	buff[STM8_FL_TARGETADDR_OFFSET + 2]	= (param->target_addr >> 0) & 0xFF;
	buff[STM8_FL_CMD_OFFSET]			= param->cmd;
	buff[STM8_FL_CMD_OFFSET + 1]		= 1;
	
	ret = swim_wotf(STM8_FL_PARAM_ADDR, buff, sizeof(buff));
	
	return ret;
}
#endif

static uint8_t stm8_swim_enabled = 0;

ENTER_PROGRAM_MODE_HANDLER(stm8swim)
{
	uint8_t test_buf0[7], test_buf1[6];
	RESULT ret = ERROR_OK;
	struct chip_param_t *param = context->param;
#ifdef STM8_USE_FLASHLOADER
	struct operation_t *op = context->op;
#endif
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
		delay_ms(10);
		swim_wotf_reg(STM8_REG_SWIM_CSR, STM8_SWIM_CSR_SAFT_MASK | 
								STM8_SWIM_CSR_SWIM_DM | STM8_SWIM_CSR_RST, 1);
		delay_ms(10);
		reset_set();
		delay_ms(10);
		// enable double speed if supported
		swim_wotf_reg(param->param[STM8_PARAM_CLK_CKDIVR], 0x00, 1);
		if (param->param[STM8_PARAM_CLK_SWIMCCR] != 0)
		{
			swim_wotf_reg(param->param[STM8_PARAM_CLK_SWIMCCR], 0x01, 1);
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
			swim_wotf_reg(STM8_REG_SWIM_CSR, STM8_SWIM_CSR_SAFT_MASK 
				| STM8_SWIM_CSR_SWIM_DM | STM8_SWIM_CSR_HS 
				| STM8_SWIM_CSR_RST | STM8_SWIM_CSR_PRI, 1);
			delay_ms(10);
			swim_set_param(target_mhz, 8, 2);
		}
		else
		{
			swim_wotf_reg(STM8_REG_SWIM_CSR, STM8_SWIM_CSR_SAFT_MASK 
				| STM8_SWIM_CSR_SWIM_DM | STM8_SWIM_CSR_RST 
				| STM8_SWIM_CSR_PRI, 1);
			delay_ms(10);
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
		swim_wotf_reg(STM8_REG_DM_CSR2, 
							STM8_DM_CSR2_STALL | STM8_DM_CSR2_FLUSH, 1);
#ifdef STM8_USE_FLASHLOADER
		if (op->erase_operations || op->write_operations)
		{
			uint32_t tmp = STM8_FL_ADDR;
			
			// download flashloader
			swim_wotf(tmp, stm8_flashloader, sizeof(stm8_flashloader));
			// clear sync and err
			swim_wotf_reg(STM8_FL_SYNC_ADDR, 0, 2);
			tmp = (tmp >> 16) | (tmp & 0xFF00) | (tmp << 16);
			swim_wotf_reg(0x007F01, tmp, 3);
			// flush decode and restart core
			swim_wotf_reg(STM8_REG_DM_CSR2, STM8_DM_CSR2_FLUSH, 1);
		}
		ret = commit();
#endif
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

#ifdef STM8_USE_FLASHLOADER
static uint8_t stm8_fl_ticktock = 0;
#endif
ERASE_TARGET_HANDLER(stm8swim)
{
	RESULT ret = ERROR_OK;
	struct chip_param_t *param = context->param;
	struct operation_t *op = context->op;
#ifdef STM8_USE_FLASHLOADER
	struct stm8_fl_param_t fl_param;
	uint16_t ram_addr;
#endif
	
	REFERENCE_PARAMETER(size);
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		switch (area)
		{
		case APPLICATION_CHAR:
			if (op->write_operations & APPLICATION)
			{
				break;
			}
			goto do_erase;
		case EEPROM_CHAR:
			if (op->write_operations & EEPROM)
			{
				break;
			}
do_erase:
#ifdef STM8_USE_FLASHLOADER
			if (stm8_fl_ticktock & 1)
			{
				ram_addr = 0x0100;
			}
			else
			{
				ram_addr = 0x0180;
			}
			stm8_fl_ticktock++;
			if (ERROR_OK != swim_wotf_reg(ram_addr, 0, 4))
			{
				stm8_fl_print_context();
				return ERROR_FAIL;
			}
			
			fl_param.iapsr_addr	= (uint16_t)param->param[STM8_PARAM_FLASH_IAPSR];
			fl_param.cr2_addr	= (uint16_t)param->param[STM8_PARAM_FLASH_CR2];
			fl_param.ncr2_addr	= (uint16_t)param->param[STM8_PARAM_FLASH_NCR2];
			fl_param.block_size	= 4;
			fl_param.data_addr	= (uint16_t)ram_addr;
			fl_param.target_addr= addr;
			fl_param.cmd		= STM8_FLASH_CR2_ERASE;
			if (ERROR_OK != stm8_fl_run(&fl_param))
			{
				stm8_fl_print_context();
				return ERROR_FAIL;
			}
#else
			ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
					param->param[STM8_PARAM_FLASH_NCR2], 
					param->param[STM8_PARAM_FLASH_IAPSR], 
					STM8_FLASH_CR2_ERASE, addr, 0, 4);
#endif
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
	struct program_info_t *pi = context->pi;
	struct chip_param_t *param = context->param;
	struct operation_t *op = context->op;
	uint8_t cmd;
	uint8_t fuse_page[128];
#ifdef STM8_USE_FLASHLOADER
	struct stm8_fl_param_t fl_param;
	uint16_t ram_addr;
#endif
	
	switch (context->pi->mode)
	{
	case STM8_SWIM:
		switch (area)
		{
		case APPLICATION_CHAR:
			if (op->erase_operations & APPLICATION)
			{
				cmd = STM8_FLASH_CR2_PRG;
			}
			else
			{
				cmd = STM8_FLASH_CR2_FPRG;
			}
			goto do_program;
		case EEPROM_CHAR:
			if (op->erase_operations & EEPROM)
			{
				cmd = STM8_FLASH_CR2_PRG;
			}
			else
			{
				cmd = STM8_FLASH_CR2_FPRG;
			}
do_program:
#ifdef STM8_USE_FLASHLOADER
			if ((size != 64) && (size != 128))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_INVALID_VALUE), size, "page_size");
				return ERROR_FAIL;
			}
			if (stm8_fl_ticktock & 1)
			{
				ram_addr = 0x0100;
			}
			else
			{
				ram_addr = 0x0180;
			}
			stm8_fl_ticktock++;
			if (ERROR_OK != swim_wotf(ram_addr, buff, (uint16_t)size))
			{
				stm8_fl_print_context();
				return ERROR_FAIL;
			}
// commit should not be called here for faster operation
// but if commit not called after stm8_fl_run, commit below will fail
// because there is chances that wotf is issued when MCU is writing flash
//			if (ERROR_OK != commit())
//			{
//				stm8_fl_print_context();
//				return ERROR_FAIL;
//			}
			
			fl_param.iapsr_addr	= (uint16_t)param->param[STM8_PARAM_FLASH_IAPSR];
			fl_param.cr2_addr	= (uint16_t)param->param[STM8_PARAM_FLASH_CR2];
			fl_param.ncr2_addr	= (uint16_t)param->param[STM8_PARAM_FLASH_NCR2];
			fl_param.block_size	= (uint8_t)size;
			fl_param.data_addr	= (uint16_t)ram_addr;
			fl_param.target_addr= addr;
			fl_param.cmd		= cmd;
			if (ERROR_OK != stm8_fl_run(&fl_param))
			{
				stm8_fl_print_context();
				return ERROR_FAIL;
			}
// if commit is not called here
// there will be chances that wotf is issued when MCU is writing flash
			if (ERROR_OK != commit())
			{
				stm8_fl_print_context();
				return ERROR_FAIL;
			}
#else
			ret = stm8_program_block(param->param[STM8_PARAM_FLASH_CR2], 
					param->param[STM8_PARAM_FLASH_NCR2], 
					param->param[STM8_PARAM_FLASH_IAPSR], 
					cmd, addr, buff, (uint8_t)size);
#endif
			break;
		case FUSE_CHAR:
			swim_rotf(STM8_FUSEPAGE_ADDR, fuse_page, sizeof(fuse_page));
			ret = commit();
			if (ret != ERROR_OK)
			{
				break;
			}
			cmd = STM8_FLASH_CR2_OPT;
			if (NULL == buff)
			{
				buff = (uint8_t*)&pi->program_areas[FUSE_IDX].value;
			}
			switch (param->param[STM8_PARAM_TYPE])
			{
			case STM8_TYPE_STM8S:
				if ((ERROR_OK == ret) && (buff[0] != fuse_page[0]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 0, buff[0], 1);
				}
				if ((ERROR_OK == ret) && (buff[1] != fuse_page[1]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 1, buff[1], 1);
					if (ERROR_OK == ret)
					{
						ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
								param->param[STM8_PARAM_FLASH_NCR2], 
								param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
								STM8_FUSEPAGE_ADDR + 2, ~buff[1], 1);
					}
				}
				if ((ERROR_OK == ret) && (buff[2] != fuse_page[3]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 3, buff[2], 1);
					if (ERROR_OK == ret)
					{
						ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
								param->param[STM8_PARAM_FLASH_NCR2], 
								param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
								STM8_FUSEPAGE_ADDR + 4, ~buff[2], 1);
					}
				}
				if ((ERROR_OK == ret) && (buff[3] != fuse_page[5]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 5, buff[3], 1);
					if (ERROR_OK == ret)
					{
						ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
								param->param[STM8_PARAM_FLASH_NCR2], 
								param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
								STM8_FUSEPAGE_ADDR + 6, ~buff[3], 1);
					}
				}
				if ((ERROR_OK == ret) && (buff[4] != fuse_page[7]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 7, buff[4], 1);
					if (ERROR_OK == ret)
					{
						ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
								param->param[STM8_PARAM_FLASH_NCR2], 
								param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
								STM8_FUSEPAGE_ADDR + 8, ~buff[4], 1);
					}
				}
				if ((ERROR_OK == ret) && (buff[5] != fuse_page[9]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 9, buff[5], 1);
					if (ERROR_OK == ret)
					{
						ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
								param->param[STM8_PARAM_FLASH_NCR2], 
								param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
								STM8_FUSEPAGE_ADDR + 10, ~buff[5], 1);
					}
				}
				if ((ERROR_OK == ret) && (buff[6] != fuse_page[13]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 13, buff[6], 1);
					if (ERROR_OK == ret)
					{
						ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
								param->param[STM8_PARAM_FLASH_NCR2], 
								param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
								STM8_FUSEPAGE_ADDR + 14, ~buff[6], 1);
					}
				}
				if ((ERROR_OK == ret) && (buff[7] != fuse_page[126]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 126, buff[7], 1);
					if (ERROR_OK == ret)
					{
						ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
								param->param[STM8_PARAM_FLASH_NCR2], 
								param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
								STM8_FUSEPAGE_ADDR + 127, ~buff[7], 1);
					}
				}
				break;
			case STM8_TYPE_STM8L:
				if ((ERROR_OK == ret) && (buff[0] != fuse_page[0]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 0, buff[0], 1);
				}
				if ((ERROR_OK == ret) && (buff[1] != fuse_page[2]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 2, buff[1], 1);
				}
				if ((ERROR_OK == ret) && (buff[2] != fuse_page[3]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 3, buff[2], 1);
				}
				if ((ERROR_OK == ret) && (buff[3] != fuse_page[8]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 8, buff[3], 1);
				}
				if ((ERROR_OK == ret) && (buff[5] != fuse_page[9]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 9, buff[5], 1);
				}
				if ((ERROR_OK == ret) && (buff[6] != fuse_page[10]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 10, buff[6], 1);
				}
				if ((ERROR_OK == ret) && (buff[7] != fuse_page[11]))
				{
					ret = stm8_program_reg(param->param[STM8_PARAM_FLASH_CR2], 
							param->param[STM8_PARAM_FLASH_NCR2], 
							param->param[STM8_PARAM_FLASH_IAPSR], cmd, 
							STM8_FUSEPAGE_ADDR + 11, buff[7], 1);
				}
				break;
			case STM8_TYPE_STM8A:
			default:
				ret = ERROR_FAIL;
			}
			if (ERROR_OK == ret)
			{
				ret = commit();
			}
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
	struct chip_param_t *param = context->param;
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
			swim_rotf(STM8_FUSEPAGE_ADDR, fuse_page, sizeof(fuse_page));
			ret = commit();
			if (ret != ERROR_OK)
			{
				break;
			}
			switch (param->param[STM8_PARAM_TYPE])
			{
			case STM8_TYPE_STM8S:
				buff[0] = fuse_page[0];
				buff[1] = fuse_page[1];
				buff[2] = fuse_page[3];
				buff[3] = fuse_page[5];
				buff[4] = fuse_page[7];
				buff[5] = fuse_page[9];
				buff[6] = fuse_page[13];
				buff[7] = fuse_page[126];
				break;
			case STM8_TYPE_STM8L:
				buff[0] = fuse_page[0];
				buff[1] = fuse_page[2];
				buff[2] = fuse_page[3];
				buff[3] = fuse_page[8];
				buff[4] = 0;
				buff[5] = fuse_page[9];
				buff[6] = fuse_page[10];
				buff[7] = fuse_page[11];
				break;
			case STM8_TYPE_STM8A:
			default:
				ret = ERROR_FAIL;
				break;
			}
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

