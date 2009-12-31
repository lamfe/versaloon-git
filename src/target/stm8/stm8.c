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
	delay_ms(3);
	stm8_swim_rotf(STM8_REG_FLASH_IAPSR, buff, 1);
	if (ERROR_OK != commit())
	{
		LOG_ERROR(_GETTEXT("Fail to erase block at 0x%06X\n"), block_addr);
		return ERRCODE_FAILURE_OPERATION;
	}
	if (buff[0] & STM8_FLASH_IAPSR_WRPGDIS)
	{
		LOG_ERROR(_GETTEXT("current block at 0x%06X is RO\n"), block_addr);
		return ERRCODE_FAILURE_OPERATION;
	}
	return ERROR_OK;
}

static RESULT stm8_program_block(uint32_t block_addr, uint8_t *block_buff)
{
	uint8_t buff[1];
	
	stm8_swim_wotf_reg(STM8_REG_FLASH_CR2, STM8_FLASH_CR2_FPRG);
	stm8_swim_wotf_reg(STM8_REG_FLASH_NCR2, (uint8_t)~STM8_FLASH_CR2_FPRG);
	stm8_swim_wotf(block_addr, block_buff, STM8_FLASH_PAGESIZE);
	delay_ms(3);
	stm8_swim_rotf(STM8_REG_FLASH_IAPSR, buff, 1);
	if (ERROR_OK != commit())
	{
		LOG_ERROR(_GETTEXT("Fail to program block at 0x%06X\n"), block_addr);
		return ERRCODE_FAILURE_OPERATION;
	}
	if (buff[0] & STM8_FLASH_IAPSR_WRPGDIS)
	{
		LOG_ERROR(_GETTEXT("current block at 0x%06X is RO\n"), block_addr);
		return ERRCODE_FAILURE_OPERATION;
	}
	return ERROR_OK;
}

static RESULT stm8_swim_program(struct operation_t operations, 
					struct program_info_t *pi, struct programmer_info_t *prog)
{
	uint16_t voltage;
	uint8_t page_buf[STM8_FLASH_PAGESIZE];
	int32_t i;
	uint32_t j, k, len_current_list;
	RESULT ret;
	uint32_t target_size;
	uint8_t *tbuff, page_size, retry;
	struct memlist **ml, *ml_tmp;
	int verbosity_tmp;

	operations = operations;
	pi = pi;
	p = prog;
	
	// get target voltage
	if (ERROR_OK != get_target_voltage(&voltage))
	{
		return ERROR_FAIL;
	}
	LOG_DEBUG(_GETTEXT(INFOMSG_TARGET_VOLTAGE), voltage / 1000.0);
	if (voltage < 2700)
	{
		LOG_WARNING(_GETTEXT(INFOMSG_TARGET_LOW_POWER));
	}
	
	retry = 3;
	verbosity_tmp = verbosity;
	verbosity = -1;
enter_program_mode:
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
	swim_set_param(10, 20 + 2, 2 + 1);
	delay_ms(10);
	if (ERROR_OK != commit())
	{
		ret = ERROR_FAIL;
		goto leave_program_mode;
	}
	stm8_swim_srst();
	delay_ms(10);
	stm8_swim_wotf_reg(STM8_REG_SWIM_CSR, 
						STM8_SWIM_CSR_SAFT_MASK | STM8_SWIM_CSR_SWIM_DM);
	delay_ms(10);
	reset_set();
	commit();
	stm8_swim_rotf(0x0067F0, page_buf, 6);
	if (ERROR_OK != commit())
	{
		if (retry--)
		{
			swim_fini();
			reset_input();
			swim_input();
			reset_fini();
			commit();
			goto enter_program_mode;
		}
		verbosity = verbosity_tmp;
		ret = ERROR_FAIL;
		goto leave_program_mode;
	}
	verbosity = verbosity_tmp;
	page_buf[6] = '\0';
	LOG_INFO(_GETTEXT("is this chip UID: %s\n"), page_buf);
	
	// enable high speed mode
	stm8_swim_wotf_reg(STM8_REG_SWIM_CSR, 
							STM8_SWIM_CSR_SAFT_MASK | STM8_SWIM_CSR_SWIM_DM 
							| STM8_SWIM_CSR_HS | STM8_SWIM_CSR_RST);
	swim_set_param(10, 8, 2 + 1);
	delay_ms(10);
	if (ERROR_OK != commit())
	{
		ret = ERROR_FAIL;
		goto leave_program_mode;
	}
	
	page_size = STM8_FLASH_PAGESIZE;
	ml = &pi->program_areas[APPLICATION_IDX].memlist;
	target_size = MEMLIST_CalcAllSize(*ml);
	tbuff = pi->program_areas[APPLICATION_IDX].buff;
	
	stm8_unlock_flash();
	if (operations.erase_operations > 0)
	{
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "chip");
		pgbar_init("erasing chip |", "|", 0, 
					target_chip_param.chip_areas[APPLICATION_IDX].page_num, 
					PROGRESS_STEP, '=');
		
		for (j = 0; 
			j < target_chip_param.chip_areas[APPLICATION_IDX].size; 
			j += target_chip_param.chip_areas[APPLICATION_IDX].page_size)
		{
			if (ERROR_OK != stm8_erase_block(STM8_FLASH_ADDR + j))
			{
				pgbar_fini();
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "erase chip");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
			pgbar_update(1);
		}
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "chip");
	}
	
	if (operations.write_operations & APPLICATION)
	{
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = *ml;
		while (ml_tmp != NULL)
		{
			if ((ml_tmp->addr + ml_tmp->len)
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = ml_tmp->len;
			}
			else
			{
				k = page_size - (ml_tmp->addr % page_size);
			}
			
			len_current_list = (uint32_t)ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len 
							- (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				
				if (ERROR_OK != stm8_program_block(ml_tmp->addr + i, 
								&tbuff[ml_tmp->addr - STM8_FLASH_ADDR + i]))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "write flash", ml_tmp->addr + i);
						ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				pgbar_update(k);
				len_current_list -= k;
				if (len_current_list >= page_size)
				{
					k = page_size;
				}
				else
				{
					k = len_current_list;
				}
			}
			
			ml_tmp = MEMLIST_GetNext(ml_tmp);
		}
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), "flash", target_size);
	}
	
	if ((operations.read_operations & APPLICATION) 
		|| (operations.verify_operations & APPLICATION))
	{
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "flash");
		}
		else
		{
			ret = MEMLIST_Add(ml, STM8_FLASH_ADDR, 
						pi->program_areas[APPLICATION_IDX].size, page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							"add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			target_size = MEMLIST_CalcAllSize(*ml);
			LOG_INFO(_GETTEXT(INFOMSG_READING), "flash");
		}
		pgbar_init("reading flash |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = *ml;
		while (ml_tmp != NULL)
		{
			if ((ml_tmp->addr + ml_tmp->len)
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = ml_tmp->len;
			}
			else
			{
				k = page_size - (ml_tmp->addr % page_size);
			}
			
			len_current_list = (uint32_t)ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len 
							- (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				stm8_swim_rotf(ml_tmp->addr + i, page_buf, page_size);
				if (ERROR_OK != commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "read flash", ml_tmp->addr + i);
						ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				if (operations.verify_operations & APPLICATION)
				{
					for (j = 0; j < page_size; j++)
					{
						if (page_buf[j] 
							!= tbuff[ml_tmp->addr - STM8_FLASH_ADDR + i + j])
						{
							pgbar_fini();
							LOG_ERROR(
								_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
								"flash", ml_tmp->addr + i + j, page_buf[j], 
								tbuff[ml_tmp->addr - STM8_FLASH_ADDR + i + j]);
							ret = ERRCODE_FAILURE_VERIFY_TARGET;
							goto leave_program_mode;
						}
					}
				}
				else
				{
					memcpy(&tbuff[ml_tmp->addr - STM8_FLASH_ADDR + i], 
								page_buf, page_size);
				}
				
				pgbar_update(k);
				len_current_list -= k;
				if (len_current_list >= page_size)
				{
					k = page_size;
				}
				else
				{
					k = len_current_list;
				}
			}
			
			ml_tmp = MEMLIST_GetNext(ml_tmp);
		}
		
		pgbar_fini();
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), "flash", target_size);
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ), "flash");
		}
	}
	
	page_size = STM8_EE_PAGESIZE;
	ml = &pi->program_areas[EEPROM_IDX].memlist;
	target_size = MEMLIST_CalcAllSize(*ml);
	tbuff = pi->program_areas[EEPROM_IDX].buff;
	stm8_unlock_eeprom_option();
	if ((operations.read_operations & EEPROM) 
		|| (operations.verify_operations & EEPROM))
	{
		if (operations.verify_operations & EEPROM)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "eeprom");
		}
		else
		{
			ret = MEMLIST_Add(ml, STM8_EE_ADDR, 
							pi->program_areas[EEPROM_IDX].size, page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
							"add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			target_size = MEMLIST_CalcAllSize(*ml);
			LOG_INFO(_GETTEXT(INFOMSG_READING), "eeprom");
		}
		pgbar_init("reading eeprom |", "|", 0, target_size, PROGRESS_STEP, '=');
		
		ml_tmp = *ml;
		while (ml_tmp != NULL)
		{
			if ((ml_tmp->addr + ml_tmp->len)
				<= (ml_tmp->addr - (ml_tmp->addr % page_size) + page_size))
			{
				k = ml_tmp->len;
			}
			else
			{
				k = page_size - (ml_tmp->addr % page_size);
			}
			
			len_current_list = (uint32_t)ml_tmp->len;
			for (i = -(int32_t)(ml_tmp->addr % page_size); 
				 i < ((int32_t)ml_tmp->len 
							- (int32_t)(ml_tmp->addr % page_size)); 
				 i += page_size)
			{
				stm8_swim_rotf(ml_tmp->addr + i, page_buf, page_size);
				if (ERROR_OK != commit())
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_ADDR), 
								  "read eeprom", ml_tmp->addr + i);
						ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				if (operations.verify_operations & EEPROM)
				{
					for (j = 0; j < page_size; j++)
					{
						if (page_buf[j] 
							!= tbuff[ml_tmp->addr - STM8_EE_ADDR + i + j])
						{
							pgbar_fini();
							LOG_ERROR(
								_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
								"eeprom", ml_tmp->addr + i + j, page_buf[j], 
								tbuff[ml_tmp->addr - STM8_EE_ADDR + i + j]);
							ret = ERRCODE_FAILURE_VERIFY_TARGET;
							goto leave_program_mode;
						}
					}
				}
				else
				{
					memcpy(&tbuff[ml_tmp->addr - STM8_EE_ADDR + i], page_buf, 
								page_size);
				}
				
				pgbar_update(k);
				len_current_list -= k;
				if (len_current_list >= page_size)
				{
					k = page_size;
				}
				else
				{
					k = len_current_list;
				}
			}
			
			ml_tmp = MEMLIST_GetNext(ml_tmp);
		}
		
		pgbar_fini();
		if (operations.verify_operations & EEPROM)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), "eeprom", target_size);
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ), "eeprom");
		}
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

