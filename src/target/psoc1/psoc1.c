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

#include "psoc1.h"
#include "psoc1_internal.h"

#define CUR_TARGET_STRING			PSOC1_STRING

const struct program_area_map_t psoc1_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, AREA_ATTR_EW},
	{APPLICATION_CHKSUM_CHAR, 1, 0, 0, AREA_ATTR_R},
	{LOCK_CHAR, 1, 0, 0, AREA_ATTR_W},
	{0, 0, 0, 0, 0}
};

const struct program_mode_t psoc1_program_mode[] = 
{
	{'r', "", ISSP},
	{'p', "", ISSP},
	{0, NULL, 0}
};

#define VECTORS_NUM				17
#define VECTORS_TABLE_SIZE		128

void psoc1_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<r|p>\n\n", 
			CUR_TARGET_STRING);
}

RESULT psoc1_parse_argument(char cmd, const char *argu)
{
	argu = argu;
	
	switch (cmd)
	{
	case 'h':
		psoc1_usage();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}






static struct programmer_info_t *p = NULL;

#define PSOC1_SSC_CMD_SWBootReset			0x00
#define PSOC1_SSC_CMD_ReadBlock				0x01
#define PSOC1_SSC_CMD_WriteBlock			0x02
#define PSOC1_SSC_CMD_EraseBlock			0x03
#define PSOC1_SSC_CMD_ProtectBlock			0x04
#define PSOC1_SSC_CMD_EraseAll				0x05
#define PSOC1_SSC_CMD_TableRead				0x06
#define PSOC1_SSC_CMD_CheckSum				0x07
#define PSOC1_SSC_CMD_Calibrate0			0x08
#define PSOC1_SSC_CMD_Calibrate1			0x09

#define get_target_voltage(v)				p->get_target_voltage(v)

#define issp_init()							p->issp_init()
#define issp_fini()							p->issp_fini()
#define issp_enter_program_mode(mode)		p->issp_enter_program_mode(mode)
#define issp_leave_program_mode(mode)		p->issp_leave_program_mode(mode)
#define issp_wait_and_poll()				p->issp_wait_and_poll()
#define issp_commit()						p->issp_commit()

#define issp_0s()							\
							p->issp_vector(ISSP_VECTOR_0S, 0x00, 0x00, NULL)
#define issp_read_sram(addr, buf)			\
			p->issp_vector(ISSP_VECTOR_READ_SRAM, (uint8_t)(addr), 0x00, (buf))
#define issp_write_sram(addr, data)			\
	p->issp_vector(ISSP_VECTOR_WRITE_SRAM,(uint8_t)(addr),(uint8_t)(data),NULL)
#define issp_write_reg(addr, data)			\
	p->issp_vector(ISSP_VECTOR_WRITE_REG, (uint8_t)(addr), (uint8_t)(data),NULL)

#define issp_set_cup_a(cmd)					issp_write_reg(0xF0, (cmd))
#define issp_set_cup_sp(sp)					issp_write_reg(0xF6, (sp))
#define issp_set_cpu_f(f)					issp_write_reg(0xF7, (f))

#define issp_ssc_set_key1()					issp_write_sram(0xF8, 0x3A)
#define issp_ssc_set_key2(key2)				issp_write_sram(0xF9, (key2))
#define issp_ssc_set_blockid(id)			issp_write_sram(0xFA, (id))
#define issp_ssc_set_pointer(p)				issp_write_sram(0xFB, (p))
#define issp_ssc_set_clock(c)				issp_write_sram(0xFC, (c))
#define issp_ssc_set_delay(dly)				issp_write_sram(0xFE, (dly))
#define issp_ssc_set_cmd(cmd)				issp_set_cup_a(cmd)
#define issp_ssc_execute()					issp_write_reg(0xFF, 0x12)

#define issp_sel_reg_bank(xio)				issp_set_cpu_f((xio) ? 0x10 : 0x00)
#define issp_set_flash_bank(bank)			issp_write_reg(0xFA, (bank) & 0x03)

#define PSOC1_ISSP_SSC_DEFAULT_SP			0x08
#define PSOC1_ISSP_SSC_DEFAULT_POINTER		0x80
#define PSOC1_ISSP_SSC_DEFAULT_CLOCK_ERASE	0x15
#define PSOC1_ISSP_SSC_DEFAULT_CLOCK_FLASH	0x54
#define PSOC1_ISSP_SSC_DEFAULT_DELAY		0x56
#define PSOC1_ISSP_SSC_RETURN_OK			0x00

RESULT issp_wait_and_poll_with_ret(uint8_t *buf, uint8_t want_ssc_return_value)
{
	uint8_t i;

#ifdef PARAM_CHECK
	if (want_ssc_return_value > 8)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (ERROR_OK != issp_wait_and_poll())
	{
		return ERROR_FAIL;
	}
	
	for (i = 0; i < want_ssc_return_value; i++)
	{
		if (ERROR_OK != issp_read_sram(0xF8 + i, buf + i))
		{
			return ERROR_FAIL;
		}
	}
	if (ERROR_OK != issp_commit())
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT issp_init3_half(uint8_t f9_1, uint8_t f9_2)
{
	issp_write_reg(0xF7, 0x00);
	issp_write_reg(0xF4, 0x03);
	issp_write_reg(0xF5, 0x00);
	issp_write_reg(0xF6, 0x08);
	issp_write_reg(0xF8, 0x51);
	issp_write_reg(0xF9, f9_1);
	issp_write_reg(0xFA, 0x30);
	issp_write_reg(0xFF, 0x12);
	issp_0s();
	
	issp_write_reg(0xF7, 0x00);
	issp_write_reg(0xF4, 0x03);
	issp_write_reg(0xF5, 0x00);
	issp_write_reg(0xF6, 0x08);
	issp_write_reg(0xF8, 0x60);
	issp_write_reg(0xF9, f9_2);
	issp_write_reg(0xFA, 0x30);
	issp_write_reg(0xF7, 0x10);
	issp_write_reg(0xFF, 0x12);
	issp_0s();
	
	return ERROR_OK;
}

RESULT issp_call_ssc(uint8_t cmd, uint8_t id, uint8_t poll_ready, uint8_t * buf, 
					 uint8_t want_return)
{
	issp_sel_reg_bank(0x00);
	issp_set_cup_sp(PSOC1_ISSP_SSC_DEFAULT_SP);
	issp_ssc_set_key1();
	issp_ssc_set_key2(PSOC1_ISSP_SSC_DEFAULT_SP + 3);
	issp_write_reg(0xF5, 0x00);
	issp_write_reg(0xF4, 0x03);
	issp_ssc_set_pointer(0x80);
	issp_write_reg(0xF9, 0x30);
	issp_write_reg(0xFA, 0x40);
	issp_ssc_set_blockid(id);
	issp_ssc_set_cmd(cmd);
	issp_write_reg(0xF8, 0x00);
	issp_ssc_execute();
	
	if (poll_ready > 0)
	{
		return issp_wait_and_poll_with_ret(buf, want_return);
	}
	else
	{
		return issp_commit();
	}
}

RESULT psoc1_program(struct operation_t operations, struct program_info_t *pi, 
					 struct programmer_info_t *prog)
{
	uint16_t voltage;
	uint8_t bank, addr, page_buf[64];
	RESULT ret = ERROR_OK;
	uint8_t tmp8;
	uint16_t tmp16;
	uint16_t block;
	uint16_t checksum = 0;
	uint32_t target_size, page_size, page_num;
	uint8_t *tbuff;
	struct memlist **ml;
	
	p = prog;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if ((   (operations.read_operations & APPLICATION) 
			&& (NULL == pi->program_areas[APPLICATION_IDX].buff)) 
		|| ((   (operations.write_operations & APPLICATION) 
				|| (operations.verify_operations & APPLICATION)) 
			&& (NULL == pi->program_areas[APPLICATION_IDX].buff)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for flash");
		return ERRCODE_INVALID_BUFFER;
	}
	if ((   (operations.read_operations & LOCK) 
			&& (NULL == pi->program_areas[LOCK_IDX].buff)) 
		|| ((   (operations.write_operations & LOCK) 
				|| (operations.verify_operations & LOCK)) 
			&& (NULL == pi->program_areas[LOCK_IDX].buff)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for secure");
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	ml = &pi->program_areas[APPLICATION_IDX].memlist;
	target_size = MEMLIST_CalcAllSize(*ml);
	if ((operations.read_operations & APPLICATION) 
		|| (operations.write_operations & APPLICATION))
	{
		if (target_size != (uint32_t)(
						target_chip_param.param[PSOC1_PARAM_BANK_NUM] 
						* target_chip_param.chip_areas[APPLICATION_IDX].page_num
						* target_chip_param.chip_areas[APPLICATION_IDX].page_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID), "flash size", "target chip");
			return ERRCODE_INVALID;
		}
	}
	
	// check mode
	switch (program_mode)
	{
	case PSOC1_RESET_MODE:
		if (!(target_chip_param.program_mode & (1 << PSOC1_RESET_MODE)))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "RESET", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		break;
	case PSOC1_POWERON_MODE:
		if (!(target_chip_param.program_mode & (1 << PSOC1_POWERON_MODE)))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "POWERON", 
					  target_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), program_mode, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID_PROG_MODE;
		break;
	}
	
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
	
	// here we go
	// ISSP Init
	if (ERROR_OK != issp_init())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "initialize issp");
		return ERRCODE_FAILURE_OPERATION;
	}
	
	// enter program mode
	if (program_mode & (1 << PSOC1_RESET_MODE))
	{
		// Reset Mode
		// enter prog mode
		if (ERROR_OK != issp_enter_program_mode(ISSP_PM_RESET))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "enter program mode");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
	}
	else
	{
		if (voltage > 2000)
		{
			LOG_ERROR(_GETTEXT("Target should power off in power-on mode\n"));
			ret = ERROR_FAIL;
			goto leave_program_mode;
		}
		
		if (ERROR_OK != issp_enter_program_mode(ISSP_PM_POWER_ON))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "enter program mode");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
	}
	
	// read chip_id
	// call table_read no.0 and read 2 bytes from 0xF8 in sram
	pi->chip_id = 0;
	ret = issp_call_ssc(PSOC1_SSC_CMD_TableRead, 0, 1, (uint8_t*)&pi->chip_id, 2);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "read chip id");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	LOG_INFO(_GETTEXT(INFOMSG_TARGET_CHIP_ID), pi->chip_id);
	if (!(operations.read_operations & CHIPID))
	{
		if (pi->chip_id != target_chip_param.chip_id)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHIP_ID), pi->chip_id, 
					  target_chip_param.chip_id);
			ret = ERRCODE_INVALID_CHIP_ID;
			goto leave_program_mode;
		}
	}
	else
	{
		goto leave_program_mode;
	}
	
	// init
	// init1 call_calibrate
	// call calibrate1
	ret = issp_call_ssc(PSOC1_SSC_CMD_Calibrate1, 0, 1, NULL, 0);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "call calibrate1");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	// init2 read table no.1
	ret = issp_call_ssc(PSOC1_SSC_CMD_TableRead, 1, 1, NULL, 0);
	if (ret != ERROR_OK)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
				  "read table no.1");
		ret = ERRCODE_FAILURE_OPERATION;
		goto leave_program_mode;
	}
	// init3 do the hell
	if (voltage < 4000)
	{
		// 3.3V
		issp_init3_half(0xF8, 0xEA);
		issp_init3_half(0xF9, 0xE8);
	}
	else
	{
		// 5V
		issp_init3_half(0xFC, 0xEA);
		issp_init3_half(0xFD, 0xE8);
	}
	
	// init sys_clock
	issp_sel_reg_bank(1);
	issp_write_reg(0xE0, 0x02);
	
	if (operations.erase_operations > 0)
	{
		LOG_INFO(_GETTEXT(INFOMSG_ERASING), "chip");
		pgbar_init("erasing chip |", "|", 0, 1, PROGRESS_STEP, '=');
		
		issp_ssc_set_clock(PSOC1_ISSP_SSC_DEFAULT_CLOCK_ERASE);
		issp_ssc_set_delay(PSOC1_ISSP_SSC_DEFAULT_DELAY);
		
		ret = issp_call_ssc(PSOC1_SSC_CMD_EraseAll, 0, 1, &tmp8, 1);
		if ((ret != ERROR_OK) || (tmp8 != PSOC1_ISSP_SSC_RETURN_OK))
		{
			pgbar_fini();
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "call erase_all");
			ret = ERRCODE_FAILURE_OPERATION;
			goto leave_program_mode;
		}
		
		pgbar_update(1);
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_ERASED), "chip");
	}
	
	page_size = target_chip_param.chip_areas[APPLICATION_IDX].page_size;
	page_num = target_chip_param.chip_areas[APPLICATION_IDX].page_num;
	tbuff = pi->program_areas[APPLICATION_IDX].buff;
	if (operations.write_operations & APPLICATION)
	{
		// program flash
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, 
					(target_size + page_size - 1) / page_size, 
					PROGRESS_STEP, '=');
		
		checksum = 0;
		for (bank = 0; 
			bank < target_chip_param.param[PSOC1_PARAM_BANK_NUM]; 
			bank++)
		{
			// select bank by write xio in fls_pr1(in reg_bank 1)
			if (target_chip_param.param[PSOC1_PARAM_BANK_NUM] > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			
			for (block = 0; 
				block < target_chip_param.chip_areas[APPLICATION_IDX].page_num; 
				block++)
			{
				uint32_t block_num = bank * page_num + block;
				uint32_t block_addr = block_num * page_size;
				
				// write data into sram
				for (addr = 0; addr < page_size; addr++)
				{
					checksum += tbuff[block_addr + addr];
					issp_write_sram(PSOC1_ISSP_SSC_DEFAULT_POINTER + addr, 
									tbuff[block_addr + addr]);
				}
				issp_ssc_set_clock(PSOC1_ISSP_SSC_DEFAULT_CLOCK_FLASH);
				issp_ssc_set_delay(PSOC1_ISSP_SSC_DEFAULT_DELAY);
				
				ret = issp_call_ssc(PSOC1_SSC_CMD_WriteBlock, 
									(uint8_t)(block & 0xFF), 1, &tmp8, 1);
				if ((ret != ERROR_OK) || (tmp8 != PSOC1_ISSP_SSC_RETURN_OK))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_MESSAGE), 
							  "write flash", "block secured?");
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				pgbar_update(1);
			}
		}
		
		pgbar_fini();
		if (pi->program_areas[APPLICATION_IDX].checksum_value != checksum)
		{
			LOG_DEBUG(_GETTEXT(INFOMSG_CHECKSUM), checksum);
		}
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
			ret = MEMLIST_Add(ml, 0, pi->program_areas[APPLICATION_IDX].size, 
								page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			target_size = MEMLIST_CalcAllSize(*ml);
			LOG_INFO(_GETTEXT(INFOMSG_READING), "flash");
		}
		pgbar_init("reading flash |", "|", 0, 
					(target_size + page_size - 1) / page_size, 
					PROGRESS_STEP, '=');
		
		checksum = 0;
		for (bank = 0; 
			bank < target_chip_param.param[PSOC1_PARAM_BANK_NUM]; 
			bank++)
		{
			if (target_chip_param.param[PSOC1_PARAM_BANK_NUM] > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			
			for (block = 0; block < page_num; block++)
			{
				uint32_t block_num = bank * page_num + block;
				uint32_t block_addr = block_num * page_size;
				
				ret = issp_call_ssc(PSOC1_SSC_CMD_ReadBlock, 
									(uint8_t)(block & 0xFF), 1, &tmp8, 1);
				if ((ret != ERROR_OK) || (tmp8 != PSOC1_ISSP_SSC_RETURN_OK))
				{
					pgbar_fini();
					LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION_MESSAGE), 
								"call read_block", "block secured?");
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				for (addr = 0; addr < page_size; addr++)
				{
					issp_read_sram(PSOC1_ISSP_SSC_DEFAULT_POINTER + addr, 
								   page_buf + addr);
				}
				
				// commit
				if (ERROR_OK != issp_commit())
				{
					pgbar_fini();
					ret = ERROR_FAIL;
					goto leave_program_mode;
				}
				
				// read or verify
				for (addr = 0; addr < page_size; addr++)
				{
					checksum += page_buf[addr];
					if (operations.verify_operations & APPLICATION)
					{
						// verify
						if (page_buf[addr] != tbuff[block_addr + addr])
						{
							pgbar_fini();
							LOG_ERROR(
								_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
								"flash", addr, page_buf[addr], 
								tbuff[block_addr + addr]);
							ret = ERRCODE_FAILURE_VERIFY_TARGET;
							goto leave_program_mode;
						}
					}
					else
					{
						// read
						tbuff[block_addr + addr] = page_buf[addr];
					}
				}
				
				pgbar_update(1);
			}
		}
		
		pgbar_fini();
		if (pi->program_areas[APPLICATION_IDX].checksum_value != checksum)
		{
			LOG_DEBUG(_GETTEXT(INFOMSG_CHECKSUM), checksum);
		}
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFIED_SIZE), "flash", target_size);
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READ), "flash");
		}
		
		
		// checksum
		if ((pi->areas_defined & APPLICATION_CHKSUM) 
			&& (operations.verify_operations & APPLICATION))
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "checksum");
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READING), "checksum");
		}
		
		checksum = 0;
		for (bank = 0; 
			bank < target_chip_param.param[PSOC1_PARAM_BANK_NUM]; 
			bank++)
		{
			if (target_chip_param.param[PSOC1_PARAM_BANK_NUM] > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			
			ret = issp_call_ssc(PSOC1_SSC_CMD_CheckSum, (uint8_t)page_num, 1, 
								(uint8_t*)&tmp16, 2);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
						  "read checksum");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
			
			checksum += tmp16;
			LOG_DEBUG(_GETTEXT(INFOMSG_CHECKSUM_BANK), bank, tmp16);
		}
		
		if ((pi->areas_defined & APPLICATION_CHKSUM) 
			&& (operations.verify_operations & APPLICATION))
		{
			// verify
			if (pi->program_areas[APPLICATION_IDX].checksum_value == checksum)
			{
				LOG_INFO(_GETTEXT(INFOMSG_VERIFIED), "checksum");
			}
			else
			{
				LOG_INFO(_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_04X), 
					"checksum", checksum, 
					(uint16_t)pi->program_areas[APPLICATION_IDX].checksum_value);
			}
		}
		else
		{
			// read
			LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE), "checksum", checksum);
		}
	}
	
	tbuff = pi->program_areas[LOCK_IDX].buff;
	if (operations.write_operations & LOCK)
	{
		// program secure
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "secure");
		pgbar_init("writing secure |", "|", 0, 
			target_chip_param.param[PSOC1_PARAM_BANK_NUM], PROGRESS_STEP, '=');
		
		for (bank = 0; 
			bank < target_chip_param.param[PSOC1_PARAM_BANK_NUM]; 
			bank++)
		{
			uint32_t lock_bank_addr = bank * (page_num >> 2);
			
			if (target_chip_param.param[PSOC1_PARAM_BANK_NUM] > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			for (addr = 0; addr < (page_num >> 2); addr++)
			{
				issp_write_sram(PSOC1_ISSP_SSC_DEFAULT_POINTER + addr, 
								tbuff[lock_bank_addr + addr]);
			}
			issp_ssc_set_clock(PSOC1_ISSP_SSC_DEFAULT_CLOCK_FLASH);
			issp_ssc_set_delay(PSOC1_ISSP_SSC_DEFAULT_DELAY);
			
			ret = issp_call_ssc(PSOC1_SSC_CMD_ProtectBlock, 0, 1, &tmp8, 1);
			if ((ret != ERROR_OK) || (tmp8 != PSOC1_ISSP_SSC_RETURN_OK))
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "write secure");
				ret = ERRCODE_FAILURE_OPERATION;
				goto leave_program_mode;
			}
			
			pgbar_update(1);
		}
		
		pgbar_fini();
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED), "secure");
	}
	
leave_program_mode:
	// leave program mode
	if (program_mode & (1 << PSOC1_RESET_MODE))
	{
		if (ERROR_OK != issp_leave_program_mode(ISSP_PM_RESET))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "leave program mode");
			ret = ERRCODE_FAILURE_OPERATION;
		}
	}
	else
	{
		if (ERROR_OK != issp_leave_program_mode(ISSP_PM_POWER_ON))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "leave program mode");
			ret = ERRCODE_FAILURE_OPERATION;
		}
	}
	issp_fini();
	if (ERROR_OK != issp_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATE_DEVICE), "target chip");
		ret = ERRCODE_FAILURE_OPERATION;
	}
	
	return ret;
}

