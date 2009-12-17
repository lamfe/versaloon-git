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
#define cur_chip_param				target_chip_param
#define cur_chips_param				target_chips.chips_param
#define cur_chips_num				target_chips.num_of_chips
#define cur_prog_mode				program_mode
#define cur_target_defined			target_defined

const program_area_map_t psoc1_program_area_map[] = 
{
	{APPLICATION, APPLICATION_CHAR, 1, 0, 0, PSOC1_FLASH_CHAR},
	{LOCK, LOCK_CHAR, 1, 0, 0x00100000, PSOC1_SECURE_CHAR},
	{0, 0, 0, 0, 0, 0}
};

#define VECTORS_NUM				17
#define VECTORS_TABLE_SIZE		128

void psoc1_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<r|p>\n\n", CUR_TARGET_STRING);
}

void psoc1_support(void)
{
	uint32_t i;

	printf("Support list of %s:\n", CUR_TARGET_STRING);
	for (i = 0; i < cur_chips_num; i++)
	{
		printf("\
%s: id = 0x%04x, init_mode = %s, flash_size = %d, secure_size = %d\n", 
				cur_chips_param[i].chip_name, 
				cur_chips_param[i].chip_id,
				cur_chips_param[i].program_mode_str, 
				cur_chips_param[i].app_page_size 
					* cur_chips_param[i].app_page_num 
					* cur_chips_param[i].param[PSOC1_PARAM_BANK_NUM],
				cur_chips_param[i].param[PSOC1_PARAM_BANK_NUM] 
					* cur_chips_param[i].app_page_num >> 2);
	}
	printf("\n");
}

RESULT psoc1_parse_argument(char cmd, const char *argu)
{
	argu = argu;
	
	switch (cmd)
	{
	case 'h':
		psoc1_usage();
		break;
	case 'S':
		psoc1_support();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

RESULT psoc1_prepare_buffer(program_info_t *pi)
{
	if (pi->app != NULL)
	{
		memset(pi->app, PSOC1_FLASH_CHAR, pi->app_size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	if (pi->lock != NULL)
	{
		memset(pi->lock, PSOC1_SECURE_CHAR, pi->lock_size);
	}
	else
	{
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

RESULT psoc1_fini(program_info_t *pi, programmer_info_t *prog)
{
	pi = pi;
	prog = prog;
	
	return ERROR_OK;
}

RESULT psoc1_init(program_info_t *pi, programmer_info_t *prog)
{
	uint8_t i;
	operation_t opt_tmp;
	
	memset(&opt_tmp, 0, sizeof(opt_tmp));
	
	if (strcmp(pi->chip_type, CUR_TARGET_STRING))
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_HANDLER), CUR_TARGET_STRING, 
				pi->chip_type);
		return ERRCODE_INVALID_HANDLER;
	}
	
	if (NULL == pi->chip_name)
	{
		// auto detect
		LOG_INFO(_GETTEXT(INFOMSG_TRY_AUTODETECT));
		opt_tmp.read_operations = CHIP_ID;
		cur_chip_param.program_mode = PSOC1_RESET_MODE | PSOC1_POWERON_MODE;
		
		if (ERROR_OK != psoc1_program(opt_tmp, pi, prog))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
			return ERRCODE_AUTODETECT_FAIL;
		}
		
		LOG_INFO(_GETTEXT(INFOMSG_AUTODETECT_SIGNATURE), pi->chip_id);
		for (i = 0; i < cur_chips_num; i++)
		{
			if (pi->chip_id == cur_chips_param[i].chip_id)
			{
				memcpy(&cur_chip_param, cur_chips_param + i, 
					   sizeof(cur_chip_param));
				cur_chip_param.lock_size = 
									cur_chip_param.param[PSOC1_PARAM_BANK_NUM] 
									* cur_chip_param.app_page_num >> 2;
				if (cur_chip_param.lock_size < PSOC1_MIN_SECURE_SIZE)
				{
					cur_chip_param.lock_size = PSOC1_MIN_SECURE_SIZE;
				}
				cur_chip_param.app_size = 
									cur_chip_param.param[PSOC1_PARAM_BANK_NUM] 
									* cur_chip_param.app_page_num 
									* cur_chip_param.app_page_size;
				
				pi->app_size = cur_chip_param.app_size;
				pi->lock_size = cur_chip_param.lock_size;
				
				LOG_INFO(_GETTEXT(INFOMSG_CHIP_FOUND), 
						 cur_chip_param.chip_name);
				pi->chip_name = (char *)cur_chip_param.chip_name;
				
				return ERROR_OK;
			}
		}
		
		LOG_ERROR(_GETTEXT(ERRMSG_AUTODETECT_FAIL), pi->chip_type);
		return ERRCODE_AUTODETECT_FAIL;
	}
	else
	{
		for (i = 0; i < cur_chips_num; i++)
		{
			if (!strcmp(cur_chips_param[i].chip_name, pi->chip_name))
			{
				memcpy(&cur_chip_param, cur_chips_param + i, 
					   sizeof(cur_chip_param));
				cur_chip_param.lock_size = 
									cur_chip_param.param[PSOC1_PARAM_BANK_NUM] 
									* cur_chip_param.app_page_num >> 2;
				if (cur_chip_param.lock_size < PSOC1_MIN_SECURE_SIZE)
				{
					cur_chip_param.lock_size = PSOC1_MIN_SECURE_SIZE;
				}
				cur_chip_param.app_size = 
									cur_chip_param.param[PSOC1_PARAM_BANK_NUM] 
									* cur_chip_param.app_page_num 
									* cur_chip_param.app_page_size;
				
				pi->app_size = cur_chip_param.app_size;
				pi->lock_size = cur_chip_param.lock_size;
				
				return ERROR_OK;
			}
		}
		
		return ERROR_FAIL;
	}
}

uint32_t psoc1_interface_needed(void)
{
	return PSOC1_INTERFACE_NEEDED;
}

RESULT psoc1_write_buffer_from_file_callback(uint32_t address, uint32_t seg_addr, 
											 uint8_t* data, uint32_t length, 
											 void* buffer)
{
	program_info_t *pi = (program_info_t *)buffer;
	uint32_t mem_addr = address & 0x0000FFFF;
	RESULT ret;
	
#ifdef PARAM_CHECK
	if ((length > 0) && (NULL == data))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
#endif
	
	if (seg_addr != 0)
	{
		LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), 
				  "segment address", CUR_TARGET_STRING);
		return ERRCODE_NOT_SUPPORT;
	}
	
	// flash from 0x00000000, secure from 0x00100000, checksum from 0x00200000
	switch (address >> 16)
	{
	case 0x0000:
		if (NULL == pi->app)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), 
					  "pi->app");
			return ERRCODE_INVALID_BUFFER;
		}
		
		if ((mem_addr >= cur_chip_param.app_size) 
			|| (length > cur_chip_param.app_size) 
			|| ((mem_addr + length) > cur_chip_param.app_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "flash memory");
			return ERRCODE_INVALID;
		}
		cur_target_defined |= APPLICATION;
		
		memcpy(pi->app + mem_addr, data, length);
		
		ret = MEMLIST_Add(&pi->app_memlist, mem_addr, length, 
						  cur_chip_param.app_page_size);
		if (ret != ERROR_OK)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
			return ERRCODE_FAILURE_OPERATION;
		}
		break;
	case 0x0010:
		if (NULL == pi->lock)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), 
					  "pi->lock");
			return ERRCODE_INVALID_BUFFER;
		}
		
		if ((mem_addr >= cur_chip_param.lock_size) 
			|| (length > cur_chip_param.lock_size) 
			|| ((mem_addr + length) > cur_chip_param.lock_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "secure memory");
			return ERRCODE_INVALID;
		}
		cur_target_defined |= LOCK;
		memcpy(pi->lock + mem_addr, data, length);
		break;
	case 0x0020:
		if ((mem_addr != 0) || (length != 2))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_RANGE), "checksum memory");
			return ERRCODE_INVALID;
		}
		cur_target_defined |= USER_TARGET(0);
		pi->app_checksum_value = (data[0] << 8) + data[1];
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_ADDRESS), address, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID;
		break;
	}
	
	return ERROR_OK;
}


RESULT psoc1_get_mass_product_data_size(operation_t operations, 
										program_info_t *pi, uint32_t *size)
{
	operations = operations;
	
	// prog_mode(1 byte), flash_size(4 bytes), 
	// secure_size(4 bytes), checksum(2 bytes)
	*size = sizeof(cur_chip_param) + sizeof(operations) + 9 
		+ MEMLIST_CalcAllSize(pi->app_memlist) 
		+ MEMLIST_CalcAllSize(pi->lock_memlist);
	
	return ERROR_OK;
}

RESULT psoc1_prepare_mass_product_data(operation_t operations, 
									   program_info_t *pi, uint8_t *buff)
{
	uint32_t index = 0;
	uint32_t target_size;
	
#ifdef PARAM_CHECK
	if ((   (operations.read_operations & APPLICATION) 
			&& (NULL == pi->app)) 
		|| ((   (operations.write_operations & APPLICATION) 
				|| (operations.verify_operations & APPLICATION)) 
			&& (NULL == pi->app)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for flash");
		return ERRCODE_INVALID_BUFFER;
	}
	if ((   (operations.read_operations & LOCK) 
			&& (NULL == pi->lock)) 
		|| ((   (operations.write_operations & LOCK) 
				|| (operations.verify_operations & LOCK)) 
			&& (NULL == pi->lock)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for secure");
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	// check mode
	switch (cur_prog_mode & PSOC1_MODE_MASK)
	{
	case 0:
		LOG_WARNING(_GETTEXT(INFOMSG_USE_DEFAULT), "Program mode", 
					"RESET mode");
		cur_prog_mode = PSOC1_RESET_MODE;
	case PSOC1_RESET_MODE:
		if (!(cur_chip_param.program_mode & PSOC1_RESET_MODE))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "RESET", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		break;
	case PSOC1_POWERON_MODE:
		if (!(cur_chip_param.program_mode & PSOC1_POWERON_MODE))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "POWERON", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), cur_prog_mode, 
				  CUR_TARGET_STRING);
		return ERRCODE_INVALID_PROG_MODE;
		break;
	}
	
	memcpy(buff + index, &cur_chip_param, sizeof(cur_chip_param));
	index += sizeof(cur_chip_param);
	memcpy(buff + index, &operations, sizeof(operations));
	index += sizeof(operations);
	memcpy(buff + index, &cur_prog_mode, sizeof(cur_prog_mode));
	index += sizeof(cur_prog_mode);
	target_size = MEMLIST_CalcAllSize(pi->app_memlist);
	memcpy(buff + index, &target_size, sizeof(target_size));
	index += sizeof(target_size);
	target_size = MEMLIST_CalcAllSize(pi->lock_memlist);
	memcpy(buff + index, &target_size, sizeof(target_size));
	index += sizeof(target_size);
	memcpy(buff + index, &pi->app_checksum_value, 
		   sizeof(pi->app_checksum_value));
	index += sizeof(pi->app_checksum_value);
	memcpy(buff + index, pi->app, MEMLIST_CalcAllSize(pi->app_memlist));
	index += MEMLIST_CalcAllSize(pi->app_memlist);
	memcpy(buff + index, pi->lock, MEMLIST_CalcAllSize(pi->lock_memlist));
	index += MEMLIST_CalcAllSize(pi->lock_memlist);
	
	return ERROR_OK;
}






static programmer_info_t *p = NULL;

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

#define issp_0s()							p->issp_vector(ISSP_VECTOR_0S, 0x00, 0x00, NULL)
#define issp_read_sram(addr, buf)			p->issp_vector(ISSP_VECTOR_READ_SRAM, (uint8_t)(addr), 0x00, (buf))
#define issp_write_sram(addr, data)			p->issp_vector(ISSP_VECTOR_WRITE_SRAM, (uint8_t)(addr), (uint8_t)(data), NULL)
#define issp_write_reg(addr, data)			p->issp_vector(ISSP_VECTOR_WRITE_REG, (uint8_t)(addr), (uint8_t)(data), NULL)

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

RESULT psoc1_program(operation_t operations, program_info_t *pi, 
					 programmer_info_t *prog)
{
	uint16_t voltage;
	uint8_t bank, addr, page_buf[64];
	RESULT ret = ERROR_OK;
	uint8_t tmp8;
	uint16_t tmp16;
	uint16_t block;
	uint16_t checksum = 0;
	uint32_t target_size;
	
	p = prog;
	
#ifdef PARAM_CHECK
	if (NULL == prog)
	{
		LOG_BUG(_GETTEXT(ERRMSG_INVALID_PARAMETER), __FUNCTION__);
		return ERRCODE_INVALID_PARAMETER;
	}
	if ((   (operations.read_operations & APPLICATION) 
			&& (NULL == pi->app)) 
		|| ((   (operations.write_operations & APPLICATION) 
				|| (operations.verify_operations & APPLICATION)) 
			&& (NULL == pi->app)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for flash");
		return ERRCODE_INVALID_BUFFER;
	}
	if ((   (operations.read_operations & LOCK) 
			&& (NULL == pi->lock)) 
		|| ((   (operations.write_operations & LOCK) 
				|| (operations.verify_operations & LOCK)) 
			&& (NULL == pi->lock)))
	{
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_BUFFER), "for secure");
		return ERRCODE_INVALID_BUFFER;
	}
#endif
	
	target_size = MEMLIST_CalcAllSize(pi->app_memlist);
	if ((operations.read_operations & APPLICATION) 
		|| (operations.write_operations & APPLICATION))
	{
		if (target_size != (uint32_t)(
								cur_chip_param.param[PSOC1_PARAM_BANK_NUM] 
								* cur_chip_param.app_page_num 
								* cur_chip_param.app_page_size))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID), "flash size", "target chip");
			return ERRCODE_INVALID;
		}
	}
	
	// check mode
	switch (cur_prog_mode & PSOC1_MODE_MASK)
	{
	case 0:
		LOG_WARNING(_GETTEXT(INFOMSG_USE_DEFAULT), "Program mode", 
					"RESET mode");
		cur_prog_mode = PSOC1_RESET_MODE;
	case PSOC1_RESET_MODE:
		if (!(cur_chip_param.program_mode & PSOC1_RESET_MODE))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "RESET", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		break;
	case PSOC1_POWERON_MODE:
		if (!(cur_chip_param.program_mode & PSOC1_POWERON_MODE))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_NOT_SUPPORT_BY), "POWERON", 
					  cur_chip_param.chip_name);
			return ERRCODE_NOT_SUPPORT;
		}
		break;
	default:
		LOG_ERROR(_GETTEXT(ERRMSG_INVALID_PROG_MODE), cur_prog_mode, 
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
	if (cur_prog_mode & PSOC1_RESET_MODE)
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
	if (!(operations.read_operations & CHIP_ID))
	{
		if (pi->chip_id != cur_chip_param.chip_id)
		{
			LOG_ERROR(_GETTEXT(ERRMSG_INVALID_CHIP_ID), pi->chip_id, 
					  cur_chip_param.chip_id);
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
	
	if (operations.write_operations & APPLICATION)
	{
		// program flash
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "flash");
		pgbar_init("writing flash |", "|", 0, 
				   (target_size + cur_chip_param.app_page_size - 1) 
						/ cur_chip_param.app_page_size, 
				   PROGRESS_STEP, '=');
		
		checksum = 0;
		for (bank = 0; bank < cur_chip_param.param[PSOC1_PARAM_BANK_NUM]; 
			 bank++)
		{
			// select bank by write xio in fls_pr1(in reg_bank 1)
			if (cur_chip_param.param[PSOC1_PARAM_BANK_NUM] > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			
			for (block = 0; block < cur_chip_param.app_page_num; block++)
			{
				uint32_t block_num = 
							bank * cur_chip_param.app_page_num + block;
				uint32_t block_addr = block_num * cur_chip_param.app_page_size;
				
				// write data into sram
				for (addr = 0; addr < cur_chip_param.app_page_size; addr++)
				{
					checksum += pi->app[block_addr + addr];
					issp_write_sram(PSOC1_ISSP_SSC_DEFAULT_POINTER + addr, 
									pi->app[block_addr + addr]);
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
		if (pi->app_checksum_value != checksum)
		{
			LOG_DEBUG(_GETTEXT(INFOMSG_CHECKSUM), checksum);
		}
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMED_SIZE), "flash", target_size);
	}
	
	if (operations.read_operations & APPLICATION)
	{
		if (operations.verify_operations & APPLICATION)
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "flash");
		}
		else
		{
			ret = MEMLIST_Add(&pi->app_memlist, 0, pi->app_size, 
								cur_chip_param.app_page_size);
			if (ret != ERROR_OK)
			{
				LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), "add memory list");
				return ERRCODE_FAILURE_OPERATION;
			}
			target_size = MEMLIST_CalcAllSize(pi->app_memlist);
			LOG_INFO(_GETTEXT(INFOMSG_READING), "flash");
		}
		pgbar_init("reading flash |", "|", 0, 
					(target_size + cur_chip_param.app_page_size - 1) 
						/ cur_chip_param.app_page_size, 
					PROGRESS_STEP, '=');
		
		checksum = 0;
		for (bank = 0; bank < cur_chip_param.param[PSOC1_PARAM_BANK_NUM]; bank++)
		{
			if (cur_chip_param.param[PSOC1_PARAM_BANK_NUM] > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			
			for (block = 0; block < cur_chip_param.app_page_num; block++)
			{
				uint32_t block_num = 
							bank * cur_chip_param.app_page_num + block;
				uint32_t block_addr = block_num * cur_chip_param.app_page_size;
				
				ret = issp_call_ssc(PSOC1_SSC_CMD_ReadBlock, 
									(uint8_t)(block & 0xFF), 1, &tmp8, 1);
				if ((ret != ERROR_OK) || (tmp8 != PSOC1_ISSP_SSC_RETURN_OK))
				{
					pgbar_fini();
					LOG_ERROR(
						_GETTEXT(ERRMSG_FAILURE_OPERATION_MESSAGE), 
						"call read_block", "block secured?");
					ret = ERRCODE_FAILURE_OPERATION;
					goto leave_program_mode;
				}
				
				for (addr = 0; addr < cur_chip_param.app_page_size; addr++)
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
				for (addr = 0; addr < cur_chip_param.app_page_size; addr++)
				{
					checksum += page_buf[addr];
					if (operations.verify_operations & APPLICATION)
					{
						// verify
						if (page_buf[addr] != pi->app[block_addr + addr])
						{
							pgbar_fini();
							LOG_ERROR(
								_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_AT_02X), 
								"flash", addr, page_buf[addr], 
								pi->app[block_addr + addr]);
							ret = ERRCODE_FAILURE_VERIFY_TARGET;
							goto leave_program_mode;
						}
					}
					else
					{
						// read
						pi->app[block_addr + addr] = page_buf[addr];
					}
				}
				
				pgbar_update(1);
			}
		}
		
		pgbar_fini();
		if (pi->app_checksum_value != checksum)
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
		if ((cur_target_defined & USER_TARGET(0)) 
			&& (operations.verify_operations & APPLICATION))
		{
			LOG_INFO(_GETTEXT(INFOMSG_VERIFYING), "checksum");
		}
		else
		{
			LOG_INFO(_GETTEXT(INFOMSG_READING), "checksum");
		}
		
		checksum = 0;
		for (bank = 0; bank < cur_chip_param.param[PSOC1_PARAM_BANK_NUM]; bank++)
		{
			if (cur_chip_param.param[PSOC1_PARAM_BANK_NUM] > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			
			ret = issp_call_ssc(PSOC1_SSC_CMD_CheckSum, 
								(uint8_t)(cur_chip_param.app_page_num), 
								1, (uint8_t*)&tmp16, 2);
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
		
		if ((cur_target_defined & USER_TARGET(0)) 
			&& (operations.verify_operations & APPLICATION))
		{
			// verify
			if (pi->app_checksum_value == checksum)
			{
				LOG_INFO(_GETTEXT(INFOMSG_VERIFIED), "checksum");
			}
			else
			{
				LOG_INFO(_GETTEXT(ERRMSG_FAILURE_VERIFY_TARGET_04X), 
						 "checksum", checksum, pi->app_checksum_value);
			}
		}
		else
		{
			// read
			LOG_INFO(_GETTEXT(INFOMSG_READ_VALUE), "checksum", checksum);
		}
	}
	
	if (operations.write_operations & LOCK)
	{
		// program secure
		LOG_INFO(_GETTEXT(INFOMSG_PROGRAMMING), "secure");
		pgbar_init("writing secure |", "|", 0, 
				   cur_chip_param.param[PSOC1_PARAM_BANK_NUM], 
				   PROGRESS_STEP, '=');
		
		for (bank = 0; bank < cur_chip_param.param[PSOC1_PARAM_BANK_NUM]; 
			 bank++)
		{
			uint32_t lock_bank_addr = 
								bank * (cur_chip_param.app_page_num >> 2);
			
			if (cur_chip_param.param[PSOC1_PARAM_BANK_NUM] > 1)
			{
				issp_sel_reg_bank(1);
				issp_set_flash_bank(bank);
				issp_sel_reg_bank(0);
			}
			for (addr = 0; 
				 addr < (cur_chip_param.app_page_num >> 2); 
				 addr++)
			{
				issp_write_sram(PSOC1_ISSP_SSC_DEFAULT_POINTER + addr, 
								pi->lock[lock_bank_addr + addr]);
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
	if (cur_prog_mode & PSOC1_RESET_MODE)
	{
		if (ERROR_OK != issp_leave_program_mode(ISSP_PM_RESET))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "leave program mode");
			ret = ERRCODE_FAILURE_OPERATION;
		}
	}
	else
	{
		if (ERROR_OK != issp_leave_program_mode(ISSP_PM_POWER_ON))
		{
			LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATION), 
					  "leave program mode");
			ret = ERRCODE_FAILURE_OPERATION;
		}
	}
	issp_fini();
	if (ERROR_OK != issp_commit())
	{
		LOG_ERROR(_GETTEXT(ERRMSG_FAILURE_OPERATE_DEVICE), 
				  "target chip");
		ret = ERRCODE_FAILURE_OPERATION;
	}
	
	return ret;
}

