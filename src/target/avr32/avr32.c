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
#include <ctype.h>

#include "app_cfg.h"
#include "app_type.h"
#include "app_err.h"
#include "app_log.h"
#include "prog_interface.h"

#include "timer.h"

#include "memlist.h"
#include "filelist.h"
#include "pgbar.h"

#include "vsprog.h"
#include "programmer.h"
#include "target.h"

#include "avr32.h"
#include "avr32_internal.h"

#define CUR_TARGET_STRING			AVR32_STRING

struct program_area_map_t avr32_program_area_map[] = 
{
	{APPLICATION_CHAR, 1, 0, 0, 0, AREA_ATTR_EWR | AREA_ATTR_RNP},
	{0, 0, 0, 0, 0, 0}
};

const struct program_mode_t avr32_program_mode[] = 
{
	{'j', SET_FREQUENCY, JTAG_HL},
	{0, NULL, 0}
};

ENTER_PROGRAM_MODE_HANDLER(avr32jtag);
LEAVE_PROGRAM_MODE_HANDLER(avr32jtag);
ERASE_TARGET_HANDLER(avr32jtag);
WRITE_TARGET_HANDLER(avr32jtag);
READ_TARGET_HANDLER(avr32jtag);
struct program_functions_t avr32_program_functions = 
{
	NULL,			// execute
	ENTER_PROGRAM_MODE_FUNCNAME(avr32jtag), 
	LEAVE_PROGRAM_MODE_FUNCNAME(avr32jtag), 
	ERASE_TARGET_FUNCNAME(avr32jtag), 
	WRITE_TARGET_FUNCNAME(avr32jtag), 
	READ_TARGET_FUNCNAME(avr32jtag)
};

static void avr32_usage(void)
{
	printf("\
Usage of %s:\n\
  -m,  --mode <MODE>                        set mode<j>\n\
  -F,  --frequency <FREQUENCY>              set JTAG frequency, in KHz\n\n",
			CUR_TARGET_STRING);
}

PARSE_ARGUMENT_HANDLER(avr32)
{
	uint8_t mode;
	
	switch (cmd)
	{
	case 'h':
		avr32_usage();
		break;
	case 'm':
		if (NULL == argu)
		{
			LOG_ERROR(ERRMSG_INVALID_OPTION, cmd);
			return ERRCODE_INVALID_OPTION;
		}
		mode = (uint8_t)strtoul(argu, NULL,0);
		switch (mode)
		{
		case AVR32_JTAG:
			break;
		}
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

#define jtag_init()					interfaces->jtag_hl.init()
#define jtag_fini()					interfaces->jtag_hl.fini()
#define jtag_config(kHz,a,b,c,d)	\
	interfaces->jtag_hl.config((kHz), (a), (b), (c), (d))
#define jtag_runtest(len)			interfaces->jtag_hl.runtest(len)
#define jtag_ir_write(i, len)		\
	interfaces->jtag_hl.ir((uint8_t*)(i), (len), AVR32_JTAG_RTI_CYCLE, 0)
#define jtag_dr_write(d, len)		\
	interfaces->jtag_hl.dr((uint8_t*)(d), (len), AVR32_JTAG_RTI_CYCLE, 0)
#define jtag_dr_read(d, len)		\
	interfaces->jtag_hl.dr((uint8_t*)(d), (len), AVR32_JTAG_RTI_CYCLE, 1)
#define jtag_register_callback(s,r)	\
	interfaces->jtag_hl.register_callback((s), (r))

// retry 1000 times with 0 interval
#define poll_start()				interfaces->poll.start(1000, 0)
#define poll_end()					interfaces->poll.end()
#define poll_ok(o, m, v)			\
	interfaces->poll.checkok(POLL_CHECK_EQU, (o), 1, (m), (v))
#define poll_fail(o, m, v)		\
	interfaces->poll.checkfail(POLL_CHECK_EQU, (o), 1, (m), (v))

#define delay_ms(ms)				interfaces->delay.delayms((ms) | 0x8000)
#define delay_us(us)				interfaces->delay.delayus((us) & 0x7FFF)
#define jtag_commit()				interfaces->peripheral_commit()

#define avr32jtag_Instr(ir)			jtag_ir_write((ir), AVR32_JTAG_INS_Len)
#define avr32jtag_DataW				jtag_dr_write
#define avr32jtag_DataR				jtag_dr_read

static struct interfaces_info_t *interfaces = NULL;

static uint8_t pending_4bytes = 0;
RESULT avr32jtag_receive_callback(enum jtag_irdr_t cmd, uint32_t ir, 
									uint8_t *dest_buffer, uint8_t *src_buffer, 
									uint16_t bytelen, uint16_t *processed)
{
	if (NULL == src_buffer)
	{
		return ERROR_FAIL;
	}
	
	switch(cmd)
	{
	case JTAG_SCANTYPE_IR:
		return ERROR_OK;
		break;
	case JTAG_SCANTYPE_DR:
		if ((5 == bytelen) 
			&& ((AVR32_JTAG_INS_MEMORY_WORD_ACCESS == ir) 
				|| (AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS == ir)))
		{
			*processed = 1;
			if (dest_buffer != NULL)
			{
				memcpy(dest_buffer, src_buffer, 4);
			}
		}
		return ERROR_OK;
		break;
	}
	
	return ERROR_FAIL;
}

RESULT avr32jtag_send_callback(enum jtag_irdr_t cmd, uint32_t ir, 
								uint8_t *dest_buffer, uint8_t *src_buffer,
								uint16_t bytelen, uint16_t *processed_len)
{
	if ((NULL == src_buffer) || (NULL == dest_buffer))
	{
		return ERROR_FAIL;
	}
	
	switch(cmd)
	{
	case JTAG_SCANTYPE_IR:
		return ERROR_OK;
		break;
	case JTAG_SCANTYPE_DR:
		if ((5 == bytelen) 
			&& pending_4bytes
			&& ((AVR32_JTAG_INS_MEMORY_WORD_ACCESS == ir) 
				|| (AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS == ir)))
		{
			pending_4bytes = 0;
			*processed_len = 5;
			memset(dest_buffer, 0, 5);
		}
		return ERROR_OK;
		break;
	}
	
	return ERROR_FAIL;
}

static RESULT avr32jtag_sab_word_access(uint8_t slave_addr, uint32_t addr, 
											uint8_t *data, uint8_t read)
{
	uint8_t ir;
	uint64_t cmd;
	
	// check
	if ((slave_addr != AVR32_SAB_SLAVE_OCD)
		&& (slave_addr != AVR32_SAB_SLAVE_HSB)
		&& (slave_addr != AVR32_SAB_SLAVE_MSU))
	{
		LOG_ERROR(ERRMSG_INVALID_ADDRESS, slave_addr, "sab slave address");
		return ERROR_FAIL;
	}
	if (addr & 3)
	{
		LOG_ERROR(ERRMSG_INVALID_ADDRESS, addr, "sab word access");
		return ERROR_FAIL;
	}
	
	// Phase 1: Write AVR32_JTAG_INS_MEMORY_WORD_ACCESS to IR
	// and poll for BUSY, note that PROTECT and ERROR is the failure bit
	poll_start();
	ir = AVR32_JTAG_INS_MEMORY_WORD_ACCESS;
	avr32jtag_Instr(&ir);
	poll_fail(0, AVR32_JTAG_IRRET_PROTECT, AVR32_JTAG_IRRET_PROTECT);
	poll_fail(0, AVR32_JTAG_IRRET_ERROR, AVR32_JTAG_IRRET_ERROR);
	poll_ok(0, AVR32_JTAG_IRRET_BUSY, 0);
	poll_end();
	
	// Phase 2: Write Address
	cmd = (read > 0) | (addr >> 1) | ((uint64_t)slave_addr << 31);
	avr32jtag_DataW(&cmd, 35);
	
	// Phase 3: Read/Write Data
	if (read)
	{
		poll_start();
		pending_4bytes = 1;
		avr32jtag_DataR(data, 34);
		poll_fail(0, AVR32_JTAG_DRRET_ERROR, AVR32_JTAG_DRRET_ERROR);
		poll_ok(0, AVR32_JTAG_DRRET_BUSY, 0);
		poll_end();
	}
	else
	{
		// no error will occur here
		poll_start();
		avr32jtag_DataW(data, 32);
		poll_fail(3, AVR32_JTAG_IRRET_PROTECT, AVR32_JTAG_IRRET_PROTECT);
		poll_fail(3, AVR32_JTAG_IRRET_ERROR, AVR32_JTAG_IRRET_ERROR);
		poll_ok(3, AVR32_JTAG_DRRET_BUSY, 0);
		poll_end();
		
		// Phase 4: Wait Ready
		poll_start();
		ir = AVR32_JTAG_INS_MEMORY_WORD_ACCESS;
		avr32jtag_Instr(&ir);
		poll_fail(0, AVR32_JTAG_IRRET_PROTECT, AVR32_JTAG_IRRET_PROTECT);
		poll_fail(0, AVR32_JTAG_IRRET_ERROR, AVR32_JTAG_IRRET_ERROR);
		poll_ok(0, AVR32_JTAG_IRRET_BUSY, 0);
		poll_end();
	}
	
	return ERROR_OK;
}

static RESULT avr32jtag_sab_access(uint8_t slave_addr, uint32_t addr, 
									uint8_t *data, uint8_t read, uint32_t len)
{
	uint8_t ir;
	uint32_t i;
	
	if (0 == len)
	{
		return ERROR_OK;
	}
	
	// Phase 1: Read/Write the first Word
	avr32jtag_sab_word_access(slave_addr, addr, data, read);
	if (1 == len)
	{
		return ERROR_OK;
	}
	
	// Phase 2: Write AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS to IR
	ir = AVR32_JTAG_INS_MEMORY_BLOCK_ACCESS;
	avr32jtag_Instr(&ir);

	// Phase 3: DR Read Loop
	for (i = 1; i < len; i++)
	{
		if (read)
		{
			poll_start();
			pending_4bytes = 1;
			avr32jtag_DataR(data + 4 * i, 34);
			poll_fail(0, AVR32_JTAG_DRRET_ERROR, AVR32_JTAG_DRRET_ERROR);
			poll_ok(0, AVR32_JTAG_DRRET_BUSY, 0);
			poll_end();
		}
		else
		{
			poll_start();
			avr32jtag_DataW(data + 4 * i, 32);
			poll_fail(3, AVR32_JTAG_IRRET_PROTECT, AVR32_JTAG_IRRET_PROTECT);
			poll_fail(3, AVR32_JTAG_IRRET_ERROR, AVR32_JTAG_IRRET_ERROR);
			poll_ok(3, AVR32_JTAG_IRRET_BUSY, 0);
			poll_end();
		}
	}
	
	return ERROR_OK;
}

static RESULT avr32jtag_fcmd_call(uint8_t command, uint16_t pagen)
{
	uint32_t data;
	uint32_t start, end;
	
	data = command | (pagen << 8) | AVR32_FLASHC_FCMD_KEY;
	avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FCR, 
								(uint8_t*)&data, AVR32_JTAG_WRITE, 1);
	
	start = get_time_in_ms();
	do
	{
		data = 0;
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FSR, 
								(uint8_t*)&data, AVR32_JTAG_READ, 1);
		if (ERROR_OK != jtag_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read fsr");
			return ERRCODE_FAILURE_OPERATION;
		}
		end = get_time_in_ms();
	} while (!(data & 1) && ((end - start) < 5000));
	
	if (!(data & 1) || (data & 0x0C))
	{
		LOG_DEBUG(INFOMSG_REG_08X, "FLASHC_FSR", data);
		return ERROR_FAIL;
	}
	
	return ERROR_OK;
}

ENTER_PROGRAM_MODE_HANDLER(avr32jtag)
{
	struct program_info_t *pi = context->pi;
	
	interfaces = &(context->prog->interfaces);
	
	if (!pi->frequency)
	{
		pi->frequency = 4500;
	}
	
	// init
	jtag_init();
	jtag_config(pi->frequency, pi->jtag_pos.ub, pi->jtag_pos.ua, 
					pi->jtag_pos.bb, pi->jtag_pos.ba);
	if (ERROR_OK != jtag_commit())
	{
		LOG_ERROR(ERRMSG_FAILURE_OPERATION, "init jtag");
		return ERROR_FAIL;
	}
	jtag_register_callback(avr32jtag_send_callback, 
								   avr32jtag_receive_callback);
	
	return jtag_commit();
}

LEAVE_PROGRAM_MODE_HANDLER(avr32jtag)
{
	RESULT ret;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(success);
	
	jtag_fini();
	ret = jtag_commit();
	jtag_register_callback(NULL, NULL);
	return ret;
}

ERASE_TARGET_HANDLER(avr32jtag)
{
	struct chip_param_t *param = context->param;
	RESULT ret = ERROR_OK;
	uint32_t i;
	uint32_t data;
	uint16_t pagen;
	
	REFERENCE_PARAMETER(context);
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(size);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		// read fsr to check lock
		data = 0;
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FSR, 
								(uint8_t*)&data, AVR32_JTAG_READ, 1);
		if (ERROR_OK != jtag_commit())
		{
			return ERROR_FAIL;
		}
		LOG_DEBUG(INFOMSG_REG_08X, "FLASHC_FSR", data);
		for (i = 0; i < 16; i++)
		{
			if (data & (1 << (16 + i)))
			{
				// call AVR32_FLASHC_FCMD_UP
				pagen = (uint16_t)(param->chip_areas[APPLICATION_IDX].page_num * i / 16);
				ret = avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_UP, pagen);
				if (ret != ERROR_OK)
				{
					LOG_ERROR(ERRMSG_FAILURE_OPERATION, "unlock flash page");
					return ERRCODE_FAILURE_OPERATION;
				}
			}
		}
		
		avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_EGPB, 17);
		avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_EGPB, 18);
		avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_EGPB, 19);
		
		// check BOOTPROT
		data = 0;
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FGPFRHI, 
								(uint8_t*)&data, AVR32_JTAG_READ, 1);
		if (ERROR_OK != jtag_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read fsr");
			return ERRCODE_FAILURE_OPERATION;
		}
		LOG_DEBUG(INFOMSG_REG_08X, "fusehi", data);
		data = 0;
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FGPFRLO, 
								(uint8_t*)&data, AVR32_JTAG_READ, 1);
		if (ERROR_OK != jtag_commit())
		{
			LOG_ERROR(ERRMSG_FAILURE_OPERATION, "read fsr");
			return ERRCODE_FAILURE_OPERATION;
		}
		LOG_DEBUG(INFOMSG_REG_08X, "fuselo", data);
		
		return avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_EA, 0);
		break;
	default:
		return ERROR_FAIL;
		break;
	}
}

WRITE_TARGET_HANDLER(avr32jtag)
{
	struct chip_param_t *param = context->param;
	struct chip_area_info_t *flash_info = &param->chip_areas[APPLICATION_IDX];
	uint16_t pagen;
	
	REFERENCE_PARAMETER(addr);
	REFERENCE_PARAMETER(buff);
	
	switch (area)
	{
	case APPLICATION_CHAR:
		pagen = (uint16_t)((addr - flash_info->addr) / flash_info->page_size);
		avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_CPB, 0);
		avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, addr, 
								buff, AVR32_JTAG_WRITE, size / 4);
		jtag_commit();
		if (ERROR_OK != avr32jtag_fcmd_call(AVR32_FLASHC_FCMD_WP, pagen))
		{
			return ERROR_FAIL;
		}
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}

READ_TARGET_HANDLER(avr32jtag)
{
	struct chip_param_t *param = context->param;
	uint32_t page_size;
	uint32_t current_size;
	uint8_t ir;
	uint32_t dr;
	
	switch (area)
	{
	case CHIPID_CHAR:
		// read jtag_id use IDCODE
		// this should always work
		ir = AVR32_JTAG_INS_IDCODE;
		avr32jtag_Instr(&ir);
		dr = 0;
		jtag_dr_read(&dr, 32);
		if (ERROR_OK != jtag_commit())
		{
			return ERROR_FAIL;
		}
		LOG_DEBUG(INFOMSG_REG_08X, "JTAGID", dr);
		
		for (ir = 0; ir < 10; ir++)
		{
			dr = 0;
			avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, AVR32_FLASHC_FSR, 
									(uint8_t*)&dr, AVR32_JTAG_READ, 1);
			if (ERROR_OK != jtag_commit())
			{
				return ERROR_FAIL;
			}
			LOG_DEBUG(INFOMSG_REG_08X, "FLASHC_FSR", dr);
			
			dr = 0;
			avr32jtag_sab_access(AVR32_SAB_SLAVE_OCD, 0, 
									(uint8_t*)&dr, AVR32_JTAG_READ, 1);
			if (ERROR_OK != jtag_commit())
			{
				return ERROR_FAIL;
			}
			LOG_DEBUG(INFOMSG_REG_08X, "OCD_DID", dr);
		}
		
		// clear rev area of id
		dr &= ~0xF0000000;
		memcpy(buff, &dr, 4);
		break;
	case APPLICATION_CHAR:
		// check
		page_size = param->chip_areas[APPLICATION_IDX].page_size;
		if (size % page_size)
		{
			return ERROR_FAIL;
		}
		
		current_size = 0;
		while (current_size < size)
		{
			avr32jtag_sab_access(AVR32_SAB_SLAVE_HSB, addr + current_size, 
						&buff[current_size], AVR32_JTAG_READ, page_size / 4);
			current_size += page_size;
			pgbar_update(page_size);
		}
		return jtag_commit();
		break;
	default:
		return ERROR_FAIL;
		break;
	}
	
	return ERROR_OK;
}
